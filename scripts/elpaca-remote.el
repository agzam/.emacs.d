;;; scripts/elpaca-remote.el --- keep elpaca's clones in step with their recipes -*- lexical-binding: t; -*-
;; Shared by `bb update's two drivers (elpaca-update.el, elpaca-live-update.el).
;; Three heals for the clones elpaca keeps under `elpaca-sources-directory', each
;; about a clone drifting from its recipe or its upstream:
;;
;; `elpaca-remote-sync-origins' - elpaca computes a clone's URL once, at clone
;; time, and never looks at `origin' again.  A recipe that later names another
;; host (a mirror, once the upstream host dropped off DNS) steers fresh clones
;; only; an existing clone keeps fetching from the dead host and fails every
;; update.  Runs before the fetch: re-points `origin' wherever the recipe's
;; URL differs from the clone's.
;;
;; `elpaca-remote-retarget-renamed' - a clone stays on the branch it was
;; cloned on.  Once upstream renames its default branch, every update fails
;; at the fetch or the update log; move the clone onto the remote's new
;; default and merge again.  Runs before the diverged reset.
;;
;; `elpaca-remote-reset-diverged' - elpaca merges with `--ff-only', which
;; refuses whenever upstream rewrote its history (a force-pushed master).  The
;; clone is a cache of that upstream, so take upstream's side: reset a clean
;; worktree onto its tracking branch and put the package back through the
;; merge, which now fast-forwards and rebuilds.  Runs once the update queue
;; has settled, over the packages whose merge step failed.

(require 'cl-lib)
(require 'subr-x)
(require 'elpaca nil t)

(defvar elpaca-sources-directory)

(defun elpaca-remote--git (dir &rest args)
  "Run git ARGS in DIR; return (EXIT . OUTPUT), OUTPUT being trimmed stdout+stderr."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (cons (apply #'call-process "git" nil t nil args)
            (string-trim (buffer-string))))))

(defun elpaca-remote--clone-p (dir)
  "Non-nil when DIR is a clone elpaca made, as opposed to a build-in-place source."
  (and dir (boundp 'elpaca-sources-directory)
       (file-in-directory-p dir elpaca-sources-directory)))

;;; Origin drift

(defun elpaca-remote--config-origin-url (text)
  "The url of remote \"origin\" in git config TEXT, or nil when it has none."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward "^\\[remote \"origin\"\\][ \t]*$" nil t)
      (let ((end (or (save-excursion (re-search-forward "^\\[" nil t)) (point-max))))
        (when (re-search-forward "^[ \t]*url[ \t]*=[ \t]*\\(.+?\\)[ \t]*$" end t)
          (match-string 1))))))

(defun elpaca-remote-origin-url (dir)
  "URL clone DIR fetches `origin' from, read from .git/config without a process.
A `git remote get-url' per package would cost the live session seconds at
kickoff; the file read is what the update's snapshot does for HEAD too."
  (let ((config (expand-file-name ".git/config" dir)))
    (when (file-readable-p config)
      (elpaca-remote--config-origin-url
       (with-temp-buffer (insert-file-contents config) (buffer-string))))))

(defun elpaca-remote--recipe-url (e)
  "The URL elpaca would clone E from today, or nil when that is not for us to say.
Nil for a recipe that configures its own remotes (:remotes) - elpaca manages
those - for a non-git type, and for anything elpaca cannot resolve."
  (when (fboundp 'elpaca-git--repo-uri)
    (let ((recipe (elpaca<-recipe e)))
      (when (and (memq (plist-get recipe :type) '(nil git))
                 (not (plist-get recipe :remotes)))
        (condition-case nil (elpaca-git--repo-uri recipe) (error nil))))))

(defun elpaca-remote-sync-origins (&optional packages emit)
  "Re-point `origin' of every clone whose recipe now names another URL.
PACKAGES, when non-nil, limits the scan to those ids.  EMIT, when non-nil,
is called as (EMIT FMT &rest ARGS) per re-pointed clone.  Return the ids
re-pointed.  Packages sharing one clone read the same config, so the second
finds it already in step."
  (let (synced)
    (dolist (cell (elpaca--queued))
      (let ((id (car cell)) (e (cdr cell)))
        (when-let* (((or (null packages) (memq id packages)))
                    (dir (ignore-errors (elpaca<-source-dir e)))
                    ((elpaca-remote--clone-p dir))
                    (want (elpaca-remote--recipe-url e))
                    (have (elpaca-remote-origin-url dir))
                    ((not (string= want have))))
          (pcase-let ((`(,exit . ,out)
                       (elpaca-remote--git dir "remote" "set-url" "origin" want)))
            (if (zerop exit)
                (progn
                  (push id synced)
                  (when emit (funcall emit "remote: %s origin %s -> %s" id have want)))
              (when emit
                (funcall emit "remote: %s origin %s -> %s failed: %s" id have want out)))))))
    (nreverse synced)))

;;; Renamed default branch

(defun elpaca-remote-gone-upstream (dir)
  "Describe clone DIR's branch when the remote no longer has its upstream, or nil.
Returns (:branch B :remote R :upstream U).  Asks the remote, because a
single-branch clone fails its fetch before any tracking ref goes stale."
  (pcase-let ((`(,exit . ,branch) (elpaca-remote--git dir "symbolic-ref" "--short" "-q" "HEAD")))
    (when (zerop exit)
      (pcase-let ((`(,remote ,ref)
                   (split-string
                    (cdr (elpaca-remote--git
                          dir "for-each-ref"
                          "--format=%(upstream:remotename)%09%(upstream:remoteref)"
                          (concat "refs/heads/" branch)))
                    "\t")))
        ;; ls-remote exits 2 when the remote answers without the ref
        (when (and ref (= 2 (car (elpaca-remote--git dir "ls-remote" "--exit-code" remote ref))))
          (list :branch branch :remote remote
                :upstream (concat remote "/" (string-remove-prefix "refs/heads/" ref))))))))

(defun elpaca-remote--default-branch (dir remote)
  "REMOTE's default branch, asked of the remote from clone DIR, or nil."
  (pcase-let ((`(,exit . ,out) (elpaca-remote--git dir "ls-remote" "--symref" remote "HEAD")))
    (when (and (zerop exit) (string-match "^ref: refs/heads/\\(.+\\)\tHEAD$" out))
      (match-string 1 out))))

(defun elpaca-remote--pinned-p (recipe)
  "Non-nil when RECIPE chooses its own branch, ref or remotes."
  (cl-some (lambda (key) (plist-get recipe key)) '(:branch :tag :ref :pin :remotes)))

(defun elpaca-remote--git-until-failure (dir &rest commands)
  "Run the git COMMANDS in DIR in order, skipping nil ones.
Return the output of the first that fails, or nil when all succeed."
  (cl-loop for args in (delq nil commands)
           for (exit . out) = (apply #'elpaca-remote--git dir args)
           unless (zerop exit) return out))

(defun elpaca-remote-retarget-renamed (&optional emit)
  "Move failed clones whose upstream branch is gone onto the remote's default.
Fetch only that branch, rename the local one after it, track it, and queue
`elpaca-merge' again.  EMIT, when non-nil, is called as (EMIT FMT &rest ARGS)
per package.  Return the ids re-merged; callers wait for elpaca to settle."
  (let (moved)
    (dolist (cell (elpaca--queued))
      (let ((id (car cell)) (e (cdr cell)))
        (when-let* (((eq (elpaca<-status e) 'failed))
                    ((not (elpaca-remote--pinned-p (elpaca<-recipe e))))
                    (dir (ignore-errors (elpaca<-source-dir e)))
                    ((elpaca-remote--clone-p dir))
                    (gone (elpaca-remote-gone-upstream dir)))
          (let* ((branch (plist-get gone :branch))
                 (remote (plist-get gone :remote))
                 (target (elpaca-remote--default-branch dir remote))
                 (err (if (null target)
                          (format "no default branch on %s" remote)
                        (elpaca-remote--git-until-failure
                         dir
                         (list "remote" "set-branches" remote target)
                         (list "fetch" "-q" remote)
                         (unless (equal branch target) (list "branch" "-m" branch target))
                         (list "branch" "-u" (concat remote "/" target) target)
                         (list "remote" "set-head" remote target)))))
            (if err
                (when emit (funcall emit "retarget (renamed): %s failed: %s" id err))
              (when emit
                (funcall emit "retarget (renamed): %s %s -> %s/%s, upstream %s is gone"
                         id branch remote target (plist-get gone :upstream)))
              (elpaca-merge id)
              (push id moved))))))
    (when (and moved (fboundp 'elpaca-process-queues))
      (elpaca-process-queues))
    (nreverse moved)))

;;; Rewritten upstream

(defun elpaca-remote-divergence (dir)
  "Describe how clone DIR's branch diverged from its upstream, or nil.
Returns (:branch B :upstream U :old SHA :new SHA :dropped N) only when the
checked-out branch tracks an upstream it is not an ancestor of - so no
fast-forward exists - and no tracked file is modified.  Anything else is not
this heal's to touch: a detached or untracked branch, a fast-forward that
failed for another reason, or edits a reset would destroy.  N counts the
local-only commits a reset leaves behind (in the reflog)."
  (pcase-let ((`(,b-exit . ,branch) (elpaca-remote--git dir "symbolic-ref" "--short" "-q" "HEAD"))
              (`(,u-exit . ,upstream) (elpaca-remote--git dir "rev-parse" "--abbrev-ref" "@{u}")))
    (when (and (zerop b-exit) (zerop u-exit)
               (not (zerop (car (elpaca-remote--git dir "merge-base" "--is-ancestor"
                                                    "HEAD" "@{u}"))))
               (string-empty-p (cdr (elpaca-remote--git dir "status" "--porcelain"
                                                        "--untracked-files=no"))))
      (list :branch branch :upstream upstream
            :old (cdr (elpaca-remote--git dir "rev-parse" "--short" "HEAD"))
            :new (cdr (elpaca-remote--git dir "rev-parse" "--short" "@{u}"))
            :dropped (string-to-number
                      (cdr (elpaca-remote--git dir "rev-list" "--count" "@{u}..HEAD")))))))

(defun elpaca-remote-reset-diverged (&optional emit)
  "Reset clones whose merge failed on a rewritten upstream, and merge them again.
For every package elpaca failed in its merge step whose clone diverged from
its tracking branch (see `elpaca-remote-divergence'): `git reset --hard'
onto that branch, then queue the package through `elpaca-merge' again - a
fast-forward now, followed by the rebuild - and process the queue once.
EMIT, when non-nil, is called as (EMIT FMT &rest ARGS) per package.  Return
the ids put back through the merge; callers wait for elpaca to settle."
  (let (reset)
    (dolist (cell (elpaca--queued))
      (let ((id (car cell)) (e (cdr cell)))
        (when-let* (((eq (elpaca<-status e) 'failed))
                    ((eq (elpaca<-current-step e) 'elpaca-git--merge))
                    (dir (ignore-errors (elpaca<-source-dir e)))
                    ((elpaca-remote--clone-p dir))
                    (div (elpaca-remote-divergence dir)))
          (pcase-let ((`(,exit . ,out) (elpaca-remote--git dir "reset" "-q" "--hard" "@{u}")))
            (if (zerop exit)
                (progn
                  (when emit
                    (funcall emit "reset (diverged): %s %s %s -> %s %s, %d dropped commit(s) stay in the reflog"
                             id (plist-get div :branch) (plist-get div :old)
                             (plist-get div :upstream) (plist-get div :new)
                             (plist-get div :dropped)))
                  (elpaca-merge id)
                  (push id reset))
              (when emit (funcall emit "reset (diverged): %s failed: %s" id out)))))))
    (when (and reset (fboundp 'elpaca-process-queues))
      (elpaca-process-queues))
    (nreverse reset)))

(provide 'elpaca-remote)
;;; scripts/elpaca-remote.el ends here
