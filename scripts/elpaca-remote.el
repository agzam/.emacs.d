;;; scripts/elpaca-remote.el --- keep elpaca's clones in step with their recipes -*- lexical-binding: t; -*-
;; Shared by `bb update's two drivers (elpaca-update.el, elpaca-live-update.el).
;; Two heals for the clones elpaca keeps under `elpaca-sources-directory', both
;; about a clone and its recipe drifting apart:
;;
;; `elpaca-remote-sync-origins' - elpaca computes a clone's URL once, at clone
;; time, and never looks at `origin' again.  A recipe that later names another
;; host (a mirror, once the upstream host dropped off DNS) steers fresh clones
;; only; an existing clone keeps fetching from the dead host and fails every
;; update.  Runs before the fetch: re-points `origin' wherever the recipe's
;; URL differs from the clone's.
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
