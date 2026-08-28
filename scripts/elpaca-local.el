;;; scripts/elpaca-local.el --- build-in-place packages during an update -*- lexical-binding: t; -*-
;; Shared by `bb update's two drivers (elpaca-update.el, elpaca-live-update.el).
;; Two jobs, both about packages elpaca builds in place - the
;; `local-dev-packages' checkouts and the vendored in-module ones: keep git out
;; of their sources (`elpaca-local-update-remotes'), and rebuild them when
;; those sources change (`elpaca-local-rebuild-changed').
;;
;; A git-driven update (fetch + ff-only merge) only rebuilds a package when the
;; merge moves its HEAD.  A build-in-place local checkout - the
;; `local-dev-packages' redirects and the two in-module vendored packages - is
;; edited on disk, not pulled, so `bb update' alone never rebuilds it: a newly
;; added source file stays unlinked and the autoloads go stale (the
;; go-jira-status breakage).  These helpers spot such packages by comparing
;; source mtimes to the last build and queue `elpaca-rebuild' for them.
;;
;; "Changed" here means the package's own SOURCE changed.  A dependent left
;; stale by a dependency's recompile (its own source untouched) is a different
;; problem and out of scope.

(require 'cl-lib)
(require 'elpaca nil t)

(defun elpaca-local--autoloads-mtime (build-dir)
  "Modification time of BUILD-DIR's generated autoloads, or nil when absent.
Elpaca rewrites `PKG-autoloads.el' on every build, so its mtime is a reliable
\"last built\" marker to compare source files against."
  (when-let* (((file-directory-p build-dir))
              (file (car (directory-files build-dir t "-autoloads\\.el\\'"))))
    (file-attribute-modification-time (file-attributes file))))

(defun elpaca-local--generated-p (source)
  "Non-nil when SOURCE is an Elpaca-generated build artifact, not a user source.
Elpaca rewrites `PKG-autoloads.el' (and any `PKG-pkg.el') on every build, and
for a build-in-place checkout those land back in the source tree.  Their mtime
therefore always trails the just-written build marker by a hair, so comparing
them would flag every such package dirty forever - the perpetual `rebuild
\(local change)' on an untouched package."
  (string-match-p "\\(?:-autoloads\\|-pkg\\)\\.el\\'" source))

(defun elpaca-local--dirty-p (files build-dir)
  "Non-nil when FILES hold a source change not reflected in BUILD-DIR's build.
FILES is an alist of (SOURCE . BUILD-TARGET) as `elpaca--files' returns.  A
build is dirty when a real source file was never linked into it (a newly added
file), or a source mtime sits past the autoloads timestamp; a build missing
its autoloads entirely counts as dirty.  Elpaca's own generated artifacts are
skipped - they are rewritten every build and would otherwise never settle."
  (if-let* ((built (elpaca-local--autoloads-mtime build-dir)))
      (cl-some
       (lambda (pair)
         (unless (elpaca-local--generated-p (car pair))
           (or (not (file-exists-p (cdr pair)))
               (time-less-p built (file-attribute-modification-time
                                   (file-attributes (car pair)))))))
       files)
    t))

(defun elpaca-local-package-p (e)
  "Non-nil when elpaca E builds in place from a local source rather than a clone.
Local checkouts and the in-module vendored packages live outside
`elpaca-sources-directory'; upstream clones live inside it."
  (when-let* ((src (ignore-errors (elpaca<-source-dir e))))
    (not (file-in-directory-p src elpaca-sources-directory))))

(defun elpaca-local-build-dirty-p (e)
  "Non-nil when build-in-place package E has source changes not yet built."
  (when-let* ((build (ignore-errors (elpaca<-build-dir e)))
              (files (ignore-errors (elpaca--files e))))
    (elpaca-local--dirty-p files build)))

(defun elpaca-local--branch (dir)
  "Return the branch checked out in DIR, or nil when DIR is no git worktree.
A detached HEAD answers with its short sha, having no branch to name."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (when (eq 0 (call-process "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD"))
        (let ((branch (string-trim (buffer-string))))
          (if (not (equal branch "HEAD"))
              branch
            (erase-buffer)
            (when (eq 0 (call-process "git" nil t nil "rev-parse" "--short" "HEAD"))
              (string-trim (buffer-string)))))))))

(defun elpaca-local--skip-lines (packages)
  "Return one aligned line per entry of PACKAGES, an alist of (ID . SOURCE-DIR).
Each line reads `ID - PATH', with ` -- BRANCH' appended where the checkout sits
off `main' or `master' - those two would be on nearly every line and say
nothing.  Names and paths are padded to the widest of the set, which is why the
caller hands the whole set over at once; a tab cannot align columns whose width
is only known here."
  (let* ((rows (mapcar
                (lambda (cell)
                  (let ((branch (elpaca-local--branch (cdr cell))))
                    (list (symbol-name (car cell))
                          (abbreviate-file-name (directory-file-name (cdr cell)))
                          (unless (member branch '(nil "main" "master")) branch))))
                packages))
         (id-width (apply #'max 0 (mapcar (lambda (r) (length (nth 0 r))) rows)))
         ;; Only branch-carrying lines need their path padded; padding the rest
         ;; would trail whitespace to the end of the block.
         (path-width (apply #'max 0 (mapcar (lambda (r) (if (nth 2 r)
                                                            (length (nth 1 r))
                                                          0))
                                            rows))))
    (mapcar (lambda (row)
              (pcase-let ((`(,id ,path ,branch) row))
                (if branch
                    (concat (string-pad id id-width) " - "
                            (string-pad path path-width) " -- " branch)
                  (concat (string-pad id id-width) " - " path))))
            rows)))

(defun elpaca-local--skip-merge-a (fn id &optional fetch interactive)
  "Leave a build-in-place package's source untouched, else call FN on ID.
FN is `elpaca-merge'; FETCH and INTERACTIVE are its own arguments."
  (unless (when-let* ((e (elpaca-get id))) (elpaca-local-package-p e))
    (funcall fn id fetch interactive)))

(defun elpaca-local-update-remotes (&optional packages interactive emit)
  "Fetch and merge the packages elpaca cloned, leaving build-in-place ones alone.
PACKAGES, when non-nil, limits the update to those ids.  INTERACTIVE processes
the queue at once, as in `elpaca-update-all'.  EMIT, when non-nil, is called as
\(EMIT FMT &rest ARGS) once per skipped package.

Neither kind of build-in-place package is elpaca's to update.  A checkout
redirected by `local-dev-packages' is a working tree its author drives with git
- its branch, its unpushed commits, its rebases - and the ff-only merge fails
outright on a branch with no upstream.  A package vendored into a module has no
upstream at all: its `:repo' points inside the config, so the same merge would
run in the config repo.  Both change on disk instead, and reach the build
through `elpaca-local-rebuild-changed'.

The skipped set is known from the queue before anything runs, so its block
prints up front rather than trickling in among the fetches."
  (let ((skipped (cl-loop for (id . e) in (elpaca--queued)
                          when (and (elpaca-local-package-p e)
                                    (or (null packages) (memq id packages)))
                          collect (cons id (elpaca<-source-dir e)))))
    (when emit
      (dolist (line (elpaca-local--skip-lines skipped)) (funcall emit "%s" line)))
    (advice-add 'elpaca-merge :around #'elpaca-local--skip-merge-a)
    (unwind-protect
        (if packages
            (dolist (id packages) (elpaca-update id interactive))
          (elpaca-update-all interactive))
      (advice-remove 'elpaca-merge #'elpaca-local--skip-merge-a))
    (mapcar #'car skipped)))

(defun elpaca-local-changed ()
  "Return queued build-in-place local elpacas whose source changed since build."
  (cl-loop for (_ . e) in (elpaca--queued)
           when (and (elpaca-local-package-p e)
                     (elpaca-local-build-dirty-p e))
           collect e))

(defun elpaca-local-rebuild-changed (&optional emit)
  "Queue and process `elpaca-rebuild' for every changed build-in-place local.
EMIT, when non-nil, is called as (EMIT FMT &rest ARGS) to report each rebuild.
Return the list of rebuilt package ids.

`elpaca-rebuild' only kicks the queue when called interactively; queued here
non-interactively it just flips the package to `queued' and returns, so
without processing the queue ourselves the rebuild never runs and the package
sits at `queued' forever - the update never settles (the go-jira stall).  We
process once after queueing, exactly as elpaca's own interactive updaters do."
  (let ((changed (elpaca-local-changed)))
    (dolist (e changed)
      (when emit (funcall emit "rebuild (local change): %s" (elpaca<-id e)))
      (elpaca-rebuild (elpaca<-id e)))
    (when (and changed (fboundp 'elpaca-process-queues))
      (elpaca-process-queues))
    (mapcar #'elpaca<-id changed)))

(provide 'elpaca-local)
;;; scripts/elpaca-local.el ends here
