;;; scripts/elpaca-local.el --- rebuild changed build-in-place packages -*- lexical-binding: t; -*-
;; Shared by `bb update's two drivers (elpaca-update.el, elpaca-live-update.el).
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
