;;; tests/scripts/elpaca-local-tests.el --- changed-local rebuild specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "scripts/elpaca-local.el")

;; Elpaca is absent from the buttercup sandbox: give the struct accessor the
;; glue reaches for an inert stub the specs override via `cl-letf', and mark
;; the sources-dir var special so the specs can dynamically bind it.
(unless (fboundp 'elpaca<-source-dir) (defun elpaca<-source-dir (_e) nil))
(unless (fboundp 'elpaca<-id) (defun elpaca<-id (_e) nil))
(unless (fboundp 'elpaca-rebuild) (defun elpaca-rebuild (_id &optional _interactive) nil))
(unless (fboundp 'elpaca-process-queues) (defun elpaca-process-queues (&optional _filter) nil))
(defvar elpaca-sources-directory)

(defun elpaca-local-tests--touch (path secs)
  "Create PATH (making parent dirs) and set its mtime to SECS epoch seconds."
  (make-directory (file-name-directory path) t)
  (write-region "" nil path nil 'silent)
  (set-file-times path (time-convert secs 'list)))

(describe "elpaca-local--autoloads-mtime"
  (it "returns nil for a directory with no autoloads file"
    (let ((dir (make-temp-file "elpaca-local" t)))
      (unwind-protect
          (expect (elpaca-local--autoloads-mtime dir) :to-be nil)
        (delete-directory dir t))))
  (it "returns the mtime of PKG-autoloads.el"
    (let ((dir (make-temp-file "elpaca-local" t)))
      (unwind-protect
          (progn
            (elpaca-local-tests--touch (expand-file-name "pkg-autoloads.el" dir) 2000)
            (expect (time-convert (elpaca-local--autoloads-mtime dir) 'integer)
                    :to-equal 2000))
        (delete-directory dir t)))))

(describe "elpaca-local--generated-p"
  (it "flags elpaca's regenerated autoloads and pkg files"
    (expect (elpaca-local--generated-p "/x/foo-autoloads.el") :to-be-truthy)
    (expect (elpaca-local--generated-p "/x/foo-pkg.el") :to-be-truthy))
  (it "leaves real sources alone, including -md/-core suffixes"
    (expect (elpaca-local--generated-p "/x/foo.el") :to-be nil)
    (expect (elpaca-local--generated-p "/x/foo-md.el") :to-be nil)
    (expect (elpaca-local--generated-p "/x/foo-core.el") :to-be nil)))

(describe "elpaca-local--dirty-p"
  (let (root src build)
    (before-each
      (setq root (make-temp-file "elpaca-local" t)
            src (expand-file-name "src/" root)
            build (expand-file-name "build/" root))
      (make-directory src)
      (make-directory build)
      ;; the build's autoloads is the "last built at t=2000" marker
      (elpaca-local-tests--touch (expand-file-name "pkg-autoloads.el" build) 2000))
    (after-each (delete-directory root t))

    (it "is nil when every linked source predates the build"
      (let ((s (expand-file-name "a.el" src))
            (tgt (expand-file-name "a.el" build)))
        (elpaca-local-tests--touch s 1000)
        (elpaca-local-tests--touch tgt 2000)
        (expect (elpaca-local--dirty-p (list (cons s tgt)) build) :to-be nil)))

    (it "is non-nil when a linked source is newer than the build"
      (let ((s (expand-file-name "a.el" src))
            (tgt (expand-file-name "a.el" build)))
        (elpaca-local-tests--touch s 3000)
        (elpaca-local-tests--touch tgt 2000)
        (expect (elpaca-local--dirty-p (list (cons s tgt)) build) :to-be-truthy)))

    (it "ignores a regenerated autoloads artifact newer than the build"
      ;; Elpaca rewrites PKG-autoloads.el back into a build-in-place source
      ;; tree every build, so it always postdates the marker; comparing it
      ;; would flag an otherwise-clean package dirty forever (the prisma bug).
      (let ((s (expand-file-name "a.el" src))
            (tgt (expand-file-name "a.el" build))
            (al-src (expand-file-name "pkg-autoloads.el" src))
            (al-tgt (expand-file-name "pkg-autoloads.el" build)))
        (elpaca-local-tests--touch s 1000)
        (elpaca-local-tests--touch tgt 2000)
        (elpaca-local-tests--touch al-src 3000) ; newer than the t=2000 build
        (elpaca-local-tests--touch al-tgt 2000)
        (expect (elpaca-local--dirty-p
                 (list (cons s tgt) (cons al-src al-tgt)) build)
                :to-be nil)))

    (it "is non-nil for a new source file never linked into the build"
      (let ((s (expand-file-name "new.el" src))
            (tgt (expand-file-name "new.el" build)))
        (elpaca-local-tests--touch s 1000)     ; old mtime still counts as new
        (expect (file-exists-p tgt) :to-be nil)
        (expect (elpaca-local--dirty-p (list (cons s tgt)) build) :to-be-truthy)))

    (it "treats a build missing its autoloads as dirty"
      (let ((s (expand-file-name "a.el" src))
            (tgt (expand-file-name "a.el" build))
            (fresh (expand-file-name "empty/" root)))
        (make-directory fresh)
        (elpaca-local-tests--touch s 1000)
        (elpaca-local-tests--touch tgt 1000)
        (expect (elpaca-local--dirty-p (list (cons s tgt)) fresh) :to-be-truthy)))))

(describe "elpaca-local-package-p"
  (it "is nil for a source under the elpaca sources (clone) directory"
    (let* ((sources (file-name-as-directory (make-temp-file "sources" t)))
           (elpaca-sources-directory sources))
      (cl-letf (((symbol-function 'elpaca<-source-dir)
                 (lambda (_e) (expand-file-name "gptel/" sources))))
        (expect (elpaca-local-package-p 'fake) :to-be nil))))
  (it "is non-nil for a source outside the sources directory (build-in-place)"
    (let* ((sources (file-name-as-directory (make-temp-file "sources" t)))
           (elpaca-sources-directory sources)
           (local (make-temp-file "local-checkout" t)))
      (cl-letf (((symbol-function 'elpaca<-source-dir) (lambda (_e) local)))
        (expect (elpaca-local-package-p 'fake) :to-be-truthy)))))

(describe "elpaca-local-rebuild-changed"
  ;; Regression: `elpaca-rebuild' only kicks the queue when called
  ;; interactively, so queueing rebuilds here without a following
  ;; `elpaca-process-queues' left the package stuck at `queued' forever - the
  ;; live update never settling on go-jira.
  (it "queues each changed local, then processes the queue exactly once"
    (let (rebuilt (processed 0))
      (cl-letf (((symbol-function 'elpaca-local-changed) (lambda () '(a b)))
                ((symbol-function 'elpaca<-id) (lambda (e) e))
                ((symbol-function 'elpaca-rebuild)
                 (lambda (id &optional _i) (push id rebuilt)))
                ((symbol-function 'elpaca-process-queues)
                 (lambda (&optional _f) (cl-incf processed))))
        (let ((ids (elpaca-local-rebuild-changed)))
          (expect (nreverse rebuilt) :to-equal '(a b))
          (expect processed :to-equal 1)
          (expect ids :to-equal '(a b))))))

  (it "does not touch the queue when nothing changed"
    (let ((processed 0))
      (cl-letf (((symbol-function 'elpaca-local-changed) (lambda () nil))
                ((symbol-function 'elpaca<-id) (lambda (e) e))
                ((symbol-function 'elpaca-rebuild)
                 (lambda (&rest _) (error "should not rebuild")))
                ((symbol-function 'elpaca-process-queues)
                 (lambda (&optional _f) (cl-incf processed))))
        (expect (elpaca-local-rebuild-changed) :to-be nil)
        (expect processed :to-equal 0))))

  (it "reports each rebuild through the EMIT callback"
    (let (lines)
      (cl-letf (((symbol-function 'elpaca-local-changed) (lambda () '(go-jira)))
                ((symbol-function 'elpaca<-id) (lambda (e) e))
                ((symbol-function 'elpaca-rebuild) (lambda (&rest _) nil))
                ((symbol-function 'elpaca-process-queues) (lambda (&optional _f) nil)))
        (elpaca-local-rebuild-changed
         (lambda (fmt &rest args) (push (apply #'format fmt args) lines)))
        (expect lines :to-equal '("rebuild (local change): go-jira"))))))

;;; tests/scripts/elpaca-local-tests.el ends here
