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
(unless (fboundp 'elpaca-get) (defun elpaca-get (_id) nil))
(unless (fboundp 'elpaca-merge) (defun elpaca-merge (_id &optional _fetch _interactive) nil))
(unless (fboundp 'elpaca-update) (defun elpaca-update (_id &optional _interactive) nil))
(unless (fboundp 'elpaca-update-all) (defun elpaca-update-all (&optional _interactive) nil))
;; `elpaca--queued' gets no such stub: the report suite asserts elpaca is
;; absent from this sandbox, so it may only exist inside a spec that needs it.
(defmacro elpaca-local-tests--with-queue (queue &rest body)
  "Run BODY with `elpaca--queued' answering QUEUE, leaving it unbound after."
  (declare (indent 1))
  `(let ((bound (fboundp 'elpaca--queued)))
     (unwind-protect
         (cl-letf (((symbol-function 'elpaca--queued) (lambda () ,queue)))
           ,@body)
       (unless bound (fmakunbound 'elpaca--queued)))))
(defvar elpaca-sources-directory)

(defun elpaca-local-tests--git-repo (dir branch)
  "Initialize a git repo in DIR holding one empty commit on BRANCH."
  (let ((default-directory (file-name-as-directory dir)))
    (call-process "git" nil nil nil "init" "-q" "-b" branch ".")
    (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@example"
                  "commit" "-q" "--allow-empty" "-m" "seed")))

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

(describe "elpaca-local--branch"
  (it "names the checked-out branch"
    (let ((dir (make-temp-file "branch-test" t)))
      (unwind-protect
          (progn (elpaca-local-tests--git-repo dir "test-branch")
                 (expect (elpaca-local--branch dir) :to-equal "test-branch"))
        (delete-directory dir t))))

  (it "is nil where there is no git worktree"
    (let ((dir (make-temp-file "no-git" t)))
      (unwind-protect
          (expect (elpaca-local--branch dir) :to-be nil)
        (delete-directory dir t)))))

(describe "elpaca-local--skip-lines"
  (it "pads the names to the widest of the set"
    (cl-letf (((symbol-function 'elpaca-local--branch) (lambda (_d) "main")))
      (expect (elpaca-local--skip-lines
               '((gptel-anthropic-oauth . "/x/modules/ai/gptel-anthropic-oauth/")
                 (occult . "/x/occult.el/")))
              :to-equal
              (list "gptel-anthropic-oauth - /x/modules/ai/gptel-anthropic-oauth"
                    (concat "occult" (make-string 16 ?\s) "- /x/occult.el")))))

  (it "lines the branches up behind the widest branch-carrying path"
    (cl-letf (((symbol-function 'elpaca-local--branch)
               (lambda (dir) (if (string-match-p "hammerspoon" dir) "fix/ready" "topic"))))
      (expect (elpaca-local--skip-lines
               '((spacehammer . "/x/.hammerspoon/") (pdf-text . "/x/GitHub/pdf-text/")))
              :to-equal
              '("spacehammer - /x/.hammerspoon    -- fix/ready"
                "pdf-text    - /x/GitHub/pdf-text -- topic"))))

  (it "keeps main, master and a source without git free of a branch column"
    (cl-letf (((symbol-function 'elpaca-local--branch)
               (lambda (dir) (cond ((string-match-p "prisma" dir) "main")
                                   ((string-match-p "reddigg" dir) "master")))))
      (expect (elpaca-local--skip-lines
               '((prisma . "/x/prisma.el/") (reddigg . "/x/emacs-reddigg/")
                 (gptel-tools . "/x/modules/ai/gptel-tools/")))
              :to-equal
              '("prisma      - /x/prisma.el"
                "reddigg     - /x/emacs-reddigg"
                "gptel-tools - /x/modules/ai/gptel-tools"))))

  (it "has nothing to say about an empty set"
    (expect (elpaca-local--skip-lines nil) :to-be nil)))

(describe "elpaca-local--skip-merge-a"
  (it "leaves a build-in-place source alone"
    (let (merged)
      (cl-letf (((symbol-function 'elpaca-get) (lambda (id) id))
                ((symbol-function 'elpaca-local-package-p) (lambda (_e) t)))
        (elpaca-local--skip-merge-a
         (lambda (&rest args) (push args merged)) 'pdf-text 'fetch nil))
      (expect merged :to-be nil)))

  (it "passes a package elpaca clones straight through, arguments intact"
    (let (merged)
      (cl-letf (((symbol-function 'elpaca-get) (lambda (id) id))
                ((symbol-function 'elpaca-local-package-p) (lambda (_e) nil)))
        (elpaca-local--skip-merge-a
         (lambda (&rest args) (push args merged)) 'gptel 'fetch t))
      (expect merged :to-equal '((gptel fetch t)))))

  (it "merges an id elpaca does not know, leaving the error to elpaca"
    (let (merged)
      (cl-letf (((symbol-function 'elpaca-get) (lambda (_id) nil)))
        (elpaca-local--skip-merge-a
         (lambda (&rest args) (push args merged)) 'unknown))
      (expect merged :to-equal '((unknown nil nil))))))

(describe "elpaca-local-update-remotes"
  ;; The advice rides elpaca's own merge-all loop rather than reimplementing
  ;; it, so it must be gone again the moment the loop returns.
  (it "advises the merge for the run and removes it afterwards"
    (let (advised-during)
      (elpaca-local-tests--with-queue nil
        (cl-letf (((symbol-function 'elpaca-update-all)
                   (lambda (&optional _i)
                     (setq advised-during
                           (advice-member-p #'elpaca-local--skip-merge-a 'elpaca-merge)))))
          (elpaca-local-update-remotes)))
      (expect advised-during :to-be-truthy)
      (expect (advice-member-p #'elpaca-local--skip-merge-a 'elpaca-merge) :to-be nil)))

  (it "removes the advice even when the update signals"
    (elpaca-local-tests--with-queue nil
      (cl-letf (((symbol-function 'elpaca-update-all)
                 (lambda (&optional _i) (error "boom"))))
        (expect (elpaca-local-update-remotes) :to-throw 'error)))
    (expect (advice-member-p #'elpaca-local--skip-merge-a 'elpaca-merge) :to-be nil))

  (it "updates only the named packages when given a list"
    (let (updated)
      (elpaca-local-tests--with-queue nil
        (cl-letf (((symbol-function 'elpaca-update)
                   (lambda (id &optional i) (push (cons id i) updated)))
                  ((symbol-function 'elpaca-update-all)
                   (lambda (&optional _i) (error "should not update everything"))))
          (elpaca-local-update-remotes '(gptel magit) t)))
      (expect (nreverse updated) :to-equal '((gptel . t) (magit . t)))))

  ;; The block is printed from the queue before anything runs, so it arrives in
  ;; one aligned piece instead of trickling in among the fetch lines.
  (it "prints the aligned block up front and returns the ids it skipped"
    (let (lines skipped)
      (elpaca-local-tests--with-queue '((prisma . prisma-e) (gptel . gptel-e)
                                        (pdf-text . pdf-text-e))
        (cl-letf (((symbol-function 'elpaca-local-package-p)
                   (lambda (e) (memq e '(prisma-e pdf-text-e))))
                  ((symbol-function 'elpaca<-source-dir)
                   (lambda (e) (if (eq e 'prisma-e) "/x/prisma.el/" "/x/pdf-text/")))
                  ((symbol-function 'elpaca-local--branch)
                   (lambda (dir) (if (string-match-p "pdf-text" dir) "mupdf-backend" "main")))
                  ((symbol-function 'elpaca-update-all)
                   (lambda (&optional _i) (push "update ran" lines))))
          (setq skipped (elpaca-local-update-remotes
                         nil nil (lambda (fmt &rest args)
                                   (push (apply #'format fmt args) lines))))))
      (expect skipped :to-equal '(prisma pdf-text))
      (expect (nreverse lines)
              :to-equal '("prisma   - /x/prisma.el"
                          "pdf-text - /x/pdf-text -- mupdf-backend"
                          "update ran")))))

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
