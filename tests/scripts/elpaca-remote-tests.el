;;; tests/scripts/elpaca-remote-tests.el --- clone-vs-recipe heal specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "scripts/elpaca-remote.el")

;; Elpaca is absent from the buttercup sandbox: the struct accessors and
;; queue entry points the heals reach for get inert stubs the specs override
;; via `cl-letf'.  `elpaca--queued' gets none - the report suite asserts on
;; its absence - so each spec that needs it binds it and unbinds it after.
(unless (fboundp 'elpaca<-source-dir) (defun elpaca<-source-dir (_e) nil))
(unless (fboundp 'elpaca<-recipe) (defun elpaca<-recipe (_e) nil))
(unless (fboundp 'elpaca<-status) (defun elpaca<-status (_e) nil))
(unless (fboundp 'elpaca<-current-step) (defun elpaca<-current-step (_e) nil))
(unless (fboundp 'elpaca-merge) (defun elpaca-merge (_id &optional _fetch _interactive) nil))
(unless (fboundp 'elpaca-process-queues) (defun elpaca-process-queues (&optional _filter) nil))
(defvar elpaca-sources-directory)

(defmacro elpaca-remote-tests--with-queue (queue &rest body)
  "Run BODY with `elpaca--queued' answering QUEUE, leaving it unbound after."
  (declare (indent 1))
  `(let ((bound (fboundp 'elpaca--queued)))
     (unwind-protect
         (cl-letf (((symbol-function 'elpaca--queued) (lambda () ,queue)))
           ,@body)
       (unless bound (fmakunbound 'elpaca--queued)))))

(defun elpaca-remote-tests--git (dir &rest args)
  "Run git ARGS in DIR with a fixed identity; return trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (apply #'call-process "git" nil t nil
             "-c" "user.name=t" "-c" "user.email=t@example"
             "-c" "commit.gpgsign=false" args)
      (string-trim (buffer-string)))))

(defun elpaca-remote-tests--commit (dir subject)
  "Commit a change to file f in DIR under SUBJECT; return the short sha."
  (write-region (concat subject "\n") nil (expand-file-name "f" dir) t 'silent)
  (elpaca-remote-tests--git dir "add" "-A")
  (elpaca-remote-tests--git dir "commit" "-q" "-m" subject)
  (elpaca-remote-tests--git dir "rev-parse" "--short" "HEAD"))

(defun elpaca-remote-tests--upstream-and-clone (root)
  "Under ROOT, an upstream repo with two commits and a clone of it.
Return (UPSTREAM CLONE)."
  (let ((upstream (expand-file-name "upstream" root))
        (clone (expand-file-name "sources/pkg" root)))
    (make-directory upstream t)
    (make-directory (file-name-directory clone) t)
    (elpaca-remote-tests--git upstream "init" "-q" "-b" "master")
    (elpaca-remote-tests--commit upstream "one")
    (elpaca-remote-tests--commit upstream "two")
    (elpaca-remote-tests--git root "clone" "-q" upstream clone)
    (list upstream clone)))

(defun elpaca-remote-tests--rewrite-upstream (upstream)
  "Drop UPSTREAM's last commit and add another - a force-pushed history."
  (elpaca-remote-tests--git upstream "reset" "-q" "--hard" "HEAD~1")
  (elpaca-remote-tests--commit upstream "two-rewritten"))

(describe "elpaca-remote--config-origin-url"
  (it "reads origin's url among other remotes"
    (expect (elpaca-remote--config-origin-url
             "[core]\n\tbare = false\n[remote \"fork\"]\n\turl = https://f/x.git\n[remote \"origin\"]\n\turl = https://o/x.git\n\tfetch = +refs/heads/*:refs/remotes/origin/*\n[branch \"master\"]\n\tremote = origin\n")
            :to-equal "https://o/x.git"))
  (it "does not read past origin's section"
    (expect (elpaca-remote--config-origin-url
             "[remote \"origin\"]\n\tfetch = +refs/heads/*:refs/remotes/origin/*\n[remote \"fork\"]\n\turl = https://f/x.git\n")
            :to-be nil))
  (it "is nil without an origin"
    (expect (elpaca-remote--config-origin-url "[core]\n\tbare = false\n") :to-be nil)))

(describe "elpaca-remote-origin-url"
  (it "reads a real clone's origin without spawning git"
    (let ((root (make-temp-file "elpaca-remote" t)))
      (unwind-protect
          (pcase-let ((`(,upstream ,clone) (elpaca-remote-tests--upstream-and-clone root)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest _) (error "no process allowed here"))))
              (expect (elpaca-remote-origin-url clone) :to-equal upstream)))
        (delete-directory root t))))
  (it "is nil for a directory that is no clone"
    (let ((root (make-temp-file "elpaca-remote" t)))
      (unwind-protect
          (expect (elpaca-remote-origin-url root) :to-be nil)
        (delete-directory root t)))))

(describe "elpaca-remote-sync-origins"
  (let (root clone)
    (before-each
      (setq root (make-temp-file "elpaca-remote" t)
            clone (cadr (elpaca-remote-tests--upstream-and-clone root))))
    (after-each (delete-directory root t))

    (it "re-points origin to the recipe's url, reports it and returns the id"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root))
            out)
        (elpaca-remote-tests--with-queue '((pkg . e))
          (cl-letf (((symbol-function 'elpaca<-source-dir) (lambda (_e) clone))
                    ((symbol-function 'elpaca<-recipe)
                     (lambda (_e) '(:package "pkg" :host github :repo "mirror/pkg")))
                    ((symbol-function 'elpaca-git--repo-uri)
                     (lambda (_recipe) "https://github.com/mirror/pkg.git")))
            (expect (elpaca-remote-sync-origins
                     nil (lambda (fmt &rest args) (push (apply #'format fmt args) out)))
                    :to-equal '(pkg))))
        (expect (elpaca-remote-tests--git clone "remote" "get-url" "origin")
                :to-equal "https://github.com/mirror/pkg.git")
        (expect (car out) :to-match "^remote: pkg origin .* -> https://github.com/mirror/pkg.git$")))

    (it "leaves a clone alone whose origin already matches"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root))
            (upstream (elpaca-remote-tests--git clone "remote" "get-url" "origin")))
        (elpaca-remote-tests--with-queue '((pkg . e))
          (cl-letf (((symbol-function 'elpaca<-source-dir) (lambda (_e) clone))
                    ((symbol-function 'elpaca<-recipe) (lambda (_e) '(:package "pkg")))
                    ((symbol-function 'elpaca-git--repo-uri) (lambda (_recipe) upstream)))
            (expect (elpaca-remote-sync-origins) :to-be nil)))
        (expect (elpaca-remote-tests--git clone "remote" "get-url" "origin")
                :to-equal upstream)))

    (it "skips recipes that manage their own remotes, and build-in-place sources"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root)))
        (elpaca-remote-tests--with-queue '((pkg . e) (local . l))
          (cl-letf (((symbol-function 'elpaca<-source-dir)
                     (lambda (e) (if (eq e 'e) clone (expand-file-name "upstream/" root))))
                    ((symbol-function 'elpaca<-recipe)
                     (lambda (e) (if (eq e 'e)
                                     '(:package "pkg" :remotes ("fork" :repo "me/pkg"))
                                   '(:package "local"))))
                    ((symbol-function 'elpaca-git--repo-uri) (lambda (_recipe) "https://elsewhere/x.git")))
            (expect (elpaca-remote-sync-origins) :to-be nil)))))

    (it "honors the package filter"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root)))
        (elpaca-remote-tests--with-queue '((pkg . e))
          (cl-letf (((symbol-function 'elpaca<-source-dir) (lambda (_e) clone))
                    ((symbol-function 'elpaca<-recipe) (lambda (_e) '(:package "pkg")))
                    ((symbol-function 'elpaca-git--repo-uri) (lambda (_recipe) "https://elsewhere/x.git")))
            (expect (elpaca-remote-sync-origins '(other)) :to-be nil)))))))

(describe "elpaca-remote-divergence"
  (let (root upstream clone)
    (before-each
      (setq root (make-temp-file "elpaca-remote" t))
      (pcase-let ((`(,u ,c) (elpaca-remote-tests--upstream-and-clone root)))
        (setq upstream u clone c)))
    (after-each (delete-directory root t))

    (it "describes a clone left behind by a rewritten upstream"
      (let ((old (elpaca-remote-tests--git clone "rev-parse" "--short" "HEAD"))
            (new (elpaca-remote-tests--rewrite-upstream upstream)))
        (elpaca-remote-tests--git clone "fetch" "-q")
        (let ((div (elpaca-remote-divergence clone)))
          (expect (plist-get div :branch) :to-equal "master")
          (expect (plist-get div :upstream) :to-equal "origin/master")
          (expect (plist-get div :old) :to-equal old)
          (expect (plist-get div :new) :to-equal new)
          (expect (plist-get div :dropped) :to-equal 1))))

    (it "is nil when upstream merely moved ahead (a fast-forward exists)"
      (elpaca-remote-tests--commit upstream "three")
      (elpaca-remote-tests--git clone "fetch" "-q")
      (expect (elpaca-remote-divergence clone) :to-be nil))

    (it "is nil when the clone is in step with upstream"
      (expect (elpaca-remote-divergence clone) :to-be nil))

    (it "is nil when a tracked file is modified - a reset would destroy the edit"
      (elpaca-remote-tests--rewrite-upstream upstream)
      (elpaca-remote-tests--git clone "fetch" "-q")
      (write-region "edit\n" nil (expand-file-name "f" clone) t 'silent)
      (expect (elpaca-remote-divergence clone) :to-be nil))

    (it "ignores untracked files, which a reset leaves alone"
      (elpaca-remote-tests--rewrite-upstream upstream)
      (elpaca-remote-tests--git clone "fetch" "-q")
      (write-region "" nil (expand-file-name "stray" clone) nil 'silent)
      (expect (elpaca-remote-divergence clone) :not :to-be nil))

    (it "is nil on a detached HEAD"
      (elpaca-remote-tests--rewrite-upstream upstream)
      (elpaca-remote-tests--git clone "fetch" "-q")
      (elpaca-remote-tests--git clone "checkout" "-q" "--detach")
      (expect (elpaca-remote-divergence clone) :to-be nil))))

(describe "elpaca-remote-reset-diverged"
  (let (root upstream clone)
    (before-each
      (setq root (make-temp-file "elpaca-remote" t))
      (pcase-let ((`(,u ,c) (elpaca-remote-tests--upstream-and-clone root)))
        (setq upstream u clone c)))
    (after-each (delete-directory root t))

    (it "resets a merge-failed diverged clone onto upstream and re-merges it"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root))
            (new (elpaca-remote-tests--rewrite-upstream upstream))
            merged processed out)
        (elpaca-remote-tests--git clone "fetch" "-q")
        (elpaca-remote-tests--with-queue '((pkg . e))
          (cl-letf (((symbol-function 'elpaca<-status) (lambda (_e) 'failed))
                    ((symbol-function 'elpaca<-current-step) (lambda (_e) 'elpaca-git--merge))
                    ((symbol-function 'elpaca<-source-dir) (lambda (_e) clone))
                    ((symbol-function 'elpaca-merge) (lambda (id &rest _) (push id merged)))
                    ((symbol-function 'elpaca-process-queues) (lambda (&rest _) (setq processed t))))
            (expect (elpaca-remote-reset-diverged
                     (lambda (fmt &rest args) (push (apply #'format fmt args) out)))
                    :to-equal '(pkg))))
        (expect (elpaca-remote-tests--git clone "rev-parse" "--short" "HEAD") :to-equal new)
        (expect merged :to-equal '(pkg))
        (expect processed :to-be t)
        (expect (car out)
                :to-match "^reset (diverged): pkg master [0-9a-f]+ -> origin/master [0-9a-f]+, 1 dropped commit(s) stay in the reflog$")))

    (it "leaves a package that failed in another step alone"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root))
            (old (elpaca-remote-tests--git clone "rev-parse" "--short" "HEAD"))
            processed)
        (elpaca-remote-tests--rewrite-upstream upstream)
        (elpaca-remote-tests--git clone "fetch" "-q")
        (elpaca-remote-tests--with-queue '((pkg . e))
          (cl-letf (((symbol-function 'elpaca<-status) (lambda (_e) 'failed))
                    ((symbol-function 'elpaca<-current-step) (lambda (_e) 'elpaca-git--fetch))
                    ((symbol-function 'elpaca<-source-dir) (lambda (_e) clone))
                    ((symbol-function 'elpaca-process-queues) (lambda (&rest _) (setq processed t))))
            (expect (elpaca-remote-reset-diverged) :to-be nil)))
        (expect (elpaca-remote-tests--git clone "rev-parse" "--short" "HEAD") :to-equal old)
        (expect processed :to-be nil)))

    (it "leaves a merge failure that is no divergence alone"
      (let ((elpaca-sources-directory (expand-file-name "sources/" root))
            (old (elpaca-remote-tests--git clone "rev-parse" "--short" "HEAD")))
        (elpaca-remote-tests--with-queue '((pkg . e))
          (cl-letf (((symbol-function 'elpaca<-status) (lambda (_e) 'failed))
                    ((symbol-function 'elpaca<-current-step) (lambda (_e) 'elpaca-git--merge))
                    ((symbol-function 'elpaca<-source-dir) (lambda (_e) clone)))
            (expect (elpaca-remote-reset-diverged) :to-be nil)))
        (expect (elpaca-remote-tests--git clone "rev-parse" "--short" "HEAD") :to-equal old)))))

;;; tests/scripts/elpaca-remote-tests.el ends here
