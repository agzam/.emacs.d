;;; tests/lisp/shell-env-tests.el --- shell environment import specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "lisp/shell-env.el")

;; exec-path-from-shell is absent in --batch; define the entry point to stub.
(unless (fboundp 'exec-path-from-shell-initialize)
  (defun exec-path-from-shell-initialize (&rest _) nil))

(describe "shell-environment-incomplete-p"
  (it "is non-nil when rg is not on exec-path"
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (expect (shell-environment-incomplete-p) :to-be t)))
  (it "is nil when rg resolves"
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/rg")))
      (expect (shell-environment-incomplete-p) :to-be nil))))

(describe "import-shell-environment"
  (it "initializes exec-path-from-shell with -l -i when PATH is short"
    (let ((args 'unset)
          (initialized nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                ((symbol-function 'exec-path-from-shell-initialize)
                 (lambda (&rest _)
                   (setq initialized t
                         args exec-path-from-shell-arguments))))
        (with-fake-feature 'exec-path-from-shell
          (import-shell-environment)))
      (expect initialized :to-be t)
      (expect args :to-equal '("-l" "-i"))))

  (it "is a no-op when rg already resolves"
    (let ((called nil))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _) "/opt/homebrew/bin/rg"))
                ((symbol-function 'exec-path-from-shell-initialize)
                 (lambda (&rest _) (setq called t))))
        (import-shell-environment))
      (expect called :to-be nil))))

;;; tests/lisp/shell-env-tests.el ends here