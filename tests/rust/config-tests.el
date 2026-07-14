;;; tests/rust/config-tests.el --- rust module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar rustic-lsp-setup-p)
(defvar rustic-mode-hook)

(defvar rust-tests--packages nil)

(defun rust-tests--expand-use-package (name &rest args)
  "Stub `use-package' expander: record NAME, run its :init/:config forms."
  `(progn (push ',name rust-tests--packages)
          ,@(apply #'use-package-body-forms args '(:init :config))))

(defun rust-tests--load ()
  "Load modules/rust/config.el with `use-package' reduced to side effects."
  (setq rust-tests--packages nil
        rustic-lsp-setup-p 'unset
        rustic-mode-hook nil)
  (cl-letf (((symbol-function 'use-package)
             (cons 'macro #'rust-tests--expand-use-package)))
    (load-module-file "modules/rust/config.el")))

(describe "rust module config"
  (before-each (rust-tests--load))

  (it "installs rustic and not a redundant rust-mode (it is a rustic dep)"
    (expect (memq 'rustic rust-tests--packages) :to-be-truthy)
    (expect (memq 'rust-mode rust-tests--packages) :to-be nil))

  (it "disables rustic's own lsp setup so lsp! is the single entry"
    (expect rustic-lsp-setup-p :to-be nil))

  (it "starts lsp on rustic-mode through lsp!"
    (expect (memq 'lsp! rustic-mode-hook) :to-be-truthy)))
