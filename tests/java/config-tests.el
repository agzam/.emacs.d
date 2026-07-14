;;; tests/java/config-tests.el --- java module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar java-ts-mode-hook)
(defvar java-mode-hook)
(defvar lsp-java-workspace-dir)

(defvar java-tests--packages nil)

(defun java-tests--expand-use-package (name &rest args)
  "Stub `use-package' expander: record NAME, run its :init/:config forms."
  `(progn (push ',name java-tests--packages)
          ,@(apply #'use-package-body-forms args '(:init :config))))

(defun java-tests--load ()
  "Load modules/java/config.el with `use-package' reduced to side effects."
  (setq java-tests--packages nil
        java-ts-mode-hook nil
        java-mode-hook nil)
  (cl-letf (((symbol-function 'use-package)
             (cons 'macro #'java-tests--expand-use-package)))
    (load-module-file "modules/java/config.el")))

(describe "java module config"
  (before-each (java-tests--load))

  (it "installs lsp-java"
    (expect (memq 'lsp-java java-tests--packages) :to-be-truthy))

  (it "starts lsp via lsp! on both the ts and non-ts java modes"
    (expect (memq 'lsp! java-ts-mode-hook) :to-be-truthy)
    (expect (memq 'lsp! java-mode-hook) :to-be-truthy)))

(describe "java state quarantine"
  (it "keeps lsp-java-workspace-dir under doom-data-dir"
    (expect lsp-java-workspace-dir :to-equal (concat doom-data-dir "java-workspace/"))
    (expect (string-prefix-p doom-data-dir lsp-java-workspace-dir) :to-be-truthy)))
