;;; tests/elisp/evilify-edebug-tests.el --- elisp/autoload/evilify-edebug.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; evil-collection isn't installed in the batch tier - stub the feature;
;; edebug is built-in and loads for real. Live keybinding behavior is
;; probe/live territory.
(provide 'evil-collection)

(load-module-file "modules/elisp/autoload/evilify-edebug.el")

(describe "evilify-edebug-setup"
  (it "sets the initial state and lays down the step bindings"
    (let (initial-state bindings)
      (cl-letf (((symbol-function 'evil-set-initial-state)
                 (lambda (mode state) (setq initial-state (cons mode state))))
                ((symbol-function 'evil-collection-define-key)
                 (lambda (_state _map &rest keys)
                   (setq bindings (append bindings keys)))))
        (let ((edebug-mode-hook nil))
          (evilify-edebug-setup)
          (expect initial-state :to-equal '(edebug-mode . normal))
          (expect (memq 'evil-normalize-keymaps edebug-mode-hook) :to-be-truthy))
        (expect (plist-get bindings "s" #'equal) :to-be 'edebug-step-mode)
        (expect (plist-get bindings "q" #'equal) :to-be 'top-level)))))
