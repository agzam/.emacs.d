;;; tests/completion/yasnippet-tests.el --- yasnippet helper specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/completion/autoload/yasnippet.el")

(describe "yas-completing-prompt-a"
  (it "hands the wrapped fn a completing-read without require-match"
    (let (got)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest args) (setq got args) "typed-freely")))
        (expect (yas-completing-prompt-a
                 (lambda (prompt choices _display-fn completing-fn)
                   (funcall completing-fn prompt choices nil t nil nil))
                 "Choose: " '("aa" "bb") nil nil)
                :to-equal "typed-freely"))
      ;; the stand-in drops require-match even when the caller passed t
      (expect (nth 3 got) :to-be nil))))

(describe "yas-expand-to-completion-h"
  (it "prepends yasnippet-capf buffer-locally"
    (with-temp-buffer
      (yas-expand-to-completion-h)
      (expect (car completion-at-point-functions) :to-be #'yasnippet-capf)
      (expect (local-variable-p 'completion-at-point-functions)
              :to-be-truthy))))

(describe "temporarily-disable-smart-parens"
  (it "is a no-op while smartparens isn't installed"
    (expect (temporarily-disable-smart-parens) :to-be nil))
  (it "toggles the mode off and schedules the re-enable when active"
    (defvar smartparens-mode)
    (let (off-called timer-args)
      (cl-letf (((symbol-function 'turn-off-smartparens-mode)
                 (lambda () (setq off-called t)))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest args) (setq timer-args args))))
        (with-temp-buffer
          (setq-local smartparens-mode t)
          (temporarily-disable-smart-parens)))
      (expect off-called :to-be-truthy)
      (expect (car timer-args) :to-equal 0.1)
      (expect (nth 2 timer-args) :to-be #'turn-on-smartparens-mode))))
