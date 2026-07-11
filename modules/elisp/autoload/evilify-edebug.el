;;; modules/elisp/autoload/evilify-edebug.el --- evil bindings for edebug -*- lexical-binding: t; -*-
;;; Commentary:
;; Based on emacs-evil/evil-collection's edebug module (James Nguyen):
;; https://github.com/emacs-evil/evil-collection/blob/master/modes/edebug/evil-collection-edebug.el
;; doom.d carried it as an in-module :local-repo package; a lazy autoload
;; file does the job - setup is called from config.el's after! edebug, when
;; both requires below already resolve.
;;; Code:

(require 'edebug)
(require 'evil-collection)

;;;###autoload
(defun evilify-edebug-setup ()
  "Set up evil bindings for edebug."
  (evil-set-initial-state 'edebug-mode 'normal)

  (add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key nil 'edebug-mode-map
    "g" nil
    "G" nil
    "h" nil
    "v" nil)

  (evil-collection-define-key 'normal 'edebug-mode-map
    ;; control
    "v" nil
    "s" 'edebug-step-mode
    "\C-n" 'edebug-next-mode
    "go" 'edebug-go-mode
    "gO" 'edebug-Go-nonstop-mode
    "t" 'edebug-trace-mode
    "T" 'edebug-Trace-fast-mode
    "c" 'edebug-continue-mode
    "C" 'edebug-Continue-fast-mode

    "f" 'edebug-forward-sexp
    "H" 'edebug-goto-here
    "I" 'edebug-instrument-callee
    "\C-i" 'edebug-step-in
    "o" 'edebug-step-out

    ;; quit
    "q" 'top-level
    "Q" 'edebug-top-level-nonstop
    "a" 'abort-recursive-edit
    "S" 'edebug-stop

    ;; breakpoints
    "b" 'edebug-set-breakpoint
    "u" 'edebug-unset-breakpoint
    "B" 'edebug-next-breakpoint
    "x" 'edebug-set-conditional-breakpoint
    "X" 'edebug-set-global-break-condition

    ;; evaluation
    "r" 'edebug-previous-result
    "e" 'edebug-eval-expression
    (kbd "C-x C-e") 'edebug-eval-last-sexp
    "EL" 'edebug-visit-eval-list

    ;; views
    "WW" 'edebug-where
    "p" 'edebug-bounce-point
    "P" 'edebug-view-outside ;; same as v
    "WS" 'edebug-toggle-save-windows

    ;; misc
    "g?" 'edebug-help
    "d" 'edebug-backtrace

    "-" 'negative-argument

    ;; statistics
    "=" 'edebug-temp-display-freq-count

    ;; GUD bindings
    (kbd "C-c C-s") 'edebug-step-mode
    (kbd "C-c C-n") 'edebug-next-mode
    (kbd "C-c C-c") 'edebug-go-mode

    (kbd "C-x SPC") 'edebug-set-breakpoint
    (kbd "C-c C-d") 'edebug-unset-breakpoint
    (kbd "C-c C-t") (cmd! (edebug-set-breakpoint t))
    (kbd "C-c C-l") 'edebug-where))

;;;###autoload
(defun edebug-eval-current-sexp ()
  "Like `eval-current-sexp' but inside an edebug session."
  (interactive)
  (let ((evil-move-beyond-eol t))
    (save-excursion
      (goto-char
       (plist-get (or (sp-get-enclosing-sexp)
                      (sp-get-expression))
                  :end))
      (call-interactively #'edebug-eval-last-sexp))))

;;; evilify-edebug.el ends here
