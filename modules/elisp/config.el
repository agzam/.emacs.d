;;; modules/elisp/config.el --- elisp dev: eval, edebug, profiler, info -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/elisp.  Deviations:
;; - let-plist + evilify-edebug folded into autoload/ (in-module :local-repo
;;   packages under straight; the loaddefs macro-autoload also heals
;;   general's bare let-plist uses).
;; - macrostep added explicitly (doom.d got it from Doom's :lang emacs-lisp);
;;   eros likewise (:tools eval +overlay) - the eval helpers render through
;;   its overlays.
;; - Renames: sp-eval-current-sexp -> eval-current-sexp (user fn, not
;;   smartparens's), with-editor-eval -> eval-current-with-log (rebuilt: the
;;   doom.d body called eval-current-form-sp, void there - rot),
;;   edebug-eval-current-form-sp -> edebug-eval-current-sexp.
;; - elisp-format behind :commands (doom.d demand-loaded it - :after against
;;   the preloaded elisp-mode fires at startup).
;; - flycheck scratch hook fboundp-guarded (:checkers unported);
;;   doom-scratch-buffer-created-hook -> scratch-buffer-created-hook.
;; - profiler-report hook consolidated into one named fn; yas disable
;;   guarded so it doesn't load yasnippet just to turn it off.
;;; Code:

(use-package macrostep
  :defer t)

(use-package eros
  :hook (emacs-lisp-mode . eros-mode))

;; REPL-convenience library (a-get and friends); load by hand when needed
(use-package a
  :defer t)

(use-package elisp-format
  :ensure (elisp-format :host github :repo "Yuki-Inoue/elisp-format")
  :commands (elisp-format-buffer elisp-format-region elisp-format-file))

(map! :map profiler-report-mode-map
      :n "RET" #'profiler-report-helpful-symbol-at-point
      "M-l" #'profiler-report-expand-entry
      "M-h" #'profiler-report-collapse-entry
      "M-j" #'profiler-report-next-entry
      "M-k" #'profiler-report-previous-entry)

(add-hook! 'profiler-report-mode-hook
  (defun profiler-report-setup-h ()
    (when (bound-and-true-p yas-minor-mode) (yas-minor-mode -1))
    (run-with-timer 0.1 nil #'profiler-report-expand-all)))

(after! edebug
  (setopt edebug-print-level nil
          edebug-print-length nil
          edebug-save-windows nil)
  (after! evil-collection
    (evilify-edebug-setup))
  (map! :map edebug-mode-map
        :localleader
        "ec" #'edebug-eval-current-sexp))

(after! elisp-mode
  ;; K -> the symbol's helpful buffer, in Emacs (not the online fallback).
  (set-lookup-handlers! '(emacs-lisp-mode lisp-interaction-mode lisp-data-mode
                          helpful-mode inferior-emacs-lisp-mode)
    :documentation #'elisp-lookup-documentation)
  (add-hook! 'scratch-buffer-created-hook
    (defun flycheck-off-h ()
      ;; fboundp: flycheck waits on :checkers
      (when (fboundp 'flycheck-mode) (flycheck-mode -1))))
  (map! :localleader
        :map (emacs-lisp-mode-map
              lisp-data-mode-map
              lisp-interaction-mode-map)
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("d" . "debug")
                 "f" #'edebug-instrument-defun-on
                 "F" #'edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
                 "c" #'eval-current-sexp
                 "b" #'eval-buffer
                 "d" #'eval-defun
                 "l" #'eval-last-sexp
                 "i" #'eval-current-with-log
                 "r" #'eval-region
                 "L" #'load-library
                 "p" #'pp-eval-current
                 ";" #'eval-print-last-sexp)
        (:prefix ("g" . "goto")
                 "f" #'find-function
                 "v" #'find-variable
                 "l" #'elisp-fully-qualified-symbol-with-gh-link
                 "d" #'xref-find-definitions
                 "r" #'xref-find-references
                 "D" #'xref-find-definitions-other-window)
        (:prefix ("h" . "help")
                 "h" #'helpful-at-point)
        (:prefix ("k" . "kill")
                 "m" #'erase-messages-buffer)
        (:prefix ("s" . "repl")
         :desc "messages" "s" #'switch-to-messages-buffer-other-window
         :desc "clear " "l" #'erase-messages-buffer
         :desc "hide" "k" #'hide-messages-window))
  (map! :map emacs-lisp-mode-map
        :g "C-c C-f" nil ; unbind elisp-byte-compile-file
        :i "#" #'sharp-quote)

  (map! :localleader
        :map messages-buffer-mode-map
        (:prefix ("k" . "kill")
                 "m" #'erase-messages-buffer)
        (:prefix ("s" . "repl")
         :desc "clear" "l" #'erase-messages-buffer
         :desc "back to elisp" "s" #'switch-to-last-elisp-buffer
         :desc "hide" "k" #'hide-messages-window))

  (map! :map lisp-interaction-mode-map
        :i "C-j" #'eval-print-last-sexp)

  (add-hook! 'emacs-lisp-mode-hook
             #'visual-wrap-prefix-mode
             (defun always-lexical-binding-h ()
               (setq lexical-binding t))))

(after! debug
  (map! :map debugger-mode-map
        :n "e" #'debugger-eval-expression
        :n "n" #'backtrace-forward-frame
        :n "p" #'backtrace-backward-frame
        :n "v" #'backtrace-toggle-locals))

(after! info
  (map! :map Info-mode-map
        :n "C-j" #'Info-goto-node
        :n "^" #'Info-up
        :n "H" #'Info-history-back
        :n "L" #'Info-history-forward
        :n "C-<return>" #'Info-follow-nearest-node-new-window
        :n "n" #'Info-search-next
        :n "N" #'Info-search-backward
        :localleader
        "y" #'info-copy-node-url
        "w" #'Info-goto-node-web
        "g" #'Info-goto-node
        "s" #'Info-search
        "i" #'Info-index
        "h" #'Info-history
        "d" #'Info-directory))

(add-to-list
 'display-buffer-alist
 `("\\*Backtrace\\*"
   (display-buffer-reuse-window
    display-buffer-reuse-mode-window
    display-buffer-in-quadrant)
   (direction . right)
   (window . root)))

(after! woman
  (setopt woman-manpath '("/Applications/kitty.app/Contents/Resources/man/"
                          "/usr/share/man"
                          "/opt/homebrew/share/man"
                          "/usr/X11/man"
                          "/Library/Apple/usr/share/man")))

;; the helpful package itself rides the root layer (its SPC h bindings live
;; there); this advice is the elisp-module half of that story
(defadvice! helpful-for-describe-function-a (_fn &rest args)
  "Route `describe-function' through helpful (works in profiler, transients)."
  :around #'describe-function
  (apply #'helpful-symbol args))

;;; config.el ends here
