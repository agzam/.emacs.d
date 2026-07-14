;;; general/autoload/sexp-transient.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of doom.d lisp/sexp-transient.el.  A lazy general-module autoload
;; (mirrors expreg.el) instead of an eagerly loaded lisp/ lib: the prefix and
;; its helpers load on first SPC k.  smartparens is globally on by then; avy
;; and edit-indirect stay lazy: the avy sexp commands `require' avy at call
;; time (avy-jump carries no autoload cookie upstream, so it is void until avy
;; loads), edit-indirect loads when its command first runs - nothing here
;; hard-requires them at load, which keeps the file loadable in the bare -Q
;; batch test env.  avy's dynamic vars are forward-declared below so the let
;; bindings stay dynamic even when this file is byte-compiled before avy is
;; present.  The avy sexp orders register via
;; with-eval-after-load.  The shared transient-bypass-keys engine lives in
;; lisp/transient-bypass.el (required here, not duplicated).  Deviations from
;; doom.d: sp-eval-current-sexp -> eval-current-sexp (the elisp module's user
;; fn, not smartparens's); the dead transient--all-layout-keys is gone.
;;; Code:

(require 'transient)
(require 'transient-bypass)

;;;###autoload
(defun sp-evil-sexp-go-back ()
  "Find previous sexp."
  (interactive)
  (backward-char)
  (search-backward-regexp "[])}]\\|[[({]"))

;;;###autoload
(defun sp-evil-sexp-go-forward ()
  "Find next sexp."
  (interactive)
  (let* ((curr (point)))
    (forward-char)
    (unless (eq curr (search-forward-regexp "[[({]\\|[])}]"))
      (backward-char))))

;;;###autoload
(defun sp-narrow-to-current-sexp ()
  "Narrow screen to current sexp."
  (interactive)
  (save-mark-and-excursion
    (sp-beginning-of-sexp)
    (backward-char)
    (sp-mark-sexp)
    (narrow-to-region
     (region-beginning)
     (region-end))))

;;;###autoload
(defun sp-edit-indirect-current-sexp ()
  "Edit current sexp in an indirect buffer."
  (interactive)
  (let* ((reg (save-mark-and-excursion
               (sp-beginning-of-sexp)
               (backward-char)
               (sp-mark-sexp)
               (list (region-beginning)
                     (region-end))))
         (edit-indirect-guess-mode-function
          (lambda (pb _ _)
            (funcall (with-current-buffer pb major-mode)))))
    (funcall-interactively
     #'edit-indirect-region
     (car reg) (cadr reg) t)))

;; avy-jump is not autoloaded and avy's dynamic vars are only special once avy
;; loads; forward-declare them so the let bindings below bind dynamically even
;; when this file is byte-compiled before avy is present.
(defvar avy-command)
(defvar avy-style)
(defvar avy-all-windows)
(defvar avy-action)

;;;###autoload
(defun avy-goto-beg-sexp ()
  "Use avy to jump to the beginning of a sexp in the current window."
  (interactive)
  (require 'avy)
  (let ((avy-command this-command)  ; for look up in avy-orders-alist
        (avy-style 'post)
        (avy-all-windows nil)       ; scope to the current window only
        (avy-action nil))           ; land on the paren, not a leaked action
    (avy-jump "(+\\|\\[+\\|{+" :window-flip nil)))

;;;###autoload
(defun avy-goto-end-sexp ()
  "Use avy to jump to the end of a sexp in the current window."
  (interactive)
  (require 'avy)
  (let ((avy-command this-command)
        (avy-style 'post)
        (avy-all-windows nil)
        ;; let-bound so the :action below cannot leak into the next avy call
        (avy-action nil))
    (avy-jump "\\([^])}>]+\\)[])}]+"
              :window-flip nil
              :action (lambda (pt)
                        (goto-char pt)
                        (re-search-forward "[])}]+" nil t 1)))))

;; avy loads lazily (first jump); register the sexp orders once it is present.
(with-eval-after-load 'avy
  (add-to-list 'avy-orders-alist '(avy-goto-beg-sexp . avy-order-closest))
  (add-to-list 'avy-orders-alist '(avy-goto-end-sexp . avy-order-closest)))

;;;###autoload
(defun sp-eval-current-in-mode ()
  "Evals current sexp in its dedicated mode evaluator."
  (interactive)
  (cond
   ((derived-mode-p 'clojure-mode)
    (call-interactively #'cider-eval-sexp-at-point*))
   (t (call-interactively #'eval-current-sexp))))

(defun sp-pp-eval-current-in-mode ()
  "Eval & pretty-print sexp."
  (interactive)
  (cond
   ((derived-mode-p 'clojure-mode)
    (call-interactively #'cider-pprint-eval-sexp-at-point))
   (t (call-interactively #'pp-eval-current))))

;;;###autoload
(transient-define-prefix sexp-transient ()
  "rule the parens"
  ["Navigation"
   :hide always
   [("k" "k" sp-evil-sexp-go-back :transient t)
    ("j" "j" sp-evil-sexp-go-forward :transient t)
    ("h" "h" sp-backward-parallel-sexp :transient t)
    ("l" "l" sp-forward-parallel-sexp :transient t)
    ("<down>" "j" evil-next-visual-line :transient t)
    ("<up>" "k" evil-previous-visual-line :transient t)
    ("<left>" "h" evil-backward-char :transient t)
    ("<right>" "l" evil-forward-char :transient t)]]
  ["bypass keys"
   :class transient-column
   :hide always
   :setup-children
   (lambda (_)
     (transient-bypass-keys
      'sexp-transient
      '("p" "P" "C-;" "g" "G"
        "SPC" "," ":" "M-x" "M-:" "`"
        "s-k" "s-]" "s-j" "s-]"
        "[" "]"
        ("C-l" t) ("C-e" t) ("C-y" t)
        ("s" nil evil-surround-region)
        ("%" t)
        ("o" t evilmi-jump-items)
        ("0" t) ("$" t)
        ("f" t) ("F" t) ("T" t)
        ("/" t))))]
  ["sexp"
   [("a" "avy (" avy-goto-beg-sexp :transient t)
    ("A" "avy )" avy-goto-end-sexp :transient t)]
   [("w" "wrap" sp-wrap-sexp :transient t)
    ("W" "unwrap" sp-unwrap-sexp :transient t)
    ("=" "reindent" sp-reindent :transient t)]
   [("r" "raise" raise-sexp :transient t)
    ("c" "convolute" sp-convolute-sexp :transient t)
    ("t" "transpose" sp-transpose-sexp :transient t)]
   [("|" "split" sp-split-sexp :transient t)
    ("J" "join" sp-join-sexp :transient t)]
   [("n n" "narrow" sp-narrow-to-current-sexp :transient t)
    ("n w" "widen" widen :transient t)
    ("E" "edit" sp-edit-indirect-current-sexp :transient t)]
   [("M-l" "slurp" sp-forward-slurp-sexp :transient t)
    ("M-h" "barf" sp-forward-barf-sexp :transient t)
    ("M-S-h" "left slurp" sp-backward-slurp-sexp :transient t)
    ("M-S-l" "left barf" sp-backward-barf-sexp :transient t)]
   [("d x" "kill" sp-kill-sexp)
    ("y" "copy" sp-copy-sexp)
    ("v" "select" (lambda () (interactive)
                    (expreg-expand)
                    (expreg-transient)))
    ("u" "undo" evil-undo :transient t)]
   [("e c" "eval current" sp-eval-current-in-mode)
    ("e p" "pprint" sp-pp-eval-current-in-mode)
    ("e ;" "eval to comment"
     cider-pprint-eval-last-sexp-to-comment
     :if (lambda () (derived-mode-p 'clojure-mode)))
    ("#" "ignore" clojure-toggle-ignore
     :if (lambda () (derived-mode-p 'clojure-mode)))]]
  ["Clojure"
   :if (lambda () (derived-mode-p 'clojure-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("> SPC" "->" lsp-clojure-thread-first :transient t)
    (">>" "->>" lsp-clojure-thread-last :transient t)
    ("<" "un-thread" lsp-clojure-unwind-thread :transient t)
    ("; c" "wrap comment" clojure-wrap-rich-comment)]])

;;; sexp-transient.el ends here
