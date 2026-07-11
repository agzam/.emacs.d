;;; modules/elisp/autoload/eval.el --- current-sexp eval helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; eval-current-with-log was doom.d's with-editor-eval; rebuilt - its body
;; called eval-current-form-sp, void in the live install (rot), and the
;; with-editor- name collided with the real with-editor package.
;; edebug-instrument-defun-on/off vendored from Doom :lang emacs-lisp.
;;; Code:

;;;###autoload
(defun eval-current-sexp ()
  "Eval the sexp enclosing point, rendering through eros."
  (interactive)
  (save-excursion
    (let ((evil-move-beyond-eol t))
      (when (looking-at-p "[[({]")
        (forward-char))
      (sp-up-sexp)
      (call-interactively 'eros-eval-last-sexp))))

;;;###autoload
(defun pp-eval-current ()
  "Like `pp-eval-last-sexp', but for the sexp enclosing point."
  (interactive)
  ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
  (let ((evil-move-beyond-eol t))
    (save-excursion
      (goto-char
       (plist-get (or (sp-get-enclosing-sexp)
                      (sp-get-expression))
                  :end))
      (call-interactively 'pp-eval-last-sexp))))

;; http://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
;;;###autoload
(defun sharp-quote ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

;;;###autoload
(defun eval-current-with-log ()
  "Eval the sexp enclosing point and pop its *Messages* output in a buffer."
  (interactive)
  (let ((last-pos (with-current-buffer (messages-buffer) (point-max))))
    (eval-current-sexp)
    (let ((log (with-current-buffer (messages-buffer)
                 (buffer-substring last-pos (point-max)))))
      (with-current-buffer (get-buffer-create "*eval*")
        (erase-buffer)
        (insert log)
        (switch-to-buffer-other-window (current-buffer))))))

;;;###autoload
(defun edebug-instrument-defun-on ()
  "Instrument the top-level defun at point for edebug."
  (interactive)
  (eval-defun 'edebugit))

;;;###autoload
(defun edebug-instrument-defun-off ()
  "Remove edebug instrumentation from the top-level defun at point."
  (interactive)
  (eval-defun nil))

;;; eval.el ends here
