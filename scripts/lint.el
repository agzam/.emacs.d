;;; scripts/lint.el --- check-parens over files given as args -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -l scripts/lint.el FILE...

(require 'cl-lib)

(let ((files command-line-args-left)
      (failures 0))
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (condition-case err
          (check-parens)
        (error
         (cl-incf failures)
         (message "PARENS %s:%d: %s"
                  file (line-number-at-pos) (error-message-string err))))))
  (message "lint: %d file(s) checked, %d failure(s)" (length files) failures)
  (kill-emacs (if (zerop failures) 0 1)))
