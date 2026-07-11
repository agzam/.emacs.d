;;; modules/elisp/autoload/let-plist.el --- let-alist for plists -*- lexical-binding: t; -*-
;;; Commentary:
;; Borrowed from https://emacs.stackexchange.com/questions/45581.  doom.d
;; carried it as an in-module :local-repo package (straight load-order
;; workaround); here the cookied macro rides the module loaddefs, which also
;; heals general's bare let-plist uses (url.el, misc.el) at call time.
;;; Code:

(require 'let-alist)

(defun let-plist--list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `plist-get' on VAR."
  `(plist-get ,(if (cdr list)
                   (let-plist--list-to-sexp (cdr list) var)
                 var)
              ',(intern (concat ":" (symbol-name (car list))))))

(defun let-plist--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (let-alist--remove-dot symbol))
         (name (symbol-name clean)))
    (if (string-match "\\`\\." name)
        clean
      (let-plist--list-to-sexp
       (mapcar #'intern (nreverse (split-string name "\\.")))
       variable))))

;;;###autoload
(defmacro let-plist (plist &rest body)
  "Let-bind dotted symbols to their values in PLIST and execute BODY.
Similar to `let-alist'."
  (declare (indent 1))
  (let ((var (make-symbol "plist")))
    `(let ((,var ,plist))
       (let ,(mapcar (lambda (x) `(,(car x) ,(let-plist--access-sexp (car x) var)))
                     (delete-dups (let-alist--deep-dot-search body)))
         ,@body))))

;;; let-plist.el ends here
