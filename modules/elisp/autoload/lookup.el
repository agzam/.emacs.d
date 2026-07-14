;;; modules/elisp/autoload/lookup.el --- K documentation via helpful -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of Doom's +emacs-lisp-lookup-documentation, minus its Doom-module
;; help branch (no such concept here).  config.el registers it as the
;; :documentation handler for the elisp/helpful modes, so K opens the
;; symbol's helpful buffer inside Emacs instead of an online search.
;;; Code:

;;;###autoload
(defun elisp-lookup-documentation (thing)
  "Show help for THING (a symbol-name string) with helpful, inside Emacs.
Uses `helpful-symbol' for a named symbol (`describe-symbol' for a class),
and `helpful-at-point' when nothing is named.  A `lookup-documentation'
:documentation handler, so it returns non-nil/moves point on success."
  (cond (thing
         (let ((sym (intern thing)))
           (if (and (not (cl-find-class sym))
                    (fboundp 'helpful-symbol))
               (helpful-symbol sym)
             (describe-symbol sym)
             (pop-to-buffer (help-buffer)))))
        ((call-interactively
          (if (fboundp 'helpful-at-point)
              #'helpful-at-point
            #'describe-symbol)))))

;;; lookup.el ends here
