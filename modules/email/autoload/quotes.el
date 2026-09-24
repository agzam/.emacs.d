;;; modules/email/autoload/quotes.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Every line that starts with `>' gets a face by its quote depth, with
;; the faces and the depth rule message-mode uses in a reply, so reading
;; and answering look alike.  The faces are text properties because the
;; thread view copies each rendered body as text, which drops overlays.
;;; Code:

(require 'message)

(defun mail-quote-face (depth)
  "Face for a line quoted DEPTH times; past four, the faces start over."
  (nth (mod (1- depth) 4)
       '(message-cited-text-1 message-cited-text-2
         message-cited-text-3 message-cited-text-4)))

;;;###autoload
(defun highlight-mail-quotes ()
  "Face every line that starts with `>' by its depth, markers included.
Faces already on the text, such as links, keep priority."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*>[ \t>]*" nil t)
      (add-face-text-property
       (match-beginning 0) (line-end-position)
       (mail-quote-face (funcall message-cite-level-function (match-string 0)))
       t))))

;;; quotes.el ends here
