;;; modules/email/autoload/html.el -*- lexical-binding: t; -*-
;;; Commentary:
;; HTML mail through shr, set up for reading under any theme of the
;; rotation.  Paragraphs stay unfilled so the buffer wraps them at the
;; window edge, the way eww leaves them; the sender's colors give way to
;; the theme's; and each blockquote level is drawn as a `>' marker, so
;; quoted history reads and is faced like the quotes of plain mail.  The
;; shr settings are bound around each render, which leaves eww its own.
;;; Code:

(require 'mm-decode)
(require 'shr)

(defun mail-shr-tag-blockquote (dom)
  "Render the blockquote DOM as shr does and count it into the quote depth.
Each level adds one to the `mail-quote-depth' of the text it holds."
  (let ((pos (point)))
    (shr-tag-blockquote dom)
    (while (< pos (point))
      (let ((next (next-single-property-change pos 'mail-quote-depth nil (point))))
        (put-text-property pos next 'mail-quote-depth
                           (1+ (or (get-text-property pos 'mail-quote-depth) 0)))
        (setq pos next)))))

(defun mail-html-line-depth ()
  "Quote depth of the line at point, or nil when the line holds no text."
  (save-excursion
    (skip-chars-forward " \t" (line-end-position))
    (unless (eolp)
      (or (get-text-property (point) 'mail-quote-depth) 0))))

(defun mail-html-next-depth ()
  "Quote depth of the next line after point that holds text, or 0."
  (save-excursion
    (catch 'depth
      (while (zerop (forward-line 1))
        (when-let* ((depth (mail-html-line-depth)))
          (throw 'depth depth)))
      0)))

(defun draw-mail-html-quotes ()
  "Replace the indentation shr gives each blockquote level with a `>' marker.
A blank line inside a quote gets the markers of the shallower of its
neighbors, so a quoted paragraph break stays quoted."
  (save-excursion
    (goto-char (point-min))
    (let ((previous 0))
      (while (not (eobp))
        (let* ((text (mail-html-line-depth))
               (depth (or text (if (zerop previous) 0
                                 (min previous (mail-html-next-depth))))))
          (when (< 0 depth)
            (let ((markers (apply #'concat (make-list depth "> "))))
              (delete-region (point) (if (not text)
                                         (line-end-position)
                                       (skip-chars-forward " " (min (+ (point) (* 4 depth))
                                                                    (line-end-position)))
                                       (point)))
              (insert (if text markers (string-trim-right markers)))))
          (setq previous depth))
        (forward-line 1)))
    (remove-list-of-text-properties (point-min) (point-max) '(mail-quote-depth))))

;;;###autoload
(defun render-mail-html (handle)
  "Render the HTML part HANDLE with shr, for reading in a mail buffer.
The part's quoted levels come out as `>' lines faced by depth."
  (let ((start (point))
        (shr-fill-text nil)
        (shr-use-fonts nil)
        (shr-use-colors nil)
        (shr-external-rendering-functions
         (cons '(blockquote . mail-shr-tag-blockquote) shr-external-rendering-functions)))
    (mm-shr handle)
    (save-restriction
      (narrow-to-region start (point))
      ;; shr pads table rows with spaces aligned to pixel columns far past
      ;; the window, which a wrapping buffer shows as blank continuation
      ;; lines and early breaks
      (delete-trailing-whitespace (point-min) (point-max))
      (draw-mail-html-quotes)
      (highlight-mail-quotes))))

;;; html.el ends here
