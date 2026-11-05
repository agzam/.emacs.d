;;; modules/email/autoload/html.el -*- lexical-binding: t; -*-
;;; Commentary:
;; HTML mail through shr, readable under any theme of the rotation:
;; paragraphs stay unfilled for the window to wrap, the theme's colors
;; replace the sender's, each blockquote level is a `>' marker, and code
;; sits in fixed pitch on a shade of the background.  eww keeps its own.
;;; Code:

(require 'cl-lib)
(require 'color)
(require 'dom)
(require 'gnus)
(require 'mm-decode)
(require 'shr)

(defface mail-code '((t :inherit fixed-pitch))
  "Inline code in HTML mail; `shade-mail-code-face' gives it a background."
  :group 'gnus-article)

(defface mail-code-block '((t :inherit mail-code :extend t))
  "A code block in HTML mail, its background reaching the window edge."
  :group 'gnus-article)

(defvar mail-code-shade 0.08
  "Share of the text color in the background of `mail-code'.")

;;;###autoload
(defun mail-shade (share)
  "Color SHARE of the way from the theme's background to its text, or nil."
  (when-let* ((background (color-name-to-rgb (or (face-background 'default nil t) "")))
              (text (color-name-to-rgb (or (face-foreground 'default nil t) ""))))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (from to) (+ from (* share (- to from))))
                              background text)
                   '(2)))))

(defun shade-mail-code-face (&rest _)
  "Set the background of `mail-code' to a shade of the current theme.
A terminal whose colors Emacs does not know leaves it without one."
  (when-let* ((color (mail-shade mail-code-shade)))
    (set-face-background 'mail-code color)))

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

(defun fontify-mail-code (start end mode)
  "Give the code from START to END the faces the font lock of MODE gives it."
  (let ((code (buffer-substring-no-properties start end))
        (pos 0))
    (with-temp-buffer
      (insert code)
      ;; its hooks start what editing needs, such as linters
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (setq code (buffer-string)))
    (while (< pos (length code))
      (let ((next (next-single-property-change pos 'face code (length code))))
        (when-let* ((face (get-text-property pos 'face code)))
          (add-face-text-property (+ start pos) (+ start next) face))
        (setq pos next)))))

(defun mail-shr-tag-pre (dom)
  "Render the pre DOM as shr does, in fixed pitch.
A pre holding code is a code block in `mail-code-block', highlighted as
the major mode the group parameter `mail-code-major-mode' names."
  (shr-ensure-newline)
  (let ((start (point)))
    (shr-tag-pre dom)
    ;; `draw-mail-html-quotes' leaves the indentation of this text alone
    (put-text-property start (point) 'mail-preformatted t)
    (if (not (dom-child-by-tag dom 'code))
        (add-face-text-property start (point) 'fixed-pitch t)
      (when-let* ((group gnus-newsgroup-name)
                  (mode (gnus-group-find-parameter group 'mail-code-major-mode)))
        (fontify-mail-code start (point) mode))
      (add-face-text-property start (point) 'mail-code-block t))))

(defun mail-shr-tag-code (dom)
  "Render the code DOM as shr does, in `mail-code' outside a code block."
  (let ((start (point)))
    (shr-tag-code dom)
    ;; inside a pre, which faces the whole block
    (unless (eq shr-folding-mode 'none)
      (add-face-text-property start (point) 'mail-code t))))

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

(defun mail-html-skip-indentation (depth)
  "Move past the indentation shr gave the line at point, DEPTH quotes deep.
Preformatted text keeps the indentation it was written with."
  (let ((limit (min (+ (point) (* 4 depth)) (line-end-position))))
    (skip-chars-forward " " (or (text-property-any (point) limit 'mail-preformatted t)
                                limit))))

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
                                       (mail-html-skip-indentation depth)
                                       (point)))
              (insert (if text markers (string-trim-right markers)))))
          (setq previous depth))
        (forward-line 1)))
    (remove-list-of-text-properties (point-min) (point-max)
                                    '(mail-quote-depth mail-preformatted))))

;;;###autoload
(defun render-mail-html (handle)
  "Render the HTML part HANDLE with shr, for reading in a mail buffer.
The part's quoted levels come out as `>' lines faced by depth, and its
code in `mail-code' and `mail-code-block'."
  (let ((start (point))
        (shr-fill-text nil)
        (shr-use-fonts nil)
        (shr-use-colors nil)
        (shr-external-rendering-functions
         (append '((blockquote . mail-shr-tag-blockquote)
                   (pre . mail-shr-tag-pre)
                   (code . mail-shr-tag-code))
                 shr-external-rendering-functions)))
    (mm-shr handle)
    (save-restriction
      (narrow-to-region start (point))
      ;; shr pads table rows with spaces aligned to pixel columns far past
      ;; the window, which a wrapping buffer shows as blank continuation
      ;; lines and early breaks
      (delete-trailing-whitespace (point-min) (point-max))
      (draw-mail-html-quotes)
      (highlight-mail-quotes))))

(provide 'mail-html)
;;; html.el ends here
