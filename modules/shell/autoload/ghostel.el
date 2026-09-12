;;; modules/shell/autoload/ghostel.el --- evil-ghostel cursor math in cells -*- lexical-binding: t; -*-

;; The renderer lets a fallback-font glyph wider than its cell claim the blank
;; cell after it: the glyph gets a `min-width' of 2, the blank a zero-width
;; space spec.  `current-column' honors the zero width but not the min-width,
;; so every Emacs column past such a cell (the prompt's ❯) is one short of the
;; terminal column, and evil-ghostel, which derives its arrow counts from
;; `current-column', parks zle's cursor one cell left of the target.  The
;; renderer emits one character per cell, so counting cells from the buffer
;; text with `string-width' ignores display properties and stays exact.

(defvar ghostel--term)
(defvar ghostel--cursor-pos)
(defvar ghostel--cursor-char-pos)
(declare-function ghostel--viewport-row-at "ghostel" (pos))
(declare-function ghostel--send-encoded "ghostel" (key-name mods &optional utf8))
(declare-function evil-ghostel--input-end "evil-ghostel")
(declare-function evil-ghostel--sync-render "evil-ghostel")

;;;###autoload
(defun ghostel-cell-distance (from to)
  "Signed count of terminal cells from buffer position FROM to TO on one row."
  (let ((cells (string-width
                (buffer-substring-no-properties (min from to) (max from to)))))
    (if (< to from) (- cells) cells)))

;;;###autoload
(defun ghostel-cell-column (pos)
  "Terminal column of buffer position POS, counted from its row start."
  (save-excursion
    (goto-char pos)
    (ghostel-cell-distance (line-beginning-position) pos)))

;;;###autoload
(defun evil-ghostel-goto-input-position-a (pos)
  "Drive the terminal cursor and point to POS by cell count."
  (when (and ghostel--term ghostel--cursor-pos)
    (let* ((start-col (car ghostel--cursor-pos))
           (start-row (cdr ghostel--cursor-pos))
           (target-row (or (ghostel--viewport-row-at pos) start-row))
           (dy (- target-row start-row))
           (same-row (and (zerop dy) ghostel--cursor-char-pos)))
      ;; A right arrow at the end of input accepts a trailing autosuggestion.
      (when (and same-row (< ghostel--cursor-char-pos pos))
        (setq pos (min pos (or (evil-ghostel--input-end) pos))))
      (let ((dx (if same-row
                    (ghostel-cell-distance ghostel--cursor-char-pos pos)
                  (- (ghostel-cell-column pos) start-col))))
        (dotimes (_ (abs dy))
          (ghostel--send-encoded (if (< 0 dy) "down" "up") ""))
        (dotimes (_ (abs dx))
          (ghostel--send-encoded (if (< 0 dx) "right" "left") ""))
        (unless (and (zerop dx) (zerop dy))
          (evil-ghostel--sync-render))
        (goto-char pos)
        t))))

;;;###autoload
(defun evil-ghostel-reset-cursor-point-a (orig-fn)
  "Snap point to the renderer's own cursor position when it is known."
  (if ghostel--cursor-char-pos
      (goto-char ghostel--cursor-char-pos)
    (funcall orig-fn)))
