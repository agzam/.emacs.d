;;; custom/general/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun evil-ex-visual-star-search-a (fn unbounded direction count &optional symbol)
  "Visual-star search in evil.

I want * and # operators to respect marked region."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (1+ (region-end)))
             (delta (- end beg 1))
             (s (buffer-substring beg end))
             (evil-ex-search-pattern s)
             (evil-ex-search-offset 0))
        (deactivate-mark)
        (if (eq direction 'forward)
            (goto-char end)
          (goto-char beg))
        (when (evil-ex-search-full-pattern s count direction)
          (let ((p (progn (when (eq direction 'forward)
                            (backward-char))
                          (point))))
            (set-mark p)
            (goto-char (if (eq direction 'forward)
                           (- p delta)
                         (+ p delta))))))
    (funcall fn unbounded direction count symbol)))


;; I may not need to override these manually,
;; watch for PR doomemacs/doomemacs#7218
;;;###autoload
(defun window-move-right ()
  "Swap windows to the right"
  (interactive)
  (require 'windmove)
  (if (and (window-at-side-p nil 'right)
           (not (or (window-in-direction 'above)
                    (window-in-direction 'below))))
      (evil-window-move-far-left)
    ;; rename-sweep collision fix: this used to call Doom's
    ;; +evil/window-move-right; calling window-move-right here recursed
    (windmove-swap-states-right)))

;;;###autoload
(defun window-move-left ()
  "Swap windows to the left"
  (interactive)
  (require 'windmove)
  (if (and (window-at-side-p nil 'left)
           (not (or (window-in-direction 'above)
                    (window-in-direction 'below))))
      (evil-window-move-far-right)
    (windmove-swap-states-left)))

;;;###autoload
(defun window-move-up ()
  "Swap windows upward"
  (interactive)
  (require 'windmove)
  (if (and (window-at-side-p nil 'top)
           (not (or (window-in-direction 'left)
                    (window-in-direction 'right))))
      (evil-window-move-very-bottom)
    (windmove-swap-states-up)))

;;;###autoload
(defun window-move-down ()
  "Swap windows downward"
  (interactive)
  (require 'windmove)
  (if (and (window-at-side-p nil 'bottom)
           (not (or (window-in-direction 'left)
                    (window-in-direction 'right))))
      (evil-window-move-very-top)
    (windmove-swap-states-down)))

;; declarations: loading this file must not require evil (batch tests),
;; and the lets below must stay dynamic
(defvar evil-split-window-below)
(defvar evil-vsplit-window-right)

;;;###autoload
(defun window-split-and-follow ()
  "Split horizontally, then focus the new window.
Inverts `evil-split-window-below': non-nil means focus stays put."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;;;###autoload
(defun window-vsplit-and-follow ()
  "Split vertically, then focus the new window.
Inverts `evil-vsplit-window-right': non-nil means focus stays put."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))
