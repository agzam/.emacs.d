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


;; Port of Doom's +evil--window-swap (post doomemacs/doomemacs#7218, which
;; folded doom.d's manual wrap-around overrides into core; wrap is always
;; on here, like the doom.d wrappers had it).  The earlier lab version
;; called bare windmove-swap-states-*, which errors when no window exists
;; in the direction - Doom throws the window to the far side instead.
(defun window-move-in-direction (direction)
  "Move the selected window DIRECTION-ward.
Swap with the window there when one exists; with none, throw the
window to the far DIRECTION side of the frame; a full-span window
already at that frame edge wraps around to the opposite side; a
sole window splits the frame and follows."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (that-window (window-in-direction direction nil this-window)))
    (when (and that-window
               (or (minibufferp (window-buffer that-window))
                   (window-dedicated-p that-window)))
      (setq that-window nil))
    (cond
     (that-window
      (window-swap-states this-window that-window)
      (select-window that-window))
     ((one-window-p t)
      (let ((that-window (split-window this-window nil direction)))
        (with-selected-window that-window
          (switch-to-buffer (get-scratch-buffer-create)))
        (window-swap-states this-window that-window)
        (select-window that-window)))
     ((and (window-at-side-p this-window
                             (pcase direction ('up 'top) ('down 'bottom) (_ direction)))
           (not (seq-some (lambda (dir) (window-in-direction dir nil this-window))
                          (if (memq direction '(left right))
                              '(up down)
                            '(left right)))))
      ;; full-span window at the frame edge: wrap to the opposite side
      (funcall (pcase direction
                 ('left #'evil-window-move-far-right)
                 ('right #'evil-window-move-far-left)
                 ('up #'evil-window-move-very-bottom)
                 ('down #'evil-window-move-very-top))))
     (t
      (funcall (pcase direction
                 ('left #'evil-window-move-far-left)
                 ('right #'evil-window-move-far-right)
                 ('up #'evil-window-move-very-top)
                 ('down #'evil-window-move-very-bottom)))))))

;;;###autoload
(defun window-move-right ()
  "Move the window right: swap, throw to the far right, or wrap."
  (interactive)
  (window-move-in-direction 'right))

;;;###autoload
(defun window-move-left ()
  "Move the window left: swap, throw to the far left, or wrap."
  (interactive)
  (window-move-in-direction 'left))

;;;###autoload
(defun window-move-up ()
  "Move the window up: swap, throw to the very top, or wrap."
  (interactive)
  (window-move-in-direction 'up))

;;;###autoload
(defun window-move-down ()
  "Move the window down: swap, throw to the very bottom, or wrap."
  (interactive)
  (window-move-in-direction 'down))

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
