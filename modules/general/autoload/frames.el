;;; custom/general/autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload
(defun display-current-workarea ()
  "Return (X Y WIDTH HEIGHT) of the monitor hosting the selected frame."
  (let* ((current-frame (selected-frame))
         (monitor-at-pos (car (seq-filter
                               (lambda (monitor)
                                 (let ((geometry (cdr (assoc 'geometry monitor))))
                                   (and (<= (car geometry) (frame-parameter current-frame 'left))
                                        (< (frame-parameter current-frame 'left)
                                           (+ (car geometry) (nth 2 geometry))))))
                               (display-monitor-attributes-list)))))
    (alist-get 'workarea monitor-at-pos)))

(defvar font-size-increment 20
  "Height units (1/10 pt) each font size step adds or removes.
Keep it a whole-point multiple of 10: sub-point sizes render as the
nearest point on macOS, making half-point steps look like no-ops.
20 matches Doom's 2pt `doom-font-increment' feel.")

(defvar font-size--initial-height nil
  "Default face height captured before the first adjustment.")

;;;###autoload
(defun font-size-increase (&optional step)
  "Increase the default face height by STEP in every frame.
Session-wide counterpart of the buffer-local `text-scale-increase';
faces with a relative :height follow along."
  (interactive)
  (unless font-size--initial-height
    (setq font-size--initial-height (face-attribute 'default :height)))
  (set-face-attribute 'default nil :height
                      (max 10 (+ (face-attribute 'default :height)
                                 (or step font-size-increment)))))

;;;###autoload
(defun font-size-decrease ()
  "Decrease the default face height in every frame."
  (interactive)
  (font-size-increase (- font-size-increment)))

;;;###autoload
(defun font-size-reset ()
  "Restore the default face height from before the first adjustment."
  (interactive)
  (when font-size--initial-height
    (set-face-attribute 'default nil :height font-size--initial-height)))

;;;###autoload
(defun text-scale-reset ()
  "Reset the current buffer's text scale."
  (interactive)
  (text-scale-set 0))

(defun reset-ns-autohide-menu-bar ()
  "Flip `ns-auto-hide-menu-bar' off and back to repaint the macOS menu bar.
Frame resizing on macOS can desync it otherwise."
  (when (and (eq system-type 'darwin)
             (boundp 'ns-auto-hide-menu-bar))
    (let ((val ns-auto-hide-menu-bar))
      (setf ns-auto-hide-menu-bar (not val))
      (setf ns-auto-hide-menu-bar val))))

;;;###autoload
(defun toggle-frame-full-height ()
  "Toggle an undecorated frame stretched to the full workarea height.
Hides the OS titlebar; workarea coordinates keep the frame clear of
the macOS menu bar or a Linux panel."
  (interactive)
  (when (fboundp 'posframe-delete-all)
    (posframe-delete-all))
  (let* ((fr (selected-frame))
         (x (car (frame-position fr))))
    (if (frame-parameter fr 'undecorated-fullheight)
        (progn
          (set-frame-parameter fr 'undecorated-fullheight nil)
          (set-frame-parameter fr 'undecorated nil)
          (set-frame-parameter fr 'fullscreen nil))
      (pcase-let ((`(,_ ,y ,_ ,h) (display-current-workarea)))
        (reset-ns-autohide-menu-bar)
        (set-frame-parameter fr 'undecorated t)
        (set-frame-parameter fr 'undecorated-fullheight t)
        (set-frame-position fr x y)
        (set-frame-height fr (- h (tab-bar-height fr t)) nil :pixelwise)))
    (redraw-display)))

;; (autoload cookie removed: it made the loaddefs require the built-in
;; transient at startup, before Elpaca activates the github one)
(require 'transient)

;;;###autoload
(transient-define-prefix frame-zoom-transient ()
  "Font and text zoom."
  ["Zoom"
   ["Font (all frames)"
    ("j" "decrease" font-size-decrease :transient t)
    ("k" "increase" font-size-increase :transient t)
    ("0" "reset" font-size-reset :transient t)]
   ["Text scale (buffer)"
    ("s-j" "decrease" text-scale-decrease :transient t)
    ("s-k" "increase" text-scale-increase :transient t)
    ("s-0" "reset" text-scale-reset :transient t)]
   ["Frame"
    ("h" "full height" toggle-frame-full-height)]])
