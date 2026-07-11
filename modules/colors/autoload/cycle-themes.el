;;; modules/colors/autoload/cycle-themes.el -*- lexical-binding: t; -*-

(require 'transient)
(require 'ring)
(require 'circadian)

(defvar cycle-themes-ring nil)

(defun cycle-themes-ring-init ()
  "Build the theme ring from `circadian-themes' once, then reuse it."
  (when (null cycle-themes-ring)
    (setq cycle-themes-ring
          (ring-convert-sequence-to-ring
           (seq-map #'cdr circadian-themes))))
  cycle-themes-ring)

(defun load-cycled-theme (direction)
  "Load the ring neighbor of the active theme in DIRECTION (`next' or `prev').
Themes outside the ring get inserted so cycling can proceed from them.
Disables all enabled themes first so face definitions don't leak across."
  (let* ((ring (cycle-themes-ring-init))
         (current (car custom-enabled-themes))
         (theme (progn
                  (unless (ring-member ring current)
                    (ring-extend ring 1)
                    (ring-insert ring current))
                  (if (eq direction 'next)
                      (ring-next ring current)
                    (ring-previous ring current)))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme :no-confirm)
    theme))

;;;###autoload
(defun load-next-theme ()
  "Switch to the next theme in the `circadian-themes' ring."
  (interactive)
  (load-cycled-theme 'next))

;;;###autoload
(defun load-prev-theme ()
  "Switch to the previous theme in the `circadian-themes' ring."
  (interactive)
  (load-cycled-theme 'prev))

;;;###autoload
(transient-define-prefix cycle-themes ()
  "Cycle color themes."
  [:description
   (lambda () (format "Theme: %s\n" (car custom-enabled-themes)))
   [("n" "next" load-next-theme :transient t)
    ("p" "previous" load-prev-theme :transient t)
    ("l" "list themes" consult-theme)]])

;;;###autoload
(defun cycle-themes-up ()
  "Open the theme transient and step to the previous theme."
  (interactive)
  (cycle-themes)
  (load-prev-theme))

;;;###autoload
(defun cycle-themes-down ()
  "Open the theme transient and step to the next theme."
  (interactive)
  (cycle-themes)
  (load-next-theme))
