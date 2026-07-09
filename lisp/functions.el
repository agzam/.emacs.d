;;; lisp/functions.el -*- lexical-binding: t; -*-

;;;###autoload
(defun display-buffer-in-quadrant (buffer alist)
  "Display BUFFER in a side window while preserving existing window dimensions.
When displaying BUFFER for the first time, creates a new window using a quarter
- 25% of the frame width. If BUFFER is already displayed, reuses the existing
window without modifying its dimensions.

The side is determined by the direction entry, e.g., (direction . right)
Initial width controlled by init-width entry, e.g., (init-width . 0.10)
- would occupy the 10% of the frame width

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'. It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((existing (get-buffer-window buffer))
        (init-w (alist-get 'init-width alist 0.25)))
    (if existing
        (display-buffer-reuse-window buffer alist)
      (when-let ((window (display-buffer-in-direction buffer alist)))
        (with-selected-window window
          (window-resize window
                         (- (round (* init-w (frame-pixel-width)))
                            (window-width window t))
                         t nil t))
        window))))

(defun system-dist-name ()
  "Returns system distribution name.

e.g. Ubuntu, Arch; or macOS 15.5, etc."
  (let ((cmd+prefix (alist-get
                     system-type
                     '((gnu/linux . ("lsb_release -si 2>/dev/null"))
                       (darwin . ("sw_vers -productVersion" "macOS "))))))
    (thread-last
      (car cmd+prefix)
      shell-command-to-string
      string-trim
      (concat (cadr cmd+prefix)))))

;; Doom hook names kept: vendored doom-keybinds.el registers on the before
;; hook (which-key replacement-alist reset).
(defvar doom-before-reload-hook nil
  "Hooks run by `reload-config' before reloading.")
(defvar doom-after-reload-hook nil
  "Hooks run by `reload-config' after reloading.")

(defun reload-config ()
  "Reload the config in place, the lab analogue of Doom's doom/reload.

Re-runs the layers init.el runs, in the same order: lisp/doom-defaults,
lisp/functions, every module in `active-modules' (regenerating stale
loaddefs), the root config.el, then custom.el.  Package declarations
re-queue with elpaca and any new ones install on the spot.

Not covered - restart instead: the elpaca bootstrap, the macro layer
(doom-compat/doom-keybinds) and `active-modules' changes.  Edited
autoload/*.el files keep their old in-memory definitions (loaddefs never
re-defines loaded functions); `load-file' those directly."
  (interactive)
  (let ((start-time (current-time)))
    (doom-run-hooks 'doom-before-reload-hook)
    (dolist (file '("lisp/doom-defaults" "lisp/functions"))
      (load (expand-file-name file user-emacs-directory) nil 'nomessage))
    (mapc #'load-module active-modules)
    (load (expand-file-name "config" user-emacs-directory) nil 'nomessage)
    (when custom-file
      (load custom-file 'noerror 'nomessage))
    (elpaca-process-queues)
    (doom-run-hooks 'doom-after-reload-hook)
    (message "Config reloaded in %.02fs" (float-time (time-since start-time)))))
