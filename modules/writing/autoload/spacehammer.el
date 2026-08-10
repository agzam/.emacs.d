;;; modules/writing/autoload/spacehammer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun spacehammer-edit-with-emacs-h (buffer-name pid title)
  (with-current-buffer (get-buffer buffer-name)
    ;; need to set a filename, otherwise lsp in that buffer won't work
    (set-visited-file-name (format "/tmp/%s_%s_%s" buffer-name pid title))
    (set-buffer-modified-p nil)
    (markdown-mode)
    ;; major-mode change wipes buffer-locals; permanent-local property
    ;; should preserve ours, but re-set pid just in case
    (setq-local spacehammer--caller-pid pid)
    (goto-char (point-max))
    (evil-insert-state)))

;;;###autoload
(defun spacehammer-before-finish-edit-with-emacs-h (bufname pid)
  (with-current-buffer bufname
    (set-buffer-modified-p nil)))

;;;###autoload
(defun frame-facing-direction ()
  "Return the direction facing away from the Emacs frame's screen edge.
If Emacs is on the right half of the screen, returns `left' (the buffer
should open on the left, facing the other app). Otherwise returns `right'."
  (let* ((frame-x (car (frame-position)))
         (frame-center (+ frame-x (/ (frame-pixel-width) 2)))
         (screen-width (display-pixel-width)))
    (if (< (/ screen-width 2) frame-center)
        'left
      'right)))

;;;###autoload
(defun spacehammer-display-edit-buffer (buffer alist)
  "Display spacehammer edit BUFFER on the side facing away from Emacs frame.
Computes direction from frame position on screen—no AppleScript needed."
  (let ((alist (append `((direction . ,(frame-facing-direction)))
                       alist)))
    (display-buffer-in-quadrant buffer alist)))