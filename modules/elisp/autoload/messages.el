;;; modules/elisp/autoload/messages.el --- *Messages* helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; last-known-elisp-buffer gained its defvar on port (doom.d setq'd it into
;; the void); the back-jump also guards a dead window now instead of passing
;; nil to select-window.
;;; Code:

(defvar last-known-elisp-buffer nil
  "Buffer to jump back to from *Messages*.")

;;;###autoload
(defun erase-messages-buffer ()
  "Erase *Messages*."
  (interactive)
  (with-current-buffer (messages-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;;###autoload
(defun switch-to-messages-buffer-other-window ()
  "Show *Messages* in another window, remembering the current buffer."
  (interactive)
  (setq last-known-elisp-buffer (current-buffer))
  (if-let* ((mwin (get-buffer-window (messages-buffer))))
      (select-window mwin)
    (window-vsplit-and-follow)
    (switch-to-messages-buffer)))

;;;###autoload
(defun switch-to-last-elisp-buffer ()
  "Jump back to the buffer `switch-to-messages-buffer-other-window' left."
  (interactive)
  (when-let* ((win (and last-known-elisp-buffer
                        (get-buffer-window last-known-elisp-buffer))))
    (select-window win)))

;;;###autoload
(defun hide-messages-window ()
  "Delete the window showing *Messages*, if any."
  (interactive)
  (when-let* ((mw (get-buffer-window (messages-buffer))))
    (delete-window mw)))

;;; messages.el ends here
