;;; modules/email/autoload/quit.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Leaving mail in one step, with nothing typed lost and no Gnus buffer
;; left: unsent messages go to the drafts group, each summary closes the
;; way q closes it, and Gnus's own exit runs its hooks and kills the rest.
;;; Code:

(require 'gnus)
(require 'gnus-group)
(require 'message)

;;;###autoload
(defun quit-mail ()
  "Leave mail with no Gnus buffer left behind.
An unsent message is saved to the drafts group first, and each summary
closes the way `quit-mail-summary' does, which asks about its queue."
  (interactive)
  (when (gnus-alive-p)
    (dolist (buffer (gnus-buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (cond ((and (derived-mode-p 'message-mode) (buffer-modified-p))
                 ;; a Gnus message visits its draft file
                 (save-buffer))
                ((derived-mode-p 'gnus-summary-mode)
                 (quit-mail-summary))))))
    (let ((gnus-interactive-exit nil))
      (with-current-buffer gnus-group-buffer
        (gnus-group-exit)))))

;;; quit.el ends here
