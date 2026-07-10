;;; modules/git/autoload/gh-notify.el -*- lexical-binding: t; -*-

(defun gh-notify-notification-read-p (&optional notification)
  "Non-nil when NOTIFICATION (or the one at point) is read."
  (when-let* ((obj (gh-notify-notification-forge-obj
                    (or notification
                        (gh-notify-current-notification)))))
    (not (oref obj unread-p))))

;;;###autoload
(defun gh-notify-mark-read-and-move-next (&optional arg)
  "Mark notification read and move to the next (or prev if ARG)."
  (interactive)
  (let* ((notification (gh-notify-current-notification))
         (forge-obj (gh-notify-notification-forge-obj notification))
         (repo (forge-get-repository forge-obj))
         (topic (forge-get-topic repo (gh-notify-notification-topic notification)))
         (gh-notify-redraw-on-visit nil)
         (_ (gh-notify-set-notification-status notification 'done)))
    (ignore repo topic)
    (setf (cl-struct-slot-value
           'gh-notify-notification
           'unread notification) nil)
    (with-current-buffer (current-buffer)
      (read-only-mode -1)
      (kill-whole-line)
      (insert (funcall 'gh-notify-render-notification notification))
      (insert "\n")
      (read-only-mode +1)))
  (forward-line (if arg -2 0)))

;;;###autoload
(defun gh-notify-mark-read-and-move-prev ()
  "Mark notification read and move to the previous one."
  (interactive)
  (funcall-interactively #'gh-notify-mark-read-and-move-next :prev))

;;;###autoload
(defun gh-notify-code-review-forge-pr-at-point ()
  "Jump to PR review straight from the notifications list."
  (interactive)
  (unwind-protect
      (progn
        (add-hook 'magit-post-display-buffer-hook #'code-review-forge-pr-at-point)
        (forge-visit-topic
         (gh-notify-notification-forge-obj
          (gh-notify-current-notification))))
    (remove-hook 'magit-post-display-buffer-hook #'code-review-forge-pr-at-point)))

;;;###autoload
(defun gh-notify-forge-browse-topic-at-point ()
  "Browse topic straight from the notifications list."
  (interactive)
  (browse-url
   ;; notification url usually points to api, e.g.:
   ;; https://api.github.com/repos/advthreat/tenzin/pulls/2030,
   ;;
   ;; we need to make it look like: https://github.com/advthreat/tenzin/pulls/2030
   (replace-regexp-in-string
    "\\(pull\\)s" "\\1"
    (replace-regexp-in-string
     "api\\.\\|repos/" ""
     (gh-notify-notification-url
      (gh-notify-current-notification))))))
