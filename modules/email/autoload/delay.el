;;; modules/email/autoload/delay.el -*- lexical-binding: t; -*-
;;; Commentary:
;; C-c C-c puts a message into gnus-delay's queue for a minute instead of
;; sending it, so it can still be fixed or dropped.  Gnus's demon sends what
;; is due while Emacs idles, and quitting Gnus or Emacs sends the rest.
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mail-utils)
(require 'message)
(require 'gnus)
(require 'gnus-delay)
(require 'gnus-demon)
(require 'gnus-draft)
(require 'nndraft)

(defvar mail-queue-retry-time 0
  "Time before which the demon leaves the queue alone after a failure.")

(defvar mail-queue-backoff 300
  "Seconds the demon waits after a failed send.")

(defvar mail-queue-failure nil
  "Why the last queue run failed, nil after one that did not.")

(defvar mail-queue-sent nil
  "Where the messages sent in the current queue run went.")

(defun mail-queue-group ()
  "The group that holds queued mail."
  (format "nndraft:%s" gnus-delay-group))

(defun remove-delay-header ()
  "Drop the message's delay header."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header gnus-delay-header))))

;;;###autoload
(defun send-later (&optional now)
  "Queue the message for `gnus-delay-default-delay', or send it NOW.
Without Gnus running there is no queue, and the message leaves at once."
  (interactive "P" message-mode)
  ;; a message reopened from the queue still carries its old deadline
  (remove-delay-header)
  (if (or now (not (gnus-alive-p)))
      (message-send-and-exit)
    ;; the queue sends with the syntax checks off
    (message-fix-before-sending)
    (when (and (message-news-p) (not (message-check-news-syntax)))
      (user-error "The post fails its checks"))
    (let ((to (save-restriction
                (message-narrow-to-headers)
                (or (message-fetch-field "to") (message-fetch-field "newsgroups")))))
      (save-mail-format)
      (gnus-delay-article gnus-delay-default-delay)
      (message "Sending to %s in %s" to gnus-delay-default-delay))))

(defun refuse-mail-prompt (&optional prompt &rest _)
  "Fail the queued send that would wait for an answer to PROMPT."
  (error "Queued mail would ask %S" (or prompt (minibuffer-prompt))))

(defun run-mail-queue (send)
  "Call SEND with queued mail asking nothing, saying nothing, windows kept.
Return (SENT FAILURE): where the messages went, and why SEND stopped."
  (let ((mail-queue-sent nil)
        (message-confirm-send nil)
        (minibuffer-setup-hook (cons #'refuse-mail-prompt minibuffer-setup-hook))
        failure)
    (cl-letf (((symbol-function 'read-multiple-choice) #'refuse-mail-prompt))
      (save-window-excursion
        (condition-case err
            (let ((inhibit-message t))
              (funcall send))
          ((error quit) (setq failure (error-message-string err))))))
    (list (reverse mail-queue-sent) failure)))

(defun queued-mail-recipient (article group)
  "Whom queued ARTICLE of GROUP goes to."
  (or (when (gnus-request-head article group)
        (with-current-buffer nntp-server-buffer
          (or (mail-fetch-field "to") (mail-fetch-field "newsgroups"))))
      "?"))

;;;###autoload
(defun send-queued-article-a (send article &optional group interactive)
  "Call SEND on ARTICLE of GROUP and INTERACTIVE, noting where it went.
A failed queued send leaves no buffer behind, and one that left before
it failed also leaves the queue, so it cannot go twice."
  (if (not (equal group (mail-queue-group)))
      (funcall send article group interactive)
    (let ((to (queued-mail-recipient article group))
          (before (buffer-list))
          (left nil))
      (condition-case err
          (progn
            (funcall send article group interactive)
            (push to mail-queue-sent))
        ((error quit)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (with-current-buffer buffer
               (when (derived-mode-p 'message-mode)
                 (setq left (or left message-sent-message-via))
                 (set-buffer-modified-p nil)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer))))))
         (if (not left)
             (error "Delayed mail to %s failed: %s" to (error-message-string err))
           (gnus-request-expire-articles (list article) group t)
           (push (format "%s (then: %s)" to (error-message-string err)) mail-queue-sent)))))))

(defun send-queued-mail ()
  "Send every queued message now, due or not, as `run-mail-queue' does."
  (let ((group (mail-queue-group)))
    (if (not (and (gnus-alive-p) (gnus-group-entry group)))
        (list nil nil)
      (run-mail-queue
       (lambda ()
         (gnus-activate-group group)
         (let ((message-send-hook (append message-send-hook (list #'remove-delay-header))))
           (dolist (article (nndraft-articles))
             (gnus-draft-send article group))))))))

(defun report-mail-queue (result &optional retry)
  "Say what queue run RESULT sent and why it failed.
With RETRY the demon waits before its next try; a new failure warns."
  (pcase-let ((`(,sent ,failure) result))
    (when sent
      (message "Sent delayed mail to %s" (string-join sent ", ")))
    (when failure
      (when retry
        (setq mail-queue-retry-time (time-add nil mail-queue-backoff)))
      (unless (equal failure mail-queue-failure)
        (display-warning 'mail (format "%s.  It stays in %s%s." failure (mail-queue-group)
                                       (if retry (format " and goes again in %d minutes"
                                                         (/ mail-queue-backoff 60))
                                         "")))))
    (setq mail-queue-failure failure)))

;;;###autoload
(defun send-due-mail ()
  "Send the queued messages that are due, unless a send failed lately.
A minibuffer in use defers the run to the next tick."
  (when (and (gnus-alive-p)
             (not (active-minibuffer-window))
             (time-less-p mail-queue-retry-time nil))
    (report-mail-queue (run-mail-queue #'gnus-delay-send-queue) t)))

(defun send-queued-mail-before-quit ()
  "Send queued mail before Emacs quits; ask only when a send fails."
  (pcase-let ((`(,_ ,failure) (send-queued-mail)))
    (or (not failure)
        (yes-or-no-p (format "%s.  Quit anyway? " failure)))))

;;;###autoload
(defun start-mail-queue ()
  "Send queued mail when due and Emacs idles, and before Emacs quits."
  (gnus-demon-add-handler #'send-due-mail 2 1)
  (add-hook 'kill-emacs-query-functions #'send-queued-mail-before-quit))

;;;###autoload
(defun stop-mail-queue ()
  "Stop sending on schedule, and send everything still queued."
  (gnus-demon-remove-handler #'send-due-mail)
  (remove-hook 'kill-emacs-query-functions #'send-queued-mail-before-quit)
  (report-mail-queue (send-queued-mail)))

;;; delay.el ends here
