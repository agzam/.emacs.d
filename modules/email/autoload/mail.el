;;; modules/email/autoload/mail.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-sum)
(require 'url-util)

(defvar mail-sync-program)
(defvar mail-inbox-group)

;;; Sync

(defun mail-sync-command (&optional full)
  "Command line for `mail-sync-program'; FULL syncs the archive folder too."
  (list mail-sync-program (if full "full" "sync")))

(defun mail-sync-sentinel (proc event)
  "Report PROC's EVENT and refresh the Gnus group buffer after a clean exit."
  (when (memq (process-status proc) '(exit signal))
    (if (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc)))
        (progn
          (message "Mail synced")
          (when (and (gnus-alive-p) (get-buffer gnus-group-buffer))
            (with-current-buffer gnus-group-buffer
              (gnus-group-get-new-news))))
      (message "Mail sync failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer proc))))))

;;;###autoload
(defun sync-mail (&optional full)
  "Sync mail with Gmail in the background; with FULL, the archive folder too."
  (interactive "P")
  (let ((buf (get-buffer-create " *mail-sync*")))
    (with-current-buffer buf
      (erase-buffer))
    (make-process :name "mail-sync"
                  :buffer buf
                  :command (mail-sync-command full)
                  :sentinel #'mail-sync-sentinel)))

;;; Navigation

(defun refresh-mail-group (group)
  "Rescan GROUP's maildir and merge its flags, like `g' on the group line."
  (let ((method (gnus-find-method-for-group group)))
    (gnus-activate-group group 'scan nil method)
    (when-let* ((info (gnus-get-info group)))
      (gnus-request-update-info info method)
      (gnus-get-unread-articles-in-group info (gnus-active group)))))

;;;###autoload
(defun open-mail-inbox ()
  "Start Gnus if needed and enter `mail-inbox-group', subscribing it first."
  (interactive)
  (unless (gnus-alive-p)
    (gnus))
  (unless (gnus-group-entry mail-inbox-group)
    (with-current-buffer gnus-group-buffer
      (gnus-subscribe-newsgroup mail-inbox-group)))
  (refresh-mail-group mail-inbox-group)
  (gnus-summary-read-group mail-inbox-group t t))

;;;###autoload
(defun search-mail (query)
  "Read an ephemeral group of every Gmail message matching the notmuch QUERY."
  (interactive "sSearch mail: ")
  (unless (gnus-alive-p)
    (gnus))
  (gnus-group-read-ephemeral-search-group
   t `((search-query-spec . ((query . ,query) (raw . t)))
       (search-group-spec . (("nnmaildir:gmail"))))))

;;; Web archives

(defun bare-message-id (message-id)
  "MESSAGE-ID without its angle brackets."
  (string-trim message-id "<" ">"))

(defun gmail-message-url (message-id)
  "Gmail web URL of the message with MESSAGE-ID."
  (concat "https://mail.google.com/mail/u/0/#search/"
          (url-hexify-string (concat "rfc822msgid:" (bare-message-id message-id)))))

(defun list-archive-message-url (message-id recipients)
  "Public archive URL of MESSAGE-ID on the mailing list found in RECIPIENTS.
RECIPIENTS is the To and Cc header text.  Returns nil when no known list
address appears there."
  (let ((id (bare-message-id message-id)))
    (cond
     ((string-match-p "emacs-orgmode@gnu\\.org" recipients)
      (format "https://list.orgmode.org/orgmode/%s/" (url-hexify-string id)))
     ((string-match "\\([[:alnum:]._-]+\\)@gnu\\.org" recipients)
      (format "https://yhetil.org/%s/%s" (match-string 1 recipients)
              (url-hexify-string id)))
     ((string-match "\\([[:alnum:]._-]+\\)@googlegroups\\.com" recipients)
      (concat "https://groups.google.com/forum/#!topicsearchin/"
              (match-string 1 recipients) "/messageid$3A"
              (url-hexify-string (concat "\"" id "\"")))))))

(defun summary-message-id ()
  "Message-ID of the article at point in a Gnus summary buffer."
  (mail-header-id (gnus-summary-article-header)))

;;;###autoload
(defun open-message-in-gmail ()
  "Open the article at point in the Gmail web UI."
  (interactive nil gnus-summary-mode)
  (browse-url (gmail-message-url (summary-message-id))))

;;;###autoload
(defun open-message-in-list-archive ()
  "Open the article at point in its mailing list's public archive."
  (interactive nil gnus-summary-mode)
  (let* ((header (gnus-summary-article-header))
         (extra (mail-header-extra header))
         (recipients (concat (cdr (assq 'To extra)) " " (cdr (assq 'Cc extra)))))
    (if-let* ((url (list-archive-message-url (mail-header-id header) recipients)))
        (browse-url url)
      (user-error "No known mailing list among the recipients"))))

;;; mail.el ends here
