;;; modules/email/autoload/mail.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-sum)
(require 'nnmaildir)
(require 'url-util)

(defvar gmail-maildir)
(defvar mail-sync-program)
(defvar mail-inbox-group)
(defvar mail-groups)
(defvar mail-bulk-groups)

;;; Sync

(defun mail-sync-command (&optional full)
  "Command line for `mail-sync-program'; FULL runs every sync tier at once."
  (list mail-sync-program (if full "full" "sync")))

(defun mail-sync-sentinel (proc event)
  "Report PROC's EVENT and refresh the routine mail groups after a clean exit."
  (when (memq (process-status proc) '(exit signal))
    (if (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc)))
        (progn
          (message "Mail synced")
          (when (gnus-alive-p)
            (refresh-mail-groups)))
      (message "Mail sync failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer proc))))))

;;;###autoload
(defun sync-mail (&optional full)
  "Sync mail with Gmail in the background; with FULL, every tier at once."
  (interactive "P")
  (let ((buf (get-buffer-create " *mail-sync*")))
    (with-current-buffer buf
      (erase-buffer))
    (make-process :name "mail-sync"
                  :buffer buf
                  :command (mail-sync-command full)
                  :sentinel #'mail-sync-sentinel)))

;;; Navigation

(defun subscribe-mail-group (group)
  "Subscribe GROUP unless the newsrc already lists it."
  (unless (gnus-group-entry group)
    (with-current-buffer gnus-group-buffer
      (gnus-subscribe-newsgroup group))))

(defun maildir-groups ()
  "Every nnmaildir group the mbsync store holds, one per Gmail label."
  (when (file-directory-p gmail-maildir)
    (mapcar (lambda (dir) (concat "nnmaildir+gmail:" (file-name-nondirectory dir)))
            (seq-filter #'file-directory-p
                        (directory-files gmail-maildir t "\\`[^.]")))))

(defun defer-bulk-mail-groups ()
  "Put `mail-bulk-groups' one level above `gnus-activate-level'.
A routine scan then leaves them alone; they still list and enter normally."
  (let ((level (1+ gnus-activate-level)))
    (dolist (group mail-bulk-groups)
      (when-let* ((known (gnus-group-entry group))
                  (old (gnus-group-level group)))
        (unless (= old level)
          (gnus-group-change-level group level old))))))

;;;###autoload
(defun subscribe-mail-groups ()
  "Subscribe every maildir group plus `mail-groups', skipping known ones.
Subscribing activates, and gnus-search silently drops a hit whose group
nnmaildir never opened - notmuch returns whichever duplicate it likes.
The bulk groups are then moved out of the routine scan."
  (mapc #'subscribe-mail-group (append (maildir-groups) mail-groups))
  (defer-bulk-mail-groups))

(defun refresh-mail-group (group)
  "Rescan GROUP's maildir and merge its flags, like `g' on the group line."
  (let ((method (gnus-find-method-for-group group)))
    (gnus-activate-group group 'scan nil method)
    (when-let* ((info (gnus-get-info group)))
      (gnus-request-update-info info method)
      (gnus-get-unread-articles-in-group info (gnus-active group)))))

(defun scanned-mail-groups ()
  "Maildir groups at or below `gnus-activate-level'."
  (seq-filter (lambda (group)
                (and (<= (gnus-group-level group) gnus-activate-level)
                     (eq (car (gnus-find-method-for-group group)) 'nnmaildir)))
              gnus-group-list))

;;;###autoload
(defun refresh-mail-groups ()
  "Rescan the maildir groups a routine scan covers now, one group at a time.
`gnus-group-get-new-news' leaves these rescans to timer turns instead,
through `defer-mail-server-scan-a'."
  (interactive)
  (dolist (group (scanned-mail-groups))
    (refresh-mail-group group)
    (gnus-group-update-group group t)))

;;; Reading the store without blocking

(defvar mail-refresh-queue nil
  "Maildir groups `refresh-next-mail-group' has yet to read, the next one first.")

(defvar mail-refresh-timer nil
  "Timer of the next `refresh-next-mail-group' turn, or nil when none is due.")

;;;###autoload
(defun queue-mail-refresh ()
  "Queue the maildir groups a routine scan covers, the inbox first.
Each group is read on a timer turn of its own, so a key pressed meanwhile
waits for one group, not for all of them."
  (let ((groups (scanned-mail-groups)))
    (setq mail-refresh-queue
          (if (member mail-inbox-group groups)
              (cons mail-inbox-group (remove mail-inbox-group groups))
            groups)))
  (unless (timerp mail-refresh-timer)
    (setq mail-refresh-timer (run-with-timer 0 nil #'refresh-next-mail-group))))

(defun refresh-next-mail-group ()
  "Rescan the next group of `mail-refresh-queue' and redraw its line.
The next turn is set before this one reads, so a group that fails to
read leaves the rest of the queue running."
  (setq mail-refresh-timer nil)
  (when-let* (((gnus-alive-p))
              (group (pop mail-refresh-queue)))
    (when mail-refresh-queue
      (setq mail-refresh-timer (run-with-timer 0 nil #'refresh-next-mail-group)))
    (refresh-mail-group group)
    (gnus-group-update-group group t)))

;;;###autoload
(defun defer-mail-server-scan-a (fn &optional group server)
  "Call FN to scan GROUP on SERVER; queue the routine groups instead of all.
With no GROUP nnmaildir reads every label in the store, the archive and
the mailing lists included, in the main thread - tens of seconds before
the group buffer appears."
  (if group
      (funcall fn group server)
    (queue-mail-refresh)
    t))

;;;###autoload
(defun scan-mail-group-on-miss-a (fn base-name group server)
  "Call FN for BASE-NAME in GROUP on SERVER; on a miss, scan GROUP and retry.
gnus-search maps each notmuch hit through this, and notmuch answers with
the archive's copy of nearly every message, a group no startup reads."
  (or (funcall fn base-name group server)
      (progn
        (nnmaildir-request-scan group server)
        (funcall fn base-name group server))))

;;;###autoload
(defun scan-unknown-mail-group-a (fn group &optional server &rest args)
  "Call FN on GROUP, SERVER and ARGS; if it fails, scan GROUP and call again.
nnmaildir refuses a group it has not read this session with \"No such
group\" - after a start, every group but the routine ones - so entering
a label, filing a copy or moving a message into one would fail."
  (or (apply fn group server args)
      (progn
        (nnmaildir-request-scan group server)
        (apply fn group server args))))

;;;###autoload
(defun open-mail-inbox ()
  "Start Gnus if needed and enter `mail-inbox-group', subscribing it first."
  (interactive)
  (unless (gnus-alive-p)
    (gnus))
  (subscribe-mail-group mail-inbox-group)
  (refresh-mail-group mail-inbox-group)
  (gnus-summary-read-group mail-inbox-group t t))

;;;###autoload
(defun read-mail-article ()
  "Show the article at point and put point in its buffer.
The select call is what creates the article buffer, without which
`gnus-summary-select-article-buffer' errors."
  (interactive nil gnus-summary-mode)
  (gnus-summary-select-article)
  (gnus-summary-select-article-buffer))

;;;###autoload
(defun search-mail (query)
  "Read an ephemeral group of every Gmail message matching the notmuch QUERY."
  (interactive "sSearch mail: ")
  (unless (gnus-alive-p)
    (gnus))
  ;; mbsync creates a group dir the moment a label appears
  (subscribe-mail-groups)
  (gnus-group-read-ephemeral-search-group
   t `((search-query-spec . ((query . ,query) (raw . t)))
       (search-group-spec . (("nnmaildir:gmail"))))))

;;; Order and folds

(defvar-local mail-sort-reversed nil
  "Non-nil when the summary's last sort ran reversed.")

(defun sort-mail (predicate)
  "Sort the summary by PREDICATE, reversed when the same sort command repeats.
Gnus reverses only on a prefix argument."
  (setq mail-sort-reversed (and (eq last-command this-command)
                                (not mail-sort-reversed)))
  (gnus-summary-sort predicate mail-sort-reversed))

;;;###autoload
(defun sort-mail-by-date ()
  "Sort the summary newest thread first; again, oldest first."
  (interactive nil gnus-summary-mode)
  (sort-mail 'most-recent-date))

;;;###autoload
(defun sort-mail-by-author ()
  "Sort the summary by author; again, in reverse."
  (interactive nil gnus-summary-mode)
  (sort-mail 'author))

;;;###autoload
(defun sort-mail-by-subject ()
  "Sort the summary by subject; again, in reverse."
  (interactive nil gnus-summary-mode)
  (sort-mail 'subject))

;;;###autoload
(defun toggle-mail-thread-fold ()
  "Fold the thread at point, or unfold it when it is folded."
  (interactive nil gnus-summary-mode)
  (unless (gnus-summary-show-thread)
    (gnus-summary-hide-thread)))

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

(defun mail-header-on-screen ()
  "Header of the message the current summary, thread or article buffer shows."
  (pcase-let ((`(,summary . ,article) (mail-on-screen)))
    (with-current-buffer summary
      (gnus-summary-article-header article))))

;;;###autoload
(defun open-message-in-gmail ()
  "Open the message at point in the Gmail web UI."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (browse-url (gmail-message-url (mail-header-id (mail-header-on-screen)))))

;;;###autoload
(defun open-message-in-list-archive ()
  "Open the message at point in its mailing list's public archive."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (let* ((header (mail-header-on-screen))
         (extra (mail-header-extra header))
         (recipients (concat (cdr (assq 'To extra)) " " (cdr (assq 'Cc extra)))))
    (if-let* ((url (list-archive-message-url (mail-header-id header) recipients)))
        (browse-url url)
      (user-error "No known mailing list among the recipients"))))

;;; mail.el ends here
