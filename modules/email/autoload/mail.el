;;; modules/email/autoload/mail.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'gnus-search)
(require 'gnus-sum)
(require 'nnmaildir)
(require 'url-util)

(defvar gmail-maildir)
(defvar news-maildir)
(defvar mail-sync-program)
(defvar mail-inbox-group)
(defvar mail-trash-group)
(defvar mail-archive-group)
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
  "Sync mail with Gmail, fetch the news and the feeds, all in the background.
With FULL, the mail sync runs every tier at once."
  (interactive "P")
  (let ((buf (get-buffer-create " *mail-sync*")))
    (with-current-buffer buf
      (erase-buffer))
    (make-process :name "mail-sync"
                  :buffer buf
                  :command (mail-sync-command full)
                  :sentinel #'mail-sync-sentinel))
  (fetch-news)
  (fetch-feeds))

;;; Navigation

(defun subscribe-mail-group (group)
  "Subscribe GROUP unless the newsrc already lists it.
Gnus resolves a server it has not opened to a bare method and keeps that
in the newsrc, dropping what `gnus-secondary-select-methods' defines."
  (unless (gnus-group-entry group)
    (with-current-buffer gnus-group-buffer
      (let ((gnus-override-subscribe-method
             (seq-find (lambda (method)
                         (equal (gnus-method-to-server method) (gnus-group-server group)))
                       gnus-secondary-select-methods)))
        (gnus-subscribe-newsgroup group)))))

;;;###autoload
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

;;;###autoload
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
        ;; the first search of a session reads All Mail, some ten seconds
        (message "Reading %s for the search..." group)
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

;;; Search

(defvar mail-search-limit 500
  "How many of the newest matches `search-mail' shows unless asked for more.")

(defvar mail-search-limit-max 2000
  "Most matches `search-mail' shows; Gnus draws the whole summary in one step.")

;;;###autoload
(defun read-mail-search-limit ()
  "Ask how many of the newest matches a search shows."
  (read-number (format "Show how many of the newest matches (up to %d): "
                       mail-search-limit-max)
               mail-search-limit))

(defun mail-search-servers ()
  "Every nnmaildir server, as a search's group spec names them."
  (mapcar (lambda (method) (list (gnus-method-to-server method)))
          (seq-filter (lambda (method) (eq (car method) 'nnmaildir))
                      gnus-secondary-select-methods)))

(defun notmuch-query-text (query)
  "QUERY without the keys gnus-search takes out of it, such as limit:N."
  (alist-get 'query (gnus-search-prepare-query `((query . ,query) (raw . t)))))

;;;###autoload
(defun search-mail (query &optional limit)
  "Read an ephemeral group of the newest Gmail messages matching notmuch QUERY.
LIMIT caps how many, `mail-search-limit' by default; 0 or a count past
`mail-search-limit-max' gives that maximum.  A prefix argument asks for LIMIT."
  (interactive (list (read-string "Search mail: ")
                     (and current-prefix-arg (read-mail-search-limit))))
  (unless (gnus-alive-p)
    (gnus))
  ;; mbsync creates a group dir the moment a label appears
  (subscribe-mail-groups)
  (let* ((limit (cond ((null limit) mail-search-limit)
                      ((<= 1 limit mail-search-limit-max) limit)
                      (t mail-search-limit-max)))
         ;; the cap replaces Gnus's question of how many to show
         (gnus-large-ephemeral-newsgroup nil)
         (group (gnus-group-read-ephemeral-search-group
                 t `((search-query-spec . ((query . ,query) (raw . t) (limit . ,limit)))
                     (search-group-spec . ,(mail-search-servers))))))
    (when-let* ((group)
                (shown (with-current-buffer (gnus-summary-buffer-name group)
                         (length gnus-newsgroup-articles)))
                ((<= limit shown)))
      (message "The newest %d of %d matches; %s"
               shown (car (count-mail (list (notmuch-query-text query))))
               (if (< limit mail-search-limit-max)
                   (format "a prefix argument shows up to %d" mail-search-limit-max)
                 "narrow the query to reach older ones")))))

(defun mail-file-group (file)
  "Group of FILE, a message file in the Gmail store or the news store, or nil."
  (seq-some (pcase-lambda (`(,root . ,prefix))
              (when (string-prefix-p root file)
                (concat prefix (car (split-string (substring file (length root)) "/")))))
            (list (cons (file-name-as-directory (expand-file-name gmail-maildir))
                        "nnmaildir+gmail:")
                  (cons (file-name-as-directory (expand-file-name news-maildir))
                        "nnmaildir+news:"))))

(defun mail-copy-rank (file)
  "Rank of FILE, one copy of a message; the lowest shows.
The inbox copy wins, so search results act on what the inbox shows.  A
label read at startup comes next, then the news store, All Mail, the
list labels and trash."
  (let ((group (mail-file-group file)))
    (cond ((equal group mail-inbox-group) 0)
          ((null group) 6)
          ((equal group mail-trash-group) 5)
          ((news-group-p group) 2)
          ((equal group mail-archive-group) 3)
          ((member group mail-bulk-groups) 4)
          (t 1))))

(defun message-copies (files firsts)
  "FILES, every copy of the messages notmuch found, as one list per message.
FIRSTS holds each message's first copy, which starts its list: notmuch
names a message's copies in the same order with and without
--duplicate=1."
  (let (messages)
    (dolist (file files)
      (cond ((equal file (car firsts))
             (pop firsts)
             (push (list file) messages))
            (messages
             (push file (car messages)))))
    (nreverse (mapcar #'reverse messages))))

(defun maildir-file-names (dir)
  "Hash of each message file's name in maildir directory DIR by its base name."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (name (directory-files dir nil "\\`[^.]" t))
      (puthash (car (split-string name ":")) (expand-file-name name dir) names))
    names))

(defun current-mail-file (file listings)
  "FILE under the name it has now, or nil once it is gone.
A scan moves new mail into cur/ and a saved flag renames the file, and
notmuch learns the new name at its next run.  LISTINGS holds the cur/
directories read so far, each by `maildir-file-names'."
  (if (file-exists-p file)
      file
    (let ((cur (expand-file-name
                "cur" (file-name-directory (directory-file-name (file-name-directory file))))))
      (gethash (car (split-string (file-name-nondirectory file) ":"))
               (with-memoization (gethash cur listings)
                 (maildir-file-names cur))))))

(defun likeliest-copy (copies &optional listings)
  "The one of COPIES, a message's files, a search shows.
A copy counts under the name it has now, and not at all once it is gone.
LISTINGS caches directory reads across the messages of one search."
  (let ((listings (or listings (make-hash-table :test #'equal))))
    (seq-some (lambda (file) (current-mail-file file listings))
              (seq-sort-by #'mail-copy-rank #'< copies))))

(defun notmuch-files (text)
  "The file names in TEXT, notmuch's output, without the lines around them."
  (seq-filter #'file-name-absolute-p (split-string text "\n" t)))

(defun likeliest-copies (engine query groups firsts)
  "Each message's likeliest copy, for FIRSTS, the files ENGINE found for QUERY.
GROUPS are the search's groups.  nil when notmuch fails to name every copy."
  (let* ((args (remove "--duplicate=1"
                       (gnus-search-indexed-search-command
                        engine (gnus-search-make-query-string engine query) query groups)))
         (files (with-temp-buffer
                  (apply #'call-process (slot-value engine 'program) nil '(t nil) nil args)
                  (notmuch-files (buffer-string))))
         (listings (make-hash-table :test #'equal)))
    (when files
      (seq-keep (lambda (copies) (likeliest-copy copies listings))
                (message-copies files firsts)))))

;;;###autoload
(defun search-likeliest-copies-a (fn engine server query &optional groups)
  "Call FN with ENGINE, SERVER, QUERY and GROUPS on each hit's likeliest copy.
notmuch answers with one copy per message, nearly always All Mail's, so
marks and moves in search results would miss the inbox.  Each server
keeps the copies in its own store, so a message shows once."
  (when (object-of-class-p engine 'gnus-search-notmuch)
    (let* ((root (file-name-as-directory
                  (expand-file-name (slot-value engine 'remove-prefix))))
           (firsts (notmuch-files (buffer-string)))
           ;; a thread search asks for every copy already; a failed
           ;; second run leaves the first run's hits
           (hits (or (unless (alist-get 'thread query)
                       (likeliest-copies engine query groups firsts))
                     firsts)))
      (erase-buffer)
      (dolist (file hits)
        (when (string-prefix-p root file)
          (insert file "\n")))))
  (funcall fn engine server query groups))

;;; Flags set elsewhere
;;
;; nnmaildir remembers each file's flags from when it read the file, and
;; mbsync renames the file whenever the phone changes one.

(defun rebase-mail-flags (from to onto)
  "Maildir suffix ONTO, with the flags that differ from FROM to TO set as in TO."
  (let ((from (string-to-list (substring from 3)))
        (to (string-to-list (substring to 3))))
    (concat ":2," (sort (seq-union (seq-difference (string-to-list (substring onto 3))
                                                   (seq-difference from to))
                                   (seq-difference to from))
                        #'<))))

;;;###autoload
(defun keep-flags-set-elsewhere-a (fn article new-suffix curdir)
  "Call FN to give ARTICLE in CURDIR the flags of NEW-SUFFIX, keeping the rest.
nnmaildir derives NEW-SUFFIX from the flags it remembers, and renaming
the file to it would drop a flag the phone changed since."
  (let* ((prefix (nnmaildir--art-prefix article))
         (old (nnmaildir--art-suffix article))
         (file (current-mail-file (concat curdir prefix old)
                                  (make-hash-table :test #'equal)))
         (now (and file (substring (file-name-nondirectory file) (length prefix)))))
    (funcall fn article
             (if (and now (not (equal now old)) (string-prefix-p ":2," now))
                 (rebase-mail-flags old new-suffix now)
               new-suffix)
             curdir)))

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
