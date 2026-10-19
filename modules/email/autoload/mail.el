;;; modules/email/autoload/mail.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'gnus-search)
(require 'gnus-sum)
(require 'nnmaildir)
(require 'url-util)

(defvar gmail-maildir)
(defvar mail-inbox-group)
(defvar mail-trash-group)
(defvar mail-archive-group)
(defvar mail-groups)
(defvar mail-bulk-groups)
(defvar gnus-topic-alist)

;;; What a sync changed

(defun mail-sync-state-time (group)
  "When mbsync last wrote its state file in GROUP's folder, or nil."
  (when-let* ((store (mail-group-store group)))
    (file-attribute-modification-time
     (file-attributes (expand-file-name (concat (gnus-group-real-name group) "/.mbsyncstate")
                                        store)))))

;;;###autoload
(defun refresh-synced-mail-groups (since)
  "Read again, in the background, each routine group a sync changed SINCE.
mbsync rewrites a folder's state file only when it changed the folder.
A label made or dropped meanwhile gains or loses its group first."
  (when (gnus-alive-p)
    (follow-mail-folders)
    ;; a file system may keep whole seconds
    (let ((since (time-convert since 'integer)))
      (when-let* ((changed (seq-filter (lambda (group)
                                         (when-let* ((time (mail-sync-state-time group)))
                                           (not (time-less-p time since))))
                                       (scanned-mail-groups))))
        (load-mail-groups changed)))))

;;; Navigation

;;;###autoload
(defun subscribe-mail-group (group)
  "Subscribe GROUP unless the newsrc lists it or it was killed; t if it did.
Gnus resolves a server it has not opened to a bare method and keeps that
in the newsrc, dropping what `gnus-secondary-select-methods' defines."
  (unless (or (gnus-group-entry group) (member group gnus-killed-list))
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
  "Subscribe every maildir group plus `mail-groups' Gnus lacks; return those.
Subscribing activates, and gnus-search silently drops a hit whose group
nnmaildir never opened - notmuch returns whichever duplicate it likes.
The bulk groups move out of the routine scan, a new label into its topic."
  (let ((new (seq-filter #'subscribe-mail-group (append (maildir-groups) mail-groups))))
    (defer-bulk-mail-groups)
    (when new
      (apply-mail-topics))
    new))

;;;###autoload
(defun drop-vanished-mail-groups ()
  "Kill each Gmail group whose folder is gone, and forget what was read of it.
A label dropped in Gmail, or deleted or renamed here, leaves no folder.
Nothing goes while the inbox folder is missing.  Return the groups killed."
  (when-let* (((file-directory-p (expand-file-name (gnus-group-real-name mail-inbox-group)
                                                   gmail-maildir)))
              (gone (seq-filter (lambda (group)
                                  (and (string-prefix-p "nnmaildir+gmail:" group)
                                       (not (file-directory-p
                                             (expand-file-name (gnus-group-real-name group)
                                                               gmail-maildir)))))
                                gnus-group-list)))
    (dolist (group gone)
      (forget-mail-group group)
      (gnus-group-change-level group gnus-level-killed (gnus-group-level group))
      ;; a label made again under this name is subscribed again
      (setq gnus-killed-list (delete group gnus-killed-list))
      (gnus-set-active group nil)
      (dolist (topic gnus-topic-alist)
        (setcdr topic (delete group (cdr topic)))))
    (when (gnus-buffer-live-p gnus-group-buffer)
      (with-current-buffer gnus-group-buffer
        (gnus-group-list-groups)))
    (refresh-mail-labels-soon)
    (message "Groups gone with their folders: %s"
             (mapconcat #'gnus-group-short-name gone ", "))
    gone))

;;;###autoload
(defun follow-mail-folders ()
  "Kill each Gmail group whose folder went, and subscribe and read each new one."
  (when (gnus-alive-p)
    (drop-vanished-mail-groups)
    (when-let* ((new (subscribe-mail-groups)))
      (load-mail-groups new))))

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
  "Read the maildir groups a routine scan covers again, in the background."
  (interactive)
  (load-mail-groups (scanned-mail-groups)))

;;; Reading the store without blocking

;;;###autoload
(defun defer-mail-server-scan-a (fn &optional group server)
  "Call FN to scan GROUP on SERVER only when nnmaildir holds GROUP already.
A group it lacks would be read whole in the main thread, and one a batch
Emacs reads now would be numbered twice; the batch reads both.  A scan
of the whole server reads the routine groups the same way."
  (if-let* (((stringp group))
            (full (mail-full-group-name group server)))
      (cond ((mail-group-reading-p full) t)
            ((not (mail-group-loaded-p full))
             (load-mail-groups (list full) t)
             t)
            (t (funcall fn group server)))
    (unless group
      (refresh-mail-groups))
    t))

;;;###autoload
(defun scan-mail-group-on-miss-a (fn base-name group server)
  "Call FN for BASE-NAME in GROUP on SERVER; on a miss, scan GROUP and retry.
gnus-search maps each notmuch hit through this, and a hit can be mail
that arrived after the group was read."
  (or (funcall fn base-name group server)
      (when-let* ((full (mail-full-group-name group server))
                  ((mail-group-loaded-p full)))
        (nnmaildir-request-scan group server)
        (funcall fn base-name group server))))

;;;###autoload
(defun open-mail-inbox ()
  "Start Gnus if needed and enter `mail-inbox-group' once it is read afresh."
  (interactive)
  (unless (gnus-alive-p)
    (gnus))
  (subscribe-mail-group mail-inbox-group)
  (load-mail-groups (list mail-inbox-group) t)
  (wait-for-mail-load 'open "to open the inbox"
                      (lambda () (not (mail-group-pending-p mail-inbox-group)))
                      (lambda ()
                        (if (mail-group-loaded-p mail-inbox-group)
                            (with-current-buffer gnus-group-buffer
                              (gnus-summary-read-group mail-inbox-group t t))
                          (message "The inbox did not load")))))

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
  (when-mail-loaded "for the search" (lambda () (search-mail-now query limit))))

;;;###autoload
(defun when-mail-loaded (what action)
  "Call ACTION with Gnus running once no maildir group is loading.
A search maps its hits to groups nnmaildir holds.  WHAT says in the
progress messages what waits, such as \"for the search\"."
  (unless (gnus-alive-p)
    (gnus))
  ;; mbsync creates a group dir the moment a label appears
  (subscribe-mail-groups)
  (load-unloaded-mail-groups)
  (if (mail-load-busy-p)
      (wait-for-mail-load 'search what (lambda () (not (mail-load-busy-p)))
                          (lambda () (when-mail-loaded what action)))
    (funcall action)))

;;;###autoload
(defun search-mail-now (query limit)
  "Read the ephemeral group of the newest LIMIT messages matching QUERY.
Returns the group's name, nil when nothing matched."
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
                 "narrow the query to reach older ones")))
    group))

;;;###autoload
(defun mail-file-group (file)
  "Group of FILE, a message file in the store of an nnmaildir server, or nil."
  (seq-some (lambda (method)
              (when-let* (((eq (car method) 'nnmaildir))
                          (root (file-name-as-directory
                                 (expand-file-name (cadr (assq 'directory (cddr method))))))
                          ((string-prefix-p root file)))
                (format "nnmaildir+%s:%s" (cadr method)
                        (car (split-string (substring file (length root)) "/")))))
            gnus-secondary-select-methods))

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

(defun mail-base-without-uid (base)
  "BASE, a message file's name before its flags, without the UID mbsync adds."
  (if (string-match ",U=[0-9]+\\'" base)
      (substring base 0 (match-beginning 0))
    base))

(defun maildir-file-names (dir)
  "Hash of each message file's name in maildir directory DIR by its base name."
  (let* ((files (directory-files dir nil "\\`[^.]" t))
         (names (make-hash-table :test #'equal :size (length files))))
    (dolist (name files)
      (puthash (substring name 0 (string-search ":" name)) name names))
    names))

(defun maildir-uploads (names)
  "Hash of each base name in NAMES carrying mbsync's UID, by the base without it.
NAMES is a `maildir-file-names' hash."
  (let ((uploads (make-hash-table :test #'equal)))
    (maphash (lambda (base _)
               (when-let* ((at (string-match-p ",U=[0-9]+\\'" base)))
                 (puthash (substring base 0 at) base uploads)))
             names)
    uploads))

;;;###autoload
(defun current-mail-file (file listings)
  "FILE under the name it has now, or nil once it is gone.
A scan, a saved flag and mbsync's upload rename a file, and notmuch
learns the new name at its next run.  LISTINGS caches the cur/
directories read so far, each by `maildir-file-names'."
  (if (file-exists-p file)
      file
    (let* ((cur (expand-file-name
                 "cur" (file-name-directory (directory-file-name (file-name-directory file)))))
           (names (with-memoization (gethash cur listings)
                    (maildir-file-names cur)))
           (base (car (split-string (file-name-nondirectory file) ":")))
           (name (or (gethash base names)
                     (when-let* ((uploaded (gethash (mail-base-without-uid base)
                                                    (with-memoization
                                                        (gethash (cons 'uploads cur) listings)
                                                      (maildir-uploads names)))))
                       (gethash uploaded names)))))
      (and name (expand-file-name name cur)))))

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

;;;###autoload
(defun accept-mail-without-nul-a (fn &rest args)
  "Call FN with ARGS to file the current buffer's article, NUL bytes dropped.
Gmail closes the connection on a NUL in an upload, and mbsync then fails
that folder on every run.  nnmaildir writes the buffer when it has no
`nnmaildir--file' to link."
  (if (save-excursion
        (goto-char (point-min))
        (search-forward "\0" nil t))
      (let ((nnmaildir--file nil))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "\0" nil t)
            (replace-match "" t t)))
        (apply fn args))
    (apply fn args)))

;;; Names mbsync changed
;;
;; nnmaildir builds a message's file name from what it remembers, and
;; drops the message once no file has that name.

(defvar mail-names-checked (make-hash-table :test #'eq :weakness 'key)
  "Each nnmaildir group's cur/ time when its file names were last checked.")

(defun follow-renamed-mail (group dir)
  "Point nnmaildir GROUP's articles at their files in maildir DIR, as renamed.
mbsync renames a file when a flag changes elsewhere, and adds a UID once
it uploaded the file.  A name GROUP knows already and a file gone for
good are left to nnmaildir."
  (let* ((cur (nnmaildir--cur dir))
         (time (file-attribute-modification-time (file-attributes cur)))
         (flist (nnmaildir--grp-flist group))
         (then (gethash group mail-names-checked)))
    (unless (or (null time) (not (hash-table-p flist))
                (and then (time-equal-p time then)))
      (puthash group time mail-names-checked)
      (let ((names (maildir-file-names cur))
            missing)
        (maphash (lambda (prefix article)
                   (if-let* ((name (gethash prefix names)))
                       (let ((suffix (nnmaildir--art-suffix article)))
                         (unless (and suffix (eq t (compare-strings suffix nil nil
                                                                    name (length prefix) nil)))
                           (setf (nnmaildir--art-suffix article) (substring name (length prefix)))))
                     (push article missing)))
                 flist)
        ;; only a file Gnus wrote lacks the UID mbsync gives an upload
        (when-let* ((wrote (seq-remove (lambda (article)
                                         (string-search ",U=" (nnmaildir--art-prefix article)))
                                       missing))
                    (uploads (maildir-uploads names)))
          (dolist (article wrote)
            (when-let* ((base (gethash (nnmaildir--art-prefix article) uploads))
                        ((not (gethash base flist))))
              (remhash (nnmaildir--art-prefix article) flist)
              (setf (nnmaildir--art-prefix article) base
                    (nnmaildir--art-suffix article) (substring (gethash base names) (length base)))
              (puthash base article flist))))))))

(defun follow-renamed-mail-group (gname server)
  "Run `follow-renamed-mail' on group GNAME of nnmaildir SERVER, if it holds one."
  (when-let* ((group (nnmaildir--prepare server gname)))
    (follow-renamed-mail group (nnmaildir--srvgrp-dir (nnmaildir--srv-dir nnmaildir--cur-server)
                                                      (nnmaildir--grp-name group)))))

;;;###autoload
(defun follow-renamed-mail-a (_articles &optional gname server &rest _)
  "Bring nnmaildir's file names in group GNAME on SERVER up to date first."
  (follow-renamed-mail-group gname server))

;;;###autoload
(defun follow-renamed-mail-marks-a (gname _info &optional server)
  "Bring nnmaildir's file names in group GNAME on SERVER up to date first."
  (follow-renamed-mail-group gname server))

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

(defconst emacs-list-inboxes
  '(("emacs-devel" . "emacs-devel")
    ("help-gnu-emacs" . "emacs-user")
    ("bug-gnu-emacs" . "emacs-bugs")
    ("emacs-orgmode" . "orgmode")
    ("emacs-tangents" . "emacs-tangents"))
  "Emacs lists at gnu.org, each with the yhetil.org inbox that archives it.")

(defun bare-message-id (message-id)
  "MESSAGE-ID without its angle brackets."
  (string-trim message-id "<" ">"))

;;;###autoload
(defun gmail-message-url (message-id)
  "Gmail web URL of the message with MESSAGE-ID."
  (concat "https://mail.google.com/mail/u/0/#search/"
          (url-hexify-string (concat "rfc822msgid:" (bare-message-id message-id)))))

(defun yhetil-message-url (inbox message-id &optional thread)
  "URL of the message with MESSAGE-ID in the yhetil.org INBOX.
With THREAD, the URL of its thread, nested by reply, scrolled to it."
  (format "https://yhetil.org/%s/%s/%s" inbox
          (url-hexify-string (bare-message-id message-id))
          (if thread "t/#u" "")))

(defun emacs-list-inbox ()
  "The yhetil.org inbox of the Emacs list the head in this buffer went to.
List-Id names the list, else To or Cc do: an own sent copy has no List-Id."
  (let ((text (concat (mail-fetch-field "List-Id") " "
                      (mail-fetch-field "To") " " (mail-fetch-field "Cc")))
        (start 0)
        inbox)
    (while (and (not inbox)
                (string-match "\\([[:alnum:]_-]+\\)[.@]gnu\\.org" text start))
      (setq inbox (cdr (assoc-string (match-string 1 text) emacs-list-inboxes t))
            start (match-end 0)))
    inbox))

(defun archived-at-url ()
  "URL the Archived-At header in this buffer names, or nil.
Gmane's permalink service is gone, so its URLs count as none."
  (when-let* ((value (mail-fetch-field "Archived-At"))
              (url (string-trim value "[ \t\n<]+" "[ \t\n>]+"))
              ((not (string-match-p "\\`https?://permalink\\.gmane\\.org/" url))))
    url))

(defun list-archive-message-url (head)
  "Public archive URL of the message with HEAD, or nil.
That is where its Archived-At header points, else its page in the
archive of its mailing list."
  (with-temp-buffer
    (insert head)
    (let ((id (mail-fetch-field "Message-ID"))
          (recipients (concat (mail-fetch-field "To") " " (mail-fetch-field "Cc"))))
      (or (archived-at-url)
          (when-let* ((inbox (emacs-list-inbox)))
            (yhetil-message-url inbox id))
          (and (string-match "\\([[:alnum:]._-]+\\)@googlegroups\\.com" recipients)
               (concat "https://groups.google.com/forum/#!topicsearchin/"
                       (match-string 1 recipients) "/messageid$3A"
                       (url-hexify-string (concat "\"" (bare-message-id id) "\""))))))))

(defun mail-web-url (group head)
  "URL of the web page of the message with HEAD that GROUP holds, or nil.
Emacs list mail opens in its yhetil.org thread, other Gmail mail in
Gmail, and the rest where its Archived-At header points."
  (with-temp-buffer
    (insert head)
    (let ((id (mail-fetch-field "Message-ID")))
      (or (when-let* ((inbox (emacs-list-inbox)))
            (yhetil-message-url inbox id t))
          (and (string-prefix-p "nnmaildir+gmail:" group)
               (gmail-message-url id))
          (archived-at-url)))))

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

(defun mail-head-on-screen ()
  "Group holding the message on screen and its head, as (GROUP . HEAD)."
  (pcase-let ((`(,summary . ,article) (mail-on-screen)))
    (with-current-buffer summary
      (let ((group (mail-article-group article)))
        (unless (gnus-request-head article gnus-newsgroup-name)
          (user-error "Cannot read this message"))
        (cons group (with-current-buffer nntp-server-buffer
                      (buffer-string)))))))

;;;###autoload
(defun open-mail-on-web ()
  "Open the message at point where it lives on the web.
Emacs list mail opens in its yhetil.org thread, other Gmail mail in
Gmail, and the rest where its Archived-At header points."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (pcase-let ((`(,group . ,head) (mail-head-on-screen)))
    (browse-url (or (mail-web-url group head)
                    (user-error "No web page known for this message of %s" group)))))

;;;###autoload
(defun open-message-in-list-archive ()
  "Open the message at point in its public archive.
That is where its Archived-At header points, else its page in the
archive of its mailing list."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (browse-url (or (list-archive-message-url (cdr (mail-head-on-screen)))
                  (user-error "No public archive known for this message"))))

;;; mail.el ends here
