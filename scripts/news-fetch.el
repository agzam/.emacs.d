;;; scripts/news-fetch.el --- fill local news folders from an NNTP server -*- lexical-binding: t; -*-
;;; Commentary:
;; Usage: emacs -Q --batch --init-directory DIR -l scripts/news-fetch.el
;;        --eval '(news-fetch-main "STORE" "ACTIVE-FILE" "ADDRESS")'
;; Each folder under STORE is a news group; nnmaildir delivers its new posts
;; and builds their overviews here, so a Gnus session only registers files.
;;; Code:

(require 'cl-lib)
(require 'gnus)
;; the header parser reads a decoder variable gnus-sum defines
(require 'gnus-sum)
(require 'nnmaildir)
(require 'nntp)
(require 'seq)

(defconst news-fetch-server "news"
  "Name of the nnmaildir server posts are delivered through.")

(defconst news-fetch-mark-file "news-fetched"
  "File in a group's .nnmaildir/ holding the last article number fetched.")

(defvar news-fetch-days 365
  "How many days back the first fetch of a group goes.")

(defvar news-fetch-unread-days 7
  "Posts of a first fetch younger than this many days arrive unread.")

(defvar news-fetch-chunk 1000
  "Articles per overview request.")

(defvar news-fetch-batch 400
  "Articles per pipelined ARTICLE request.")

(defvar news-fetch-active-age 86400
  "Seconds before the saved group list is fetched again.")

(defvar news-fetch-watchdog 1800
  "Seconds a run may take before it gives up.")

;;; The store

(defun news-fetch-groups (store)
  "News groups under STORE, one per folder."
  (seq-filter (lambda (name) (file-directory-p (expand-file-name name store)))
              (directory-files store nil "\\`[^.]")))

(defun news-fetch-mark-path (store group)
  "File holding the last article number fetched into GROUP under STORE."
  (expand-file-name (concat group "/.nnmaildir/" news-fetch-mark-file) store))

(defun news-fetch-read-mark (store group)
  "Last article number fetched into GROUP under STORE, or nil before the first."
  (let ((file (news-fetch-mark-path store group)))
    (when (file-exists-p file)
      (let ((number (with-temp-buffer
                      (insert-file-contents file)
                      (string-to-number (buffer-string)))))
        (and (< 0 number) number)))))

(defun news-fetch-save-mark (store group number)
  "Save NUMBER as the last article fetched into GROUP under STORE."
  (let* ((file (news-fetch-mark-path store group))
         (part (concat file ".part")))
    (make-directory (file-name-directory file) t)
    (write-region (number-to-string number) nil part nil 'silent)
    (rename-file part file t)))

(defun news-fetch-scan (store group)
  "Register GROUP's folder under STORE with nnmaildir; its table of Message-IDs.
The table grows as posts are delivered."
  (dolist (sub '("cur" "new" "tmp"))
    (make-directory (expand-file-name (concat group "/" sub) store) t))
  (nnmaildir-request-scan group news-fetch-server)
  (if-let* ((known (nnmaildir--prepare news-fetch-server group)))
      (nnmaildir--grp-mlist known)
    (error "No folder %s to deliver into: %s" group
           (nnmaildir--srv-error nnmaildir--cur-server))))

;;; The server

(defun news-fetch-active (group address)
  "Select GROUP on ADDRESS and return its range as (LOW . HIGH)."
  (unless (nntp-request-group group address)
    (error "No group %s on %s: %s" group address (nnheader-get-report 'nntp)))
  (with-current-buffer nntp-server-buffer
    (goto-char (point-min))
    (unless (re-search-forward "^211 [0-9]+ \\([0-9]+\\) \\([0-9]+\\)" nil t)
      (error "Unreadable answer to GROUP %s: %s" group (buffer-string)))
    (cons (string-to-number (match-string 1)) (string-to-number (match-string 2)))))

(defun news-fetch-overview (from to)
  "Overview rows of articles FROM to TO in the selected group, oldest first.
Each row is (NUMBER TIME MESSAGE-ID REFERENCES); TIME is nil for a date
that does not parse.  Only a range with no article answers nothing."
  (if (not (nntp-send-command-and-decode "\r?\n\\.\r?\n" "OVER" (format "%d-%d" from to)))
      ;; a lost connection answers nil too, and must not pass for an
      ;; empty range, or the saved mark would skip its posts for good
      (unless (string-prefix-p "423" (nnheader-get-report 'nntp))
        (error "No overview of %d-%d: %s" from to (nnheader-get-report 'nntp)))
    (with-current-buffer nntp-server-buffer
      (goto-char (point-min))
      (let (rows)
        (while (not (eobp))
          (let ((fields (split-string (buffer-substring (point) (line-end-position)) "\t")))
            (when (and (nth 5 fields) (string-match-p "\\`[0-9]+\\'" (car fields)))
              (push (list (string-to-number (nth 0 fields))
                          (ignore-errors (float-time (date-to-time (nth 3 fields))))
                          (nth 4 fields)
                          (nth 5 fields))
                    rows)))
          (forward-line 1))
        (nreverse rows)))))

(defun news-fetch-rows (from to)
  "Overview rows of articles FROM to TO, oldest first."
  (let (rows)
    (while (<= from to)
      (let ((end (min to (+ from news-fetch-chunk -1))))
        (setq rows (nconc rows (news-fetch-overview from end))
              from (1+ end))))
    rows))

(defun news-fetch-recent-rows (low high cutoff)
  "Overview rows between LOW and HIGH dated after CUTOFF, oldest first.
The walk goes back from HIGH and stops at the first chunk that is mostly
older than CUTOFF, so a few misdated posts neither end it nor extend it."
  (let ((to high) rows done)
    (while (and (not done) (<= low to))
      (let* ((from (max low (- to news-fetch-chunk -1)))
             (chunk (news-fetch-overview from to))
             (older (seq-count (lambda (row) (and (cadr row) (< (cadr row) cutoff))) chunk)))
        (setq rows (append chunk rows)
              to (1- from)
              done (< (length chunk) (* 2 older)))))
    (seq-remove (lambda (row) (and (cadr row) (< (cadr row) cutoff))) rows)))

(defconst news-fetch-message-id-regexp
  (let* ((atom "[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+")
         (dot-atom (concat atom "\\(?:\\." atom "\\)*")))
    (concat "<" dot-atom "@" dot-atom ">"))
  "A well-formed Message-ID: the RFC 5536 form without its quoted parts.")

(defun news-fetch-references (rows)
  "Well-formed Message-IDs the References of ROWS name, each once.
The server answers a malformed one, or one longer than 250, with an error
the pipelined requests of nntp.el never count: they wait until it hangs up."
  (let (ids)
    (dolist (row rows)
      (let ((references (or (nth 3 row) ""))
            (start 0))
        (while (string-match news-fetch-message-id-regexp references start)
          (when (<= (- (match-end 0) (match-beginning 0)) 250)
            (push (match-string 0 references) ids))
          (setq start (match-end 0)))))
    (seq-uniq (nreverse ids))))

(defun news-fetch-parents (rows known)
  "Message-IDs the References of ROWS name, outside ROWS and the table KNOWN."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (row rows)
      (puthash (nth 2 row) t ids))
    (seq-remove (lambda (id) (or (gethash id ids) (gethash id known)))
                (news-fetch-references rows))))

;;; Delivery

(defun news-fetch-articles (articles group address)
  "Texts of ARTICLES, numbers or Message-IDs, from GROUP on ADDRESS.
An article the server lacks has no text and is left out."
  (let ((map (nntp-retrieve-articles articles group address)))
    (with-current-buffer nntp-server-buffer
      (seq-remove #'string-empty-p
                  (cl-mapcar (lambda (entry end) (buffer-substring (cdr entry) end))
                             map
                             (append (mapcar #'cdr (cdr map)) (list (point-max))))))))

(defun news-fetch-deliver (articles group address &optional read)
  "Deliver ARTICLES of GROUP on ADDRESS into GROUP's folder, marked READ or not.
ARTICLES are numbers or Message-IDs; each batch is one pipelined request,
marked as it lands, so a run cut short leaves no read post unread.
Returns how many the server had."
  (let ((count 0))
    (while articles
      (let (numbers)
        (dolist (text (news-fetch-articles (seq-take articles news-fetch-batch) group address))
          (with-temp-buffer
            (insert text)
            (when-let* ((delivered (nnmaildir-request-accept-article group news-fetch-server)))
              (push (cdr delivered) numbers))))
        (when (and read numbers)
          (nnmaildir-request-set-mark
           group `((,(gnus-compress-sequence (sort numbers #'<) t) add (read)))
           news-fetch-server))
        (setq count (+ count (length numbers))
              articles (nthcdr news-fetch-batch articles))))
    count))

(defun news-fetch-group (store group address)
  "Fetch GROUP's new posts from ADDRESS into its folder under STORE.
Return (GROUP NEW OLDER SECONDS).  A first fetch covers `news-fetch-days',
unread only for the last `news-fetch-unread-days'; a later one takes what
came after the mark.  Older posts they refer to come first, read."
  (let* ((start (float-time))
         (known (news-fetch-scan store group))
         (range (news-fetch-active group address))
         (mark (news-fetch-read-mark store group))
         ;; a mark past the server's end means it renumbered the group
         (first (not (and mark (<= mark (cdr range)))))
         (fetched (if first
                      (news-fetch-recent-rows (car range) (cdr range)
                                              (- start (* news-fetch-days 86400)))
                    (news-fetch-rows (1+ mark) (cdr range))))
         (rows (seq-remove (lambda (row) (gethash (nth 2 row) known)) fetched))
         ;; before the rows, since an entry shows the highest numbers as the
         ;; newest posts; over all of FETCHED, since a run cut short left
         ;; its delivered posts known and their parents owed
         (older (news-fetch-deliver (news-fetch-parents fetched known) group address t))
         (unread-after (- start (* news-fetch-unread-days 86400)))
         (old-p (lambda (row) (and first (cadr row) (< (cadr row) unread-after))))
         (new (+ (news-fetch-deliver (mapcar #'car (seq-filter old-p rows)) group address t)
                 (news-fetch-deliver (mapcar #'car (seq-remove old-p rows)) group address))))
    (news-fetch-save-mark store group (cdr range))
    (list group new older (- (float-time) start))))

;;; The group list

(defun news-fetch-active-stale-p (file)
  "Non-nil when the group list in FILE is missing or older than a day."
  (or (not (file-exists-p file))
      (< news-fetch-active-age
         (float-time (time-since (file-attribute-modification-time (file-attributes file)))))))

(defun news-fetch-save-active (file address)
  "Save the group list of ADDRESS into FILE, one \"GROUP HIGH LOW FLAG\" line each."
  (unless (nntp-request-list address)
    (error "No group list from %s: %s" address (nnheader-get-report 'nntp)))
  (let ((part (concat file ".part")))
    (make-directory (file-name-directory file) t)
    (with-current-buffer nntp-server-buffer
      (write-region nil nil part nil 'silent))
    (rename-file part file t)))

;;; Entry point

(defun news-fetch-run (store active-file address &optional port)
  "Fetch every news group under STORE from ADDRESS, reached at PORT.
The group list goes into ACTIVE-FILE when it is stale.  Returns one
\(GROUP NEW OLDER SECONDS) or (GROUP error MESSAGE) row per group."
  (let ((gnus-newsrc-hashtb (gnus-make-hashtable))
        (gnus-verbose-backends 0)
        ;; the server needs no login, and ~/.authinfo.gpg would want gpg
        (auth-sources nil)
        (defs `((directory ,store) (get-new-mail nil))))
    (unless (nnmaildir-open-server news-fetch-server defs)
      (error "No store %s to deliver into: %s" store
             (nnmaildir--srv-error nnmaildir--cur-server)))
    ;; with a second group registered, nnmaildir resolves its method
    ;; through Gnus's server tables, which no Gnus session filled here
    (setf (nnmaildir--srv-method nnmaildir--cur-server)
          `(nnmaildir ,news-fetch-server ,@defs))
    (unwind-protect
        (progn
          (unless (nntp-open-server address `((nntp-address ,address)
                                              ,@(and port `((nntp-port-number ,port)))))
            (error "No connection to %s: %s" address (nnheader-get-report 'nntp)))
          (append (cl-letf (((symbol-function 'unix-sync) #'ignore))
                    ;; nnmaildir syncs every disk after each delivery,
                    ;; and each later write waits for it: 114 ms a post
                    ;; instead of 8
                    (mapcar (lambda (group)
                              (condition-case err
                                  (news-fetch-group store group address)
                                (error (list group 'error (error-message-string err)))))
                            (news-fetch-groups store)))
                  (condition-case err
                      (progn
                        (when (news-fetch-active-stale-p active-file)
                          (news-fetch-save-active active-file address))
                        nil)
                    (error (list (list "group list" 'error (error-message-string err)))))))
      (nntp-close-server address)
      ;; nnmaildir-close-server does nothing while no group is selected,
      ;; and a later run in this process must not inherit this store
      (setf (alist-get news-fetch-server nnmaildir--servers nil 'remove #'equal) nil)
      (setq nnmaildir--cur-server nil))))

(defun news-fetch-main (store active-file address &optional port)
  "Fetch the news groups under STORE from ADDRESS at PORT, report, and exit.
ACTIVE-FILE receives the server's group list once a day."
  (run-at-time news-fetch-watchdog nil
               (lambda ()
                 (message "news-fetch: no end after %d s" news-fetch-watchdog)
                 (kill-emacs 2)))
  (let (failed)
    (condition-case err
        (dolist (row (news-fetch-run store active-file address port))
          (pcase row
            (`(,group error ,text)
             (setq failed t)
             (message "%s: %s" group text))
            (`(,group ,new ,older ,seconds)
             (message "%s: %d new, %d older for replies, %.1f s" group new older seconds))))
      (error
       (setq failed t)
       (message "news-fetch: %s" (error-message-string err))))
    (kill-emacs (if failed 1 0))))

;;; news-fetch.el ends here
