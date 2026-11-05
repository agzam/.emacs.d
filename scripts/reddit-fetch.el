;;; scripts/reddit-fetch.el --- fill local subreddit folders from Reddit's JSON -*- lexical-binding: t; -*-
;;; Commentary:
;; Usage: emacs -Q --batch --init-directory DIR -l scripts/reddit-fetch.el
;;        --eval '(reddit-fetch-main "STORE" "BASE-URL")'
;; Each folder r.NAME under STORE holds subreddit NAME, every thread whole and
;; threaded by reply.  Reddit serves its JSON only to a browser on a login.
;;; Code:

(require 'cl-lib)
(require 'gnus)
;; the header parser reads a decoder variable gnus-sum defines
(require 'gnus-sum)
(require 'nnmaildir)
(require 'rfc2047)
(require 'seq)
(require 'subr-x)
(require 'xml)

(defconst reddit-fetch-server "reddit"
  "Name of the nnmaildir server posts and comments are delivered through.")

(defconst reddit-fetch-known-file "reddit-fetched"
  "File in a folder's .nnmaildir/ listing the ids ever delivered there.")

(defconst reddit-fetch-threads-file "reddit-threads"
  "File in a folder's .nnmaildir/ holding the comment count of each thread fetched.")

(defconst reddit-fetch-scores-file "reddit-scores"
  "File in a folder's .nnmaildir/ holding each message's score and the login's vote.")

(defvar reddit-fetch-limit 100
  "Posts, and recent comments, a listing is asked for.")

(defvar reddit-fetch-thread-limit 500
  "Comments a thread is asked for.")

(defvar reddit-fetch-max-threads 60
  "Threads a run fetches at most, the rest waiting for the next run.
Reddit answers a login 100 requests in 10 minutes, the user's own among them.")

(defvar reddit-fetch-unread-days 7
  "Posts and comments older than this many days arrive read.")

(defvar reddit-fetch-watchdog 1800
  "Seconds a run may take before it gives up.")

;;; Requests

(defvar reddit-fetch-browser
  (seq-find #'file-executable-p
            (delq nil (list "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
                            (executable-find "brave-browser")
                            (executable-find "brave"))))
  "Chromium-family browser whose headless runs fetch Reddit's JSON.")

(defvar reddit-fetch-cookies
  (seq-find #'file-exists-p
            (mapcar #'expand-file-name
                    '("~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Cookies"
                      "~/.config/BraveSoftware/Brave-Browser/Default/Cookies")))
  "Cookie database of the `reddit-fetch-browser' profile logged into Reddit.")

(defvar reddit-fetch-user-agent
  (format "Mozilla/5.0 (%s) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36"
          (if (eq system-type 'darwin) "Macintosh; Intel Mac OS X 10_15_7" "X11; Linux x86_64"))
  "User-Agent of the headless browser; Reddit blocks one naming HeadlessChrome.")

(defvar reddit-fetch-timeout 30
  "Seconds a request may take; a blocked one can hang the browser.")

(defvar reddit-fetch-api "https://api.reddit.com"
  "Host that answers a login with Reddit's JSON.
Paths carry no .json suffix: Reddit refuses .json listings on every host.")

(defvar reddit-fetch-profile nil
  "Browser profile of the current run, made at its first request.")

(defun reddit-fetch-copy-cookies (source dest)
  "Write to DEST the Reddit cookies of the cookie database SOURCE.
The browser may be writing SOURCE, so its WAL files come along."
  (unless (and source (file-exists-p source))
    (error "No browser cookie database at %s" source))
  (make-directory (file-name-directory dest) t)
  (dolist (suffix '("" "-wal" "-shm"))
    (when (file-exists-p (concat source suffix))
      (copy-file (concat source suffix) (concat dest suffix) t)))
  (let ((db (sqlite-open dest)))
    (unwind-protect
        (sqlite-execute db "DELETE FROM cookies WHERE host_key NOT LIKE '%reddit.com'")
      (sqlite-close db))))

(defun reddit-fetch-ensure-profile ()
  "The browser profile of this run, holding the user's Reddit cookies."
  (or reddit-fetch-profile
      (let ((profile (make-temp-file "reddit-fetch-profile" t)))
        (reddit-fetch-copy-cookies reddit-fetch-cookies
                                   (expand-file-name "Default/Cookies" profile))
        (setq reddit-fetch-profile profile))))

(defun reddit-fetch-browser-command (url profile)
  "Command line of a headless browser over PROFILE printing the page at URL."
  (unless reddit-fetch-browser
    (error "No Chromium-family browser to fetch Reddit with"))
  (list reddit-fetch-browser "--headless=new"
        (concat "--user-data-dir=" profile)
        (concat "--user-agent=" reddit-fetch-user-agent)
        "--no-first-run" "--no-default-browser-check" "--dump-dom" url))

(defun reddit-fetch-parse (text)
  "Reddit's JSON TEXT, parsed, false as :false and null as nil.
The login's vote on a thing is true, false or null."
  (json-parse-string text :object-type 'hash-table :null-object nil :false-object :false))

(defun reddit-fetch-page-json (page)
  "The JSON a browser's dump PAGE of a JSON document holds, parsed, or nil.
The document holds it escaped in one <pre>; `&amp;' unescapes last, once.
A regexp spanning the <pre> overflows on a listing past a megabyte."
  (when (string-match "<pre[^>]*>" page)
    (let ((start (match-end 0)))
      (when (string-match "</pre>" page start)
        (let ((text (substring page start (match-beginning 0))))
          (pcase-dolist (`(,entity . ,char) '(("&lt;" . "<") ("&gt;" . ">")
                                               ("&nbsp;" . "\u00a0") ("&amp;" . "&")))
            (setq text (string-replace entity char text)))
          (reddit-fetch-parse text))))))

(defun reddit-fetch-json (url)
  "The parsed JSON at URL, read by a headless browser on the user's Reddit login."
  (let ((output (generate-new-buffer " *reddit-fetch-browser*"))
        (errors (generate-new-buffer " *reddit-fetch-browser-errors*"))
        (deadline (+ (float-time) reddit-fetch-timeout))
        process)
    (unwind-protect
        (progn
          (setq process (make-process :name "reddit-fetch-browser" :buffer output :stderr errors
                                      :command (reddit-fetch-browser-command
                                                url (reddit-fetch-ensure-profile))
                                      :coding 'utf-8 :connection-type 'pipe :noquery t))
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output process 0.1))
          (when (process-live-p process)
            (error "%s: no answer in %d s" url reddit-fetch-timeout))
          ;; output can still be on its way when the exit is seen
          (while (accept-process-output process 0.05))
          (or (reddit-fetch-page-json (with-current-buffer output (buffer-string)))
              (error "%s: no JSON in the page; a rate limit, a block or a lost login" url)))
      (when process
        (delete-process process))
      (when-let* ((pipe (get-buffer-process errors)))
        (delete-process pipe))
      (kill-buffer output)
      (kill-buffer errors))))

(defun reddit-fetch-children (listing)
  "The things of Reddit's LISTING, as a list."
  (append (gethash "children" (gethash "data" listing)) nil))

;;; Entries

(defun reddit-fetch-entry (thing base &optional title ancestors)
  "Reddit's THING, a post or a comment, as a plist; its link leads to BASE.
A comment carries its post's TITLE and the ids of its ANCESTORS, top first.
Its :likes is the login's vote: 1 up, -1 down, nil for none."
  (let* ((data (gethash "data" thing))
         (comment (equal (gethash "kind" thing) "t1"))
         (html (or (gethash (if comment "body_html" "selftext_html") data) "")))
    (cl-flet ((value (key)
                (let ((value (gethash key data)))
                  (unless (eq value :false) value))))
      (list :id (gethash "name" data)
            :post (if comment
                      (string-remove-prefix "t3_" (gethash "link_id" data))
                    (gethash "id" data))
            :comment (and comment (gethash "id" data))
            :ancestors ancestors
            :author (concat "u/" (or (gethash "author" data) "[deleted]"))
            :title (if comment title (gethash "title" data))
            :time (gethash "created_utc" data)
            :edited (value "edited")
            :link (concat base (gethash "permalink" data))
            :html (if (or comment (value "is_self"))
                      html
                    (let ((url (xml-escape-string (gethash "url" data))))
                      (format "%s<p><a href=\"%s\">%s</a></p>" html url url)))
            :comments (gethash "num_comments" data)
            :score (gethash "score" data)
            :hidden (value (if comment "score_hidden" "hide_score"))
            :likes (pcase (gethash "likes" data) ('t 1) (:false -1))))))

(defun reddit-fetch-thread (json base)
  "The post, then every comment, of the thread JSON; links lead to BASE.
A comment comes after its parent."
  (let* ((post (reddit-fetch-entry (car (reddit-fetch-children (aref json 0))) base))
         (title (plist-get post :title))
         comments)
    (cl-labels ((walk (things ancestors)
                  (dolist (thing things)
                    (when (equal (gethash "kind" thing) "t1")
                      (let ((entry (reddit-fetch-entry thing base title ancestors))
                            (replies (gethash "replies" (gethash "data" thing))))
                        (push entry comments)
                        ;; a comment without replies holds "" there
                        (when (hash-table-p replies)
                          (walk (reddit-fetch-children replies)
                                (append ancestors (list (plist-get entry :comment))))))))))
      (walk (reddit-fetch-children (aref json 1)) nil))
    (cons post (nreverse comments))))

(defun reddit-fetch-oldest-first (entries)
  "ENTRIES sorted oldest first."
  (sort (copy-sequence entries)
        (lambda (a b) (time-less-p (plist-get a :time) (plist-get b :time)))))

(defun reddit-fetch-subreddit (group)
  "Subreddit of the folder GROUP, named r.NAME."
  (string-remove-prefix "r." group))

;;; Messages

(defun reddit-fetch-message-id (post &optional comment)
  "Message-ID of POST, or of COMMENT on POST."
  (format "<%s@reddit.com>" (if comment (concat comment "." post) post)))

(defun reddit-fetch-entry-message-id (entry)
  "Message-ID of ENTRY."
  (reddit-fetch-message-id (plist-get entry :post) (plist-get entry :comment)))

(defun reddit-fetch-from (author)
  "From header value of AUTHOR, u/NAME, at an address nothing delivers to."
  (format "\"%s\" <%s@reddit.invalid>"
          author
          (replace-regexp-in-string "[^[:alnum:]_-]" "_" (string-remove-prefix "u/" author))))

(defun reddit-fetch-date (time)
  "TIME, in seconds since the epoch, as a Date header value."
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T +0000" time t)))

(defun reddit-fetch-edited (entry)
  "When ENTRY was last edited, as its X-Edited header says it, or nil."
  (let ((edited (plist-get entry :edited)))
    ;; for some old edits Reddit says true instead of the time
    (and (numberp edited) (reddit-fetch-date edited))))

(defun reddit-fetch-message (entry)
  "The message of ENTRY, a reply to its last ancestor, or to its post."
  (let* ((post (plist-get entry :post))
         (comment (plist-get entry :comment))
         (references (when comment
                       (cons (reddit-fetch-message-id post)
                             (mapcar (lambda (ancestor) (reddit-fetch-message-id post ancestor))
                                     (plist-get entry :ancestors))))))
    (concat "From: " (reddit-fetch-from (plist-get entry :author)) "\n"
            "Subject: " (rfc2047-encode-string
                         (concat (if comment "Re: " "") (plist-get entry :title)))
            "\n"
            "Date: " (reddit-fetch-date (plist-get entry :time)) "\n"
            (if-let* ((edited (reddit-fetch-edited entry)))
                (concat "X-Edited: " edited "\n")
              "")
            "Message-ID: " (reddit-fetch-entry-message-id entry) "\n"
            (if references
                (format "References: %s\nIn-Reply-To: %s\n"
                        (string-join references " ") (car (last references)))
              "")
            "Archived-At: <" (or (plist-get entry :link) "") ">\n"
            "MIME-Version: 1.0\n"
            "Content-Type: text/html; charset=utf-8\n"
            "Content-Transfer-Encoding: 8bit\n\n"
            (plist-get entry :html) "\n")))

;;; The store

(defun reddit-fetch-groups (store)
  "Subreddit folders under STORE."
  (seq-filter (lambda (name) (file-directory-p (expand-file-name name store)))
              (directory-files store nil "\\`[^.]")))

(defun reddit-fetch-scan (store group)
  "Register GROUP's folder under STORE with nnmaildir; its table of Message-IDs."
  (dolist (sub '("cur" "new" "tmp"))
    (make-directory (expand-file-name (concat group "/" sub) store) t))
  (nnmaildir-request-scan group reddit-fetch-server)
  (if-let* ((known (nnmaildir--prepare reddit-fetch-server group)))
      (nnmaildir--grp-mlist known)
    (error "No folder %s to deliver into: %s" group
           (nnmaildir--srv-error nnmaildir--cur-server))))

(defun reddit-fetch-state-file (store group name)
  "File NAME in GROUP's .nnmaildir/ under STORE."
  (expand-file-name (concat group "/.nnmaildir/" name) store))

(defun reddit-fetch-read-state (store group name)
  "The value saved as NAME for GROUP under STORE, or nil."
  (let ((file (reddit-fetch-state-file store group name)))
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (read (current-buffer))))))

(defun reddit-fetch-save-state (store group name value)
  "Save VALUE as NAME for GROUP under STORE."
  (let* ((file (reddit-fetch-state-file store group name))
         (part (concat file ".part"))
         (coding-system-for-write 'utf-8)
         (print-length nil)
         (print-level nil))
    (make-directory (file-name-directory file) t)
    (with-temp-file part
      (prin1 value (current-buffer)))
    (rename-file part file t)))

(defun reddit-fetch-deliver (group entry read)
  "Deliver ENTRY into GROUP, marked READ or not."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string (reddit-fetch-message entry) 'utf-8))
    (when-let* ((delivered (nnmaildir-request-accept-article group reddit-fetch-server)))
      (when read
        (nnmaildir-request-set-mark group `((,(list (cdr delivered)) add (read)))
                                    reddit-fetch-server))
      (cdr delivered))))

(defun reddit-fetch-file (store group article)
  "File of nnmaildir ARTICLE in folder GROUP under STORE, as named now, or nil.
The session renames a file when it saves a flag."
  (car (directory-files (expand-file-name (concat group "/cur") store) t
                        (concat "\\`" (regexp-quote (nnmaildir--art-prefix article))
                                "\\(?::\\|\\'\\)"))))

(defun reddit-fetch-stored-edit (file)
  "The X-Edited header of the message in FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (narrow-to-region (point-min) (or (search-forward "\n\n" nil t) (point-max)))
    (mail-fetch-field "X-Edited")))

(defun reddit-fetch-rewrite (entry file)
  "Write the message of ENTRY over FILE, which a folder's cur/ holds.
The name stays, so nnmaildir keeps the message's number and flags.  The
text goes through tmp/, so no reader sees half of it."
  (let ((tmp (expand-file-name (file-name-nondirectory file)
                               (expand-file-name "../tmp" (file-name-directory file))))
        (coding-system-for-write 'no-conversion))
    (write-region (encode-coding-string (reddit-fetch-message entry) 'utf-8) nil tmp nil 'silent)
    (rename-file tmp file t)))

;;; A subreddit

(defun reddit-fetch-old-p (entry)
  "Non-nil when ENTRY is older than `reddit-fetch-unread-days'."
  (time-less-p (plist-get entry :time)
               (time-subtract nil (* reddit-fetch-unread-days 86400))))

(defun reddit-fetch-group (store group base &optional threads)
  "Fetch subreddit folder GROUP under STORE, its links leading to BASE.
New posts arrive first, then every thread with new or edited comments,
oldest first, until a fetch fails; with THREADS, post ids, those threads
alone.  An edited message is written over, each score seen saved.
Return (GROUP POSTS COMMENTS EDITS WAITING PROBLEM)."
  (let* ((name (reddit-fetch-subreddit group))
         (mlist (reddit-fetch-scan store group))
         (known (make-hash-table :test #'equal))
         (scores (make-hash-table :test #'equal))
         (counts (reddit-fetch-read-state store group reddit-fetch-threads-file))
         (posts 0)
         (comments 0)
         (edits 0)
         (fetched 0)
         problem)
    (dolist (id (reddit-fetch-read-state store group reddit-fetch-known-file))
      (puthash id t known))
    (pcase-dolist (`(,id . ,score) (reddit-fetch-read-state store group reddit-fetch-scores-file))
      (puthash id score scores))
    (cl-labels ((seen-p (entry)
                  (or (gethash (plist-get entry :id) known)
                      (gethash (reddit-fetch-entry-message-id entry) mlist)))
                (outdated (entry)
                  ;; the file of the entry's message while it holds an older text
                  (when-let* ((edited (reddit-fetch-edited entry))
                              (article (gethash (reddit-fetch-entry-message-id entry) mlist))
                              (file (reddit-fetch-file store group article))
                              ((not (equal edited (reddit-fetch-stored-edit file)))))
                    file))
                (refresh (entry)
                  (when-let* ((file (outdated entry)))
                    (reddit-fetch-rewrite entry file)
                    (cl-incf edits)))
                (deliver (entry read)
                  (reddit-fetch-deliver group entry (or read (reddit-fetch-old-p entry)))
                  (puthash (plist-get entry :id) t known))
                (listing (path)
                  (mapcar (lambda (thing) (reddit-fetch-entry thing base))
                          (reddit-fetch-children
                           (reddit-fetch-json (format "%s/r/%s/%s?limit=%d&raw_json=1"
                                                      reddit-fetch-api name path
                                                      reddit-fetch-limit)))))
                (score (entry)
                  (puthash (reddit-fetch-entry-message-id entry)
                           (list (unless (plist-get entry :hidden) (plist-get entry :score))
                                 (plist-get entry :likes))
                           scores))
                (save ()
                  (reddit-fetch-save-state store group reddit-fetch-known-file
                                           (hash-table-keys known))
                  (reddit-fetch-save-state store group reddit-fetch-threads-file counts))
                (save-scores ()
                  ;; a message deleted from the folder takes its score along
                  (let (rows)
                    (maphash (lambda (id score)
                               (when (gethash id mlist)
                                 (push (cons id score) rows)))
                             scores)
                    (reddit-fetch-save-state store group reddit-fetch-scores-file
                                             (sort rows :key #'car)))))
      (unwind-protect
          (let* ((listed (unless threads (listing "new")))
                 (recent (unless threads (listing "comments")))
                 (ids (mapcar (lambda (post) (plist-get post :post)) listed))
                 (times (mapcar (lambda (post) (cons (plist-get post :post) (plist-get post :time)))
                                listed)))
            (dolist (post (reddit-fetch-oldest-first (seq-remove #'seen-p listed)))
              (deliver post nil)
              (cl-incf posts))
            (mapc #'refresh listed)
            (mapc #'score (append listed recent))
            ;; a thread past the listing is found by its new or edited
            ;; comments alone, and the oldest thread goes first: article
            ;; numbers follow time, and a group shows its newest numbers
            (unless threads
              (setq counts (seq-filter (lambda (count) (member (car count) ids)) counts)
                    threads (sort (delete-dups
                                   (append
                                    (mapcar (lambda (post) (plist-get post :post))
                                            (seq-remove (lambda (post)
                                                          (eql (plist-get post :comments)
                                                               (alist-get (plist-get post :post)
                                                                          counts 0 nil #'equal)))
                                                        listed))
                                    (mapcar (lambda (comment) (plist-get comment :post))
                                            (seq-filter (lambda (comment)
                                                          (or (not (seen-p comment))
                                                              (outdated comment)))
                                                        recent))))
                                  (lambda (a b)
                                    (< (alist-get a times 0 nil #'equal)
                                       (alist-get b times 0 nil #'equal))))))
            (while (and threads (not problem) (< fetched reddit-fetch-max-threads))
              (condition-case err
                  (let* ((thread (reddit-fetch-thread
                                  (reddit-fetch-json
                                   (format "%s/r/%s/comments/%s?limit=%d&raw_json=1"
                                           reddit-fetch-api name (car threads)
                                           reddit-fetch-thread-limit))
                                  base))
                         (post (car thread)))
                    ;; a post the listing no longer holds comes as context
                    (unless (seen-p post)
                      (deliver post t))
                    (dolist (comment (cdr thread))
                      (unless (seen-p comment)
                        (deliver comment nil)
                        (cl-incf comments)))
                    (mapc #'refresh thread)
                    (mapc #'score thread)
                    (setf (alist-get (pop threads) counts nil nil #'equal)
                          (plist-get post :comments))
                    (cl-incf fetched)
                    (save))
                (error (setq problem (error-message-string err)))))
            (list group posts comments edits (length threads) problem))
        (save)
        (save-scores)))))

;;; Entry point

(defun reddit-fetch-run (store base &optional threads)
  "Fetch every subreddit folder under STORE, its links leading to BASE.
THREADS, as ((GROUP POST...)...), limits the run to those threads.
Returns one (GROUP POSTS COMMENTS EDITS WAITING PROBLEM) or (GROUP error
MESSAGE) row per folder."
  (let ((gnus-newsrc-hashtb (gnus-make-hashtable))
        (gnus-verbose-backends 0)
        (defs `((directory ,store) (get-new-mail nil))))
    (unless (nnmaildir-open-server reddit-fetch-server defs)
      (error "No store %s to deliver into: %s" store
             (nnmaildir--srv-error nnmaildir--cur-server)))
    ;; with a second group registered, nnmaildir resolves its method
    ;; through Gnus's server tables, which no Gnus session filled here
    (setf (nnmaildir--srv-method nnmaildir--cur-server)
          `(nnmaildir ,reddit-fetch-server ,@defs))
    (unwind-protect
        ;; nnmaildir syncs every disk after each delivery
        (cl-letf (((symbol-function 'unix-sync) #'ignore))
          (mapcar (pcase-lambda (`(,group . ,posts))
                    (condition-case err
                        (reddit-fetch-group store group base posts)
                      (error (list group 'error (error-message-string err)))))
                  (or threads (mapcar #'list (reddit-fetch-groups store)))))
      ;; a later run in this process must not inherit this store
      (setf (alist-get reddit-fetch-server nnmaildir--servers nil 'remove #'equal) nil)
      (setq nnmaildir--cur-server nil)
      ;; the profile holds the user's Reddit login
      (when reddit-fetch-profile
        (delete-directory reddit-fetch-profile t)
        (setq reddit-fetch-profile nil)))))

(defun reddit-fetch-main (store base &optional threads)
  "Fetch the subreddit folders under STORE, links leading to BASE; report, exit.
THREADS limits the fetch as in `reddit-fetch-run'."
  (run-at-time reddit-fetch-watchdog nil
               (lambda ()
                 (message "reddit-fetch: no end after %d s" reddit-fetch-watchdog)
                 (kill-emacs 2)))
  (let (failed)
    (condition-case err
        (dolist (row (reddit-fetch-run store base threads))
          (pcase row
            (`(,group error ,text)
             (setq failed t)
             (message "%s: %s" group text))
            (`(,group ,posts ,comments ,edits ,waiting ,problem)
             (when problem
               (setq failed t))
             (message "%s: %d posts, %d comments, %d edits, %d threads waiting%s"
                      group posts comments edits waiting
                      (if problem (concat "; " problem) "")))))
      (error
       (setq failed t)
       (message "reddit-fetch: %s" (error-message-string err))))
    (kill-emacs (if failed 1 0))))

;;; reddit-fetch.el ends here
