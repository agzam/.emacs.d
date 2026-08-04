;;; modules/web-browsing/autoload/reddit-meetup.el -*- lexical-binding: t; -*-

;; reddigg and its promise/JSON deps load with the entry commands: this file
;; is also loaded bare by the test sandbox, which has neither.

(require 'time-date)
(require 'parse-time)
(require 'subr-x)

(declare-function reddigg-fetch-json "reddigg")
(declare-function reddigg-post-form "reddigg")
(declare-function promise-then "promise")
(declare-function promise-catch "promise")
(declare-function markdown-mode "markdown-mode")

(defvar reddit-meetup-subreddit "emacs"
  "Subreddit the announcements are posted to.")

(defvar reddit-meetup-body-regexp "meetup\\.com/emacsatx"
  "Body pattern telling an announcement apart from any other post.
The titles have drifted from one month to the next, the body has not.")

(defvar reddit-meetup-buffer-name "*reddit-meetup*"
  "Name of the draft buffer.")

(defconst reddit-meetup--me-url "https://www.reddit.com/api/me.json"
  "Carries the logged-in name and the modhash every write has to quote.")

(defconst reddit-meetup--user-posts-url
  "https://www.reddit.com/user/%s/submitted.json?limit=100&sort=new")

(defconst reddit-meetup--submit-url "https://www.reddit.com/api/submit")

;;; The meeting date

(defun reddit-meetup--first-thursday (year month)
  "Day of MONTH in YEAR falling on its first Thursday."
  (let ((weekday (nth 6 (decode-time (encode-time (list 0 0 12 1 month year nil -1 nil))))))
    (1+ (mod (- 4 weekday) 7))))

(defun reddit-meetup--next-meeting (&optional now)
  "Noon of the first-Thursday meeting due at or after NOW."
  (let* ((parts (decode-time (or now (current-time))))
         (year (nth 5 parts))
         (month (nth 4 parts))
         (day (reddit-meetup--first-thursday year month)))
    (when (< day (nth 3 parts))
      (setq month (1+ month))
      (when (< 12 month)
        (setq month 1
              year (1+ year)))
      (setq day (reddit-meetup--first-thursday year month)))
    (encode-time (list 0 0 12 day month year nil -1 nil))))

(defun reddit-meetup--ordinal (n)
  "N with its English ordinal suffix."
  (format "%d%s" n
          (cond ((memq (mod n 100) '(11 12 13)) "th")
                ((= 1 (mod n 10)) "st")
                ((= 2 (mod n 10)) "nd")
                ((= 3 (mod n 10)) "rd")
                (t "th"))))

(defun reddit-meetup--date-string (meeting)
  "MEETING spelled the way the announcements spell it."
  (format "%s %s"
          (format-time-string "%B" meeting)
          (reddit-meetup--ordinal (nth 3 (decode-time meeting)))))

(defun reddit-meetup--month-day (meeting)
  "MEETING as the (MONTH . DAY) cons dates are compared by."
  (let ((parts (decode-time meeting)))
    (cons (nth 4 parts) (nth 3 parts))))

(defun reddit-meetup--stated-date (body)
  "Date the reminder sentence in BODY names, as a (MONTH . DAY) cons.
The year is left out on purpose: it is never written down, and the day
and month alone are already unique among the announcements at hand."
  (when (and body (string-match "This time is on \\([A-Za-z]+\\)[ \t]+\\([0-9]+\\)" body))
    (when-let* ((month (cdr (assoc (downcase (match-string 1 body)) parse-time-months))))
      (cons month (string-to-number (match-string 2 body))))))

(defun reddit-meetup--title (meeting &optional now)
  "Title for the MEETING announcement, worded by its distance from NOW."
  (let ((days (- (time-to-days meeting) (time-to-days (or now (current-time)))))
        (date (reddit-meetup--date-string meeting)))
    (cond ((< days 1) "Emacs ATX meetup today, in a few hours")
          ((= days 1) (format "Emacs ATX meetup tomorrow, %s" date))
          ((< days 7) (format "Emacs ATX meetup this Thursday, %s" date))
          (t (format "Emacs ATX meetup on %s" date)))))

;;; Reworking the previous announcement

(defconst reddit-meetup--notes-link-regexp "^\\[Notes[^]]*\\](http[^)]*)\\.?[ \t]*\n"
  "Notes and video link, edited into an announcement after its meeting.")

(defun reddit-meetup--set-date (body date)
  "Point the reminder sentence in BODY at DATE, when BODY has one."
  (if (string-match "This time is on \\([^.\n]*\\)\\." body)
      (concat (substring body 0 (match-beginning 1))
              date
              (substring body (match-end 1)))
    body))

(defun reddit-meetup--strip-notes-link (body)
  "Drop the notes link BODY inherits from a meeting that already happened."
  (let ((text (replace-regexp-in-string reddit-meetup--notes-link-regexp "" body)))
    (setq text (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))
    ;; the horizontal rule that fenced the link is left with nothing to fence
    (replace-regexp-in-string "^-\\{3,\\}[ \t]*\n+\\(-\\{3,\\}[ \t]*\n\\)" "\\1" text)))

(defun reddit-meetup--add-previous (body url)
  "Put URL at the head of the previous-meetings list in BODY."
  (cond
   ((string-match-p (regexp-quote url) body) body)
   ((string-match "^Previous[^\n]*:[ \t]*\n" body)
    (concat (substring body 0 (match-end 0))
            "\n" url "\n"
            (substring body (match-end 0))))
   (t (concat (string-trim-right body)
              "\n\n----\nPrevious meetings and notes:\n\n" url "\n"))))

(defun reddit-meetup--compose-body (previous date url)
  "Rework the PREVIOUS announcement body for DATE, listing URL as the last one."
  (reddit-meetup--add-previous
   (reddit-meetup--strip-notes-link
    (reddit-meetup--set-date previous date))
   url))

;;; Talking to reddit

(defun reddit-meetup--announcement-p (post)
  "Whether POST is one of the announcements."
  (let ((case-fold-search t)
        (body (or (gethash "selftext" post) "")))
    (and (equal (gethash "subreddit" post) reddit-meetup-subreddit)
         (or (string-match-p reddit-meetup-body-regexp body)
             (reddit-meetup--stated-date body))
         t)))

(defun reddit-meetup--announcements (listing)
  "Announcement posts in LISTING, newest first."
  (seq-filter #'reddit-meetup--announcement-p
              (seq-map (lambda (child) (gethash "data" child))
                       (gethash "children" (gethash "data" listing)))))

(defun reddit-meetup--find-announcement (listing)
  "Newest announcement in LISTING, the one a draft is worked out of."
  (car (reddit-meetup--announcements listing)))

(defun reddit-meetup--announced-for (listing date)
  "Announcement in LISTING already covering DATE, a (MONTH . DAY) cons.
Announcements name their date in the body, so that is what they are
matched by.  The newest one naming no date counts as well: it cannot be
read, so it cannot be ruled out either."
  (let ((posts (reddit-meetup--announcements listing)))
    (or (seq-find (lambda (post)
                    (equal date (reddit-meetup--stated-date (gethash "selftext" post))))
                  posts)
        (and posts
             (null (reddit-meetup--stated-date (gethash "selftext" (car posts))))
             (car posts)))))

(defun reddit-meetup--split-draft (text)
  "Split TEXT into its title, the first line, and the body under it."
  (let ((lines (split-string text "\n")))
    (cons (string-trim (car lines))
          (string-trim (mapconcat #'identity (cdr lines) "\n")))))

(defun reddit-meetup--submit-params (title body modhash source)
  "Form parameters posting TITLE and BODY, per SOURCE, authorized by MODHASH."
  (append
   `(("api_type" . "json")
     ("kind" . "self")
     ("sr" . ,(plist-get source :subreddit))
     ("title" . ,title)
     ("text" . ,body)
     ("uh" . ,modhash)
     ("sendreplies" . "true")
     ("validate_on_submit" . "true"))
   (when-let* ((flair (plist-get source :flair-id)))
     `(("flair_id" . ,flair)
       ("flair_text" . ,(or (plist-get source :flair-text) ""))))))

(defun reddit-meetup--posted-url (response)
  "URL of the post RESPONSE created.
Reddit reports refusals inside an HTTP 200, so they surface from here."
  (let* ((payload (gethash "json" response))
         (errors (gethash "errors" payload)))
    (when (< 0 (length errors))
      (user-error "Reddit refused the post: %s"
                  (mapconcat (lambda (e) (format "%s %s" (aref e 0) (aref e 1)))
                             errors "; ")))
    (when-let* ((data (gethash "data" payload)))
      (gethash "url" data))))

(defun reddit-meetup--report-failure (reason)
  "Report REASON, in whatever shape the rejected request left it."
  (message "reddit: %s" (if (and (consp reason) (symbolp (car reason)))
                            (error-message-string reason)
                          reason)))

;;; The draft buffer

(defvar-local reddit-meetup--source nil
  "Plist describing the announcement the draft was derived from.")

(defvar-local reddit-meetup--submitting nil
  "Non-nil while a submission is in flight, so a second one cannot start.")

(defvar reddit-meetup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'reddit-meetup-submit)
    (define-key map (kbd "C-c C-k") #'reddit-meetup-cancel)
    map)
  "Keymap of `reddit-meetup-mode'.")

(defun reddit-meetup--header-line ()
  "Header line stating target and keys, which cannot live in the post text."
  (list
   (propertize (format " r/%s " (plist-get reddit-meetup--source :subreddit))
               'face '(:weight bold :inherit font-lock-function-name-face))
   (when-let* ((flair (plist-get reddit-meetup--source :flair-text)))
     (propertize (format "[%s] " flair) 'face 'shadow))
   (propertize "│ " 'face 'shadow)
   (propertize "line 1" 'face '(:weight bold))
   (propertize " = title " 'face 'shadow)
   (propertize "│ " 'face 'shadow)
   (propertize "C-c C-c" 'face '(:weight bold :inherit success))
   (propertize " submit " 'face 'shadow)
   (propertize "│ " 'face 'shadow)
   (propertize "C-c C-k" 'face '(:weight bold :inherit error))
   (propertize " cancel" 'face 'shadow)
   (when (plist-get reddit-meetup--source :announced)
     (propertize "  ! this meeting already has an announcement" 'face 'warning))))

(define-minor-mode reddit-meetup-mode
  "Editing mode for a reddit meetup announcement draft."
  :lighter " Meetup"
  :keymap reddit-meetup-mode-map
  (setq header-line-format (and reddit-meetup-mode (reddit-meetup--header-line))))

(defun reddit-meetup--draft (listing)
  "Open a draft announcement worked out of the newest one in LISTING."
  (let* ((post (reddit-meetup--find-announcement listing))
         (meeting (reddit-meetup--next-meeting))
         (announced (reddit-meetup--announced-for
                     listing (reddit-meetup--month-day meeting)))
         (url (gethash "url" post))
         (buffer (get-buffer-create reddit-meetup-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (if (require 'markdown-mode nil :no-error)
          (markdown-mode)
        (text-mode))
      (insert (reddit-meetup--title meeting) "\n\n"
              (reddit-meetup--compose-body (gethash "selftext" post)
                                           (reddit-meetup--date-string meeting)
                                           url)
              "\n")
      (setq reddit-meetup--source
            (list :subreddit (gethash "subreddit" post)
                  :flair-id (gethash "link_flair_template_id" post)
                  :flair-text (gethash "link_flair_text" post)
                  :previous url
                  :meeting meeting
                  :announced (and announced (gethash "url" announced))))
      (reddit-meetup-mode 1)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)))

;;; Commands

;;;###autoload
(defun reddit-meetup-announce ()
  "Draft the next meetup announcement out of the last one posted."
  (interactive)
  (require 'reddigg)
  (message "Looking up the last announcement...")
  (promise-catch
   (promise-then
    (reddigg-fetch-json reddit-meetup--me-url)
    (lambda (me)
      (promise-then
       (reddigg-fetch-json
        (format reddit-meetup--user-posts-url (gethash "name" (gethash "data" me))))
       (lambda (listing)
         (if (reddit-meetup--find-announcement listing)
             (reddit-meetup--draft listing)
           (message "No r/%s post matching %S to work from"
                    reddit-meetup-subreddit reddit-meetup-body-regexp))))))
   #'reddit-meetup--report-failure))

(defun reddit-meetup--post-draft (title body modhash source buffer)
  "Post TITLE and BODY the way SOURCE says, authorized by MODHASH.
Retires BUFFER once the post is up."
  (promise-then
   (reddigg-post-form reddit-meetup--submit-url
                      (reddit-meetup--submit-params title body modhash source))
   (lambda (response)
     (let ((url (reddit-meetup--posted-url response)))
       (kill-new url)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (set-buffer-modified-p nil)
           (kill-buffer)))
       (message "Posted, url copied: %s" url)))))

(defun reddit-meetup-submit (&optional force)
  "Post the draft to reddit.
Refuses when the meeting the draft announces already has a post, unless
FORCE (a prefix argument) says to go ahead anyway."
  (interactive "P")
  (unless reddit-meetup--source
    (user-error "Not a meetup draft"))
  (when reddit-meetup--submitting
    (user-error "Already submitting"))
  (require 'reddigg)
  (let* ((draft (reddit-meetup--split-draft
                 (buffer-substring-no-properties (point-min) (point-max))))
         (title (car draft))
         (body (cdr draft))
         (source reddit-meetup--source)
         ;; the draft's own sentence rules: it survives hand-edits and a
         ;; draft left sitting past midnight
         (date (or (reddit-meetup--stated-date body)
                   (reddit-meetup--month-day (plist-get source :meeting))))
         (buffer (current-buffer)))
    (when (string-empty-p title)
      (user-error "The first line has to be the title"))
    (when (string-empty-p body)
      (user-error "The post has no body"))
    (setq reddit-meetup--submitting t)
    (message "Posting to r/%s..." (plist-get source :subreddit))
    (promise-catch
     (promise-then
      (reddigg-fetch-json reddit-meetup--me-url)
      (lambda (me)
        (let ((account (gethash "data" me)))
          (promise-then
           (reddigg-fetch-json
            (format reddit-meetup--user-posts-url (gethash "name" account)))
           (lambda (listing)
             (unless force
               (when-let* ((posted (reddit-meetup--announced-for listing date)))
                 (user-error "Already announced by %S: %s (C-u C-c C-c posts anyway)"
                             (gethash "title" posted) (gethash "url" posted))))
             (reddit-meetup--post-draft
              title body (gethash "modhash" account) source buffer))))))
     (lambda (reason)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq reddit-meetup--submitting nil)))
       (reddit-meetup--report-failure reason)))))

(defun reddit-meetup-cancel ()
  "Discard the draft."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard the draft? "))
    (set-buffer-modified-p nil)
    (quit-window :kill)))
