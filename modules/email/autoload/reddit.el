;;; modules/email/autoload/reddit.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Subreddits, the inbox and the login's own posts as groups of the reddit
;; server: scripts/reddit-fetch.el turns Reddit's JSON, which a headless Brave
;; on the user's login fetches, into messages threaded by reply, and this file
;; runs it the way news.el runs news.
;;; Code:

(require 'gnus)
(require 'gnus-art)
(require 'gnus-bcklg)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-topic)
(require 'mail-utils)
(require 'nnmaildir)
(require 'subr-x)

(defvar doom-cache-dir)
(defvar doom-emacs-dir)
(defvar reddit-maildir)
(defvar reddit-topic)
(defvar mail-groups)
(defvar mail-load-running)
;; the thread view's, which runs the hook that fetches its thread
(defvar mail-thread-buffer-name)
(defvar mail-thread-group)
(defvar mail-thread-summary-buffer)

(defvar reddit-fetch-script (expand-file-name "scripts/reddit-fetch.el" doom-emacs-dir)
  "Batch script that fills `reddit-maildir' from Reddit.")

(defvar reddit-fetch-directory (expand-file-name "reddit-fetch/" doom-cache-dir)
  "Init directory of the Emacs that runs `reddit-fetch-script'.")

(defvar reddit-url "https://www.reddit.com"
  "Where the links of the Reddit messages lead.")

(defvar reddit-fetch-interval 600
  "Seconds between Reddit fetches while Emacs runs.")

(defvar reddit-fetch-process nil
  "The running Reddit fetch, or nil.")

(defvar reddit-fetch-timer nil
  "Timer of the periodic Reddit fetch, or nil.")

(defvar reddit-fetched-at nil
  "When the last Reddit fetch ended well, as a float time, or nil.")

(defvar reddit-fetch-pending nil
  "Non-nil when a fetch waits for the mail load reading a Reddit group.")

(defvar reddit-thread-fetch-interval 60
  "Seconds after a thread's fetch in which opening it fetches nothing.")

(defvar reddit-thread-fetched-at nil
  "(POST . TIME) of each thread an opened view fetched lately.")

(defvar reddit-thread-pending nil
  "(GROUP . POST) of the thread to fetch once the running fetch or read ends.")

(defvar reddit-fetch-end-hook nil
  "Hook run once a Reddit fetch ended, a thread's or every folder's.")

(defconst reddit-group-prefix "nnmaildir+reddit:"
  "Prefix of the groups in `reddit-maildir'.")

;;;###autoload
(defun reddit-group-p (group)
  "Non-nil when GROUP lives in `reddit-maildir'."
  (string-prefix-p reddit-group-prefix group))

;;;###autoload
(defun subreddit-group-p (group)
  "Non-nil when GROUP is a subreddit's folder of `reddit-maildir'."
  (string-prefix-p (concat reddit-group-prefix "r.") group))

;;;###autoload
(defun ensure-reddit-folders ()
  "Create the folder of each Reddit group `mail-groups' names.
Gnus gives up on a server whose directory is missing, and the fetch fills
only the folders it finds."
  (make-directory reddit-maildir t)
  (dolist (group (seq-filter #'reddit-group-p mail-groups))
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name (concat (gnus-group-real-name group) "/" sub)
                                        reddit-maildir)
                      t))))

(defun reddit-fetch-command (&optional thread)
  "Command line of the batch Emacs that fetches each folder `mail-groups' names.
With THREAD, (GROUP . POST), it fetches that thread alone.  A folder made
for the threads `show-in-subreddit' asks for gets no listing fetched."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch" "--init-directory" reddit-fetch-directory
        "-l" reddit-fetch-script
        "--eval" (format "(reddit-fetch-main %S %S '%S)"
                         (expand-file-name reddit-maildir) reddit-url
                         (if thread
                             `((,(gnus-group-real-name (car thread)) ,(cdr thread)))
                           (mapcar (lambda (group) (list (gnus-group-real-name group)))
                                   (seq-filter #'reddit-group-p mail-groups))))))

(defun start-reddit-fetch (command sentinel)
  "Run the batch fetch COMMAND in the background, SENTINEL watching it."
  (make-directory reddit-fetch-directory t)
  (let ((buffer (get-buffer-create " *reddit-fetch*")))
    (with-current-buffer buffer
      (erase-buffer))
    (setq reddit-fetch-process
          (make-process :name "reddit-fetch" :buffer buffer :command command
                        :connection-type 'pipe :noquery t :sentinel sentinel))
    ;; a question the batch Emacs asked would wait for an answer forever
    (process-send-eof reddit-fetch-process)))

(defun reddit-fetch-count (process what)
  "Sum of the counts of WHAT, such as \"comments\", the fetch PROCESS reported."
  (let ((count 0))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (while (re-search-forward
                (format "^[^:\n]+: [0-9]+ posts, \\(?:[0-9]+ [a-z]+, \\)*?\\([0-9]+\\) %s," what)
                nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    count))

(defun forget-shown-articles ()
  "Drop the copies Gnus keeps of the articles it showed.
A fetch writes an edited message over its file, and Gnus would show the
older text again."
  (gnus-backlog-shutdown)
  (gnus-flush-original-article-buffer))

(defun reddit-fetch-sentinel (process event)
  "Read the Reddit groups again in the background once PROCESS ended with EVENT.
A failed fetch reads them too, for what it delivered first.  A thread an
opened view asked for meanwhile is fetched next."
  (when (memq (process-status process) '(exit signal))
    (when (< 0 (reddit-fetch-count process "edits"))
      (forget-shown-articles))
    (when (gnus-alive-p)
      (load-mail-groups (seq-filter #'reddit-group-p gnus-group-list)))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (setq reddit-fetched-at (float-time))
      (message "Reddit fetch failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer process))))
    (run-hooks 'reddit-fetch-end-hook)
    (fetch-pending-reddit-thread)))

;;;###autoload
(defun fetch-reddit ()
  "Fetch every subreddit folder into `reddit-maildir' in the background.
Nothing starts while a fetch runs, and a batch reading a Reddit group
starts it when done: both would number the messages it delivers."
  (interactive)
  (setq reddit-fetch-pending
        (and (seq-some #'reddit-group-p (bound-and-true-p mail-load-running)) t))
  (unless (or reddit-fetch-pending (process-live-p reddit-fetch-process))
    (ensure-reddit-folders)
    (start-reddit-fetch (reddit-fetch-command) #'reddit-fetch-sentinel)))

;;;###autoload
(defun fetch-stale-reddit ()
  "Fetch Reddit unless a fetch ended well within `reddit-fetch-interval'."
  (unless (and reddit-fetched-at
               (< (- (float-time) reddit-fetched-at) reddit-fetch-interval))
    (fetch-reddit)))

;;;###autoload
(defun start-reddit-fetch-timer ()
  "Fetch Reddit every `reddit-fetch-interval' seconds while Emacs runs."
  (unless (or noninteractive (timerp reddit-fetch-timer))
    (setq reddit-fetch-timer
          (run-with-timer reddit-fetch-interval reddit-fetch-interval #'fetch-reddit))))

;;;###autoload
(defun reddit-group-parked-p (group)
  "Non-nil when GROUP is a Reddit group and a Reddit fetch delivers into it now."
  (and (reddit-group-p group) (process-live-p reddit-fetch-process)))

;;;###autoload
(defun fetch-pending-reddit ()
  "Start the Reddit fetch a batch read of a Reddit group held back."
  (when reddit-fetch-pending
    (fetch-reddit)))

;;; The thread a view opened

(defvar reddit-thread-asked nil
  "Post of the thread the user asked to fetch, shown again whatever came.")

(defvar reddit-thread-then nil
  "(POST . FUNCTION) to call once a good fetch of thread POST was read in.")

(defun reddit-post-id (message-id)
  "Reddit post of MESSAGE-ID, a post's or a comment's, or nil."
  (when (and message-id
             (string-match "\\`<\\(?:[^.@<>]+\\.\\)?\\([^.@<>]+\\)@reddit\\.com>\\'" message-id))
    (match-string 1 message-id)))

;;;###autoload
(defun fetch-reddit-thread (group post)
  "Fetch the newest replies of thread POST of Reddit GROUP in the background.
It waits for a running fetch or a read of a Reddit group, and fetches
nothing within `reddit-thread-fetch-interval' of its last fetch."
  (let ((now (float-time)))
    (setq reddit-thread-fetched-at
          (seq-filter (lambda (fetched) (< (- now (cdr fetched)) reddit-thread-fetch-interval))
                      reddit-thread-fetched-at))
    (cond ((assoc post reddit-thread-fetched-at))
          ((or (process-live-p reddit-fetch-process)
               (seq-some #'reddit-group-p (bound-and-true-p mail-load-running)))
           (setq reddit-thread-pending (cons group post)))
          (t
           (setq reddit-thread-pending nil)
           (push (cons post now) reddit-thread-fetched-at)
           (start-reddit-fetch (reddit-fetch-command (cons group post))
                               (lambda (process event)
                                 (reddit-thread-sentinel process event group post)))))))

(defun reddit-thread-sentinel (process event group post)
  "Bring on screen what the fetch PROCESS of thread POST brought.
EVENT says how it ended.  GROUP is read again first, ahead of others.
A good fetch the user asked for shows the thread whatever it brought, and
one `reddit-thread-then' waits on calls its function once GROUP is read."
  (when (memq (process-status process) '(exit signal))
    (let* ((ok (and (eq (process-status process) 'exit)
                    (zerop (process-exit-status process))))
           (asked (and ok (equal post reddit-thread-asked)))
           (then (when (equal post (car reddit-thread-then))
                   (prog1 (and ok (cdr reddit-thread-then))
                     (setq reddit-thread-then nil))))
           (count (reddit-fetch-count process "comments"))
           (edits (reddit-fetch-count process "edits")))
      (unless ok
        (message "Reddit thread fetch failed: %s (see %s)"
                 (string-trim event) (buffer-name (process-buffer process))))
      (when asked
        (setq reddit-thread-asked nil))
      (when (< 0 edits)
        (forget-shown-articles))
      ;; the store may hold what an earlier fetch brought
      (when (and (or asked then (< 0 (+ count edits))) (gnus-alive-p))
        (load-mail-groups (list group) t)
        (wait-for-mail-load 'reddit-thread "for the fetched thread"
                            (lambda () (not (mail-group-pending-p group)))
                            (lambda ()
                              (show-reddit-replies group post count edits asked)
                              (when then
                                (funcall then))))))
    (run-hooks 'reddit-fetch-end-hook)
    (fetch-pending-reddit-thread)))

(defun show-reddit-replies (group post count edits &optional asked)
  "Show the COUNT replies and EDITS edits fetched into thread POST of GROUP.
GROUP's summary takes the replies in, and a thread view showing POST in
a window lays the thread out again.  A fetch the user ASKED for takes in
and lays out whatever the store holds, though it brought nothing."
  (when-let* ((summary (get-buffer (gnus-summary-buffer-name group))))
    (when (or asked (< 0 count))
      (if-let* ((window (get-buffer-window summary t)))
          (with-selected-window window
            (gnus-summary-insert-new-articles))
        (with-current-buffer summary
          (gnus-summary-insert-new-articles))))
    (when-let* ((view (get-buffer mail-thread-buffer-name))
                ((get-buffer-window view t))
                ((eq (buffer-local-value 'mail-thread-summary-buffer view) summary)))
      (with-current-buffer view
        (when (member post (mapcar #'reddit-post-id (mail-thread-message-ids)))
          (mail-thread-reload)
          (if-let* ((said (delq nil (list (and (< 0 count)
                                               (format "%d new %s" count
                                                       (if (= count 1) "reply" "replies")))
                                          (and (< 0 edits) (format "%d edited" edits))))))
              (message "%s" (string-join said ", "))
            (message "The thread is up to date")))))))

;;;###autoload
(defun fetch-pending-reddit-thread ()
  "Fetch the thread an opened view asked for while the store was busy."
  (when-let* ((thread reddit-thread-pending))
    (fetch-reddit-thread (car thread) (cdr thread))))

;;;###autoload
(defun fetch-reddit-thread-h ()
  "Fetch the newest replies of the subreddit thread the view just opened.
The inbox and mine take theirs with the next fetch of their listing."
  (when (subreddit-group-p mail-thread-group)
    (when-let* ((post (seq-some #'reddit-post-id (mail-thread-message-ids))))
      (fetch-reddit-thread mail-thread-group post))))

;;;###autoload
(defun refetch-reddit-thread ()
  "Fetch the Reddit thread in the view now, then lay it out again."
  (interactive nil mail-thread-mode)
  (let ((post (or (and (reddit-group-p mail-thread-group)
                       (seq-some #'reddit-post-id (mail-thread-message-ids)))
                  (user-error "Not a Reddit thread"))))
    ;; the view's opening fetched it within the interval
    (setq reddit-thread-fetched-at (assoc-delete-all post reddit-thread-fetched-at)
          reddit-thread-asked post)
    (fetch-reddit-thread mail-thread-group post)
    (message "Fetching the thread from Reddit...")))

;;; The message on screen in its subreddit

;;;###autoload
(defun reddit-message-on-screen ()
  "(GROUP . HEADER) of the Reddit message on screen, or nil."
  (pcase-let ((`(,summary . ,article) (mail-on-screen)))
    (when (and summary article (buffer-live-p (get-buffer summary)))
      (with-current-buffer summary
        (when-let* (((reddit-group-p gnus-newsgroup-name))
                    (header (gnus-summary-article-header article)))
          (cons gnus-newsgroup-name header))))))

(defun subreddit-copy (id)
  "(GROUP . ARTICLE) of the copy of Message-ID ID a subreddit's group holds, or nil."
  (seq-some (lambda (group)
              (when-let* ((loaded (mail-group-loaded-p group))
                          (article (gethash id (nnmaildir--grp-mlist loaded))))
                (cons group (nnmaildir--art-num article))))
            (seq-filter #'subreddit-group-p gnus-group-list)))

(defun goto-mail-summary-article (group article)
  "Show GROUP's summary, entering GROUP unless it is open, at ARTICLE.
An ARTICLE the summary leaves out comes into it."
  (with-current-buffer gnus-group-buffer
    (gnus-group-read-group nil t group))
  (when-let* ((window (get-buffer-window (gnus-summary-buffer-name group))))
    (select-window window)
    (gnus-summary-goto-subject article t)))

(defun reddit-message-link (group article)
  "URL the Archived-At header of ARTICLE of GROUP names, or nil."
  (when (gnus-request-head article group)
    (with-current-buffer nntp-server-buffer
      (when-let* ((value (mail-fetch-field "Archived-At")))
        (string-trim value "[ \t\n<]+" "[ \t\n>]+")))))

(defun subreddit-group-of-link (link)
  "Group of the subreddit whose permalink LINK is, or nil.
A group Gnus knows under the name in another case is that group."
  (when (and link (string-match "\\`https?://[^/]+/r/\\([^/]+\\)/" link))
    (let ((group (concat reddit-group-prefix "r." (match-string 1 link))))
      (or (seq-find (lambda (known) (string-equal-ignore-case known group))
                    (seq-filter #'subreddit-group-p gnus-group-list))
          group))))

(defun ensure-subreddit-group (group)
  "Make subreddit GROUP's folder, and the group unless Gnus has it.
It goes under `reddit-topic', and no routine scan reads it: the fetch of
a thread fills it, then reads it."
  (dolist (sub '("cur" "new" "tmp"))
    (make-directory (expand-file-name (concat (gnus-group-real-name group) "/" sub)
                                      reddit-maildir)
                    t))
  (unless (gnus-group-entry group)
    (subscribe-mail-group group)
    (gnus-group-change-level group (1+ gnus-activate-level) (gnus-group-level group))
    ;; a group moved to a topic Gnus lacks would leave every topic
    (when (assoc reddit-topic gnus-topic-alist)
      (put-group-in-topic group reddit-topic))))

(defun show-subreddit-copy (id &optional link)
  "Go to the copy of Message-ID ID in its subreddit's summary.
Without one, the thread comes first into the group of the subreddit whose
permalink LINK is, made if need be; with no LINK it says there is none."
  (if-let* ((copy (subreddit-copy id)))
      (goto-mail-summary-article (car copy) (cdr copy))
    (if-let* ((post (reddit-post-id id))
              (group (subreddit-group-of-link link)))
        (progn
          (ensure-subreddit-group group)
          ;; Gnus may be off screen by the time the thread is in
          (setq reddit-thread-then (cons post (lambda ()
                                                (if (gnus-shown-p)
                                                    (show-subreddit-copy id)
                                                  (message "The thread is in %s"
                                                           (gnus-group-short-name group)))))
                reddit-thread-fetched-at (assoc-delete-all post reddit-thread-fetched-at))
          (fetch-reddit-thread group post)
          (message "Fetching the thread into %s..." (gnus-group-short-name group)))
      (message "No subreddit's group holds this message; %s opens it on Reddit"
               (substitute-command-keys "\\[open-mail-on-web]")))))

;;;###autoload
(defun show-in-subreddit ()
  "Go to the Reddit message on screen in the summary of its subreddit.
The inbox and mine hold copies of what a subreddit's group holds, under
the same Message-ID; a thread the group lacks is fetched into it first."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (pcase-let* ((`(,group . ,header) (or (reddit-message-on-screen)
                                        (user-error "No Reddit message here")))
               (id (mail-header-id header))
               (link (reddit-message-link group (mail-header-number header)))
               (unloaded (seq-remove #'mail-group-loaded-p
                                     (seq-filter #'subreddit-group-p gnus-group-list))))
    (when unloaded
      (load-mail-groups unloaded t))
    (wait-for-mail-load 'subreddit "to find the message in its subreddit"
                        (lambda () (not (seq-some #'mail-group-pending-p unloaded)))
                        (lambda () (show-subreddit-copy id link)))))

;;; reddit.el ends here
