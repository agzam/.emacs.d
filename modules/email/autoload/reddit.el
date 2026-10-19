;;; modules/email/autoload/reddit.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Subreddits as groups of the reddit server: scripts/reddit-fetch.el turns
;; Reddit's JSON, which a headless Brave on the user's login fetches, into
;; messages threaded by reply, and this file runs it the way news.el runs news.
;;; Code:

(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'subr-x)

(defvar doom-cache-dir)
(defvar doom-emacs-dir)
(defvar reddit-maildir)
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
  "Where the subreddits come from.")

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

(defconst reddit-group-prefix "nnmaildir+reddit:"
  "Prefix of the groups in `reddit-maildir'.")

;;;###autoload
(defun reddit-group-p (group)
  "Non-nil when GROUP lives in `reddit-maildir'."
  (string-prefix-p reddit-group-prefix group))

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
  "Command line of the batch Emacs that fetches every subreddit folder.
With THREAD, (GROUP . POST), it fetches that thread alone."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch" "--init-directory" reddit-fetch-directory
        "-l" reddit-fetch-script
        "--eval" (if thread
                     (format "(reddit-fetch-main %S %S '%S)"
                             (expand-file-name reddit-maildir) reddit-url
                             `((,(gnus-group-real-name (car thread)) ,(cdr thread))))
                   (format "(reddit-fetch-main %S %S)"
                           (expand-file-name reddit-maildir) reddit-url))))

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

(defun reddit-fetch-sentinel (process event)
  "Read the Reddit groups again in the background once PROCESS ended with EVENT.
A failed fetch reads them too, for what it delivered first.  A thread an
opened view asked for meanwhile is fetched next."
  (when (memq (process-status process) '(exit signal))
    (when (gnus-alive-p)
      (load-mail-groups (seq-filter #'reddit-group-p gnus-group-list)))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (setq reddit-fetched-at (float-time))
      (message "Reddit fetch failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer process))))
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

(defun reddit-fetch-comments (process)
  "Comments the Reddit fetch PROCESS reported delivering."
  (let ((count 0))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (while (re-search-forward "^[^:\n]+: [0-9]+ posts, \\([0-9]+\\) comments" nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    count))

(defun reddit-thread-sentinel (process event group post)
  "Bring the replies the fetch PROCESS of thread POST delivered on screen.
EVENT says how it ended.  GROUP is read again first, ahead of others."
  (when (memq (process-status process) '(exit signal))
    (unless (and (eq (process-status process) 'exit)
                 (zerop (process-exit-status process)))
      (message "Reddit thread fetch failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer process))))
    (let ((count (reddit-fetch-comments process)))
      (when (and (< 0 count) (gnus-alive-p))
        (load-mail-groups (list group) t)
        (wait-for-mail-load 'reddit-thread "for the new replies"
                            (lambda () (not (mail-group-pending-p group)))
                            (lambda () (show-reddit-replies group post count)))))
    (fetch-pending-reddit-thread)))

(defun show-reddit-replies (group post count)
  "Bring COUNT replies fetched into thread POST of GROUP on screen.
GROUP's summary takes them in, and a thread view showing POST in a
window lays the thread out again."
  (when-let* ((summary (get-buffer (gnus-summary-buffer-name group))))
    (if-let* ((window (get-buffer-window summary t)))
        (with-selected-window window
          (gnus-summary-insert-new-articles))
      (with-current-buffer summary
        (gnus-summary-insert-new-articles)))
    (when-let* ((view (get-buffer mail-thread-buffer-name))
                ((get-buffer-window view t))
                ((eq (buffer-local-value 'mail-thread-summary-buffer view) summary)))
      (with-current-buffer view
        (when (member post (mapcar #'reddit-post-id (mail-thread-message-ids)))
          (mail-thread-reload)
          (message "%d new %s" count (if (= count 1) "reply" "replies")))))))

;;;###autoload
(defun fetch-pending-reddit-thread ()
  "Fetch the thread an opened view asked for while the store was busy."
  (when-let* ((thread reddit-thread-pending))
    (fetch-reddit-thread (car thread) (cdr thread))))

;;;###autoload
(defun fetch-reddit-thread-h ()
  "Fetch the newest replies of the Reddit thread the view just opened."
  (when (reddit-group-p mail-thread-group)
    (when-let* ((post (seq-some #'reddit-post-id (mail-thread-message-ids))))
      (fetch-reddit-thread mail-thread-group post))))

;;; reddit.el ends here
