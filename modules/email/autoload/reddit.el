;;; modules/email/autoload/reddit.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Subreddits as groups of the reddit server: scripts/reddit-fetch.el turns
;; Reddit's JSON, which a headless Brave on the user's login fetches, into
;; messages threaded by reply, and this file runs it the way news.el runs news.
;;; Code:

(require 'gnus)
(require 'gnus-group)
(require 'subr-x)

(defvar doom-cache-dir)
(defvar doom-emacs-dir)
(defvar reddit-maildir)
(defvar mail-groups)
(defvar mail-load-running)

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

(defun reddit-fetch-command ()
  "Command line of the batch Emacs that fetches every subreddit folder."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch" "--init-directory" reddit-fetch-directory
        "-l" reddit-fetch-script
        "--eval" (format "(reddit-fetch-main %S %S)" (expand-file-name reddit-maildir) reddit-url)))

(defun reddit-fetch-sentinel (process event)
  "Read the Reddit groups again in the background once PROCESS ended with EVENT.
A failed fetch reads them too, for what it delivered first."
  (when (memq (process-status process) '(exit signal))
    (when (gnus-alive-p)
      (load-mail-groups (seq-filter #'reddit-group-p gnus-group-list)))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (setq reddit-fetched-at (float-time))
      (message "Reddit fetch failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer process))))))

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
    (make-directory reddit-fetch-directory t)
    (let ((buffer (get-buffer-create " *reddit-fetch*")))
      (with-current-buffer buffer
        (erase-buffer))
      (setq reddit-fetch-process
            (make-process :name "reddit-fetch" :buffer buffer :command (reddit-fetch-command)
                          :connection-type 'pipe :noquery t
                          :sentinel #'reddit-fetch-sentinel))
      ;; a question the batch Emacs asked would wait for an answer forever
      (process-send-eof reddit-fetch-process))))

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

;;; reddit.el ends here
