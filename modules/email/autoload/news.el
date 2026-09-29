;;; modules/email/autoload/news.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-group)
(require 'nnmaildir)
(require 'subr-x)

(defvar doom-cache-dir)
(defvar doom-emacs-dir)
(defvar news-maildir)
(defvar news-server)
(defvar mail-groups)
(defvar mail-load-running)

(defvar news-fetch-script (expand-file-name "scripts/news-fetch.el" doom-emacs-dir)
  "Batch script that fills `news-maildir' from `news-server'.")

(defvar news-fetch-directory (expand-file-name "news-fetch/" doom-cache-dir)
  "Init directory of the Emacs that runs `news-fetch-script'.")

(defvar news-active-file (expand-file-name "news-active" doom-cache-dir)
  "The news server's group list, as the last fetch saved it.")

(defvar news-fetch-interval 1800
  "Seconds between news fetches while Emacs runs.")

(defvar news-fetch-process nil
  "The running news fetch, or nil.")

(defvar news-fetch-timer nil
  "Timer of the periodic news fetch, or nil.")

(defvar news-fetched-at nil
  "When the last news fetch ended well, as a float time, or nil.")

(defconst news-group-prefix "nnmaildir+news:"
  "Prefix of the groups in `news-maildir'.")

;;; The store

;;;###autoload
(defun news-group-p (group)
  "Non-nil when GROUP lives in `news-maildir'."
  (string-prefix-p news-group-prefix group))

;;;###autoload
(defun make-news-folder (group)
  "Create the folder of news GROUP, which the next fetch fills."
  (dolist (sub '("cur" "new" "tmp"))
    (make-directory (expand-file-name (concat (gnus-group-real-name group) "/" sub)
                                      news-maildir)
                    t)))

;;;###autoload
(defun ensure-news-folders ()
  "Create the folder of each news group `mail-groups' names.
The news server of Gnus fails for the session when its store is missing."
  (make-directory news-maildir t)
  (mapc #'make-news-folder (seq-filter #'news-group-p mail-groups)))

;;; The server's group list

(defvar news-active-cache nil
  "(MTIME . GROUPS) of the `news-active-file' read last.")

(defun read-news-active-file ()
  "Groups of `news-active-file' as (GROUP . ARTICLES), in its order."
  (with-temp-buffer
    (insert-file-contents news-active-file)
    (let (groups)
      (while (re-search-forward "^\\([^ \n]+\\) +\\([0-9]+\\) +\\([0-9]+\\)" nil t)
        (push (cons (concat news-group-prefix (match-string 1))
                    (max 0 (- (1+ (string-to-number (match-string 2)))
                              (string-to-number (match-string 3)))))
              groups))
      (nreverse groups))))

;;;###autoload
(defun news-active-groups ()
  "The news server's groups as (GROUP . ARTICLES), from the list a fetch saved."
  (when-let* ((attributes (file-attributes news-active-file)))
    (let ((mtime (file-attribute-modification-time attributes)))
      (unless (equal mtime (car news-active-cache))
        (setq news-active-cache (cons mtime (read-news-active-file))))
      (cdr news-active-cache))))

;;; Fetching

(defun news-fetch-command ()
  "Command line of the batch Emacs that fetches every news group."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch" "--init-directory" news-fetch-directory
        "-l" news-fetch-script
        "--eval" (format "(news-fetch-main %S %S %S)"
                         (expand-file-name news-maildir) news-active-file news-server)))

(defun news-fetch-sentinel (process event)
  "Read the news groups again in the background once PROCESS ended with EVENT.
A failed fetch reads them too, for the posts it delivered first."
  (when (memq (process-status process) '(exit signal))
    (when (gnus-alive-p)
      (load-mail-groups (seq-filter #'news-group-p gnus-group-list)))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (setq news-fetched-at (float-time))
      (message "News fetch failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer process))))))

(defvar news-fetch-pending nil
  "Non-nil when a fetch waits for the mail load reading a news group.")

;;;###autoload
(defun fetch-news ()
  "Fetch every news group into `news-maildir' in the background.
Nothing starts while a fetch runs, and a batch reading a news group
starts it when done: both would number the posts it delivers."
  (interactive)
  (setq news-fetch-pending
        (and (seq-some #'news-group-p (bound-and-true-p mail-load-running)) t))
  (unless (or news-fetch-pending (process-live-p news-fetch-process))
    (ensure-news-folders)
    ;; the TLS checks save what they saw there, and fail the connection
    ;; when they cannot
    (make-directory news-fetch-directory t)
    (let ((buffer (get-buffer-create " *news-fetch*")))
      (with-current-buffer buffer
        (erase-buffer))
      (setq news-fetch-process
            (make-process :name "news-fetch" :buffer buffer :command (news-fetch-command)
                          :connection-type 'pipe :noquery t
                          :sentinel #'news-fetch-sentinel))
      ;; a question the batch Emacs asked would wait for an answer forever
      (process-send-eof news-fetch-process))))

;;;###autoload
(defun fetch-stale-news ()
  "Fetch news unless a fetch ended well within `news-fetch-interval'."
  (unless (and news-fetched-at
               (< (- (float-time) news-fetched-at) news-fetch-interval))
    (fetch-news)))

;;;###autoload
(defun start-news-fetch-timer ()
  "Fetch news every `news-fetch-interval' seconds while Emacs runs."
  (unless (or noninteractive (timerp news-fetch-timer))
    (setq news-fetch-timer
          (run-with-timer news-fetch-interval news-fetch-interval #'fetch-news))))

;;; news.el ends here
