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

(defun read-news-groups ()
  "News groups nnmaildir has read this session."
  (when-let* ((server (alist-get "news" nnmaildir--servers nil nil #'equal))
              (groups (nnmaildir--srv-groups server)))
    (mapcar (lambda (name) (concat news-group-prefix name)) (hash-table-keys groups))))

;;;###autoload
(defun merge-news-flags-a (fn group &optional server &rest args)
  "Call FN on GROUP, SERVER and ARGS, merging the flags of a news group it read.
A plain entry keeps the read marks the newsrc had, and a fetch delivers
older posts read, so they would show unread."
  (let ((known (and (equal server "news") (nnmaildir--prepare server group))))
    (prog1 (apply fn group server args)
      (when-let* (((equal server "news"))
                  ((not known))
                  ((nnmaildir--prepare server group))
                  (info (gnus-get-info (concat news-group-prefix group))))
        (gnus-request-update-info info (gnus-find-method-for-group (gnus-info-group info)))))))

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
  "Refresh the news groups Gnus read once PROCESS ended with EVENT.
A group Gnus has not read would be read whole here; its entry reads it.
A failed fetch refreshes them too, for the posts it delivered first."
  (when (memq (process-status process) '(exit signal))
    (when (gnus-alive-p)
      (dolist (group (read-news-groups))
        (refresh-mail-group group)
        (gnus-group-update-group group t)))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (setq news-fetched-at (float-time))
      (message "News fetch failed: %s (see %s)"
               (string-trim event) (buffer-name (process-buffer process))))))

;;;###autoload
(defun fetch-news ()
  "Fetch every news group into `news-maildir' in the background.
Nothing starts while a fetch runs."
  (interactive)
  (unless (process-live-p news-fetch-process)
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
