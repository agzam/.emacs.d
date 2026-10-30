;;; modules/email/autoload/sync.el -*- lexical-binding: t; -*-
;;; Commentary:
;; mbsync mirrors Gmail into the store by `mail-sync-config', and notmuch
;; indexes the store after it.  Between syncs, curl renames or deletes a
;; label in Gmail.  Each step is a process whose sentinel starts the
;; next, so the main thread waits on none of them.
;;; Code:

(defvar doom-emacs-dir)
(defvar doom-state-dir)
(defvar gmail-maildir)
(defvar mail-from-address)
(defvar mail-inbox-address)
(defvar gnus-search-notmuch-config-file)
(defvar server-process)

(defvar mail-sync-hook nil
  "Hook `sync-mail' runs to fetch what lives outside Gmail.")

(defvar mbsync-program "mbsync"
  "Program that syncs `gmail-maildir' with Gmail.")

(defvar notmuch-program "notmuch"
  "Program that indexes the stores after a sync.")

(defvar mail-imap-program "curl"
  "Program that sends Gmail the IMAP command of a label change.")

(defvar mail-sync-config (expand-file-name "modules/email/isyncrc" doom-emacs-dir)
  "Config file every mbsync run reads: the account, the stores and the channels.")

(defvar mail-sync-directory (expand-file-name "mail-sync/" doom-state-dir)
  "Directory of the sync's log, its tier markers and the label folders set aside.")

(defvar mail-sync-log-buffer-name "*mail sync log*"
  "Name of the buffer that shows the sync's log.")

(defvar mail-sync-log-bury-delay 5
  "Seconds the log stays on screen after the sync `sync-mail' showed it for ends.")

(defvar mail-sync-log-window nil
  "Window `sync-mail' showed the log in, until the log leaves it.")

(defvar mail-sync-log-timer nil
  "Timer that takes the log off the screen, or nil.")

(defvar mail-sync-log-font-lock-keywords
  '(("^.*: exit [1-9][0-9]* in .*$" . 'error)
    ("^--- \\(?:done in .*, status [1-9][0-9]*\\|failed in .*\\) ---$" . 'error)
    ("^.*\\<\\(?:error\\|could not\\)\\>.*$" . 'error))
  "Lines of the sync's log that say something failed.")

(defvar mail-sync-interval 300
  "Seconds between the syncs `mail-sync-timer' starts.")

(defvar mail-sync-fast-interval 1800
  "Seconds after a clean fast or full sync before the next takes the fast tier.")

(defvar mail-sync-full-interval 86400
  "Seconds after a clean full sync before the next takes the full tier.")

(defvar mail-sync-removed-days 30
  "Days a label folder set aside stays before a sync deletes it.")

(defvar mail-sync-process nil
  "Process of the sync step that runs, or nil.")

(defvar mail-sync-timer nil
  "Timer of the periodic mail sync, or nil.")

(defvar mail-sync-report nil
  "Non-nil when the running sync says in the echo area how it ended.")

(defvar running-label-change nil
  "The label change that runs, or nil.")

(defvar waiting-label-changes nil
  "Label changes that wait for the running sync, as (CHANGE . DONE), oldest first.")

(defconst mail-sync-tiers
  '((inbox (("inbox")) ())
    (fast (("fast") ("--pull-new" "--push-flags" "bulk")) ("last-fast"))
    (full (("fast" "bulk")) ("last-fast" "last-full")))
  "Each tier's mbsync runs, in order, and the markers a clean pass moves.
The isyncrc's group fast holds the inbox, sent, trash and the small
labels; bulk holds All Mail and the mailing-list labels.")

(define-derived-mode mail-sync-log-mode special-mode "Mail sync log"
  "Mode of the buffer that shows the sync's log, what failed in the face `error'."
  (setq-local font-lock-defaults '(mail-sync-log-font-lock-keywords t t)))

(defun mail-sync-log-buffer ()
  "The buffer named `mail-sync-log-buffer-name', made read-only if missing."
  (or (get-buffer mail-sync-log-buffer-name)
      (with-current-buffer (get-buffer-create mail-sync-log-buffer-name)
        (mail-sync-log-mode)
        (buffer-disable-undo)
        (current-buffer))))

(defun mail-sync-log (format-string &rest args)
  "Append a line of FORMAT-STRING and ARGS to the sync's log and its buffer."
  (let ((line (concat (apply #'format format-string args) "\n")))
    (write-region line nil (expand-file-name "mail-sync.log" mail-sync-directory) t 'silent)
    (with-current-buffer (mail-sync-log-buffer)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          ;; a window at the end follows the line, one elsewhere stays
          (insert-before-markers line))))))

(defun show-mail-sync-log ()
  "Show the sync's log in another window, unless a window shows it already."
  (when (timerp mail-sync-log-timer)
    (cancel-timer mail-sync-log-timer)
    (setq mail-sync-log-timer nil))
  (let ((buffer (mail-sync-log-buffer)))
    (unless (get-buffer-window buffer 'visible)
      (setq mail-sync-log-window (display-buffer buffer '(nil (inhibit-same-window . t)))))))

(defun bury-mail-sync-log ()
  "Take the log off the window `show-mail-sync-log' gave it, unless it is selected."
  (when-let* ((window mail-sync-log-window)
              ((window-live-p window))
              ((eq (window-buffer window) (get-buffer mail-sync-log-buffer-name)))
              ((not (eq window (selected-window)))))
    (quit-restore-window window 'bury))
  (setq mail-sync-log-window nil
        mail-sync-log-timer nil))

(defun mail-sync-due-p (marker interval)
  "Non-nil when MARKER of `mail-sync-directory' is missing or INTERVAL seconds old."
  (let ((time (file-attribute-modification-time
               (file-attributes (expand-file-name marker mail-sync-directory)))))
    (or (null time) (< interval (float-time (time-since time))))))

(defun mail-sync-tier (&optional full)
  "Tier to sync now: `full' daily or with FULL, `fast' half-hourly, else `inbox'."
  (cond ((or full (mail-sync-due-p "last-full" mail-sync-full-interval)) 'full)
        ((mail-sync-due-p "last-fast" mail-sync-fast-interval) 'fast)
        (t 'inbox)))

(defun mbsync-command (&rest args)
  "Command line of mbsync with ARGS, reading `mail-sync-config'."
  (append (list mbsync-program "-c" (expand-file-name mail-sync-config)) args))

(defun run-mail-sync-step (command then &optional quiet name)
  "Run COMMAND in the background, log how it ended, then call THEN.
THEN gets the exit status, 128 plus the signal, or 127 when COMMAND
cannot start, and the output.  QUIET logs the output only on a failure.
NAME stands for COMMAND in the log."
  (let* ((start (current-time))
         (buffer (get-buffer-create " *mail-sync*"))
         (finish (lambda (status output)
                   (unless (or (string-empty-p output) (and quiet (zerop status)))
                     (mail-sync-log "%s" (string-trim-right output)))
                   (mail-sync-log "%s: exit %d in %.1fs" (or name (string-join command " "))
                                  status (float-time (time-since start)))
                   (funcall then status output))))
    (with-current-buffer buffer
      (erase-buffer))
    (when-let* ((process
                 (condition-case err
                     (make-process
                      :name "mail-sync" :buffer buffer :command command
                      :connection-type 'pipe :noquery t
                      :sentinel (lambda (process _event)
                                  (unless (process-live-p process)
                                    (process-put process 'ended t)
                                    (funcall finish
                                             (if (eq (process-status process) 'exit)
                                                 (process-exit-status process)
                                               (+ 128 (process-exit-status process)))
                                             (if (buffer-live-p buffer)
                                                 (with-current-buffer buffer (buffer-string))
                                               "")))))
                   (error (funcall finish 127 (error-message-string err))
                          nil))))
      (setq mail-sync-process process)
      ;; a prompt would wait for an answer forever
      (process-send-eof process))))

;;; Labels Gmail dropped
;;
;; mbsync fails every pass on a synced folder whose label Gmail dropped,
;; so the folder moves out of the store first.  Every message of a label
;; is in All Mail too.

(defun gmail-boxes (listing)
  "The lines of LISTING, mbsync -ls naming Gmail's boxes; nil without the inbox."
  (let ((boxes (split-string listing "\n" t)))
    (and (member "INBOX" boxes) boxes)))

(defun gmail-label-box (folder boxes)
  "The box of BOXES that mbsync keeps in FOLDER, or nil.
mbsync keeps the box a/b in the folder a.b."
  (seq-find (lambda (box) (equal (string-replace "/" "." box) folder)) boxes))

(defun dropped-label-folders (boxes)
  "Folders of synced labels that Gmail's BOXES lack."
  (seq-filter (lambda (name)
                (and (not (member name '("inbox" "sent" "trash" "archive")))
                     (not (gmail-label-box name boxes))
                     (file-exists-p (expand-file-name (concat name "/.mbsyncstate")
                                                      gmail-maildir))))
              (directory-files gmail-maildir nil "\\`[^.]")))

(defun set-aside-label-folder (name why)
  "Move the folder of label NAME out of the store, logging WHY it went.
Return nil, or why it could not move."
  (let ((dest (expand-file-name (format "removed/%s-%s" name
                                        (format-time-string "%Y%m%d-%H%M%S"))
                                mail-sync-directory)))
    (condition-case err
        (progn
          (make-directory (file-name-directory dest) t)
          (rename-file (expand-file-name name gmail-maildir) dest)
          ;; its age counts from the move
          (set-file-times dest)
          (mail-sync-log "label %s %s, its folder moved to %s" name why dest)
          nil)
      (file-error
       (let ((problem (format "could not move the folder of label %s: %s"
                              name (error-message-string err))))
         (mail-sync-log "%s" problem)
         problem)))))

(defun set-aside-dropped-labels (status listing)
  "Set aside the folder of each label Gmail dropped, by LISTING of mbsync -ls.
A listing whose STATUS is not 0 moves nothing.  Folders set aside over
`mail-sync-removed-days' ago go."
  (if-let* (((zerop status))
            (boxes (gmail-boxes listing)))
      (dolist (name (dropped-label-folders boxes))
        (set-aside-label-folder name "is gone from Gmail"))
    (mail-sync-log "could not list Gmail's labels, no folder moved"))
  (when-let* ((removed (expand-file-name "removed/" mail-sync-directory))
              ((file-directory-p removed))
              (expired (seq-filter
                        (lambda (file)
                          (< (* mail-sync-removed-days 86400)
                             (float-time (time-since (file-attribute-modification-time
                                                      (file-attributes file))))))
                        (directory-files removed t "\\`[^.]"))))
    ;; a label's folder can hold thousands of files
    (make-process :name "mail-sync-prune" :command (append '("rm" "-rf" "--") expired)
                  :connection-type 'pipe :noquery t)))

;;; The sync

(defun ensure-notmuch-config ()
  "Write the notmuch config the search and the index read, unless it exists.
Its paths are absolute, so no tracked file can be it."
  (let ((file (expand-file-name gnus-search-notmuch-config-file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "[database]\nmail_root="
                (directory-file-name
                 (file-name-directory (directory-file-name (expand-file-name gmail-maildir))))
                "\n[user]\nname=" user-full-name
                "\nprimary_email=" mail-from-address
                (if mail-inbox-address (concat "\nother_email=" mail-inbox-address) "")
                "\n[new]\nignore=.nnmaildir;.mbsyncstate;.mbsyncstate.new;"
                ".mbsyncstate.journal;.mbsyncstate.lock;.uidvalidity\n"
                "[maildir]\nsynchronize_flags=true\n"
                "[index]\nheader.List=List-Id\n")))))

(defun finish-mail-sync (status start)
  "End the sync begun at START with STATUS, and read again what it changed."
  (mail-sync-log "--- done in %ds, status %d ---"
                 (round (float-time (time-since start))) status)
  (when mail-sync-report
    (if (zerop status)
        (message "Mail synced")
      (message "%s" (propertize
                     (format "Mail sync failed, status %d (see %s)" status
                             (abbreviate-file-name
                              (expand-file-name "mail-sync.log" mail-sync-directory)))
                     'face 'error)))
    (when mail-sync-log-window
      (setq mail-sync-log-timer
            (run-with-timer mail-sync-log-bury-delay nil #'bury-mail-sync-log))))
  (when (and (featurep 'gnus) (gnus-alive-p))
    (refresh-synced-mail-groups start))
  (run-waiting-label-change))

(defun notmuch-new-command ()
  "Command line of notmuch indexing the stores."
  (list notmuch-program (concat "--config=" (expand-file-name gnus-search-notmuch-config-file))
        "new" "--quiet"))

(defun index-synced-mail (status start)
  "Index the stores, then end the sync begun at START, whose mbsync gave STATUS."
  (run-mail-sync-step (notmuch-new-command)
                      (lambda (indexed _)
                        (finish-mail-sync (if (zerop status) indexed status) start))))

(defun run-mail-sync-tier (runs markers start)
  "Run mbsync once with each of RUNS until one fails, then index the stores.
A clean pass moves MARKERS.  START is when the sync began."
  (if runs
      (run-mail-sync-step (apply #'mbsync-command (car runs))
                          (lambda (status _)
                            (if (zerop status)
                                (run-mail-sync-tier (cdr runs) markers start)
                              (index-synced-mail status start))))
    (dolist (marker markers)
      (write-region "" nil (expand-file-name marker mail-sync-directory) nil 'silent))
    (index-synced-mail 0 start)))

;;;###autoload
(defun mail-sync-running-p ()
  "Non-nil while a mail sync or a label change runs.
A step's process reads as exited before its sentinel starts the next."
  (and (processp mail-sync-process)
       (not (process-get mail-sync-process 'ended))))

;;;###autoload
(defun start-mail-sync (&optional full report)
  "Start a mail sync in the background; FULL runs every tier.
Nothing starts while a sync or a label change runs.  REPORT has the
sync say in the echo area how it ended.  Non-nil when one started."
  (unless (mail-sync-running-p)
    (pcase-let* ((tier (mail-sync-tier full))
                 (`(,runs ,markers) (alist-get tier mail-sync-tiers))
                 (start (current-time)))
      (setq mail-sync-report report)
      (make-directory mail-sync-directory t)
      (ensure-notmuch-config)
      (mail-sync-log "--- %s %s sync ---" (format-time-string "%F %T" start) tier)
      (if (eq tier 'inbox)
          (run-mail-sync-tier runs markers start)
        (run-mail-sync-step (mbsync-command "-ls" "gmail-remote")
                            (lambda (status listing)
                              (set-aside-dropped-labels status listing)
                              (run-mail-sync-tier runs markers start))
                            t))
      t)))

;;;###autoload
(defun sync-mail (&optional full)
  "Sync mail with Gmail and run `mail-sync-hook', all in the background.
With FULL, the mail sync runs every tier at once.  The log shows in
another window until `mail-sync-log-bury-delay' seconds after the sync
ends, and the echo area says how it ended."
  (interactive "P")
  (if (and running-label-change (mail-sync-running-p))
      (message "A label change runs; sync once it is done")
    ;; before the start, which may end the sync at once
    (show-mail-sync-log)
    (unless (start-mail-sync full t)
      (setq mail-sync-report t)
      (message "A mail sync runs already; it says when it is done")))
  (run-hooks 'mail-sync-hook))

;;; Label changes
;;
;; One IMAP command through curl renames or deletes a label in Gmail, and
;; its folder follows.  The isyncrc's UserCmd and PassCmd print the
;; account to curl's input, so Emacs never holds the password.

(defun mail-sync-config-value (keyword)
  "The value of KEYWORD in `mail-sync-config', unquoted as mbsync reads it."
  (with-temp-buffer
    (insert-file-contents mail-sync-config)
    (when (re-search-forward (format "^%s[ \t]+\\(.+\\)$" (regexp-quote keyword)) nil t)
      (let ((value (string-trim-right (match-string 1))))
        (if (string-match "\\`\"\\(.*\\)\"\\'" value)
            (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (match-string 1 value))
          value)))))

(defun imap-command (request)
  "Command line that sends REQUEST to the IMAP server of `mail-sync-config'."
  (list "sh" "-c"
        (concat "printf 'user = \"%s:%s\"\\n' \"$(sh -c \"$1\")\" \"$(sh -c \"$2\")\""
                " | \"$3\" --silent --show-error --config - --url \"imaps://$4/\""
                " --request \"$5\"")
        "imap" (mail-sync-config-value "UserCmd") (mail-sync-config-value "PassCmd")
        mail-imap-program (mail-sync-config-value "Host") request))

(defun run-imap-step (request then)
  "Send REQUEST to Gmail in the background, then call THEN.
THEN gets what `run-mail-sync-step' gives it."
  (run-mail-sync-step (imap-command request) then nil (concat "imap " request)))

(defun mbsync-state-holds-p (folder uidvalidity)
  "Non-nil when mbsync's state of FOLDER is for Gmail's UIDVALIDITY."
  (let ((state (expand-file-name ".mbsyncstate" folder)))
    (and (file-exists-p state)
         (with-temp-buffer
           ;; the first line; a state holds a line per message
           (insert-file-contents state nil 0 256)
           (looking-at-p (format "FarUidValidity %s$" (regexp-quote uidvalidity)))))))

(defun move-label-folder (name new)
  "Move the folder of label NAME to NEW; return nil, or why it could not."
  (condition-case err
      (progn
        (rename-file (expand-file-name name gmail-maildir) (expand-file-name new gmail-maildir))
        (mail-sync-log "label %s renamed to %s, its folder with it" name new)
        nil)
    (file-error (format "could not move the folder of label %s: %s"
                        name (error-message-string err)))))

(defun delete-gmail-label (name boxes finish)
  "Delete the label NAME, one of Gmail's BOXES, and set its folder aside.
FINISH gets nil, or why it failed.  A label no sync took to Gmail yet
has only its folder."
  (let ((box (gmail-label-box name boxes))
        (folder (expand-file-name name gmail-maildir)))
    (cond (box
           (run-imap-step (format "DELETE \"%s\"" box)
                          (lambda (status _)
                            (funcall finish
                                     (cond ((not (zerop status))
                                            (format "Gmail kept label %s" box))
                                           ((file-directory-p folder)
                                            (set-aside-label-folder name "deleted")))))))
          ((file-directory-p folder)
           (funcall finish (set-aside-label-folder name "deleted")))
          (t (funcall finish (format "no label %s" name))))))

(defun rename-gmail-label (name new boxes finish)
  "Rename the label NAME, one of Gmail's BOXES, to NEW, and its folder with it.
FINISH gets nil, or why it failed.  The folder keeps mbsync's state only
while Gmail keeps the label's UIDVALIDITY; else it goes aside, and the
next sync fetches the label anew."
  (let ((box (gmail-label-box name boxes))
        (folder (expand-file-name name gmail-maildir)))
    (cond ((or (file-exists-p (expand-file-name new gmail-maildir))
               (gmail-label-box new boxes))
           (funcall finish (format "label %s exists" new)))
          (box
           (run-imap-step
            (format "RENAME \"%s\" \"%s\"" box new)
            (lambda (status _)
              (if (not (zerop status))
                  (funcall finish (format "Gmail kept label %s" box))
                (run-imap-step
                 (format "STATUS \"%s\" (UIDVALIDITY)" new)
                 (lambda (_ output)
                   (funcall finish
                            (cond ((and (string-match "UIDVALIDITY \\([0-9]+\\)" output)
                                        (mbsync-state-holds-p folder (match-string 1 output)))
                                   (move-label-folder name new))
                                  ((file-directory-p folder)
                                   (set-aside-label-folder
                                    name (format "renamed to %s" new)))))))))))
          ((and (file-directory-p folder)
                (not (file-exists-p (expand-file-name ".mbsyncstate" folder))))
           ;; a label made here that no sync took to Gmail yet
           (funcall finish (move-label-folder name new)))
          (t (funcall finish (format "no label %s in Gmail" name))))))

(defun finish-gmail-label-change (problem done start)
  "End the label change begun at START, which PROBLEM stopped unless nil.
A change made indexes the stores first.  DONE gets PROBLEM, and then
the oldest change that waits starts."
  (let ((end (lambda ()
               (let ((seconds (round (float-time (time-since start)))))
                 (if problem
                     (mail-sync-log "--- failed in %ds: %s ---" seconds problem)
                   (mail-sync-log "--- done in %ds ---" seconds)))
               (setq running-label-change nil)
               (unwind-protect (funcall done problem)
                 (run-waiting-label-change)))))
    (if problem
        (funcall end)
      (run-mail-sync-step (notmuch-new-command) (lambda (_status _output) (funcall end))))))

(defun run-gmail-label-change (change done)
  "Make CHANGE now, then call DONE, as `start-gmail-label-change' says."
  (let ((start (current-time)))
    (setq running-label-change change)
    (make-directory mail-sync-directory t)
    (mail-sync-log "--- %s %s ---" (format-time-string "%F %T" start)
                   (pcase change
                     (`(delete ,name) (format "delete label %s" name))
                     (`(rename ,name ,new) (format "rename label %s to %s" name new))))
    (run-mail-sync-step
     (mbsync-command "-ls" "gmail-remote")
     (lambda (status listing)
       (let ((finish (lambda (problem) (finish-gmail-label-change problem done start))))
         (if-let* (((zerop status))
                   (boxes (gmail-boxes listing)))
             (pcase change
               (`(delete ,name) (delete-gmail-label name boxes finish))
               (`(rename ,name ,new) (rename-gmail-label name new boxes finish)))
           (funcall finish "could not list Gmail's labels"))))
     t)))

(defun run-waiting-label-change ()
  "Start the oldest label change that waits for the sync, if any."
  (when-let* ((next (pop waiting-label-changes)))
    (run-gmail-label-change (car next) (cdr next))))

;;;###autoload
(defun start-gmail-label-change (change done)
  "Make CHANGE in Gmail and the store in the background, then call DONE.
CHANGE is (delete NAME) or (rename NAME NEW), and DONE gets nil, or why
it failed.  A change waits for a running sync.  Non-nil when it starts."
  (if (mail-sync-running-p)
      (progn
        (setq waiting-label-changes (append waiting-label-changes (list (cons change done))))
        nil)
    (run-gmail-label-change change done)
    t))

;;;###autoload
(defun start-mail-sync-timer ()
  "Sync mail every `mail-sync-interval' seconds, the first half a minute on.
Only the Emacs that owns the server syncs, so a second Emacs or a batch
run never touches Gmail."
  (when (and (not noninteractive)
             (process-live-p (bound-and-true-p server-process))
             (not (timerp mail-sync-timer)))
    (setq mail-sync-timer (run-with-timer 30 mail-sync-interval #'start-mail-sync))))

;;; sync.el ends here
