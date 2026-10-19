;;; modules/email/autoload/sync.el -*- lexical-binding: t; -*-
;;; Commentary:
;; mbsync mirrors Gmail into the store by `mail-sync-config', and notmuch
;; indexes the store after it.  Each step is a process whose sentinel
;; starts the next, so the main thread waits on none of them.
;;; Code:

(defvar doom-emacs-dir)
(defvar doom-state-dir)
(defvar gmail-maildir)
(defvar mail-from-address)
(defvar mail-inbox-address)
(defvar gnus-search-notmuch-config-file)
(defvar mail-label-change-process)
(defvar server-process)

(defvar mail-sync-hook nil
  "Hook `sync-mail' runs to fetch what lives outside Gmail.")

(defvar mbsync-program "mbsync"
  "Program that syncs `gmail-maildir' with Gmail.")

(defvar notmuch-program "notmuch"
  "Program that indexes the stores after a sync.")

(defvar mail-sync-config (expand-file-name "modules/email/isyncrc" doom-emacs-dir)
  "Config file every mbsync run reads: the account, the stores and the channels.")

(defvar mail-sync-directory (expand-file-name "mail-sync/" doom-state-dir)
  "Directory of the sync's log, its tier markers and the label folders set aside.")

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

(defconst mail-sync-tiers
  '((inbox (("inbox")) ())
    (fast (("fast") ("--pull-new" "--push-flags" "bulk")) ("last-fast"))
    (full (("fast" "bulk")) ("last-fast" "last-full")))
  "Each tier's mbsync runs, in order, and the markers a clean pass moves.
The isyncrc's group fast holds the inbox, sent, trash and the small
labels; bulk holds All Mail and the mailing-list labels.")

(defun mail-sync-log (format-string &rest args)
  "Append a line of FORMAT-STRING and ARGS to the sync's log."
  (write-region (concat (apply #'format format-string args) "\n") nil
                (expand-file-name "mail-sync.log" mail-sync-directory) t 'silent))

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

(defun run-mail-sync-step (command then &optional quiet)
  "Run COMMAND in the background, log how it ended, then call THEN.
THEN gets the exit status, 128 plus the signal, or 127 when COMMAND
cannot start, and the output.  QUIET logs the output only on a failure."
  (let* ((start (current-time))
         (buffer (get-buffer-create " *mail-sync*"))
         (finish (lambda (status output)
                   (unless (or (string-empty-p output) (and quiet (zerop status)))
                     (mail-sync-log "%s" (string-trim-right output)))
                   (mail-sync-log "%s: exit %d in %.1fs" (string-join command " ") status
                                  (float-time (time-since start)))
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

(defun dropped-label-folders (boxes)
  "Folders of synced labels that Gmail's BOXES lack.
mbsync keeps the box a/b in the folder a.b."
  (let ((folders (mapcar (lambda (box) (string-replace "/" "." box)) boxes)))
    (seq-filter (lambda (name)
                  (and (not (member name '("inbox" "sent" "trash" "archive")))
                       (not (member name folders))
                       (file-exists-p (expand-file-name (concat name "/.mbsyncstate")
                                                        gmail-maildir))))
                (directory-files gmail-maildir nil "\\`[^.]"))))

(defun set-aside-label-folder (name why)
  "Move the folder of label NAME out of the store, logging WHY it went."
  (let ((dest (expand-file-name (format "removed/%s-%s" name
                                        (format-time-string "%Y%m%d-%H%M%S"))
                                mail-sync-directory)))
    (condition-case err
        (progn
          (make-directory (file-name-directory dest) t)
          (rename-file (expand-file-name name gmail-maildir) dest)
          ;; its age counts from the move
          (set-file-times dest)
          (mail-sync-log "label %s %s, its folder moved to %s" name why dest))
      (file-error
       (mail-sync-log "could not move the folder of label %s: %s"
                      name (error-message-string err))))))

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
                "\nother_email=" mail-inbox-address
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
      (message "Mail sync failed, status %d (see %s)" status
               (abbreviate-file-name (expand-file-name "mail-sync.log" mail-sync-directory)))))
  (when (and (featurep 'gnus) (gnus-alive-p))
    (refresh-synced-mail-groups start)))

(defun index-synced-mail (status start)
  "Index the stores, then end the sync begun at START, whose mbsync gave STATUS."
  (run-mail-sync-step (list notmuch-program
                            (concat "--config=" (expand-file-name gnus-search-notmuch-config-file))
                            "new" "--quiet")
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
  "Non-nil while a mail sync runs.
A step's process reads as exited before its sentinel starts the next."
  (and (processp mail-sync-process)
       (not (process-get mail-sync-process 'ended))))

;;;###autoload
(defun start-mail-sync (&optional full report)
  "Start a mail sync in the background; FULL runs every tier.
Nothing starts while a sync or a label change runs.  REPORT has the
sync say in the echo area how it ended.  Non-nil when one started."
  (unless (or (mail-sync-running-p)
              (process-live-p (bound-and-true-p mail-label-change-process)))
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
With FULL, the mail sync runs every tier at once.  The echo area says
how it ended."
  (interactive "P")
  (cond ((start-mail-sync full t))
        ((mail-sync-running-p)
         (setq mail-sync-report t)
         (message "A mail sync runs already; it says when it is done"))
        (t (message "A label change runs; sync once it is done")))
  (run-hooks 'mail-sync-hook))

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
