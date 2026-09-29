;;; modules/email/autoload/load.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'nnmaildir)

(defvar doom-cache-dir)
(defvar doom-emacs-dir)
(defvar mail-inbox-group)
(defvar mail-archive-group)
(defvar news-fetch-process)

;; the table format lives with the batch side that writes it
(require 'mail-load (expand-file-name "scripts/mail-load" doom-emacs-dir))

(defvar mail-load-script (expand-file-name "scripts/mail-load.el" doom-emacs-dir)
  "Batch script that reads maildir groups for the session.")

(defvar mail-load-directory (expand-file-name "mail-load/" doom-cache-dir)
  "Init directory of the Emacs that runs `mail-load-script'.")

(defvar mail-table-directory (expand-file-name "mail-tables/" doom-cache-dir)
  "Where `mail-load-script' saves the table of each group it reads.")

(defvar mail-load-slice 0.03
  "Seconds of table work per timer turn.")

(defvar mail-load-queue nil
  "Maildir groups waiting for a batch read, the next first.")

(defvar mail-load-running nil
  "Groups the running batch has yet to report, in its order.")

(defvar mail-load-process nil
  "The running batch Emacs, or nil.")

(defvar mail-load-tables nil
  "(GROUP . FILE) of each table the batch reported and nothing applied yet.")

(defvar mail-load-job nil
  "The table being applied, or nil.")

(defvar mail-load-timer nil
  "Timer of the next `mail-load-step', or nil.")

(defvar mail-load-waiters nil
  "(KEY WHAT PREDICATE ACTION) of work waiting for loads, one per KEY.")

(defvar mail-load-said-at 0.0
  "When `mail-load-progress' last spoke.")

(defvar mail-load-gc-threshold nil
  "The `gc-cons-threshold' the loads raised, until Emacs idles; nil otherwise.")

(cl-defstruct (mail-load-job (:constructor make-mail-load-job) (:copier nil))
  "A table being turned into an nnmaildir group, a slice at a time."
  group server buffer header grp nlist ready)

;;; Groups

;;;###autoload
(defun maildir-group-p (group)
  "Non-nil when GROUP is an nnmaildir group."
  (string-prefix-p "nnmaildir+" group))

(defun mail-group-server (group)
  "The nnmaildir server struct of GROUP, or nil while that server is closed."
  (alist-get (car (mail-table-group-parts group)) nnmaildir--servers nil nil #'equal))

;;;###autoload
(defun mail-group-loaded-p (group)
  "Non-nil when nnmaildir holds GROUP in memory."
  (when-let* ((server (mail-group-server group))
              (groups (nnmaildir--srv-groups server)))
    (gethash (cdr (mail-table-group-parts group)) groups)))

;;;###autoload
(defun mail-group-pending-p (group)
  "Non-nil while GROUP waits for a batch read or for its table."
  (or (member group mail-load-queue)
      (member group mail-load-running)
      (assoc group mail-load-tables)
      (and mail-load-job (equal group (mail-load-job-group mail-load-job)))))

;;;###autoload
(defun mail-group-reading-p (group)
  "Non-nil while the batch has GROUP to read, which the session must leave alone."
  (member group mail-load-running))

(defun mail-load-parked-p (group)
  "Non-nil when GROUP is a news group and a news fetch delivers into it now.
Its read waits for the fetch, whose end asks for it again."
  (and (news-group-p group)
       (boundp 'news-fetch-process)
       (process-live-p news-fetch-process)))

;;;###autoload
(defun mail-load-busy-p ()
  "Non-nil while a group is read or waits for a read that can start."
  (or mail-load-running mail-load-tables mail-load-job
      (seq-remove #'mail-load-parked-p mail-load-queue)))

(defun mail-group-store (group)
  "Directory of the nnmaildir server GROUP lives on."
  (let ((server (car (mail-table-group-parts group))))
    (seq-some (lambda (method)
                (when (and (eq (car method) 'nnmaildir) (equal (cadr method) server))
                  (file-name-as-directory
                   (expand-file-name (cadr (assq 'directory (cddr method)))))))
              gnus-secondary-select-methods)))

;;;###autoload
(defun mail-full-group-name (group server)
  "Full name of nnmaildir GROUP on SERVER, the current server when nil."
  (when-let* ((server (or server
                          (and nnmaildir--cur-server
                               (nnmaildir--srv-address nnmaildir--cur-server)))))
    (concat "nnmaildir+" server ":" group)))

;;; The batch

(defun mail-load-command (groups)
  "Command line of the batch Emacs that reads GROUPS."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch" "--init-directory" mail-load-directory
        "-l" mail-load-script
        "--eval" (format "(mail-load-main %S '%S)" mail-table-directory
                         (mapcar (lambda (group)
                                   ;; a group nnmaildir lacks needs its table
                                   ;; even when nothing changed
                                   (list group (mail-group-store group)
                                         (not (mail-group-loaded-p group))))
                                 groups))))

;;;###autoload
(defun load-mail-groups (groups &optional first)
  "Read maildir GROUPS in a batch Emacs and apply what changed, in the background.
With FIRST they go ahead of the rest, even when that stops a batch busy
with other groups; one it reads for the first time right now is not read
twice."
  (let* ((groups (seq-uniq (seq-filter (lambda (group)
                                         (and (maildir-group-p group) (mail-group-store group)))
                                       groups)))
         (groups (if first
                     (seq-remove (lambda (group)
                                   (and (equal group (car mail-load-running))
                                        (not (mail-group-loaded-p group))))
                                 groups)
                   groups)))
    (when (and first groups (process-live-p mail-load-process))
      (let ((process mail-load-process))
        (setq mail-load-queue (append mail-load-running mail-load-queue)
              mail-load-running nil
              mail-load-process nil)
        (delete-process process)))
    (setq mail-load-queue
          (if first
              (append groups (seq-difference mail-load-queue groups))
            (append mail-load-queue (seq-difference groups mail-load-queue))))
    (when (and groups (gnus-alive-p))
      (mail-load-hold-gc))
    (start-mail-load)))

(defun start-mail-load ()
  "Start a batch Emacs on the queued groups unless one runs or Gnus is down.
A news group waits while a news fetch delivers into it."
  (when (and (gnus-alive-p) (not (process-live-p mail-load-process)))
    (let ((groups (seq-remove #'mail-load-parked-p mail-load-queue)))
      (when groups
        (make-directory mail-load-directory t)
        (setq mail-load-queue (seq-difference mail-load-queue groups)
              mail-load-running groups
              mail-load-process (make-process :name "mail-load"
                                              :buffer (get-buffer-create " *mail-load*")
                                              :command (mail-load-command groups)
                                              :connection-type 'pipe :noquery t
                                              :filter #'mail-load-filter
                                              :sentinel #'mail-load-sentinel))
        ;; a question the batch Emacs asked would wait for an answer forever
        (process-send-eof mail-load-process)))))

(defun mail-load-filter (process output)
  "Log OUTPUT of PROCESS and act on each report it completes."
  (let ((lines (split-string (concat (process-get process 'pending) output) "\n")))
    (process-put process 'pending (car (last lines)))
    (dolist (line (butlast lines))
      (when (buffer-live-p (process-buffer process))
        (with-current-buffer (process-buffer process)
          (goto-char (point-max))
          (insert line "\n")))
      (when (and (eq process mail-load-process) (string-prefix-p "(mail-load " line))
        (mail-load-note (cdr (car (read-from-string line))))))))

(defun mail-load-note (report)
  "Act on REPORT, one group's result from the batch."
  (pcase report
    (`(loaded ,group ,file)
     ;; a newer table of the group replaces one not applied yet
     (setq mail-load-tables (append (seq-remove (lambda (entry) (equal (car entry) group))
                                                mail-load-tables)
                                    (list (cons group file))))
     (when (and mail-load-job (equal group (mail-load-job-group mail-load-job)))
       (kill-buffer (mail-load-job-buffer mail-load-job))
       (setq mail-load-job nil)))
    (`(failed ,group ,reason)
     (message "Could not read %s: %s" group reason)))
  (setq mail-load-running (delete (nth 1 report) mail-load-running))
  (mail-load-schedule))

(defun mail-load-sentinel (process event)
  "Forget PROCESS once EVENT ends it, report the groups it never read, go on."
  (when (and (memq (process-status process) '(exit signal))
             (eq process mail-load-process))
    (dolist (group mail-load-running)
      (message "Could not read %s: %s" group (string-trim event)))
    (setq mail-load-running nil
          mail-load-process nil)
    (when (and (boundp 'news-fetch-pending) news-fetch-pending)
      (fetch-news))
    (start-mail-load)
    (mail-load-schedule)))

;;; Applying a table

(defun mail-load-schedule ()
  "Run `mail-load-step' on the next timer turn unless it is due already."
  (unless (timerp mail-load-timer)
    (setq mail-load-timer (run-with-timer 0 nil #'mail-load-step))))

(defun mail-load-begin (group file)
  "Start applying GROUP's table FILE, opening its server first if need be.
Gnus opens a server only when a group on it is read, and no start
reads a group of the news server."
  (when-let* ((server (or (mail-group-server group)
                          (progn (gnus-check-server (gnus-find-method-for-group group))
                                 (mail-group-server group)))))
    (let ((buffer (generate-new-buffer " *mail-table*" t)))
      (with-current-buffer buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (let ((header (read buffer)))
          (make-mail-load-job :group group :server server :buffer buffer :header header
                              :grp (mail-table-new-group (cdr (mail-table-group-parts group))
                                                         header)))))))

(defun mail-load-rows (job deadline)
  "Add JOB's next table rows to its group, a hundred at a time, until DEADLINE.
Non-nil once every row is in."
  (with-current-buffer (mail-load-job-buffer job)
    (let ((group (mail-load-job-grp job))
          (nlist (mail-load-job-nlist job))
          done)
      (while (progn
               (dotimes (_ 100)
                 (unless done
                   (skip-chars-forward "\n")
                   (if (eobp)
                       (setq done t)
                     (push (mail-table-add-row group (read (current-buffer))) nlist))))
               (and (not done) (< (float-time) deadline))))
      (setf (mail-load-job-nlist job) nlist)
      done)))

(defun mail-load-mark-times (header)
  "The mark times of the table HEADER as nnmaildir keeps them, a hash table."
  (let ((times (make-hash-table)))
    (pcase-dolist (`(,mark . ,time) (plist-get header :mmth))
      (puthash mark time times))
    times))

(defun mail-load-apply-marks (info header before)
  "Give INFO the read ranges and marks of the table HEADER.
BEFORE holds the file times INFO's marks came from; a mark whose files
kept their time keeps what INFO has, as `nnmaildir-request-update-info'
does.  BEFORE is nil on a group's first read."
  (let (read marks)
    (pcase-dolist (`(,mark . ,time) (plist-get header :mmth))
      (let* ((then (and before (gethash mark before)))
             (ranges (if (and then (time-equal-p time then))
                         (if (eq mark 'read)
                             (gnus-info-read info)
                           (alist-get mark (gnus-info-marks info)))
                       (if (eq mark 'read)
                           (plist-get header :read)
                         (alist-get mark (plist-get header :marks))))))
        (if (eq mark 'read)
            (setq read ranges)
          (when ranges
            (push (cons mark ranges) marks)))))
    (setf (gnus-info-read info) read)
    (gnus-info-set-marks info marks 'extend)))

(defun mail-table-fresh-p (dir header)
  "Non-nil when no file in maildir DIR changed since the table HEADER was read.
Mail arriving in new/ meanwhile waits for the next read, as before."
  (time-equal-p (plist-get header :cur)
                (file-attribute-modification-time (file-attributes (nnmaildir--cur dir)))))

(defun mail-load-keep-newer (group old dir)
  "Give GROUP, read from a table, what OLD learned of maildir DIR since.
That is a file renamed by a flag this session saved, or one it delivered."
  (let ((curdir (nnmaildir--cur dir))
        (flist (nnmaildir--grp-flist group))
        extra)
    (maphash (lambda (prefix before)
               (let ((after (gethash prefix flist))
                     (suffix (nnmaildir--art-suffix before)))
                 (when (and (not (and after (equal suffix (nnmaildir--art-suffix after))))
                            (file-exists-p (concat curdir prefix suffix)))
                   (if after
                       (setf (nnmaildir--art-suffix after) suffix)
                     (push before extra)))))
             (nnmaildir--grp-flist old))
    (when extra
      (dolist (article extra)
        (puthash (nnmaildir--art-prefix article) article flist)
        (puthash (nnmaildir--art-msgid article) article (nnmaildir--grp-mlist group)))
      (mail-table-finish-group
       group
       (cl-merge 'list (nnmaildir--grp-nlist group)
              (sort (mapcar (lambda (article) (cons (nnmaildir--art-num article) article))
                            extra)
                    (lambda (a b) (< (car b) (car a))))
              (lambda (a b) (< (car b) (car a))))))))

(defun mail-load-finish (job)
  "Hand JOB's group to nnmaildir and bring its line in the group buffer up to date.
A maildir that changed while the batch read it is read again."
  (let* ((group (mail-load-job-group job))
         (server (mail-load-job-server job))
         (header (mail-load-job-header job))
         (name (cdr (mail-table-group-parts group)))
         (dir (nnmaildir--srvgrp-dir (nnmaildir--srv-dir server) name))
         (groups (nnmaildir--srv-groups server))
         (old (gethash name groups))
         (loaded (mail-load-job-grp job))
         (fresh (mail-table-fresh-p dir header)))
    ;; a Gnus restart meanwhile opened a server of its own
    (when (and groups (eq server (mail-group-server group)))
      (mail-table-finish-group loaded (mail-load-job-nlist job))
      (when old
        (mail-load-keep-newer loaded old dir))
      (let ((marks (or fresh (not old))))
        (setf (nnmaildir--grp-mmth loaded)
              (if marks (mail-load-mark-times header) (nnmaildir--grp-mmth old)))
        (puthash name loaded groups)
        (when (eq (nnmaildir--srv-curgrp server) old)
          (setf (nnmaildir--srv-curgrp server) loaded))
        (when-let* ((info (gnus-get-info group)))
          (when marks
            (mail-load-apply-marks info header (and old (nnmaildir--grp-mmth old))))
          (gnus-activate-group group nil nil (gnus-find-method-for-group group))
          (gnus-get-unread-articles-in-group info (gnus-active group))
          (gnus-group-update-group group t)))
      (unless fresh
        (load-mail-groups (list group))))))

(defun mail-load-step ()
  "Apply tables for up to `mail-load-slice' seconds, then yield to the keyboard.
A group's finish takes a turn of its own."
  (setq mail-load-timer nil)
  (mail-load-hold-gc)
  (let ((deadline (+ (float-time) mail-load-slice)))
    (condition-case err
        (while (and (or mail-load-job mail-load-tables)
                    (not (input-pending-p))
                    (progn
                      (cond ((null mail-load-job)
                             (pcase-let ((`(,group . ,file) (pop mail-load-tables)))
                               (setq mail-load-job (mail-load-begin group file))))
                            ((mail-load-job-ready mail-load-job)
                             (let ((job mail-load-job))
                               (setq mail-load-job nil
                                     deadline 0)
                               (kill-buffer (mail-load-job-buffer job))
                               (mail-load-finish job)))
                            ((mail-load-rows mail-load-job deadline)
                             (setf (mail-load-job-ready mail-load-job) t
                                   deadline 0)))
                      (< (float-time) deadline))))
      (error
       (message "Could not apply a mail table: %s" (error-message-string err))
       (when mail-load-job
         (kill-buffer (mail-load-job-buffer mail-load-job))
         (setq mail-load-job nil)))))
  (mail-load-run-waiters)
  (mail-load-progress)
  (if (or mail-load-job mail-load-tables)
      (setq mail-load-timer
            (run-with-timer (if (input-pending-p) 0.05 0) nil #'mail-load-step))
    (run-with-idle-timer 1 nil #'mail-load-collect)))

(defun mail-load-hold-gc ()
  "Keep garbage collection off until the loads are done and Emacs idles.
A collection takes about 0.1 s in a large session, three slices' worth,
and reading every group allocates about 60 MB."
  (unless mail-load-gc-threshold
    (setq mail-load-gc-threshold gc-cons-threshold
          gc-cons-threshold (max gc-cons-threshold (* 128 1024 1024)))))

(defun mail-load-collect ()
  "Restore the GC threshold the loads raised, and collect what they left."
  (when (and mail-load-gc-threshold (not (or mail-load-job mail-load-tables)))
    (setq gc-cons-threshold mail-load-gc-threshold
          mail-load-gc-threshold nil)
    (garbage-collect)))

;;; Work waiting for loads

;;;###autoload
(defun wait-for-mail-load (key what predicate action)
  "Run ACTION once PREDICATE holds, which a finished load makes true.
KEY replaces earlier work of the same kind, and WHAT says in progress
messages what waits, such as \"for the search\"."
  (setq mail-load-waiters (cons (list key what predicate action)
                                (seq-remove (lambda (waiter) (eq (car waiter) key))
                                            mail-load-waiters))
        mail-load-said-at 0.0)
  (mail-load-run-waiters)
  (mail-load-progress))

(defun mail-load-run-waiters ()
  "Run the waiting work whose loads are done."
  (dolist (waiter mail-load-waiters)
    (pcase-let ((`(,_key ,_what ,predicate ,action) waiter))
      (when (funcall predicate)
        (setq mail-load-waiters (delq waiter mail-load-waiters))
        (condition-case err
            (funcall action)
          (error (message "%s" (error-message-string err))))))))

(defun mail-load-status ()
  "What the loads do now, such as \"archive (40%)\"."
  (cond (mail-load-job
         (with-current-buffer (mail-load-job-buffer mail-load-job)
           (format "%s (%d%%)" (gnus-group-short-name (mail-load-job-group mail-load-job))
                   (/ (* 100 (point)) (max 1 (point-max))))))
        (mail-load-running
         (format "%s (reading its folder)" (gnus-group-short-name (car mail-load-running))))
        (t "the mail")))

(defun mail-load-progress ()
  "Say what the waiting work waits for, at most every quarter second."
  (when-let* ((waiter (car mail-load-waiters))
              ((not (active-minibuffer-window)))
              ((< 0.25 (- (float-time) mail-load-said-at))))
    (setq mail-load-said-at (float-time))
    (let ((message-log-max nil))
      (message "Loading %s %s..." (mail-load-status) (nth 1 waiter)))))

(defun gnus-shown-p ()
  "Non-nil when the selected window shows a Gnus buffer."
  (with-current-buffer (window-buffer (selected-window))
    (derived-mode-p 'gnus-group-mode 'gnus-summary-mode 'gnus-article-mode 'mail-thread-mode)))

;;;###autoload
(defun wait-for-mail-group-a (fn group &rest args)
  "Call FN on GROUP and ARGS, or once GROUP loads when nnmaildir lacks it.
The group opens then if Gnus is still on screen."
  (if (or (not (maildir-group-p group)) (mail-group-loaded-p group))
      (apply fn group args)
    (let ((name (gnus-group-short-name group)))
      (load-mail-groups (list group) t)
      (wait-for-mail-load
       'open (concat "to open " name)
       (lambda () (or (mail-group-loaded-p group) (not (mail-group-pending-p group))))
       (lambda ()
         (cond ((not (mail-group-loaded-p group))
                (message "%s did not load" name))
               ((and (gnus-shown-p) (gnus-buffer-live-p gnus-group-buffer))
                (with-current-buffer gnus-group-buffer
                  (apply fn group args)))
               (t (message "%s is ready" name))))))
    nil))

;;;###autoload
(defun wait-for-mail-loads-a (fn &rest args)
  "Call FN with ARGS once no maildir group is loading.
A search maps its hits through the groups nnmaildir holds."
  (if (not (mail-load-busy-p))
      (apply fn args)
    (let ((buffer (current-buffer)))
      (wait-for-mail-load 'search "for the search"
                          (lambda () (not (mail-load-busy-p)))
                          (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (apply fn args))))))
    nil))

(defvar nnmaildir-status-string ""
  "Why nnmaildir refused a request, where Gnus's move reports look.
nnmaildir keeps its own errors in the server struct only.")

;;;###autoload
(defun refuse-unloaded-mail-group-a (fn group &optional server &rest args)
  "Call FN on GROUP, SERVER and ARGS once nnmaildir holds GROUP.
nnmaildir would read GROUP whole first; its load starts instead, and the
request fails saying so."
  (let ((full (mail-full-group-name group server)))
    (if (or (not full) (mail-group-loaded-p full))
        (apply fn group server args)
      (load-mail-groups (list full) t)
      (setq nnmaildir-status-string (format "%s is still loading" group))
      (when-let* ((srv (mail-group-server full)))
        (setf (nnmaildir--srv-error srv) nnmaildir-status-string))
      nil)))

;;;###autoload
(defun load-all-mail-groups ()
  "Read every maildir group Gnus knows in the background, the inbox first.
The routine groups follow, then the rest, All Mail last."
  (let* ((groups (seq-filter #'maildir-group-p gnus-group-list))
         (routine (seq-filter (lambda (group) (<= (gnus-group-level group) gnus-activate-level))
                              groups)))
    (load-mail-groups (seq-uniq (seq-filter (lambda (group) (member group groups))
                                            (append (list mail-inbox-group) routine
                                                    (remove mail-archive-group groups)
                                                    (list mail-archive-group)))))))

;;;###autoload
(defun load-unloaded-mail-groups ()
  "Read each maildir group Gnus knows that nnmaildir lacks and nothing reads.
A label mbsync created since the start is one."
  (load-mail-groups (seq-remove (lambda (group)
                                  (or (mail-group-loaded-p group) (mail-group-pending-p group)))
                                (seq-filter #'maildir-group-p gnus-group-list))))

;;;###autoload
(defun stop-mail-load ()
  "Drop every load and the work waiting for one; Gnus is leaving."
  (when (process-live-p mail-load-process)
    (delete-process mail-load-process))
  (when (timerp mail-load-timer)
    (cancel-timer mail-load-timer))
  (when mail-load-job
    (kill-buffer (mail-load-job-buffer mail-load-job)))
  (when mail-load-gc-threshold
    (setq gc-cons-threshold mail-load-gc-threshold
          mail-load-gc-threshold nil))
  (setq mail-load-process nil
        mail-load-queue nil
        mail-load-running nil
        mail-load-tables nil
        mail-load-job nil
        mail-load-timer nil
        mail-load-waiters nil))

;;; load.el ends here
