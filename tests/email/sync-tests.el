;;; tests/email/sync-tests.el --- mail sync specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
;; the end of a sync asks Gnus whether it runs
(require 'gnus)

;; config.el, labels.el and server.el define these; the specs bind them
(defvar gmail-maildir)
(defvar mail-from-address)
(defvar mail-inbox-address)
(defvar gnus-search-notmuch-config-file)
(defvar mail-label-change-process)
(defvar server-process)

(load-module-file "modules/email/autoload/sync.el")
;; a sync no spec set up must not reach Gmail
(setq mbsync-program "/nonexistent/mbsync"
      notmuch-program "/nonexistent/notmuch")

(defun sync-tests--program (dir name answers)
  "Write the stand-in program NAME into DIR/bin/, which logs each call to DIR/calls.
ANSWERS maps the arguments after the config, joined by spaces, to (EXIT
OUTPUT), EXIT `hang' for a run that sleeps and `read' for one that reads
a line; any other call exits 0.  While DIR/hold exists the program waits."
  (let ((file (expand-file-name (concat "bin/" name) dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "#!/bin/sh\n"
              (format "printf '%%s\\n' \"%s $*\" >> %s\n"
                      name (shell-quote-argument (expand-file-name "calls" dir)))
              (format "while [ -f %s ]; do sleep 0.02; done\n"
                      (shell-quote-argument (expand-file-name "hold" dir)))
              "[ \"$1\" = -c ] && shift 2\n"
              "case \"$1\" in --config=*) shift;; esac\n"
              "case \"$*\" in\n")
      (pcase-dolist (`(,args ,exit ,output) answers)
        (insert (format "  %s) %s;;\n" (shell-quote-argument args)
                        (pcase exit
                          ('hang "exec sleep 10")
                          ;; 3 once the input ended, as a prompt would see it
                          ('read "read answer; exit 3")
                          (_ (format "printf '%%s' %s; exit %d"
                                     (shell-quote-argument output) exit))))))
      (insert "esac\n"))
    (set-file-modes file #o755)
    file))

(defun sync-tests--wait ()
  "Wait up to 5 s for the running sync to end."
  (with-timeout (5)
    (while (mail-sync-running-p)
      (accept-process-output nil 0.02))))

(defmacro sync-tests--with (answers &rest body)
  "Run BODY over an empty store, stand-in mbsync and notmuch answering ANSWERS.
`dir' names the temp dir: the store is DIR/mail/gmail/, the sync keeps
its state in DIR/state/, and DIR/calls lists the programs' calls."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "sync-tests" t)))
          (gmail-maildir (expand-file-name "mail/gmail/" dir))
          (mail-sync-directory (expand-file-name "state/" dir))
          (mail-sync-config (expand-file-name "isyncrc" dir))
          (gnus-search-notmuch-config-file (expand-file-name "notmuch/config" dir))
          (mail-from-address "ann@example.com")
          (mail-inbox-address "inbox@example.com")
          (mbsync-program (sync-tests--program dir "mbsync" ,answers))
          (notmuch-program (sync-tests--program dir "notmuch" ,answers))
          (mail-sync-process nil)
          (mail-sync-report nil)
          (mail-sync-hook nil)
          (mail-label-change-process nil))
     (make-directory gmail-maildir t)
     (make-directory mail-sync-directory t)
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file (expand-file-name "hold" dir)))
       (sync-tests--wait)
       (when (process-live-p mail-sync-process)
         (delete-process mail-sync-process))
       (delete-directory dir t))))

(defun sync-tests--calls (dir)
  "Each call the stand-ins under DIR logged, in order."
  (let ((file (expand-file-name "calls" dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t)))))

(defun sync-tests--mbsync (args)
  "The call of the stand-in mbsync with ARGS, as it logs it."
  (format "mbsync -c %s %s" mail-sync-config args))

(defun sync-tests--notmuch ()
  "The call of the stand-in notmuch that indexes the store, as it logs it."
  (format "notmuch --config=%s new --quiet" gnus-search-notmuch-config-file))

(defun sync-tests--marker (name age)
  "Make the marker NAME of the sync's directory AGE seconds old."
  (let ((file (expand-file-name name mail-sync-directory)))
    (write-region "" nil file nil 'silent)
    (set-file-times file (time-subtract nil age))))

(defun sync-tests--fresh-markers ()
  "Make both markers a minute old, so a sync takes the inbox tier."
  (sync-tests--marker "last-full" 60)
  (sync-tests--marker "last-fast" 60))

(defun sync-tests--age (name)
  "Seconds since the marker NAME moved, or nil without it."
  (when-let* ((time (file-attribute-modification-time
                     (file-attributes (expand-file-name name mail-sync-directory)))))
    (float-time (time-since time))))

(defun sync-tests--log ()
  "What the sync's log holds."
  (let ((file (expand-file-name "mail-sync.log" mail-sync-directory)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun sync-tests--folder (name &optional synced)
  "Make the label folder NAME in the store; SYNCED gives it mbsync's state file."
  (let ((folder (expand-file-name name gmail-maildir)))
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name sub folder) t))
    (when synced
      (write-region "" nil (expand-file-name ".mbsyncstate" folder) nil 'silent))))

(defun sync-tests--store ()
  "The folders of the store, sorted."
  (sort (directory-files gmail-maildir nil "\\`[^.]") #'string<))

(defun sync-tests--removed ()
  "What the sync's removed directory holds, sorted."
  (let ((removed (expand-file-name "removed/" mail-sync-directory)))
    (and (file-directory-p removed)
         (sort (directory-files removed nil "\\`[^.]") #'string<))))

(defun sync-tests--sync (&optional full)
  "Run one sync, FULL or by the markers, to its end."
  (prog1 (start-mail-sync full)
    (sync-tests--wait)))

(defun sync-tests--said (fn)
  "Call FN; return what it said in the echo area, oldest first."
  (let (said)
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (when format
                   (push (apply #'format format args) said)))))
      (funcall fn))
    (nreverse said)))

(describe "mail-sync-tier"
  (it "takes the full tier once its marker is a day old, or missing"
    (sync-tests--with nil
      (sync-tests--marker "last-fast" 60)
      (expect (mail-sync-tier) :to-be 'full)
      (sync-tests--marker "last-full" (* 25 3600))
      (expect (mail-sync-tier) :to-be 'full)))
  (it "takes the fast tier once its marker is half an hour old, or missing"
    (sync-tests--with nil
      (sync-tests--marker "last-full" 60)
      (expect (mail-sync-tier) :to-be 'fast)
      (sync-tests--marker "last-fast" (* 31 60))
      (expect (mail-sync-tier) :to-be 'fast)))
  (it "syncs the inbox alone while both markers are younger than that"
    (sync-tests--with nil
      (sync-tests--marker "last-full" (* 23 3600))
      (sync-tests--marker "last-fast" (* 29 60))
      (expect (mail-sync-tier) :to-be 'inbox)))
  (it "takes the full tier when asked, whatever the markers say"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (expect (mail-sync-tier t) :to-be 'full))))

(describe "start-mail-sync"
  (it "syncs the inbox alone through the module's isyncrc, then indexes the store"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (expect (sync-tests--sync) :to-be-truthy)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "inbox") (sync-tests--notmuch)))
      ;; neither marker moved
      (expect (< 59 (sync-tests--age "last-fast")) :to-be t)
      (expect (< 59 (sync-tests--age "last-full")) :to-be t)))
  (it "lists Gmail's labels first on the fast tier, syncs the small boxes, then the bulk ones' news"
    (sync-tests--with nil
      (sync-tests--marker "last-full" 60)
      (sync-tests--sync)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "-ls gmail-remote")
                              (sync-tests--mbsync "fast")
                              (sync-tests--mbsync "--pull-new --push-flags bulk")
                              (sync-tests--notmuch)))
      ;; the clean pass moved the fast marker alone
      (expect (< (sync-tests--age "last-fast") 5) :to-be t)
      (expect (< 59 (sync-tests--age "last-full")) :to-be t)))
  (it "skips the bulk boxes after a failed fast run, indexes still, and moves no marker"
    (sync-tests--with '(("fast" 1 ""))
      (sync-tests--marker "last-full" 60)
      (sync-tests--sync)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "-ls gmail-remote")
                              (sync-tests--mbsync "fast")
                              (sync-tests--notmuch)))
      (expect (sync-tests--age "last-fast") :to-be nil)))
  (it "moves no marker when the bulk boxes fail"
    (sync-tests--with '(("--pull-new --push-flags bulk" 1 ""))
      (sync-tests--marker "last-full" 60)
      (sync-tests--marker "last-fast" 3600)
      (sync-tests--sync)
      (expect (< 3599 (sync-tests--age "last-fast")) :to-be t)
      (expect (car (last (sync-tests--calls dir))) :to-equal (sync-tests--notmuch))))
  (it "syncs every box at once on the full tier and moves both markers"
    (sync-tests--with nil
      (sync-tests--sync)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "-ls gmail-remote")
                              (sync-tests--mbsync "fast bulk")
                              (sync-tests--notmuch)))
      (expect (< (sync-tests--age "last-fast") 5) :to-be t)
      (expect (< (sync-tests--age "last-full") 5) :to-be t)))
  (it "moves no marker when the full run fails"
    (sync-tests--with '(("fast bulk" 1 ""))
      (sync-tests--sync)
      (expect (list (sync-tests--age "last-fast") (sync-tests--age "last-full"))
              :to-equal '(nil nil))))
  (it "runs the full tier on request, though both markers are fresh"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (sync-tests--sync t)
      (expect (sync-tests--calls dir) :to-contain (sync-tests--mbsync "fast bulk"))))
  (it "returns while mbsync runs, and starts no second sync until it ends"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (write-region "" nil (expand-file-name "hold" dir))
      (let ((start (float-time)))
        (expect (start-mail-sync) :to-be-truthy)
        (expect (< (- (float-time) start) 0.2) :to-be t))
      (expect (mail-sync-running-p) :to-be-truthy)
      (expect (start-mail-sync) :to-be nil)
      (delete-file (expand-file-name "hold" dir))
      (sync-tests--wait)
      (expect (mail-sync-running-p) :to-be nil)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "inbox") (sync-tests--notmuch)))))
  (it "runs until its step's sentinel ran, though the step's process reads as exited"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (start-mail-sync)
      (let ((process mail-sync-process)
            (deadline (+ (float-time) 5)))
        ;; a wait that runs no sentinel, while the status catches up
        (while (and (process-live-p process) (< (float-time) deadline)))
        (expect (process-live-p process) :to-be nil)
        (expect (mail-sync-running-p) :to-be-truthy)
        (expect (start-mail-sync) :to-be nil))
      (sync-tests--wait)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "inbox") (sync-tests--notmuch)))))
  (it "starts nothing while a label change runs"
    (sync-tests--with nil
      (let ((mail-label-change-process
             (make-process :name "sync-tests-label" :command '("sleep" "10") :noquery t)))
        (unwind-protect
            (progn
              (expect (start-mail-sync) :to-be nil)
              (expect (mail-sync-running-p) :to-be nil)
              (expect (sync-tests--calls dir) :to-be nil))
          (delete-process mail-label-change-process)))))
  (it "goes on when a program cannot start, and logs why"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (let ((mbsync-program (expand-file-name "missing" dir)))
        (sync-tests--sync))
      (expect (sync-tests--calls dir) :to-equal (list (sync-tests--notmuch)))
      (expect (sync-tests--log) :to-match "/missing -c [^\n]* inbox: exit 127 in ")
      (expect (sync-tests--log) :to-match "status 127 ---\n\\'")))
  (it "counts a run a signal ended as failed"
    (sync-tests--with '(("fast" hang ""))
      (sync-tests--marker "last-full" 60)
      (sync-tests--marker "last-fast" 3600)
      (start-mail-sync)
      ;; the listing went through, and the fast run sleeps
      (with-timeout (5)
        (while (< (length (sync-tests--calls dir)) 2)
          (accept-process-output nil 0.02)))
      (signal-process mail-sync-process 'TERM)
      (sync-tests--wait)
      (expect (sync-tests--log) :to-match " fast: exit 143 in ")
      (expect (sync-tests--calls dir) :not :to-contain
              (sync-tests--mbsync "--pull-new --push-flags bulk"))
      (expect (< 3599 (sync-tests--age "last-fast")) :to-be t)))
  (it "counts a run that was stopped and went on as one run"
    (sync-tests--with '(("inbox" hang ""))
      (sync-tests--fresh-markers)
      (start-mail-sync)
      (with-timeout (5)
        (while (null (sync-tests--calls dir))
          (accept-process-output nil 0.02)))
      (signal-process mail-sync-process 'STOP)
      (accept-process-output nil 0.2)
      (signal-process mail-sync-process 'CONT)
      (accept-process-output nil 0.2)
      (expect (mail-sync-running-p) :to-be-truthy)
      (signal-process mail-sync-process 'TERM)
      (sync-tests--wait)
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "inbox") (sync-tests--notmuch)))
      (expect (sync-tests--log) :to-match " inbox: exit 143 in ")))
  (it "closes each run's input, so a question fails the run instead of waiting"
    (sync-tests--with '(("inbox" read ""))
      (sync-tests--fresh-markers)
      (sync-tests--sync)
      (expect (sync-tests--log) :to-match " inbox: exit 3 in ")))
  (it "counts a failed index as a failed sync"
    (sync-tests--with '(("new --quiet" 1 ""))
      (sync-tests--fresh-markers)
      (sync-tests--sync)
      (expect (sync-tests--log) :to-match "status 1 ---\n\\'")))
  (it "logs the start with its tier, each run's report, exit status and time, and the end"
    (sync-tests--with '(("inbox" 0 "Channels: 1    Boxes: 1\n"))
      (sync-tests--fresh-markers)
      (sync-tests--sync)
      ;; no blank line for the index, which said nothing
      (expect (length (split-string (sync-tests--log) "\n")) :to-equal 6)
      (let ((lines (split-string (sync-tests--log) "\n" t)))
        (expect (length lines) :to-equal 5)
        (expect (nth 0 lines)
                :to-match "\\`--- [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]\\{8\\} inbox sync ---\\'")
        (expect (nth 1 lines) :to-equal "Channels: 1    Boxes: 1")
        (expect (nth 2 lines)
                :to-match (concat "\\`" (regexp-quote (string-join (mbsync-command "inbox") " "))
                                  ": exit 0 in [0-9]+\\.[0-9]s\\'"))
        (expect (nth 3 lines) :to-match ": exit 0 in [0-9]+\\.[0-9]s\\'")
        (expect (nth 4 lines) :to-match "\\`--- done in [0-9]+s, status 0 ---\\'"))))
  (it "keeps a good label listing out of the log, and a failed one in it"
    (sync-tests--with '(("-ls gmail-remote" 0 "===== gmail-remote:\nINBOX\nsomelabel\n"))
      (sync-tests--sync)
      (expect (sync-tests--log) :not :to-match "somelabel"))
    (sync-tests--with '(("-ls gmail-remote" 1 "IMAP command LIST returned an error\n"))
      (sync-tests--sync)
      (expect (sync-tests--log) :to-match "IMAP command LIST returned an error"))))

(describe "labels Gmail dropped"
  (it "moves the folder of a synced label Gmail no longer lists out of the store"
    (sync-tests--with '(("-ls gmail-remote" 0
                         "===== gmail-remote:\nINBOX\njob\nwork/qlik\n[Gmail]/All Mail\n"))
      (dolist (name '("inbox" "job" "work.qlik" "gone"))
        (sync-tests--folder name t))
      ;; made here, and no sync took it to Gmail yet
      (sync-tests--folder "fresh")
      (sync-tests--sync t)
      (expect (sync-tests--store) :to-equal '("fresh" "inbox" "job" "work.qlik"))
      (expect (length (sync-tests--removed)) :to-equal 1)
      (expect (car (sync-tests--removed)) :to-match "\\`gone-[0-9]\\{8\\}-[0-9]\\{6\\}\\'")
      (expect (file-exists-p (expand-file-name (concat "removed/" (car (sync-tests--removed))
                                                       "/.mbsyncstate")
                                               mail-sync-directory))
              :to-be t)
      (expect (sync-tests--log) :to-match "label gone is gone from Gmail, its folder moved to ")
      ;; before mbsync runs, which would fail on the vanished box
      (expect (sync-tests--calls dir)
              :to-equal (list (sync-tests--mbsync "-ls gmail-remote")
                              (sync-tests--mbsync "fast bulk")
                              (sync-tests--notmuch)))))
  (it "moves it on the fast tier too"
    (sync-tests--with '(("-ls gmail-remote" 0 "===== gmail-remote:\nINBOX\n"))
      (sync-tests--marker "last-full" 60)
      (sync-tests--folder "gone" t)
      (sync-tests--sync)
      (expect (sync-tests--store) :to-be nil)))
  (it "keeps the inbox, sent, trash and archive, whose boxes Gmail names otherwise"
    (sync-tests--with '(("-ls gmail-remote" 0 "===== gmail-remote:\nINBOX\n"))
      (dolist (name '("inbox" "sent" "trash" "archive"))
        (sync-tests--folder name t))
      (sync-tests--sync t)
      (expect (sync-tests--store) :to-equal '("archive" "inbox" "sent" "trash"))))
  (it "moves nothing when the listing fails or lacks the inbox, and syncs anyway"
    (dolist (answer '(("-ls gmail-remote" 1 "IMAP error\n")
                      ;; cut off after the inbox
                      ("-ls gmail-remote" 1 "===== gmail-remote:\nINBOX\n")
                      ("-ls gmail-remote" 0 "===== gmail-remote:\njob\n")))
      (sync-tests--with (list answer)
        (sync-tests--folder "gone" t)
        (sync-tests--sync t)
        (expect (sync-tests--store) :to-equal '("gone"))
        (expect (sync-tests--log) :to-match "could not list Gmail's labels, no folder moved")
        (expect (sync-tests--calls dir) :to-contain (sync-tests--mbsync "fast bulk")))))
  (it "leaves the label folders alone on the inbox tier"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (sync-tests--folder "gone" t)
      (sync-tests--sync)
      (expect (sync-tests--store) :to-equal '("gone"))))
  (it "logs a folder that cannot move, and syncs anyway"
    (sync-tests--with '(("-ls gmail-remote" 0 "===== gmail-remote:\nINBOX\n"))
      (sync-tests--folder "gone" t)
      ;; a file where the removed directory goes
      (write-region "" nil (expand-file-name "removed" mail-sync-directory))
      (sync-tests--sync t)
      (expect (sync-tests--store) :to-equal '("gone"))
      (expect (sync-tests--log) :to-match "could not move the folder of label gone: ")
      (expect (sync-tests--calls dir) :to-contain (sync-tests--mbsync "fast bulk"))))
  (it "deletes the folders set aside over 30 days ago, in a process of its own"
    (sync-tests--with '(("-ls gmail-remote" 0 "===== gmail-remote:\nINBOX\n"))
      (let ((removed (expand-file-name "removed/" mail-sync-directory)))
        (dolist (entry '(("old-20260901-000000" . 31) ("recent-20261001-000000" . 29)))
          (make-directory (expand-file-name (concat (car entry) "/cur") removed) t)
          (set-file-times (expand-file-name (car entry) removed)
                          (time-subtract nil (* (cdr entry) 86400))))
        ;; the folder of a big label takes long to delete
        (cl-letf (((symbol-function 'delete-directory)
                   (lambda (&rest _) (error "Deleted in the main thread"))))
          (sync-tests--sync t))
        (with-timeout (5)
          (while (member "old-20260901-000000" (sync-tests--removed))
            (accept-process-output nil 0.02)))
        (expect (sync-tests--removed) :to-equal '("recent-20261001-000000")))))
  (it "keeps a folder 30 days from its move, however old the folder"
    (sync-tests--with '(("-ls gmail-remote" 0 "===== gmail-remote:\nINBOX\n"))
      (sync-tests--folder "gone" t)
      (set-file-times (expand-file-name "gone" gmail-maildir) (time-subtract nil (* 40 86400)))
      (sync-tests--sync t)
      ;; a delete the prune started has gone through by now
      (accept-process-output nil 0.3)
      (expect (length (sync-tests--removed)) :to-equal 1))))

(describe "the notmuch config"
  (it "is written before the first index, with the stores' root and both addresses"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (let ((user-full-name "Ann Example"))
        (sync-tests--sync))
      (expect (with-temp-buffer
                (insert-file-contents gnus-search-notmuch-config-file)
                (buffer-string))
              :to-equal
              (concat "[database]\nmail_root=" (expand-file-name "mail" dir) "\n"
                      "[user]\nname=Ann Example\n"
                      "primary_email=ann@example.com\n"
                      "other_email=inbox@example.com\n"
                      "[new]\nignore=.nnmaildir;.mbsyncstate;.mbsyncstate.new;"
                      ".mbsyncstate.journal;.mbsyncstate.lock;.uidvalidity\n"
                      "[maildir]\nsynchronize_flags=true\n"
                      "[index]\nheader.List=List-Id\n"))))
  (it "is left alone once it exists"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (make-directory (file-name-directory gnus-search-notmuch-config-file) t)
      (write-region "[database]\nmail_root=/elsewhere\n" nil gnus-search-notmuch-config-file)
      (sync-tests--sync)
      (expect (with-temp-buffer
                (insert-file-contents gnus-search-notmuch-config-file)
                (buffer-string))
              :to-equal "[database]\nmail_root=/elsewhere\n"))))

(describe "sync-mail"
  (it "starts the mail sync in the background, then runs mail-sync-hook"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (write-region "" nil (expand-file-name "hold" dir))
      (let* ((ran nil)
             (mail-sync-hook (list (lambda () (push (and (mail-sync-running-p) t) ran)))))
        (sync-tests--said (lambda ()
                            (sync-mail)
                            (delete-file (expand-file-name "hold" dir))
                            (sync-tests--wait)))
        (expect ran :to-equal '(t)))))
  (it "says in the echo area how the mail sync ended"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (expect (sync-tests--said (lambda () (sync-mail) (sync-tests--wait)))
              :to-equal '("Mail synced")))
    (sync-tests--with '(("inbox" 1 ""))
      (sync-tests--fresh-markers)
      (expect (sync-tests--said (lambda () (sync-mail) (sync-tests--wait)))
              :to-equal (list (format "Mail sync failed, status 1 (see %s)"
                                      (abbreviate-file-name
                                       (expand-file-name "mail-sync.log" mail-sync-directory)))))))
  (it "says nothing of a sync the timer started"
    (sync-tests--with '(("inbox" 1 ""))
      (sync-tests--fresh-markers)
      (expect (sync-tests--said (lambda () (start-mail-sync) (sync-tests--wait))) :to-be nil)))
  (it "has a sync that runs already say when it ends"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (write-region "" nil (expand-file-name "hold" dir))
      (start-mail-sync)
      (expect (sync-tests--said
               (lambda ()
                 (sync-mail)
                 (delete-file (expand-file-name "hold" dir))
                 (sync-tests--wait)))
              :to-equal '("A mail sync runs already; it says when it is done" "Mail synced"))
      (expect (length (sync-tests--calls dir)) :to-equal 2)))
  (it "says a label change holds the mail sync back, and fetches the rest"
    (sync-tests--with nil
      (let* ((mail-label-change-process
              (make-process :name "sync-tests-label" :command '("sleep" "10") :noquery t))
             (ran nil)
             (mail-sync-hook (list (lambda () (setq ran t)))))
        (unwind-protect
            (expect (sync-tests--said #'sync-mail)
                    :to-equal '("A label change runs; sync once it is done"))
          (delete-process mail-label-change-process))
        (expect ran :to-be t)
        (expect (sync-tests--calls dir) :to-be nil))))
  (it "runs the full tier with a prefix argument"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (sync-tests--said (lambda () (sync-mail '(4)) (sync-tests--wait)))
      (expect (sync-tests--calls dir) :to-contain (sync-tests--mbsync "fast bulk")))))

(describe "the end of a sync"
  (it "reads again what the sync changed, once the store is indexed, while Gnus runs"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (let ((before (current-time))
            (refreshed nil))
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'refresh-synced-mail-groups)
                   (lambda (since)
                     (push (list since (length (sync-tests--calls dir))) refreshed))))
          (sync-tests--sync))
        (expect (length refreshed) :to-equal 1)
        (pcase-let ((`(,since ,calls) (car refreshed)))
          ;; since the sync's start, after both runs
          (expect (time-less-p since before) :to-be nil)
          (expect (time-less-p (current-time) since) :to-be nil)
          (expect calls :to-equal 2)))))
  (it "reads nothing while Gnus is down"
    (sync-tests--with nil
      (sync-tests--fresh-markers)
      (let (refreshed)
        (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
                  ((symbol-function 'refresh-synced-mail-groups)
                   (lambda (since) (push since refreshed))))
          (sync-tests--sync))
        (expect refreshed :to-be nil)))))

(describe "start-mail-sync-timer"
  (it "syncs every five minutes, the first half a minute on, in the Emacs that owns the server"
    (let ((mail-sync-timer nil)
          (noninteractive nil)
          (server-process (make-process :name "sync-tests-server" :command '("sleep" "10")
                                        :noquery t)))
      (unwind-protect
          (progn
            (start-mail-sync-timer)
            (start-mail-sync-timer)
            (expect (timerp mail-sync-timer) :to-be t)
            (expect (timer--repeat-delay mail-sync-timer) :to-equal 300)
            (expect (timer--function mail-sync-timer) :to-be #'start-mail-sync)
            (expect (< 25 (float-time (time-subtract (timer--time mail-sync-timer) nil)) 31)
                    :to-be t)
            (expect (seq-count (lambda (timer) (eq (timer--function timer) #'start-mail-sync))
                               timer-list)
                    :to-equal 1))
        (when (timerp mail-sync-timer)
          (cancel-timer mail-sync-timer))
        (delete-process server-process))))
  (it "starts none in batch, in an Emacs without the server, or after its server died"
    (let ((mail-sync-timer nil)
          (server-process (make-process :name "sync-tests-server" :command '("sleep" "10")
                                        :noquery t)))
      (unwind-protect
          (progn
            (start-mail-sync-timer)
            (let ((noninteractive nil))
              (let ((server-process nil))
                (start-mail-sync-timer))
              (delete-process server-process)
              (start-mail-sync-timer))
            (expect mail-sync-timer :to-be nil))
        (when (timerp mail-sync-timer)
          (cancel-timer mail-sync-timer))
        (when (process-live-p server-process)
          (delete-process server-process))))))

(describe "the sync and the main thread"
  (it "waits on nothing: each step is a process whose sentinel starts the next"
    (with-temp-buffer
      (insert-file-contents (expand-file-name "modules/email/autoload/sync.el" test-config-root))
      (dolist (fn '("call-process" "call-process-region" "process-file" "process-lines"
                    "shell-command" "shell-command-to-string" "accept-process-output"
                    "sleep-for" "sit-for"))
        (goto-char (point-min))
        (expect (and (re-search-forward (format "\\_<%s\\_>" (regexp-quote fn)) nil t) fn)
                :to-be nil))))
  (it "never asks Emacs for the password; mbsync gets it from gpg"
    (with-temp-buffer
      (insert-file-contents (expand-file-name "modules/email/autoload/sync.el" test-config-root))
      (expect (buffer-string) :not :to-match "auth-source"))))

(defun sync-tests--isyncrc-values (keyword)
  "Values of every KEYWORD line of the module's isyncrc, in order."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/email/isyncrc" test-config-root))
    (let (values)
      (while (re-search-forward (format "^%s +\\(.*\\)$" keyword) nil t)
        (push (match-string 1) values))
      (nreverse values))))

(describe "the module's isyncrc"
  (it "defines every channel and group a tier runs, and the store a listing reads"
    (let ((names (append (sync-tests--isyncrc-values "Channel")
                         (mapcar (lambda (value) (car (split-string value)))
                                 (sync-tests--isyncrc-values "Group")))))
      (pcase-dolist (`(,_tier ,runs ,_markers) mail-sync-tiers)
        (dolist (run runs)
          (dolist (arg (seq-remove (lambda (arg) (string-prefix-p "-" arg)) run))
            (expect names :to-contain arg))))
      (expect (sync-tests--isyncrc-values "IMAPStore") :to-equal '("gmail-remote"))))
  (it "keeps the mail where Gnus reads it"
    (expect (sync-tests--isyncrc-values "Path") :to-equal '("~/.mail/gmail/"))
    (expect (sync-tests--isyncrc-values "Inbox") :to-equal '("~/.mail/gmail/inbox")))
  (it "names no account: the login and the app password come from ~/.authinfo.gpg"
    (expect (sync-tests--isyncrc-values "User") :to-be nil)
    (expect (with-temp-buffer
              (insert-file-contents (expand-file-name "modules/email/isyncrc" test-config-root))
              (buffer-string))
            :not :to-match "@")
    (dolist (keyword '("UserCmd" "PassCmd"))
      (expect (car (sync-tests--isyncrc-values keyword))
              :to-match "\\`\"gpg -q --batch -d ~/\\.authinfo\\.gpg | awk '")))
  (it "prints the login and the password of the imap.gmail.com line, its pairs in any order"
    (dolist (line '("machine imap.gmail.com login ann port imaps password app-password"
                    "machine imap.gmail.com login ann password app-password"
                    "machine imap.gmail.com password app-password port imaps login ann"))
      (let ((authinfo (make-temp-file
                       "sync-tests-authinfo" nil nil
                       (concat "machine smtp.example.com login ann@example.com"
                               " password other-password port 587\n"
                               line "\n"))))
        (unwind-protect
            (pcase-dolist (`(,keyword ,printed) '(("UserCmd" "ann\n") ("PassCmd" "app-password\n")))
              ;; the command as mbsync runs it: quotes gone, escapes undone
              (let* ((value (car (sync-tests--isyncrc-values keyword)))
                     (command (replace-regexp-in-string "\\\\\\(.\\)" "\\1"
                                                        (substring value 1 -1))))
                (expect (with-temp-buffer
                          (call-process "/bin/sh" nil t nil "-c"
                                        (string-replace "gpg -q --batch -d ~/.authinfo.gpg"
                                                        (concat "cat " authinfo) command))
                          (buffer-string))
                        :to-equal printed)))
          (delete-file authinfo))))))

;;; sync-tests.el ends here
