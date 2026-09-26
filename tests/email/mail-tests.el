;;; tests/email/mail-tests.el --- email module autoload specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar gmail-maildir "/nonexistent-mail-tests/")
(defvar mail-sync-program "mail-sync")
(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-groups '("nnmaildir+gmail:inbox" "nntp+news.gmane.io:gmane.emacs.devel"))

(load-module-file "modules/email/autoload/mail.el")

(defun mail-tests--newsrc (specs)
  "Newsrc hashtable and group list for SPECS, each (GROUP LEVEL METHOD).
Returns the hashtable; the caller binds `gnus-group-list' itself."
  (let ((table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,group ,level ,method) specs)
      (puthash group (list nil (gnus-info-make group level nil nil method)) table))
    table))

(describe "gmail-message-url"
  (it "searches Gmail by rfc822msgid with the brackets stripped"
    (expect (gmail-message-url "<abc@example.com>")
            :to-equal "https://mail.google.com/mail/u/0/#search/rfc822msgid%3Aabc%40example.com"))
  (it "accepts a bare id"
    (expect (gmail-message-url "abc@example.com")
            :to-match "rfc822msgid%3Aabc%40example.com\\'")))

(describe "list-archive-message-url"
  (it "points gnu.org lists at yhetil"
    (expect (list-archive-message-url "<id@x>" "emacs-devel@gnu.org, Someone <s@y.org>")
            :to-equal "https://yhetil.org/emacs-devel/id%40x"))
  (it "points the org-mode list at list.orgmode.org"
    (expect (list-archive-message-url "<id@x>" "emacs-orgmode@gnu.org")
            :to-equal "https://list.orgmode.org/orgmode/id%40x/"))
  (it "points google groups at the topic search"
    (expect (list-archive-message-url "<id@x>" "clojure@googlegroups.com")
            :to-equal "https://groups.google.com/forum/#!topicsearchin/clojure/messageid$3A%22id%40x%22"))
  (it "returns nil when no list address is among the recipients"
    (expect (list-archive-message-url "<id@x>" "someone@example.com") :to-be nil)))

(describe "mail-sync-command"
  (it "runs the incremental sync by default"
    (expect (mail-sync-command) :to-equal '("mail-sync" "sync")))
  (it "runs the full sync on request"
    (expect (mail-sync-command t) :to-equal '("mail-sync" "full"))))

(defmacro mail-tests--with-sentinel-stubs (code calls &rest body)
  "Run BODY with the sentinel's dependencies stubbed, PROC exiting with CODE.
Every refresh path logs into CALLS, so a spec can tell the scoped refresh
from the server-wide scan that froze the frame."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
             ((symbol-function 'process-exit-status) (lambda (_) ,code))
             ((symbol-function 'process-buffer)
              (lambda (_) (get-buffer-create " *mail-tests log*")))
             ((symbol-function 'refresh-mail-groups) (lambda () (push 'refresh ,calls)))
             ((symbol-function 'gnus-group-get-new-news)
              (lambda (&rest _) (push 'scan-every-group ,calls))))
     ,@body))

(describe "mail-sync-sentinel"
  (it "refreshes the routine mail groups after a clean exit while Gnus runs"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t)))
        (mail-tests--with-sentinel-stubs 0 calls
          (mail-sync-sentinel 'proc "finished\n")))
      (expect calls :to-equal '(refresh))))
  (it "asks for nothing when Gnus is not running"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () nil)))
        (mail-tests--with-sentinel-stubs 0 calls
          (mail-sync-sentinel 'proc "finished\n")))
      (expect calls :to-be nil)))
  (it "leaves Gnus alone and reports a failed exit"
    (let ((calls nil) (said nil))
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (mail-tests--with-sentinel-stubs 1 calls
          (mail-sync-sentinel 'proc "exited abnormally with code 1\n")))
      (expect calls :to-be nil)
      (expect said :to-match "Mail sync failed"))))

(describe "scanned-mail-groups"
  (it "takes the maildir groups at or below the activate level, and nothing else"
    (let* ((gnus-activate-level 3)
           (gnus-newsrc-hashtb
            (mail-tests--newsrc
             '(("nnmaildir+gmail:inbox" 3 (nnmaildir "gmail"))
               ("nnmaildir+gmail:sent" 2 (nnmaildir "gmail"))
               ;; above the activate level: a scan of this one reads the
               ;; whole label
               ("nnmaildir+gmail:emacs" 4 (nnmaildir "gmail"))
               ;; low level, but a scan of it is an NNTP round trip
               ("nntp+news.gmane.io:gmane.emacs.devel" 1 (nntp "news.gmane.io"))
               ("nndraft:drafts" 1 (nndraft "")))))
           (gnus-group-list (hash-table-keys gnus-newsrc-hashtb)))
      (expect (sort (scanned-mail-groups) #'string<)
              :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent")))))

(describe "refresh-mail-groups"
  (it "rescans each group by name and redraws its line"
    (let (calls)
      (cl-letf (((symbol-function 'scanned-mail-groups)
                 (lambda () '("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent")))
                ((symbol-function 'refresh-mail-group)
                 (lambda (g) (push (list 'refresh g) calls)))
                ((symbol-function 'gnus-group-update-group)
                 (lambda (g &rest _) (push (list 'redraw g) calls))))
        (refresh-mail-groups)
        (expect (nreverse calls)
                :to-equal '((refresh "nnmaildir+gmail:inbox")
                            (redraw "nnmaildir+gmail:inbox")
                            (refresh "nnmaildir+gmail:sent")
                            (redraw "nnmaildir+gmail:sent")))))))

(describe "queue-mail-refresh"
  (it "queues the routine groups, the inbox first, for one timer turn"
    (let ((mail-refresh-queue nil)
          (mail-refresh-timer nil)
          (turns nil))
      (cl-letf (((symbol-function 'scanned-mail-groups)
                 (lambda () (list "nnmaildir+gmail:sent" "nnmaildir+gmail:inbox"
                                  "nnmaildir+gmail:job")))
                ((symbol-function 'run-with-timer)
                 (lambda (secs repeat fn) (push (list secs repeat fn) turns) (timer-create))))
        (queue-mail-refresh)
        (expect mail-refresh-queue
                :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent"
                            "nnmaildir+gmail:job"))
        (expect turns :to-equal '((0 nil refresh-next-mail-group)))
        ;; a start asks twice, once per form of the server's method
        (queue-mail-refresh)
        (expect (length turns) :to-equal 1)))))

(defmacro mail-tests--with-turn-stubs (calls alive &rest body)
  "Run BODY with a turn's dependencies logging into CALLS, Gnus ALIVE or not."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'gnus-alive-p) (lambda () ,alive))
             ((symbol-function 'run-with-timer)
              (lambda (&rest _) (push 'next-turn ,calls) (timer-create)))
             ((symbol-function 'refresh-mail-group)
              (lambda (g) (push (list 'refresh g) ,calls)))
             ((symbol-function 'gnus-group-update-group)
              (lambda (g &rest _) (push (list 'redraw g) ,calls))))
     ,@body))

(describe "refresh-next-mail-group"
  (it "sets the next turn, then reads one group and redraws its line"
    (let ((mail-refresh-queue (list "nnmaildir+gmail:inbox" "nnmaildir+gmail:sent"))
          (mail-refresh-timer nil)
          (calls nil))
      (mail-tests--with-turn-stubs calls t
        (refresh-next-mail-group))
      ;; set first, so a group that fails to read leaves the rest running
      (expect (nreverse calls)
              :to-equal '(next-turn
                          (refresh "nnmaildir+gmail:inbox")
                          (redraw "nnmaildir+gmail:inbox")))
      (expect mail-refresh-queue :to-equal '("nnmaildir+gmail:sent"))
      (expect (timerp mail-refresh-timer) :to-be t)))
  (it "sets no turn after the last group"
    (let ((mail-refresh-queue (list "nnmaildir+gmail:sent"))
          (mail-refresh-timer (timer-create))
          (calls nil))
      (mail-tests--with-turn-stubs calls t
        (refresh-next-mail-group))
      (expect (nreverse calls)
              :to-equal '((refresh "nnmaildir+gmail:sent") (redraw "nnmaildir+gmail:sent")))
      (expect mail-refresh-queue :to-be nil)
      (expect mail-refresh-timer :to-be nil)))
  (it "reads nothing once Gnus has gone"
    (let ((mail-refresh-queue (list "nnmaildir+gmail:inbox"))
          (mail-refresh-timer nil)
          (calls nil))
      (mail-tests--with-turn-stubs calls nil
        (refresh-next-mail-group))
      (expect calls :to-be nil)
      (expect mail-refresh-timer :to-be nil))))

(describe "defer-mail-server-scan-a"
  (it "passes the scan of one group through"
    (let (calls)
      (cl-letf (((symbol-function 'queue-mail-refresh) (lambda () (push 'queue calls))))
        (expect (defer-mail-server-scan-a
                 (lambda (group server) (push (list 'scan group server) calls) 'scanned)
                 "inbox" "gmail")
                :to-be 'scanned))
      (expect calls :to-equal '((scan "inbox" "gmail")))))
  (it "queues the routine groups instead of scanning the whole server"
    (let (calls)
      (cl-letf (((symbol-function 'queue-mail-refresh) (lambda () (push 'queue calls))))
        (expect (defer-mail-server-scan-a (lambda (&rest args) (push (cons 'scan args) calls)))
                :to-be t))
      (expect calls :to-equal '(queue)))))

(describe "scan-mail-group-on-miss-a"
  (it "answers a hit without reading the group"
    (let (scans)
      (cl-letf (((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans))))
        (expect (scan-mail-group-on-miss-a (lambda (&rest _) 7) "1700.1.host" "inbox" "gmail")
                :to-equal 7))
      (expect scans :to-be nil)))
  (it "reads the group on a miss and asks again"
    ;; notmuch answers from the archive, which no start reads
    (let ((read nil) (scans nil))
      (cl-letf (((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans) (setq read t))))
        (expect (scan-mail-group-on-miss-a (lambda (&rest _) (and read 23195))
                                           "1700.1.host" "archive" "gmail")
                :to-equal 23195))
      (expect scans :to-equal '(("archive" "gmail"))))))

(describe "scan-unknown-mail-group-a"
  (it "answers for a group nnmaildir knows without reading it again"
    (let (scans)
      (cl-letf (((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans))))
        (expect (scan-unknown-mail-group-a (lambda (&rest _) t) "inbox" "gmail" nil nil)
                :to-be t))
      (expect scans :to-be nil)))
  (it "reads a group nnmaildir refused, then asks again with every argument"
    (let ((read nil) (scans nil) (asked nil))
      (cl-letf (((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans) (setq read t))))
        (expect (scan-unknown-mail-group-a
                 (lambda (&rest args) (push args asked) read)
                 "emacs" "gmail" t)
                :to-be t))
      (expect scans :to-equal '(("emacs" "gmail")))
      (expect asked :to-equal '(("emacs" "gmail" t) ("emacs" "gmail" t))))))

(describe "defer-bulk-mail-groups"
  (it "moves a subscribed bulk group one level above the activate level"
    (let* ((gnus-activate-level 3)
           (gnus-newsrc-hashtb
            (mail-tests--newsrc '(("nnmaildir+gmail:emacs" 3 (nnmaildir "gmail"))
                                  ("nnmaildir+gmail:inbox" 3 (nnmaildir "gmail")))))
           (gnus-group-list (hash-table-keys gnus-newsrc-hashtb))
           (gnus-newsrc-alist (mapcar (lambda (g) (nth 1 (gnus-group-entry g)))
                                      gnus-group-list))
           (gnus-active-hashtb (make-hash-table :test #'equal))
           (mail-bulk-groups '("nnmaildir+gmail:emacs")))
      (defer-bulk-mail-groups)
      (expect (gnus-group-level "nnmaildir+gmail:emacs") :to-equal 4)
      (expect (gnus-group-level "nnmaildir+gmail:inbox") :to-equal 3)))
  (it "leaves a group that already sits there, and one the newsrc never had"
    (let* ((gnus-activate-level 3)
           (gnus-newsrc-hashtb
            (mail-tests--newsrc '(("nnmaildir+gmail:emacs" 4 (nnmaildir "gmail")))))
           (gnus-group-list (hash-table-keys gnus-newsrc-hashtb))
           (mail-bulk-groups '("nnmaildir+gmail:emacs" "nnmaildir+gmail:absent"))
           changes)
      (cl-letf (((symbol-function 'gnus-group-change-level)
                 (lambda (&rest args) (push args changes))))
        (defer-bulk-mail-groups))
      (expect changes :to-be nil))))

(defmacro mail-tests--with-gnus-stubs (calls &rest body)
  "Run BODY with the Gnus entry points stubbed to log into CALLS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gnus-subscribe-newsgroup)
              (lambda (g &rest _) (push (list 'subscribe g) ,calls)))
             ((symbol-function 'gnus-find-method-for-group) (lambda (_) '(nnmaildir "gmail")))
             ((symbol-function 'gnus-activate-group)
              (lambda (g scan &rest _) (push (list 'activate g scan) ,calls)))
             ((symbol-function 'gnus-request-update-info)
              (lambda (&rest _) (push 'update-info ,calls)))
             ((symbol-function 'gnus-get-unread-articles-in-group)
              (lambda (&rest _) (push 'count-unread ,calls)))
             ((symbol-function 'gnus-summary-read-group)
              (lambda (g &rest _) (push (list 'read g) ,calls))))
     ,@body))

(describe "subscribe-mail-groups"
  (it "subscribes every maildir group, since gnus-search drops a hit in a group nnmaildir never opened"
    (let ((calls nil)
          (deferred nil)
          (gmail-maildir (make-temp-file "mail-tests-maildir" t))
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-group-buffer " *mail-tests group*"))
      (puthash "nnmaildir+gmail:inbox" '(entry (info)) gnus-newsrc-hashtb)
      (dolist (d '("inbox" "sent" "archive" "emacs"))
        (make-directory (expand-file-name d gmail-maildir)))
      ;; nnmaildir keeps its own state in a dot-dir beside the groups
      (make-directory (expand-file-name ".mbsyncstate" gmail-maildir))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (mail-tests--with-gnus-stubs calls
              (cl-letf (((symbol-function 'defer-bulk-mail-groups)
                         (lambda () (setq deferred t))))
                (subscribe-mail-groups))
              ;; inbox is already in the newsrc, the dot-dir is not a group
              (expect (sort (mapcar #'cadr (nreverse calls)) #'string<)
                      :to-equal '("nnmaildir+gmail:archive"
                                  "nnmaildir+gmail:emacs"
                                  "nnmaildir+gmail:sent"
                                  "nntp+news.gmane.io:gmane.emacs.devel"))
              ;; a fresh subscription lands at the default level, so the
              ;; bulk groups have to be moved out of the scan afterwards
              (expect deferred :to-be t))
          (kill-buffer gnus-group-buffer)
          (delete-directory gmail-maildir t))))))

(describe "open-mail-inbox"
  (it "starts Gnus, subscribes the inbox once, rescans it, then reads it"
    (let ((calls nil)
          (alive nil)
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-active-hashtb (make-hash-table :test #'equal))
          (gnus-group-buffer " *mail-tests group*"))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'gnus-alive-p) (lambda () alive))
                      ((symbol-function 'gnus) (lambda (&rest _) (setq alive t) (push 'gnus calls))))
              (mail-tests--with-gnus-stubs calls
                (open-mail-inbox))
              (expect (nreverse calls)
                      :to-equal '(gnus
                                  (subscribe "nnmaildir+gmail:inbox")
                                  (activate "nnmaildir+gmail:inbox" scan)
                                  (read "nnmaildir+gmail:inbox"))))
          (kill-buffer gnus-group-buffer)))))
  (it "skips the subscription and merges the flags when the group is known"
    (let ((calls nil)
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-active-hashtb (make-hash-table :test #'equal)))
      (puthash "nnmaildir+gmail:inbox" '(entry (info)) gnus-newsrc-hashtb)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t)))
        (mail-tests--with-gnus-stubs calls
          (open-mail-inbox))
        (expect (nreverse calls)
                :to-equal '((activate "nnmaildir+gmail:inbox" scan)
                            update-info
                            count-unread
                            (read "nnmaildir+gmail:inbox")))))))

(describe "read-mail-article"
  (it "selects the article before asking for its buffer"
    ;; gnus-summary-select-article-buffer errors when no article buffer
    ;; exists, which is the state every group is entered in
    (let (calls)
      (cl-letf (((symbol-function 'gnus-summary-select-article)
                 (lambda (&rest _) (push 'select calls)))
                ((symbol-function 'gnus-summary-select-article-buffer)
                 (lambda (&rest _) (push 'select-buffer calls))))
        (read-mail-article)
        (expect (nreverse calls) :to-equal '(select select-buffer))))))

(defmacro mail-tests--sorting (sorts &rest body)
  "Run BODY in a buffer of its own with each summary sort logged into SORTS."
  (declare (indent 1))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'gnus-summary-sort)
                (lambda (predicate reverse) (push (list predicate reverse) ,sorts))))
       ,@body)))

(defun mail-tests--press (command &optional previous)
  "Call COMMAND the way a key does, PREVIOUS being the command before it."
  (let ((this-command command)
        (last-command previous))
    (funcall command)))

(describe "sort-mail"
  (it "sorts forward, reverses when the same sort runs again, then goes forward again"
    ;; Gnus's own sort commands reverse only on a prefix argument
    (let (sorts)
      (mail-tests--sorting sorts
        (mail-tests--press #'sort-mail-by-date 'evil-next-line)
        (mail-tests--press #'sort-mail-by-date #'sort-mail-by-date)
        (mail-tests--press #'sort-mail-by-date #'sort-mail-by-date))
      (expect (nreverse sorts)
              :to-equal '((most-recent-date nil) (most-recent-date t) (most-recent-date nil)))))
  (it "starts forward when another sort ran last, reversed or not"
    (let (sorts)
      (mail-tests--sorting sorts
        (mail-tests--press #'sort-mail-by-author 'evil-next-line)
        (mail-tests--press #'sort-mail-by-author #'sort-mail-by-author)
        (mail-tests--press #'sort-mail-by-subject #'sort-mail-by-author))
      (expect (nreverse sorts)
              :to-equal '((author nil) (author t) (subject nil)))))
  (it "keeps the direction per summary"
    (let (sorts)
      (mail-tests--sorting sorts
        (mail-tests--press #'sort-mail-by-date 'evil-next-line)
        (mail-tests--press #'sort-mail-by-date #'sort-mail-by-date)
        (expect mail-sort-reversed :to-be t))
      (with-temp-buffer
        (expect mail-sort-reversed :to-be nil)))))

(describe "toggle-mail-thread-fold"
  (it "unfolds a folded thread and leaves it at that"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-summary-show-thread)
                 (lambda () (push 'show calls) 42))
                ((symbol-function 'gnus-summary-hide-thread)
                 (lambda () (push 'hide calls))))
        (toggle-mail-thread-fold))
      (expect calls :to-equal '(show))))
  (it "folds a thread that had nothing folded"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-summary-show-thread)
                 (lambda () (push 'show calls) nil))
                ((symbol-function 'gnus-summary-hide-thread)
                 (lambda () (push 'hide calls))))
        (toggle-mail-thread-fold))
      (expect (nreverse calls) :to-equal '(show hide)))))

(describe "open-message-in-gmail"
  (it "opens the message the current buffer shows, looked up in its summary"
    (let ((summary (generate-new-buffer " *mail-tests summary*"))
          opened)
      (unwind-protect
          (progn
            (with-current-buffer summary
              (setq-local gnus-newsgroup-data
                          (list (gnus-data-make
                                 7 gnus-read-mark 1
                                 (make-full-mail-header 7 "s" "a@x" "" "<seven@x>" "" 0 0)
                                 0))))
            (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7)))
                      ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
              (with-temp-buffer
                (open-message-in-gmail)))
            (expect opened :to-equal (gmail-message-url "<seven@x>")))
        (kill-buffer summary)))))

(describe "search-mail"
  (it "hands the raw notmuch query to an ephemeral search over the gmail server"
    (let (captured)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'subscribe-mail-groups) #'ignore)
                ((symbol-function 'gnus-group-read-ephemeral-search-group)
                 (lambda (_no-parse specs) (setq captured specs))))
        (search-mail "from:someone subject:hello")
        (expect (cdr (assq 'search-query-spec captured))
                :to-equal '((query . "from:someone subject:hello") (raw . t)))
        (expect (cdr (assq 'search-group-spec captured))
                :to-equal '(("nnmaildir:gmail")))))))
