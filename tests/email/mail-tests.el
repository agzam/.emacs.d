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
