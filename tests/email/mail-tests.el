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

(describe "mail-sync-sentinel"
  (it "refreshes the group buffer after a clean exit while Gnus runs"
    (let ((refreshed nil)
          (gnus-group-buffer " *mail-tests group*"))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                      ((symbol-function 'process-exit-status) (lambda (_) 0))
                      ((symbol-function 'gnus-alive-p) (lambda () t))
                      ((symbol-function 'gnus-group-get-new-news)
                       (lambda (&rest _) (setq refreshed (current-buffer)))))
              (mail-sync-sentinel 'proc "finished\n")
              (expect refreshed :to-be (get-buffer gnus-group-buffer)))
          (kill-buffer gnus-group-buffer)))))
  (it "leaves Gnus alone and reports a failed exit"
    (let ((refreshed nil) (said nil))
      (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                ((symbol-function 'process-exit-status) (lambda (_) 1))
                ((symbol-function 'process-buffer) (lambda (_) (get-buffer-create " *mail-tests log*")))
                ((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'gnus-group-get-new-news) (lambda (&rest _) (setq refreshed t)))
                ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (mail-sync-sentinel 'proc "exited abnormally with code 1\n")
        (expect refreshed :to-be nil)
        (expect said :to-match "Mail sync failed")))))

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
              (subscribe-mail-groups)
              ;; inbox is already in the newsrc, the dot-dir is not a group
              (expect (sort (mapcar #'cadr (nreverse calls)) #'string<)
                      :to-equal '("nnmaildir+gmail:archive"
                                  "nnmaildir+gmail:emacs"
                                  "nnmaildir+gmail:sent"
                                  "nntp+news.gmane.io:gmane.emacs.devel")))
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
