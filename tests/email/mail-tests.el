;;; tests/email/mail-tests.el --- email module autoload specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar gmail-maildir "/nonexistent-mail-tests/")
(defvar news-maildir "/nonexistent-mail-tests-news/")
(defvar mail-sync-program "mail-sync")
(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar mail-archive-group "nnmaildir+gmail:archive")
(defvar mail-groups '("nnmaildir+gmail:inbox" "nnmaildir+news:gmane.emacs.devel"
                      "nnatom+www.reddit.com/r/emacs/new/.rss:r/emacs"))
(defvar mail-bulk-groups
  '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs" "nnmaildir+gmail:org-mode"
    "nnmaildir+gmail:new" "nnmaildir+news:gmane.emacs.devel"))

(load-module-file "scripts/mail-load.el")
(load-module-file "modules/email/autoload/load.el")
(load-module-file "modules/email/autoload/mail.el")
(load-module-file "modules/email/autoload/news.el")

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

(describe "yhetil-message-url"
  (it "points at the message's own page in the inbox"
    (expect (yhetil-message-url "emacs-devel" "<id@x>")
            :to-equal "https://yhetil.org/emacs-devel/id%40x/"))
  (it "points at its thread, nested by reply, with the message in view"
    (expect (yhetil-message-url "emacs-devel" "<id@x>" t)
            :to-equal "https://yhetil.org/emacs-devel/id%40x/t/#u"))
  (it "escapes the + and / a Message-ID may hold"
    (expect (yhetil-message-url "emacs-devel" "<jwv1.fsf-monnier+emacs/x@gnu.org>" t)
            :to-equal "https://yhetil.org/emacs-devel/jwv1.fsf-monnier%2Bemacs%2Fx%40gnu.org/t/#u")))

(defun mail-tests--inbox (head)
  "What `emacs-list-inbox' says of a message whose head is HEAD."
  (with-temp-buffer
    (insert head)
    (emacs-list-inbox)))

(describe "emacs-list-inbox"
  (it "names the yhetil inbox of each Emacs list a List-Id names"
    (expect (mapcar (lambda (list)
                      (mail-tests--inbox (format "List-Id: A list <%s.gnu.org>\n" list)))
                    '("emacs-devel" "help-gnu-emacs" "bug-gnu-emacs" "emacs-orgmode"
                      "emacs-tangents"))
            :to-equal '("emacs-devel" "emacs-user" "emacs-bugs" "orgmode" "emacs-tangents")))
  (it "finds the list among To and Cc on an own sent copy, which carries no List-Id"
    (expect (mail-tests--inbox
             "To: Eli <eliz@gnu.org>\nCc: Someone <s@y.org>, help-gnu-emacs@gnu.org\n")
            :to-equal "emacs-user"))
  (it "reads a list address written with capitals"
    (expect (mail-tests--inbox "To: Emacs-devel@gnu.org\n") :to-equal "emacs-devel"))
  (it "takes the list the copy came through over another one in Cc"
    (expect (mail-tests--inbox "Cc: emacs-devel@gnu.org\nList-Id: <emacs-orgmode.gnu.org>\n")
            :to-equal "orgmode"))
  (it "knows no inbox for a person at gnu.org or a list outside Emacs"
    (expect (mail-tests--inbox "List-Id: <guix-devel.gnu.org>\nTo: eliz@gnu.org\n")
            :to-be nil)))

(defconst mail-tests--gmane-head
  (concat "Message-ID: <id@x>\n"
          "Newsgroups: gmane.emacs.devel\n"
          "List-Id: \"Emacs development discussions.\" <emacs-devel.gnu.org>\n"
          "Archived-At: <http://permalink.gmane.org/gmane.emacs.devel/336074>\n")
  "Head of a gmane post of emacs-devel, as the news store holds one.")

(describe "list-archive-message-url"
  (it "takes the archive the message's Archived-At names"
    (expect (list-archive-message-url
             "Message-ID: <c1@reddit.com>\nArchived-At: <https://www.reddit.com/c1/>\n")
            :to-equal "https://www.reddit.com/c1/"))
  (it "points Emacs list mail at the message's page in its yhetil inbox"
    (expect (list-archive-message-url "Message-ID: <id@x>\nTo: emacs-orgmode@gnu.org\n")
            :to-equal "https://yhetil.org/orgmode/id%40x/"))
  (it "passes over gmane's dead Archived-At for the list's archive"
    (expect (list-archive-message-url mail-tests--gmane-head)
            :to-equal "https://yhetil.org/emacs-devel/id%40x/"))
  (it "points google groups at the topic search"
    (expect (list-archive-message-url "Message-ID: <id@x>\nTo: clojure@googlegroups.com\n")
            :to-equal "https://groups.google.com/forum/#!topicsearchin/clojure/messageid$3A%22id%40x%22"))
  (it "returns nil for no list it knows and no live Archived-At"
    (expect (list-archive-message-url "Message-ID: <id@x>\nTo: someone@example.com\n")
            :to-be nil)
    (expect (list-archive-message-url
             (concat "Message-ID: <id@x>\nList-Id: <ding.gnus.org>\n"
                     "Archived-At: <http://permalink.gmane.org/gmane.emacs.gnus.general/1>\n"))
            :to-be nil)))

(describe "mail-web-url"
  (it "opens a gmane post of an Emacs list in its yhetil thread, not at its dead Archived-At"
    (expect (mail-web-url "nnmaildir+news:gmane.emacs.devel" mail-tests--gmane-head)
            :to-equal "https://yhetil.org/emacs-devel/id%40x/t/#u"))
  (it "opens Emacs list mail Gmail holds on yhetil too"
    (expect (mail-web-url "nnmaildir+gmail:archive"
                          (concat "Message-ID: <id@x>\n"
                                  "List-Id: Users list for the GNU Emacs text editor"
                                  " <help-gnu-emacs.gnu.org>\n"))
            :to-equal "https://yhetil.org/emacs-user/id%40x/t/#u"))
  (it "opens an own sent copy to an Emacs list on yhetil"
    (expect (mail-web-url "nnmaildir+gmail:sent" "Message-ID: <id@x>\nTo: emacs-devel@gnu.org\n")
            :to-equal "https://yhetil.org/emacs-devel/id%40x/t/#u"))
  (it "opens a Reddit message where its Archived-At points"
    (expect (mail-web-url "nnmaildir+reddit:r.emacs"
                          (concat "Message-ID: <c1.p1@reddit.com>\n"
                                  "Archived-At: <https://www.reddit.com/r/emacs/comments/p1/t/c1/>\n"))
            :to-equal "https://www.reddit.com/r/emacs/comments/p1/t/c1/"))
  (it "opens other Gmail mail in Gmail, though its list names an archive"
    (expect (mail-web-url "nnmaildir+gmail:inbox"
                          (concat "Message-ID: <id@x>\nTo: me@example.com\n"
                                  "Archived-At: <https://lists.sr.ht/~u/list/%3Cid@x%3E>\n"))
            :to-equal (gmail-message-url "<id@x>")))
  (it "knows no page for a news post of a list outside Emacs, whose Archived-At is dead"
    (expect (mail-web-url "nnmaildir+news:gmane.emacs.gnus.general"
                          (concat "Message-ID: <id@x>\nList-Id: <ding.gnus.org>\n"
                                  "Archived-At: <http://permalink.gmane.org/gmane.emacs.gnus.general/1>\n"))
            :to-be nil)))

(describe "mail-sync-command"
  (it "runs the incremental sync by default"
    (expect (mail-sync-command) :to-equal '("mail-sync" "sync")))
  (it "runs the full sync on request"
    (expect (mail-sync-command t) :to-equal '("mail-sync" "full"))))

(describe "sync-mail"
  (it "starts the mail sync in the background, then runs mail-sync-hook"
    (let* (made
           fetched
           (mail-sync-hook (list (lambda () (push (length made) fetched)))))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args) (push (plist-get args :command) made) 'process)))
        (sync-mail))
      (expect made :to-equal '(("mail-sync" "sync")))
      ;; the hook ran once, after the sync had started
      (expect fetched :to-equal '(1)))))

(describe "mail-file-group"
  (it "names a file's group after the nnmaildir server whose store holds it"
    (let ((gnus-secondary-select-methods
           '((nnmaildir "gmail" (directory "/m/gmail/") (get-new-mail nil))
             (nnmaildir "reddit" (directory "/m/reddit"))
             (nnatom "example.org/feed"))))
      (expect (mail-file-group "/m/gmail/inbox/cur/1:2,S") :to-equal "nnmaildir+gmail:inbox")
      ;; a directory named without its trailing slash still matches
      (expect (mail-file-group "/m/reddit/r.emacs/cur/2:2,")
              :to-equal "nnmaildir+reddit:r.emacs")
      (expect (mail-file-group "/m/other/x/cur/3") :to-be nil))))

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
             ((symbol-function 'follow-mail-folders) (lambda () (push 'follow ,calls)))
             ((symbol-function 'gnus-group-get-new-news)
              (lambda (&rest _) (push 'scan-every-group ,calls))))
     ,@body))

(describe "mail-sync-sentinel"
  (it "follows the folders, then refreshes the routine groups, after a clean exit"
    ;; a label made or dropped in Gmail gains or loses its group first
    (let (calls)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t)))
        (mail-tests--with-sentinel-stubs 0 calls
          (mail-sync-sentinel 'proc "finished\n")))
      (expect (nreverse calls) :to-equal '(follow refresh))))
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
  (it "reads the routine groups again in the background"
    (let (asked)
      (cl-letf (((symbol-function 'scanned-mail-groups)
                 (lambda () '("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent")))
                ((symbol-function 'load-mail-groups)
                 (lambda (&rest args) (push args asked))))
        (refresh-mail-groups))
      (expect asked :to-equal '((("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent")))))))

(defmacro mail-tests--with-scan-stubs (calls loaded reading &rest body)
  "Run BODY with the group LOADED or not and READING by a batch or not.
Scans, loads and refreshes log into CALLS."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'mail-group-loaded-p) (lambda (_) ,loaded))
             ((symbol-function 'mail-group-reading-p) (lambda (_) ,reading))
             ((symbol-function 'load-mail-groups)
              (lambda (&rest args) (push (cons 'load args) ,calls)))
             ((symbol-function 'refresh-mail-groups) (lambda () (push 'refresh ,calls))))
     ,@body))

(describe "defer-mail-server-scan-a"
  (it "scans a group nnmaildir holds"
    (let (calls)
      (mail-tests--with-scan-stubs calls t nil
        (expect (defer-mail-server-scan-a
                 (lambda (group server) (push (list 'scan group server) calls) 'scanned)
                 "inbox" "gmail")
                :to-be 'scanned))
      (expect calls :to-equal '((scan "inbox" "gmail")))))
  (it "loads a group nnmaildir lacks instead of reading it whole here"
    (let (calls)
      (mail-tests--with-scan-stubs calls nil nil
        (expect (defer-mail-server-scan-a (lambda (&rest args) (push (cons 'scan args) calls))
                                          "emacs" "gmail")
                :to-be t))
      (expect calls :to-equal '((load ("nnmaildir+gmail:emacs") t)))))
  (it "leaves a group alone that a batch reads now"
    ;; both would number the same new files
    (let (calls)
      (mail-tests--with-scan-stubs calls t t
        (expect (defer-mail-server-scan-a (lambda (&rest args) (push (cons 'scan args) calls))
                                          "inbox" "gmail")
                :to-be t))
      (expect calls :to-be nil)))
  (it "reads the routine groups again instead of scanning the whole server"
    (let (calls)
      (mail-tests--with-scan-stubs calls t nil
        (expect (defer-mail-server-scan-a (lambda (&rest args) (push (cons 'scan args) calls)))
                :to-be t))
      (expect calls :to-equal '(refresh))))
  (it "registers no group when asked for new ones"
    ;; nnmaildir would register every label unread, and it would open empty
    (let (calls)
      (mail-tests--with-scan-stubs calls t nil
        (expect (defer-mail-server-scan-a (lambda (&rest args) (push (cons 'scan args) calls))
                                          'find-new-groups "gmail")
                :to-be t))
      (expect calls :to-be nil))))

(describe "scan-mail-group-on-miss-a"
  (it "answers a hit without reading the group"
    (let (scans)
      (cl-letf (((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans))))
        (expect (scan-mail-group-on-miss-a (lambda (&rest _) 7) "1700.1.host" "inbox" "gmail")
                :to-equal 7))
      (expect scans :to-be nil)))
  (it "reads what arrived in a group nnmaildir holds on a miss, and asks again"
    (let ((read nil) (scans nil))
      (cl-letf (((symbol-function 'mail-group-loaded-p) (lambda (_) t))
                ((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans) (setq read t))))
        (expect (scan-mail-group-on-miss-a (lambda (&rest _) (and read 23195))
                                           "1700.1.host" "archive" "gmail")
                :to-equal 23195))
      (expect scans :to-equal '(("archive" "gmail")))))
  (it "drops a hit in a group still loading rather than read it whole"
    (let (scans)
      (cl-letf (((symbol-function 'mail-group-loaded-p) #'ignore)
                ((symbol-function 'nnmaildir-request-scan)
                 (lambda (&rest args) (push args scans))))
        (expect (scan-mail-group-on-miss-a #'ignore "1700.1.host" "archive" "gmail")
                :to-be nil))
      (expect scans :to-be nil))))

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
  "Run BODY with the Gnus entry points and the loader stubbed to log into CALLS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gnus-subscribe-newsgroup)
              (lambda (g &rest _) (push (list 'subscribe g) ,calls)))
             ((symbol-function 'gnus-find-method-for-group) (lambda (_) '(nnmaildir "gmail")))
             ((symbol-function 'load-mail-groups)
              (lambda (groups &optional first) (push (list 'load groups first) ,calls)))
             ((symbol-function 'gnus-summary-read-group)
              (lambda (g &rest _) (push (list 'read g) ,calls))))
     ,@body))

(describe "subscribe-mail-groups"
  (it "subscribes every maildir group, since gnus-search drops a hit in a group nnmaildir never opened"
    (let ((calls nil)
          (deferred nil)
          (gmail-maildir (make-temp-file "mail-tests-maildir" t))
          (mail-groups '("nnmaildir+gmail:inbox" "nntp+news.gmane.io:gmane.emacs.devel"))
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
                         (lambda () (setq deferred t)))
                        ((symbol-function 'apply-mail-topics)
                         (lambda () (push '(arranged) calls))))
                (expect (sort (subscribe-mail-groups) #'string<)
                        :to-equal '("nnmaildir+gmail:archive"
                                    "nnmaildir+gmail:emacs"
                                    "nnmaildir+gmail:sent"
                                    "nntp+news.gmane.io:gmane.emacs.devel")))
              ;; inbox is already in the newsrc, the dot-dir is not a group
              (expect (sort (mapcar #'cadr (seq-filter (lambda (call) (eq (car call) 'subscribe))
                                                       calls))
                            #'string<)
                      :to-equal '("nnmaildir+gmail:archive"
                                  "nnmaildir+gmail:emacs"
                                  "nnmaildir+gmail:sent"
                                  "nntp+news.gmane.io:gmane.emacs.devel"))
              ;; a fresh subscription lands at the default level, so the
              ;; bulk groups have to be moved out of the scan afterwards
              (expect deferred :to-be t)
              ;; Gnus subscribes into the root topic, which the topics undo
              (expect (car calls) :to-equal '(arranged))
              (expect (seq-count (lambda (call) (equal call '(arranged))) calls) :to-be 1))
          (kill-buffer gnus-group-buffer)
          (delete-directory gmail-maildir t)))))
  (it "returns nothing and arranges no topic when Gnus knows every group"
    (let ((calls nil)
          (gmail-maildir (make-temp-file "mail-tests-maildir" t))
          (mail-groups '("nnmaildir+gmail:inbox"))
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-group-buffer " *mail-tests group*"))
      (puthash "nnmaildir+gmail:inbox" '(entry (info)) gnus-newsrc-hashtb)
      (make-directory (expand-file-name "inbox" gmail-maildir))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (mail-tests--with-gnus-stubs calls
              (cl-letf (((symbol-function 'defer-bulk-mail-groups) #'ignore)
                        ((symbol-function 'apply-mail-topics)
                         (lambda () (push '(arranged) calls))))
                (expect (subscribe-mail-groups) :to-be nil))
              (expect calls :to-be nil))
          (kill-buffer gnus-group-buffer)
          (delete-directory gmail-maildir t))))))

(describe "subscribe-mail-group"
  (it "subscribes no group the user killed this session"
    ;; a label hidden with dd would come back at the next look otherwise
    (let ((gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-killed-list '("nnmaildir+gmail:job"))
          (gnus-group-buffer " *mail-tests group*")
          (calls nil))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (mail-tests--with-gnus-stubs calls
              (expect (subscribe-mail-group "nnmaildir+gmail:job") :to-be nil)
              (expect calls :to-be nil))
          (kill-buffer gnus-group-buffer)))))
  (it "says whether it subscribed"
    (let ((gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-killed-list nil)
          (gnus-group-buffer " *mail-tests group*")
          (calls nil))
      (puthash "nnmaildir+gmail:inbox" '(entry (info)) gnus-newsrc-hashtb)
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (mail-tests--with-gnus-stubs calls
              (expect (subscribe-mail-group "nnmaildir+gmail:job") :to-be-truthy)
              (expect (subscribe-mail-group "nnmaildir+gmail:inbox") :to-be nil)
              (expect calls :to-equal '((subscribe "nnmaildir+gmail:job"))))
          (kill-buffer gnus-group-buffer)))))
  (it "subscribes with the method gnus-secondary-select-methods defines for the server"
    ;; Gnus resolves a server it has not opened to its bare method, and
    ;; the group would lose the server's definitions for good
    (let ((gnus-secondary-select-methods
           '((nnmaildir "gmail" (directory "/mail/"))
             (nnatom "www.example.org/feed" (nnatom-read-feed-function ignore))))
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-group-buffer " *mail-tests group*")
          (used 'none))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'gnus-subscribe-newsgroup)
                       (lambda (&rest _) (setq used gnus-override-subscribe-method))))
              (subscribe-mail-group "nnatom+www.example.org/feed:r/emacs")
              (expect used :to-equal
                      '(nnatom "www.example.org/feed" (nnatom-read-feed-function ignore))))
          (kill-buffer gnus-group-buffer))))))

(defmacro mail-tests--with-store (folders calls &rest body)
  "Run BODY over a Gmail store holding FOLDERS, Gnus's calls logged into CALLS.
The newsrc lists inbox, job, gone and a news group at their levels; gone
and the news group have no folder in the store."
  (declare (indent 2))
  `(let* ((gmail-maildir (file-name-as-directory (make-temp-file "mail-tests-store" t)))
          (gnus-newsrc-hashtb
           (mail-tests--newsrc '(("nnmaildir+gmail:inbox" 3 (nnmaildir "gmail"))
                                 ("nnmaildir+gmail:job" 3 (nnmaildir "gmail"))
                                 ("nnmaildir+gmail:gone" 4 (nnmaildir "gmail"))
                                 ("nnmaildir+news:gmane.emacs.devel" 4 (nnmaildir "news")))))
          (gnus-group-list '("nnmaildir+gmail:inbox" "nnmaildir+gmail:job" "nnmaildir+gmail:gone"
                             "nnmaildir+news:gmane.emacs.devel"))
          (gnus-newsrc-alist (cons '("dummy.group" 0 nil)
                                   (mapcar (lambda (g) (nth 1 (gnus-group-entry g)))
                                           gnus-group-list)))
          (gnus-active-hashtb (make-hash-table :test #'equal))
          (gnus-killed-list nil)
          (gnus-zombie-list nil)
          (gnus-group-change-level-function nil)
          (gnus-topic-alist (list (list "Gnus") (list "Labels" "nnmaildir+gmail:job"
                                                      "nnmaildir+gmail:gone")
                                  (list "Gmail" "nnmaildir+gmail:inbox")))
          (mail-sync-states (make-hash-table :test #'equal))
          (gnus-group-buffer " *mail-tests group*"))
     (dolist (folder ,folders)
       (make-directory (expand-file-name folder gmail-maildir)))
     (puthash "nnmaildir+gmail:gone" '(1 0) mail-sync-states)
     (puthash "nnmaildir+gmail:gone" '(1 . 9) gnus-active-hashtb)
     (get-buffer-create gnus-group-buffer)
     (unwind-protect
         (cl-letf (((symbol-function 'forget-mail-group)
                    (lambda (group) (push (list 'forget group) ,calls)))
                   ((symbol-function 'gnus-group-list-groups)
                    (lambda (&rest _) (push (list 'redraw (buffer-name)) ,calls)))
                   ((symbol-function 'refresh-mail-labels-soon)
                    (lambda (&rest _) (push '(labels) ,calls))))
           ,@body)
       (kill-buffer gnus-group-buffer)
       (delete-directory gmail-maildir t))))

(describe "drop-vanished-mail-groups"
  (it "kills each Gmail group whose folder is gone, and leaves the rest"
    (let (calls)
      (mail-tests--with-store '("inbox" "job") calls
        (cl-letf (((symbol-function 'message) #'ignore))
          (expect (drop-vanished-mail-groups) :to-equal '("nnmaildir+gmail:gone")))
        (expect (gnus-group-entry "nnmaildir+gmail:gone") :to-be nil)
        (expect gnus-group-list
                :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:job"
                            "nnmaildir+news:gmane.emacs.devel"))
        (expect (assoc "nnmaildir+gmail:gone" gnus-newsrc-alist) :to-be nil)
        ;; a news group's folder lives in another store
        (expect (gnus-group-level "nnmaildir+news:gmane.emacs.devel") :to-equal 4))))
  (it "forgets what was read of a killed group, its sync state time too"
    (let (calls)
      (mail-tests--with-store '("inbox" "job") calls
        (cl-letf (((symbol-function 'message) #'ignore))
          (drop-vanished-mail-groups))
        (expect (assoc 'forget calls) :to-equal '(forget "nnmaildir+gmail:gone"))
        (expect (seq-count (lambda (call) (eq (car call) 'forget)) calls) :to-be 1)
        (expect (gethash "nnmaildir+gmail:gone" mail-sync-states 'none) :to-be 'none)
        (expect (gnus-active "nnmaildir+gmail:gone") :to-be nil))))
  (it "leaves no killed mark, so a label made again under the name is subscribed again"
    (let (calls)
      (mail-tests--with-store '("inbox" "job") calls
        (cl-letf (((symbol-function 'message) #'ignore))
          (drop-vanished-mail-groups))
        (expect gnus-killed-list :to-be nil))))
  (it "takes a killed group out of every topic"
    (let (calls)
      (mail-tests--with-store '("inbox" "job") calls
        (cl-letf (((symbol-function 'message) #'ignore))
          (drop-vanished-mail-groups))
        (expect gnus-topic-alist
                :to-equal '(("Gnus") ("Labels" "nnmaildir+gmail:job")
                            ("Gmail" "nnmaildir+gmail:inbox"))))))
  (it "redraws the group buffer and the labels, and says which groups went"
    (let (calls said)
      (mail-tests--with-store '("inbox" "job") calls
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (drop-vanished-mail-groups))
        (expect (assoc 'redraw calls) :to-equal '(redraw " *mail-tests group*"))
        (expect (assoc 'labels calls) :to-be-truthy)
        (expect said :to-equal "Groups gone with their folders: gone"))))
  (it "kills nothing while the inbox folder is missing"
    ;; a store unmounted or not synced yet has no folder at all
    (let (calls)
      (mail-tests--with-store '("job") calls
        (expect (drop-vanished-mail-groups) :to-be nil)
        (expect (gnus-group-entry "nnmaildir+gmail:gone") :to-be-truthy)
        (expect calls :to-be nil))))
  (it "kills and redraws nothing while every folder is there"
    (let (calls)
      (mail-tests--with-store '("inbox" "job" "gone") calls
        (expect (drop-vanished-mail-groups) :to-be nil)
        (expect (length gnus-group-list) :to-be 4)
        (expect calls :to-be nil)))))

(describe "follow-mail-folders"
  (it "kills the groups whose folder went, then subscribes and reads the new ones"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'drop-vanished-mail-groups) (lambda () (push 'drop calls)))
                ((symbol-function 'subscribe-mail-groups)
                 (lambda () (push 'subscribe calls) '("nnmaildir+gmail:fresh")))
                ((symbol-function 'load-mail-groups)
                 (lambda (groups &rest _) (push (list 'load groups) calls))))
        (follow-mail-folders))
      (expect (nreverse calls)
              :to-equal '(drop subscribe (load ("nnmaildir+gmail:fresh"))))))
  (it "reads nothing when no folder is new"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'drop-vanished-mail-groups) #'ignore)
                ((symbol-function 'subscribe-mail-groups) #'ignore)
                ((symbol-function 'load-mail-groups)
                 (lambda (&rest args) (push args calls))))
        (follow-mail-folders))
      (expect calls :to-be nil)))
  (it "does nothing while Gnus is down"
    (let (calls)
      (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
                ((symbol-function 'drop-vanished-mail-groups) (lambda () (push 'drop calls)))
                ((symbol-function 'subscribe-mail-groups) (lambda () (push 'subscribe calls))))
        (follow-mail-folders))
      (expect calls :to-be nil))))

(defmacro mail-tests--with-empty-loader (&rest body)
  "Run BODY with nothing loading and nothing waiting."
  (declare (indent 0))
  `(let ((mail-load-queue nil)
         (mail-load-running nil)
         (mail-load-tables nil)
         (mail-load-job nil)
         (mail-load-waiters nil))
     ,@body))

(describe "open-mail-inbox"
  (it "starts Gnus, subscribes the inbox once, reads it afresh, then opens it"
    (let ((calls nil)
          (alive nil)
          (loaded nil)
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-group-buffer " *mail-tests group*"))
      (with-current-buffer (get-buffer-create gnus-group-buffer)
        (unwind-protect
            (mail-tests--with-empty-loader
              (cl-letf (((symbol-function 'gnus-alive-p) (lambda () alive))
                        ((symbol-function 'gnus) (lambda (&rest _) (setq alive t) (push 'gnus calls)))
                        ((symbol-function 'mail-group-loaded-p) (lambda (_) loaded)))
                (mail-tests--with-gnus-stubs calls
                  (cl-letf (((symbol-function 'load-mail-groups)
                             (lambda (groups &optional first)
                               (push (list 'load groups first) calls)
                               (setq mail-load-queue groups))))
                    (open-mail-inbox)
                    ;; nothing opens before the fresh read lands
                    (expect (assq 'read calls) :to-be nil)
                    (setq mail-load-queue nil
                          loaded t)
                    (mail-load-run-waiters))))
              (expect (nreverse calls)
                      :to-equal '(gnus
                                  (subscribe "nnmaildir+gmail:inbox")
                                  (load ("nnmaildir+gmail:inbox") t)
                                  (read "nnmaildir+gmail:inbox"))))
          (kill-buffer gnus-group-buffer)))))
  (it "says so when the inbox did not load"
    (let ((calls nil)
          (said nil)
          (gnus-newsrc-hashtb (make-hash-table :test #'equal)))
      (puthash "nnmaildir+gmail:inbox" '(entry (info)) gnus-newsrc-hashtb)
      (mail-tests--with-empty-loader
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'mail-group-loaded-p) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (mail-tests--with-gnus-stubs calls
            (open-mail-inbox))))
      (expect (assq 'read calls) :to-be nil)
      (expect said :to-equal '("The inbox did not load")))))

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

(defun mail-tests--open (command head &optional group search)
  "URL COMMAND opens for article 7, whose HEAD sits in GROUP.
GROUP defaults to a Reddit group.  SEARCH names the search group the
summary shows the article in, if any.  A nil HEAD cannot be read."
  (let ((group (or group "nnmaildir+reddit:r.test"))
        (summary (generate-new-buffer " *mail-tests summary*"))
        (nntp-server-buffer (generate-new-buffer " *mail-tests nntp*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer summary
            (setq-local gnus-newsgroup-name (or search group)))
          (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7)))
                    ((symbol-function 'mail-article-group)
                     (lambda (article)
                       (and (eq (current-buffer) summary) (eql article 7) group)))
                    ((symbol-function 'gnus-request-head)
                     (lambda (article name)
                       (with-current-buffer nntp-server-buffer
                         (erase-buffer)
                         (insert (or head "")))
                       (and head (eql article 7) (equal name (or search group)))))
                    ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
            (with-temp-buffer
              (funcall command)))
          opened)
      (kill-buffer summary)
      (kill-buffer nntp-server-buffer))))

(describe "open-mail-on-web"
  (it "opens where the message on screen lives on the web"
    (expect (mail-tests--open #'open-mail-on-web mail-tests--gmane-head
                              "nnmaildir+news:gmane.emacs.devel")
            :to-equal "https://yhetil.org/emacs-devel/id%40x/t/#u"))
  (it "tells a search result's Gmail mail by the group holding its file"
    (expect (mail-tests--open #'open-mail-on-web "Message-ID: <id@x>\nTo: me@example.com\n"
                              "nnmaildir+gmail:inbox" "nnselect:search-1")
            :to-equal (gmail-message-url "<id@x>")))
  (it "says it knows no page, naming the message's group"
    (expect (mail-tests--open #'open-mail-on-web "Message-ID: <id@x>\n"
                              "nnmaildir+news:gmane.test")
            :to-throw 'user-error
            '("No web page known for this message of nnmaildir+news:gmane.test")))
  (it "says so when the message cannot be read"
    (expect (mail-tests--open #'open-mail-on-web nil "nnmaildir+gmail:inbox")
            :to-throw 'user-error '("Cannot read this message"))))

(describe "open-message-in-list-archive"
  (it "opens where the message's Archived-At header points"
    (expect (mail-tests--open #'open-message-in-list-archive
                              "Message-ID: <c1.p2@reddit.com>\nArchived-At: <https://www.reddit.com/c1/>\n")
            :to-equal "https://www.reddit.com/c1/"))
  (it "opens the list's archive without one"
    (expect (mail-tests--open #'open-message-in-list-archive
                              "Message-ID: <seven@x>\nTo: emacs-orgmode@gnu.org\n")
            :to-equal "https://yhetil.org/orgmode/seven%40x/"))
  (it "says it knows no archive for mail of no list it knows"
    (expect (mail-tests--open #'open-message-in-list-archive
                              "Message-ID: <seven@x>\nTo: someone@example.com\n")
            :to-throw 'user-error '("No public archive known for this message"))))

(defmacro mail-tests--searching (hits &rest body)
  "Run BODY with the search stubbed to open a summary of HITS articles.
The search specs land in `captured', and what Gnus's count question
would have been bounded by in `large'."
  (declare (indent 1))
  `(let (captured large (summary (get-buffer-create "*Summary nnselect:search-tests*"))
                  (mail-load-queue nil) (mail-load-running nil) (mail-load-tables nil)
                  (mail-load-job nil) (mail-load-waiters nil))
     (unwind-protect
         (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                   ((symbol-function 'subscribe-mail-groups) #'ignore)
                   ((symbol-function 'load-unloaded-mail-groups) #'ignore)
                   ((symbol-function 'gnus-group-read-ephemeral-search-group)
                    (lambda (_no-parse specs)
                      (setq captured specs
                            large gnus-large-ephemeral-newsgroup)
                      (with-current-buffer summary
                        (setq-local gnus-newsgroup-articles (number-sequence 1 ,hits)))
                      (and (< 0 ,hits) "nnselect:search-tests"))))
           ,@body)
       (kill-buffer summary))))

(describe "search-mail"
  (it "hands the raw notmuch query to an ephemeral search over every maildir server"
    ;; notmuch indexes the Gmail store and the news store alike
    (mail-tests--searching 3
      (let ((gnus-secondary-select-methods
             '((nnmaildir "gmail" (directory "/mail/gmail/"))
               (nnmaildir "news" (directory "/mail/news/"))
               (nnatom "www.example.org/feed"))))
        (search-mail "from:someone subject:hello"))
      (expect (cdr (assq 'search-group-spec captured))
              :to-equal '(("nnmaildir:gmail") ("nnmaildir:news")))
      (expect (alist-get 'query (cdr (assq 'search-query-spec captured)))
              :to-equal "from:someone subject:hello")
      (expect (alist-get 'raw (cdr (assq 'search-query-spec captured))) :to-be t)))
  (it "shows the newest 500 matches unless told otherwise"
    ;; a list query finds tens of thousands, and Gnus blocks on each
    (mail-tests--searching 3
      (search-mail "List:x")
      (expect (alist-get 'limit (cdr (assq 'search-query-spec captured))) :to-equal 500)))
  (it "takes the number of matches it is given, up to the most it ever shows"
    ;; Gnus draws a summary in one step: 33533 lines took 10.9 s
    (mail-tests--searching 3
      (dolist (asked '((50 . 50) (2000 . 2000) (5000 . 2000) (0 . 2000)))
        (search-mail "List:x" (car asked))
        (expect (alist-get 'limit (cdr (assq 'search-query-spec captured)))
                :to-equal (cdr asked)))))
  (it "never lets Gnus ask how many articles to show"
    (mail-tests--searching 3
      (search-mail "List:x")
      (expect large :to-be nil)))
  (it "asks for the number of matches on a prefix argument"
    (mail-tests--searching 3
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "List:x"))
                ((symbol-function 'read-number) (lambda (&rest _) 800)))
        (let ((current-prefix-arg '(4)))
          (call-interactively #'search-mail)))
      (expect (alist-get 'limit (cdr (assq 'search-query-spec captured))) :to-equal 800)))
  (it "says how many match in all when the cap cut the result"
    (let (said counted)
      (mail-tests--searching 500
        (cl-letf (((symbol-function 'count-mail)
                   (lambda (queries) (setq counted queries) '(33492)))
                  ((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (search-mail "limit:700 List:x" 500)))
      (expect counted :to-equal '("List:x"))
      (expect said :to-equal
              "The newest 500 of 33492 matches; a prefix argument shows up to 2000")))
  (it "sends the query on to narrow down once the most it shows is cut"
    (let (said)
      (mail-tests--searching 2000
        (cl-letf (((symbol-function 'count-mail) (lambda (_) '(33492)))
                  ((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (search-mail "List:x" 0)))
      (expect said :to-equal
              "The newest 2000 of 33492 matches; narrow the query to reach older ones")))
  (it "reads a label mbsync created since the start before it searches"
    (let (asked)
      (mail-tests--searching 3
        (cl-letf (((symbol-function 'load-unloaded-mail-groups) (lambda () (push t asked))))
          (search-mail "List:x")))
      (expect asked :to-equal '(t))))
  (it "waits while groups load, then searches with the same query and count"
    ;; a hit maps to its group only once nnmaildir holds the group
    (mail-tests--searching 3
      (setq mail-load-running (list "nnmaildir+gmail:archive"))
      (search-mail "List:x" 50)
      (expect captured :to-be nil)
      (setq mail-load-running nil)
      (mail-load-run-waiters)
      (expect (alist-get 'query (cdr (assq 'search-query-spec captured))) :to-equal "List:x")
      (expect (alist-get 'limit (cdr (assq 'search-query-spec captured))) :to-equal 50)))
  (it "counts nothing when every match is shown"
    (let (counted)
      (mail-tests--searching 12
        (cl-letf (((symbol-function 'count-mail) (lambda (queries) (setq counted queries) '(12))))
          (search-mail "List:x")
          (search-mail "List:x" 0)))
      (expect counted :to-be nil))))

(describe "search-mail-now"
  (it "returns the group it reads, so the live search can open its thread"
    (mail-tests--searching 3
      (expect (search-mail-now "thread:abc" nil) :to-equal "nnselect:search-tests")))
  (it "returns nil when nothing matched"
    (mail-tests--searching 0
      (expect (search-mail-now "thread:abc" nil) :to-be nil))))

(describe "when-mail-loaded"
  (it "runs at once when no group is loading"
    (let (ran)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'subscribe-mail-groups) #'ignore)
                ((symbol-function 'load-unloaded-mail-groups) #'ignore)
                ((symbol-function 'mail-load-busy-p) #'ignore))
        (when-mail-loaded "for the test" (lambda () (setq ran t))))
      (expect ran :to-be t)))
  (it "starts Gnus first when it is down"
    (let (started)
      (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
                ((symbol-function 'gnus) (lambda (&rest _) (setq started t)))
                ((symbol-function 'subscribe-mail-groups) #'ignore)
                ((symbol-function 'load-unloaded-mail-groups) #'ignore)
                ((symbol-function 'mail-load-busy-p) #'ignore))
        (when-mail-loaded "for the test" #'ignore))
      (expect started :to-be t)))
  (it "waits while groups load, saying what waits, then runs"
    (let ((mail-load-queue nil) (mail-load-running (list "nnmaildir+gmail:archive"))
          (mail-load-tables nil) (mail-load-job nil) (mail-load-waiters nil)
          ran)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'subscribe-mail-groups) #'ignore)
                ((symbol-function 'load-unloaded-mail-groups) #'ignore))
        (when-mail-loaded "for the test" (lambda () (setq ran t)))
        (expect (list ran (nth 1 (car mail-load-waiters))) :to-equal '(nil "for the test"))
        (setq mail-load-running nil)
        (mail-load-run-waiters))
      (expect ran :to-be t))))

(describe "message-copies"
  (it "splits every copy notmuch named into one list per message"
    (expect (message-copies '("a1" "a2" "b1" "c1" "c2" "c3") '("a1" "b1" "c1"))
            :to-equal '(("a1" "a2") ("b1") ("c1" "c2" "c3"))))
  (it "drops the lines before the first message"
    (expect (message-copies '("x" "a1" "a2") '("a1")) :to-equal '(("a1" "a2")))))

(defun mail-tests--fill-store (root files)
  "Create FILES, named relative to the store at ROOT; each maildir gets cur/ and new/."
  (dolist (file files)
    (let* ((path (expand-file-name file root))
           (maildir (file-name-directory (directory-file-name (file-name-directory path)))))
      (dolist (sub '("cur" "new"))
        (make-directory (expand-file-name sub maildir) t))
      (write-region "" nil path nil 'silent))))

(defmacro mail-tests--in-store (files &rest body)
  "Run BODY with `root' naming a temporary Gmail store that holds FILES.
`news-root' names the news store beside it; both are the module's stores."
  (declare (indent 1))
  `(let* ((parent (make-temp-file "mail-store" t))
          (root (file-name-as-directory (expand-file-name "gmail" parent)))
          (news-root (file-name-as-directory (expand-file-name "news" parent)))
          (gmail-maildir root)
          (news-maildir news-root)
          (gnus-secondary-select-methods `((nnmaildir "gmail" (directory ,root))
                                           (nnmaildir "news" (directory ,news-root)))))
     (ignore news-root)
     (unwind-protect
         (progn
           (make-directory root)
           (make-directory news-root)
           (mail-tests--fill-store root ,files)
           ,@body)
       (delete-directory parent t))))

(defun mail-tests--in (root &rest files)
  "FILES, named relative to the store at ROOT, as absolute names."
  (mapcar (lambda (file) (concat root file)) files))

(describe "likeliest-copy"
  (it "takes the inbox copy over any other"
    (mail-tests--in-store '("archive/cur/1" "github/cur/2" "inbox/cur/3")
      (expect (likeliest-copy (mail-tests--in root "archive/cur/1" "github/cur/2" "inbox/cur/3"))
              :to-equal (concat root "inbox/cur/3"))))
  (it "takes a label read at startup over All Mail, and All Mail over a list label"
    (mail-tests--in-store '("archive/cur/1" "github/new/2" "emacs/new/3" "archive/cur/4")
      (expect (likeliest-copy (mail-tests--in root "archive/cur/1" "github/new/2"))
              :to-equal (concat root "github/new/2"))
      (expect (likeliest-copy (mail-tests--in root "emacs/new/3" "archive/cur/4"))
              :to-equal (concat root "archive/cur/4"))))
  (it "takes a list's news copy over All Mail's, and the inbox copy over the news copy"
    ;; the list is read in the news group now; mail sent to the reader
    ;; himself stays the inbox's
    (mail-tests--in-store '("archive/cur/1" "emacs/cur/2" "inbox/cur/3")
      (mail-tests--fill-store news-root '("gmane.test/cur/1" "gmane.test/cur/3"))
      (expect (likeliest-copy (list (concat root "archive/cur/1") (concat root "emacs/cur/2")
                                    (concat news-root "gmane.test/cur/1")))
              :to-equal (concat news-root "gmane.test/cur/1"))
      (expect (likeliest-copy (list (concat news-root "gmane.test/cur/3")
                                    (concat root "inbox/cur/3")))
              :to-equal (concat root "inbox/cur/3"))))
  (it "takes the trash copy last"
    (mail-tests--in-store '("trash/cur/1" "emacs/cur/2")
      (expect (likeliest-copy (mail-tests--in root "trash/cur/1" "emacs/cur/2"))
              :to-equal (concat root "emacs/cur/2"))))
  (it "keeps notmuch's order between copies of one rank"
    (mail-tests--in-store '("github/cur/1" "job/cur/2")
      (expect (likeliest-copy (mail-tests--in root "github/cur/1" "job/cur/2"))
              :to-equal (concat root "github/cur/1"))))
  (it "takes a copy under the name Gnus gave it after notmuch indexed it"
    ;; a scan moves new mail into cur/, and a saved flag renames the file
    (mail-tests--in-store '("archive/new/1:2," "inbox/cur/2:2,S")
      (expect (likeliest-copy (mail-tests--in root "archive/new/1:2," "inbox/new/2:2,"))
              :to-equal (concat root "inbox/cur/2:2,S"))))
  (it "passes over a copy that is gone"
    (mail-tests--in-store '("archive/cur/1:2,S" "inbox/cur/other:2,")
      (expect (likeliest-copy (mail-tests--in root "archive/cur/1:2,S" "inbox/cur/2:2,S"))
              :to-equal (concat root "archive/cur/1:2,S"))))
  (it "answers nil when every copy is gone"
    (mail-tests--in-store '("archive/cur/other:2," "inbox/cur/other:2,")
      (expect (likeliest-copy (mail-tests--in root "archive/cur/1:2," "inbox/cur/2:2,"))
              :to-be nil)))
  (it "reads a cur/ once for every copy one search looks up in it"
    (mail-tests--in-store '("inbox/cur/1:2,S" "inbox/cur/2:2,S")
      (let ((listings (make-hash-table :test #'equal)))
        (spy-on 'maildir-file-names :and-call-through)
        (expect (list (likeliest-copy (mail-tests--in root "inbox/new/1:2,") listings)
                      (likeliest-copy (mail-tests--in root "inbox/new/2:2,") listings))
                :to-equal (mail-tests--in root "inbox/cur/1:2,S" "inbox/cur/2:2,S"))
        (expect (spy-calls-count 'maildir-file-names) :to-be 1)))))

(defun mail-tests--notmuch (dir files)
  "Stand-in notmuch in DIR: log its arguments to DIR/args, print FILES."
  (let ((script (expand-file-name "notmuch" dir)))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"$*\" >> " (shell-quote-argument (expand-file-name "args" dir)) "\n"
              (if files
                  (format "printf '%%s\\n' %s\n" (mapconcat #'shell-quote-argument files " "))
                "exit 1\n")))
    (set-file-modes script #o755)
    script))

(defmacro mail-tests--parsing (store firsts all query &rest body)
  "Run BODY after the likeliest-copy advice read FIRSTS for QUERY.
STORE names the files the store holds, relative to `root'.  FIRSTS is
what the first notmuch run left in the engine's buffer, ALL what the
second run prints.  BODY sees what reached the parser in `parsed' and
notmuch's arguments in `runs'."
  (declare (indent 4))
  `(let* ((dir (make-temp-file "mail-tests" t))
          (root (file-name-as-directory dir))
          (gmail-maildir root)
          (news-maildir (file-name-as-directory (make-temp-file "mail-tests-news" t)))
          (gnus-secondary-select-methods `((nnmaildir "gmail" (directory ,root))
                                           (nnmaildir "news" (directory ,news-maildir))))
          (gnus-search-notmuch-config-file "/config/file")
          (engine (make-instance 'gnus-search-notmuch
                                 :program (mail-tests--notmuch dir ,all)
                                 :remove-prefix root))
          parsed runs)
     (unwind-protect
         (with-current-buffer (slot-value engine 'proc-buffer)
           (mail-tests--fill-store root ,store)
           (insert (mapconcat (lambda (file) (concat file "\n")) ,firsts "")
                   "\nProcess search-nnmaildir:gmail finished\n")
           (search-likeliest-copies-a (lambda (&rest _) (setq parsed (buffer-string)))
                                      engine "nnmaildir:gmail" ,query)
           (setq runs (and (file-exists-p (expand-file-name "args" dir))
                           (with-temp-buffer
                             (insert-file-contents (expand-file-name "args" dir))
                             (split-string (buffer-string) "\n" t))))
           ,@body)
       (kill-buffer (slot-value engine 'proc-buffer))
       (delete-directory dir t)
       (delete-directory news-maildir t))))

(describe "search-likeliest-copies-a"
  (it "hands the parser each message's inbox copy in place of All Mail's"
    (let ((mail-inbox-group "nnmaildir+gmail:inbox")
          (mail-archive-group "nnmaildir+gmail:archive")
          (mail-trash-group "nnmaildir+gmail:trash")
          (mail-bulk-groups '("nnmaildir+gmail:archive")))
      (mail-tests--parsing '("archive/cur/a" "inbox/cur/a2" "archive/cur/b" "github/cur/b2")
          (list (concat root "archive/cur/a") (concat root "archive/cur/b"))
          (list (concat root "archive/cur/a") (concat root "inbox/cur/a2")
                (concat root "archive/cur/b") (concat root "github/cur/b2"))
          '((query . "from:x") (raw . t) (limit . 500))
        (expect parsed :to-equal (concat root "inbox/cur/a2\n" root "github/cur/b2\n"))
        ;; the second run names every copy of the same newest 500
        (expect runs :to-equal '("--config=/config/file search --output=files --limit=500 from:x")))))
  (it "hands the parser a copy Gnus renamed under its new name, and drops a gone message"
    ;; notmuch names the files as they were at its last run
    (let ((mail-inbox-group "nnmaildir+gmail:inbox")
          (mail-archive-group "nnmaildir+gmail:archive")
          (mail-trash-group "nnmaildir+gmail:trash")
          (mail-bulk-groups '("nnmaildir+gmail:archive")))
      (mail-tests--parsing '("archive/new/a:2," "inbox/cur/a2:2,S" "archive/cur/c:2,S")
          (list (concat root "archive/new/a:2,") (concat root "archive/cur/b:2,")
                (concat root "archive/cur/c:2,S"))
          (list (concat root "archive/new/a:2,") (concat root "inbox/new/a2:2,")
                (concat root "archive/cur/b:2,") (concat root "inbox/cur/b2:2,")
                (concat root "archive/cur/c:2,S"))
          '((query . "from:x") (raw . t))
        (expect parsed :to-equal (concat root "inbox/cur/a2:2,S\n" root "archive/cur/c:2,S\n")))))
  (it "keeps the first run's hits when the second run fails"
    (mail-tests--parsing nil (list (concat root "archive/cur/a")) nil
        '((query . "from:x") (raw . t))
      (expect parsed :to-equal (concat root "archive/cur/a\n"))))
  (it "passes a thread search's copies on as they are, and runs notmuch no more"
    (mail-tests--parsing nil (list (concat root "archive/cur/a") (concat root "inbox/cur/a2"))
        (list "/x")
        '((query . "id:a") (thread . t))
      (expect parsed :to-equal (concat root "archive/cur/a\n" root "inbox/cur/a2\n"))
      (expect runs :to-be nil))))

(defun mail-tests--parse-in (engine-root firsts all query)
  "What the likeliest-copy advice hands the parser of an engine over ENGINE-ROOT.
FIRSTS is the first notmuch run's output and ALL the second's, for QUERY."
  (let* ((dir (make-temp-file "mail-tests-notmuch" t))
         (engine (make-instance 'gnus-search-notmuch
                                :program (mail-tests--notmuch dir all)
                                :remove-prefix engine-root))
         parsed)
    (unwind-protect
        (with-current-buffer (slot-value engine 'proc-buffer)
          (insert (mapconcat (lambda (file) (concat file "\n")) firsts ""))
          (search-likeliest-copies-a (lambda (&rest _) (setq parsed (buffer-string)))
                                     engine "nnmaildir:any" query)
          parsed)
      (kill-buffer (slot-value engine 'proc-buffer))
      (delete-directory dir t))))

(describe "search-likeliest-copies-a over two stores"
  (it "shows each message once, from the store of its likeliest copy"
    ;; each server's engine sees what notmuch finds in both stores
    (let ((mail-inbox-group "nnmaildir+gmail:inbox")
          (mail-archive-group "nnmaildir+gmail:archive")
          (mail-trash-group "nnmaildir+gmail:trash")
          (mail-bulk-groups '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs")))
      (mail-tests--in-store '("archive/cur/a" "emacs/cur/a2" "inbox/cur/b" "archive/cur/b2")
        (mail-tests--fill-store news-root '("gmane.test/cur/a3" "gmane.test/cur/b3"
                                            "gmane.test/cur/c"))
        (let ((firsts (list (concat root "archive/cur/a") (concat news-root "gmane.test/cur/b3")
                            (concat news-root "gmane.test/cur/c")))
              (all (list (concat root "archive/cur/a") (concat root "emacs/cur/a2")
                         (concat news-root "gmane.test/cur/a3")
                         (concat news-root "gmane.test/cur/b3") (concat root "inbox/cur/b")
                         (concat root "archive/cur/b2")
                         (concat news-root "gmane.test/cur/c")))
              (query '((query . "List:x") (raw . t))))
          (expect (mail-tests--parse-in root firsts all query)
                  :to-equal (concat root "inbox/cur/b\n"))
          (expect (mail-tests--parse-in news-root firsts all query)
                  :to-equal (concat news-root "gmane.test/cur/a3\n"
                                    news-root "gmane.test/cur/c\n"))))))
  (it "keeps a thread search to the engine's own store"
    ;; the other store's paths would name no group of this server
    (mail-tests--in-store '("inbox/cur/a")
      (mail-tests--fill-store news-root '("gmane.test/cur/a2"))
      (let ((firsts (list (concat root "inbox/cur/a") (concat news-root "gmane.test/cur/a2")))
            (query '((query . "id:a") (thread . t))))
        (expect (mail-tests--parse-in root firsts nil query)
                :to-equal (concat root "inbox/cur/a\n"))
        (expect (mail-tests--parse-in news-root firsts nil query)
                :to-equal (concat news-root "gmane.test/cur/a2\n"))))))

(describe "rebase-mail-flags"
  (it "applies what Gnus changed to the flags the file has now"
    ;; Gnus unreads a message nnmaildir read as S, which the phone starred
    (expect (rebase-mail-flags ":2,S" ":2," ":2,FS") :to-equal ":2,F")
    ;; Gnus reads a message the phone starred
    (expect (rebase-mail-flags ":2," ":2,S" ":2,F") :to-equal ":2,FS"))
  (it "keeps the flags sorted, as maildir wants them"
    (expect (rebase-mail-flags ":2," ":2,T" ":2,FS") :to-equal ":2,FST")))

(describe "keep-flags-set-elsewhere-a"
  (it "renames a file the phone renamed to Gnus's change over the phone's flags"
    (mail-tests--in-store '("inbox/cur/1.x:2,FS")
      (let ((article (make-nnmaildir--art :prefix "1.x" :suffix ":2,S"))
            (cur (concat root "inbox/cur/")))
        (keep-flags-set-elsewhere-a #'nnmaildir--article-set-flags article ":2," cur)
        (expect (directory-files cur nil "\\`[^.]") :to-equal '("1.x:2,F"))
        (expect (nnmaildir--art-suffix article) :to-equal ":2,F"))))
  (it "passes the new flags on when the file still has the ones nnmaildir read"
    (mail-tests--in-store '("inbox/cur/1.x:2,S")
      (let ((article (make-nnmaildir--art :prefix "1.x" :suffix ":2,S"))
            (cur (concat root "inbox/cur/"))
            called)
        (keep-flags-set-elsewhere-a (lambda (&rest args) (setq called args)) article ":2," cur)
        (expect called :to-equal (list article ":2," cur)))))
  (it "passes the new flags on when the file is gone, which nnmaildir reports"
    (mail-tests--in-store '("inbox/cur/other:2,")
      (let ((article (make-nnmaildir--art :prefix "1.x" :suffix ":2,S"))
            (cur (concat root "inbox/cur/"))
            called)
        (keep-flags-set-elsewhere-a (lambda (&rest args) (setq called args)) article ":2," cur)
        (expect called :to-equal (list article ":2," cur))))))

;;; Names mbsync changed

(describe "maildir-file-names"
  (it "files each name under its base name, the flags cut off"
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest _) '("1.x:2,S" "1.x,U=7:2," "2.x"))))
      (let ((names (maildir-file-names "/m/cur/")))
        (expect (mapcar (lambda (base) (gethash base names)) '("1.x" "1.x,U=7" "2.x"))
                :to-equal '("1.x:2,S" "1.x,U=7:2," "2.x"))))))

(describe "maildir-uploads"
  (it "finds a base name mbsync gave a UID by the base before it"
    (let ((names (make-hash-table :test #'equal)))
      (puthash "1.x,U=7" "1.x,U=7:2," names)
      (puthash "2.x" "2.x:2,S" names)
      (let ((uploads (maildir-uploads names)))
        (expect (list (gethash "1.x" uploads) (gethash "2.x" uploads)
                      (hash-table-count uploads))
                :to-equal '("1.x,U=7" nil 1))))))

(describe "current-mail-file"
  (it "finds a file under the name mbsync gave it with a UID once it went up"
    (mail-tests--in-store '("trash/cur/1.x,U=7:2,S")
      (expect (current-mail-file (concat root "trash/cur/1.x:2,S")
                                 (make-hash-table :test #'equal))
              :to-equal (concat root "trash/cur/1.x,U=7:2,S"))))
  (it "finds a file whose UID mbsync changed"
    (mail-tests--in-store '("inbox/cur/1.x,U=9:2,")
      (expect (current-mail-file (concat root "inbox/cur/1.x,U=5:2,")
                                 (make-hash-table :test #'equal))
              :to-equal (concat root "inbox/cur/1.x,U=9:2,"))))
  (it "takes a file of the same base name over one that only lacks its UID"
    (mail-tests--in-store '("inbox/cur/1.x,U=7:2," "inbox/cur/1.x:2,S")
      (expect (current-mail-file (concat root "inbox/cur/1.x:2,")
                                 (make-hash-table :test #'equal))
              :to-equal (concat root "inbox/cur/1.x:2,S")))))

(defun mail-tests--group (remembered)
  "An nnmaildir group remembering REMEMBERED, each (PREFIX . SUFFIX), numbered from 1."
  (let ((group (make-nnmaildir--grp :name "inbox" :flist (make-hash-table :test #'equal)))
        (num 0))
    (pcase-dolist (`(,prefix . ,suffix) remembered)
      (puthash prefix (make-nnmaildir--art :prefix prefix :suffix suffix :num (cl-incf num))
               (nnmaildir--grp-flist group)))
    group))

(defun mail-tests--names (group)
  "GROUP's file table as (KEY PREFIX SUFFIX NUMBER) rows, sorted by key."
  (let (rows)
    (maphash (lambda (key article)
               (push (list key (nnmaildir--art-prefix article) (nnmaildir--art-suffix article)
                           (nnmaildir--art-num article))
                     rows))
             (nnmaildir--grp-flist group))
    (sort rows (lambda (a b) (string< (car a) (car b))))))

(describe "follow-renamed-mail"
  (it "gives an article the flags its file has now"
    (mail-tests--in-store '("inbox/cur/1.x:2,FS" "inbox/cur/2.x:2,")
      (let ((group (mail-tests--group '(("1.x" . ":2,") ("2.x" . ":2,")))))
        (follow-renamed-mail group (concat root "inbox/"))
        (expect (mail-tests--names group)
                :to-equal '(("1.x" "1.x" ":2,FS" 1) ("2.x" "2.x" ":2," 2))))))
  (it "points an article at the name mbsync gave its upload, keeping its number"
    (mail-tests--in-store '("trash/cur/1.x,U=7:2,S")
      (let ((group (mail-tests--group '(("1.x" . ":2,")))))
        (follow-renamed-mail group (concat root "trash/"))
        (expect (mail-tests--names group) :to-equal '(("1.x,U=7" "1.x,U=7" ":2,S" 1))))))
  (it "leaves an upload's old name to nnmaildir when the group knows the new one"
    ;; a scan numbered the uploaded file already, and two articles of one
    ;; file would show the message twice
    (mail-tests--in-store '("trash/cur/1.x,U=7:2,")
      (let ((group (mail-tests--group '(("1.x" . ":2,") ("1.x,U=7" . ":2,")))))
        (follow-renamed-mail group (concat root "trash/"))
        (expect (mail-tests--names group)
                :to-equal '(("1.x" "1.x" ":2," 1) ("1.x,U=7" "1.x,U=7" ":2," 2))))))
  (it "leaves an article whose file is gone for good to nnmaildir"
    (mail-tests--in-store '("inbox/cur/other:2,")
      (let ((group (mail-tests--group '(("1.x" . ":2,S")))))
        (follow-renamed-mail group (concat root "inbox/"))
        (expect (mail-tests--names group) :to-equal '(("1.x" "1.x" ":2,S" 1))))))
  (it "reads cur/ again only once something in it changed"
    (mail-tests--in-store '("inbox/cur/1.x:2,")
      (let ((group (mail-tests--group '(("1.x" . ":2,"))))
            (cur (concat root "inbox/cur/")))
        (spy-on 'maildir-file-names :and-call-through)
        (set-file-times cur (time-subtract nil 60))
        (follow-renamed-mail group (concat root "inbox/"))
        (follow-renamed-mail group (concat root "inbox/"))
        (expect (spy-calls-count 'maildir-file-names) :to-be 1)
        (rename-file (concat cur "1.x:2,") (concat cur "1.x:2,S"))
        (set-file-times cur)
        (follow-renamed-mail group (concat root "inbox/"))
        (expect (spy-calls-count 'maildir-file-names) :to-be 2)
        (expect (mail-tests--names group) :to-equal '(("1.x" "1.x" ":2,S" 1)))))))

(defmacro mail-tests--with-nnmaildir (files &rest body)
  "Run BODY with nnmaildir server gmail open over `store', holding FILES.
FILES are (NAME SUBJECT), NAME under the store's inbox or trash; both
groups are read, and the renamed-file advice is on as config.el puts it."
  (declare (indent 1))
  `(let* ((store (file-name-as-directory (make-temp-file "mail-tests-nnmaildir" t)))
          (nnmaildir--servers nil)
          (nnmaildir--cur-server nil)
          (nntp-server-buffer (generate-new-buffer " *mail-tests-nntp*"))
          (gnus-newsrc-hashtb (gnus-make-hashtable))
          (gnus-verbose 0)
          (advised '((follow-renamed-mail-a nnmaildir-retrieve-headers nnmaildir-request-article
                                            nnmaildir-request-move-article
                                            nnmaildir-request-expire-articles)
                     (follow-renamed-mail-marks-a nnmaildir-request-set-mark
                                                  nnmaildir-request-update-info))))
     (unwind-protect
         (progn
           (dolist (group '("inbox" "trash"))
             (dolist (sub '("cur" "new" "tmp"))
               (make-directory (expand-file-name (concat group "/" sub) store) t)))
           (pcase-dolist (`(,name ,subject) ,files)
             (with-temp-file (expand-file-name name store)
               (insert "From: Ann <ann@example.com>\nSubject: " subject
                       "\nMessage-ID: <" subject "@example.com>\n\nbody\n")))
           (nnmaildir-open-server "gmail" `((directory ,store) (get-new-mail nil)))
           (setf (nnmaildir--srv-method nnmaildir--cur-server)
                 `(nnmaildir "gmail" (directory ,store) (get-new-mail nil)))
           (dolist (group '("inbox" "trash"))
             (nnmaildir-request-scan group "gmail"))
           (pcase-dolist (`(,advice . ,fns) advised)
             (dolist (fn fns)
               (advice-add fn :before advice)))
           ,@body)
       (pcase-dolist (`(,advice . ,fns) advised)
         (dolist (fn fns)
           (advice-remove fn advice)))
       (kill-buffer nntp-server-buffer)
       (delete-directory store t))))

(defun mail-tests--files (store dir)
  "The message files in DIR under STORE."
  (directory-files (expand-file-name dir store) nil "\\`[^.]"))

(defun mail-tests--number (group base)
  "Number nnmaildir gave the file named BASE in GROUP of server gmail."
  (nnmaildir-base-name-to-article-number base group "gmail"))

(describe "follow-renamed-mail-a"
  (it "lets nnmaildir read the header of a message the phone renamed"
    (mail-tests--with-nnmaildir '(("inbox/cur/1.x:2," "one"))
      (let ((number (mail-tests--number "inbox" "1.x")))
        (rename-file (concat store "inbox/cur/1.x:2,") (concat store "inbox/cur/1.x:2,F"))
        (nnmaildir-retrieve-headers (list number) "inbox" "gmail")
        (expect (with-current-buffer nntp-server-buffer (buffer-string)) :to-match "\tone\t"))))
  (it "lets nnmaildir open a copy mbsync renamed with a UID"
    (mail-tests--with-nnmaildir '(("trash/cur/1.x:2,S" "one"))
      (let ((number (mail-tests--number "trash" "1.x")))
        (rename-file (concat store "trash/cur/1.x:2,S") (concat store "trash/cur/1.x,U=7:2,S"))
        (expect (nnmaildir-request-article number "trash" "gmail")
                :to-equal (cons "trash" number)))))
  (it "moves a message the phone renamed, as d x asks"
    (mail-tests--with-nnmaildir '(("inbox/cur/1.x:2," "one"))
      (let ((number (mail-tests--number "inbox" "1.x")))
        (rename-file (concat store "inbox/cur/1.x:2,") (concat store "inbox/cur/1.x:2,F"))
        (expect (nnmaildir-request-move-article
                 number "inbox" "gmail" '(nnmaildir-request-accept-article "trash" "gmail"))
                :to-be-truthy)
        (expect (list (mail-tests--files store "inbox/cur")
                      (length (mail-tests--files store "trash/cur")))
                :to-equal '(nil 1)))))
  (it "deletes the file of a message the phone renamed, as an archive asks"
    (mail-tests--with-nnmaildir '(("inbox/cur/1.x:2," "one"))
      (let ((number (mail-tests--number "inbox" "1.x")))
        (rename-file (concat store "inbox/cur/1.x:2,") (concat store "inbox/cur/1.x:2,S"))
        (nnmaildir-request-expire-articles (list number) "inbox" "gmail" t)
        (expect (mail-tests--files store "inbox/cur") :to-be nil))))
  (it "leaves a group nnmaildir does not hold alone"
    (mail-tests--with-nnmaildir nil
      (expect (follow-renamed-mail-a nil "nosuch" "gmail") :to-be nil))))

(describe "follow-renamed-mail-marks-a"
  (it "keeps the UID mbsync added when Gnus saves a flag"
    ;; nnmaildir would rename the file to the name it remembers
    (mail-tests--with-nnmaildir '(("trash/cur/1.x:2," "one"))
      (let ((number (mail-tests--number "trash" "1.x")))
        (rename-file (concat store "trash/cur/1.x:2,") (concat store "trash/cur/1.x,U=7:2,"))
        (nnmaildir-request-set-mark "trash" `(((,number . ,number) add (read))) "gmail")
        (expect (mail-tests--files store "trash/cur") :to-equal '("1.x,U=7:2,S")))))
  (it "reads the flags of a copy mbsync renamed with a UID"
    (mail-tests--with-nnmaildir '(("trash/cur/1.x:2," "one"))
      (let ((number (mail-tests--number "trash" "1.x"))
            (info (gnus-info-make "trash" 3 nil nil '(nnmaildir "gmail"))))
        (rename-file (concat store "trash/cur/1.x:2,") (concat store "trash/cur/1.x,U=7:2,S"))
        (nnmaildir-request-update-info "trash" info "gmail")
        (expect (range-member-p number (gnus-info-read info)) :to-be-truthy)))))

;;; Syncs launchd runs

(defun mail-tests--look (states times)
  "Run one `check-mail-syncs' over STATES, the state files dated per TIMES.
TIMES maps each routine group to its state file's time, nil for none.
Returns the groups it read again."
  (let ((mail-sync-states states)
        loaded)
    (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
              ((symbol-function 'follow-mail-folders) #'ignore)
              ((symbol-function 'scanned-mail-groups) (lambda () (mapcar #'car times)))
              ((symbol-function 'mail-sync-state-time) (lambda (group) (cdr (assoc group times))))
              ((symbol-function 'load-mail-groups) (lambda (groups) (setq loaded groups))))
      (check-mail-syncs))
    loaded))

(describe "check-mail-syncs"
  (it "only notes the times at its first look"
    (expect (mail-tests--look (make-hash-table :test #'equal)
                              '(("inbox" . (1 0)) ("trash" . nil)))
            :to-be nil))
  (it "reads again only the groups whose state file mbsync rewrote"
    (let ((states (make-hash-table :test #'equal)))
      (mail-tests--look states '(("inbox" . (1 0)) ("trash" . (1 0)) ("job" . (1 0))))
      (expect (mail-tests--look states '(("inbox" . (2 0)) ("trash" . (1 0)) ("job" . (3 0))))
              :to-equal '("inbox" "job"))
      (expect (mail-tests--look states '(("inbox" . (2 0)) ("trash" . (1 0)) ("job" . (3 0))))
              :to-be nil)))
  (it "reads again a group whose state file appeared"
    ;; mbsync's first sync of a new label
    (let ((states (make-hash-table :test #'equal)))
      (mail-tests--look states '(("job" . nil)))
      (expect (mail-tests--look states '(("job" . (1 0)))) :to-equal '("job"))))
  (it "looks at nothing while Gnus is down"
    (let ((mail-sync-states (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'gnus-alive-p) #'ignore))
        (spy-on 'scanned-mail-groups)
        (check-mail-syncs)
        (expect 'scanned-mail-groups :not :to-have-been-called))))
  (it "follows the folders a launchd sync changed before it reads the state files"
    ;; a label Gmail dropped has no state file to read, and a new one needs its group
    (let ((mail-sync-states (make-hash-table :test #'equal))
          calls)
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'follow-mail-folders) (lambda () (push 'follow calls)))
                ((symbol-function 'scanned-mail-groups)
                 (lambda () (push 'scanned calls) '("nnmaildir+gmail:inbox")))
                ((symbol-function 'mail-sync-state-time) (lambda (_) '(1 0))))
        (check-mail-syncs))
      (expect (nreverse calls) :to-equal '(follow scanned)))))

(describe "mail-sync-state-time"
  (it "reads when mbsync last wrote the state file in the group's folder"
    (mail-tests--in-store '("inbox/cur/1.x:2,")
      (let ((gnus-secondary-select-methods `((nnmaildir "gmail" (directory ,root))))
            (state (concat root "inbox/.mbsyncstate")))
        (write-region "" nil state nil 'silent)
        (set-file-times state '(25000 0))
        (expect (time-equal-p (mail-sync-state-time "nnmaildir+gmail:inbox") '(25000 0))
                :to-be t))))
  (it "answers nil for a folder mbsync never synced"
    (mail-tests--in-store '("inbox/cur/1.x:2,")
      (let ((gnus-secondary-select-methods `((nnmaildir "gmail" (directory ,root)))))
        (expect (mail-sync-state-time "nnmaildir+gmail:inbox") :to-be nil)))))

(describe "start-mail-sync-check"
  (it "notes the times at once, then looks again every interval"
    (let ((mail-sync-check-timer nil)
          (mail-sync-check-interval 15)
          (mail-sync-states (make-hash-table :test #'equal))
          looks)
      (cl-letf (((symbol-function 'check-mail-syncs) (lambda () (push 'look looks))))
        (unwind-protect
            (progn
              (start-mail-sync-check)
              (expect looks :to-equal '(look))
              (expect (timer--repeat-delay mail-sync-check-timer) :to-equal 15)
              (expect (timer--function mail-sync-check-timer) :to-be 'check-mail-syncs))
          (stop-mail-sync-check)))))
  (it "keeps one timer however often Gnus starts"
    (let ((mail-sync-check-timer nil)
          (mail-sync-check-interval 15)
          (mail-sync-states (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'check-mail-syncs) #'ignore))
        (unwind-protect
            (progn
              (start-mail-sync-check)
              (start-mail-sync-check)
              (expect (seq-count (lambda (timer) (eq (timer--function timer) 'check-mail-syncs))
                                 timer-list)
                      :to-be 1))
          (stop-mail-sync-check))))))

(describe "stop-mail-sync-check"
  (it "cancels the timer and forgets the times seen"
    (let ((mail-sync-check-timer (run-with-timer 3600 3600 #'ignore))
          (mail-sync-states (make-hash-table :test #'equal)))
      (puthash "nnmaildir+gmail:inbox" '(1 0) mail-sync-states)
      (let ((timer mail-sync-check-timer))
        (stop-mail-sync-check)
        (expect (list (memq timer timer-list) mail-sync-check-timer
                      (hash-table-count mail-sync-states))
                :to-equal '(nil nil 0))))))
