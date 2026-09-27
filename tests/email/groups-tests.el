;;; tests/email/groups-tests.el --- group buffer specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; each spec binds these, so a suite loaded later still sees config.el's
(defvar mail-topics)
(defvar mail-bulk-groups)
(defvar mail-remote-backends)

(load-module-file "modules/email/autoload/groups.el")

;; bound by the group buffer around each line it draws
(defvar gnus-tmp-group)

(defconst groups-tests-topics
  '(("Gmail" ("nnmaildir+gmail:inbox" . "Inbox") ("nnmaildir+gmail:trash"))
    ("Lists" ("nntp+news.gmane.io:gmane.emacs.devel" . "emacs-devel over NNTP")
     ("nnmaildir+gmail:emacs" . "emacs-devel, delivered")))
  "A `mail-topics' of two topics, one group in it without a description.")

(defconst groups-tests-fresh
  '((("Gnus" visible) (("misc" visible)))
    . (("misc" "nndraft:drafts")
       ("Gnus" "nnmaildir+gmail:inbox" "nnmaildir+gmail:trash" "nnmaildir+gmail:html"
        "nntp+news.gmane.io:gmane.emacs.devel" "nnmaildir+gmail:emacs")))
  "(TOPOLOGY . ALIST) as Gnus leaves them once the subscriptions exist.")

(defun groups-tests-known (&rest groups)
  "Predicate for the groups Gnus knows: GROUPS, or every group when none."
  (lambda (group) (or (null groups) (member group groups))))

(defun groups-tests-arrange (topology alist &optional known)
  "`arrange-mail-topics' over TOPOLOGY and ALIST, KNOWN or every group known."
  (arrange-mail-topics topology alist groups-tests-topics
                       (or known (groups-tests-known))))

(defun groups-tests-method (group)
  "The select method GROUP's name says it lives on."
  (cond ((string-prefix-p "nntp+" group) '(nntp "news.gmane.io"))
        ((string-prefix-p "nnatom+" group) '(nnatom "www.reddit.com/r/emacs/new/.rss"))
        (t '(nnmaildir "gmail"))))

(describe "gnus-user-format-function-C"
  (it "draws the description mail-topics gives the group of the line"
    (let ((mail-topics groups-tests-topics)
          (gnus-tmp-group "nnmaildir+gmail:emacs"))
      (expect (gnus-user-format-function-C nil) :to-equal "emacs-devel, delivered")))
  (it "draws nothing for a group without a description or outside mail-topics"
    (let ((mail-topics groups-tests-topics))
      (dolist (group '("nnmaildir+gmail:trash" "nnmaildir+gmail:html"))
        (let ((gnus-tmp-group group))
          (expect (gnus-user-format-function-C nil) :to-equal ""))))))

(describe "mail-topics-regexp"
  (it "matches every group mail-topics names and nothing else"
    (let ((mail-topics groups-tests-topics))
      (dolist (group '("nnmaildir+gmail:inbox" "nnmaildir+gmail:trash"
                       "nntp+news.gmane.io:gmane.emacs.devel" "nnmaildir+gmail:emacs"))
        (expect group :to-match (mail-topics-regexp)))
      ;; a name that only starts or ends like a named group
      (dolist (group '("nnmaildir+gmail:inbox2" "x-nnmaildir+gmail:inbox" "nnmaildir+gmail:html"))
        (expect (string-match-p (mail-topics-regexp) group) :to-be nil)))))

(describe "arrange-mail-topics"
  (it "puts its topics under the root in order, each holding its groups in order"
    (pcase-let ((`(,topology . ,alist)
                 (groups-tests-arrange (car groups-tests-fresh) (cdr groups-tests-fresh))))
      (expect topology :to-equal '(("Gnus" visible) (("Gmail" visible)) (("Lists" visible))
                                   (("misc" visible))))
      (expect (cdr (assoc "Gmail" alist))
              :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:trash"))
      (expect (cdr (assoc "Lists" alist))
              :to-equal '("nntp+news.gmane.io:gmane.emacs.devel" "nnmaildir+gmail:emacs"))
      ;; the groups it does not name stay where Gnus put them
      (expect (cdr (assoc "Gnus" alist)) :to-equal '("nnmaildir+gmail:html"))
      (expect (cdr (assoc "misc" alist)) :to-equal '("nndraft:drafts"))))

  (it "leaves out a group Gnus does not know"
    (let ((alist (cdr (groups-tests-arrange
                       (car groups-tests-fresh) (cdr groups-tests-fresh)
                       (groups-tests-known "nnmaildir+gmail:inbox" "nnmaildir+gmail:emacs")))))
      (expect (cdr (assoc "Gmail" alist)) :to-equal '("nnmaildir+gmail:inbox"))
      (expect (cdr (assoc "Lists" alist)) :to-equal '("nnmaildir+gmail:emacs"))))

  (it "takes back a group moved by hand and keeps one moved in, after its own"
    (let ((alist (cdr (groups-tests-arrange
                       '(("Gnus" visible) (("Gmail" visible)) (("Lists" visible)))
                       '(("Gnus")
                         ("Gmail" "nnmaildir+gmail:html" "nnmaildir+gmail:trash")
                         ("Lists" "nnmaildir+gmail:inbox" "nnmaildir+gmail:emacs"
                          "nntp+news.gmane.io:gmane.emacs.devel"))))))
      (expect (cdr (assoc "Gmail" alist))
              :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:trash" "nnmaildir+gmail:html"))
      (expect (cdr (assoc "Lists" alist))
              :to-equal '("nntp+news.gmane.io:gmane.emacs.devel" "nnmaildir+gmail:emacs"))))

  (it "keeps a topic it does not name, after its own, with its state and other groups"
    (pcase-let ((`(,topology . ,alist)
                 (groups-tests-arrange
                  '(("Gnus" visible) (("Later" visible nil ((visible . t))))
                    (("Gmail" invisible)) (("Lists" visible)))
                  '(("Gnus")
                    ("Later" "nnmaildir+gmail:html" "nnmaildir+gmail:trash")
                    ("Gmail" "nnmaildir+gmail:inbox")
                    ("Lists")))))
      ;; Gmail was folded by hand and stays folded
      (expect topology :to-equal '(("Gnus" visible) (("Gmail" invisible)) (("Lists" visible))
                                   (("Later" visible nil ((visible . t))))))
      (expect (cdr (assoc "Later" alist)) :to-equal '("nnmaildir+gmail:html"))))

  (it "brings a topic indented under another back under the root, subtopics and all"
    (expect (car (groups-tests-arrange
                  '(("Gnus" visible)
                    (("Later" visible) (("Gmail" visible) (("Sub" visible))) (("Lists" visible))))
                  '(("Gnus") ("Later") ("Gmail") ("Sub") ("Lists"))))
            :to-equal '(("Gnus" visible) (("Gmail" visible) (("Sub" visible))) (("Lists" visible))
                        (("Later" visible)))))

  (it "creates a topic Gnus does not have yet"
    (pcase-let ((`(,topology . ,alist)
                 (groups-tests-arrange '(("Gnus" visible)) '(("Gnus" "nnmaildir+gmail:inbox")))))
      (expect topology :to-equal '(("Gnus" visible) (("Gmail" visible)) (("Lists" visible))))
      (expect (cdr (assoc "Gmail" alist)) :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:trash"))
      (expect (cdr (assoc "Gnus" alist)) :to-be nil)))

  (it "changes nothing when applied again, and lists no group twice"
    ;; inbox also copied into misc by hand, which Gnus allows
    (let* ((once (groups-tests-arrange
                  (car groups-tests-fresh)
                  (cons '("misc" "nndraft:drafts" "nnmaildir+gmail:inbox")
                        (cdr (cdr groups-tests-fresh)))))
           (twice (groups-tests-arrange (car once) (cdr once)))
           (groups (apply #'append (mapcar #'cdr (cdr twice)))))
      (expect twice :to-equal once)
      (expect (length groups) :to-equal (length (seq-uniq groups)))
      (expect (cdr (assoc "misc" (cdr twice))) :to-equal '("nndraft:drafts"))))

  (it "leaves Gnus's own structures alone"
    (let ((topology (copy-tree (car groups-tests-fresh)))
          (alist (copy-tree (cdr groups-tests-fresh))))
      (groups-tests-arrange topology alist)
      (expect topology :to-equal (car groups-tests-fresh))
      (expect alist :to-equal (cdr groups-tests-fresh)))))

(describe "apply-mail-topics"
  (it "arranges Gnus's topics, lists the named groups always and redraws the group buffer"
    (let ((mail-topics groups-tests-topics)
          (gnus-topic-topology (copy-tree (car groups-tests-fresh)))
          (gnus-topic-alist (copy-tree (cdr groups-tests-fresh)))
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-permanently-visible-groups nil)
          (gnus-group-buffer " *groups-tests group*")
          (listed nil))
      (dolist (group '("nnmaildir+gmail:inbox" "nnmaildir+gmail:trash" "nnmaildir+gmail:emacs"))
        (puthash group '(0 nil) gnus-newsrc-hashtb))
      (get-buffer-create gnus-group-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-group-list-groups)
                     (lambda (&rest _) (setq listed (current-buffer)))))
            (apply-mail-topics)
            (expect (mapcar #'caar (cdr gnus-topic-topology)) :to-equal '("Gmail" "Lists" "misc"))
            ;; gmane is not in this newsrc
            (expect (cdr (assoc "Lists" gnus-topic-alist)) :to-equal '("nnmaildir+gmail:emacs"))
            (expect "nnmaildir+gmail:trash" :to-match gnus-permanently-visible-groups)
            (expect (string-match-p gnus-permanently-visible-groups "nnmaildir+gmail:html")
                    :to-be nil)
            (expect listed :to-be (get-buffer gnus-group-buffer)))
        (kill-buffer gnus-group-buffer)))))

(describe "put-group-in-topic"
  (it "moves a group out of every topic holding it and into the end of another"
    (let ((gnus-topic-alist (copy-tree '(("Gnus" "a" "b") ("Later" "b" "c") ("Lists" "d")))))
      (put-group-in-topic "b" "Lists")
      (expect gnus-topic-alist :to-equal '(("Gnus" "a") ("Later" "c") ("Lists" "d" "b"))))))

(describe "words-wildmat"
  (it "asks for the names holding the words in order"
    (expect (words-wildmat "reddit emacs") :to-equal "*reddit*emacs*")
    (expect (words-wildmat "  emacs ") :to-equal "*emacs*")))

(describe "words-regexp"
  (it "matches the names holding the words in order, and only those"
    (expect "gwene.com.reddit.emacs" :to-match (words-regexp "reddit emacs"))
    (expect (string-match-p (words-regexp "emacs reddit") "gwene.com.reddit.emacs") :to-be nil)
    (expect (string-match-p (words-regexp "a.b") "axb") :to-be nil)))

(defconst groups-tests-active
  (concat "215 Newsgroups in form \"group high low status\"\r\n"
          "gwene.com.reddit.emacs 0000025996 0000000001 m\r\n"
          "gwene.com.reddit.r.planetemacs 0000006587 0000000003 m\r\n"
          ".\r\n")
  "What news.gmane.io answers to LIST ACTIVE *reddit*emacs*, cut to two groups.")

(defmacro groups-tests-with-server (asked &rest body)
  "Run BODY against a stand-in news server that logs each wildmat into ASKED."
  (declare (indent 1))
  `(let ((nntp-server-buffer (get-buffer-create " *groups-tests nntp*")))
     (unwind-protect
         (cl-letf (((symbol-function 'gnus-check-server) (lambda (&rest _) t))
                   ((symbol-function 'nntp-list-active-group)
                    (lambda (pattern &optional server)
                      (push (list pattern server) ,asked)
                      (with-current-buffer nntp-server-buffer
                        (erase-buffer)
                        (insert groups-tests-active))
                      t)))
           ,@body)
       (kill-buffer nntp-server-buffer))))

(describe "news-groups-matching"
  (it "asks the server for the names holding the words and counts each group's articles"
    (let (asked)
      (groups-tests-with-server asked
        (expect (news-groups-matching "reddit emacs" '(nntp "news.gmane.io"))
                :to-equal '(("nntp+news.gmane.io:gwene.com.reddit.emacs" . 25996)
                            ("nntp+news.gmane.io:gwene.com.reddit.r.planetemacs" . 6585))))
      (expect asked :to-equal '(("*reddit*emacs*" "news.gmane.io")))))
  (it "answers nothing when the server cannot be reached"
    (cl-letf (((symbol-function 'gnus-check-server) #'ignore)
              ((symbol-function 'nntp-list-active-group)
               (lambda (&rest _) (error "Asked a server that is down"))))
      (expect (news-groups-matching "emacs" '(nntp "news.gmane.io")) :to-be nil))))

(defmacro groups-tests-reading (levels picked &rest body)
  "Run BODY with group LEVELS, labels in the store and news groups stood in.
LEVELS is an alist of (GROUP . LEVEL); any other group reads as killed.
The completion prompt answers with the first candidate into PICKED, as
\(PICK COLLECTION ANNOTATION), and the words asked for are \"emacs\"."
  (declare (indent 2))
  `(let ((gnus-secondary-select-methods '((nnmaildir "gmail") (nntp "news.gmane.io"))))
     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "emacs"))
               ((symbol-function 'maildir-groups)
                (lambda () '("nnmaildir+gmail:emacs" "nnmaildir+gmail:inbox"
                             "nnmaildir+gmail:org-mode")))
               ((symbol-function 'news-groups-matching)
                (lambda (words method)
                  (when (equal (list words method) '("emacs" (nntp "news.gmane.io")))
                    '(("nntp+news.gmane.io:gmane.emacs.devel" . 346572)
                      ("nntp+news.gmane.io:gwene.com.reddit.emacs" . 25996)))))
               ((symbol-function 'gnus-group-level)
                (lambda (group) (alist-get group ,levels 9 nil #'equal)))
               ((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _)
                  (let ((pick (caar collection)))
                    (setq ,picked (list pick collection
                                        (plist-get completion-extra-properties
                                                   :annotation-function)))
                    pick))))
       ,@body)))

(describe "read-group-to-add"
  (it "offers the labels and news groups holding the words that are not subscribed"
    (let (picked)
      (groups-tests-reading '(("nnmaildir+gmail:emacs" . 6)
                              ("nnmaildir+gmail:inbox" . 3)
                              ("nntp+news.gmane.io:gmane.emacs.devel" . 4))
          picked
        (expect (read-group-to-add) :to-equal "nnmaildir+gmail:emacs"))
      (pcase-let ((`(,_ ,collection ,annotate) picked))
        ;; inbox is subscribed and its name does not hold the word;
        ;; org-mode's name does not hold it either, gmane is subscribed
        (expect (mapcar #'car collection)
                :to-equal '("nnmaildir+gmail:emacs" "nntp+news.gmane.io:gwene.com.reddit.emacs"))
        (expect (funcall annotate "nnmaildir+gmail:emacs") :to-equal "  label")
        (expect (funcall annotate "nntp+news.gmane.io:gwene.com.reddit.emacs")
                :to-equal "  25996 articles"))))
  (it "says so when nothing unsubscribed holds the words"
    (let (picked)
      (groups-tests-reading '(("nnmaildir+gmail:emacs" . 4)
                              ("nntp+news.gmane.io:gmane.emacs.devel" . 4)
                              ("nntp+news.gmane.io:gwene.com.reddit.emacs" . 3))
          picked
        (expect (read-group-to-add) :to-throw 'user-error))
      (expect picked :to-be nil))))

(defmacro groups-tests-adding (level calls &rest body)
  "Run BODY with Gnus's subscription machinery logging into CALLS.
Every group starts at LEVEL, point sits in topic Lists, and the topic
alist has Gnus, Gmail and Lists."
  (declare (indent 2))
  `(let ((gnus-activate-level 3)
         (mail-bulk-groups '("nnmaildir+gmail:archive" "nntp+news.gmane.io:gmane.emacs.devel"))
         (gnus-topic-alist (copy-tree '(("Gnus") ("Gmail" "nnmaildir+gmail:inbox")
                                        ("Lists" "nnmaildir+gmail:emacs")))))
     (cl-letf (((symbol-function 'gnus-current-topic) (lambda () "Lists"))
               ((symbol-function 'gnus-find-method-for-group) #'groups-tests-method)
               ((symbol-function 'gnus-group-level) (lambda (_) ,level))
               ((symbol-function 'gnus-group-change-level)
                (lambda (group level old &rest _)
                  (push (list 'level group level old) ,calls)
                  ;; Gnus files a new group into the topic at point
                  (nconc (assoc "Gnus" gnus-topic-alist) (list group))))
               ((symbol-function 'refresh-mail-group)
                (lambda (group) (push (list 'read group) ,calls)))
               ((symbol-function 'apply-mail-topics) (lambda () (push 'arranged ,calls)))
               ((symbol-function 'gnus-group-jump-to-group)
                (lambda (group &rest _) (push (list 'goto group) ,calls))))
       ,@body)))

(describe "add-mail-group"
  (it "subscribes a label into the routine scan and the topic at point, then reads it"
    (let (calls)
      (groups-tests-adding 9 calls
        (add-mail-group "nnmaildir+gmail:job")
        (expect (nreverse calls)
                :to-equal '((level "nnmaildir+gmail:job" 3 9)
                            (read "nnmaildir+gmail:job")
                            arranged
                            (goto "nnmaildir+gmail:job")))
        (expect gnus-topic-alist
                :to-equal '(("Gnus") ("Gmail" "nnmaildir+gmail:inbox")
                            ("Lists" "nnmaildir+gmail:emacs" "nnmaildir+gmail:job"))))))
  (it "subscribes a news group above the routine scan, reading nothing"
    ;; a news group in the routine scan costs every start a round trip
    (let (calls)
      (groups-tests-adding 9 calls
        (add-mail-group "nntp+news.gmane.io:gwene.com.reddit.emacs")
        (expect (nreverse calls)
                :to-equal '((level "nntp+news.gmane.io:gwene.com.reddit.emacs" 4 9)
                            arranged
                            (goto "nntp+news.gmane.io:gwene.com.reddit.emacs"))))))
  (it "keeps a bulk label out of the routine scan"
    (let (calls)
      (groups-tests-adding 6 calls
        (add-mail-group "nnmaildir+gmail:archive")
        (expect (car (last calls)) :to-equal '(level "nnmaildir+gmail:archive" 4 6))
        (expect (assq 'read calls) :to-be nil))))
  (it "raises an unsubscribed group from the level it has"
    (let (calls)
      (groups-tests-adding 6 calls
        (add-mail-group "nnmaildir+gmail:emacs")
        (expect (car (last calls)) :to-equal '(level "nnmaildir+gmail:emacs" 3 6))))))

(describe "read-news-server"
  (it "takes the only news server without asking"
    (let ((gnus-secondary-select-methods '((nnmaildir "gmail") (nntp "news.gmane.io"))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (error "Asked with one server"))))
        (expect (read-news-server) :to-equal '(nntp "news.gmane.io")))))
  (it "asks which one when there are several"
    (let ((gnus-secondary-select-methods '((nntp "news.gmane.io") (nntp "news.example.org"))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt servers &rest _) (cadr servers))))
        (expect (read-news-server) :to-equal '(nntp "news.example.org")))))
  (it "says so when there is none"
    (let ((gnus-secondary-select-methods '((nnmaildir "gmail"))))
      (expect (read-news-server) :to-throw 'user-error))))

(describe "browse-news-groups"
  (it "browses only the groups holding the words, and restores the full list after"
    (let ((original (symbol-function 'nntp-request-list))
          asked browsed listing)
      (groups-tests-with-server asked
        (cl-letf (((symbol-function 'gnus-browse-foreign-server)
                   (lambda (method &rest _)
                     (setq browsed method)
                     ;; what the browse reads its list through
                     (when (nntp-request-list "news.gmane.io")
                       (setq listing (with-current-buffer nntp-server-buffer
                                       (buffer-string)))))))
          (browse-news-groups '(nntp "news.gmane.io") "reddit emacs")))
      (expect browsed :to-equal '(nntp "news.gmane.io"))
      (expect asked :to-equal '(("*reddit*emacs*" "news.gmane.io")))
      ;; the list the browse buffer parses, without NNTP's status line and end
      (expect listing :to-equal (concat "gwene.com.reddit.emacs 0000025996 0000000001 m\n"
                                        "gwene.com.reddit.r.planetemacs 0000006587 0000000003 m\n"))
      (expect (symbol-function 'nntp-request-list) :to-be original))))

(describe "read-atom-feed"
  (it "has curl fetch the feed for nnatom, under its own User-Agent"
    ;; Reddit answers url.el with 403 Blocked, whatever its agent
    (let (seen)
      (cl-letf (((symbol-function 'nnatom--read-feed)
                 (lambda (feed group)
                   (setq seen (list feed group mm-url-use-external mm-url-program
                                    mm-url-arguments))
                   'parsed)))
        (expect (read-atom-feed "www.reddit.com/r/emacs/new/.rss" nil) :to-be 'parsed))
      (pcase-let ((`(,feed ,group ,external ,program ,args) seen))
        (expect (list feed group external program)
                :to-equal '("www.reddit.com/r/emacs/new/.rss" nil t "curl"))
        (expect (cadr (member "--user-agent" args)) :to-equal feed-user-agent)
        ;; a 403 page would parse as a feed of nothing
        (expect (member "--fail" args) :to-be-truthy)
        (expect (member "--max-time" args) :to-be-truthy)))))

(describe "defer-news-group-h"
  (it "moves a news group or a feed subscribed just now above the routine scan"
    ;; nnatom fetches the feed whenever Gnus activates the group
    (let ((gnus-activate-level 3) (mail-remote-backends '(nntp nnatom)) calls)
      (cl-letf (((symbol-function 'gnus-find-method-for-group) #'groups-tests-method)
                ((symbol-function 'gnus-group-level) (lambda (_) 3))
                ((symbol-function 'gnus-group-change-level)
                 (lambda (&rest args) (push args calls))))
        (defer-news-group-h "nntp+news.gmane.io:gwene.com.reddit.emacs")
        (defer-news-group-h "nnatom+www.reddit.com/r/emacs/new/.rss:r/emacs")
        (defer-news-group-h "nnmaildir+gmail:job"))
      (expect (nreverse calls)
              :to-equal '(("nntp+news.gmane.io:gwene.com.reddit.emacs" 4 3)
                          ("nnatom+www.reddit.com/r/emacs/new/.rss:r/emacs" 4 3))))))

;;; groups-tests.el ends here
