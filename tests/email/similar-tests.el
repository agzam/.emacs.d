;;; tests/email/similar-tests.el --- find mail like this specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar gmail-maildir "/nonexistent-similar-tests/gmail/")
(defvar mail-from-address "agzam.ibragimov@gmail.com")
(defvar mail-inbox-address "to.plotnick@gmail.com")
(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-archive-group "nnmaildir+gmail:archive")
(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar gnus-message-archive-group "nnmaildir+gmail:sent")

(load-module-file "modules/email/autoload/similar.el")

(defun similar-tests-kinds (message)
  "The kinds of the queries `similar-mail-queries' builds for MESSAGE, in order."
  (mapcar #'car (similar-mail-queries message)))

(defun similar-tests-notmuch (dir output)
  "Stand-in notmuch in DIR: log its arguments and stdin, print OUTPUT.
With OUTPUT nil, count --batch answers 2, 3, ... for its queries."
  (let ((script (expand-file-name "notmuch" dir)))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"$*\" >> " (shell-quote-argument (expand-file-name "args" dir)) "\n"
              (if output
                  (format "printf '%%s\\n' %s\n"
                          (mapconcat #'shell-quote-argument output " "))
                (concat "n=1\n"
                        "while IFS= read -r query; do\n"
                        "  printf '%s\\n' \"$query\" >> "
                        (shell-quote-argument (expand-file-name "queries" dir)) "\n"
                        "  n=$((n+1)); echo $n\n"
                        "done\n"))))
    (set-file-modes script #o755)
    script))

(defun similar-tests-lines (file)
  "Lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(describe "similar-mail-queries"
  (it "leads with the correspondent both ways for a message from a person"
    (expect (similar-mail-queries
             '(:from "Bob Smith <bob@acme.com>" :subject "Re: lunch plans"))
            :to-equal
            '((both-ways . "from:\"bob@acme.com\" or to:\"bob@acme.com\"")
              (from . "from:\"bob@acme.com\"")
              (domain . "from:\"acme.com\"")
              (subject . "subject:\"lunch plans\"")
              (attachments . "from:\"bob@acme.com\" and tag:attachment"))))
  (it "leads with the list for a list post, and offers no domain"
    ;; a list host's domain spans every list it hosts
    (expect (similar-mail-queries
             '(:from "Eli <eliz@gnu.org>" :subject "Re: a patch"
               :list-id "\"Emacs development discussions.\" <emacs-devel.gnu.org>"
               :bulk t))
            :to-equal
            '((list . "List:\"emacs-devel.gnu.org\"")
              (from . "from:\"eliz@gnu.org\"")
              (both-ways . "from:\"eliz@gnu.org\" or to:\"eliz@gnu.org\"")
              (subject . "subject:\"a patch\"")
              (attachments . "from:\"eliz@gnu.org\" and tag:attachment"))))
  (it "leads with the sender's domain for bulk mail"
    (expect (car (similar-mail-queries
                  '(:from "Shop <deals@e.shop.example.com>" :subject "Sale" :bulk t)))
            :to-equal '(domain . "from:\"example.com\""))
    (expect (car (similar-mail-queries
                  '(:from "Bank <no.reply.alerts@bank.com>" :subject "Payment")))
            :to-equal '(domain . "from:\"bank.com\"")))
  (it "takes the domain a company's subdomains share"
    (expect (alist-get 'domain (similar-mail-queries
                                '(:from "Card <card@card-e.em.discover.com>")))
            :to-equal "from:\"discover.com\""))
  (it "offers no domain shared by strangers"
    (expect (similar-tests-kinds '(:from "Pal <pal@gmail.com>" :subject "hi"))
            :to-equal '(both-ways from subject attachments)))
  (it "turns to the first other recipient of a message the account sent"
    (expect (car (similar-mail-queries
                  '(:from "Ag <agzam.ibragimov@gmail.com>"
                    :to "to.plotnick@gmail.com, Dan <dan@example.org>, eve@example.org"
                    :subject "Re: plan")))
            :to-equal '(both-ways . "from:\"dan@example.org\" or to:\"dan@example.org\"")))
  (it "finds nobody to correspond with in a note to the account itself"
    (expect (similar-tests-kinds '(:from "agzam.ibragimov@gmail.com"
                                   :to "To.Plotnick@gmail.com" :subject "note"))
            :to-equal '(subject)))
  (it "adds a query per label folder after the rest"
    (expect (last (similar-mail-queries
                   '(:from "a@b.org" :folders ("gmail/github" "gmail/job")))
                  2)
            :to-equal '((label . "folder:\"gmail/github\"") (label . "folder:\"gmail/job\"")))))

(describe "simplified-subject"
  (it "strips every reply and forward prefix"
    (expect (simplified-subject "Re: Fwd: RE[2]: AW: the plan") :to-equal "the plan"))
  (it "drops the quotes, which would end a notmuch phrase"
    (expect (simplified-subject "Re: \"ethically\" running code")
            :to-equal "ethically running code"))
  (it "leaves a subject that starts with a word ending in re alone"
    (expect (simplified-subject "Score: 10") :to-equal "Score: 10")))

(describe "list-id-value"
  (it "answers the identifier in angle brackets"
    (expect (list-id-value "\"Org\" <emacs-orgmode.gnu.org>") :to-equal "emacs-orgmode.gnu.org"))
  (it "answers a bare identifier whole"
    (expect (list-id-value " owner/repo ") :to-equal "owner/repo")))

(describe "notmuch-term"
  (it "quotes the value, doubling any quote inside it"
    (expect (notmuch-term "subject" "say \"hi\"") :to-equal "subject:\"say \"\"hi\"\"\""))
  (it "keeps the query on one line, which a counted batch needs"
    (expect (notmuch-term "List" "owner/repo\n <repo.owner.github.com>")
            :to-equal "List:\"owner/repo <repo.owner.github.com>\"")))

(describe "useful-mail-queries"
  (it "drops the queries that find no mail but this message"
    (expect (useful-mail-queries '((from . "a") (subject . "b") (list . "c")) '(5 1 0))
            :to-equal '(("a" from 5))))
  (it "keeps the first of the queries that find as many messages, in order"
    ;; nested queries with one count find the same messages
    (expect (useful-mail-queries '((domain . "d") (from . "f") (both-ways . "b")) '(52 45 45))
            :to-equal '(("d" domain 52) ("f" from 45)))))

(describe "count-mail"
  (it "counts every query in one notmuch process, on its config"
    (let* ((dir (make-temp-file "similar-tests" t))
           (gnus-search-notmuch-program (similar-tests-notmuch dir nil))
           (gnus-search-notmuch-config-file "/config/file"))
      (unwind-protect
          (progn
            (expect (count-mail '("from:\"a@b.org\"" "List:\"x.y\"")) :to-equal '(2 3))
            (expect (similar-tests-lines (expand-file-name "args" dir))
                    :to-equal '("--config=/config/file count --batch"))
            (expect (similar-tests-lines (expand-file-name "queries" dir))
                    :to-equal '("from:\"a@b.org\"" "List:\"x.y\"")))
        (delete-directory dir t)))))

(describe "mail-folders"
  (it "answers the label folders holding the message, the system ones left out"
    (let* ((dir (make-temp-file "similar-tests" t))
           (gnus-search-notmuch-program
            (similar-tests-notmuch
             dir (mapcar (lambda (file) (concat gmail-maildir file))
                         '("inbox/cur/1:2,S" "github/cur/2:2,S" "archive/cur/3:2,S"
                           "sent/cur/4:2,S" "trash/cur/5" "github/new/6" "job/cur/7"))))
           (gnus-search-notmuch-config-file "/config/file"))
      (unwind-protect
          (progn
            (expect (mail-folders "<id@x>") :to-equal '("gmail/github" "gmail/job"))
            (expect (similar-tests-lines (expand-file-name "args" dir))
                    :to-equal '("--config=/config/file search --output=files id:\"id@x\"")))
        (delete-directory dir t)))))

(defun similar-tests-summary (header)
  "A stand-in summary holding HEADER's article, the way Gnus keeps its data."
  (let ((summary (generate-new-buffer " *similar-tests summary*")))
    (with-current-buffer summary
      (setq-local gnus-newsgroup-name "nnmaildir+gmail:devs")
      (setq-local gnus-newsgroup-data
                  (list (gnus-data-make (mail-header-number header) gnus-read-mark 1
                                        header 0))))
    summary))

(describe "similar-mail-message"
  (it "reads the list headers from the article's head and the rest from the summary"
    (let* ((header (make-full-mail-header
                    7 "=?UTF-8?Q?Re:_caf=C3=A9?=" "Ann <ann@x.org>"
                    "Mon, 21 Sep 2026 10:00:00 +0000" "<seven@x>" "" 0 0 nil
                    '((To . "overview@x.org"))))
           (summary (similar-tests-summary header))
           (nntp-server-buffer (generate-new-buffer " *similar-tests nntp*")))
      (unwind-protect
          (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7)))
                    ((symbol-function 'gnus-request-head)
                     (lambda (article group)
                       (with-current-buffer nntp-server-buffer
                         (erase-buffer)
                         (insert "From: Ann <ann@x.org>\n"
                                 "To: head@x.org\n"
                                 "List-Id: Devs\n <devs.x.org>\n"
                                 "List-Unsubscribe: <mailto:leave@x.org>\n"))
                       (and (eql article 7) (equal group "nnmaildir+gmail:devs"))))
                    ((symbol-function 'mail-folders)
                     (lambda (id) (and (equal id "<seven@x>") '("gmail/devs")))))
            (let ((message (similar-mail-message)))
              (expect (plist-get message :from) :to-equal "Ann <ann@x.org>")
              (expect (plist-get message :to) :to-equal "head@x.org")
              (expect (plist-get message :subject) :to-equal "Re: café")
              (expect (list-id-value (plist-get message :list-id)) :to-equal "devs.x.org")
              (expect (plist-get message :bulk) :to-be t)
              (expect (plist-get message :folders) :to-equal '("gmail/devs"))))
        (kill-buffer summary)
        (kill-buffer nntp-server-buffer))))
  (it "falls back to the overview's To when the head is out of reach"
    (let* ((header (make-full-mail-header 7 "hi" "Ann <ann@x.org>" "" "<seven@x>" "" 0 0 nil
                                          '((To . "overview@x.org"))))
           (summary (similar-tests-summary header)))
      (unwind-protect
          (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7)))
                    ((symbol-function 'gnus-request-head) (lambda (&rest _) nil))
                    ((symbol-function 'mail-folders) #'ignore))
            (let ((message (similar-mail-message)))
              (expect (plist-get message :to) :to-equal "overview@x.org")
              (expect (plist-get message :list-id) :to-be nil)
              (expect (plist-get message :bulk) :to-be nil)))
        (kill-buffer summary)))))

(describe "read-similar-mail-query"
  (it "offers the queries in their order, the first as the default, each with its kind and count"
    (let (table default)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest args)
                   (setq table collection
                         default (nth 4 args))
                   default)))
        (expect (read-similar-mail-query '(("List:\"x\"" list 7) ("from:\"a@b\"" from 294)))
                :to-equal "List:\"x\""))
      (let ((metadata (cdr (funcall table "" nil 'metadata))))
        (expect (funcall (alist-get 'display-sort-function metadata) '("b" "a"))
                :to-equal '("b" "a"))
        (expect (funcall (alist-get 'annotation-function metadata) "from:\"a@b\"")
                :to-match "from +294\\'"))
      (expect (all-completions "" table) :to-equal '("List:\"x\"" "from:\"a@b\"")))))

(describe "search-mail-like-this"
  :var (searched)
  (before-each
    (setq searched nil)
    (spy-on 'similar-mail-message :and-return-value '(:from "Bob <bob@acme.com>"))
    (spy-on 'search-mail :and-call-fake (lambda (query) (setq searched query))))

  (it "searches with the query picked from the counted candidates"
    (spy-on 'count-mail :and-call-fake (lambda (queries) (make-list (length queries) 3)))
    (spy-on 'read-similar-mail-query :and-call-fake #'caar)
    (search-mail-like-this)
    (expect (spy-calls-args-for 'read-similar-mail-query 0)
            :to-equal '((("from:\"bob@acme.com\" or to:\"bob@acme.com\"" both-ways 3))))
    (expect searched :to-equal "from:\"bob@acme.com\" or to:\"bob@acme.com\""))
  (it "hands the pick to the minibuffer first when asked to edit"
    (spy-on 'count-mail :and-call-fake (lambda (queries) (number-sequence 2 (1+ (length queries)))))
    (spy-on 'read-similar-mail-query :and-call-fake #'caar)
    (spy-on 'read-string :and-call-fake (lambda (_prompt initial) (concat initial " and tag:unread")))
    (search-mail-like-this t)
    (expect searched :to-equal "from:\"bob@acme.com\" or to:\"bob@acme.com\" and tag:unread"))
  (it "says so when no query finds other mail"
    (spy-on 'count-mail :and-call-fake (lambda (queries) (make-list (length queries) 1)))
    (spy-on 'read-similar-mail-query)
    (expect (search-mail-like-this) :to-throw 'user-error)
    (expect 'read-similar-mail-query :not :to-have-been-called)
    (expect searched :to-be nil)))

;;; similar-tests.el ends here
