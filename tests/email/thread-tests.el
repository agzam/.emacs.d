;;; tests/email/thread-tests.el --- one-buffer thread view specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/email/autoload/thread.el")

(defun thread-tests-header (number subject from date id &optional references)
  "Header of the article NUMBER the specs build a thread from.
SUBJECT, FROM, DATE, ID and REFERENCES are its fields."
  (make-full-mail-header number subject from date id references 0 0))

(defvar thread-tests-entries
  (list (cons 10 (thread-tests-header
                  10 "Plan" "Ann <ann@example.com>"
                  "Mon, 21 Sep 2026 10:00:00 +0000" "<1@x>"))
        (cons 11 (thread-tests-header
                  11 "Re: Plan" "Bob <bob@example.com>"
                  "Mon, 21 Sep 2026 11:00:00 +0000" "<2@x>" "<1@x>"))
        (cons 12 (thread-tests-header
                  12 "Re: Plan, and the budget" "Ann <ann@example.com>"
                  "Mon, 21 Sep 2026 12:00:00 +0000" "<3@x>" "<1@x> <2@x>")))
  "Three messages of one thread, oldest first.")

(defvar thread-tests-rendered nil
  "Articles `mail-thread-render' was asked for, newest first.")

(defvar thread-tests-marked nil
  "Articles marked read in the summary, newest first.")

(defmacro thread-tests-with-buffer (entry unreads &rest body)
  "Run BODY in a thread buffer entered on ENTRY with UNREADS unread.
Rendering and the summary are stubbed; what they were asked for lands in
`thread-tests-rendered' and `thread-tests-marked'."
  (declare (indent 2))
  `(let ((buffer (generate-new-buffer " *thread-tests*"))
         (summary (generate-new-buffer " *thread-tests summary*")))
     (setq thread-tests-rendered nil
           thread-tests-marked nil)
     (unwind-protect
         (cl-letf (((symbol-function 'mail-thread-render)
                    (lambda (_group article)
                      (push article thread-tests-rendered)
                      (format "body of %d\nsecond line" article)))
                   ((symbol-function 'gnus-summary-mark-article)
                    (lambda (article &rest _) (push article thread-tests-marked))))
           (with-current-buffer buffer
             (mail-thread-mode)
             (setq mail-thread-group "nnmaildir+gmail:inbox"
                   mail-thread-summary-buffer summary)
             (mail-thread-build thread-tests-entries ,entry ,unreads)
             ,@body))
       (kill-buffer summary)
       (kill-buffer buffer))))

(defun thread-tests-open-articles ()
  "Articles whose body is visible in the thread buffer."
  (mapcar #'mail-thread-message-article
          (seq-filter #'mail-thread-message-open-p mail-thread-messages)))

(defun thread-tests-message (article)
  "The message of ARTICLE in the thread buffer."
  (seq-find (lambda (message) (eql (mail-thread-message-article message) article))
            mail-thread-messages))

(describe "mail-thread-sender"
  (it "prefers the display name"
    (expect (mail-thread-sender (thread-tests-header 1 "s" "Ann Lee <ann@example.com>" "d" "<i>"))
            :to-equal "Ann Lee"))
  (it "falls back to the address"
    (expect (mail-thread-sender (thread-tests-header 1 "s" "ann@example.com" "d" "<i>"))
            :to-equal "ann@example.com"))
  (it "decodes an encoded name"
    (expect (mail-thread-sender
             (thread-tests-header 1 "s" "=?utf-8?B?SsO8cmdlbg==?= <j@example.com>" "d" "<i>"))
            :to-equal "Jürgen")))

(describe "mail-thread-message-line"
  (let ((message (mail-thread-message-create
                  :article 11 :header (cdr (nth 1 thread-tests-entries))
                  :marker (point-min-marker))))
    (it "names the sender"
      (expect (substring-no-properties (mail-thread-message-line message "Plan" t))
              :to-match "Bob"))
    (it "leaves the subject out while it is the thread's"
      (expect (substring-no-properties (mail-thread-message-line message "Plan" t))
              :not :to-match "Plan"))
    (it "shows a subject the message changed"
      (expect (substring-no-properties
               (mail-thread-message-line
                (mail-thread-message-create
                 :article 12 :header (cdr (nth 2 thread-tests-entries))
                 :marker (point-min-marker))
                "Plan" t))
              :to-match "and the budget"))
    (it "points the indicator at the fold state"
      (expect (substring-no-properties (mail-thread-message-line message "Plan" t))
              :to-match "\\`▼")
      (expect (substring-no-properties (mail-thread-message-line message "Plan" nil))
              :to-match "\\`▶"))))

(describe "mail-thread-build"
  (it "keeps the messages in date order"
    (thread-tests-with-buffer 11 nil
      (expect (mapcar #'mail-thread-message-article mail-thread-messages)
              :to-equal '(10 11 12))))
  (it "unfolds the message the summary was on and every unread one"
    (thread-tests-with-buffer 11 '(12)
      (expect (thread-tests-open-articles) :to-equal '(11 12))))
  (it "renders nothing for the messages it folds"
    ;; the store holds articles of tens of megabytes; a folded message
    ;; must cost a line, not a render
    (thread-tests-with-buffer 11 nil
      (expect thread-tests-rendered :to-equal '(11))))
  (it "marks every unfolded message read"
    (thread-tests-with-buffer 11 '(12)
      (expect (sort thread-tests-marked #'<) :to-equal '(11 12))))
  (it "names the thread and its size in the header line"
    (thread-tests-with-buffer 11 nil
      (expect header-line-format :to-equal "Plan   3 messages")))
  (it "gives every message a line of its own"
    (thread-tests-with-buffer 11 nil
      (expect (length (seq-filter (lambda (line) (string-match-p "\\`[▼▶]" line))
                                  (split-string (buffer-string) "\n")))
              :to-equal 3))))

(describe "mail-thread-toggle-message"
  (it "renders a folded message on the first unfold only"
    (thread-tests-with-buffer 11 nil
      (mail-thread-goto-message (thread-tests-message 10))
      (mail-thread-toggle-message)
      (expect (thread-tests-open-articles) :to-equal '(10 11))
      (expect (buffer-string) :to-match "body of 10")
      (mail-thread-toggle-message)
      (mail-thread-toggle-message)
      (expect (thread-tests-open-articles) :to-equal '(10 11))
      (expect thread-tests-rendered :to-equal '(10 11))))
  (it "marks the message read when it unfolds it"
    (thread-tests-with-buffer 11 nil
      (mail-thread-goto-message (thread-tests-message 12))
      (mail-thread-toggle-message)
      (expect thread-tests-marked :to-equal '(12 11))))
  (it "keeps the rendered body while it is folded"
    (thread-tests-with-buffer 11 nil
      (mail-thread-goto-message (thread-tests-message 11))
      (mail-thread-toggle-message)
      (expect (thread-tests-open-articles) :to-equal nil)
      (expect (buffer-string) :to-match "body of 11")))
  (it "leaves point on the message it toggled"
    (thread-tests-with-buffer 11 nil
      (mail-thread-goto-message (thread-tests-message 10))
      (mail-thread-toggle-message)
      (expect (mail-thread-message-article (mail-thread-message-at-point))
              :to-equal 10))))

(describe "mail-thread-next-message"
  (it "moves to the message after the one point is in"
    (thread-tests-with-buffer 10 nil
      (mail-thread-goto-message (thread-tests-message 10))
      (mail-thread-next-message)
      (expect (mail-thread-message-article (mail-thread-message-at-point))
              :to-equal 11)))
  (it "moves from inside a body, not only from a message line"
    (thread-tests-with-buffer 10 nil
      (goto-char (point-max))
      (mail-thread-previous-message)
      (expect (mail-thread-message-article (mail-thread-message-at-point))
              :to-equal 11)))
  (it "refuses to walk past the last message"
    (thread-tests-with-buffer 12 nil
      (expect (mail-thread-next-message) :to-throw 'user-error)))
  (it "refuses to walk before the first"
    (thread-tests-with-buffer 10 nil
      (mail-thread-goto-message (thread-tests-message 10))
      (expect (mail-thread-previous-message) :to-throw 'user-error))))

(describe "mail-thread-entries"
  (it "returns the whole thread oldest first, dropping what Gnus made up"
    ;; gnus-fetch-old-headers invents a header for a parent the store
    ;; does not hold; requesting it raises
    (let* ((ghost (thread-tests-header 9 "Plan" "Ghost <ghost@example.com>"
                                       "Mon, 21 Sep 2026 09:00:00 +0000" "<0@x>"))
           (gnus-newsgroup-sparse '(9))
           (gnus-newsgroup-data
            (mapcar (pcase-lambda (`(,article . ,header))
                      (gnus-data-make article gnus-unread-mark 1 header 0))
                    (cons (cons 9 ghost) thread-tests-entries))))
      (cl-letf (((symbol-function 'gnus-summary-top-thread) (lambda () 9))
                ((symbol-function 'gnus-summary-articles-in-thread)
                 (lambda (&rest _) '(12 9 10 11))))
        (expect (mapcar #'car (mail-thread-entries)) :to-equal '(10 11 12))))))

(describe "open-mail-thread"
  (it "says so when point is on no article"
    (cl-letf (((symbol-function 'gnus-summary-article-number) (lambda (&rest _) nil)))
      (expect (open-mail-thread) :to-throw 'user-error))))
