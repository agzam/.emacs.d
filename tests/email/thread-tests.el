;;; tests/email/thread-tests.el --- one-buffer thread view specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar mail-archive-group "nnmaildir+gmail:archive")
(defvar mail-label-colors nil)

(load-module-file "modules/email/autoload/thread.el")
(load-module-file "modules/email/autoload/quotes.el")
(load-module-file "modules/email/autoload/html.el")
;; the header line names the thread's labels; labels.el's (require
;; 'consult) must no-op in the batch tier
(provide 'consult)
(load-module-file "scripts/mail-load.el")
(load-module-file "modules/email/autoload/load.el")
(load-module-file "modules/email/autoload/labels.el")

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

(defun thread-tests-entry (number hour id &optional references)
  "Entry of the article NUMBER sent at HOUR, its Message-ID ID.
REFERENCES names the messages it answers."
  (cons number (thread-tests-header
                number "Re: Plan" "Ann <ann@example.com>"
                (format "Mon, 21 Sep 2026 %02d:00:00 +0000" hour) id references)))

(defvar thread-tests-branches
  (list (thread-tests-entry 20 10 "<r@x>")
        (thread-tests-entry 21 11 "<a@x>" "<r@x>")
        (thread-tests-entry 24 16 "<a1@x>" "<r@x> <a@x>")
        (thread-tests-entry 23 13 "<c@x>" "<r@x>")
        (thread-tests-entry 22 12 "<b@x>" "<r@x>"))
  "A root, its three answers and an answer to the first, in the summary's order.
The summary puts the subthread holding the newest message first.")

(defun thread-tests-arranged (entries)
  "ENTRIES in the order `mail-thread-arrange' gives, each as (ARTICLE . DEPTH)."
  (mapcar (pcase-lambda (`(,depth ,article . ,_)) (cons article depth))
          (mail-thread-arrange entries)))

(defvar thread-tests-gmane-article
  (concat "From: Ann <ann@example.com>\n"
          "Subject: Plan\n"
          "Date: Mon, 21 Sep 2026 10:00:00 +0000\n"
          "Message-ID: <1@x>\n"
          "Xref: news.gmane.io gmane.emacs.devel:346572\n"
          "\n"
          "The plan, as posted.\n")
  "Raw article shaped like gmane's: every gmane article carries an Xref header.")

(defun thread-tests-quoted-article (body)
  "Raw reply whose body is BODY."
  (concat "From: Bob <bob@example.com>\n"
          "Subject: Re: Plan\n"
          "Date: Mon, 21 Sep 2026 11:00:00 +0000\n"
          "Message-ID: <2@x>\n"
          "\n"
          body))

(defun thread-tests-html-article (html)
  "Raw reply whose body is the single text/html part HTML."
  (concat "From: Bob <bob@example.com>\n"
          "Subject: Re: Plan\n"
          "Date: Mon, 21 Sep 2026 11:00:00 +0000\n"
          "Message-ID: <2@x>\n"
          "MIME-Version: 1.0\n"
          "Content-Type: text/html; charset=utf-8\n"
          "\n"
          html))

(defvar thread-tests-quotes t
  "Treatment condition the specs register `highlight-mail-quotes' under.")

(defvar thread-tests-rendered nil
  "Articles `mail-thread-render' was asked for, newest first.")

(defvar thread-tests-marked nil
  "Articles marked read in the summary, newest first.")

(defun thread-tests-mark-on-display ()
  "Stand-in for Gnus's display marking: log the article it marks.
An article marked while point is on another line is logged as
\(off-line ARTICLE): Gnus's own function reads the mark at point."
  (push (if (eql gnus-current-article (gnus-summary-article-number))
            gnus-current-article
          (list 'off-line gnus-current-article))
        thread-tests-marked))

(defun thread-tests-summary-lines (articles)
  "Give the current buffer a summary line and Gnus data for each of ARTICLES."
  (setq-local gnus-newsgroup-data nil)
  (dolist (article articles)
    (push (gnus-data-make article gnus-unread-mark (1+ (point)) nil 0)
          gnus-newsgroup-data)
    (insert (propertize (format "%d\n" article) 'gnus-number article)))
  (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
  (goto-char (point-min)))

(defmacro thread-tests-with-buffer (entry unreads &rest body)
  "Run BODY in a thread buffer entered on ENTRY with UNREADS unread.
Rendering is stubbed and the summary holds a line per message; what the
render and Gnus's display hook were asked for lands in
`thread-tests-rendered' and `thread-tests-marked'.  BODY sees the stub
summary buffer as `summary'."
  (declare (indent 2))
  `(let ((buffer (generate-new-buffer " *thread-tests*"))
         (summary (generate-new-buffer " *thread-tests summary*"))
         (gnus-mark-article-hook (list #'thread-tests-mark-on-display)))
     (setq thread-tests-rendered nil
           thread-tests-marked nil)
     (unwind-protect
         (cl-letf (((symbol-function 'mail-thread-render)
                    (lambda (_group article)
                      (push article thread-tests-rendered)
                      (format "body of %d\nsecond line" article))))
           (with-current-buffer summary
             (thread-tests-summary-lines (mapcar #'car thread-tests-entries)))
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

(defun thread-tests-waiting ()
  "Articles the fill has yet to render, in fill order."
  (mapcar #'mail-thread-message-article mail-thread-waiting))

(defun thread-tests-fill ()
  "Start a fill in the thread buffer and run it the way its idle timer does."
  (mail-thread-start-fill)
  (mail-thread-fill (current-buffer) mail-thread-fill-timer))

(defun thread-tests-timer-active-p (timer)
  "Non-nil while TIMER is still scheduled."
  (and (memq timer timer-idle-list) t))

(defun thread-tests-message (article)
  "The message of ARTICLE in the thread buffer."
  (seq-find (lambda (message) (eql (mail-thread-message-article message) article))
            mail-thread-messages))

(defun thread-tests-thread-text (raw)
  "Thread buffer text, properties kept, once its one message RAW renders.
Nothing in the render is stubbed but the fetch; `highlight-mail-quotes'
is the only treatment."
  (let ((buffer (generate-new-buffer " *thread-tests*"))
        (summary (generate-new-buffer " *thread-tests summary*")))
    (unwind-protect
        (let ((gnus-newsrc-hashtb (gnus-make-hashtable))
              (gnus-treatment-function-alist
               '((thread-tests-quotes highlight-mail-quotes))))
          (cl-letf (((symbol-function 'gnus-request-article)
                     (lambda (_article _group target)
                       (with-current-buffer target (insert raw))
                       t)))
            (with-current-buffer buffer
              (mail-thread-mode)
              (setq mail-thread-group "nnmaildir+gmail:inbox"
                    mail-thread-summary-buffer summary)
              (mail-thread-build (list (nth 1 thread-tests-entries)) 11 nil)
              (buffer-string))))
      (kill-buffer summary)
      (kill-buffer buffer))))

(defun thread-tests-wrap-prefixes (depth &rest lines)
  "Each of LINES's `wrap-prefix', drawn DEPTH replies deep and fontified."
  (with-temp-buffer
    (mail-thread-mode)
    (let ((inhibit-read-only t))
      (insert (propertize (mapconcat (lambda (line) (concat line "\n")) lines "")
                          'line-prefix (mail-thread-guides depth))))
    (jit-lock-fontify-now)
    (goto-char (point-min))
    (let (prefixes)
      (while (not (eobp))
        (push (get-text-property (point) 'wrap-prefix) prefixes)
        (forward-line))
      (nreverse prefixes))))

(defun thread-tests-guides-from (start end)
  "Distinct `line-prefix' values of the lines from START to END, as plain text."
  (save-excursion
    (goto-char start)
    (let (guides)
      (while (< (point) end)
        (cl-pushnew (when-let* ((prefix (get-text-property (point) 'line-prefix)))
                      (substring-no-properties prefix))
                    guides :test #'equal)
        (forward-line))
      (nreverse guides))))

(defun thread-tests-line (message)
  "Text of MESSAGE's line in the thread buffer."
  (save-excursion
    (goto-char (mail-thread-message-marker message))
    (buffer-substring-no-properties (point) (pos-eol))))

(defun thread-tests-line-labels (message)
  "What the label slot on MESSAGE's line shows, or nil before it is drawn."
  (save-excursion
    (goto-char (mail-thread-message-marker message))
    (when-let* ((slot (text-property-not-all (point) (line-end-position) 'mail-labels nil)))
      (when-let* ((shown (get-text-property slot 'display)))
        (substring-no-properties shown)))))

(defun thread-tests-line-face (text line)
  "Face on the last character of LINE in TEXT."
  (let ((start (string-match (concat "^" (regexp-quote line) "$") text)))
    (get-text-property (+ start (length line) -1) 'face text)))

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
              :to-match "\\`▶"))
    (it "leaves a slot for the message's labels after the date, holding its Message-ID"
      (let* ((line (mail-thread-message-line message "Plan" t))
             (slot (text-property-not-all 0 (length line) 'mail-labels nil line)))
        (expect (get-text-property slot 'mail-labels line) :to-equal "<2@x>")
        (expect (substring line (1- slot) slot) :to-match "[0-9]")
        (expect (text-property-not-all (1+ slot) (length line) 'mail-labels nil line)
                :to-be nil)))))

(describe "mail-thread-arrange"
  (it "puts each answer right under the message it answers, siblings oldest first"
    (expect (thread-tests-arranged thread-tests-branches)
            :to-equal '((20 . 0) (21 . 1) (24 . 2) (22 . 1) (23 . 1))))
  (it "takes the last of the References for the message answered"
    (expect (thread-tests-arranged thread-tests-entries)
            :to-equal '((10 . 0) (11 . 1) (12 . 2))))
  (it "counts no level for a message the thread lacks"
    ;; Gnus's placeholder for the missing parent is not among the entries
    (expect (thread-tests-arranged
             (list (thread-tests-entry 31 12 "<c@x>" "<r@x> <gone@x>")
                   (thread-tests-entry 30 10 "<r@x>")))
            :to-equal '((30 . 0) (31 . 1))))
  (it "puts the answers to a message the thread lacks side by side at the top, oldest first"
    ;; Gnus gathers them by their References under one false root
    (expect (thread-tests-arranged
             (list (thread-tests-entry 40 11 "<a@x>" "<gone@x>")
                   (thread-tests-entry 42 13 "<b1@x>" "<gone@x> <b@x>")
                   (thread-tests-entry 41 12 "<b@x>" "<gone@x>")))
            :to-equal '((40 . 0) (41 . 0) (42 . 1))))
  (it "keeps every message of a References loop"
    (expect (thread-tests-arranged
             (list (thread-tests-entry 51 11 "<y@x>" "<x@x>")
                   (thread-tests-entry 50 10 "<x@x>" "<y@x>")
                   (thread-tests-entry 52 12 "<z@x>" "<z@x>")))
            :to-equal '((50 . 0) (51 . 1) (52 . 0)))))

(describe "mail-thread-build"
  (it "lays the messages out in reply order, each knowing its depth"
    (let ((thread-tests-entries thread-tests-branches))
      (thread-tests-with-buffer 21 nil
        (expect (mapcar (lambda (message)
                          (cons (mail-thread-message-article message)
                                (mail-thread-message-depth message)))
                        mail-thread-messages)
                :to-equal '((20 . 0) (21 . 1) (24 . 2) (22 . 1) (23 . 1)))
        (expect (mapcar (lambda (message)
                          (marker-position (mail-thread-message-marker message)))
                        mail-thread-messages)
                :to-equal (sort (mapcar (lambda (message)
                                          (marker-position (mail-thread-message-marker message)))
                                        mail-thread-messages))))))
  (it "names the thread after its root, wherever the summary put it"
    (let ((thread-tests-entries
           (list (thread-tests-entry 61 11 "<a@x>" "<r@x>")
                 (cons 60 (thread-tests-header 60 "Root subject" "Ann <ann@example.com>"
                                               "Mon, 21 Sep 2026 10:00:00 +0000" "<r@x>")))))
      (thread-tests-with-buffer 60 nil
        (expect header-line-format :to-equal "Root subject   2 messages")
        (expect (thread-tests-line (thread-tests-message 60)) :not :to-match "Root subject")
        (expect (thread-tests-line (thread-tests-message 61)) :to-match "Re: Plan"))))
  (it "renders only the message the summary was on"
    ;; every unread gmane message is an NNTP round trip; the buffer must
    ;; not wait for them
    (thread-tests-with-buffer 11 '(12)
      (expect (thread-tests-open-articles) :to-equal '(11))
      (expect thread-tests-rendered :to-equal '(11))))
  (it "queues every other unread message for the fill"
    (thread-tests-with-buffer 11 '(11 12)
      (expect (thread-tests-waiting) :to-equal '(12))))
  (it "renders nothing for the messages it folds"
    ;; the store holds articles of tens of megabytes; a folded message
    ;; must cost a line, not a render
    (thread-tests-with-buffer 11 nil
      (expect thread-tests-rendered :to-equal '(11))
      (expect (thread-tests-waiting) :to-equal nil)))
  (it "marks only the message it rendered read"
    (thread-tests-with-buffer 11 '(12)
      (expect thread-tests-marked :to-equal '(11))))
  (it "names the thread and its size in the header line"
    (thread-tests-with-buffer 11 nil
      (expect header-line-format :to-equal "Plan   3 messages")))
  (it "puts every label of the thread's messages after the subject, as Gmail does"
    (let ((tables (list (cons "nnmaildir+gmail:inbox" (make-hash-table :test #'equal))
                        (cons "nnmaildir+gmail:job" (make-hash-table :test #'equal))))
          (mail-label-colors nil))
      (puthash "<3@x>" t (cdr (nth 0 tables)))
      (puthash "<1@x>" t (cdr (nth 1 tables)))
      (cl-letf (((symbol-function 'mail-label-tables) (lambda () tables)))
        (thread-tests-with-buffer 11 nil
          (expect (substring-no-properties header-line-format)
                  :to-equal "Plan   Inbox   job    3 messages")
          (expect (get-text-property (string-search "job" header-line-format) 'face
                                     header-line-format)
                  :to-be 'mail-label)
          ;; a news thread's messages that Gmail holds too carry its labels
          (setq mail-thread-group "nnmaildir+news:gmane.emacs.devel")
          (mail-thread-update-header-line)
          (expect (substring-no-properties header-line-format)
                  :to-equal "Plan   Inbox   job    3 messages")))))
  (it "shows each message's own labels on its line, and none where it has none"
    (let ((tables (list (cons "nnmaildir+gmail:inbox" (make-hash-table :test #'equal))
                        (cons "nnmaildir+gmail:job" (make-hash-table :test #'equal))))
          (mail-label-colors nil))
      (puthash "<3@x>" t (cdr (nth 0 tables)))
      (puthash "<1@x>" t (cdr (nth 1 tables)))
      (puthash "<3@x>" t (cdr (nth 1 tables)))
      (cl-letf (((symbol-function 'mail-label-tables) (lambda () tables)))
        (thread-tests-with-buffer 11 nil
          (expect (mapcar (lambda (article)
                            (thread-tests-line-labels (thread-tests-message article)))
                          '(10 11 12))
                  :to-equal '("   job " "" "   Inbox   job "))
          (expect (buffer-modified-p) :to-be nil)))))
  (it "gives every message a line of its own"
    (thread-tests-with-buffer 11 nil
      (expect (length (seq-filter (lambda (line) (string-match-p "\\`[▼▶]" line))
                                  (split-string (buffer-string) "\n")))
              :to-equal 3))))

(describe "mail-thread-guides"
  (it "draws a bar per level in the window divider's dim face, and nothing for a root"
    (expect (mail-thread-guides 0) :to-be nil)
    (expect (mail-thread-guides 1) :to-equal "│ ")
    (expect (mail-thread-guides 3) :to-equal "│ │ │ ")
    (expect (get-text-property 0 'face (mail-thread-guides 3)) :to-be 'vertical-border))
  (it "stops at the cap, a deeper message indented as far and no further"
    (let ((mail-thread-max-indent 6))
      (expect (mail-thread-guides 6) :to-equal "│ │ │ │ │ │ ")
      (expect (mail-thread-guides 9) :to-equal "│ │ │ │ │ │ ")
      (expect (mail-thread-guides 58) :to-equal "│ │ │ │ │ │ "))))

(describe "mail-thread-wrap-prefix"
  (it "puts the bars alone where visual-wrap left no prefix"
    (expect (mail-thread-wrap-prefix "│ " nil) :to-equal "│ ")
    (expect (mail-thread-wrap-prefix "│ " "") :to-equal "│ "))
  (it "puts the bars before a prefix visual-wrap repeats"
    (expect (mail-thread-wrap-prefix "│ " "    ") :to-equal "│     "))
  (it "turns visual-wrap's stretch from the window edge into one after the bars"
    (let ((prefix (mail-thread-wrap-prefix "│ " '(space :align-to (+ (22) (0 . width))))))
      (expect (substring-no-properties prefix) :to-equal "│  ")
      (expect (get-text-property 2 'display prefix)
              :to-equal '(space :width (+ (22) (0 . width)))))))

(describe "the guide bars in the thread view"
  (it "start each message's line at its depth"
    (let ((thread-tests-entries thread-tests-branches))
      (thread-tests-with-buffer 21 nil
        (expect (mapcar (lambda (message)
                          (thread-tests-guides-from (mail-thread-message-marker message)
                                                    (mail-thread-body-start message)))
                        mail-thread-messages)
                :to-equal '((nil) ("│ ") ("│ │ ") ("│ ") ("│ "))))))
  (it "start every line of a body, the blank line after it too"
    (let ((thread-tests-entries thread-tests-branches))
      (thread-tests-with-buffer 21 nil
        (let ((message (thread-tests-message 21)))
          (expect (count-lines (mail-thread-body-start message) (mail-thread-body-end message))
                  :to-equal 3)
          (expect (thread-tests-guides-from (mail-thread-body-start message)
                                            (mail-thread-body-end message))
                  :to-equal '("│ "))))))
  (it "start the lines of a body the fill renders later"
    (let ((thread-tests-entries thread-tests-branches))
      (thread-tests-with-buffer 21 '(24)
        (thread-tests-fill)
        (let ((message (thread-tests-message 24)))
          (expect (mail-thread-message-open-p message) :to-be-truthy)
          (expect (thread-tests-guides-from (mail-thread-body-start message)
                                            (mail-thread-body-end message))
                  :to-equal '("│ │ "))))))
  (it "go on along a wrapped line"
    (expect (thread-tests-wrap-prefixes 2 "a plain line") :to-equal '("│ │ ")))
  (it "go before the indentation visual-wrap repeats"
    (expect (thread-tests-wrap-prefixes 2 "    an indented line") :to-equal '("│ │     ")))
  (it "go before whatever visual-wrap makes of a quote marker"
    ;; Emacs 30 repeats it as spaces, later ones as a stretch
    (let ((root (car (thread-tests-wrap-prefixes 0 "> a quoted line")))
          (reply (car (thread-tests-wrap-prefixes 2 "> a quoted line"))))
      (expect root :to-be-truthy)
      (expect (equal-including-properties
               reply (mail-thread-wrap-prefix (mail-thread-guides 2) root))
              :to-be t)))
  (it "stay single when a line is fontified again"
    (with-temp-buffer
      (mail-thread-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "a plain line\n" 'line-prefix (mail-thread-guides 2))))
      (jit-lock-fontify-now)
      (jit-lock-refontify)
      (jit-lock-fontify-now)
      (expect (get-text-property (point-min) 'wrap-prefix) :to-equal "│ │ "))))

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

(describe "mail-thread-fill"
  (it "takes the messages after the entry before the ones above it"
    (thread-tests-with-buffer 11 '(10 12)
      (expect (thread-tests-waiting) :to-equal '(12 10))))
  (it "takes the messages above the entry nearest first"
    (thread-tests-with-buffer 12 '(10 11)
      (expect (thread-tests-waiting) :to-equal '(11 10))))
  (it "renders every waiting message, marks each read, and stops"
    (thread-tests-with-buffer 11 '(10 12)
      (mail-thread-goto-message (thread-tests-message 11))
      (thread-tests-fill)
      (expect (thread-tests-open-articles) :to-equal '(10 11 12))
      (expect (reverse thread-tests-rendered) :to-equal '(11 12 10))
      (expect (reverse thread-tests-marked) :to-equal '(11 12 10))
      (expect mail-thread-fill-timer :to-be nil)))
  (it "renders the waiting message at point first"
    (thread-tests-with-buffer 10 '(11 12)
      (mail-thread-goto-message (thread-tests-message 12))
      (thread-tests-fill)
      (expect (reverse thread-tests-rendered) :to-equal '(10 12 11))))
  (it "yields to pending input and resumes on the next idle period"
    (thread-tests-with-buffer 11 '(12)
      (mail-thread-start-fill)
      (let ((timer mail-thread-fill-timer))
        (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) t)))
          (mail-thread-fill (current-buffer) timer))
        (expect (thread-tests-open-articles) :to-equal '(11))
        (expect (thread-tests-timer-active-p timer) :to-be t)
        (mail-thread-fill (current-buffer) timer)
        (expect (thread-tests-open-articles) :to-equal '(11 12))
        (expect (thread-tests-timer-active-p timer) :to-be nil))))
  (it "leaves alone a message the reader unfolded and folded again"
    (thread-tests-with-buffer 10 '(11 12)
      (mail-thread-goto-message (thread-tests-message 12))
      (mail-thread-toggle-message)
      (mail-thread-toggle-message)
      (thread-tests-fill)
      (expect (thread-tests-open-articles) :to-equal '(10 11))
      (expect (seq-count (lambda (article) (eql article 12)) thread-tests-rendered)
              :to-equal 1)))
  (it "keeps point on the message after a body that lands above it"
    ;; with point on 12's line, 11's body goes in right at point
    (thread-tests-with-buffer 12 '(10 11)
      (let ((marker (mail-thread-message-marker (thread-tests-message 12))))
        (goto-char marker)
        (thread-tests-fill)
        (expect (thread-tests-open-articles) :to-equal '(10 11 12))
        (expect (point) :to-equal (marker-position marker)))))
  (it "keeps a window whose top line is the next message where it was"
    (thread-tests-with-buffer 12 '(10 11)
      (save-window-excursion
        (let ((marker (mail-thread-message-marker (thread-tests-message 12))))
          (set-window-buffer (selected-window) (current-buffer))
          (set-window-start (selected-window) marker)
          (thread-tests-fill)
          (expect (window-start) :to-equal (marker-position marker)))))))

(describe "mail-thread-quit"
  (it "gives the thread's window back to the summary it came from"
    (thread-tests-with-buffer 11 nil
      (let (layouts)
        (cl-letf (((symbol-function 'gnus-configure-windows)
                   (lambda (setting &optional force)
                     (push (list setting force gnus-summary-buffer) layouts))))
          (mail-thread-quit))
        (expect layouts :to-equal `((summary force ,summary)))))))

(describe "a cancelled fill"
  (it "stops when the reader quits, leaving the rest unread"
    (thread-tests-with-buffer 11 '(12)
      (mail-thread-start-fill)
      ;; quitting selects the summary's window, and its buffer with it
      (let ((thread (current-buffer))
            (timer mail-thread-fill-timer))
        (mail-thread-quit)
        (expect (thread-tests-timer-active-p timer) :to-be nil)
        (mail-thread-fill thread timer)
        (with-current-buffer thread
          (expect (thread-tests-open-articles) :to-equal '(11)))
        (expect thread-tests-marked :to-equal '(11)))))
  (it "stops when another thread takes the buffer"
    (thread-tests-with-buffer 11 '(12)
      (mail-thread-start-fill)
      (let ((timer mail-thread-fill-timer)
            (inhibit-read-only t))
        (erase-buffer)
        (mail-thread-mode)
        (expect (thread-tests-timer-active-p timer) :to-be nil)
        (mail-thread-fill (current-buffer) timer)
        (expect (buffer-size) :to-equal 0)
        (expect thread-tests-rendered :to-equal '(11)))))
  (it "stops when the thread buffer is killed"
    (thread-tests-with-buffer 11 '(12)
      (mail-thread-start-fill)
      (let ((timer mail-thread-fill-timer))
        (kill-buffer buffer)
        (expect (thread-tests-timer-active-p timer) :to-be nil))))
  (it "stops once the summary is gone"
    (thread-tests-with-buffer 11 '(12)
      (mail-thread-start-fill)
      (let ((timer mail-thread-fill-timer))
        (kill-buffer summary)
        (mail-thread-fill (current-buffer) timer)
        (expect (thread-tests-open-articles) :to-equal '(11))
        (expect (thread-tests-timer-active-p timer) :to-be nil))))
  (it "stops on a quit during a fetch and passes the quit on"
    (thread-tests-with-buffer 11 '(12)
      (mail-thread-start-fill)
      (let ((timer mail-thread-fill-timer))
        (cl-letf (((symbol-function 'mail-thread-render)
                   (lambda (&rest _) (signal 'quit nil))))
          (expect (condition-case nil
                      (progn (mail-thread-fill (current-buffer) timer) 'finished)
                    (quit 'quit))
                  :to-be 'quit))
        (expect (thread-tests-timer-active-p timer) :to-be nil)
        (expect (thread-tests-open-articles) :to-equal '(11))))))

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
  (it "returns the whole thread in summary order, dropping what Gnus made up"
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
        (expect (mapcar #'car (mail-thread-entries)) :to-equal '(12 10 11))))))

(describe "mail-thread-render"
  (it "reads the raw article it renders, not the one Gnus showed last"
    ;; article-decode-group-name looks every Xref header up again in the
    ;; original-article buffer, which holds whatever the article buffer
    ;; showed last - here a mail article too short for the match
    (let ((shown (generate-new-buffer " *thread-tests original*")))
      (unwind-protect
          (let ((gnus-original-article-buffer shown)
                (gnus-newsrc-hashtb (gnus-make-hashtable)))
            (with-current-buffer shown
              (insert "From: bob@example.com\n\nshort\n"))
            (cl-letf (((symbol-function 'gnus-request-article)
                       (lambda (_article _group buffer)
                         (with-current-buffer buffer
                           (insert thread-tests-gmane-article))
                         t)))
              (expect (mail-thread-render "nntp+news.gmane.io:gmane.emacs.devel" 346572)
                      :to-equal "The plan, as posted.")))
        (kill-buffer shown)))))

(describe "mail-render-message"
  (it "renders a raw message from a buffer, as the article buffer would"
    ;; the live search's preview has a file, not an article of a group
    (let ((raw (generate-new-buffer " *thread-tests raw*"))
          (gnus-newsrc-hashtb (gnus-make-hashtable))
          (mm-text-html-renderer #'render-mail-html))
      (unwind-protect
          (progn
            (with-current-buffer raw
              (insert (thread-tests-html-article "<p>Agreed.</p><blockquote><p>The plan.</p></blockquote>")))
            (let ((text (mail-render-message raw "nnmaildir+gmail:inbox")))
              (expect text :to-match "\\`Agreed\\.")
              (expect text :to-match "^> The plan\\.")))
        (kill-buffer raw)))))

(describe "quoted lines in the thread view"
  ;; the thread view copies each rendered body as text, which drops
  ;; overlays; the faces only survive as text properties
  (it "keep the depth faces the render gave them"
    (let ((text (thread-tests-thread-text
                 (thread-tests-quoted-article
                  "Agreed.\n> The plan, as posted.\n> > And its first draft.\n"))))
      (expect (thread-tests-line-face text "Agreed.") :to-be nil)
      (expect (thread-tests-line-face text "> The plan, as posted.")
              :to-be 'message-cited-text-1)
      (expect (thread-tests-line-face text "> > And its first draft.")
              :to-be 'message-cited-text-2)))
  (it "keep them in a message bigger than gnus-cite would parse"
    (let* ((filler (concat "> " (make-string 70 ?x) "\n"))
           (raw (thread-tests-quoted-article
                 (concat (apply #'concat (make-list 400 filler)) "> > last\n"))))
      (expect (< 25000 (length raw)) :to-be t)
      (expect (thread-tests-line-face (thread-tests-thread-text raw) "> > last")
              :to-be 'message-cited-text-2))))

(describe "HTML messages in the thread view"
  (it "keep the > markers and depth faces the renderer drew"
    (let* ((mm-text-html-renderer #'render-mail-html)
           (text (thread-tests-thread-text
                  (thread-tests-html-article
                   (concat "<p>Agreed.</p><blockquote><p>The plan, as posted.</p>"
                           "<blockquote><p>And its first draft.</p></blockquote>"
                           "</blockquote>")))))
      (expect (thread-tests-line-face text "Agreed.") :to-be nil)
      (expect (thread-tests-line-face text "> The plan, as posted.")
              :to-be 'message-cited-text-1)
      (expect (thread-tests-line-face text "> > And its first draft.")
              :to-be 'message-cited-text-2))))

(describe "mail-thread-mode"
  (it "wraps long lines at the window edge, keeping their indentation"
    (with-temp-buffer
      (mail-thread-mode)
      (expect visual-line-mode :to-be-truthy)
      (expect visual-wrap-prefix-mode :to-be-truthy)
      (expect truncate-lines :to-be nil))))

(describe "open-mail-thread"
  (it "says so when point is on no article"
    (cl-letf (((symbol-function 'gnus-summary-article-number) (lambda (&rest _) nil)))
      (expect (open-mail-thread) :to-throw 'user-error)))
  (it "shows the thread with the entry rendered and starts filling the rest"
    (let ((summary (generate-new-buffer " *thread-tests summary*"))
          (gnus-newsgroup-unreads '(12))
          (gnus-newsgroup-name "nnmaildir+gmail:inbox")
          layouts)
      (setq thread-tests-rendered nil
            thread-tests-marked nil)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'gnus-summary-article-number) (lambda (&rest _) 11))
                      ((symbol-function 'mail-thread-entries) (lambda () thread-tests-entries))
                      ((symbol-function 'mail-thread-render)
                       (lambda (_group article)
                         (push article thread-tests-rendered)
                         (format "body of %d" article)))
                      ;; the layout itself is the config's, specced there
                      ((symbol-function 'gnus-configure-windows)
                       (lambda (setting &rest _)
                         (push (list setting gnus-summary-buffer) layouts)
                         (switch-to-buffer mail-thread-buffer-name))))
              (with-current-buffer summary
                (open-mail-thread))
              (expect layouts :to-equal `((mail-thread ,summary)))
              (expect (memq (get-buffer mail-thread-buffer-name) (gnus-buffers)) :to-be-truthy)
              (expect (buffer-name (window-buffer)) :to-equal mail-thread-buffer-name)
              (with-current-buffer mail-thread-buffer-name
                (expect (thread-tests-open-articles) :to-equal '(11))
                (expect (thread-tests-waiting) :to-equal '(12))
                (expect (thread-tests-timer-active-p mail-thread-fill-timer) :to-be t))))
        (when-let* ((thread (get-buffer mail-thread-buffer-name)))
          (kill-buffer thread))
        (kill-buffer summary)))))

(defvar thread-tests-calls nil
  "Where `thread-tests-probe' ran: its buffer, the article at point, the window.")

(defun thread-tests-probe ()
  "Log where this command runs into `thread-tests-calls'."
  (interactive)
  (push (list (current-buffer) (gnus-summary-article-number) (selected-window))
        thread-tests-calls))

(defvar thread-tests-marks-seen nil
  "The process marks `thread-tests-marks-probe' saw, newest first.")

(defun thread-tests-marks-probe ()
  "Log the process marks this command sees."
  (interactive)
  (push gnus-newsgroup-processable thread-tests-marks-seen))

(describe "mail-on-screen"
  (it "answers the summary and the message at point in the thread view"
    (thread-tests-with-buffer 11 nil
      (goto-char (mail-thread-message-marker (thread-tests-message 12)))
      (forward-line 1)
      (expect (mail-on-screen) :to-equal (cons summary 12))))
  (it "answers the summary and the article on display in the article buffer"
    (let ((summary (generate-new-buffer " *thread-tests summary*"))
          (gnus-article-current '("nnmaildir+gmail:inbox" . 12)))
      (unwind-protect
          (with-temp-buffer
            (gnus-article-mode)
            (setq gnus-article-current-summary summary)
            (expect (mail-on-screen) :to-equal (cons summary 12)))
        (kill-buffer summary))))
  (it "answers the summary itself and the article at point there"
    (with-temp-buffer
      (thread-tests-summary-lines '(10 11 12))
      (gnus-summary-goto-subject 11)
      (expect (mail-on-screen) :to-equal (cons (current-buffer) 11)))))

(describe "run-in-mail-summary"
  (before-each (setq thread-tests-calls nil))
  (it "runs the command in the summary on the message at point, and stays in the view"
    (thread-tests-with-buffer 11 nil
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) (current-buffer))
        (let ((view (selected-window))
              (summary-window (split-window nil nil 'left)))
          (set-window-buffer summary-window summary)
          (goto-char (mail-thread-message-marker (thread-tests-message 12)))
          (run-in-mail-summary #'thread-tests-probe t)
          (expect thread-tests-calls :to-equal (list (list summary 12 summary-window)))
          (expect (selected-window) :to-be view)))))
  (it "runs the command in a summary no window shows"
    (thread-tests-with-buffer 11 nil
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) (current-buffer))
        (goto-char (mail-thread-message-marker (thread-tests-message 10)))
        (run-in-mail-summary #'thread-tests-probe t)
        (expect (butlast (car thread-tests-calls)) :to-equal (list summary 10))
        (expect (window-buffer (selected-window)) :to-be (current-buffer)))))
  (it "selects the summary's window first unless told to stay, so a reply ends up selected"
    (thread-tests-with-buffer 11 nil
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) (current-buffer))
        (let ((summary-window (split-window nil nil 'left)))
          (set-window-buffer summary-window summary)
          (goto-char (mail-thread-message-marker (thread-tests-message 12)))
          (run-in-mail-summary #'thread-tests-probe)
          (expect thread-tests-calls :to-equal (list (list summary 12 summary-window)))
          (expect (selected-window) :to-be summary-window)))))
  (it "calls the command right there in the summary"
    (with-temp-buffer
      (thread-tests-summary-lines '(10 11))
      (gnus-summary-goto-subject 11)
      (run-in-mail-summary #'thread-tests-probe t)
      (expect (butlast (car thread-tests-calls)) :to-equal (list (current-buffer) 11))))
  (it "keeps the summary's process marks out of a command run from the view"
    (thread-tests-with-buffer 11 nil
      (setq thread-tests-marks-seen nil)
      (with-current-buffer summary
        (setq-local gnus-newsgroup-processable (list 10)))
      (goto-char (mail-thread-message-marker (thread-tests-message 12)))
      (save-window-excursion
        (run-in-mail-summary #'thread-tests-marks-probe t)
        (run-in-mail-summary #'thread-tests-marks-probe))
      (expect thread-tests-marks-seen :to-equal '(nil nil))
      (expect (buffer-local-value 'gnus-newsgroup-processable summary) :to-equal '(10))))
  (it "leaves the marks in force for a command run in the summary itself"
    (with-temp-buffer
      (thread-tests-summary-lines '(10 11))
      (setq-local gnus-newsgroup-processable (list 10))
      (setq thread-tests-marks-seen nil)
      (run-in-mail-summary #'thread-tests-marks-probe t)
      (expect thread-tests-marks-seen :to-equal '((10)))))
  (it "refuses when the summary is gone"
    (thread-tests-with-buffer 11 nil
      (kill-buffer summary)
      (expect (run-in-mail-summary #'thread-tests-probe t) :to-throw 'user-error)
      (expect thread-tests-calls :to-be nil)))
  (it "refuses a message the summary no longer shows, instead of acting on another"
    (thread-tests-with-buffer 11 nil
      (with-current-buffer summary
        (setq gnus-newsgroup-data
              (seq-remove (lambda (data) (eql (gnus-data-number data) 12))
                          gnus-newsgroup-data)))
      (goto-char (mail-thread-message-marker (thread-tests-message 12)))
      (expect (run-in-mail-summary #'thread-tests-probe t) :to-throw 'user-error)
      (expect thread-tests-calls :to-be nil))))

(describe "the reply commands"
  (it "run Gnus's quoting commands in the summary, which they select"
    (let (calls)
      (cl-letf (((symbol-function 'run-in-mail-summary)
                 (lambda (command &optional stay) (push (list command stay) calls))))
        (reply-to-sender)
        (reply-to-everyone)
        (reply-to-list)
        (follow-up-on-newsgroup)
        (forward-mail))
      (expect (nreverse calls)
              :to-equal '((gnus-summary-reply-with-original nil)
                          (gnus-summary-wide-reply-with-original nil)
                          (gnus-summary-reply-to-list-with-original nil)
                          (gnus-summary-followup-with-original nil)
                          (gnus-summary-mail-forward nil)))))
  (it "start a new message with the group buffer's own command there"
    (let (calls)
      (cl-letf (((symbol-function 'run-in-mail-summary)
                 (lambda (command &optional stay) (push (list command stay) calls)))
                ((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'gnus-group-mail)
                 (lambda (&rest _) (interactive) (push 'group-mail calls))))
        (with-temp-buffer
          (setq major-mode 'gnus-group-mode)
          (let ((gnus-group-buffer (buffer-name)))
            (compose-new-mail)))
        (with-temp-buffer
          (setq major-mode 'gnus-summary-mode)
          (compose-new-mail)))
      (expect (nreverse calls)
              :to-equal '(group-mail (gnus-summary-mail-other-window nil)))))
  (it "start a new message from the group buffer anywhere else, Gnus started first"
    ;; a collect buffer of the live search, for one, runs without Gnus
    (let ((group (generate-new-buffer " *thread-tests group*"))
          calls)
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-alive-p) (lambda () (memq 'started calls)))
                    ((symbol-function 'gnus) (lambda (&rest _) (push 'started calls)))
                    ((symbol-function 'gnus-group-mail)
                     (lambda (&rest _) (interactive) (push (current-buffer) calls))))
            (with-temp-buffer
              (let ((gnus-group-buffer (buffer-name group)))
                (compose-new-mail)
                (compose-new-mail))))
        (kill-buffer group))
      (expect (nreverse calls) :to-equal (list 'started group group)))))

(defun thread-tests-notmuch (output &optional delay status)
  "Stand-in notmuch printing OUTPUT after DELAY seconds, then exiting STATUS."
  (let ((script (make-temp-file "thread-tests-notmuch")))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              (if delay (format "sleep %s\n" delay) "")
              "printf '" output "'\n"
              (format "exit %d\n" (or status 0))))
    (set-file-modes script #o755)
    script))

(defun thread-tests-shown ()
  "Articles the filter leaves on show."
  (mapcar #'mail-thread-message-article (mail-thread-shown-messages)))

(defun thread-tests-wait-for (process)
  "Wait until PROCESS has exited and its sentinel has run."
  (with-timeout (5 (error "The stand-in notmuch never answered"))
    (while (buffer-live-p (process-buffer process))
      (accept-process-output nil 0.05))))

(describe "mail-thread-filter"
  (it "asks notmuch which of the thread's messages match, each Message-ID quoted"
    ;; an unquoted id holding + or = matches other messages
    (thread-tests-with-buffer 10 nil
      (let ((gnus-search-notmuch-config-file "/notmuch/config"))
        (expect (mail-thread-filter-args "from:bob")
                :to-equal '("--config=/notmuch/config" "search" "--output=messages"
                            "(id:\"1@x\" or id:\"2@x\" or id:\"3@x\")" "and" "(from:bob)")))))
  (it "hides the messages notmuch does not match, and counts the rest in the header line"
    (thread-tests-with-buffer 10 nil
      (mail-thread-hide-unmatched "from:bob" '("2@x"))
      (expect (thread-tests-shown) :to-equal '(11))
      (expect header-line-format :to-equal "Plan   1 of 3 shown: from:bob")
      ;; the entry's line and its rendered body both go
      (let ((entry (thread-tests-message 10)))
        (expect (invisible-p (mail-thread-message-marker entry)) :to-be-truthy)
        (expect (invisible-p (1- (mail-thread-body-end entry))) :to-be-truthy))
      (expect (invisible-p (mail-thread-message-marker (thread-tests-message 11)))
              :not :to-be-truthy)))
  (it "keeps every message when notmuch matches none"
    (thread-tests-with-buffer 10 nil
      (spy-on 'message)
      (mail-thread-hide-unmatched "from:zed" nil)
      (expect (thread-tests-shown) :to-equal '(10 11 12))
      (expect header-line-format :to-equal "Plan   3 messages")
      (expect 'message :to-have-been-called-with
              "Nothing in this thread matches %s" "from:zed")))
  (it "shows every message again on an empty query, folded as before"
    (thread-tests-with-buffer 10 nil
      (mail-thread-collapse (thread-tests-message 10))
      (mail-thread-hide-unmatched "from:bob" '("2@x"))
      (mail-thread-filter "")
      (expect (thread-tests-shown) :to-equal '(10 11 12))
      (expect (mail-thread-rendered-p (thread-tests-message 10)) :to-be-truthy)
      (expect (thread-tests-open-articles) :to-be nil)
      (expect header-line-format :to-equal "Plan   3 messages")))
  (it "moves between the messages on show only"
    (thread-tests-with-buffer 10 nil
      (mail-thread-hide-unmatched "from:ann" '("1@x" "3@x"))
      (mail-thread-goto-message (thread-tests-message 10))
      (mail-thread-next-message)
      (expect (mail-thread-message-article (mail-thread-message-at-point)) :to-equal 12)
      (mail-thread-previous-message)
      (expect (mail-thread-message-article (mail-thread-message-at-point)) :to-equal 10)))
  (it "takes a point in hidden text for the message shown above it, or the first"
    ;; the cursor of a point there shows at the end of that message
    (thread-tests-with-buffer 10 nil
      (mail-thread-hide-unmatched "from:ann" '("1@x" "3@x"))
      (goto-char (mail-thread-message-marker (thread-tests-message 11)))
      (expect (mail-thread-message-article (mail-thread-message-at-point)) :to-equal 10)
      (mail-thread-hide-unmatched "subject:budget" '("3@x"))
      (goto-char (point-min))
      (expect (mail-thread-message-article (mail-thread-message-at-point)) :to-equal 12)))
  (it "moves point off a message it hides, to the next one on show"
    (thread-tests-with-buffer 10 nil
      (goto-char (mail-thread-message-marker (thread-tests-message 11)))
      (mail-thread-hide-unmatched "from:ann" '("1@x" "3@x"))
      (expect (point)
              :to-equal (marker-position (mail-thread-message-marker (thread-tests-message 12))))))
  (it "leaves a hidden unread message unrendered and unread until the filter goes"
    ;; a render marks the message read
    (thread-tests-with-buffer 10 '(11 12)
      (mail-thread-hide-unmatched "from:ann" '("1@x" "3@x"))
      (thread-tests-fill)
      (expect thread-tests-rendered :to-equal '(12 10))
      (expect thread-tests-marked :to-equal '(12 10))
      (expect (thread-tests-waiting) :to-equal '(11))
      (expect (thread-tests-timer-active-p mail-thread-fill-timer) :to-be t)
      (mail-thread-filter "")
      (mail-thread-fill (current-buffer) mail-thread-fill-timer)
      (expect thread-tests-rendered :to-equal '(11 12 10))
      (expect (thread-tests-waiting) :to-be nil)))
  (it "returns before notmuch answers, and hides once it does"
    (thread-tests-with-buffer 10 nil
      (let ((gnus-search-notmuch-program (thread-tests-notmuch "id:2@x\\n" 0.3)))
        (unwind-protect
            (let ((process (progn (mail-thread-filter "from:bob")
                                  mail-thread-filter-process)))
              (expect (process-live-p process) :to-be-truthy)
              (expect (thread-tests-shown) :to-equal '(10 11 12))
              (thread-tests-wait-for process)
              (expect (thread-tests-shown) :to-equal '(11))
              (expect mail-thread-filter-process :to-be nil))
          (delete-file gnus-search-notmuch-program)))))
  (it "drops an answer for a thread the buffer no longer shows"
    (thread-tests-with-buffer 10 nil
      (let ((gnus-search-notmuch-program (thread-tests-notmuch "id:2@x\\n" 0.3)))
        (unwind-protect
            (let ((process (progn (mail-thread-filter "from:bob")
                                  mail-thread-filter-process)))
              ;; what open-mail-thread does before it builds the next one
              (let ((inhibit-read-only t))
                (erase-buffer)
                (remove-overlays))
              (mail-thread-mode)
              (setq mail-thread-group "nnmaildir+gmail:inbox"
                    mail-thread-summary-buffer summary)
              (mail-thread-build thread-tests-entries 12 nil)
              (thread-tests-wait-for process)
              (expect (thread-tests-shown) :to-equal '(10 11 12)))
          (delete-file gnus-search-notmuch-program)))))
  (it "hides nothing when notmuch fails"
    (thread-tests-with-buffer 10 nil
      (let ((gnus-search-notmuch-program (thread-tests-notmuch "" nil 1)))
        (unwind-protect
            (let ((process (progn (mail-thread-filter "from:bob")
                                  mail-thread-filter-process)))
              (spy-on 'message)
              (thread-tests-wait-for process)
              (expect (thread-tests-shown) :to-equal '(10 11 12))
              (expect 'message :to-have-been-called-with "notmuch could not filter the thread"))
          (delete-file gnus-search-notmuch-program))))))
