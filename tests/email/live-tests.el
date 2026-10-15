;;; tests/email/live-tests.el --- live mail search specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; live.el's top-level (require 'consult) must no-op in the batch tier;
;; the specs stub the consult calls the command makes
(provide 'consult)

(defvar gmail-maildir)
(defvar news-maildir)
(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar mail-archive-group "nnmaildir+gmail:archive")
(defvar mail-label-colors nil)

(load-module-file "scripts/mail-load.el")
(load-module-file "modules/email/autoload/load.el")
(load-module-file "modules/email/autoload/mail.el")
(load-module-file "modules/email/autoload/thread.el")
(load-module-file "modules/email/autoload/quotes.el")
(load-module-file "modules/email/autoload/html.el")
(load-module-file "modules/email/autoload/live.el")

(defun live-tests--thread (&rest props)
  "A notmuch thread summary with PROPS over a thread of three messages."
  (append props (list :thread "0000000000000abc" :timestamp 1791845947
                      :matched 2 :total 3 :authors "Ann, Bob| Cy"
                      :subject "Release plan" :tags '("inbox"))))

(defun live-tests--notmuch (dir output)
  "Stand-in notmuch in DIR: log its arguments to DIR/args, print OUTPUT."
  (let ((script (expand-file-name "notmuch" dir)))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"$*\" >> " (shell-quote-argument (expand-file-name "args" dir)) "\n"
              (if output (format "printf '%%s\\n' %s\n" (shell-quote-argument output)) "")))
    (set-file-modes script #o755)
    script))

(defmacro live-tests--with-notmuch (output &rest body)
  "Run BODY with a stand-in notmuch printing OUTPUT; `runs' gives its arguments.
`dir' names a scratch directory BODY may fill."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "live-tests" t)))
          (gnus-search-notmuch-program (live-tests--notmuch dir ,output))
          (gnus-search-notmuch-config-file "/config/file"))
     (cl-flet ((runs ()
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "args" dir))
                   (split-string (buffer-string) "\n" t))))
       (unwind-protect (progn ,@body)
         (delete-directory dir t)))))

(defun live-tests--message (file &rest lines)
  "Write a message to FILE: LINES of headers, an empty line, then a body."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (string-join lines "\n") "\n")))

;;; Listing threads

(describe "mail-live-command"
  (it "asks the notmuch gnus-search runs for the threads the input matches"
    (let ((gnus-search-notmuch-program "nm")
          (gnus-search-notmuch-config-file "/config/file")
          (mail-live-query nil))
      (expect (mail-live-command "from:ann plan")
              :to-equal '("nm" "--config=/config/file" "search" "--format=sexp"
                          "--output=summary" "--limit=100" "from:ann plan"))
      (expect mail-live-query :to-equal "from:ann plan")))
  (it "runs nothing for blank input"
    (let ((mail-live-query "before"))
      (expect (mail-live-command "  ") :to-be nil)
      (expect mail-live-query :to-equal "before"))))

(describe "mail-live-thread"
  (it "reads the thread on each line of notmuch's output, the first and last too"
    ;; the output is one list, a thread per line
    (expect (mapcar (lambda (line) (plist-get (mail-live-thread line) :thread))
                    '("((:thread \"a\" :subject \"one\")"
                      "(:thread \"b\" :subject \"two\")"
                      "(:thread \"c\" :subject \"three\"))"
                      "((:thread \"d\" :subject \"alone\"))"))
            :to-equal '("a" "b" "c" "d")))
  (it "skips what is not a thread"
    (expect (mapcar #'mail-live-thread '("()" "" "(:thread" "not a list" "(1 2)"))
            :to-equal '(nil nil nil nil nil))))

(describe "mail-live-candidate"
  (it "marks a thread unread and starred as summary lines do"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (expect (substring (mail-live-candidate
                          (live-tests--thread :tags '("inbox" "unread" "flagged")) "q")
                         0 2)
              :to-equal "●∗")
      (expect (substring (mail-live-candidate (live-tests--thread) "q") 0 2) :to-equal "  ")))
  (it "shows the date, the authors, the matched count and the subject"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (expect (substring-no-properties (mail-live-candidate (live-tests--thread) "q"))
              :to-equal (concat "   Today, 17:59     Ann, Bob, Cy" (make-string 12 ?\s)
                                " 2/3 Release plan"))))
  (it "leaves the count off a thread of one message"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (expect (mail-live-candidate (live-tests--thread :matched 1 :total 1) "q")
              :not :to-match " 1/1 ")))
  (it "cuts long authors to the width of the summary's sender column"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (expect (substring-no-properties
               (mail-live-candidate
                (live-tests--thread :authors "Ann Aardvark, Bob Bobbington, Cy Cyrus") "q"))
              :to-match "Ann Aardvark, Bob Bobbin 2/3 ")))
  (it "carries its thread for the preview and RET"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (expect (plist-get (get-text-property 0 'mail-thread
                                            (mail-live-candidate (live-tests--thread) "q"))
                         :thread)
              :to-equal "0000000000000abc")))
  (it "carries the query that found it, on every character"
    ;; a collect buffer outlives the search, and the next one sets
    ;; mail-live-query anew
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (let ((line (mail-live-candidate (live-tests--thread) "from:ann")))
        (expect (get-text-property 0 'mail-query line) :to-equal "from:ann")
        (expect (get-text-property (1- (length line)) 'mail-query line) :to-equal "from:ann")))))

(describe "mail-live-candidates"
  (it "makes one candidate per thread line and drops the rest"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (expect (mapcar (lambda (candidate)
                        (plist-get (get-text-property 0 'mail-thread candidate) :thread))
                      (mail-live-candidates
                       '("((:thread \"a\" :total 1 :tags nil)"
                         "garbage"
                         "(:thread \"b\" :total 1 :tags nil))")))
              :to-equal '("a" "b"))))
  (it "gives each candidate the query notmuch ran for it"
    (cl-letf (((symbol-function 'mail-live-date) (lambda (_) "Today, 17:59")))
      (let ((mail-live-query "from:ann"))
        (expect (mapcar (lambda (candidate) (get-text-property 0 'mail-query candidate))
                        (mail-live-candidates '("((:thread \"a\" :total 1 :tags nil))")))
                :to-equal '("from:ann"))))))

(describe "mail-live-candidate-threads"
  (it "takes the thread of each line and skips a line without one"
    (expect (mapcar (lambda (thread) (plist-get thread :thread))
                    (mail-live-candidate-threads
                     (list (propertize "a" 'mail-thread '(:thread "a"))
                           "typed text"
                           (propertize "b" 'mail-thread '(:thread "b")))))
            :to-equal '("a" "b"))))

(describe "mail-live-date"
  (it "dates a thread the way summary lines date a message"
    (expect (mail-live-date 1791845947)
            :to-equal (gnus-user-date (format-time-string "%a, %d %b %Y %T %z" 1791845947)))))

;;; The newest match of a thread

(describe "mail-live-newest-id"
  (it "asks for the thread's newest message the query matches"
    (live-tests--with-notmuch "id:3@x"
      (expect (mail-live-newest-id (live-tests--thread) "from:ann") :to-equal "<3@x>")
      (expect (runs)
              :to-equal '("--config=/config/file search --output=messages --sort=newest-first --limit=1 thread:0000000000000abc and (from:ann)"))))
  (it "takes the quotes notmuch puts around an unusual id off"
    (live-tests--with-notmuch "id:\"odd id@x\""
      (expect (mail-live-newest-id (live-tests--thread) "from:ann") :to-equal "<odd id@x>")))
  (it "answers nil when nothing matches"
    (live-tests--with-notmuch nil
      (expect (mail-live-newest-id (live-tests--thread) "from:ann") :to-be nil))))

(describe "mail-live-newest-file"
  (it "names the thread's newest matching file under the name it has now"
    ;; notmuch learns a rename at its next run
    (live-tests--with-notmuch nil
      (let ((indexed (concat dir "inbox/cur/1.x:2,"))
            (now (concat dir "inbox/cur/1.x:2,S")))
        (live-tests--message now "Subject: one" "" "body")
        (setq gnus-search-notmuch-program (live-tests--notmuch dir indexed))
        (expect (mail-live-newest-file (live-tests--thread) "from:ann") :to-equal now)
        (expect (car (runs))
                :to-equal "--config=/config/file search --output=files --duplicate=1 --sort=newest-first --limit=1 thread:0000000000000abc and (from:ann)"))))
  (it "answers nil when nothing matches"
    (live-tests--with-notmuch nil
      (expect (mail-live-newest-file (live-tests--thread) "from:ann") :to-be nil))))

;;; Preview

(describe "mail-live-preview-text"
  (it "shows the message's headers, unfolded, then its body"
    (let* ((dir (file-name-as-directory (make-temp-file "live-tests" t)))
           (gmail-maildir dir)
           (news-maildir (concat dir "news/"))
           (gnus-newsrc-hashtb nil)
           (file (concat dir "inbox/cur/1.x:2,")))
      (unwind-protect
          (progn
            (live-tests--message file "From: Ann <ann@example.com>"
                                 "To: Bob <bob@example.com>"
                                 "Cc: Cy <cy@example.com>,"
                                 " Di <di@example.com>"
                                 "Subject: =?UTF-8?Q?Caf=C3=A9_plan?="
                                 "Message-ID: <1@x>"
                                 ""
                                 "The plan, as posted.")
            (let ((text (mail-live-preview-text file)))
              (expect (substring-no-properties text)
                      :to-equal (concat "From: Ann <ann@example.com>\n"
                                        "To: Bob <bob@example.com>\n"
                                        "Cc: Cy <cy@example.com>, Di <di@example.com>\n"
                                        "Subject: Café plan\n"
                                        "\nThe plan, as posted."))
              (expect (get-text-property 0 'face text) :to-be 'gnus-header-name)))
        (delete-directory dir t))))
  (it "renders an HTML message as the thread view does"
    (let* ((dir (file-name-as-directory (make-temp-file "live-tests" t)))
           (gmail-maildir dir)
           (news-maildir (concat dir "news/"))
           (mm-text-html-renderer #'render-mail-html)
           (file (concat dir "inbox/cur/1.x:2,")))
      (unwind-protect
          (progn
            (live-tests--message file "From: Ann <ann@example.com>"
                                 "Subject: plan"
                                 "MIME-Version: 1.0"
                                 "Content-Type: text/html; charset=utf-8"
                                 ""
                                 "<p>Agreed.</p><blockquote><p>The plan.</p></blockquote>")
            (let* ((text (substring-no-properties (mail-live-preview-text file)))
                   (body (substring text (+ 2 (string-search "\n\n" text)))))
              (expect body :to-match "\\`Agreed\\.")
              (expect body :to-match "^> The plan\\.")
              (expect body :not :to-match "<p>")))
        (delete-directory dir t)))))

(describe "mail-live-preview-buffer"
  (it "fills a read-only buffer with the newest matching message"
    (cl-letf (((symbol-function 'mail-live-newest-file) (lambda (&rest _) "/m/1.x"))
              ((symbol-function 'mail-live-preview-text) (lambda (file) (concat "text of " file))))
      (let ((buffer (mail-live-preview-buffer (live-tests--thread) "q")))
        (unwind-protect
            (with-current-buffer buffer
              (expect (buffer-string) :to-equal "text of /m/1.x")
              (expect buffer-read-only :to-be t)
              (expect (point) :to-be 1))
          (kill-buffer buffer)))))
  (it "says so when the message left the store"
    (cl-letf (((symbol-function 'mail-live-newest-file) #'ignore))
      (let ((buffer (mail-live-preview-buffer (live-tests--thread) "q")))
        (unwind-protect
            (with-current-buffer buffer
              (expect (buffer-string) :to-equal "This message is no longer in the store."))
          (kill-buffer buffer))))))

(describe "mail-live-preview-state"
  (it "previews beside the window the search started from, and gives that window its buffer back"
    (save-window-excursion
      (delete-other-windows)
      (let* ((origin (selected-window))
             (beside (split-window-right))
             (shown (generate-new-buffer " *live-tests shown*"))
             (preview (generate-new-buffer " *live-tests preview*")))
        (unwind-protect
            (progn
              (set-window-buffer beside shown)
              (cl-letf (((symbol-function 'minibuffer-selected-window) (lambda () origin))
                        ((symbol-function 'mail-live-preview-buffer) (lambda (&rest _) preview)))
                (let ((state (mail-live-preview-state))
                      (before (window-buffer origin)))
                  (funcall state 'preview (propertize "line" 'mail-thread (live-tests--thread)))
                  (expect (list (window-buffer origin) (window-buffer beside))
                          :to-equal (list before preview))
                  (funcall state 'exit nil)
                  (expect (window-buffer beside) :to-be shown))))
          (kill-buffer shown)
          (when (buffer-live-p preview)
            (kill-buffer preview))))))
  (it "previews in the window the search started from when it is the only one"
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window))
            (preview (generate-new-buffer " *live-tests preview*")))
        (unwind-protect
            (cl-letf (((symbol-function 'minibuffer-selected-window) (lambda () origin))
                      ((symbol-function 'mail-live-preview-buffer) (lambda (&rest _) preview)))
              (let ((state (mail-live-preview-state))
                    (before (window-buffer origin)))
                (funcall state 'preview (propertize "line" 'mail-thread (live-tests--thread)))
                (expect (window-buffer origin) :to-be preview)
                (funcall state 'exit nil)
                (expect (window-buffer origin) :to-be before)))
          (when (buffer-live-p preview)
            (kill-buffer preview))))))
  (it "kills the preview buffer when the search ends"
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window)))
        (cl-letf (((symbol-function 'minibuffer-selected-window) (lambda () origin))
                  ((symbol-function 'mail-live-newest-file) #'ignore))
          (let ((state (mail-live-preview-state)))
            (funcall state 'preview (propertize "line" 'mail-thread (live-tests--thread)))
            (expect (get-buffer mail-live-preview-name) :to-be-truthy)
            (funcall state 'exit nil)
            (expect (get-buffer mail-live-preview-name) :to-be nil))))))
  (it "previews nothing for a line that carries no thread"
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window))
            asked)
        (cl-letf (((symbol-function 'minibuffer-selected-window) (lambda () origin))
                  ((symbol-function 'mail-live-preview-buffer)
                   (lambda (&rest _) (setq asked t) (current-buffer))))
          (let ((state (mail-live-preview-state)))
            (funcall state 'preview nil)
            (funcall state 'preview "a line")
            (funcall state 'exit nil)))
        (expect asked :to-be nil)))))

;;; Reading the pick

(describe "mail-live-thread-ids"
  (it "asks notmuch for every message of the thread, newest first"
    (live-tests--with-notmuch "id:1@x\nid:2@x"
      (let ((mail-search-limit-max 2000))
        (expect (mail-live-thread-ids (list (live-tests--thread))) :to-equal '("id:1@x" "id:2@x"))
        (expect (runs)
                :to-equal '("--config=/config/file search --output=messages --sort=newest-first --limit=2000 thread:0000000000000abc")))))
  (it "asks for the messages of several threads in one process"
    (live-tests--with-notmuch "id:1@x"
      (let ((mail-search-limit-max 2000))
        (mail-live-thread-ids (list (live-tests--thread) '(:thread "0000000000000def")))
        (expect (runs)
                :to-equal '("--config=/config/file search --output=messages --sort=newest-first --limit=2000 thread:0000000000000abc or thread:0000000000000def")))))
  (it "stops at the most messages a search shows"
    ;; a query of more ids would outgrow one argument on Linux
    (live-tests--with-notmuch "id:1@x"
      (let ((mail-search-limit-max 7))
        (mail-live-thread-ids (list (live-tests--thread)))
        (expect (car (runs)) :to-match " --limit=7 ")))))

(describe "read-mail-live-threads"
  (it "searches every message of the threads and hands THEN their summary"
    (let ((summary (get-buffer-create "*Summary nnselect:live-tests*"))
          asked searched got)
      (unwind-protect
          (cl-letf (((symbol-function 'mail-live-thread-ids)
                     (lambda (threads) (setq asked threads) '("id:2@x" "id:1@x" "id:9@x")))
                    ((symbol-function 'when-mail-loaded) (lambda (_what action) (funcall action)))
                    ((symbol-function 'search-mail-now)
                     (lambda (query limit) (setq searched (list query limit)) "nnselect:live-tests")))
            (read-mail-live-threads (list (live-tests--thread) '(:thread "def"))
                                    (lambda (buffer) (setq got buffer))))
        (kill-buffer summary))
      (expect (mapcar (lambda (thread) (plist-get thread :thread)) asked)
              :to-equal '("0000000000000abc" "def"))
      (expect searched :to-equal '("id:2@x or id:1@x or id:9@x" nil))
      (expect got :to-be summary)))
  (it "hands THEN nil and searches nothing when notmuch names no message"
    ;; an empty query would match all mail
    (let (searched (got 'unset))
      (cl-letf (((symbol-function 'mail-live-thread-ids) #'ignore)
                ((symbol-function 'when-mail-loaded) (lambda (_what action) (funcall action)))
                ((symbol-function 'search-mail-now) (lambda (&rest _) (setq searched t))))
        (read-mail-live-threads (list (live-tests--thread)) (lambda (buffer) (setq got buffer))))
      (expect searched :to-be nil)
      (expect got :to-be nil)))
  (it "hands THEN nil when the search group finds nothing"
    (let ((got 'unset))
      (cl-letf (((symbol-function 'mail-live-thread-ids) (lambda (_) '("id:1@x")))
                ((symbol-function 'when-mail-loaded) (lambda (_what action) (funcall action)))
                ((symbol-function 'search-mail-now) #'ignore))
        (read-mail-live-threads (list (live-tests--thread)) (lambda (buffer) (setq got buffer))))
      (expect got :to-be nil)))
  (it "asks notmuch only once the groups are loaded, and says what waits"
    (let (waited asked)
      (cl-letf (((symbol-function 'mail-live-thread-ids) (lambda (_) (setq asked t) nil))
                ((symbol-function 'when-mail-loaded)
                 (lambda (what action) (push (list what (functionp action)) waited))))
        (read-mail-live-threads (list (live-tests--thread)) #'ignore)
        (read-mail-live-threads (list (live-tests--thread) '(:thread "def")) #'ignore))
      (expect asked :to-be nil)
      (expect (nreverse waited) :to-equal '(("for the thread" t) ("for the threads" t))))))

(describe "open-mail-live-thread"
  (it "reads the thread's search group in the thread view, at its newest match"
    ;; gnus-search would take thread:X in the query for a key of its own
    (let ((summary (get-buffer-create "*Summary nnselect:live-tests*"))
          searched went opened)
      (unwind-protect
          (cl-letf (((symbol-function 'mail-live-newest-id) (lambda (&rest _) "<3@x>"))
                    ((symbol-function 'mail-live-thread-ids)
                     (lambda (_) '("id:1@x" "id:2@x" "id:3@x")))
                    ((symbol-function 'when-mail-loaded) (lambda (_what action) (funcall action)))
                    ((symbol-function 'search-mail-now)
                     (lambda (query limit) (setq searched (list query limit)) "nnselect:live-tests"))
                    ((symbol-function 'gnus-id-to-article)
                     (lambda (id) (and (equal id "<3@x>") 12)))
                    ((symbol-function 'gnus-summary-goto-subject)
                     (lambda (article &rest _) (setq went article)))
                    ((symbol-function 'open-mail-thread)
                     (lambda () (setq opened (buffer-name)))))
            (open-mail-live-thread (live-tests--thread) "from:ann"))
        (kill-buffer summary))
      (expect searched :to-equal '("id:1@x or id:2@x or id:3@x" nil))
      (expect went :to-be 12)
      (expect opened :to-equal "*Summary nnselect:live-tests*")))
  (it "opens no thread view when the search group finds nothing"
    (let (opened)
      (cl-letf (((symbol-function 'mail-live-newest-id) (lambda (&rest _) "<3@x>"))
                ((symbol-function 'mail-live-thread-ids) (lambda (_) '("id:1@x")))
                ((symbol-function 'when-mail-loaded) (lambda (_what action) (funcall action)))
                ((symbol-function 'search-mail-now) #'ignore)
                ((symbol-function 'open-mail-thread) (lambda () (setq opened t))))
        (open-mail-live-thread (live-tests--thread) "from:ann"))
      (expect opened :to-be nil)))
  (it "searches nothing when notmuch names no message of the thread"
    ;; an empty query would match all mail
    (let (searched)
      (cl-letf (((symbol-function 'mail-live-newest-id) #'ignore)
                ((symbol-function 'mail-live-thread-ids) #'ignore)
                ((symbol-function 'when-mail-loaded) (lambda (_what action) (funcall action)))
                ((symbol-function 'search-mail-now) (lambda (&rest _) (setq searched t))))
        (open-mail-live-thread (live-tests--thread) "from:ann"))
      (expect searched :to-be nil)))
  (it "waits for the groups still loading, as a search does"
    (let (waited)
      (cl-letf (((symbol-function 'mail-live-newest-id) (lambda (&rest _) "<3@x>"))
                ((symbol-function 'mail-live-thread-ids) (lambda (_) '("id:1@x")))
                ((symbol-function 'when-mail-loaded)
                 (lambda (what action) (setq waited (list what (functionp action))))))
        (open-mail-live-thread (live-tests--thread) "from:ann"))
      (expect waited :to-equal '("for the thread" t)))))

(describe "search-mail-live"
  (it "lists threads through notmuch as you type, and reads the pick"
    (let ((mail-live-query "from:bob")
          collection props opened)
      (cl-letf (((symbol-function 'consult--process-collection)
                 (lambda (builder &rest rest) (list 'collection builder rest)))
                ((symbol-function 'consult--async-transform)
                 (lambda (fn) (list 'transform fn)))
                ((symbol-function 'consult--read)
                 (lambda (table &rest rest)
                   (setq collection table props rest)
                   (propertize "line" 'mail-thread (live-tests--thread) 'mail-query "from:ann")))
                ((symbol-function 'open-mail-live-thread)
                 (lambda (thread query) (setq opened (list (plist-get thread :thread) query)))))
        (search-mail-live))
      (expect collection
              :to-equal '(collection mail-live-command
                                     (:transform (transform mail-live-candidates))))
      (expect (list (plist-get props :category) (plist-get props :require-match)
                    (plist-get props :sort) (functionp (plist-get props :state)))
              :to-equal '(mail-thread t nil t))
      ;; the line's own query, not the last one sent
      (expect opened :to-equal '("0000000000000abc" "from:ann"))))
  (it "reads nothing when the search is left without a pick"
    (let (opened)
      (cl-letf (((symbol-function 'consult--process-collection) #'ignore)
                ((symbol-function 'consult--async-transform) #'ignore)
                ((symbol-function 'consult--read) #'ignore)
                ((symbol-function 'open-mail-live-thread) (lambda (&rest _) (setq opened t))))
        (search-mail-live))
      (expect opened :to-be nil))))

;;; Embark actions on the listed threads

(defun live-tests--line (thread-id &optional query)
  "A line of the live search over the thread THREAD-ID, found by QUERY."
  (propertize (concat "line " thread-id)
              'mail-thread (list :thread thread-id :subject thread-id)
              'mail-query (or query "from:ann")))

(describe "open-mail-live-candidate"
  (it "reads the line's thread at the newest match of the line's own query"
    (let ((mail-live-query "a later search")
          opened)
      (cl-letf (((symbol-function 'open-mail-live-thread)
                 (lambda (thread query) (setq opened (list (plist-get thread :thread) query)))))
        (open-mail-live-candidate (live-tests--line "abc" "from:ann")))
      (expect opened :to-equal '("abc" "from:ann"))))
  (it "reads nothing for text that carries no thread"
    (let (opened)
      (cl-letf (((symbol-function 'open-mail-live-thread) (lambda (&rest _) (setq opened t))))
        (open-mail-live-candidate "typed text"))
      (expect opened :to-be nil))))

(describe "open-mail-live-summary"
  (it "reads the threads of every line in one summary"
    (let (read)
      (cl-letf (((symbol-function 'read-mail-live-threads)
                 (lambda (threads then)
                   (setq read (mapcar (lambda (thread) (plist-get thread :thread)) threads))
                   (funcall then nil))))
        (open-mail-live-summary (list (live-tests--line "abc") (live-tests--line "def"))))
      (expect read :to-equal '("abc" "def")))))

(describe "open-mail-live-in-gmail"
  (it "opens the thread's newest match of the line's query in Gmail"
    (let (asked browsed)
      (cl-letf (((symbol-function 'mail-live-newest-id)
                 (lambda (thread query) (setq asked (list (plist-get thread :thread) query)) "<3@x>"))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
        (open-mail-live-in-gmail (live-tests--line "abc" "from:ann")))
      (expect asked :to-equal '("abc" "from:ann"))
      (expect browsed :to-equal "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A3%40x")))
  (it "opens nothing when no message of the thread matches any more"
    (let (browsed)
      (cl-letf (((symbol-function 'mail-live-newest-id) #'ignore)
                ((symbol-function 'browse-url) (lambda (&rest _) (setq browsed t))))
        (open-mail-live-in-gmail (live-tests--line "abc")))
      (expect browsed :to-be nil))))

(describe "export-mail-live-search"
  (it "leaves current a buffer that stands in for the lines' summary"
    ;; Embark renames the buffer its exporter leaves, and nnselect finds a
    ;; summary by name, so the summary must not be that buffer
    (let ((lines (list (live-tests--line "abc") (live-tests--line "def")))
          (searched nil)
          export)
      (unwind-protect
          (cl-letf (((symbol-function 'search-mail-now) (lambda (&rest _) (setq searched t)))
                    ((symbol-function 'search-mail) (lambda (&rest _) (setq searched t))))
            (with-temp-buffer
              (let ((before (current-buffer)))
                (export-mail-live-search lines)
                (setq export (current-buffer))
                (expect export :not :to-be before))
              (expect (buffer-local-value 'mail-live-export export) :to-be lines)
              (expect (buffer-local-value 'major-mode export) :to-be 'special-mode)
              (expect (with-current-buffer export (buffer-string))
                      :to-equal "Reading 2 threads in a summary...")))
        (when (buffer-live-p export)
          (kill-buffer export)))
      (expect searched :to-be nil))))

(describe "open-mail-live-export-h"
  (it "reads the export's threads in a summary, then drops the export"
    (let ((export (generate-new-buffer " *live-tests export*"))
          read)
      (unwind-protect
          (cl-letf (((symbol-function 'read-mail-live-threads)
                     (lambda (threads then)
                       (setq read (mapcar (lambda (thread) (plist-get thread :thread)) threads))
                       (funcall then (get-buffer-create " *live-tests summary*")))))
            (with-current-buffer export
              (setq mail-live-export (list (live-tests--line "abc") (live-tests--line "def")))
              (open-mail-live-export-h)))
        (when (buffer-live-p export)
          (kill-buffer export))
        (when-let* ((summary (get-buffer " *live-tests summary*")))
          (kill-buffer summary)))
      (expect read :to-equal '("abc" "def"))
      (expect (buffer-live-p export) :to-be nil)))
  (it "drops the export when no thread of it is in the store"
    (let ((export (generate-new-buffer " *live-tests export*")))
      (unwind-protect
          (cl-letf (((symbol-function 'read-mail-live-threads)
                     (lambda (_threads then) (funcall then nil))))
            (with-current-buffer export
              (setq mail-live-export (list (live-tests--line "abc")))
              (open-mail-live-export-h)))
        (expect (buffer-live-p export) :to-be nil)
        (when (buffer-live-p export)
          (kill-buffer export)))))
  (it "leaves alone a buffer any other export made"
    (let (read)
      (cl-letf (((symbol-function 'read-mail-live-threads) (lambda (&rest _) (setq read t))))
        (with-temp-buffer
          (open-mail-live-export-h)
          (expect (buffer-live-p (current-buffer)) :to-be t)))
      (expect read :to-be nil))))

;;; live-tests.el ends here
