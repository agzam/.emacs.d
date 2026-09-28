;;; tests/scripts/news-fetch-tests.el --- news fetch specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "scripts/news-fetch.el")

(defvar news-fetch-tests--script
  (expand-file-name "scripts/news-fetch.el" test-config-root)
  "The script a child Emacs runs.")

;;; A stand-in NNTP server, answering from this Emacs

(defvar news-fetch-tests--posts nil
  "Posts of gmane.test on the stand-in, each (NUMBER ID DAYS-AGO REFERENCES BODY).")

(defvar news-fetch-tests--by-id nil
  "Posts the stand-in finds only by Message-ID, shaped like `news-fetch-tests--posts'.")

(defvar news-fetch-tests--log nil
  "Commands the stand-in received, newest first.")

(defvar news-fetch-tests--broken nil
  "Command the stand-in answers with a fault.")

(defun news-fetch-tests--date (days-ago)
  "Date header value DAYS-AGO days before now."
  (format-time-string "%a, %d %b %Y %H:%M:%S +0000"
                      (time-subtract nil (* days-ago 86400)) t))

(defun news-fetch-tests--text (post)
  "The article of POST, lines ending in CRLF as NNTP sends them."
  (pcase-let ((`(,number ,id ,days ,references ,body) post))
    (concat "From: Ann <ann@example.com>\r\n"
            "Newsgroups: gmane.test\r\n"
            "Subject: post " (number-to-string number) "\r\n"
            "Date: " (news-fetch-tests--date days) "\r\n"
            "Message-ID: " id "\r\n"
            (if references (concat "References: " references "\r\n") "")
            "\r\n"
            (or body "body\r\n"))))

(defun news-fetch-tests--range ()
  "(LOW . HIGH) of gmane.test on the stand-in."
  (let ((numbers (mapcar #'car news-fetch-tests--posts)))
    (cons (apply #'min numbers) (apply #'max numbers))))

(defun news-fetch-tests--over (range)
  "The stand-in's answer to OVER RANGE."
  (pcase-let* ((`(,from ,to) (mapcar #'string-to-number (split-string range "-")))
               (posts (seq-filter (lambda (post) (<= from (car post) to))
                                  news-fetch-tests--posts)))
    (if (null posts)
        "423 No articles in that range\r\n"
      (concat "224 Overview information follows\r\n"
              (mapconcat (pcase-lambda (`(,number ,id ,days ,references))
                           (format "%d\tpost %d\tAnn <ann@example.com>\t%s\t%s\t%s\t100\t1\r\n"
                                   number number (news-fetch-tests--date days) id
                                   (or references "")))
                         posts "")
              ".\r\n"))))

(defun news-fetch-tests--article (spec)
  "The stand-in's answer to ARTICLE SPEC, a number or a Message-ID."
  (if-let* ((post (if (string-prefix-p "<" spec)
                      (seq-find (lambda (post) (equal (nth 1 post) spec))
                                (append news-fetch-tests--posts news-fetch-tests--by-id))
                    (assq (string-to-number spec) news-fetch-tests--posts))))
      (concat (format "220 %d %s\r\n" (car post) (nth 1 post))
              (replace-regexp-in-string "^\\." ".." (news-fetch-tests--text post))
              ".\r\n")
    (if (string-prefix-p "<" spec)
        "430 No such article\r\n"
      "423 No such article number\r\n")))

(defun news-fetch-tests--malformed-p (spec)
  "Non-nil when SPEC is a Message-ID INN rejects: two @, or over 250 long."
  (and (string-prefix-p "<" spec)
       (or (string-match-p "@.*@" spec) (< 250 (length spec)))))

(defun news-fetch-tests--answer (line)
  "The stand-in's answer to command LINE."
  (push line news-fetch-tests--log)
  (pcase-let ((`(,command . ,args) (split-string line " ")))
    (cond
     ((equal command news-fetch-tests--broken) "503 program fault\r\n")
     ((equal command "CAPABILITIES")
      "101 Capability list:\r\nVERSION 2\r\nREADER\r\nOVER\r\n.\r\n")
     ((equal command "MODE") "201 stand-in, no posting\r\n")
     ((equal command "GROUP")
      (if (equal (car args) "gmane.test")
          (pcase-let ((`(,low . ,high) (news-fetch-tests--range)))
            (format "211 %d %d %d gmane.test\r\n" (length news-fetch-tests--posts) low high))
        "411 No such group\r\n"))
     ((equal command "OVER") (news-fetch-tests--over (car args)))
     ((and (equal command "ARTICLE") (news-fetch-tests--malformed-p (car args)))
      "501 Syntax error in message-ID\r\n")
     ((equal command "ARTICLE") (news-fetch-tests--article (car args)))
     ((equal command "LIST")
      (concat "215 Newsgroups follow\r\n"
              "gmane.test 0000000009 0000000001 y\r\n"
              "gmane.other 0000000005 0000000003 m\r\n"
              ".\r\n"))
     ((equal command "QUIT") "205 Bye\r\n")
     (t "500 What?\r\n"))))

(defun news-fetch-tests--filter (process string)
  "Answer every whole command line STRING completes on PROCESS."
  (let ((pending (concat (process-get process 'pending) string)))
    (while (string-match "\r\n" pending)
      (let ((line (substring pending 0 (match-beginning 0))))
        (setq pending (substring pending (match-end 0)))
        (process-send-string process (news-fetch-tests--answer line))))
    (process-put process 'pending pending)))

(defun news-fetch-tests--serve ()
  "Start the stand-in on a free local port and return its process."
  (make-network-process :name "nntp stand-in" :server t :host "127.0.0.1" :service t
                        :family 'ipv4 :coding 'binary :noquery t
                        :filter #'news-fetch-tests--filter
                        :log (lambda (_server client _message)
                               (process-send-string client "200 stand-in ready\r\n"))))

(defmacro news-fetch-tests--with-server (&rest body)
  "Run BODY with the stand-in serving, a store holding gmane.test, and a group list.
BODY sees `store', `active' and `port'."
  (declare (indent 0))
  `(let* ((server (news-fetch-tests--serve))
          (port (process-contact server :service))
          (store (file-name-as-directory (make-temp-file "news-store" t)))
          (active (expand-file-name "active" (make-temp-file "news-cache" t)))
          (news-fetch-tests--log nil)
          (news-fetch-tests--broken nil))
     (ignore port active)
     (make-directory (expand-file-name "gmane.test" store))
     (unwind-protect (progn ,@body)
       (delete-process server)
       (delete-directory store t)
       (delete-directory (file-name-directory active) t))))

(defun news-fetch-tests--run (store active port)
  "Fetch STORE's groups from the stand-in at PORT, saving the group list in ACTIVE."
  (news-fetch-run store active "127.0.0.1" port))

(defun news-fetch-tests--files (store)
  "gmane.test's message files under STORE, by subject, as (SUBJECT . FLAGS)."
  (let ((cur (expand-file-name "gmane.test/cur" store)))
    (sort (mapcar (lambda (file)
                    (cons (with-temp-buffer
                            (insert-file-contents (expand-file-name file cur))
                            (mail-fetch-field "Subject"))
                          (cadr (split-string file ":"))))
                  (directory-files cur nil "\\`[^.]"))
          (lambda (a b) (string< (car a) (car b))))))

(defun news-fetch-tests--overviews (store)
  "Number of overview files gmane.test holds under STORE."
  (length (directory-files (expand-file-name "gmane.test/.nnmaildir/nov" store)
                           nil "\\`[^.]")))

(defun news-fetch-tests--by-number (store)
  "Subjects of gmane.test's posts under STORE, in article number order."
  (let ((nov (expand-file-name "gmane.test/.nnmaildir/nov" store)))
    (mapcar #'cdr
            (sort (mapcar (lambda (prefix)
                            ;; nnmaildir's overview: [1 NUMBER MESSAGE-ID [FIELDS ...]]
                            (let ((entry (with-temp-buffer
                                           (insert-file-contents (expand-file-name prefix nov))
                                           (read (current-buffer)))))
                              (cons (aref entry 1)
                                    (car (split-string (aref (aref entry 3) 0) "\t")))))
                          (directory-files nov nil "\\`[^.]"))
                  #'car-less-than-car))))

(defun news-fetch-tests--sent (command)
  "Arguments of each COMMAND the stand-in received, oldest first."
  (seq-keep (lambda (line)
              (when (string-prefix-p (concat command " ") line)
                (substring line (1+ (length command)))))
            (reverse news-fetch-tests--log)))

(defconst news-fetch-tests--year
  '((1 "<p1@test>" 400 nil nil)
    (2 "<p2@test>" 300 nil nil)
    (3 "<p3@test>" 10 nil nil)
    (4 "<p4@test>" 2 "<p3@test>" nil))
  "gmane.test over a year and a half: one post too old, one of this week.")

;;; Specs

(describe "news-fetch-run"
  (it "fetches a year back, leaves the last week unread and builds every overview"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil)
          ;; two per chunk, so the walk back takes three requests
          (news-fetch-chunk 2))
      (news-fetch-tests--with-server
        (let ((rows (news-fetch-tests--run store active port)))
          (expect (mapcar (lambda (row) (seq-take row 3)) rows)
                  :to-equal '(("gmane.test" 3 0))))
        (expect (news-fetch-tests--files store)
                :to-equal '(("post 2" . "2,S") ("post 3" . "2,S") ("post 4" . "2,")))
        (expect (news-fetch-tests--overviews store) :to-equal 3)
        (expect (news-fetch-read-mark store "gmane.test") :to-equal 4)
        ;; the post older than a year was never downloaded
        (expect (news-fetch-tests--sent "ARTICLE") :to-equal '("2" "3" "4")))))

  (it "marks each batch read as it lands, so a run cut short leaves none unread"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil)
          (news-fetch-batch 1)
          marked)
      (news-fetch-tests--with-server
        (advice-add 'nnmaildir-request-set-mark :before
                    (lambda (_group actions &rest _) (push (caar actions) marked))
                    '((name . news-fetch-tests)))
        (unwind-protect
            (news-fetch-tests--run store active port)
          (advice-remove 'nnmaildir-request-set-mark 'news-fetch-tests))
        ;; posts 2 and 3 are the old ones, each a batch of its own
        (expect (length marked) :to-equal 2))))

  (it "never makes every disk sync after a delivery"
    ;; each later write would wait for it: 114 ms a post instead of 8
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (spy-on 'unix-sync)
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (expect (length (news-fetch-tests--files store)) :to-equal 3))
      (expect 'unix-sync :not :to-have-been-called)))

  (it "fetches only what came after the saved mark, unread"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (setq news-fetch-tests--log nil)
        (let ((news-fetch-tests--posts
               (append news-fetch-tests--year '((5 "<p5@test>" 20 nil nil)))))
          (expect (seq-take (car (news-fetch-tests--run store active port)) 3)
                  :to-equal '("gmane.test" 1 0))
          (expect (news-fetch-tests--sent "OVER") :to-equal '("5-5"))
          (expect (news-fetch-tests--sent "ARTICLE") :to-equal '("5")))
        ;; a late post is new all the same
        (expect (cdr (assoc "post 5" (news-fetch-tests--files store))) :to-equal "2,")
        (expect (news-fetch-read-mark store "gmane.test") :to-equal 5))))

  (it "brings older posts that new ones refer to, read, and only once"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id '((77 "<root@test>" 500 nil nil))))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (let ((news-fetch-tests--posts
               (append news-fetch-tests--year
                       '((5 "<p5@test>" 1 "<root@test> <p3@test> <gone@test>" nil)))))
          (expect (seq-take (car (news-fetch-tests--run store active port)) 3)
                  :to-equal '("gmane.test" 1 1))
          ;; p3 is in the folder; the server lacks gone
          (expect (seq-filter (lambda (spec) (string-prefix-p "<" spec))
                              (news-fetch-tests--sent "ARTICLE"))
                  :to-equal '("<root@test>" "<gone@test>"))
          (expect (cdr (assoc "post 77" (news-fetch-tests--files store))) :to-equal "2,S")
          (setq news-fetch-tests--log nil)
          (expect (seq-take (car (news-fetch-tests--run store active port)) 3)
                  :to-equal '("gmane.test" 0 0))
          (expect (news-fetch-tests--sent "ARTICLE") :to-be nil)))))

  (it "asks for no malformed Message-ID, whose answer would stall the requests around it"
    ;; nntp.el's pipelined requests never count the 501 it gets
    (let* ((long (concat "<" (make-string 250 ?x) "@test>"))
           (news-fetch-tests--posts
            `((1 "<p1@test>" 1 ,(concat "<root@test> <87x.fsf@ann@example.com> " long) nil)))
           (news-fetch-tests--by-id '((77 "<root@test>" 500 nil nil))))
      (news-fetch-tests--with-server
        (expect (seq-take (car (with-timeout (10 (error "The fetch stalled"))
                                 (news-fetch-tests--run store active port)))
                          3)
                :to-equal '("gmane.test" 1 1))
        (expect (seq-filter (lambda (spec) (string-prefix-p "<" spec))
                            (news-fetch-tests--sent "ARTICLE"))
                :to-equal '("<root@test>")))))

  (it "brings the parents of posts a run cut short delivered"
    ;; its posts landed, their parents did not, and it saved no mark
    (let ((news-fetch-tests--posts '((1 "<p1@test>" 2 "<root@test>" nil)))
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (delete-file (news-fetch-mark-path store "gmane.test"))
        (let ((news-fetch-tests--by-id '((77 "<root@test>" 500 nil nil))))
          (expect (seq-take (car (news-fetch-tests--run store active port)) 3)
                  :to-equal '("gmane.test" 0 1)))
        (expect (cdr (assoc "post 77" (news-fetch-tests--files store))) :to-equal "2,S"))))

  (it "numbers the posts that others refer to first, so the newest number highest"
    ;; an entry shows a group's highest numbers as its newest posts
    (let ((news-fetch-tests--posts
           (append news-fetch-tests--year '((5 "<p5@test>" 1 "<root@test>" nil))))
          (news-fetch-tests--by-id '((77 "<root@test>" 500 nil nil))))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (expect (news-fetch-tests--by-number store)
                :to-equal '("post 77" "post 2" "post 3" "post 4" "post 5")))))

  (it "fetches no post the folder holds, even without a mark"
    ;; a lost mark, or a server that renumbered the group
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (news-fetch-save-mark store "gmane.test" 1000)
        (setq news-fetch-tests--log nil)
        (expect (seq-take (car (news-fetch-tests--run store active port)) 3)
                :to-equal '("gmane.test" 0 0))
        (expect (news-fetch-tests--sent "ARTICLE") :to-be nil)
        (expect (length (news-fetch-tests--files store)) :to-equal 3)
        (expect (news-fetch-read-mark store "gmane.test") :to-equal 4))))

  (it "keeps the mark where it was when the server fails"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (setq news-fetch-tests--broken "OVER")
        (let ((row (car (news-fetch-tests--run store active port))))
          (expect (seq-take row 2) :to-equal '("gmane.test" error))
          (expect (nth 2 row) :to-match "503"))
        (expect (news-fetch-read-mark store "gmane.test") :to-be nil)
        (expect (news-fetch-tests--files store) :to-be nil))))

  (it "keeps dot-stuffed lines and non-ASCII bytes as the poster wrote them"
    (let* ((body (concat ".starts with a dot\r\n..two dots\r\ncaf\303\251\r\n"))
           (news-fetch-tests--posts `((1 "<p1@test>" 1 nil ,body)))
           (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (let ((cur (expand-file-name "gmane.test/cur" store)))
          (expect (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally (car (directory-files cur t "\\`[^.]")))
                    (buffer-string))
                  :to-equal (encode-coding-string
                             (string-replace "\r\n" "\n"
                                             (news-fetch-tests--text (car news-fetch-tests--posts)))
                             'raw-text))))))

  (it "saves the server's group list, then asks for it again only once it is a day old"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (news-fetch-tests--run store active port)
        (expect (with-temp-buffer
                  (insert-file-contents active)
                  (buffer-string))
                :to-equal (concat "gmane.test 0000000009 0000000001 y\n"
                                  "gmane.other 0000000005 0000000003 m\n"))
        (setq news-fetch-tests--log nil)
        (news-fetch-tests--run store active port)
        (expect (member "LIST" news-fetch-tests--log) :to-be nil)
        (set-file-times active (time-subtract nil (1+ news-fetch-active-age)))
        (news-fetch-tests--run store active port)
        (expect (member "LIST" news-fetch-tests--log) :to-be-truthy))))

  (it "fetches every folder of the store and reports a group the server lacks"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (make-directory (expand-file-name "gmane.gone" store))
        (let ((rows (news-fetch-tests--run store active port)))
          (expect (mapcar #'car rows) :to-equal '("gmane.gone" "gmane.test"))
          (expect (seq-take (car rows) 2) :to-equal '("gmane.gone" error))
          (expect (nth 1 (cadr rows)) :to-equal 3))))))

(describe "news-fetch-main"
  (it "runs as a program, reports each group and exits 0"
    (let ((news-fetch-tests--posts news-fetch-tests--year)
          (news-fetch-tests--by-id nil))
      (news-fetch-tests--with-server
        (let* ((sandbox (make-temp-file "news-fetch-init" t))
               (output (generate-new-buffer " *news-fetch-main*"))
               (process
                (make-process
                 :name "news-fetch-main" :buffer output :noquery t
                 :command (list (expand-file-name invocation-name invocation-directory)
                                "-Q" "--batch" "--init-directory" sandbox
                                "-l" news-fetch-tests--script
                                "--eval" (format "(news-fetch-main %S %S %S %d)"
                                                 store active "127.0.0.1" port)))))
          (unwind-protect
              (progn
                ;; the stand-in answers from this Emacs while it waits
                (with-timeout (60)
                  (while (process-live-p process)
                    (accept-process-output nil 0.05)))
                (expect (process-exit-status process) :to-equal 0)
                (expect (with-current-buffer output (buffer-string))
                        :to-match "^gmane.test: 3 new, 0 older for replies")
                (expect (length (news-fetch-tests--files store)) :to-equal 3))
            (kill-buffer output)
            (delete-directory sandbox t))))))

  (it "exits 1 when the server cannot be reached"
    (let* ((store (file-name-as-directory (make-temp-file "news-store" t)))
           (sandbox (make-temp-file "news-fetch-init" t))
           ;; a port that was free a moment ago
           (port (let ((probe (make-network-process :name "port" :server t :host "127.0.0.1"
                                                    :service t :family 'ipv4)))
                   (prog1 (process-contact probe :service)
                     (delete-process probe))))
           (output (generate-new-buffer " *news-fetch-main*")))
      (make-directory (expand-file-name "gmane.test" store))
      (unwind-protect
          (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                      nil output nil
                                      "-Q" "--batch" "--init-directory" sandbox
                                      "-l" news-fetch-tests--script
                                      "--eval" (format "(news-fetch-main %S %S %S %d)"
                                                       store (expand-file-name "active" sandbox)
                                                       "127.0.0.1" port))))
            (expect status :to-equal 1)
            (expect (with-current-buffer output (buffer-string))
                    :to-match "news-fetch: No connection to 127.0.0.1"))
        (kill-buffer output)
        (delete-directory store t)
        (delete-directory sandbox t)))))

;;; news-fetch-tests.el ends here
