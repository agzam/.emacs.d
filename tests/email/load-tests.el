;;; tests/email/load-tests.el --- background group loading specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-archive-group "nnmaildir+gmail:archive")

;; load.el takes the table format from the batch script
(load-module-file "scripts/mail-load.el")
(load-module-file "modules/email/autoload/load.el")
(load-module-file "modules/email/autoload/news.el")

(defmacro load-tests--with-state (bindings &rest body)
  "Run BODY with the loader's state empty and BINDINGS, then clean up after it.
nnmaildir's servers, the newsrc and every timer, buffer and process the
loader made are the spec's own."
  (declare (indent 1))
  `(let* ((mail-load-queue nil)
          (mail-load-running nil)
          (mail-load-process nil)
          (mail-load-tables nil)
          (mail-load-job nil)
          (mail-load-timer nil)
          (mail-load-waiters nil)
          (mail-load-said-at 0.0)
          (mail-load-gc-threshold nil)
          (gc-cons-threshold gc-cons-threshold)
          (nnmaildir--servers nil)
          (nnmaildir--cur-server nil)
          (gnus-newsrc-hashtb (gnus-make-hashtable))
          (gnus-active-hashtb (gnus-make-hashtable))
          (gnus-verbose 0)
          (gnus-secondary-select-methods
           '((nnmaildir "gmail" (directory "/mail/gmail/"))
             (nnmaildir "news" (directory "/mail/news/"))
             (nntp "news.example.org")))
          ,@bindings)
     (unwind-protect (progn ,@body)
       (when (timerp mail-load-timer)
         (cancel-timer mail-load-timer))
       (cancel-function-timers #'mail-load-collect)
       (when mail-load-job
         (kill-buffer (mail-load-job-buffer mail-load-job))))))

(defun load-tests--server (&rest names)
  "Register an nnmaildir server gmail holding groups NAMES, as a Gnus start does."
  (let ((server (make-nnmaildir--srv :address "gmail" :dir "/mail/gmail/"
                                     :groups (make-hash-table :test #'equal))))
    (dolist (name names)
      (puthash name (make-nnmaildir--grp :name name) (nnmaildir--srv-groups server)))
    (setq nnmaildir--servers (list (cons "gmail" server)))
    server))

(defun load-tests--store (count)
  "Temp store whose inbox holds COUNT messages, the odd ones read."
  (let ((store (file-name-as-directory (make-temp-file "load-tests" t))))
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name (concat "inbox/" sub) store) t))
    (dotimes (i count)
      (with-temp-file (expand-file-name (format "inbox/cur/17000%05d.%d.fixture:2,%s"
                                                i i (if (cl-oddp i) "S" ""))
                                        store)
        (insert "From: Ann <ann@example.com>\n"
                "Subject: message " (number-to-string i) "\n"
                "Date: Mon, 21 Sep 2026 10:00:00 +0000\n"
                "Message-ID: <" (number-to-string i) "@fixture.example>\n\n"
                "body\n")))
    store))

(defun load-tests--table (store tables)
  "Read STORE's inbox the way the batch Emacs does; return the table file."
  (prog1 (mail-load-group tables "nnmaildir+gmail:inbox" store nil)
    ;; the batch Emacs is gone, and so is what it held
    (setq nnmaildir--servers nil
          nnmaildir--cur-server nil)))

(defun load-tests--open (store)
  "Open the session's nnmaildir server gmail over STORE; return its struct."
  (nnmaildir-open-server "gmail" `((directory ,store) (get-new-mail nil)))
  (setf (nnmaildir--srv-method nnmaildir--cur-server)
        `(nnmaildir "gmail" (directory ,store) (get-new-mail nil)))
  nnmaildir--cur-server)

(defun load-tests--apply-all ()
  "Run apply turns until no table waits; return how many ran."
  (let ((turns 0))
    (while (and (or mail-load-job mail-load-tables) (< turns 10000))
      (mail-load-step)
      (cl-incf turns))
    (when (timerp mail-load-timer)
      (cancel-timer mail-load-timer)
      (setq mail-load-timer nil))
    turns))

(defun load-tests--rows (group)
  "(NUMBER PREFIX SUFFIX MESSAGE-ID) of each article of nnmaildir GROUP, sorted."
  (sort (mapcar (lambda (entry)
                  (let ((article (cdr entry)))
                    (list (car entry) (nnmaildir--art-prefix article)
                          (nnmaildir--art-suffix article) (nnmaildir--art-msgid article))))
                (nnmaildir--grp-nlist group))
        (lambda (a b) (< (car a) (car b)))))

(defmacro load-tests--with-gnus-stubs (calls &rest body)
  "Run BODY with the Gnus calls of a finished load logged into CALLS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gnus-find-method-for-group) (lambda (_) '(nnmaildir "gmail")))
             ((symbol-function 'gnus-activate-group)
              (lambda (group &rest _) (push (list 'activate group) ,calls)))
             ((symbol-function 'gnus-get-unread-articles-in-group)
              (lambda (info &rest _) (push (list 'count (gnus-info-group info)) ,calls)))
             ((symbol-function 'gnus-group-update-group)
              (lambda (group &rest _) (push (list 'redraw group) ,calls))))
     ,@body))

(defun load-tests--info (group)
  "Put an info for GROUP into the newsrc, nothing read; return it."
  (let ((info (gnus-info-make group 3 nil nil '(nnmaildir "gmail"))))
    (puthash group (list nil info) gnus-newsrc-hashtb)
    info))

;;; The queue and the batch

(describe "load-mail-groups"
  (it "starts one batch on the maildir groups, each once, in order"
    (load-tests--with-state ((made nil))
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (push (plist-get args :command) made) 'batch))
                ((symbol-function 'process-send-eof) #'ignore))
        (load-mail-groups '("nnmaildir+gmail:inbox" "nntp+news.example.org:gmane.test"
                            "nnmaildir+gmail:sent" "nnmaildir+gmail:inbox")))
      (expect mail-load-running :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent"))
      (expect mail-load-queue :to-be nil)
      (expect (length made) :to-equal 1)
      ;; neither group is in memory, so both tables come back
      (expect (car (last (car made)))
              :to-match (regexp-quote "((\"nnmaildir+gmail:inbox\" \"/mail/gmail/\" t) (\"nnmaildir+gmail:sent\" \"/mail/gmail/\" t))"))))

  (it "puts groups asked for first ahead, stopping a batch busy with others"
    (load-tests--with-state ((mail-load-process 'batch)
                             (mail-load-running (list "nnmaildir+gmail:sent" "nnmaildir+gmail:job"))
                             (mail-load-queue (list "nnmaildir+gmail:kids"))
                             (deleted nil)
                             (made nil))
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'process-live-p) (lambda (process) (eq process 'batch)))
                ((symbol-function 'delete-process) (lambda (process) (push process deleted)))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (push (plist-get args :command) made) 'next))
                ((symbol-function 'process-send-eof) #'ignore))
        (load-mail-groups '("nnmaildir+gmail:emacs") t))
      (expect deleted :to-equal '(batch))
      (expect mail-load-process :to-be 'next)
      (expect mail-load-running
              :to-equal '("nnmaildir+gmail:emacs" "nnmaildir+gmail:sent"
                          "nnmaildir+gmail:job" "nnmaildir+gmail:kids"))))

  (it "leaves a batch alone that reads such a group for the first time right now"
    (load-tests--with-state ((mail-load-process 'batch)
                             (mail-load-running (list "nnmaildir+gmail:emacs" "nnmaildir+gmail:job"))
                             (deleted nil))
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'process-live-p) (lambda (process) (eq process 'batch)))
                ((symbol-function 'delete-process) (lambda (process) (push process deleted))))
        (load-mail-groups '("nnmaildir+gmail:emacs") t))
      (expect deleted :to-be nil)
      (expect mail-load-queue :to-be nil)))

  (it "reads a group again that the running batch has already read"
    (load-tests--with-state ((mail-load-process 'batch)
                             (mail-load-running (list "nnmaildir+gmail:inbox")))
      (load-tests--server "inbox")
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'process-live-p) (lambda (process) (eq process 'batch))))
        (load-mail-groups '("nnmaildir+gmail:inbox")))
      (expect mail-load-queue :to-equal '("nnmaildir+gmail:inbox"))))

  (it "holds a news group back while a news fetch delivers into it"
    (load-tests--with-state ((news-fetch-process 'fetch)
                             (made nil))
      (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                ((symbol-function 'process-live-p) (lambda (process) (eq process 'fetch)))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (push (plist-get args :command) made) 'batch))
                ((symbol-function 'process-send-eof) #'ignore))
        (load-mail-groups '("nnmaildir+news:gmane.test" "nnmaildir+gmail:inbox")))
      (expect mail-load-running :to-equal '("nnmaildir+gmail:inbox"))
      (expect mail-load-queue :to-equal '("nnmaildir+news:gmane.test"))))

  (it "keeps a news group waiting for its fetch pending, and nothing else waiting on it"
    ;; a search would wait the whole fetch otherwise
    (load-tests--with-state ((news-fetch-process 'fetch)
                             (mail-load-queue (list "nnmaildir+news:gmane.test")))
      (cl-letf (((symbol-function 'process-live-p) (lambda (process) (eq process 'fetch))))
        (expect (mail-group-pending-p "nnmaildir+news:gmane.test") :to-be-truthy)
        (expect (mail-load-busy-p) :to-be nil)
        (setq mail-load-queue (list "nnmaildir+news:gmane.test" "nnmaildir+gmail:inbox"))
        (expect (mail-load-busy-p) :to-be-truthy))))

  (it "starts nothing while Gnus is down"
    (load-tests--with-state ((made nil))
      (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
                ((symbol-function 'make-process) (lambda (&rest _) (push t made))))
        (load-mail-groups '("nnmaildir+gmail:inbox")))
      (expect made :to-be nil)
      (expect mail-load-queue :to-equal '("nnmaildir+gmail:inbox")))))

(describe "mail-load-filter"
  (it "acts on each whole report, even one split across two reads"
    (load-tests--with-state ((mail-load-process (start-process "load-tests" nil "true"))
                             (mail-load-running (list "nnmaildir+gmail:inbox" "nnmaildir+gmail:sent"))
                             (said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
        (mail-load-filter mail-load-process
                          "Loading nnmaildir...\n(mail-load loaded \"nnmaildir+gmail:inbox\" \"/t/in")
        (expect mail-load-tables :to-be nil)
        (mail-load-filter mail-load-process
                          "box\")\n(mail-load failed \"nnmaildir+gmail:sent\" \"No such directory\")\n"))
      (expect mail-load-tables :to-equal '(("nnmaildir+gmail:inbox" . "/t/inbox")))
      (expect mail-load-running :to-be nil)
      (expect said :to-equal '("Could not read nnmaildir+gmail:sent: No such directory"))))

  (it "lets a newer table of a group replace one not applied yet"
    (load-tests--with-state ((mail-load-tables (list (cons "nnmaildir+gmail:inbox" "/old")
                                                     (cons "nnmaildir+gmail:sent" "/sent"))))
      (mail-load-note '(loaded "nnmaildir+gmail:inbox" "/new"))
      (expect mail-load-tables :to-equal '(("nnmaildir+gmail:sent" . "/sent")
                                           ("nnmaildir+gmail:inbox" . "/new"))))))

;;; Applying a table

(describe "mail-load-step"
  (it "builds the group nnmaildir would, a slice at a time, and hands it over whole"
    (load-tests--with-state ((store (load-tests--store 250))
                             (tables (make-temp-file "load-tests-tables" t))
                             (mail-load-slice 0)
                             (calls nil))
      (unwind-protect
          (let ((file (load-tests--table store tables))
                (server (load-tests--open store)))
            (load-tests--info "nnmaildir+gmail:inbox")
            (mail-load-note (list 'loaded "nnmaildir+gmail:inbox" file))
            (load-tests--with-gnus-stubs calls
              ;; a turn reads the table, the next ones a hundred rows each
              (mail-load-step)
              (mail-load-step)
              (expect mail-load-job :not :to-be nil)
              (expect (gethash "inbox" (nnmaildir--srv-groups server)) :to-be nil)
              (expect (load-tests--apply-all) :to-be-greater-than 1))
            (let ((loaded (gethash "inbox" (nnmaildir--srv-groups server))))
              (expect (nnmaildir--grp-count loaded) :to-equal 250)
              ;; nnmaildir's own read of the same folder
              (nnmaildir-open-server "check" `((directory ,store) (get-new-mail nil)))
              (setf (nnmaildir--srv-method nnmaildir--cur-server)
                    `(nnmaildir "check" (directory ,store) (get-new-mail nil)))
              (nnmaildir-request-scan "inbox" "check")
              (expect (load-tests--rows loaded)
                      :to-equal (load-tests--rows (nnmaildir--prepare "check" "inbox"))))
            (expect (nreverse calls)
                    :to-equal '((activate "nnmaildir+gmail:inbox")
                                (count "nnmaildir+gmail:inbox")
                                (redraw "nnmaildir+gmail:inbox"))))
        (delete-directory store t)
        (delete-directory tables t))))

  (it "starts no garbage collection inside a slice"
    ;; one takes about 0.1 s in a large session
    (load-tests--with-state ((store (load-tests--store 3))
                             (tables (make-temp-file "load-tests-tables" t))
                             (gc-cons-threshold 800000)
                             (thresholds nil)
                             (calls nil))
      (unwind-protect
          (let ((file (load-tests--table store tables)))
            (load-tests--open store)
            (mail-load-note (list 'loaded "nnmaildir+gmail:inbox" file))
            (cl-letf* ((rows (symbol-function 'mail-load-rows))
                       ((symbol-function 'mail-load-rows)
                        (lambda (&rest args)
                          (push gc-cons-threshold thresholds)
                          (apply rows args))))
              (load-tests--with-gnus-stubs calls
                (load-tests--apply-all)))
            (expect thresholds :not :to-be nil)
            (expect (seq-every-p (lambda (threshold) (<= (* 128 1024 1024) threshold)) thresholds)
                    :to-be t)
            ;; nor between slices; the collection waits for an idle Emacs
            (expect gc-cons-threshold :to-be-greater-than 800000)
            (expect (seq-some (lambda (timer) (eq (timer--function timer) 'mail-load-collect))
                              timer-idle-list)
                    :to-be-truthy)
            (mail-load-collect)
            (expect gc-cons-threshold :to-equal 800000)
            (expect mail-load-gc-threshold :to-be nil))
        (delete-directory store t)
        (delete-directory tables t))))

  (it "gives a group read the first time the read ranges and stars its flags hold"
    (load-tests--with-state ((store (load-tests--store 4))
                             (tables (make-temp-file "load-tests-tables" t))
                             (calls nil))
      (unwind-protect
          (let* ((file (load-tests--table store tables))
                 (server (load-tests--open store))
                 (info (load-tests--info "nnmaildir+gmail:inbox")))
            ;; the newsrc said everything was read
            (setf (gnus-info-read info) '((1 . 100)))
            (mail-load-note (list 'loaded "nnmaildir+gmail:inbox" file))
            (load-tests--with-gnus-stubs calls
              (load-tests--apply-all))
            (let* ((loaded (gethash "inbox" (nnmaildir--srv-groups server)))
                   (numbers (mapcar #'car (nnmaildir--grp-nlist loaded)))
                   (read (seq-filter (lambda (entry)
                                       (string-suffix-p "S" (nnmaildir--art-suffix (cdr entry))))
                                     (nnmaildir--grp-nlist loaded))))
              (expect (seq-intersection (range-uncompress (gnus-info-read info)) numbers)
                      :to-have-same-items-as (mapcar #'car read))
              (expect (hash-table-count (nnmaildir--grp-mmth loaded)) :to-be-greater-than 0)))
        (delete-directory store t)
        (delete-directory tables t))))

  (it "reads a folder again that changed while the batch read it, and leaves the marks be"
    (load-tests--with-state ((store (load-tests--store 2))
                             (tables (make-temp-file "load-tests-tables" t))
                             (calls nil))
      (unwind-protect
          (let* ((file (load-tests--table store tables))
                 (server (load-tests--open store))
                 (info (load-tests--info "nnmaildir+gmail:inbox")))
            ;; the session holds the group from an earlier table
            (puthash "inbox" (mail-load-table-group
                              "inbox" (mail-load-read-table file (expand-file-name
                                                                  "inbox/.nnmaildir/num/" store)))
                     (nnmaildir--srv-groups server))
            (setf (gnus-info-read info) '((1 . 100)))
            ;; the phone read the unread message after the batch listed it
            (let ((unread (car (directory-files (expand-file-name "inbox/cur" store) t
                                                ":2,\\'"))))
              (rename-file unread (concat unread "S"))
              ;; a rename inside the same second leaves the mtime alone
              (set-file-times (expand-file-name "inbox/cur" store) (time-add nil 5)))
            (mail-load-note (list 'loaded "nnmaildir+gmail:inbox" file))
            (cl-letf (((symbol-function 'gnus-alive-p) #'ignore))
              (load-tests--with-gnus-stubs calls
                (load-tests--apply-all)))
            (expect (gnus-info-read info) :to-equal '((1 . 100)))
            (expect mail-load-queue :to-equal '("nnmaildir+gmail:inbox")))
        (delete-directory store t)
        (delete-directory tables t))))

  (it "opens the group's server when no start opened it, as with news"
    (load-tests--with-state ((store (load-tests--store 1))
                             (tables (make-temp-file "load-tests-tables" t))
                             (checked nil)
                             (calls nil))
      (unwind-protect
          (let ((file (load-tests--table store tables)))
            (load-tests--info "nnmaildir+gmail:inbox")
            (mail-load-note (list 'loaded "nnmaildir+gmail:inbox" file))
            (cl-letf (((symbol-function 'gnus-check-server)
                       (lambda (method) (push method checked) (load-tests--open store))))
              (load-tests--with-gnus-stubs calls
                (load-tests--apply-all)))
            (expect checked :to-equal '((nnmaildir "gmail")))
            (expect (mail-group-loaded-p "nnmaildir+gmail:inbox") :not :to-be nil))
        (delete-directory store t)
        (delete-directory tables t))))

  (it "drops a table whose server Gnus closed meanwhile"
    (load-tests--with-state ((store (load-tests--store 1))
                             (tables (make-temp-file "load-tests-tables" t))
                             (calls nil))
      (unwind-protect
          (let ((file (load-tests--table store tables)))
            (load-tests--open store)
            (mail-load-note (list 'loaded "nnmaildir+gmail:inbox" file))
            (mail-load-step)
            ;; Gnus quit and started again
            (setq nnmaildir--servers nil)
            (let ((fresh (load-tests--open store)))
              (load-tests--with-gnus-stubs calls
                (load-tests--apply-all))
              (expect (gethash "inbox" (nnmaildir--srv-groups fresh)) :to-be nil)
              (expect calls :to-be nil)))
        (delete-directory store t)
        (delete-directory tables t)))))

(describe "mail-load-apply-marks"
  (it "keeps a mark whose files kept their time and takes the table's for the rest"
    (let* ((then '(26000 1 0 0))
           (now '(26000 2 0 0))
           (before (make-hash-table))
           (info (gnus-info-make "nnmaildir+gmail:inbox" 3 '((1 . 5)) '((tick 2) (reply 3)))))
      (puthash 'read then before)
      (puthash 'tick then before)
      (puthash 'reply then before)
      (mail-load-apply-marks info
                             `(:read ((1 . 9)) :marks ((tick 7) (reply 8))
                               :mmth ((read . ,then) (tick . ,now) (reply . ,then)))
                             before)
      (expect (gnus-info-read info) :to-equal '((1 . 5)))
      (expect (alist-get 'tick (gnus-info-marks info)) :to-equal '(7))
      (expect (alist-get 'reply (gnus-info-marks info)) :to-equal '(3))))

  (it "takes every mark from the table on a group's first read"
    (let ((info (gnus-info-make "nnmaildir+gmail:inbox" 3 '((1 . 100)) '((tick 2)))))
      (mail-load-apply-marks info '(:read ((1 . 9)) :marks nil
                                    :mmth ((read 26000 1 0 0) (tick 26000 1 0 0)))
                             nil)
      (expect (gnus-info-read info) :to-equal '((1 . 9)))
      (expect (gnus-info-marks info) :to-be nil))))

(describe "mail-load-keep-newer"
  (it "keeps a flag this session saved and a message it delivered after the table"
    (let* ((dir (file-name-as-directory (make-temp-file "load-tests-dir" t)))
           (old (make-nnmaildir--grp :name "inbox" :flist (make-hash-table :test #'equal)
                                     :mlist (make-hash-table :test #'equal)))
           (group (make-nnmaildir--grp :name "inbox" :flist (make-hash-table :test #'equal)
                                       :mlist (make-hash-table :test #'equal)))
           (entries nil))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "cur" dir))
            (dolist (row '((2 "a" ":2,S" "<a>") (3 "b" ":2," "<b>") (4 "c" ":2," "<c>")))
              (push (mail-table-add-row old row) entries))
            (mail-table-finish-group old entries)
            (setq entries nil)
            (dolist (row '((2 "a" ":2," "<a>") (3 "b" ":2," "<b>")))
              (push (mail-table-add-row group row) entries))
            (mail-table-finish-group group entries)
            ;; the session read a and delivered c after the batch listed
            (dolist (name '("a:2,S" "b:2," "c:2,"))
              (write-region "" nil (expand-file-name (concat "cur/" name) dir) nil 'silent))
            (mail-load-keep-newer group old dir)
            (expect (load-tests--rows group)
                    :to-equal '((2 "a" ":2,S" "<a>") (3 "b" ":2," "<b>") (4 "c" ":2," "<c>")))
            (expect (nnmaildir--grp-count group) :to-equal 3))
        (delete-directory dir t)))))

;;; Work waiting for loads

(describe "wait-for-mail-group-a"
  (it "opens a group nnmaildir holds at once"
    (load-tests--with-state ((opened nil))
      (load-tests--server "inbox")
      (expect (wait-for-mail-group-a (lambda (group &rest _) (push group opened) 'shown)
                                     "nnmaildir+gmail:inbox")
              :to-be 'shown)
      (expect opened :to-equal '("nnmaildir+gmail:inbox"))))

  (it "loads a group nnmaildir lacks first and opens it once loaded, Gnus on screen"
    (load-tests--with-state ((gnus-group-buffer (buffer-name (generate-new-buffer " *load-tests group*")))
                             (server (load-tests--server "inbox"))
                             (asked nil)
                             (opened nil))
      (unwind-protect
          (cl-letf (((symbol-function 'load-mail-groups)
                     (lambda (groups &optional first)
                       (push (list groups first) asked)
                       (setq mail-load-queue groups)))
                    ((symbol-function 'gnus-shown-p) (lambda () t)))
            (expect (wait-for-mail-group-a (lambda (group &rest args) (push (cons group args) opened))
                                           "nnmaildir+gmail:emacs" nil t)
                    :to-be nil)
            (expect asked :to-equal '((("nnmaildir+gmail:emacs") t)))
            (expect opened :to-be nil)
            (puthash "emacs" (make-nnmaildir--grp :name "emacs") (nnmaildir--srv-groups server))
            (setq mail-load-queue nil)
            (mail-load-run-waiters)
            (expect opened :to-equal '(("nnmaildir+gmail:emacs" nil t)))
            (expect mail-load-waiters :to-be nil))
        (kill-buffer gnus-group-buffer))))

  (it "says the group is ready instead when Gnus left the screen"
    (load-tests--with-state ((gnus-group-buffer (buffer-name (generate-new-buffer " *load-tests group*")))
                             (server (load-tests--server))
                             (said nil)
                             (opened nil))
      (unwind-protect
          (cl-letf (((symbol-function 'load-mail-groups)
                     (lambda (groups &rest _) (setq mail-load-queue groups)))
                    ((symbol-function 'gnus-shown-p) #'ignore)
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
            (wait-for-mail-group-a (lambda (&rest _) (push t opened)) "nnmaildir+gmail:emacs")
            (puthash "emacs" (make-nnmaildir--grp :name "emacs") (nnmaildir--srv-groups server))
            (setq mail-load-queue nil)
            (mail-load-run-waiters))
        (kill-buffer gnus-group-buffer))
      (expect opened :to-be nil)
      (expect (car said) :to-equal "emacs is ready")))

  (it "says a group that failed to load did not"
    (load-tests--with-state ((said nil))
      (load-tests--server)
      (cl-letf (((symbol-function 'load-mail-groups) #'ignore)
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
        (wait-for-mail-group-a #'ignore "nnmaildir+gmail:emacs")
        (mail-load-run-waiters))
      (expect (car said) :to-equal "emacs did not load"))))

(describe "wait-for-mail-loads-a"
  (it "searches at once when nothing loads"
    (load-tests--with-state ()
      (expect (wait-for-mail-loads-a (lambda (&rest args) args) 'query) :to-equal '(query))))

  (it "searches once the loads are done, in the buffer it was asked from"
    (load-tests--with-state ((mail-load-running (list "nnmaildir+gmail:archive"))
                             (searched nil))
      (with-temp-buffer
        (let ((asked-in (current-buffer)))
          (expect (wait-for-mail-loads-a (lambda (query)
                                           (push (list query (current-buffer)) searched))
                                         'query)
                  :to-be nil)
          (with-temp-buffer
            (setq mail-load-running nil)
            (mail-load-run-waiters))
          (expect searched :to-equal (list (list 'query asked-in))))))))

(describe "wait-for-mail-load"
  (it "lets a second wait of the same kind replace the first"
    (load-tests--with-state ((ran nil))
      (setq mail-load-queue (list "nnmaildir+gmail:inbox"))
      (wait-for-mail-load 'open "to open a" #'mail-load-idle-for-tests (lambda () (push 'a ran)))
      (wait-for-mail-load 'open "to open b" #'mail-load-idle-for-tests (lambda () (push 'b ran)))
      (setq mail-load-queue nil)
      (mail-load-run-waiters)
      (expect ran :to-equal '(b))))

  (it "says what waits and how far the load got"
    (load-tests--with-state ((said nil))
      (let ((buffer (generate-new-buffer " *load-tests table*")))
        (with-current-buffer buffer
          (insert (make-string 100 ?x))
          (goto-char 41))
        (setq mail-load-job (make-mail-load-job :group "nnmaildir+gmail:archive" :buffer buffer))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (wait-for-mail-load 'search "for the search" #'ignore #'ignore))
        (expect said :to-equal '("Loading archive (40%) for the search..."))))))

(defun mail-load-idle-for-tests ()
  "Non-nil when no group loads."
  (not (mail-load-busy-p)))

(describe "refuse-unloaded-mail-group-a"
  (it "passes a request for a group nnmaildir holds"
    (load-tests--with-state ()
      (load-tests--server "inbox")
      (expect (refuse-unloaded-mail-group-a (lambda (&rest args) args) "inbox" "gmail" t)
              :to-equal '("inbox" "gmail" t))))

  (it "refuses a group still loading, says why and loads it first"
    (load-tests--with-state ((asked nil))
      (let ((server (load-tests--server)))
        (cl-letf (((symbol-function 'load-mail-groups)
                   (lambda (groups &optional first) (push (list groups first) asked))))
          (expect (refuse-unloaded-mail-group-a (lambda (&rest _) t) "emacs" "gmail") :to-be nil))
        (expect asked :to-equal '((("nnmaildir+gmail:emacs") t)))
        (expect (nnmaildir--srv-error server) :to-equal "emacs is still loading")
        ;; where a failed move looks for the reason
        (expect (nnheader-get-report-string 'nnmaildir) :to-equal "emacs is still loading")))))

(describe "load-all-mail-groups"
  (it "loads the inbox first, then the routine groups, the rest, and All Mail last"
    (load-tests--with-state ((gnus-activate-level 3)
                             (asked nil))
      (dolist (spec '(("nnmaildir+gmail:archive" 4) ("nnmaildir+gmail:emacs" 4)
                      ("nnmaildir+gmail:sent" 3) ("nnatom+feed:r/emacs" 4)
                      ("nnmaildir+gmail:inbox" 3)))
        (puthash (car spec) (list nil (gnus-info-make (car spec) (cadr spec) nil nil nil))
                 gnus-newsrc-hashtb))
      (let ((gnus-group-list '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs"
                               "nnmaildir+gmail:sent" "nnatom+feed:r/emacs"
                               "nnmaildir+gmail:inbox")))
        (cl-letf (((symbol-function 'load-mail-groups) (lambda (groups) (push groups asked))))
          (load-all-mail-groups)))
      (expect (car asked)
              :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:sent"
                          "nnmaildir+gmail:emacs" "nnmaildir+gmail:archive")))))

(describe "load-unloaded-mail-groups"
  (it "reads each maildir group nnmaildir lacks and nothing reads"
    (load-tests--with-state ((asked nil)
                             (mail-load-queue (list "nnmaildir+gmail:job"))
                             (gnus-group-list '("nnmaildir+gmail:inbox" "nnmaildir+gmail:job"
                                                "nnmaildir+gmail:new-label" "nnatom+feed:r/emacs")))
      (load-tests--server "inbox")
      (cl-letf (((symbol-function 'load-mail-groups) (lambda (groups) (push groups asked))))
        (load-unloaded-mail-groups))
      (expect asked :to-equal '(("nnmaildir+gmail:new-label"))))))

(describe "stop-mail-load"
  (it "drops every load, the batch and the work waiting"
    (load-tests--with-state ((mail-load-process (start-process "load-tests" nil "sleep" "10"))
                             (mail-load-queue (list "nnmaildir+gmail:inbox"))
                             (mail-load-tables (list (cons "nnmaildir+gmail:sent" "/t")))
                             (mail-load-waiters (list (list 'open "x" #'ignore #'ignore))))
      (let ((process mail-load-process))
        (stop-mail-load)
        (expect (process-live-p process) :to-be nil)
        (expect (list mail-load-process mail-load-queue mail-load-tables mail-load-waiters)
                :to-equal '(nil nil nil nil))))))

;;; load-tests.el ends here
