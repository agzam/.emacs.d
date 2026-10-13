;;; tests/scripts/mail-load-tests.el --- batch group reading specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "scripts/mail-load.el")

(defconst mail-load-tests--group "nnmaildir+gmail:inbox")

(defun mail-load-tests--store (count)
  "Temp maildir store whose inbox holds COUNT messages; returns its path.
The odd ones are read in cur/, the even ones new in new/."
  (let ((store (file-name-as-directory (make-temp-file "mail-load" t))))
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name (concat "inbox/" sub) store) t))
    (dotimes (i count)
      (mail-load-tests--write store i))
    store))

(defun mail-load-tests--write (store i &optional flags)
  "Write message I into STORE's inbox, in cur/ with FLAGS, or in new/ by I."
  (with-temp-file (expand-file-name
                   (cond (flags (format "inbox/cur/170000000%d.%d.fixture:2,%s" i i flags))
                         ((cl-oddp i) (format "inbox/cur/170000000%d.%d.fixture:2,S" i i))
                         (t (format "inbox/new/170000000%d.%d.fixture" i i)))
                   store)
    (insert "From: Ann <ann@example.com>\n"
            "Subject: message " (number-to-string i) "\n"
            "Date: Mon, 21 Sep 2026 10:00:00 +0000\n"
            "Message-ID: <" (number-to-string i) "@fixture.example>\n\n"
            "body\n")))

(defun mail-load-tests--cur (store i)
  "Current file of message I in STORE's inbox."
  (car (directory-files (expand-file-name "inbox/cur" store) t
                        (format "\\`170000000%d\\.%d\\." i i))))

(defmacro mail-load-tests--with-store (bindings &rest body)
  "Run BODY with BINDINGS, then drop the store, the tables and nnmaildir's servers.
BINDINGS binds STORE and TABLES among others."
  (declare (indent 1))
  `(let* ((gnus-newsrc-hashtb (gnus-make-hashtable))
          (gnus-verbose 0)
          (nnmaildir--servers nil)
          (nnmaildir--cur-server nil)
          ,@bindings)
     (unwind-protect (progn ,@body)
       (delete-directory store t)
       (delete-directory tables t))))

(defun mail-load-tests--read (tables)
  "The inbox table under TABLES as (HEADER . ROWS)."
  (with-temp-buffer
    (insert-file-contents-literally (mail-table-file tables mail-load-tests--group))
    (goto-char (point-min))
    (let ((header (read (current-buffer))) rows)
      (while (progn (skip-chars-forward "\n") (not (eobp)))
        (push (read (current-buffer)) rows))
      (cons header (nreverse rows)))))

(defun mail-load-tests--forget ()
  "Drop every server the batch opened, as a fresh batch Emacs starts."
  (setq nnmaildir--servers nil
        nnmaildir--cur-server nil))

(describe "mail-table-group-parts"
  (it "splits a group into its server and name"
    (expect (mail-table-group-parts "nnmaildir+news:gmane.emacs.devel")
            :to-equal '("news" . "gmane.emacs.devel")))
  (it "refuses a group of another back end"
    (expect (mail-table-group-parts "nntp+news.gmane.io:gmane.emacs.devel") :to-throw)))

(describe "mail-load-group"
  (it "saves each message's number, file name, flags and Message-ID, lowest number first"
    (mail-load-tests--with-store ((store (mail-load-tests--store 3))
                                  (tables (make-temp-file "mail-tables" t)))
      (let* ((file (mail-load-group tables mail-load-tests--group store nil))
             (table (mail-load-tests--read tables))
             (rows (cdr table)))
        (expect file :to-equal (mail-table-file tables mail-load-tests--group))
        (expect (length (seq-uniq (mapcar #'car rows))) :to-equal 3)
        (expect (mapcar #'car rows) :to-equal (sort (mapcar #'car rows) #'<))
        ;; the scan moved new/ into cur/, and each name is the file's now
        (expect (sort (mapcar (lambda (row) (concat (nth 1 row) (nth 2 row))) rows) #'string<)
                :to-equal (directory-files (expand-file-name "inbox/cur" store) nil "\\`[^.]"))
        (expect (sort (mapcar (lambda (row) (nth 3 row)) rows) #'string<)
                :to-equal '("<0@fixture.example>" "<1@fixture.example>" "<2@fixture.example>"))
        (expect (plist-get (car table) :count) :to-equal 3))))

  (it "reads the read ranges and the stars from the flags"
    (mail-load-tests--with-store ((store (mail-load-tests--store 0))
                                  (tables (make-temp-file "mail-tables" t)))
      (mail-load-tests--write store 1 "S")
      (mail-load-tests--write store 2 "F")
      (mail-load-tests--write store 3 "FS")
      (mail-load-group tables mail-load-tests--group store nil)
      (pcase-let* ((`(,header . ,rows) (mail-load-tests--read tables))
                   (number (lambda (i)
                             (car (seq-find (lambda (row)
                                              (string-prefix-p (format "170000000%d." i)
                                                               (nth 1 row)))
                                            rows)))))
        ;; numbers no message holds count as read too, as nnmaildir has it
        (expect (seq-intersection (range-uncompress (plist-get header :read))
                                  (mapcar #'car rows))
                :to-have-same-items-as (list (funcall number 1) (funcall number 3)))
        (expect (range-uncompress (alist-get 'tick (plist-get header :marks)))
                :to-have-same-items-as (list (funcall number 2) (funcall number 3)))
        ;; each mark's time, so the session can tell which ones changed
        (expect (assq 'read (plist-get header :mmth)) :not :to-be nil))))

  (it "answers nil when nothing changed since its table, unless forced"
    (mail-load-tests--with-store ((store (mail-load-tests--store 2))
                                  (tables (make-temp-file "mail-tables" t)))
      (mail-load-group tables mail-load-tests--group store nil)
      (mail-load-tests--forget)
      (expect (mail-load-group tables mail-load-tests--group store nil) :to-be nil)
      (mail-load-tests--forget)
      (expect (mail-load-group tables mail-load-tests--group store t)
              :to-equal (mail-table-file tables mail-load-tests--group))))

  (it "starts from its table: a new message joins, a gone one leaves, a renamed one keeps its number"
    (mail-load-tests--with-store ((store (mail-load-tests--store 3))
                                  (tables (make-temp-file "mail-tables" t)))
      (mail-load-group tables mail-load-tests--group store nil)
      (let ((before (cdr (mail-load-tests--read tables)))
            (starred (mail-load-tests--cur store 1)))
        (mail-load-tests--forget)
        (delete-file (mail-load-tests--cur store 0))
        (rename-file starred (concat (substring starred 0 -1) "FS"))
        (mail-load-tests--write store 4)
        ;; a table read afresh would not see these as changes at all
        (spy-on 'nnmaildir--update-nov :and-call-through)
        (expect (mail-load-group tables mail-load-tests--group store nil) :not :to-be nil)
        (let ((after (cdr (mail-load-tests--read tables))))
          ;; only the new message's overview is read
          (expect 'nnmaildir--update-nov :to-have-been-called-times 1)
          (expect (mapcar (lambda (row) (nth 3 row)) after)
                  :to-equal '("<1@fixture.example>" "<2@fixture.example>" "<4@fixture.example>"))
          (expect (car (nth 0 after)) :to-equal
                  (car (seq-find (lambda (row) (equal (nth 3 row) "<1@fixture.example>")) before)))
          (expect (nth 2 (nth 0 after)) :to-equal ":2,FS")))))

  (it "trusts no table from before a rebuild renumbered the group"
    (mail-load-tests--with-store ((store (mail-load-tests--store 2))
                                  (tables (make-temp-file "mail-tables" t)))
      (mail-load-group tables mail-load-tests--group store nil)
      (let ((numdir (expand-file-name "inbox/.nnmaildir/num/" store)))
        (expect (mail-load-read-table (mail-table-file tables mail-load-tests--group) numdir)
                :not :to-be nil)
        (rename-file numdir (expand-file-name "inbox/.nnmaildir/num-old" store))
        (make-directory numdir)
        (expect (mail-load-read-table (mail-table-file tables mail-load-tests--group) numdir)
                :to-be nil)))))

(describe "mail-load-main"
  (it "reports each group as it lands, and a failed one without stopping"
    (let* ((store (mail-load-tests--store 1))
           (tables (make-temp-file "mail-tables" t))
           (sandbox (make-temp-file "mail-load-sandbox" t))
           (script (expand-file-name "scripts/mail-load.el" test-config-root))
           (specs `(("nnmaildir+gmail:missing" ,store nil)
                    (,mail-load-tests--group ,store nil)))
           (status nil))
      (unwind-protect
          (with-temp-buffer
            (setq status
                  (call-process (expand-file-name invocation-name invocation-directory)
                                nil t nil "-Q" "--batch" "--init-directory" sandbox
                                "-l" script
                                "--eval" (format "(mail-load-main %S '%S)" tables specs)))
            (expect status :to-equal 1)
            (expect (buffer-string) :to-match
                    "^(mail-load failed \"nnmaildir\\+gmail:missing\" \"[^\"]+\")$")
            (expect (buffer-string) :to-match
                    (concat "^" (regexp-quote
                                 (format "(mail-load loaded %S %S)" mail-load-tests--group
                                         (mail-table-file tables mail-load-tests--group)))
                            "$")))
        (delete-directory store t)
        (delete-directory tables t)
        (delete-directory sandbox t)))))

;;; mail-load-tests.el ends here
