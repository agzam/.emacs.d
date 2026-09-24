;;; tests/scripts/mail-index-tests.el --- nnmaildir overview build specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "scripts/mail-index.el")

(defun mail-index-tests--store (&rest groups)
  "Temp maildir store with GROUPS, each (NAME . MESSAGE-COUNT); returns its path."
  (let ((store (file-name-as-directory (make-temp-file "mail-index" t))))
    (pcase-dolist (`(,name . ,count) groups)
      (dolist (sub '("cur" "new" "tmp"))
        (make-directory (expand-file-name (concat name "/" sub) store) t))
      (dotimes (i count)
        (mail-index-tests--write store name i)))
    store))

(defun mail-index-tests--write (store group i)
  "Write message I of GROUP under STORE, the odd ones read, the even ones new."
  (with-temp-file (expand-file-name
                   (if (cl-oddp i)
                       (format "cur/170000000%d.%d.fixture:2,S" i i)
                     (format "new/170000000%d.%d.fixture" i i))
                   (expand-file-name group store))
    (insert "From: Ann <ann@example.com>\n"
            "Subject: message " (number-to-string i) "\n"
            "Date: Mon, 21 Sep 2026 10:00:00 +0000\n"
            "Message-ID: <" (number-to-string i) "@fixture.example>\n\n"
            "body\n")))

(defun mail-index-tests--numbers (store group)
  "Alist of (FILE-PREFIX . ARTICLE-NUMBER) from GROUP's overview files under STORE."
  (let ((nov (expand-file-name (concat group "/.nnmaildir/nov") store)))
    (mapcar (lambda (file)
              (with-temp-buffer
                (insert-file-contents (expand-file-name file nov))
                (cons file (aref (read (current-buffer)) 1))))
            (directory-files nov nil "\\`[^.]"))))

(defun mail-index-tests--lowest (store group &optional except)
  "The (FILE-PREFIX . ARTICLE-NUMBER) with the lowest number in GROUP under STORE.
EXCEPT names a prefix to leave out: a scan does not remove the overview
of a message that is gone."
  (car (sort (seq-remove (lambda (entry) (equal (car entry) except))
                         (mail-index-tests--numbers store group))
             (lambda (a b) (< (cdr a) (cdr b))))))

(describe "mail-index-groups"
  (it "lists the maildir directories and nothing else"
    (let ((store (mail-index-tests--store '("inbox" . 0) '("trash" . 0))))
      (unwind-protect
          (progn
            (write-region "" nil (expand-file-name "README" store) nil 'silent)
            (make-directory (expand-file-name ".notmuch" store))
            (expect (mail-index-groups store) :to-equal '("inbox" "trash")))
        (delete-directory store t)))))

(describe "mail-index-build"
  (it "builds the overview every message lacks, and reports what it did"
    (let ((store (mail-index-tests--store '("inbox" . 3) '("trash" . 1))))
      (unwind-protect
          (let ((rows (mail-index-build store '("inbox" "trash") nil)))
            (expect (mapcar (lambda (row) (seq-take row 4)) rows)
                    :to-equal '(("inbox" 3 0 3) ("trash" 1 0 1)))
            (expect (numberp (nth 4 (car rows))) :to-be t)
            (expect (mail-index-overviews store "inbox") :to-equal 3))
        (delete-directory store t))))

  (it "adds only the missing overviews on a second pass"
    (let ((store (mail-index-tests--store '("inbox" . 2))))
      (unwind-protect
          (progn
            (mail-index-build store '("inbox") nil)
            (mail-index-tests--write store "inbox" 2)
            (expect (seq-take (car (mail-index-build store '("inbox") nil)) 4)
                    :to-equal '("inbox" 3 2 3)))
        (delete-directory store t))))

  (it "leaves the store alone apart from the group's own cache directory"
    (let ((store (mail-index-tests--store '("inbox" . 1))))
      (unwind-protect
          (progn
            (mail-index-build store '("inbox") nil)
            (expect (directory-files store nil "\\`[^.]") :to-equal '("inbox"))
            (expect (directory-files (expand-file-name "inbox" store) nil "\\`[^.]")
                    :to-equal '("cur" "new" "tmp")))
        (delete-directory store t)))))

(describe "mail-index-build with rebuild"
  (it "numbers the group afresh where a fill-in keeps the old numbers"
    ;; once the lowest-numbered message is gone, a fill-in keeps the
    ;; others' numbers while a rebuild starts over
    (let ((store (mail-index-tests--store '("inbox" . 3))))
      (unwind-protect
          (let ((cur (expand-file-name "inbox/cur" store)))
            (mail-index-build store '("inbox") nil)
            (pcase-let ((`(,prefix . ,first) (mail-index-tests--lowest store "inbox")))
              ;; a scan moves new/ into cur/, so every file is there now
              (delete-file (car (directory-files cur t (concat "\\`" (regexp-quote prefix)))))
              (mail-index-build store '("inbox") nil)
              (expect (cdr (mail-index-tests--lowest store "inbox" prefix))
                      :to-be-greater-than first)
              (mail-index-build store '("inbox") t)
              (expect (cdr (mail-index-tests--lowest store "inbox" prefix))
                      :to-equal first)))
        (delete-directory store t))))

  (it "keeps the marks directory"
    (let ((store (mail-index-tests--store '("inbox" . 1))))
      (unwind-protect
          (let ((tick (expand-file-name "inbox/.nnmaildir/marks/tick/" store)))
            (mail-index-build store '("inbox") nil)
            (make-directory tick t)
            (write-region "" nil (expand-file-name "1700000000.0.fixture" tick) nil 'silent)
            (expect (seq-take (car (mail-index-build store '("inbox") t)) 4)
                    :to-equal '("inbox" 1 0 1))
            (expect (file-exists-p (expand-file-name "1700000000.0.fixture" tick)) :to-be t))
        (delete-directory store t)))))

;;; mail-index-tests.el ends here
