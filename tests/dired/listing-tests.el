;;; tests/dired/listing-tests.el --- dired/autoload/listing.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)
(require 'dired)

(load-module-file "modules/dired/autoload/listing.el")

;; Canned `ls -l' text rather than a real listing: the batch tier can't count
;; on GNU ls, and the hook only ever reads what dired put in the buffer.

(defun listing-tests--header (dir)
  (format "  %s:\n  total used in directory 12 available 100\n" dir))

(defun listing-tests--entry (name &optional dirp)
  (format "  %s  3 ryl staff  96 Jun  2 09:00 %s\n"
          (if dirp "drwxr-xr-x" "-rw-r--r--") name))

(defun listing-tests--names (text)
  "Names left in TEXT, in order, after the hook has run over it."
  (with-temp-buffer
    (setq-local dired-actual-switches "-aBhlt --group-directories-first")
    (insert text)
    (dired-dot-entries-first-h)
    (goto-char (point-min))
    (let (names)
      (while (not (eobp))
        (when (dired-move-to-filename)
          (push (dired-get-filename 'no-dir t) names))
        (forward-line 1))
      (nreverse names))))

(describe "dired-dot-entries-first-h"
  (it "lifts . and .. above a time-sorted listing"
    (expect (listing-tests--names
             (concat (listing-tests--header "/tmp/demo")
                     (listing-tests--entry "newdir" t)
                     (listing-tests--entry "." t)
                     (listing-tests--entry ".." t)
                     (listing-tests--entry "olddir" t)
                     (listing-tests--entry "newfile")))
            :to-equal '("." ".." "newdir" "olddir" "newfile")))

  (it "puts . before .. whatever their order in the listing"
    (expect (listing-tests--names
             (concat (listing-tests--header "/tmp/demo")
                     (listing-tests--entry ".." t)
                     (listing-tests--entry "dir" t)
                     (listing-tests--entry "." t)))
            :to-equal '("." ".." "dir")))

  (it "keeps the other entries in their sorted order"
    (expect (listing-tests--names
             (concat (listing-tests--header "/tmp/demo")
                     (listing-tests--entry "c" t)
                     (listing-tests--entry "." t)
                     (listing-tests--entry "a")
                     (listing-tests--entry "b")))
            :to-equal '("." "c" "a" "b")))

  (it "leaves a listing without dot entries alone"
    (expect (listing-tests--names
             (concat (listing-tests--header "/tmp/demo")
                     (listing-tests--entry "dir" t)
                     (listing-tests--entry "file")))
            :to-equal '("dir" "file")))

  (it "tolerates an empty listing"
    (expect (listing-tests--names (listing-tests--header "/tmp/demo"))
            :to-equal nil))

  (it "hoists only within the first listing, so -R subdirs keep theirs"
    (expect (listing-tests--names
             (concat (listing-tests--header "/tmp/demo")
                     (listing-tests--entry "sub" t)
                     (listing-tests--entry "." t)
                     (listing-tests--entry ".." t)
                     "\n"
                     (listing-tests--header "/tmp/demo/sub")
                     (listing-tests--entry "inner")
                     (listing-tests--entry "." t)
                     (listing-tests--entry ".." t)))
            :to-equal '("." ".." "sub" "inner" "." ".."))))

(describe "the dired module"
  (it "registers the hoist on dired-after-readin-hook"
    (expect
     (with-temp-buffer
       (insert-file-contents (expand-file-name "modules/dired/config.el"
                                               test-config-root))
       (goto-char (point-min))
       (and (re-search-forward
             (rx "(add-hook 'dired-after-readin-hook #'dired-dot-entries-first-h)")
             nil t)
            t))
     :to-be t))

  (it "registers the subtree reader as an override advice"
    (expect
     (with-temp-buffer
       (insert-file-contents (expand-file-name "modules/dired/config.el"
                                               test-config-root))
       (goto-char (point-min))
       (and (re-search-forward
             (rx "(advice-add 'dired-subtree--readin :override"
                 (+ space) "#'dired-subtree-readin-a)")
             nil t)
            t))
     :to-be t)))

(defun listing-tests--readin (total &rest entries)
  "What `dired-subtree-readin-a' makes of `ls' printing TOTAL, then ENTRIES."
  (cl-letf (((symbol-function 'insert-directory)
             (lambda (&rest _)
               (insert total)
               (dolist (entry entries)
                 (insert (substring entry 2))))))
    (dired-subtree-readin-a "/tmp/demo/")))

(defun listing-tests--subtree (&rest entries)
  "ENTRIES the way dired-subtree splices them in."
  (string-trim-right (apply #'concat entries) "\n"))

(describe "dired-subtree-readin-a"
  ;; `ls' prints a total line, then unindented entries; dired-subtree splices
  ;; the result in as-is, so it carries the indent and no final newline.
  (it "keeps a lone child listed between \"..\" and \".\""
    (expect (listing-tests--readin "total 4\n"
                                   (listing-tests--entry ".." t)
                                   (listing-tests--entry "only" t)
                                   (listing-tests--entry "." t))
            :to-equal (listing-tests--subtree (listing-tests--entry "only" t))))

  (it "keeps the entry that follows a leading \".\""
    (expect (listing-tests--readin "total 4\n"
                                   (listing-tests--entry "." t)
                                   (listing-tests--entry "sub" t)
                                   (listing-tests--entry ".." t)
                                   (listing-tests--entry "old.txt"))
            :to-equal (listing-tests--subtree (listing-tests--entry "sub" t)
                                              (listing-tests--entry "old.txt"))))

  (it "keeps a first entry whose name ends in a dot"
    (expect (listing-tests--readin "total 4\n"
                                   (listing-tests--entry "v1." t)
                                   (listing-tests--entry "." t)
                                   (listing-tests--entry ".." t))
            :to-equal (listing-tests--subtree (listing-tests--entry "v1." t))))

  (it "drops a total line printed in another language"
    (expect (listing-tests--readin "insgesamt 4\n"
                                   (listing-tests--entry "dir" t))
            :to-equal (listing-tests--subtree (listing-tests--entry "dir" t))))

  (it "drops both dot entries from the middle of the listing"
    (expect (listing-tests--readin "total used in directory 12 available 100\n"
                                   (listing-tests--entry "sub" t)
                                   (listing-tests--entry "." t)
                                   (listing-tests--entry ".." t)
                                   (listing-tests--entry "old.txt"))
            :to-equal (listing-tests--subtree (listing-tests--entry "sub" t)
                                              (listing-tests--entry "old.txt"))))

  (it "keeps a listing without dot entries whole"
    (expect (listing-tests--readin "total 4\n"
                                   (listing-tests--entry "sub" t)
                                   (listing-tests--entry "old.txt"))
            :to-equal (listing-tests--subtree (listing-tests--entry "sub" t)
                                              (listing-tests--entry "old.txt"))))

  (it "leaves no trailing newline when a dot entry ends the listing"
    (expect (listing-tests--readin "total 4\n"
                                   (listing-tests--entry "old.txt")
                                   (listing-tests--entry "." t)
                                   (listing-tests--entry ".." t))
            :to-equal (listing-tests--subtree (listing-tests--entry "old.txt"))))

  (it "yields an empty listing for a directory holding only dot entries"
    (expect (listing-tests--readin "total 0\n"
                                   (listing-tests--entry "." t)
                                   (listing-tests--entry ".." t))
            :to-equal "")))
