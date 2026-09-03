;;; modules/papers/autoload/books.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Turning the book corpus into bib entries.  No API is involved, unlike
;; the paper import: books print ISBNs rather than DOIs, and their
;; filenames already read as metadata - `Alex Xu - System Design
;; Interview', `Halloway S., Bedra A. - Programming Clojure, 2nd edition
;; - 2012'.  Paper filenames do not, which is why the two corpora take
;; different routes.
;;
;; citar needs a title and a `file' field to find and open a work.  Author
;; and year are parsed only where the filename spells them plainly and
;; left out otherwise, because a wrong author in the picker is worse than
;; a blank column.
;;; Code:

(require 'seq)
(require 'subr-x)

(defvar books-bibliography)
(defvar books-directory)

(declare-function bibliography-index "import")
(declare-function bibliography-index-add "import")
(declare-function append-bibliography-entries "import")
(declare-function unique-citekey "import")
(declare-function bibtex-safe-value "import")
(declare-function bibtex-title-key "import")

(defconst book-document-extensions '("pdf" "epub" "mobi" "fb2" "djvu")
  "Document extensions that count as a work, in order of preference.
PDF first because pdf-tools restores the reading position and org-noter
anchors to it; EPUB next, which nov opens.")

(defconst book-walk-depth 4
  "How many directory levels below `books-directory' hold works.
The bound is the point of it: one textbook ships its per-exercise
solutions as 4764 PDFs nested five deep, and an unbounded walk would
read that dump as 4764 books.")

(defconst book-name-particles
  '("van" "de" "der" "den" "von" "la" "le" "di" "del" "da" "dos" "el")
  "Lowercase particles that may sit inside a family name.")

(defun book-documents (directory &optional depth)
  "Return the document files under DIRECTORY, at most DEPTH levels down.
DEPTH defaults to `book-walk-depth'.  Dot directories are skipped, which
keeps Resilio's own `.sync' bookkeeping out of the corpus."
  (let ((depth (or depth book-walk-depth))
        files)
    (dolist (entry (ignore-errors (directory-files directory t)))
      (let ((name (file-name-nondirectory entry)))
        (unless (string-prefix-p "." name)
          (cond
           ((file-directory-p entry)
            (when (< 1 depth)
              (setq files (nconc files (book-documents entry (1- depth))))))
           ((member (downcase (or (file-name-extension entry) ""))
                    book-document-extensions)
            (setq files (nconc files (list entry))))))))
    files))

(defun book-extension-rank (file)
  "Return how preferred FILE's extension is; lower sorts first."
  (or (seq-position book-document-extensions
                    (downcase (or (file-name-extension file) "")))
      (length book-document-extensions)))

(defun book-preferred-file (files)
  "Return the document to open from FILES, which are one work's formats."
  (car (sort (copy-sequence files)
             (lambda (a b) (< (book-extension-rank a) (book-extension-rank b))))))

;;;###autoload
(defun book-works (&optional directory)
  "Return the works under DIRECTORY as plists of :name and :file.
DIRECTORY defaults to `books-directory'.  Works are grouped by filename
stem, which spares the walk from having to tell a shelf directory from a
book directory: a book shipped as PDF and EPUB is one stem, and a shelf
holds many.  :name is the stem the metadata is read from."
  (let ((table (make-hash-table :test 'equal))
        works)
    (dolist (file (book-documents (or directory books-directory)))
      (push file (gethash (file-name-base file) table)))
    (maphash (lambda (stem files)
               (push (list :name stem :file (book-preferred-file files)) works))
             table)
    (sort works (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defun book-clean-name (name)
  "Return NAME as readable text.
Underscores become spaces and bracketed release tags go, since
`[Epub][Spanish][www.lokotorrents.com]' says nothing about the work."
  (string-trim
   (replace-regexp-in-string
    "[ \t]+" " "
    (replace-regexp-in-string
     "\\[[^]]*\\]" " "
     (replace-regexp-in-string "_" " " name)))))

(defun book-year (name)
  "Return the publication year printed in NAME, or nil.
The last standalone 19xx or 20xx wins: `Linux Bible 9th Ed (2015)'
prints an edition number first, and `Rosen K ... 7ed 2012' prints two."
  (let ((start 0) year)
    (while (string-match "\\(?:19\\|20\\)[0-9][0-9]" name start)
      (let* ((from (match-beginning 0))
             (to (match-end 0))
             (before (if (< 0 from) (aref name (1- from)) ?\s))
             (after (if (< to (length name)) (aref name to) ?\s)))
        (unless (or (<= ?0 before ?9) (<= ?0 after ?9))
          (setq year (substring name from to))))
      (setq start (match-end 0)))
    year))

(defun book-name-like-p (segment)
  "Return non-nil when SEGMENT reads as a list of personal names.
Every word must be capitalised, an initial or a particle.  That is what
separates `Halloway S., Bedra A.' from `Programming Clojure, 2nd
edition', which a comma alone does not."
  (let ((case-fold-search nil)
        (words (split-string segment "[ \t,]+" t)))
    (and words
         (<= (length words) 6)
         (seq-every-p (lambda (word)
                        (or (member (downcase word) book-name-particles)
                            (string-match-p "\\`[[:upper:]]" word)))
                      words))))

(defun book-clean-author (segment)
  "Return SEGMENT without the trailing punctuation and year filenames add.
`by Nick Qi Zhu.2013' names an author and a year with no separator."
  (string-trim
   (replace-regexp-in-string "[ \t.,;-]*\\(?:19\\|20\\)[0-9][0-9]\\'" ""
                             (string-trim segment))
   "[ \t]*" "[ \t,;-]*"))

(defun book-split-author (name)
  "Return (AUTHOR . TITLE) read from NAME; AUTHOR is nil unless plain.
Only two spellings are trusted.  A leading segment before ` - ' counts
when it carries an initial or a comma and reads as names, because `A
Mind for Numbers - How to Excel at Math and Science' has the same shape
as `Alex Xu - System Design Interview' and no amount of dash-counting
tells them apart.  A trailing `by NAME' counts on the same name test."
  (let* ((case-fold-search nil)
         (dash (when (string-match "\\`\\(.+?\\) +- +\\(.+\\)\\'" name)
                 (let ((head (match-string 1 name))
                       (tail (match-string 2 name)))
                   (cons (book-clean-author head) (string-trim tail)))))
         (by (when (string-match "\\`\\(.+?\\) +by +\\(.+\\)\\'" name)
               (let ((head (match-string 1 name))
                     (tail (match-string 2 name)))
                 (cons (book-clean-author tail) (string-trim head))))))
    (cond
     ((and dash
           (string-match-p "\\(?:[[:upper:]]\\.\\|,\\)" (car dash))
           (book-name-like-p (car dash)))
      dash)
     ((and by (book-name-like-p (car by))) by)
     (t (cons nil name)))))

(defun book-strip-year (title year)
  "Return TITLE without a trailing YEAR, keeping it when nothing is left."
  (if (and year
           (string-match (concat "\\(?: +- +\\| +(\\)" (regexp-quote year) ")?[ \t]*\\'")
                         title)
           (< 0 (match-beginning 0)))
      (string-trim (substring title 0 (match-beginning 0)))
    title))

;;;###autoload
(defun book-metadata (name)
  "Return (:title :author :year) parsed from the filename stem NAME."
  (let* ((clean (book-clean-name name))
         (split (book-split-author clean))
         (year (book-year clean))
         (title (book-strip-year (cdr split) year)))
    (list :title (if (string-empty-p title) clean title)
          :author (car split)
          :year year)))

(defun book-author-family (author)
  "Return the citekey stem for AUTHOR: one family name, letters only.
Initials go first, because `Knuth D.E.' would otherwise end in `de'.
A comma means the name is written family-first, so the first word wins;
without one the last word does."
  (let* ((comma (string-match-p "," author))
         (words (seq-remove
                 (lambda (word)
                   (string-match-p "\\`[[:upper:]]\\(?:\\.[[:upper:]]?\\)*\\.?\\'" word))
                 (split-string (replace-regexp-in-string "," " " author) "[ \t]+" t)))
         (family (if comma (car words) (car (last words)))))
    (when family
      (let ((stem (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "" family))))
        (unless (string-empty-p stem) stem)))))

;;;###autoload
(defun book-citekey (metadata)
  "Return the citekey for METADATA: `family_year', else a slug of the title."
  (let* ((author (plist-get metadata :author))
         (year (plist-get metadata :year))
         (family (when author (book-author-family author))))
    (cond ((and family year) (concat family "_" year))
          (family family)
          (t (or (bibtex-title-key (plist-get metadata :title)) "book")))))

;;;###autoload
(defun book-bibtex-entry (work key)
  "Return the BibTeX entry for WORK under citekey KEY."
  (let* ((metadata (book-metadata (plist-get work :name)))
         (author (plist-get metadata :author))
         (year (plist-get metadata :year)))
    (concat (format "@book{%s,\n  title = {%s}" key
                    (bibtex-safe-value (plist-get metadata :title)))
            (when author (format ",\n  author = {%s}" (bibtex-safe-value author)))
            (when year (format ",\n  year = {%s}" year))
            (format ",\n  file = {%s}\n}" (expand-file-name (plist-get work :file))))))

;;;###autoload
(defun import-books (&optional directory)
  "Write an entry into `books-bibliography' for every unrecorded work.
DIRECTORY defaults to `books-directory'.  Nothing is confirmed and
nothing is fetched: the metadata is the filename, so the run is
repeatable and a work already recorded is passed over."
  (interactive)
  (let* ((index (bibliography-index books-bibliography))
         (works (seq-remove
                 (lambda (work)
                   (member (expand-file-name (plist-get work :file))
                           (plist-get index :files)))
                 (book-works directory)))
         entries)
    (dolist (work works)
      (let* ((key (unique-citekey (book-citekey (book-metadata (plist-get work :name)))
                                  (plist-get index :keys)))
             (entry (book-bibtex-entry work key)))
        (push entry entries)
        (setq index (bibliography-index-add index entry))))
    (let ((ordered (nreverse entries)))
      (append-bibliography-entries ordered books-bibliography)
      (message "Added %d book%s to %s" (length ordered)
               (if (= 1 (length ordered)) "" "s")
               (file-name-nondirectory books-bibliography)))))

;;; books.el ends here
