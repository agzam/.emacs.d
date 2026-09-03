;;; modules/papers/autoload/identify.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Naming the papers that print no identifier.  Most of this corpus is
;; pre-DOI computer science, so `import-paper' has nothing to resolve and
;; the filenames are no help either - `committees.pdf', `CMU-CS-95-113.pdf'
;; and `83-Article Text-110-1-10-20191031.pdf' name three real papers.
;;
;; Page text through an LLM is what is left, and it is the right reader
;; rather than a convenient one.  A search API answers confidently when
;; wrong: Crossref on raw page text scored 2 of 6 here, and the filename as
;; a query scored 0 of 8.  A model recognises `Hoare78.pdf' from the
;; journal banner it stumbles on, and recovers `wadler88-adhoc
;; polymorphism.pdf', whose text layer extracts as replacement glyphs.
;;
;; No DOI is asked for and no API is consulted afterwards.  The bar is a
;; title and a `file' field, which is what the picker searches and opens;
;; an invented DOI would be worse than none.  Every paper gets an entry,
;; falling back to its filename, because a paper missing from the picker
;; is the one failure this pass exists to prevent.
;;
;; Nothing is written until the whole run is reviewed: the proposals land
;; in one buffer, editable, and only what is left there is appended.
;;; Code:

(require 'bibtex)
(require 'seq)
(require 'subr-x)

(defvar papers-bibliography)
(defvar papers-directory)
(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-use-tools)

(declare-function paper-text "import")
(declare-function bibliography-index "import")
(declare-function bibliography-index-add "import")
(declare-function prepared-bibliography-entry "import")
(declare-function append-bibliography-entries "import")
(declare-function bibtex-safe-value "import")
(declare-function gptel-request "gptel")

(defvar paper-identification-model 'claude-sonnet-5
  "Model that names the papers, overriding `gptel-model' for the run.
Both configured cloud backends serve it.  The task is recall over old
computer science rather than reasoning, and there are 54 of them.")

(defconst paper-identification-text-limit 3000
  "How much page text to send.  A title, its authors and an abstract fit.")

(defconst paper-identification-directive
  "You identify academic papers from the text of their first pages.

Reply with exactly these five lines and nothing else:

title: <the paper's own title>
authors: <BibTeX author field: Family, Given and Family, Given>
year: <four digits>
venue: <the journal or conference it appeared in>
identified: <yes or no>

Keep every line, leaving a value empty when you do not know that field.
Answer `identified: no' when you are not sure which paper this is; a
wrong entry is worse than an unnamed one.

The title is the paper's own, not the running header, the journal banner
or the editors' names printed above it.  Some of these files have a
broken text layer and arrive as garbled glyphs: recover the paper from
the filename and any readable fragment, or answer `identified: no'.
Never guess a DOI and never add fields."
  "System prompt for the identification request.")

(defun paper-prompt-text (text)
  "Return TEXT trimmed to what is worth sending.
Blank runs collapse and the tail goes: the answer is on the title page,
and the rest is paid for in tokens."
  (let ((squeezed (string-trim
                   (replace-regexp-in-string
                    "\n\\{3,\\}" "\n\n"
                    (replace-regexp-in-string "[ \t]+\n" "\n" (or text ""))))))
    (if (< paper-identification-text-limit (length squeezed))
        (substring squeezed 0 paper-identification-text-limit)
      squeezed)))

(defun paper-identification-prompt (file text)
  "Return the prompt naming FILE and quoting its page TEXT.
The filename is sent as well as the text, and carries the answer on its
own for `wadler88-adhoc polymorphism.pdf'."
  (format "Filename: %s\n\nFirst pages:\n%s"
          (file-name-nondirectory file)
          (paper-prompt-text text)))

(defun paper-field-value (value)
  "Return VALUE as a BibTeX field value, or nil when it says nothing.
A model asked for a field it does not know answers in words as often as
it leaves the line empty."
  (when (stringp value)
    (let ((clean (bibtex-safe-value
                  (string-trim (replace-regexp-in-string "[*_`]" "" value)))))
      (unless (or (string-empty-p clean)
                  (string-match-p "\\`\\(?:unknown\\|none\\|n/a\\|-+\\)\\'"
                                  (downcase clean)))
        clean))))

(defun paper-identification-parse (response)
  "Return the metadata plist RESPONSE spells out, or nil.
Lines are read by their key, so a model that reorders them, wraps them in
markdown or adds a sentence of its own still parses."
  (when (stringp response)
    (let ((case-fold-search t)
          title author year venue identified)
      (dolist (line (split-string response "\n" t))
        (when (string-match
               (rx bos (* (any " \t*#>-"))
                   (group (or "title" "authors" "author" "year" "venue" "identified"))
                   (* (any " \t*")) ":" (* (any " \t"))
                   (group (* nonl)) eos)
               line)
          (let ((key (downcase (match-string 1 line)))
                (value (match-string 2 line)))
            (pcase key
              ("title" (setq title (paper-field-value value)))
              ((or "authors" "author") (setq author (paper-field-value value)))
              ("year" (setq year (when-let* ((v (paper-field-value value))
                                             ((string-match "[0-9]\\{4\\}" v)))
                                   (match-string 0 v))))
              ("venue" (setq venue (paper-field-value value)))
              ("identified" (setq identified
                                  (and (string-match-p "\\`\\(?:yes\\|true\\)"
                                                       (downcase (string-trim value)))
                                       t)))))))
      (list :title title :author author :year year :venue venue
            :identified (and identified title t)))))

(defun paper-fallback-title (file)
  "Return a title for FILE read from its name.
Only reached when the model returns no title at all.  The name is a poor
title - that is why this pass exists - but it keeps the paper in the
picker, where a missing entry would not be."
  (let ((name (string-trim
               (replace-regexp-in-string
                "[ \t]+" " "
                (replace-regexp-in-string
                 "\\[[^]]*\\]" " "
                 (replace-regexp-in-string "[_-]+" " " (file-name-base file)))))))
    (if (string-empty-p name) (file-name-base file) name)))

(defun paper-bibtex-entry (metadata)
  "Return a BibTeX entry for METADATA, keyed `paper'.
`prepared-bibliography-entry' replaces that key with one built from the
author and year, so the entries this pass writes are keyed like the ones
the identifier path writes."
  (let ((title (or (paper-field-value (plist-get metadata :title)) ""))
        (author (paper-field-value (plist-get metadata :author)))
        (year (paper-field-value (plist-get metadata :year)))
        (venue (paper-field-value (plist-get metadata :venue))))
    (concat (format "@%s{paper,\n  title = {%s}" (if venue "article" "misc") title)
            (when author (format ",\n  author = {%s}" author))
            (when year (format ",\n  year = {%s}" year))
            (when venue (format ",\n  journal = {%s}" venue))
            "\n}")))

(defun paper-identification-entry (metadata file index)
  "Return the entry to propose for FILE, keyed free of INDEX.
A title the model read off the page is kept even when it says it could
not identify the paper: reading a title is the easier half, and the
review buffer marks those entries for a second look."
  (let ((title (paper-field-value (plist-get metadata :title))))
    (prepared-bibliography-entry
     (paper-bibtex-entry (if title
                             metadata
                           (list :title (paper-fallback-title file))))
     file index)))

;;;###autoload
(defun papers-without-entry (&optional directory)
  "Return the PDFs in DIRECTORY that no entry in the bibliography points at.
DIRECTORY defaults to `papers-directory'."
  (let ((files (plist-get (bibliography-index) :files)))
    (seq-remove (lambda (pdf) (member (expand-file-name pdf) files))
                (directory-files (or directory papers-directory) t "\\.pdf\\'"))))

(defun paper-identification-request (file callback)
  "Ask the model to name FILE, then call CALLBACK once with the metadata plist.
CALLBACK receives nil when the request fails or is aborted, which the
fallback title covers.  It runs once per request whatever else arrives:
a reasoning block, a stream chunk and a final response all reach a gptel
callback, and continuing the walk on each of them forks it.

The request buffer holds the settings rather than a `let' around the
call, because the request outlives the call and reads them back from the
buffer it was sent in.  Streaming off makes a string response a whole
one, and tools off keeps a tool call from answering in place of the
model."
  (unless (fboundp 'gptel-request)
    (user-error "gptel is not available"))
  (with-current-buffer (get-buffer-create " *paper identification*")
    (setq-local gptel-model paper-identification-model)
    (setq-local gptel-stream nil)
    (setq-local gptel-use-tools nil)
    (let (answered)
      (gptel-request (paper-identification-prompt file (ignore-errors (paper-text file)))
        :stream nil
        :system paper-identification-directive
        :callback (lambda (response _info)
                    (when (and (not answered)
                               (or (stringp response) (memq response '(nil abort))))
                      (setq answered t)
                      (funcall callback (and (stringp response)
                                             (paper-identification-parse response)))))))))

(defun identify-papers-step (files index proposals total)
  "Name the first of FILES, then the rest, collecting PROPOSALS.
INDEX carries the citekeys taken so far, TOTAL only the progress count.
The walk is sequential: each answer arrives before the next question is
asked, so the keys stay unique and the backend sees one request at a
time."
  (if (null files)
      ;; `reverse', not `nreverse': a second answer to one request would
      ;; leave two walks sharing these conses, and reversing them in place
      ;; corrupts the list the other one is still building.
      (paper-review-show (reverse proposals))
    (let ((file (car files)))
      (message "Identifying %d/%d: %s" (1+ (- total (length files))) total
               (file-name-nondirectory file))
      (paper-identification-request
       file
       (lambda (metadata)
         (let ((entry (paper-identification-entry metadata file index)))
           (identify-papers-step
            (cdr files)
            (bibliography-index-add index entry)
            (cons (list :file file :entry entry
                        :identified (plist-get metadata :identified))
                  proposals)
            total)))))))

;;;###autoload
(defun identify-papers (&optional directory)
  "Propose an entry for every paper in DIRECTORY that has none.
DIRECTORY defaults to `papers-directory'.  The run is asynchronous and
Emacs stays usable; the proposals appear in a review buffer at the end,
and nothing is written before that buffer is accepted."
  (interactive)
  (let ((files (papers-without-entry directory)))
    (unless files
      (user-error "Every paper already has an entry"))
    (message "Identifying %d papers with %s" (length files) paper-identification-model)
    (identify-papers-step files (bibliography-index) nil (length files))))

(defconst paper-review-syntax
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "." table)
    table)
  "Braces as the only delimiters, so an entry is one sexp.
A title may carry an apostrophe or a quote, which would otherwise open a
string and swallow the rest of the buffer.")

(defun paper-review-entries ()
  "Return the entries the current buffer holds, as written.
The buffer is read back rather than the proposals being remembered: the
point of showing it is that entries can be edited and deleted there."
  (save-excursion
    (with-syntax-table paper-review-syntax
      (goto-char (point-min))
      (let (entries)
        (while (re-search-forward "^@[A-Za-z]+{" nil t)
          (let ((start (match-beginning 0)))
            (goto-char (1- (point)))
            (condition-case nil
                (progn (forward-sexp)
                       (push (buffer-substring-no-properties start (point)) entries))
              (scan-error (goto-char (point-max))))))
        (nreverse entries)))))

(defun paper-review-apply ()
  "Append every entry left in the review buffer to `papers-bibliography'."
  (interactive)
  (let ((entries (paper-review-entries)))
    (if (null entries)
        (message "Nothing left to add")
      (append-bibliography-entries entries)
      (message "Added %d entr%s to %s" (length entries)
               (if (= 1 (length entries)) "y" "ies")
               (file-name-nondirectory papers-bibliography)))
    (kill-buffer)))

(defun paper-review-abort ()
  "Drop the proposals without writing any of them."
  (interactive)
  (kill-buffer)
  (message "Proposals dropped"))

(define-derived-mode paper-review-mode bibtex-mode "Paper review"
  "Major mode for the proposed entries of an identification run."
  (setq-local header-line-format
              (substitute-command-keys
               "Edit or delete entries, \\[paper-review-apply] to add what is left, \\[paper-review-abort] to drop them")))

;; `map!' needs general, which the batch tier loading this file lacks.
(keymap-set paper-review-mode-map "C-c C-c" #'paper-review-apply)
(keymap-set paper-review-mode-map "C-c C-k" #'paper-review-abort)

(defun paper-review-show (proposals)
  "Show PROPOSALS for review and return the buffer.
One buffer for the whole run rather than a prompt per paper: 54 yes/no
questions is the shape a reader stops reading."
  (let ((buffer (get-buffer-create "*paper identification*"))
        (unsure (seq-count (lambda (p) (not (plist-get p :identified))) proposals)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "%% %d entries proposed, %d of them unsure and marked below.\n"
                      (length proposals) unsure)
              "% Edit or delete freely: what is left here is what gets written.\n")
      (dolist (proposal proposals)
        (insert (format "\n%% %s%s\n"
                        (file-name-nondirectory (plist-get proposal :file))
                        (if (plist-get proposal :identified)
                            ""
                          "   <- unsure, check the title"))
                (plist-get proposal :entry) "\n"))
      (paper-review-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

;;; identify.el ends here
