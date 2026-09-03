;;; modules/papers/autoload/import.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Turning a PDF into a bib entry.  Only the identifier path lives here: a
;; DOI or arXiv id printed on the first pages resolves to a complete entry
;; through doi.org content negotiation, which serves Crossref and DataCite
;; alike, so arXiv needs no separate backend.
;;
;; Nothing is written without the entry being shown first.  A DOI found on
;; page one is not necessarily the paper's own: WebAssembly-spec-Draft.pdf
;; carries the WebAssembly PLDI paper's DOI in its references, and resolving
;; it yields a confident, wrong entry.  Only a reader catches that.
;;
;; Papers whose first pages carry no identifier at all - most of this corpus,
;; being pre-DOI computer science - are not handled here.
;;; Code:

(require 'seq)
(require 'subr-x)

(defvar papers-bibliography)
(defvar papers-directory)

(defconst paper-placeholder-doi-regexp
  (rx (or "nnnn" "NNNN" "xxxx" "XXXX" "0000000"))
  "Matches the dummy DOIs that publisher LaTeX templates leave behind.")

;;;###autoload
(defun paper-text (file &optional pages)
  "Return the text of FILE, over PAGES (default the first two).
mutool is used rather than epdfinfo: this runs before any PDF is opened,
and metadata needs raw text, not the reflowed reading view."
  (unless (executable-find "mutool")
    (user-error "mutool not found; install mupdf-tools"))
  (with-temp-buffer
    (call-process "mutool" nil t nil
                  "draw" "-F" "txt" "-o" "-" (expand-file-name file)
                  (or pages "1-2"))
    (buffer-string)))

;;;###autoload
(defun paper-identifier (text)
  "Return (doi . DOI) or (arxiv . ID) found in TEXT, or nil.
Placeholder DOIs left by LaTeX templates are rejected: resolving one
returns somebody else's paper."
  (let ((case-fold-search t))
    (cond
     ((string-match (rx "10." (= 4 digit) (* (any "0-9")) "/"
                        (+ (not (any " \t\n\"<>,;)"))))
                    text)
      (let ((doi (string-trim-right (match-string 0 text) "[.,;]+")))
        (unless (string-match-p paper-placeholder-doi-regexp doi)
          (cons 'doi doi))))
     ((string-match (rx "arxiv" (* (any ": v")) (group (= 4 digit) "." (** 4 5 digit)))
                    text)
      (cons 'arxiv (match-string 1 text))))))

;;;###autoload
(defun bibtex-entry-for-identifier (identifier)
  "Fetch a BibTeX entry for IDENTIFIER, a cons of (doi . DOI) or (arxiv . ID).
doi.org content negotiation is used, so Crossref and DataCite both answer."
  (let* ((doi (pcase identifier
                (`(doi . ,d) d)
                (`(arxiv . ,id) (concat "10.48550/arXiv." id))
                (_ (error "Unrecognised identifier: %S" identifier))))
         (url-request-extra-headers '(("Accept" . "application/x-bibtex")))
         (url-mime-accept-string "application/x-bibtex")
         (buffer (url-retrieve-synchronously
                  (concat "https://doi.org/" (url-hexify-string doi))
                  t nil 30)))
    (unwind-protect
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "^HTTP/[0-9.]+ 20[0-9]" nil t)
              (goto-char (point-min))
              (when (search-forward "\n\n" nil t)
                (let ((body (string-trim (buffer-substring-no-properties
                                          (point) (point-max)))))
                  (decode-coding-string body 'utf-8))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun bibtex-entry-field (entry field)
  "Return FIELD of ENTRY, or nil.  Enclosing braces and quotes are stripped.
Values are read to their matching delimiter, not to the next comma: titles
carry commas, and a comma-terminated reader truncates \"Physics, Topology,
Logic and Computation\" to \"Physics\"."
  (when (string-match (format "[,{][ \t\n]*%s[ \t\n]*=[ \t\n]*" (regexp-quote field))
                      entry)
    (let ((start (match-end 0)))
      (pcase (aref entry start)
        (?{ (let ((depth 1) (i (1+ start)) (end (length entry)))
              (while (and (< i end) (< 0 depth))
                (pcase (aref entry i)
                  (?{ (setq depth (1+ depth)))
                  (?} (setq depth (1- depth))))
                (setq i (1+ i)))
              (string-trim (substring entry (1+ start) (1- i)))))
        (?\" (when (string-match "\\([^\"]*\\)\"" entry (1+ start))
               (string-trim (match-string 1 entry))))
        (_ (when (string-match "\\([^,}\n]+\\)" entry start)
             (string-trim (match-string 1 entry))))))))

;;;###autoload
(defun bibliography-dois ()
  "Return the DOIs already recorded in `papers-bibliography', downcased."
  (when (file-exists-p papers-bibliography)
    (with-temp-buffer
      (insert-file-contents papers-bibliography)
      (goto-char (point-min))
      (let (dois)
        (while (re-search-forward "^[ \t]*doi[ \t]*=[ \t]*[{\"]?\\([^},\"\n]+\\)" nil t)
          (push (downcase (string-trim (match-string 1))) dois))
        dois))))

(defun bibliography-has-entry-p (file entry)
  "Return non-nil when FILE or ENTRY's DOI is already in the bibliography.
Matching on the DOI as well as the path matters twice: the oldest entry
here records no `file' at all, and a paper printing a cited work's DOI
resolves to an entry the bibliography already holds."
  (or (member (expand-file-name file) (bibliography-files))
      (when-let* ((doi (and entry (bibtex-entry-field entry "doi"))))
        (member (downcase doi) (bibliography-dois)))))

(defun bibtex-citekey (entry)
  "Return a usable citekey for ENTRY.
DataCite answers with the DOI URL as the key, which is not a legal
BibTeX key; such keys are rebuilt from author and year instead."
  (let ((key (when (string-match "\\`\\s-*@[A-Za-z]+{\\([^,]+\\)," entry)
               (string-trim (match-string 1 entry)))))
    (if (and key (not (string-match-p "[ /:\\\\{}]" key)))
        key
      (let* ((author (bibtex-entry-field entry "author"))
             (family (when author
                       (downcase (replace-regexp-in-string
                                  "[^a-zA-Z]" ""
                                  (car (split-string author "[, ]" t))))))
             (year (or (bibtex-entry-field entry "year")
                       (when-let* ((d (bibtex-entry-field entry "date")))
                         (substring d 0 (min 4 (length d)))))))
        (cond ((and family year (not (string-empty-p family)))
               (concat family "_" year))
              ((and family (not (string-empty-p family))) family)
              (t (replace-regexp-in-string
                  "[^a-zA-Z0-9]" "_"
                  (or (bibtex-entry-field entry "doi") "paper"))))))))

;;;###autoload
(defun bibtex-entry-with-key (entry key)
  "Return ENTRY with its citekey replaced by KEY."
  (if (string-match "\\`\\(\\s-*@[A-Za-z]+{\\)\\([^,]*\\)\\(,\\)" entry)
      (replace-match key t t entry 2)
    entry))

(defun unique-citekey (key existing)
  "Return KEY, suffixed with a letter when EXISTING already holds it."
  (if (not (member key existing))
      key
    (let ((suffixes (string-to-list "abcdefghijklmnopqrstuvwxyz"))
          found)
      (while (and suffixes (not found))
        (let ((candidate (concat key (char-to-string (pop suffixes)))))
          (unless (member candidate existing) (setq found candidate))))
      (or found (concat key (format-time-string "%s"))))))

;;;###autoload
(defun bibliography-keys ()
  "Return the citekeys already recorded in `papers-bibliography'."
  (when (file-exists-p papers-bibliography)
    (with-temp-buffer
      (insert-file-contents papers-bibliography)
      (goto-char (point-min))
      (let (keys)
        (while (re-search-forward "^\\s-*@[A-Za-z]+{\\([^,]+\\)," nil t)
          (push (string-trim (match-string 1)) keys))
        keys))))

;;;###autoload
(defun bibtex-entry-with-file (entry path)
  "Return ENTRY with a `file' field holding PATH inserted before its close.
The field is what lets citar find the PDF, so nothing on disk is renamed."
  (if (string-match "\n?}\\s-*\\'" entry)
      (concat (substring entry 0 (match-beginning 0))
              (format ",\n  file = {%s}\n}" (expand-file-name path)))
    (error "Not a parseable BibTeX entry: %s" (truncate-string-to-width entry 60))))

;;;###autoload
(defun bibliography-files ()
  "Return the PDF paths already recorded in `papers-bibliography'."
  (when (file-exists-p papers-bibliography)
    (with-temp-buffer
      (insert-file-contents papers-bibliography)
      (goto-char (point-min))
      (let (files)
        (while (re-search-forward "^\\s-*file\\s-*=\\s-*{\\([^}]+\\)}" nil t)
          (push (expand-file-name (match-string 1)) files))
        files))))

(defun prepared-bibliography-entry (entry path)
  "Return ENTRY ready to append: unique citekey, plus a `file' field for PATH."
  (bibtex-entry-with-file
   (bibtex-entry-with-key entry (unique-citekey (bibtex-citekey entry)
                                                (bibliography-keys)))
   path))

(defun append-bibliography-entry (entry)
  "Append ENTRY to `papers-bibliography' and save it."
  (with-current-buffer (find-file-noselect papers-bibliography)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n" entry "\n")
    (save-buffer)))

(defun confirm-bibliography-entry (entry file)
  "Show ENTRY for FILE and return non-nil when it is accepted.
The gate is the point of the command: a DOI scraped off page one may
belong to a cited paper rather than to FILE."
  (let ((buffer (get-buffer-create "*paper entry*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";; proposed entry for %s\n\n" (file-name-nondirectory file))
                entry "\n")
        (goto-char (point-min))))
    (save-window-excursion
      (display-buffer buffer)
      (unwind-protect
          (yes-or-no-p (format "Entry describes %s? " (file-name-nondirectory file)))
        (kill-buffer buffer)))))

;;;###autoload
(defun import-paper (file)
  "Add a bib entry for the PDF FILE to `papers-bibliography'.
Only papers printing a DOI or arXiv id are handled; the rest need the
identification pass."
  (interactive
   (list (read-file-name "PDF: " papers-directory nil t nil
                         (lambda (f) (or (directory-name-p f)
                                         (string-suffix-p ".pdf" f t))))))
  (when (bibliography-has-entry-p file nil)
    (user-error "%s already has an entry" (file-name-nondirectory file)))
  (let* ((identifier (or (paper-identifier (paper-text file))
                         (user-error "No DOI or arXiv id in %s"
                                     (file-name-nondirectory file))))
         (entry (or (bibtex-entry-for-identifier identifier)
                    (user-error "%s did not resolve" (cdr identifier)))))
    (when (bibliography-has-entry-p file entry)
      (user-error "%s resolves to %s, which the bibliography already holds"
                  (file-name-nondirectory file) (cdr identifier)))
    (if (confirm-bibliography-entry entry file)
        (progn (append-bibliography-entry (prepared-bibliography-entry entry file))
               (message "Added %s" (cdr identifier)))
      (message "Skipped %s" (file-name-nondirectory file)))))

;;;###autoload
(defun import-papers-in-folder (&optional directory)
  "Import every PDF in DIRECTORY that has no entry yet, one confirmation each.
DIRECTORY defaults to `papers-directory'.  Papers with no printed
identifier are reported at the end rather than interrupting the run."
  (interactive)
  (let ((pdfs (seq-remove (lambda (f) (bibliography-has-entry-p f nil))
                          (directory-files (or directory papers-directory) t "\\.pdf\\'")))
        added skipped duplicate unidentified)
    (dolist (pdf pdfs)
      (let* ((identifier (ignore-errors (paper-identifier (paper-text pdf))))
             (entry (and identifier
                         (ignore-errors (bibtex-entry-for-identifier identifier)))))
        (cond
         ((null entry) (push pdf unidentified))
         ;; Re-read per paper, so an entry added earlier in this run counts.
         ((bibliography-has-entry-p pdf entry) (push pdf duplicate))
         ((confirm-bibliography-entry entry pdf)
          (append-bibliography-entry (prepared-bibliography-entry entry pdf))
          (push pdf added))
         (t (push pdf skipped)))))
    (message "Imported %d, declined %d, already held %d, no identifier %d, of %d"
             (length added) (length skipped) (length duplicate)
             (length unidentified) (length pdfs))))

;;; import.el ends here
