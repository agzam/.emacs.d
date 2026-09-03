;;; modules/papers/autoload/notes.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A citar notes source backed by vulpea.  citar-org-roam cannot serve here:
;; it reads the org-roam database, which indexes nothing on this machine
;; because vulpea indexes the same tree instead.
;;
;; A paper note is an ordinary vulpea note whose ROAM_REFS holds the citekey
;; as `@key'.  That reuses the property 457 notes already carry rather than
;; inventing a second convention, and it keeps paper notes inside the same
;; graph as everything else - backlinks, vulpea-find and the sidebar all work
;; without knowing papers exist.
;;
;; The note id citar passes around is the vulpea note id, so `:transform'
;; has to turn it back into a title; a raw UUID in the minibuffer is useless.
;;; Code:

(require 'seq)

(declare-function vulpea-create "vulpea")
(declare-function vulpea-db-get-by-id "vulpea-db-query")
(declare-function vulpea-db-query-by-property-key "vulpea-db-query")
(declare-function vulpea-note-id "vulpea-note")
(declare-function vulpea-note-properties "vulpea-note")
(declare-function vulpea-note-title "vulpea-note")
(declare-function vulpea-visit "vulpea")
(declare-function citar-get-value "citar")

;;;###autoload
(defun citekeys-in-roam-refs (refs)
  "Return the citekeys inside REFS, the raw value of a ROAM_REFS property.
Refs mix citekeys with plain URLs, and org writes a citekey as `@key',
`cite:key' or `[cite:@key]' depending on which era wrote it."
  (when (stringp refs)
    (let ((case-fold-search nil)
          (start 0)
          keys)
      (while (string-match "\\(?:cite:\\)?@\\([^]\s\t\n,;}]+\\)\\|cite:\\([^]\s\t\n,;}@]+\\)"
                           refs start)
        (push (or (match-string 1 refs) (match-string 2 refs)) keys)
        (setq start (match-end 0)))
      (nreverse keys))))

;;;###autoload
(defun paper-notes-by-citekey (&optional keys)
  "Return a hash table mapping citekeys to lists of vulpea note ids.
KEYS limits the result to those citekeys; nil means every citekey found.
This is the `:items' function of the citar notes source."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (note (vulpea-db-query-by-property-key "ROAM_REFS"))
      (dolist (key (citekeys-in-roam-refs
                    (cdr (assoc "ROAM_REFS" (vulpea-note-properties note)))))
        (when (or (null keys) (member key keys))
          (push (vulpea-note-id note) (gethash key table)))))
    table))

;;;###autoload
(defun paper-notes-predicate ()
  "Return a predicate telling whether a citekey has notes, or nil for none.
This is the `:hasitems' function of the citar notes source; citar calls it
once per completion session and reuses the closure over every candidate,
so the table is built here rather than per key."
  (let ((table (paper-notes-by-citekey)))
    (unless (zerop (hash-table-count table))
      (lambda (citekey) (and (gethash citekey table) t)))))

;;;###autoload
(defun paper-note-title (id)
  "Return the title of the vulpea note ID, for display in citar."
  (if-let* ((note (vulpea-db-get-by-id id)))
      (vulpea-note-title note)
    id))

;;;###autoload
(defun paper-note-visit (id)
  "Open the vulpea note ID."
  (vulpea-visit id))

;;;###autoload
(defun paper-note-create (key entry)
  "Create a paper note for citekey KEY described by bib ENTRY.
The note carries KEY in ROAM_REFS so citar finds it again, and the PDF
path in NOTER_DOCUMENT so `org-noter' starts from the note itself."
  (let* ((title (or (citar-get-value "title" entry) key))
         (author (citar-get-value "author" entry))
         (year (or (citar-get-value "year" entry)
                   (citar-get-value "date" entry)))
         (file (citar-get-value "file" entry))
         (properties (append `(("ROAM_REFS" . ,(concat "@" key)))
                             (when file `(("NOTER_DOCUMENT" . ,file)))))
         (note (vulpea-create
                (if (and author year) (format "%s (%s) %s" author year title) title)
                nil
                :properties properties
                :tags '("paper"))))
    (vulpea-visit (vulpea-note-id note))
    note))

;;; notes.el ends here
