;;; modules/papers/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; The library half of paper reading: what is in the corpus, what it is
;; called, and which note belongs to it.  No Doom original - doom.d left
;; :tools biblio commented out, so the bindings tree's `SPC n b' row is
;; pruned and this module fills it, the way the pdf module fills the
;; pruned org-noter row.
;;
;; Resilio owns the files (a flat folder that also reaches the phone),
;; the bib file beside them owns the metadata, citar is the front door,
;; and vulpea owns the notes.  Entries carry a `file' field, so no PDF is
;; ever renamed and saveplace-pdf-view keeps its positions.
;;
;; citar-org-roam is deliberately absent: it queries the org-roam
;; database, which holds no nodes here because vulpea indexes the same
;; tree.  `citar-register-notes-source' takes a vulpea-backed source
;; instead (autoload/notes.el).
;;; Code:

(defvar papers-directory (expand-file-name "~/SyncMobile/Papers/")
  "Folder holding the paper corpus, synced by Resilio.")

(defvar papers-bibliography (expand-file-name "references.bib" papers-directory)
  "Bib file that is the metadata source of truth, kept beside the PDFs.")

(defvar books-directory (expand-file-name "~/SyncMobile/Books/")
  "Tree holding the book corpus, synced by Resilio.
Unlike the flat paper folder this nests, and a work is a directory as
often as it is a file: one textbook contributes thousands of per-exercise
PDFs, so counting files says nothing about how many works are here.")

(defvar books-bibliography (expand-file-name "books.bib" books-directory)
  "Bib file for the book corpus, kept beside the books.
Separate from `papers-bibliography' so neither corpus's import pass can
disturb the other; citar reads both and searches them as one.")

(use-package citar
  :defer t
  :commands (citar-open citar-open-files citar-open-notes citar-insert-citation)
  :init
  ;; Both bibs, so one completion list covers papers and books.
  ;; `citar-library-paths' names the paper folder only: entries carry a
  ;; `file' field, which citar resolves directly, and the book tree holds
  ;; over 5000 files that a scan would walk for nothing.
  (setopt citar-bibliography (list papers-bibliography books-bibliography)
          citar-library-paths (list papers-directory))
  :config
  (setopt
   ;; The bib is read for studying, not writing: show enough to recognise a
   ;; paper in the minibuffer and nothing else.
   citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:60}")
     (suffix . "        ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor:%etal}, ${title}")))

  (citar-register-notes-source
   'vulpea (list :name "Vulpea notes"
                 :category 'vulpea-note
                 :items #'paper-notes-by-citekey
                 :hasitems #'paper-notes-predicate
                 :open #'paper-note-visit
                 :create #'paper-note-create
                 :transform #'paper-note-title))
  (setopt citar-notes-source 'vulpea))

(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode))

;; org-cite is near-worthless here (reading, not publishing), but wiring the
;; processors costs nothing and makes a stray cite: link followable.  The
;; bibliography has to match `citar-bibliography': org-cite resolves a key
;; against this list alone, so listing only the paper bib leaves every book
;; citation unresolvable.
(after! oc
  (setopt org-cite-global-bibliography (list papers-bibliography books-bibliography)
          org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar))

;; The bindings tree reserves `SPC n b' behind (modulep! :tools biblio),
;; which is structurally false here; this module owns citar, so it fills
;; the slot and hangs the rest of the library beside it.
(map! :leader
      (:prefix-map ("n" . "notes")
       :desc "Bibliographic notes" "b" #'citar-open-notes
       (:prefix ("p" . "papers")
        :desc "Open paper"      "p" #'citar-open
        :desc "Open PDF"        "f" #'citar-open-files
        :desc "Open note"       "n" #'citar-open-notes
        :desc "Import paper"    "i" #'import-paper
        :desc "Import folder"   "I" #'import-papers-in-folder
        :desc "Import books"    "k" #'import-books
        :desc "Identify papers" "g" #'identify-papers
        :desc "Visit bib file"  "b" (cmd! (find-file papers-bibliography))
        :desc "Visit book bib"  "B" (cmd! (find-file books-bibliography)))))

;;; config.el ends here
