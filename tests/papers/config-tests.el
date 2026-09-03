;;; tests/papers/config-tests.el --- papers module wiring specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun papers-tests--read-form (marker)
  "Read the form starting at MARKER in the papers config.
The specs read source: config.el cannot be loaded in the batch tier,
where neither `use-package' nor general (behind `map!') exists."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/papers/config.el" test-config-root))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward marker)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(defvar papers-tests--citar (papers-tests--read-form "(use-package citar"))

(describe "citar bibliography wiring"
  (let (init)
    (before-all
      (setq init (flatten-tree (use-package-body-forms (cdr papers-tests--citar) :init))))

    (it "reads both bibs, so one completion list covers papers and books"
      ;; citar shows bib entries, not files on disk; a corpus missing from
      ;; citar-bibliography is simply invisible
      (expect (memq 'papers-bibliography init) :to-be-truthy)
      (expect (memq 'books-bibliography init) :to-be-truthy))

    (it "keeps the book tree out of citar-library-paths"
      ;; Book entries carry a file field, which citar resolves directly.
      ;; The tree holds over 5000 files a scan would walk for nothing.
      (let ((paths (cdr (memq 'citar-library-paths init))))
        (expect (memq 'papers-directory paths) :to-be-truthy)
        (expect (memq 'books-directory paths) :to-be nil)))))

(describe "papers module variables"
  (it "puts each bib beside the corpus it describes"
    ;; Both corpora sync through Resilio, so the metadata reaches the phone
    ;; as plain text next to the documents
    (let ((papers-bib (papers-tests--read-form "(defvar papers-bibliography"))
          (books-bib (papers-tests--read-form "(defvar books-bibliography")))
      (expect (memq 'papers-directory (flatten-tree papers-bib)) :to-be-truthy)
      (expect (memq 'books-directory (flatten-tree books-bib)) :to-be-truthy)))

  (it "keeps the two bibs distinct, so one import pass cannot disturb the other"
    (expect (nth 2 (papers-tests--read-form "(defvar papers-bibliography"))
            :not :to-equal
            (nth 2 (papers-tests--read-form "(defvar books-bibliography")))))

(describe "citar notes source"
  (it "registers vulpea rather than citar-org-roam"
    ;; citar-org-roam reads the org-roam database, which indexes nothing
    ;; here because vulpea indexes the same tree
    (let ((config (flatten-tree
                   (use-package-body-forms (cdr papers-tests--citar) :config))))
      (expect (memq 'citar-register-notes-source config) :to-be-truthy)
      (expect (memq 'paper-notes-by-citekey config) :to-be-truthy)
      (expect (memq 'citar-org-roam config) :to-be nil))))

;;; config-tests.el ends here
