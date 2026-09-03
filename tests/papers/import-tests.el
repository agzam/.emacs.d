;;; tests/papers/import-tests.el --- paper import specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; config.el owns these; the autoload file only reads them at runtime.
(defvar papers-directory "/tmp/papers/")
(defvar papers-bibliography "/tmp/papers/references.bib")

(load-module-file "modules/papers/autoload/import.el")

(describe "paper-identifier"
  (it "finds a plain DOI"
    (expect (paper-identifier "we cite 10.1145/3062341.3062363 here")
            :to-equal '(doi . "10.1145/3062341.3062363")))

  (it "strips sentence punctuation trailing the DOI"
    ;; Academia Letters prints the DOI at the end of a sentence
    (expect (paper-identifier "see 10.20935/AL498.")
            :to-equal '(doi . "10.20935/AL498")))

  (it "rejects the dummy DOI publisher LaTeX templates leave behind"
    ;; hopl-4-emacs-lisp.pdf ships with the ACM template placeholder;
    ;; resolving it returns an unrelated paper
    (expect (paper-identifier "DOI: 10.1145/nnnnnnn.nnnnnnn") :to-be nil)
    (expect (paper-identifier "DOI: 10.1145/XXXXXXX.XXXXXXX") :to-be nil))

  (it "finds an arXiv id in the usual spellings"
    (expect (paper-identifier "arXiv:1809.10756") :to-equal '(arxiv . "1809.10756"))
    (expect (paper-identifier "arXiv: 0903.0340v3") :to-equal '(arxiv . "0903.0340")))

  (it "returns nil for pre-DOI papers that print no identifier"
    (expect (paper-identifier "Communicating Sequential Processes\nC. A. R. Hoare")
            :to-be nil)))

(describe "bibtex-entry-with-file"
  (it "inserts a file field before the closing brace"
    (expect (bibtex-entry-with-file "@misc{k, title={T}\n}" "/tmp/a.pdf")
            :to-equal "@misc{k, title={T},\n  file = {/tmp/a.pdf}\n}"))

  (it "refuses anything that is not an entry"
    (expect (bibtex-entry-with-file "not an entry" "/tmp/a.pdf") :to-throw 'error)))

(describe "bibtex-citekey"
  (it "keeps a key that is already legal"
    (expect (bibtex-citekey "@article{McPherson_2021, title={T}}")
            :to-equal "McPherson_2021"))

  (it "rebuilds the DOI URL DataCite hands back as a key"
    ;; arXiv entries come back keyed by their own URL, which BibTeX rejects
    (expect (bibtex-citekey
             "@article{https://doi.org/10.48550/arxiv.0903.0340,
  author = {Baez, John C. and Stay, Mike},
  year = {2009}}")
            :to-equal "baez_2009"))

  (it "falls back to the author alone when no year is present"
    (expect (bibtex-citekey "@misc{https://x/y, author = {Hoare, C. A. R.}}")
            :to-equal "hoare"))

  (it "falls back to the DOI when there is no author either"
    (expect (bibtex-citekey "@misc{https://x/y, doi = {10.5/ab}}")
            :to-equal "10_5_ab")))

(describe "bibtex-entry-field"
  (it "keeps a braced value containing commas whole"
    ;; rosetta-stone.pdf reported its title as "Physics" while the reader
    ;; stopped at the first comma
    (expect (bibtex-entry-field
             "@article{k, title = {Physics, Topology, Logic and Computation}, year = {2009}}"
             "title")
            :to-equal "Physics, Topology, Logic and Computation"))

  (it "handles nested braces"
    (expect (bibtex-entry-field "@misc{k, title = {A {B, C} D}}" "title")
            :to-equal "A {B, C} D"))

  (it "reads a quoted value"
    (expect (bibtex-entry-field "@misc{k, title = \"Plain, quoted\"}" "title")
            :to-equal "Plain, quoted"))

  (it "reads a bare value"
    (expect (bibtex-entry-field "@misc{k, year = 2017, title = {T}}" "year")
            :to-equal "2017"))

  (it "returns nil for an absent field"
    (expect (bibtex-entry-field "@misc{k, title = {T}}" "doi") :to-be nil)))

(describe "bibliography-has-entry-p"
  (let (bib)
    (before-each
      (setq bib (make-temp-file "refs" nil ".bib"))
      (with-temp-file bib
        ;; The oldest entry here records no file field at all
        (insert "@inproceedings{haas17_bring_webas,\n"
                "  DOI = {10.1145/3062341.3062363}\n}\n\n"
                "@article{baez_2009,\n  file = {/tmp/papers/rosetta.pdf}\n}\n")))
    (after-each (delete-file bib))

    (it "sees a paper recorded by its file field"
      (let ((papers-bibliography bib))
        (expect (bibliography-has-entry-p "/tmp/papers/rosetta.pdf" nil) :to-be-truthy)))

    (it "sees a paper whose entry records no file, by matching the DOI"
      ;; haas17_bring_webas.pdf was re-proposed for import because the
      ;; file-only check could not see the 2021 hand-written entry
      (let ((papers-bibliography bib))
        (expect (bibliography-has-entry-p
                 "/tmp/papers/haas17_bring_webas.pdf"
                 "@inproceedings{X, DOI = {10.1145/3062341.3062363}}")
                :to-be-truthy)))

    (it "matches the DOI case-insensitively"
      (let ((papers-bibliography bib))
        (expect (bibliography-has-entry-p
                 "/tmp/papers/other.pdf"
                 "@misc{X, doi = {10.1145/3062341.3062363}}")
                :to-be-truthy)))

    (it "catches a paper that scraped a cited work's DOI"
      ;; WebAssembly-spec-Draft.pdf prints the PLDI paper's DOI in its
      ;; references, and resolved to that paper's entry
      (let ((papers-bibliography bib))
        (expect (bibliography-has-entry-p
                 "/tmp/papers/WebAssembly-spec-Draft.pdf"
                 "@inproceedings{Haas_2017, DOI = {10.1145/3062341.3062363}}")
                :to-be-truthy)))

    (it "passes a genuinely new paper through"
      (let ((papers-bibliography bib))
        (expect (bibliography-has-entry-p
                 "/tmp/papers/new.pdf" "@misc{X, doi = {10.9999/new}}")
                :to-be nil)))))

(describe "bibtex-entry-with-key"
  (it "replaces the key and leaves the rest untouched"
    (expect (bibtex-entry-with-key "@article{old, title={T}}" "new")
            :to-equal "@article{new, title={T}}")))

(describe "unique-citekey"
  (it "returns the key when it is free"
    (expect (unique-citekey "hoare_1978" '("wadler_1989")) :to-equal "hoare_1978"))

  (it "suffixes a letter on collision"
    (expect (unique-citekey "hoare_1978" '("hoare_1978")) :to-equal "hoare_1978a")
    (expect (unique-citekey "hoare_1978" '("hoare_1978" "hoare_1978a"))
            :to-equal "hoare_1978b")))

(describe "bibliography readers"
  (let (bib)
    (before-each
      (setq bib (make-temp-file "refs" nil ".bib"))
      (with-temp-file bib
        (insert "@inproceedings{haas17_bring_webas,\n"
                "  title = {Bringing the web up to speed},\n"
                "  file = {/tmp/papers/haas.pdf}\n}\n\n"
                "@article{baez_2009,\n  file = {/tmp/papers/rosetta.pdf}\n}\n")))
    (after-each (delete-file bib))

    (it "reads every citekey"
      (let ((papers-bibliography bib))
        (expect (sort (bibliography-keys) #'string<)
                :to-equal '("baez_2009" "haas17_bring_webas"))))

    (it "reads every recorded PDF path"
      (let ((papers-bibliography bib))
        (expect (sort (bibliography-files) #'string<)
                :to-equal '("/tmp/papers/haas.pdf" "/tmp/papers/rosetta.pdf"))))

    (it "reads nothing from a bib that does not exist"
      (let ((papers-bibliography "/tmp/definitely-absent.bib"))
        (expect (bibliography-keys) :to-be nil)
        (expect (bibliography-files) :to-be nil)))))

(describe "bibtex-entry-for-identifier"
  (it "maps an arXiv id onto its DataCite DOI"
    (let (requested)
      (cl-letf (((symbol-function 'url-retrieve-synchronously)
                 (lambda (url &rest _) (setq requested url) nil)))
        (bibtex-entry-for-identifier '(arxiv . "1809.10756"))
        (expect requested :to-match "10\\.48550%2FarXiv\\.1809\\.10756"))))

  (it "returns the entry body of a successful response"
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buffer (generate-new-buffer " *stub*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\nContent-Type: application/x-bibtex\n\n"
                             "@misc{k, title={T}}\n"))
                   buffer))))
      (expect (bibtex-entry-for-identifier '(doi . "10.5/ab"))
              :to-equal "@misc{k, title={T}}")))

  (it "returns nil when the DOI does not resolve"
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buffer (generate-new-buffer " *stub*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 404 Not Found\n\nnope"))
                   buffer))))
      (expect (bibtex-entry-for-identifier '(doi . "10.5/nope")) :to-be nil)))

  (it "rejects an identifier of an unknown kind"
    (expect (bibtex-entry-for-identifier '(isbn . "123")) :to-throw 'error)))

;;; import-tests.el ends here
