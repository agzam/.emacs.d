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

(describe "bibtex-author-family"
  ;; Every fixture here is an author field copied out of references.bib.
  (it "keeps a multi-word family name whole"
    ;; Taking the first whitespace token keyed this paper `van_2018'
    (expect (bibtex-author-family
             "van de Meent, Jan-Willem and Paige, Brooks and Yang, Hongseok and Wood, Frank")
            :to-equal "van de Meent"))

  (it "reads the family name before the comma of the first author"
    (expect (bibtex-author-family "Fong, Brendan and Spivak, David I")
            :to-equal "Fong")
    (expect (bibtex-author-family "Baez, John C. and Stay, Mike") :to-equal "Baez"))

  (it "reads the last word when the first author is written given-name first"
    ;; The hand-written 2021 entry spells its authors this way
    (expect (bibtex-author-family
             "Andreas Haas and Andreas Rossberg and Derek L. Schuff and Ben Titzer")
            :to-equal "Haas"))

  (it "reads a single author"
    (expect (bibtex-author-family "McPherson, Guy") :to-equal "McPherson"))

  (it "unwraps a brace-protected family name"
    (expect (bibtex-author-family "{van de Meent}, Jan-Willem")
            :to-equal "van de Meent"))

  (it "returns nil for a missing or empty field"
    (expect (bibtex-author-family nil) :to-be nil)
    (expect (bibtex-author-family "   ") :to-be nil)))

(describe "bibtex-citekey"
  (it "normalises the key Crossref supplies, so the bib keeps one scheme"
    ;; Crossref answers `McPherson_2021' and DataCite answers a DOI URL;
    ;; passing both through left the bibliography in two styles
    (expect (bibtex-citekey
             "@article{McPherson_2021, title={Rapid Loss of Habitat for Homo sapiens},
  author={McPherson, Guy}, year={2021}}")
            :to-equal "mcpherson_2021"))

  (it "rebuilds the DOI URL DataCite hands back as a key"
    ;; arXiv entries come back keyed by their own URL, which BibTeX rejects
    (expect (bibtex-citekey
             "@article{https://doi.org/10.48550/arxiv.0903.0340,
  author = {Baez, John C. and Stay, Mike},
  year = {2009}}")
            :to-equal "baez_2009"))

  (it "keys a multi-word family name on the whole name"
    (expect (bibtex-citekey
             "@misc{https://doi.org/10.48550/arxiv.1809.10756,
  author = {van de Meent, Jan-Willem and Paige, Brooks and Yang, Hongseok and Wood, Frank},
  title = {An Introduction to Probabilistic Programming},
  year = {2018}}")
            :to-equal "vandemeent_2018"))

  (it "falls back to the author alone when no year is present"
    (expect (bibtex-citekey "@misc{https://x/y, author = {Hoare, C. A. R.}}")
            :to-equal "hoare"))

  (it "falls back to the DOI when there is no author either"
    (expect (bibtex-citekey "@misc{https://x/y, doi = {10.5/ab}}")
            :to-equal "10_5_ab")))

(describe "bibtex-entry-key"
  (it "reads the key as written, without rebuilding it"
    ;; The index records the unique key an entry carries, suffix and all
    (expect (bibtex-entry-key "@article{hoare_1978a, title={T}}")
            :to-equal "hoare_1978a"))

  (it "returns nil when there is no key"
    (expect (bibtex-entry-key "@article{, title={T}}") :to-be nil)))

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

(describe "bibliography-index"
  (let (bib index)
    (before-each
      (setq bib (make-temp-file "refs" nil ".bib"))
      (with-temp-file bib
        ;; The oldest entry here records no file field at all
        (insert "@inproceedings{haas17_bring_webas,\n"
                "  DOI = {10.1145/3062341.3062363}\n}\n\n"
                "@article{baez_2009,\n  file = {/tmp/papers/rosetta.pdf}\n}\n"))
      (setq index (let ((papers-bibliography bib)) (bibliography-index))))
    (after-each (delete-file bib))

    (it "reads files, DOIs and citekeys in one pass"
      ;; An import run holds this instead of re-parsing the bib per paper
      (expect (plist-get index :files) :to-equal '("/tmp/papers/rosetta.pdf"))
      (expect (plist-get index :dois) :to-equal '("10.1145/3062341.3062363"))
      (expect (sort (plist-get index :keys) #'string<)
              :to-equal '("baez_2009" "haas17_bring_webas")))

    (it "sees a paper recorded by its file field"
      (expect (bibliography-index-holds-p index "/tmp/papers/rosetta.pdf" nil)
              :to-be-truthy))

    (it "sees a paper whose entry records no file, by matching the DOI"
      ;; haas17_bring_webas.pdf was re-proposed for import because the
      ;; file-only check could not see the 2021 hand-written entry
      (expect (bibliography-index-holds-p
               index "/tmp/papers/haas17_bring_webas.pdf"
               "@inproceedings{X, DOI = {10.1145/3062341.3062363}}")
              :to-be-truthy))

    (it "matches the DOI case-insensitively"
      (expect (bibliography-index-holds-p
               index "/tmp/papers/other.pdf"
               "@misc{X, doi = {10.1145/3062341.3062363}}")
              :to-be-truthy))

    (it "catches a paper that scraped a cited work's DOI"
      ;; WebAssembly-spec-Draft.pdf prints the PLDI paper's DOI in its
      ;; references, and resolved to that paper's entry
      (expect (bibliography-index-holds-p
               index "/tmp/papers/WebAssembly-spec-Draft.pdf"
               "@inproceedings{Haas_2017, DOI = {10.1145/3062341.3062363}}")
              :to-be-truthy))

    (it "passes a genuinely new paper through"
      (expect (bibliography-index-holds-p
               index "/tmp/papers/new.pdf" "@misc{X, doi = {10.9999/new}}")
              :to-be nil))

    (it "holds a paper appended earlier in the same run, without re-reading"
      ;; The bib on disk is left untouched here: only the index may answer
      (let* ((entry (concat "@misc{vandemeent_2018,\n"
                            "  author = {van de Meent, Jan-Willem and Paige, Brooks},\n"
                            "  doi = {10.48550/ARXIV.1809.10756},\n"
                            "  file = {/tmp/papers/probabilistic_programming.pdf}\n}"))
             (extended (bibliography-index-add index entry)))
        (expect (bibliography-index-holds-p
                 extended "/tmp/papers/probabilistic_programming.pdf" nil)
                :to-be-truthy)
        (expect (bibliography-index-holds-p
                 extended "/tmp/papers/other.pdf"
                 "@misc{X, doi = {10.48550/arxiv.1809.10756}}")
                :to-be-truthy)
        (expect (member "vandemeent_2018" (plist-get extended :keys))
                :to-be-truthy)))

    (it "keeps the unique suffix the appended entry carries"
      ;; Recomputing the key here would drop the collision suffix
      (let ((extended (bibliography-index-add
                       index "@misc{baez_2009a, author = {Baez, John C.}, year = {2009}}")))
        (expect (member "baez_2009a" (plist-get extended :keys)) :to-be-truthy)))))

(describe "prepared-bibliography-entry"
  (it "takes the citekeys already used from the index it is handed"
    (let ((index '(:files nil :dois nil :keys ("vandemeent_2018"))))
      (expect (prepared-bibliography-entry
               "@misc{https://doi.org/10.48550/arxiv.1809.10756,
  author = {van de Meent, Jan-Willem and Paige, Brooks and Yang, Hongseok and Wood, Frank},
  year = {2018}}"
               "/tmp/papers/probabilistic_programming.pdf" index)
              :to-match "@misc{vandemeent_2018a,"))))

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

    (it "reads a path whose filename carries braces"
      ;; A path cut at the first `}' matches nothing, so its work was
      ;; proposed for import again on every run
      (let ((braced (make-temp-file "refs" nil ".bib")))
        (unwind-protect
            (progn
              (with-temp-file braced
                (insert "@book{prolog,\n"
                        "  file = {/tmp/books/Logic Programming {8EDB06BF}.1.pdf}\n}\n"))
              (expect (bibliography-files braced)
                      :to-equal '("/tmp/books/Logic Programming {8EDB06BF}.1.pdf")))
          (delete-file braced))))

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
