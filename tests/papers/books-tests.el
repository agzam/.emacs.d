;;; tests/papers/books-tests.el --- book corpus import specs -*- lexical-binding: t; -*-

;; Every filename fixture below is copied verbatim out of ~/SyncMobile/Books.
;; Invented names are what shipped the round-1 note-title defect: a fixture
;; that no real file resembles cannot fail the way real data does.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; config.el owns these; the autoload files only read them at runtime.
(defvar papers-bibliography "/tmp/papers/references.bib")
(defvar books-directory "/tmp/books/")
(defvar books-bibliography "/tmp/books/books.bib")

(load-module-file "modules/papers/autoload/import.el")
(load-module-file "modules/papers/autoload/books.el")

(describe "book-clean-name"
  (it "turns underscores into spaces"
    (expect (book-clean-name "ANAYA_Gramatica_B1") :to-equal "ANAYA Gramatica B1"))

  (it "drops the release tags filenames carry"
    (expect (book-clean-name
             "O'REILLY - Functional JavaScript [June 2013] [masum2444]")
            :to-equal "O'REILLY - Functional JavaScript")))

(describe "book-year"
  (it "reads a year in parentheses"
    (expect (book-year "A new reference grammar of modern Spanish (2019)")
            :to-equal "2019"))

  (it "takes the last year, since an edition number comes first"
    (expect (book-year "Rosen K  Discrete Mathematics and Its Applications 7ed  2012")
            :to-equal "2012"))

  (it "ignores digits inside a longer run"
    ;; an ISBN fragment is not a year
    (expect (book-year "148GTM Joseph Rotman 9781461241768") :to-be nil))

  (it "returns nil when no year is printed"
    (expect (book-year "Bash Quick Reference") :to-be nil)))

(describe "book-name-like-p"
  (it "accepts initials and particles"
    (expect (book-name-like-p "Halloway S., Bedra A.") :to-be-truthy)
    (expect (book-name-like-p "van de Meent") :to-be-truthy))

  (it "rejects a title, even one that is all capitalised words"
    ;; `case-fold-search' defaults to t, which made [[:upper:]] match
    ;; lowercase and let `Node.js Recipes' pass as an author
    (expect (book-name-like-p "Programming Clojure, 2nd edition") :to-be nil)
    (expect (book-name-like-p "A Mind for Numbers") :to-be nil)))

(describe "book-metadata"
  (it "reads author, title and year from the fullest spelling"
    (let ((meta (book-metadata
                 "Halloway S., Bedra A. - Programming Clojure, 2nd edition - 2012")))
      (expect (plist-get meta :author) :to-equal "Halloway S., Bedra A.")
      (expect (plist-get meta :title) :to-equal "Programming Clojure, 2nd edition")
      (expect (plist-get meta :year) :to-equal "2012")))

  (it "keeps the period that ends an initial"
    (expect (plist-get (book-metadata
                        "Knuth D.E. - The Art of Computer Programming - 1997")
                       :author)
            :to-equal "Knuth D.E."))

  (it "reads the trailing `by AUTHOR' spelling"
    (let ((meta (book-metadata "A Short History of Nearly Everything by Bill Bryson")))
      (expect (plist-get meta :author) :to-equal "Bill Bryson")
      (expect (plist-get meta :title) :to-equal "A Short History of Nearly Everything")))

  (it "drops the year an uploader ran into the author"
    (expect (plist-get (book-metadata
                        "Data Visualization with D3.js Cookbook by Nick Qi Zhu.2013")
                       :author)
            :to-equal "Nick Qi Zhu"))

  (it "refuses to read a subtitle as an author"
    ;; This has the same shape as `Alex Xu - System Design Interview';
    ;; a dash cannot tell them apart, so neither is credited an author
    (let ((meta (book-metadata "A Mind for Numbers - How to Excel at Math and Science")))
      (expect (plist-get meta :author) :to-be nil)
      (expect (plist-get meta :title)
              :to-equal "A Mind for Numbers - How to Excel at Math and Science")))

  (it "leaves a title-first name uncredited rather than guessing"
    (let ((meta (book-metadata "Node.js Recipes - Cory Gackenheimer")))
      (expect (plist-get meta :author) :to-be nil)
      (expect (plist-get meta :title) :to-equal "Node.js Recipes - Cory Gackenheimer")))

  (it "keeps a parenthesised year inside the title where it is not trailing"
    ;; Stripping any trailing year truncated this to `...(Addison-Wesley,'
    (expect (plist-get (book-metadata
                        "Haskell - The Craft of Functional Programming, 2ed (Addison-Wesley, 1999) by Tantanoid")
                       :title)
            :to-equal "Haskell - The Craft of Functional Programming, 2ed (Addison-Wesley, 1999)"))

  (it "strips a trailing parenthesised year from the title"
    (expect (plist-get (book-metadata "A new reference grammar of modern Spanish (2019)")
                       :title)
            :to-equal "A new reference grammar of modern Spanish")))

(describe "book-citekey"
  (it "keys family-first names on the first word"
    (expect (book-citekey (book-metadata
                           "Halloway S., Bedra A. - Programming Clojure, 2nd edition - 2012"))
            :to-equal "halloway_2012")
    (expect (book-citekey (book-metadata
                           "White R. T., Ray A. T. - Practical Discrete Mathematics - 2021"))
            :to-equal "white_2021"))

  (it "drops initials before choosing, so a key never lands on one"
    ;; `Knuth D.E.' keyed on the last word would give `de'
    (expect (book-citekey (book-metadata
                           "Knuth D.E. - The Art of Computer Programming - 1997"))
            :to-equal "knuth_1997"))

  (it "keys given-name-first names on the last word"
    (expect (book-citekey (book-metadata
                           "Robert M. Sapolsky - Why Zebras Don't Get Ulcers - 2004"))
            :to-equal "sapolsky_2004")
    (expect (book-citekey (book-metadata
                           "A Short History of Nearly Everything by Bill Bryson"))
            :to-equal "bryson"))

  (it "falls back to a slug of the title when no author is credited"
    (expect (book-citekey (book-metadata "ANAYA_Gramatica_B1"))
            :to-equal "anaya_gramatica_b1"))

  (it "keeps a slug legal and bounded"
    (let ((key (book-citekey (book-metadata
                              "A Mind for Numbers - How to Excel at Math and Science"))))
      (expect key :to-match "\\`[a-z0-9_]+\\'")
      (expect (length key) :to-be-less-than 41))))

(describe "book-works"
  (let (root)
    (before-each
      (setq root (make-temp-file "books" t))
      ;; A shelf directory of loose books, a book directory shipping two
      ;; formats plus code samples, and a solutions dump below the walk
      (make-directory (expand-file-name "Rust/Abhishek Chanda - Network Programming with Rust - 2018/code" root) t)
      (make-directory (expand-file-name "Math/Rosen/Full Solutions/Chapter 5.1" root) t)
      (dolist (path '("A Mind for Numbers - How to Excel at Math and Science.pdf"
                      "Rust/Blandy J., Orendorff J. - Programming Rust - 2018.pdf"
                      "Rust/Abhishek Chanda - Network Programming with Rust - 2018/Abhishek Chanda - Network Programming with Rust - 2018.pdf"
                      "Rust/Abhishek Chanda - Network Programming with Rust - 2018/Abhishek Chanda - Network Programming with Rust - 2018.epub"
                      "Rust/Abhishek Chanda - Network Programming with Rust - 2018/code/main.rs"
                      "Math/Rosen/Full Solutions/Chapter 5.1/exercise-1.pdf"))
        (let ((full (expand-file-name path root)))
          (make-directory (file-name-directory full) t)
          (write-region "" nil full nil 'silent))))
    (after-each (delete-directory root t))

    (it "collapses a work's formats into one entry"
      (let ((works (book-works root)))
        (expect (length works) :to-equal 3)
        (expect (seq-count (lambda (w)
                             (string-match-p "Network Programming" (plist-get w :name)))
                           works)
                :to-equal 1)))

    (it "prefers the PDF, which pdf-tools restores a position in"
      (let ((work (seq-find (lambda (w)
                              (string-match-p "Network Programming" (plist-get w :name)))
                            (book-works root))))
        (expect (plist-get work :file) :to-match "\\.pdf\\'")))

    (it "ignores code shipped beside a book"
      (expect (seq-find (lambda (w) (equal "main" (plist-get w :name)))
                        (book-works root))
              :to-be nil))

    (it "stays out of a solutions dump nested below the walk"
      ;; One textbook contributes 4764 PDFs five levels down; unbounded,
      ;; the walk would read every exercise as its own book
      (expect (seq-find (lambda (w) (string-match-p "exercise" (plist-get w :name)))
                        (book-works root))
              :to-be nil))))

(describe "book-bibtex-entry"
  (it "renders every field the picker shows"
    (let ((entry (book-bibtex-entry
                  '(:name "Halloway S., Bedra A. - Programming Clojure, 2nd edition - 2012"
                    :file "/tmp/books/Clojure/programming-clojure.pdf")
                  "halloway_2012")))
      (expect entry :to-match "\\`@book{halloway_2012,")
      (expect (bibtex-entry-field entry "title")
              :to-equal "Programming Clojure, 2nd edition")
      (expect (bibtex-entry-field entry "author") :to-equal "Halloway S., Bedra A.")
      (expect (bibtex-entry-field entry "year") :to-equal "2012")
      (expect (bibtex-entry-field entry "file")
              :to-equal "/tmp/books/Clojure/programming-clojure.pdf")))

  (it "omits the fields the filename does not carry"
    (let ((entry (book-bibtex-entry '(:name "Bash Quick Reference"
                                      :file "/tmp/books/bash.pdf")
                                    "bash_quick_reference")))
      (expect (bibtex-entry-field entry "author") :to-be nil)
      (expect (bibtex-entry-field entry "year") :to-be nil)
      (expect (bibtex-entry-field entry "title") :to-equal "Bash Quick Reference")))

  (it "drops braces that would unbalance the entry"
    (let ((entry (book-bibtex-entry
                  '(:name "144715486X Logic Programming with Prolog {8EDB06BF}"
                    :file "/tmp/books/prolog.pdf")
                  "prolog")))
      (expect (bibtex-entry-field entry "title") :not :to-match "[{}]")
      (expect (bibtex-entry-field entry "file") :to-equal "/tmp/books/prolog.pdf"))))

(describe "import-books"
  (let (root bib)
    (before-each
      (setq root (make-temp-file "books" t)
            bib (expand-file-name "books.bib" root))
      (dolist (path '("Halloway S., Bedra A. - Programming Clojure, 2nd edition - 2012.pdf"
                      "A Mind for Numbers - How to Excel at Math and Science.pdf"))
        (write-region "" nil (expand-file-name path root) nil 'silent)))
    (after-each (delete-directory root t))

    (it "writes one entry per work"
      (let ((books-bibliography bib) (books-directory root))
        (import-books root)
        (expect (sort (bibliography-keys bib) #'string<)
                :to-equal '("a_mind_for_numbers_how_to_excel_at_math" "halloway_2012"))))

    (it "adds nothing on a second run"
      ;; The run is repeatable because a recorded file is passed over
      (let ((books-bibliography bib) (books-directory root))
        (import-books root)
        (import-books root)
        (expect (length (bibliography-keys bib)) :to-equal 2)))

    (it "leaves the paper bibliography alone"
      (let ((books-bibliography bib)
            (books-directory root)
            (papers-bibliography (expand-file-name "references.bib" root)))
        (import-books root)
        (expect (file-exists-p papers-bibliography) :to-be nil)))))

;;; books-tests.el ends here
