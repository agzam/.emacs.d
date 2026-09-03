;;; tests/papers/notes-tests.el --- citar/vulpea notes bridge specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/papers/autoload/notes.el")
;; The note title needs `bibtex-author-family', which import.el owns; at
;; runtime the module's loaddefs supply it.
(load-module-file "modules/papers/autoload/import.el")

;; vulpea is absent in the batch tier; the bridge only reaches it at runtime.
;; A note here is a plist standing in for a `vulpea-note' struct.
(defun stub-note (id title properties)
  (list :id id :title title :properties properties))

(defmacro with-stub-vulpea (notes &rest body)
  "Run BODY with the vulpea accessors answering from NOTES."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'vulpea-note-id) (lambda (n) (plist-get n :id)))
             ((symbol-function 'vulpea-note-title) (lambda (n) (plist-get n :title)))
             ((symbol-function 'vulpea-note-properties)
              (lambda (n) (plist-get n :properties)))
             ((symbol-function 'vulpea-db-query-by-property-key)
              (lambda (key)
                (when (equal key "ROAM_REFS")
                  (seq-filter (lambda (n) (assoc "ROAM_REFS" (plist-get n :properties)))
                              ,notes))))
             ((symbol-function 'vulpea-db-get-by-id)
              (lambda (id) (seq-find (lambda (n) (equal id (plist-get n :id))) ,notes))))
     ,@body))

(describe "citekeys-in-roam-refs"
  (it "reads the org-cite spelling"
    (expect (citekeys-in-roam-refs "@hoare_1978") :to-equal '("hoare_1978")))

  (it "reads the bracketed spelling"
    (expect (citekeys-in-roam-refs "[cite:@wadler_1989]") :to-equal '("wadler_1989")))

  (it "reads the bare cite: spelling org-roam wrote before org-cite"
    (expect (citekeys-in-roam-refs "cite:huet_1997") :to-equal '("huet_1997")))

  (it "reads several refs from one property"
    (expect (citekeys-in-roam-refs "@hoare_1978 @milner_1980")
            :to-equal '("hoare_1978" "milner_1980")))

  (it "ignores the plain URLs the other 457 notes carry"
    ;; ROAM_REFS is already in use for web pages; those must not become citekeys
    (expect (citekeys-in-roam-refs "https://en.wikipedia.org/wiki/Polysemy") :to-be nil))

  (it "picks the citekey out of a mixed property"
    (expect (citekeys-in-roam-refs "https://example.com/x @baez_2009")
            :to-equal '("baez_2009")))

  (it "tolerates a missing property"
    (expect (citekeys-in-roam-refs nil) :to-be nil)))

(describe "paper-notes-by-citekey"
  (let ((notes (list (stub-note "id-1" "Hoare (1978) CSP"
                                '(("ROAM_REFS" . "@hoare_1978")))
                     (stub-note "id-2" "Zipper"
                                '(("ROAM_REFS" . "@huet_1997")))
                     (stub-note "id-3" "Second take on CSP"
                                '(("ROAM_REFS" . "@hoare_1978")))
                     (stub-note "id-4" "Polysemy"
                                '(("ROAM_REFS" . "https://en.wikipedia.org/wiki/Polysemy")))
                     (stub-note "id-5" "No refs at all" nil))))

    (it "maps each citekey to the notes carrying it"
      (with-stub-vulpea notes
        (let ((table (paper-notes-by-citekey)))
          (expect (sort (gethash "hoare_1978" table) #'string<)
                  :to-equal '("id-1" "id-3"))
          (expect (gethash "huet_1997" table) :to-equal '("id-2")))))

    (it "keeps URL refs out of the table"
      (with-stub-vulpea notes
        (expect (hash-table-count (paper-notes-by-citekey)) :to-equal 2)))

    (it "restricts the table to the requested citekeys"
      (with-stub-vulpea notes
        (let ((table (paper-notes-by-citekey '("huet_1997"))))
          (expect (gethash "huet_1997" table) :to-equal '("id-2"))
          (expect (gethash "hoare_1978" table) :to-be nil))))))

(describe "paper-notes-predicate"
  (it "returns a predicate answering for known citekeys"
    (with-stub-vulpea (list (stub-note "id-1" "CSP" '(("ROAM_REFS" . "@hoare_1978"))))
      (let ((has-notes (paper-notes-predicate)))
        (expect (functionp has-notes) :to-be t)
        (expect (funcall has-notes "hoare_1978") :to-be t)
        (expect (funcall has-notes "wadler_1989") :to-be nil))))

  (it "returns nil when no note carries a citekey"
    ;; citar reads nil as \"this source has nothing\" and skips the indicator
    (with-stub-vulpea (list (stub-note "id-4" "Polysemy"
                                       '(("ROAM_REFS" . "https://example.com"))))
      (expect (paper-notes-predicate) :to-be nil))))

(describe "paper-note-title"
  (it "turns a note id into its title, since citar shows what :transform returns"
    (with-stub-vulpea (list (stub-note "id-1" "Hoare (1978) CSP" nil))
      (expect (paper-note-title "id-1") :to-equal "Hoare (1978) CSP")))

  (it "falls back to the id when the note is gone"
    (with-stub-vulpea nil
      (expect (paper-note-title "id-missing") :to-equal "id-missing"))))

(describe "paper-note-create"
  ;; Every author fixture here is copied out of references.bib.  The
  ;; original spec invented `"Hoare"' as the whole author field, which no
  ;; BibTeX entry looks like, so titling a note with the field looked right.
  (defmacro with-captured-note (fields &rest body)
    "Run BODY with citar answering from FIELDS and `vulpea-create' captured."
    (declare (indent 1))
    `(let (captured)
       (cl-letf (((symbol-function 'citar-get-value)
                  (lambda (field _entry) (cdr (assoc field ,fields))))
                 ((symbol-function 'vulpea-create)
                  (lambda (title &optional _file-name &rest args)
                    (setq captured (cons title args))
                    '(:id "new")))
                 ((symbol-function 'vulpea-note-id) (lambda (_) "new"))
                 ((symbol-function 'vulpea-visit) #'ignore))
         ,@body)))

  (it "records the citekey in ROAM_REFS and the PDF in NOTER_DOCUMENT"
    (with-captured-note
        '(("title" . "Seven Sketches in Compositionality")
          ("author" . "Fong, Brendan and Spivak, David I")
          ("year" . "2018")
          ("file" . "/tmp/papers/seven-sketches.pdf"))
      (paper-note-create "fong_2018" nil)
      (let ((properties (plist-get (cdr captured) :properties)))
        (expect (car captured) :to-equal
                "Fong (2018) Seven Sketches in Compositionality")
        (expect (cdr (assoc "ROAM_REFS" properties)) :to-equal "@fong_2018")
        (expect (cdr (assoc "NOTER_DOCUMENT" properties))
                :to-equal "/tmp/papers/seven-sketches.pdf")
        (expect (plist-get (cdr captured) :tags) :to-equal '("paper")))))

  (it "titles the note with the first author, not the whole author field"
    ;; The four-author field titled this note "van de Meent, Jan-Willem and
    ;; Paige, Brooks and Yang, Hongseok and Wood, Frank (2018) ..."
    (with-captured-note
        '(("title" . "An Introduction to Probabilistic Programming")
          ("author" . "van de Meent, Jan-Willem and Paige, Brooks and Yang, Hongseok and Wood, Frank")
          ("year" . "2018"))
      (paper-note-create "van_2018" nil)
      (expect (car captured) :to-equal
              "van de Meent (2018) An Introduction to Probabilistic Programming")))

  (it "handles an author field written given-name first"
    (with-captured-note
        '(("title" . "Bringing the web up to speed with WebAssembly")
          ("author" . "Andreas Haas and Andreas Rossberg and Derek L. Schuff and Ben Titzer")
          ("year" . "2017"))
      (paper-note-create "haas17_bring_webas" nil)
      (expect (car captured) :to-equal
              "Haas (2017) Bringing the web up to speed with WebAssembly")))

  (it "omits NOTER_DOCUMENT when the entry records no file"
    (let (captured)
      (cl-letf (((symbol-function 'citar-get-value)
                 (lambda (field _entry) (when (equal field "title") "Untitled")))
                ((symbol-function 'vulpea-create)
                 (lambda (title &optional _file-name &rest args)
                   (setq captured (cons title args))
                   '(:id "new")))
                ((symbol-function 'vulpea-note-id) (lambda (_) "new"))
                ((symbol-function 'vulpea-visit) #'ignore))
        (paper-note-create "k" nil)
        (let ((properties (plist-get (cdr captured) :properties)))
          (expect (cdr (assoc "ROAM_REFS" properties)) :to-equal "@k")
          (expect (assoc "NOTER_DOCUMENT" properties) :to-be nil))))))

;;; notes-tests.el ends here
