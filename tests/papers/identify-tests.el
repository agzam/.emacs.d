;;; tests/papers/identify-tests.el --- paper identification specs -*- lexical-binding: t; -*-

;; Every fixture here is real: page text captured from the corpus with
;; `mutool draw -F txt', filenames copied out of ~/SyncMobile/Papers, and
;; author fields in the form an entry actually carries.  A fixture no real
;; input resembles cannot fail the way real data does - that is how the
;; round-1 note-title defect shipped, on an author field reading "Hoare".

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; config.el owns these; the autoload files only read them at runtime.
(defvar papers-bibliography "/tmp/papers/references.bib")
(defvar papers-directory "/tmp/papers/")

(load-module-file "modules/papers/autoload/import.el")
(load-module-file "modules/papers/autoload/identify.el")

(defconst identify-tests--hoare-text
  "Programming \nTeclmiques \n\n\n\nS. L. Graham, R. L. Rivest \nEditors \nCommunicating \nSequential Processes \n\nC.A.R. Hoare \nThe Queen's University \nBelfast, Northern Ireland \n"
  "Page one of Hoare78.pdf, where the banner and the editors print above the title.")

(defconst identify-tests--wadler-text
  "\uFFFD\n\uFFFD\n\uFFFD\n\uFFFD\uFFFD\uFFFD\uFFFD !#\"%$'&\uFFFD\uFFFD\n()&&*+\n,-/.102.13547698:02;=<6?>@8BA!CD;=3/-/;=>FEG02H9CIC\n"
  "Page one of `wadler88-adhoc polymorphism.pdf', whose text layer is broken.")

(describe "paper-prompt-text"
  (it "collapses the blank runs a page-text extract is full of"
    (expect (paper-prompt-text identify-tests--hoare-text)
            :not :to-match "\n\n\n"))

  (it "sends the title page and stops"
    (let ((paper-identification-text-limit 40))
      (expect (length (paper-prompt-text identify-tests--hoare-text))
              :to-equal 40)))

  (it "survives a paper with no text layer at all"
    (expect (paper-prompt-text nil) :to-equal "")))

(describe "paper-identification-prompt"
  (it "sends the filename beside the text"
    ;; The only signal wadler88 carries: its text extracts as glyphs, and
    ;; the name identifies Wadler and Blott to a model on its own
    (let ((prompt (paper-identification-prompt
                   "/Users/ryl/SyncMobile/Papers/wadler88-adhoc polymorphism.pdf"
                   identify-tests--wadler-text)))
      (expect prompt :to-match "wadler88-adhoc polymorphism\\.pdf")
      (expect prompt :not :to-match "/Users/ryl"))))

(describe "paper-identification-parse"
  (it "reads the five lines of a well-formed answer"
    (let ((metadata (paper-identification-parse
                     (concat "title: Communicating Sequential Processes\n"
                             "authors: Hoare, C. A. R.\n"
                             "year: 1978\n"
                             "venue: Communications of the ACM\n"
                             "identified: yes\n"))))
      (expect (plist-get metadata :title)
              :to-equal "Communicating Sequential Processes")
      (expect (plist-get metadata :author) :to-equal "Hoare, C. A. R.")
      (expect (plist-get metadata :year) :to-equal "1978")
      (expect (plist-get metadata :venue) :to-equal "Communications of the ACM")
      (expect (plist-get metadata :identified) :to-be-truthy)))

  (it "reads lines a model wrapped in markdown or reordered"
    (let ((metadata (paper-identification-parse
                     (concat "**identified**: yes\n"
                             "- title: How Do Committees Invent?\n"
                             "  authors: Conway, Melvin E.\n"
                             "year: 1968\n"))))
      (expect (plist-get metadata :title) :to-equal "How Do Committees Invent?")
      (expect (plist-get metadata :author) :to-equal "Conway, Melvin E.")
      (expect (plist-get metadata :year) :to-equal "1968")))

  (it "takes the year out of a value carrying more than one"
    (expect (plist-get (paper-identification-parse "year: 1978 (August issue)")
                       :year)
            :to-equal "1978"))

  (it "treats an empty or evasive value as no value"
    (let ((metadata (paper-identification-parse
                     "title: The Emperor's Old Clothes\nvenue:\nyear: unknown\n")))
      (expect (plist-get metadata :venue) :to-be nil)
      (expect (plist-get metadata :year) :to-be nil)))

  (it "keeps a title the model read while refusing to name the paper"
    ;; Reading a title off page one is the easier half; the review buffer
    ;; marks these rather than throwing the title away
    (let ((metadata (paper-identification-parse
                     "title: Report on a Knowledge-Based Software Assistant\nidentified: no\n")))
      (expect (plist-get metadata :title) :to-be-truthy)
      (expect (plist-get metadata :identified) :to-be nil)))

  (it "is not identified when it names nothing, whatever it claims"
    (expect (plist-get (paper-identification-parse "title:\nidentified: yes\n")
                       :identified)
            :to-be nil))

  (it "returns nothing for a failed request"
    (expect (paper-identification-parse nil) :to-be nil)))

(describe "paper-fallback-title"
  (it "reads a filename that carries words"
    (expect (paper-fallback-title
             "/Users/ryl/SyncMobile/Papers/Out-of-the-tar-pit_Ben-Moseley_2006.pdf")
            :to-equal "Out of the tar pit Ben Moseley 2006"))

  (it "keeps a filename that carries none, which is still findable"
    ;; A submission id and a tech report number: no title to recover
    (expect (paper-fallback-title
             "/Users/ryl/SyncMobile/Papers/83-Article Text-110-1-10-20191031.pdf")
            :to-equal "83 Article Text 110 1 10 20191031")
    (expect (paper-fallback-title "/Users/ryl/SyncMobile/Papers/CMU-CS-95-113.pdf")
            :to-equal "CMU CS 95 113")))

(describe "paper-bibtex-entry"
  (it "writes an article when the venue is known"
    (let ((entry (paper-bibtex-entry
                  '(:title "Communicating Sequential Processes"
                    :author "Hoare, C. A. R." :year "1978"
                    :venue "Communications of the ACM"))))
      (expect entry :to-match "\\`@article{paper,")
      (expect entry :to-match "journal = {Communications of the ACM}")))

  (it "writes a misc when it is not, rather than inventing one"
    (let ((entry (paper-bibtex-entry '(:title "How Do Committees Invent?"))))
      (expect entry :to-match "\\`@misc{paper,")
      (expect entry :not :to-match "journal")
      (expect entry :not :to-match "author")
      (expect entry :not :to-match "year")))

  (it "strips what would unbalance the entry"
    ;; Models hand back TeX: a stray brace ends the entry early and takes
    ;; every later one with it
    (expect (paper-bibtex-entry
             '(:title "Fast and Loose Reasoning is Morally Correct \\emph{again} {sic}"))
            :to-match "title = {Fast and Loose Reasoning is Morally Correct emphagain sic}")))

(describe "paper-identification-entry"
  (it "keys the entry on the family name and year, and records the file"
    (let ((entry (paper-identification-entry
                  '(:title "An Introduction to Probabilistic Programming"
                    :author "van de Meent, Jan-Willem and Paige, Brooks and Yang, Hongseok and Wood, Frank"
                    :year "2018" :identified t)
                  "/Users/ryl/SyncMobile/Papers/probabilistic_programming.pdf"
                  '(:files nil :dois nil :keys nil))))
      (expect entry :to-match "@misc{vandemeent_2018,")
      (expect entry :to-match
              "file = {/Users/ryl/SyncMobile/Papers/probabilistic_programming\\.pdf}")))

  (it "steps around a key the run already took"
    (let ((entry (paper-identification-entry
                  '(:title "The Emperor's Old Clothes" :author "Hoare, C. A. R."
                    :year "1981" :identified t)
                  "/Users/ryl/SyncMobile/Papers/hoare81emperor.pdf"
                  '(:files nil :dois nil :keys ("hoare_1981")))))
      (expect entry :to-match "@misc{hoare_1981a,")))

  (it "keys on the title when the model credits no author"
    (let ((entry (paper-identification-entry
                  '(:title "Out of the Tar Pit" :identified t)
                  "/Users/ryl/SyncMobile/Papers/Out-of-the-tar-pit_Ben-Moseley_2006.pdf"
                  '(:files nil :dois nil :keys nil))))
      (expect entry :to-match "@misc{out_of_the_tar_pit,")))

  (it "falls back to the filename when the request brings nothing back"
    ;; The one outcome this pass exists to prevent is a paper the picker
    ;; cannot see, so a failed request still writes an entry
    (let ((entry (paper-identification-entry
                  nil "/Users/ryl/SyncMobile/Papers/CMU-CS-95-113.pdf"
                  '(:files nil :dois nil :keys nil))))
      (expect entry :to-match "title = {CMU CS 95 113}")
      (expect entry :to-match "file = {/Users/ryl/SyncMobile/Papers/CMU-CS-95-113\\.pdf}"))))

(describe "paper-identification-request"
  (after-each
    (when-let* ((buffer (get-buffer " *paper identification*")))
      (kill-buffer buffer)))

  (it "answers once, whatever else the backend sends"
    ;; A run over the corpus produced 51 proposals covering 26 papers: one
    ;; request called back twice, each call continued the walk, and the two
    ;; walks then shared the list of proposals
    (let (answers)
      (cl-letf (((symbol-function 'paper-text) (lambda (&rest _) ""))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (let ((callback (plist-get args :callback)))
                     (funcall callback '(reasoning . "which paper is this") nil)
                     (funcall callback "title: How Do Committees Invent?\nidentified: yes" nil)
                     (funcall callback t nil)))))
        (paper-identification-request "/tmp/papers/committees.pdf"
                                      (lambda (metadata) (push metadata answers))))
      (expect (length answers) :to-equal 1)
      (expect (plist-get (car answers) :title) :to-equal "How Do Committees Invent?")))

  (it "answers with nothing when the request fails"
    (let (answers)
      (cl-letf (((symbol-function 'paper-text) (lambda (&rest _) ""))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (funcall (plist-get args :callback) nil '(:status "404")))))
        (paper-identification-request "/tmp/papers/committees.pdf"
                                      (lambda (metadata) (push metadata answers))))
      (expect answers :to-equal '(nil))))

  (it "does not stream, so a string response is a whole one"
    (let (sent)
      (cl-letf (((symbol-function 'paper-text) (lambda (&rest _) ""))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args) (setq sent args))))
        (paper-identification-request "/tmp/papers/committees.pdf" #'ignore))
      (expect (plist-get sent :stream) :to-be nil)
      (expect (buffer-local-value 'gptel-stream (get-buffer " *paper identification*"))
              :to-be nil))))

(describe "identify-papers-step"
  (it "asks one paper at a time and keeps the answers in that order"
    (let (shown)
      (cl-letf (((symbol-function 'paper-identification-request)
                 (lambda (file callback)
                   (funcall callback (list :title (file-name-base file) :identified t))))
                ((symbol-function 'paper-review-show)
                 (lambda (proposals) (setq shown proposals))))
        (identify-papers-step '("/tmp/papers/Hoare78.pdf"
                                "/tmp/papers/committees.pdf"
                                "/tmp/papers/huet-zipper.pdf")
                              '(:files nil :dois nil :keys nil) nil 3))
      (expect (mapcar (lambda (p) (file-name-nondirectory (plist-get p :file))) shown)
              :to-equal '("Hoare78.pdf" "committees.pdf" "huet-zipper.pdf"))))

  (it "keys each entry against the ones the run has already proposed"
    ;; The index is threaded through the walk rather than re-read: two
    ;; papers by the same author and year would otherwise share a key
    (let (shown)
      (cl-letf (((symbol-function 'paper-identification-request)
                 (lambda (_file callback)
                   (funcall callback '(:title "Communicating Sequential Processes"
                                       :author "Hoare, C. A. R." :year "1978"
                                       :identified t))))
                ((symbol-function 'paper-review-show)
                 (lambda (proposals) (setq shown proposals))))
        (identify-papers-step '("/tmp/papers/Hoare78.pdf" "/tmp/papers/hoare81emperor.pdf")
                              '(:files nil :dois nil :keys nil) nil 2))
      (expect (plist-get (nth 0 shown) :entry) :to-match "@misc{hoare_1978,")
      (expect (plist-get (nth 1 shown) :entry) :to-match "@misc{hoare_1978a,"))))

(describe "papers-without-entry"
  (let (root bib)
    (before-each
      (setq root (file-name-as-directory (make-temp-file "papers" t))
            bib (expand-file-name "references.bib" root))
      (dolist (name '("Hoare78.pdf" "committees.pdf" "huet-zipper.pdf"))
        (write-region "" nil (expand-file-name name root) nil 'silent))
      (with-temp-file bib
        (insert "@article{hoare_1978,\n  title = {Communicating Sequential Processes},\n"
                (format "  file = {%s}\n}\n" (expand-file-name "Hoare78.pdf" root)))))
    (after-each (delete-directory root t))

    (it "leaves out the papers an entry already points at"
      (let* ((papers-bibliography bib)
             (names (mapcar #'file-name-nondirectory (papers-without-entry root))))
        (expect (sort names #'string<)
                :to-equal '("committees.pdf" "huet-zipper.pdf"))))))

(describe "paper-review-entries"
  (it "reads back what the buffer holds, comments and all"
    (with-temp-buffer
      (insert "% 2 entries proposed\n\n"
              "% Hoare78.pdf\n"
              "@article{hoare_1978,\n  title = {Communicating Sequential Processes},\n"
              "  journal = {Communications of the ACM}\n}\n\n"
              "% committees.pdf   <- unsure, check the title\n"
              "@misc{conway_1968,\n  title = {How Do Committees Invent?}\n}\n")
      (let ((entries (paper-review-entries)))
        (expect (length entries) :to-equal 2)
        (expect (car entries) :to-match "\\`@article{hoare_1978,")
        (expect (car entries) :to-match "}\\'")
        (expect (nth 1 entries) :to-match "How Do Committees Invent"))))

  (it "reads an entry whose title carries an apostrophe or a quote"
    ;; A quote opening a string in the buffer's own syntax table would
    ;; swallow every entry after this one
    (with-temp-buffer
      (insert "@misc{hoare_1981,\n  title = {The Emperor's \"Old\" Clothes}\n}\n\n"
              "@misc{moggi_1991,\n  title = {Notions of Computation and Monads}\n}\n")
      (expect (length (paper-review-entries)) :to-equal 2)))

  (it "returns only what is left after a deletion"
    (with-temp-buffer
      (insert "% committees.pdf\n@misc{conway_1968,\n  title = {How Do Committees Invent?}\n}\n")
      (expect (length (paper-review-entries)) :to-equal 1)
      (erase-buffer)
      (insert "% committees.pdf\n")
      (expect (paper-review-entries) :to-be nil))))

;;; identify-tests.el ends here
