;;; tests/pdf/emureader-tests.el --- pdf/autoload/emureader.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; emureader isn't installed in the batch tier.
(defvar save-place-alist nil)
(defvar emureader-current-doc-pagecount nil)

(load-module-file "modules/pdf/autoload/emureader.el")

(describe "keep-pdf-viewer"
  (it "drops emureader's PDF entry and keeps the rest"
    (let ((auto-mode-alist '(("\\.pdf\\'" . emureader-mode)
                             ("\\.epub\\'" . emureader-mode)
                             ("\\.pdf\\'" . pdf-view-mode))))
      (keep-pdf-viewer)
      (expect auto-mode-alist
              :to-equal '(("\\.epub\\'" . emureader-mode)
                          ("\\.pdf\\'" . pdf-view-mode))))))

(defun emureader-tests--toggle-from (mode)
  "Run `toggle-emureader' in a buffer in MODE; return what `find-file' saw."
  (let (seen)
    (with-temp-buffer
      (setq major-mode mode
            buffer-file-name "/tmp/book.pdf")
      (cl-letf (((symbol-function 'kill-buffer) #'ignore)
                ((symbol-function 'find-file)
                 (lambda (file)
                   (setq seen (list :file file
                                    :remap major-mode-remap-alist
                                    :magic magic-mode-alist
                                    :auto auto-mode-alist)))))
        (let ((major-mode-remap-alist '((pdf-view-mode . emureader-mode)))
              (magic-mode-alist '(("%PDF" . pdf-view-mode))))
          (toggle-emureader)))
      (setq buffer-file-name nil))
    seen))

(describe "toggle-emureader"
  (it "forces emureader on a document in another viewer"
    (let ((seen (emureader-tests--toggle-from 'pdf-view-mode)))
      (expect (plist-get seen :file) :to-equal "/tmp/book.pdf")
      (expect (plist-get seen :magic) :to-be nil)
      (expect (plist-get seen :auto) :to-equal '(("" . emureader-mode)))))

  (it "reopens out of emureader without the remap"
    (let ((seen (emureader-tests--toggle-from 'emureader-mode)))
      (expect (plist-get seen :file) :to-equal "/tmp/book.pdf")
      (expect (plist-get seen :remap) :to-be nil)
      (expect (plist-get seen :magic) :to-equal '(("%PDF" . pdf-view-mode)))))

  (it "refuses a buffer that visits no file"
    (with-temp-buffer
      (expect (toggle-emureader) :to-throw 'user-error))))

(defun emureader-tests--fit (window-width width-after-height-fit)
  "Fits `fit-emureader-page' runs for a page WIDTH-AFTER-HEIGHT-FIT wide."
  (let (fits)
    (cl-letf (((symbol-function 'emureader-fit-to-height)
               (lambda () (push 'height fits)))
              ((symbol-function 'emureader-fit-to-width)
               (lambda () (push 'width fits)))
              ((symbol-function 'emureader--get-current-doc-image-size)
               (lambda (&rest _) (cons width-after-height-fit 1000)))
              ((symbol-function 'window-body-width)
               (lambda (&rest _) window-width)))
      (fit-emureader-page))
    (nreverse fits)))

(describe "fit-emureader-page"
  (it "keeps the height fit when the page is narrower than the window"
    (expect (emureader-tests--fit 1200 800) :to-equal '(height)))

  (it "falls back to the width fit when the height fit overflows the window"
    (expect (emureader-tests--fit 600 800) :to-equal '(height width))))

(describe "show-emureader-progress"
  (it "reports the page as a percentage of the document"
    (let ((emureader-current-doc-pagecount 10))
      (cl-letf (((symbol-function 'emureader-current-doc-pagenumber)
                 (lambda (&rest _) 5)))
        (expect (show-emureader-progress) :to-equal "at 50.0%")))))

(defun emureader-tests--page-text (output status)
  "Call `current-emureader-page-text' on page 7 while mutool prints OUTPUT.
mutool exits with STATUS; return the text and mutool's arguments."
  (let (args)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/book.epub")
      (unwind-protect
          (cl-letf (((symbol-function 'emureader-current-doc-pagenumber)
                     (lambda (&rest _) 7))
                    ((symbol-function 'call-process)
                     (lambda (_program _infile _destination _display &rest process-args)
                       (setq args process-args)
                       (insert output)
                       status)))
            (list :text (current-emureader-page-text) :args args))
        (setq buffer-file-name nil)))))

(describe "current-emureader-page-text"
  (it "reads the shown page dehyphenated, each paragraph on one line"
    (let ((got (emureader-tests--page-text
                (concat "I\n\n"
                        "Llegaron al canal \n  por la brecha\nque sube.\n \n"
                        "Eran cinco.\n\n\f\n")
                0)))
      (expect (plist-get got :args)
              :to-equal '("draw" "-q" "-F" "txt" "-O" "dehyphenate"
                          "-o" "-" "/tmp/book.epub" "7"))
      (expect (plist-get got :text)
              :to-equal "I\n\nLlegaron al canal por la brecha que sube.\n\nEran cinco.")))

  (it "signals when mutool cannot read the page"
    (expect (emureader-tests--page-text "" 1) :to-throw 'error)))

(describe "emureader-saveplace-skip-foreign-a"
  (let ((file "/tmp/book.pdf"))
    (it "lets emureader read its own record"
      (with-temp-buffer
        (setq buffer-file-name file)
        (let ((save-place-alist
               `((,file . [("emureader-saveplace" (filename . ,file) (page . 4))]))))
          (expect (emureader-saveplace-skip-foreign-a) :to-be-truthy))
        (setq buffer-file-name nil)))

    (it "skips saveplace-pdf-view's record, which it would read as a bookmark"
      (with-temp-buffer
        (setq buffer-file-name file)
        (let ((save-place-alist
               `((,file . [((pdf-view-bookmark . ("x" (page . 4))))]))))
          (expect (emureader-saveplace-skip-foreign-a) :to-be nil))
        (setq buffer-file-name nil)))

    (it "skips a plain position and a missing record"
      (with-temp-buffer
        (setq buffer-file-name file)
        (let ((save-place-alist `((,file . 1234))))
          (expect (emureader-saveplace-skip-foreign-a) :to-be nil))
        (let ((save-place-alist nil))
          (expect (emureader-saveplace-skip-foreign-a) :to-be nil))
        (setq buffer-file-name nil)))))

(describe "skip-large-file-prompt-for-documents-a"
  (it "asks only about files no document viewer opens"
    (let ((auto-mode-alist '(("\\.pdf\\'" . pdf-view-mode)
                             ("\\.txt\\'" . text-mode))))
      (expect (skip-large-file-prompt-for-documents-a 1 "open" "SCAN.PDF") :to-be nil)
      (expect (skip-large-file-prompt-for-documents-a 1 "open" "notes.txt") :to-be t))))

;;; emureader-tests.el ends here
