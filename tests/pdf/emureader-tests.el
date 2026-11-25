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

(describe "keep-pdf-and-epub-viewers"
  (it "drops emureader's PDF and EPUB entries and keeps the rest"
    (let ((auto-mode-alist '(("\\.pdf\\'" . emureader-mode)
                             ("\\.epub\\'" . emureader-mode)
                             ("\\.cbz\\'" . emureader-mode)
                             ("\\.epub\\'" . nov-mode)
                             ("\\.pdf\\'" . pdf-view-mode))))
      (keep-pdf-and-epub-viewers)
      (expect auto-mode-alist
              :to-equal '(("\\.cbz\\'" . emureader-mode)
                          ("\\.epub\\'" . nov-mode)
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

;;; emureader-tests.el ends here
