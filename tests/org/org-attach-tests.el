;;; tests/org/org-attach-tests.el --- org/autoload/org-attach.el specs -*- lexical-binding: t; -*-
;; yank-from-clipboard needs a GUI clipboard - smoke-only.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/org/autoload/org-attach.el")

(describe "yank-media--tiff-as-png-a"
  (it "converts tiff payloads to png before delegating"
    (let (seen)
      (cl-letf (((symbol-function 'shell-command-on-region)
                 (lambda (beg end _cmd &optional _buf _no-mark)
                   (delete-region beg end)
                   (insert "PNGDATA"))))
        (yank-media--tiff-as-png-a
         (lambda (mimetype data) (setq seen (list mimetype data)))
         "image/tiff" "TIFFDATA"))
      (expect seen :to-equal '("image/png" "PNGDATA"))))
  (it "passes other mimetypes through untouched"
    (let (seen)
      (yank-media--tiff-as-png-a
       (lambda (mimetype data) (setq seen (list mimetype data)))
       "image/png" "RAW")
      (expect seen :to-equal '("image/png" "RAW")))))

(describe "org-attach-file-and-insert-link"
  (it "refuses to run outside org-mode"
    (with-temp-buffer
      (setq major-mode 'fundamental-mode)
      (expect (org-attach-file-and-insert-link "x") :to-throw 'user-error)))

  (it "routes a base64 png data uri to org-download-dnd-base64"
    (let (seen)
      (with-fake-feature 'org-download
        (cl-letf (((symbol-function 'org-download-dnd-base64)
                   (lambda (path _) (setq seen (list 'b64 path)))))
          (with-temp-buffer
            (setq major-mode 'org-mode)
            (org-attach-file-and-insert-link "data:image/png;base64,AAAA"))))
      (expect seen :to-equal '(b64 "data:image/png;base64,AAAA"))))

  (it "routes an image url to org-download-image"
    (let (seen)
      (with-fake-feature 'org-download
        (cl-letf (((symbol-function 'org-download-image)
                   (lambda (uri) (setq seen (list 'img uri)))))
          (with-temp-buffer
            (setq major-mode 'org-mode)
            (org-attach-file-and-insert-link "https://ex.com/pic.png"))))
      (expect seen :to-equal '(img "https://ex.com/pic.png"))))

  (it "downloads a non-image url then inserts a link"
    (let (copied linked)
      (with-fake-feature 'org-download
        (cl-letf (((symbol-function 'org-download--fullname)
                   (lambda (uri) (concat "/tmp/" (file-name-nondirectory uri))))
                  ((symbol-function 'url-copy-file)
                   (lambda (uri new &rest _) (setq copied (list uri new))))
                  ((symbol-function 'org-download-insert-link)
                   (lambda (uri new) (setq linked (list uri new)))))
          (with-temp-buffer
            (setq major-mode 'org-mode)
            (org-attach-file-and-insert-link "https://ex.com/notes.txt"))))
      (expect copied :to-equal '("https://ex.com/notes.txt" "/tmp/notes.txt"))
      (expect linked :to-equal '("https://ex.com/notes.txt" "/tmp/notes.txt")))))
