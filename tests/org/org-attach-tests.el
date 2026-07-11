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
