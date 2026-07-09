;;; tests/general/files-tests.el --- general/autoload/files.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/files.el")

(describe "insert-file-path"
  (it "inserts the abbreviated absolute path by default"
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _) (expand-file-name "some/file.txt" "~"))))
      (with-temp-buffer
        (insert-file-path nil)
        (expect (buffer-string) :to-equal "~/some/file.txt"))))
  (it "inserts a path relative to default-directory with a prefix arg"
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _)
                 (expand-file-name "sub/file.txt" test-sandbox-dir))))
      (with-temp-buffer
        (setq default-directory test-sandbox-dir)
        (insert-file-path '(4))
        (expect (buffer-string) :to-equal "sub/file.txt")))))
