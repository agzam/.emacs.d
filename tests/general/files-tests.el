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

(describe "sudo-file-path"
  (it "wraps a local path in a root TRAMP hop"
    (expect (sudo-file-path "/etc/hosts")
            :to-equal "/sudo:root@localhost:/etc/hosts"))
  (it "chains onto an existing remote hop instead of replacing it"
    (expect (sudo-file-path "/ssh:me@box:/etc/hosts")
            :to-equal "/ssh:me@box|sudo:root@box:/etc/hosts")))

(describe "sudo-find-file"
  (it "opens the named file through the root TRAMP path"
    (let (opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (f) (setq opened f))))
        (sudo-find-file "/etc/hosts")
        (expect opened :to-equal "/sudo:root@localhost:/etc/hosts")))))

(describe "sudo-this-file"
  (it "reopens the current file as root"
    (let (opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (f) (setq opened f))))
        (with-temp-buffer
          (setq buffer-file-name "/etc/hosts")
          (sudo-this-file))
        (expect opened :to-equal "/sudo:root@localhost:/etc/hosts"))))
  (it "errors when the buffer visits no file"
    (with-temp-buffer
      (expect (sudo-this-file) :to-throw 'user-error))))

(describe "sudo-save-buffer"
  (it "writes to the root TRAMP path and clears the modified flag"
    (let (target)
      (cl-letf (((symbol-function 'write-region)
                 (lambda (_start _end file &rest _) (setq target file)))
                ((symbol-function 'clear-visited-file-modtime) #'ignore))
        (with-temp-buffer
          (setq buffer-file-name "/etc/hosts")
          (insert "edited")
          (sudo-save-buffer)
          (expect target :to-equal "/sudo:root@localhost:/etc/hosts")
          (expect (buffer-modified-p) :to-be nil)))))
  (it "errors when the buffer visits no file"
    (with-temp-buffer
      (expect (sudo-save-buffer) :to-throw 'user-error))))
