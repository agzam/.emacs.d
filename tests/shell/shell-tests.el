;;; tests/shell/shell-tests.el --- shell/autoload/shell.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)
(require 'project)

(load-module-file "modules/shell/autoload/shell.el")

;; shell-pop isn't installed in the batch tier; the functions under test
;; only need these entry points to exist.
(defvar shell-pop-shell-type nil)
(defun shell-pop (&optional _arg))
(defun shell-pop--set-shell-type (sym val) (set sym val))

(describe "shell-pop-in-project-root"
  :var (popped-in)
  (before-each
    (setq popped-in nil)
    (spy-on 'shell-pop :and-call-fake
            (lambda (&optional _arg) (setq popped-in default-directory))))

  (it "pops at the project root when inside a project"
    (spy-on 'project-current :and-return-value '(transient . "/tmp/fake-project/"))
    (spy-on 'project-root :and-return-value "/tmp/fake-project/")
    (shell-pop-in-project-root)
    (expect popped-in :to-equal "/tmp/fake-project/"))

  (it "falls back to default-directory outside projects"
    (spy-on 'project-current :and-return-value nil)
    (let ((default-directory "/tmp/"))
      (shell-pop-in-project-root)
      (expect popped-in :to-equal "/tmp/")))

  (it "passes the prefix arg through"
    (spy-on 'project-current :and-return-value nil)
    (shell-pop-in-project-root 4)
    (expect 'shell-pop :to-have-been-called-with 4)))

(describe "shell-pop-choose"
  (before-each
    (spy-on 'shell-pop))

  (it "rewires shell-pop-shell-type to the chosen shell"
    (spy-on 'completing-read :and-return-value "ghostel")
    (shell-pop-choose)
    (expect (car shell-pop-shell-type) :to-equal "ghostel")
    (expect (cadr shell-pop-shell-type) :to-equal "*ghostel*")
    (expect 'shell-pop :to-have-been-called))

  (it "builds an eshell entry for the eshell choice"
    (spy-on 'completing-read :and-return-value "eshell")
    (shell-pop-choose)
    (expect (car shell-pop-shell-type) :to-equal "eshell")
    (expect (cadr shell-pop-shell-type) :to-equal "*eshell*")))

(describe "insert-current-filename"
  (it "inserts the file behind the selected window's buffer"
    (let ((file-buf (generate-new-buffer " *filename-src*")))
      (unwind-protect
          (progn
            (with-current-buffer file-buf
              (setq buffer-file-name "/tmp/some-file.txt"))
            (with-temp-buffer
              (setq major-mode 'minibuffer-mode)
              (cl-letf (((symbol-function 'window-buffer)
                         (lambda (&optional _w) file-buf)))
                (insert-current-filename))
              (expect (buffer-string) :to-equal "/tmp/some-file.txt")))
        (with-current-buffer file-buf
          (setq buffer-file-name nil))
        (kill-buffer file-buf))))

  (it "does nothing outside the minibuffer"
    (with-temp-buffer
      (insert-current-filename)
      (expect (buffer-string) :to-equal ""))))
