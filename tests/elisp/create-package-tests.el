;;; tests/elisp/create-package-tests.el --- elisp/autoload/create-package.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/elisp/autoload/create-package.el")

;; special declaration so the let-bindings below are dynamic
(defvar magit-clone-default-directory)

(defun create-package-tests--scaffold (name)
  "Scaffold NAME into a temp root with all externals stubbed; return pkg dir."
  (let* ((root (file-name-as-directory (make-temp-file "pkg-root" t)))
         (magit-clone-default-directory root)
         (user-full-name "Test Author")
         (user-mail-address "test@example.com")
         git-init-dir)
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (cmd)
                 (if (string-match-p "github.user" cmd) "testuser\n" "fallback\n")))
              ((symbol-function 'url-copy-file)
               (lambda (&rest _) (error "no network in tests")))
              ((symbol-function 'call-process)
               (lambda (&rest args)
                 (setq git-init-dir default-directory)
                 0)))
      (let ((dir (create-emacs-package name)))
        (list :dir dir :git-init-dir git-init-dir :root root)))))

(describe "create-emacs-package"
  (it "scaffolds the full package layout with substituted templates"
    (let* ((result (create-package-tests--scaffold "mypkg"))
           (dir (plist-get result :dir)))
      (unwind-protect
          (progn
            (expect dir :to-match "testuser/mypkg\\.el/?\\'")
            (dolist (f '("mypkg.el" "Makefile" "README.org" "changelog.org"
                         ".gitignore" ".github/workflows/run-tests.yml"
                         "test/mypkg-tests.el" "LICENSE"))
              (expect (file-exists-p (expand-file-name f dir)) :to-be-truthy))
            (let ((main (with-temp-buffer
                          (insert-file-contents (expand-file-name "mypkg.el" dir))
                          (buffer-string))))
              (expect main :to-match "mypkg")
              (expect main :not :to-match "{{PKG}}")
              (expect main :to-match "Test Author"))
            ;; the license stub fallback kicked in (network stubbed away)
            (expect (with-temp-buffer
                      (insert-file-contents (expand-file-name "LICENSE" dir))
                      (buffer-string))
                    :to-match "GPL-3.0-or-later")
            ;; git init ran inside the new package dir
            (expect (file-name-as-directory (plist-get result :git-init-dir))
                    :to-equal (file-name-as-directory dir)))
        (delete-directory (plist-get result :root) t))))

  (it "rejects invalid package names"
    (expect (create-package-tests--scaffold "Bad Name")
            :to-throw 'user-error))

  (it "strips a trailing .el from the name"
    (let* ((result (create-package-tests--scaffold "trimmed.el"))
           (dir (plist-get result :dir)))
      (unwind-protect
          (expect (file-exists-p (expand-file-name "trimmed.el" dir))
                  :to-be-truthy)
        (delete-directory (plist-get result :root) t)))))
