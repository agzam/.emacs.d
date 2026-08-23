;;; tests/package-declarations-tests.el --- one elpaca order per package -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'pcase)

(defun package-declarations-tests--ensures-p (args)
  "Non-nil when a `use-package' arglist ARGS reaches elpaca.
`use-package-always-ensure' is t here, so only an explicit :ensure nil opts out."
  (if-let* ((tail (memq :ensure args)))
      (cadr tail)
    t))

(defun package-declarations-tests--order-id (order)
  "Package id an `elpaca' ORDER queues, if any.
Unwraps a quoted or backquoted order like `elpaca' does; an id only the
runtime knows, as in (elpaca `(,@elpaca-order)), counts as none."
  (when (memq (car-safe order) '(quote \`))
    (setq order (cadr order)))
  (cond ((and order (symbolp order)) order)
        ((and (consp order) (symbolp (car order))) (car order))))

(defun package-declarations-tests--collect (form)
  "Package ids FORM hands to elpaca, at any nesting depth."
  (when (proper-list-p form)
    (append
     (pcase form
       (`(use-package ,(and name (pred symbolp)) . ,args)
        (and (package-declarations-tests--ensures-p args) (list name)))
       (`(elpaca ,order . ,_)
        (when-let* ((id (package-declarations-tests--order-id order)))
          (list id))))
     (mapcan #'package-declarations-tests--collect form))))

(defun package-declarations-tests--ids (file)
  "Package ids FILE queues."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (ids)
      (condition-case nil
          (while t (setq ids (nconc ids (package-declarations-tests--collect
                                         (read (current-buffer))))))
        (end-of-file nil))
      ids)))

(defun package-declarations-tests--sources ()
  "Config sources a package declaration may live in."
  (append (directory-files test-config-root t "\\.el\\'")
          (directory-files (expand-file-name "lisp" test-config-root) t "\\.el\\'")
          (directory-files-recursively (expand-file-name "modules" test-config-root)
                                       "\\.el\\'")))

(defun package-declarations-tests--duplicates (files)
  "Alist of (ID . FILES) for every package FILES declare more than once."
  (let (seen)
    (dolist (file files)
      (dolist (id (package-declarations-tests--ids file))
        (push (file-relative-name file test-config-root) (alist-get id seen))))
    (sort (seq-filter (lambda (entry) (cdr (cdr entry))) seen)
          (lambda (a b) (string< (car a) (car b))))))

(defun package-declarations-tests--fixture (&rest contents)
  "Write each string in CONTENTS to its own temp file, return the file names."
  (mapcar (lambda (content)
            (let ((file (make-temp-file "package-declarations" nil ".el")))
              (write-region content nil file nil 'silent)
              file))
          contents))

(describe "the config's package declarations"
  (it "queues each package exactly once"
    ;; elpaca drops the second order with "Duplicate item ID queued", so the
    ;; recipe it carried (:version pins, :host/:repo forks) silently vanishes.
    (expect (package-declarations-tests--duplicates
             (package-declarations-tests--sources))
            :to-equal nil))

  (it "flags the same package declared in two files"
    (let ((files (package-declarations-tests--fixture
                  "(elpaca (ox-gfm :version (lambda (_) \"1.0\")))"
                  "(use-package ox-gfm :after org)")))
      (unwind-protect
          (expect (mapcar #'car (package-declarations-tests--duplicates files))
                  :to-equal '(ox-gfm))
        (mapc #'delete-file files))))

  (it "sees through a backquoted order"
    (let ((files (package-declarations-tests--fixture
                  "(elpaca `(gptel-tools :repo ,(expand-file-name \"x\")))"
                  "(use-package gptel-tools :defer t)")))
      (unwind-protect
          (expect (mapcar #'car (package-declarations-tests--duplicates files))
                  :to-equal '(gptel-tools))
        (mapc #'delete-file files))))

  (it "flags a package declared twice in one file"
    (let ((files (package-declarations-tests--fixture
                  "(use-package magit :defer t)\n(use-package magit :after org)")))
      (unwind-protect
          (expect (mapcar #'car (package-declarations-tests--duplicates files))
                  :to-equal '(magit))
        (mapc #'delete-file files))))

  (it "lets a package elpaca never sees be configured alongside a real order"
    (let ((files (package-declarations-tests--fixture
                  "(use-package org :ensure nil :defer t)"
                  "(use-package org :ensure (org :repo \"x\"))")))
      (unwind-protect
          (expect (package-declarations-tests--duplicates files) :to-equal nil)
        (mapc #'delete-file files)))))
