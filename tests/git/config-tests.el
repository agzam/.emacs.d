;;; tests/git/config-tests.el --- modules/git/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun git-config-tests--top-level-forms ()
  "Read every top-level form out of the git module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/git/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(describe "forge bindings"
  ;; evil-collection-forge installs forge's magit bindings on evil-friendly
  ;; keys and resets this flag, with a message, whenever it finds it set.
  ;; Forge's autoloads define the flag t and consult it as soon as magit-mode
  ;; loads, and with :ensure heading `use-package-keywords' every keyword of
  ;; the use-package form (:preface included) runs inside elpaca's activation
  ;; callback, after those autoloads.  Only a top-level form ahead of the
  ;; use-package binds it early enough.
  (it "binds forge-add-default-bindings nil at top level, before use-package forge"
    (let* ((forms (git-config-tests--top-level-forms))
           (declaration (cl-position '(defvar forge-add-default-bindings nil)
                                     forms :test #'equal))
           (package (cl-position-if (lambda (f)
                                      (and (eq (car-safe f) 'use-package)
                                           (eq (cadr f) 'forge)))
                                    forms)))
      (expect declaration :not :to-be nil)
      (expect package :not :to-be nil)
      (expect declaration :to-be-less-than package))))

(describe "embark browse keys"
  ;; both packages browse on a bare "b"; the config's convention is "b b"
  ;; for the Emacs view and "b o" for the external browser
  :var* ((forms (git-config-tests--top-level-forms))
         (package-form
          (lambda (name)
            (cl-find-if (lambda (f)
                          (and (eq (car-safe f) 'use-package)
                               (eq (cadr f) name)))
                        forms))))

  (it "opens a github-topics PR in forge on b b, in the browser on b o"
    (let ((layout (map-form-prefix-keys (funcall package-form 'github-topics)
                                        'github-topics-pr-map "b")))
      (expect (car layout) :to-be nil)
      (expect (cdr layout) :to-have-same-items-as '("b" "o"))))

  (dolist (map '(remoto-embark-repo-map
                 remoto-embark-dir-map
                 remoto-embark-file-map
                 remoto-embark-branch-map
                 remoto-embark-issue-map))
    (it (format "opens %s in Emacs on b b, in the browser on b o" map)
      (let ((layout (map-form-prefix-keys (funcall package-form 'remoto) map "b")))
        (expect (car layout) :to-be nil)
        (expect (cdr layout) :to-have-same-items-as '("b" "o")))))

  ;; an owner page is a forge page only - remoto has no Emacs view for it
  (it "gives a remoto owner the browser half alone"
    (let ((layout (map-form-prefix-keys (funcall package-form 'remoto)
                                        'remoto-embark-owner-map "b")))
      (expect (car layout) :to-be nil)
      (expect (cdr layout) :to-equal '("o")))))

(describe "code-review browse-url handler"
  ;; the package autoloads register it at boot and on every live package
  ;; update; :init and :config wait for magit and forge
  :var* ((form (cl-find-if (lambda (f)
                             (and (eq (car-safe f) 'use-package)
                                  (eq (cadr f) 'code-review)))
                           (git-config-tests--top-level-forms)))
         (preface (use-package-body-forms (cddr form) :preface))
         (pr-url "https://github.com/agzam/foo/pull/12")
         (pr-handler '("\\`https?://github\\.com/[^/?#]+/[^/?#]+/pull/[0-9]+"
                       . code-review-browse-url))
         (mail-handler '("\\`mailto:" . browse-url--mailto)))

  (before-all (require 'browse-url))

  (after-each
    (advice-remove 'browse-url-select-handler
                   'browse-url-select-handler-no-code-review-a))

  (it "keeps the handler registered at boot out of browse-url"
    (let ((browse-url-default-handlers (list pr-handler mail-handler)))
      (dolist (f preface) (eval f t))
      (expect (browse-url-select-handler pr-url) :to-be nil)))

  (it "keeps it out when a package update registers it again"
    (let ((browse-url-default-handlers (list mail-handler)))
      (dolist (f preface) (eval f t))
      ;; what the package autoloads run once browse-url is loaded
      (add-to-list 'browse-url-default-handlers pr-handler)
      (expect (browse-url-select-handler pr-url) :to-be nil)))

  (it "leaves the other handlers in place"
    (let ((browse-url-default-handlers (list pr-handler mail-handler)))
      (dolist (f preface) (eval f t))
      (expect (browse-url-select-handler "mailto:someone@example.com")
              :to-be 'browse-url--mailto))))

;;; tests/git/config-tests.el ends here
