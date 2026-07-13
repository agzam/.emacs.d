;;; tests/lookup/online-tests.el --- lookup/autoload/online.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar lookup-provider-url-alist
  '(("Fake" "https://fake.test/?q=%s")))
(defvar lookup-open-url-fn #'browse-url)

(load-module-file "modules/lookup/autoload/online.el")

(describe "lookup--online-provider"
  (before-each (setq lookup--last-provider nil))

  (it "prompts once, then reuses the answer per major-mode"
    (let ((asked 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (cl-incf asked) "Fake")))
        (expect (lookup--online-provider) :to-equal "Fake")
        (expect (lookup--online-provider) :to-equal "Fake")
        (expect asked :to-equal 1))))

  (it "force-p re-prompts"
    (setf (alist-get major-mode lookup--last-provider) "Fake")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Other")))
      (expect (lookup--online-provider 'force) :to-equal "Other"))))

(describe "lookup-online"
  (it "formats the query into the provider url"
    (let (opened)
      (let ((lookup-open-url-fn (lambda (url) (setq opened url))))
        (lookup-online "two words" "Fake")
        (expect opened :to-equal "https://fake.test/?q=two%20words"))))

  (it "user-errors on an unknown provider"
    (expect (lookup-online "q" "Nope") :to-throw 'user-error)))

(describe "provider alist hygiene"
  (it "module defaults carry only string backends (no ivy/helm fn rot)"
    ;; Read the config's defvar without loading it (map!/use-package there).
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/lookup/config.el" test-config-root))
      (goto-char (point-min))
      (search-forward "(defvar lookup-provider-url-alist")
      (goto-char (match-beginning 0))
      (let ((form (read (current-buffer))))
        (dolist (entry (eval (nth 2 form) t))
          (dolist (backend (cdr entry))
            (expect (stringp backend) :to-be-truthy)))))))
