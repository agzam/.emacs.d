;;; tests/general/misc-tests.el --- general/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/misc.el")

(describe "parse-circleci-url"
  (it "extracts org, repo and build number"
    (expect (parse-circleci-url
             "https://app.circleci.com/pipelines/project/github/acme/widget/1234")
            :to-equal '(:org "acme" :repo "widget" :build "1234" :job nil)))
  (it "extracts the job number from output URLs"
    (expect (parse-circleci-url
             "https://circleci.com/api/v1.1/project/github/acme/widget/1234/output/567/0")
            :to-equal '(:org "acme" :repo "widget" :build "1234" :job "567")))
  (it "returns nil for non-CircleCI URLs"
    (expect (parse-circleci-url "https://example.com/foo/bar") :to-be nil)))

(describe "org-wrap-in-block"
  (it "wraps the region in begin/end markers"
    (with-temp-buffer
      (insert "hello world")
      (transient-mark-mode 1)
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (org-wrap-in-block 'quote)
      (expect (buffer-string)
              :to-equal "#+begin_quote\nhello world\n#+end_quote")))
  (it "leaves point ready for the src language, in insert state"
    (with-temp-buffer
      (insert "x")
      (transient-mark-mode 1)
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (let ((inserted nil))
        (cl-letf (((symbol-function 'evil-insert-state)
                   (lambda (&rest _) (setq inserted t))))
          (org-wrap-in-block 'src))
        (expect inserted :to-be t)
        (expect (buffer-string) :to-equal "#+begin_src \nx\n#+end_src")))))

(describe "toggle-indent-style"
  (it "flips indent-tabs-mode in the current buffer"
    (with-temp-buffer
      (setq indent-tabs-mode nil)
      (toggle-indent-style)
      (expect indent-tabs-mode :to-be t)
      (toggle-indent-style)
      (expect indent-tabs-mode :to-be nil)))
  (it "never touches the global default"
    (let ((default (default-value 'indent-tabs-mode)))
      (with-temp-buffer
        (toggle-indent-style)
        (expect (default-value 'indent-tabs-mode) :to-equal default)))))
