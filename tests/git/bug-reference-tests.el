;;; tests/git/bug-reference-tests.el --- git/autoload/bug-reference.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'bug-reference)

(load-module-file "modules/git/autoload/bug-reference.el")

(describe "init-bug-reference-mode-settings"
  :var (bug-reference-bug-regexp bug-reference-url-format)
  (it "installs a regexp matching org/repo#N references"
    (init-bug-reference-mode-settings)
    (expect (string-match-p bug-reference-bug-regexp "see agzam/foo#12 there")
            :to-be-truthy)
    (expect (string-match-p bug-reference-bug-regexp "no reference here")
            :to-be nil)
    (expect bug-reference-url-format :to-be #'bug-reference-url-format-fn)))

(describe "bug-reference-url-format-fn"
  :var (bug-reference-bug-regexp bug-reference-url-format)
  (it "builds the GitHub issue url from the match"
    (init-bug-reference-mode-settings)
    (with-temp-buffer
      (insert "fix agzam/spacehammer#101 soon")
      (goto-char (point-min))
      (re-search-forward bug-reference-bug-regexp)
      (expect (bug-reference-url-format-fn)
              :to-equal "https://github.com/agzam/spacehammer/issues/101"))))
