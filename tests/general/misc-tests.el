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
