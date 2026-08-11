;;; tests/git/misc-tests.el --- git/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/git/autoload/misc.el")

(describe "parse-git-url"
  (it "parses ssh urls, stripping the .git suffix"
    (expect (parse-git-url "git@github.com:agzam/remoto.el.git")
            :to-equal '(:host "github.com" :org "agzam" :repo "remoto.el")))
  (it "parses https urls"
    (expect (parse-git-url "https://github.com/foo/bar")
            :to-equal '(:host "github.com" :org "foo" :repo "bar")))
  (it "strips www"
    (expect (parse-git-url "https://www.github.com/foo/bar.git")
            :to-equal '(:host "github.com" :org "foo" :repo "bar")))
  (it "returns nil for non-urls"
    (expect (parse-git-url "plainstring") :to-be nil)))

(describe "bisect-github-url"
  (it "recognizes a bare repo url"
    (let ((parts (bisect-github-url "https://github.com/fniessen/refcard-org-mode")))
      (expect (plist-get parts :forge) :to-equal "https://github.com")
      (expect (plist-get parts :org) :to-equal "fniessen")
      (expect (plist-get parts :repo) :to-equal "refcard-org-mode")
      (expect (plist-get parts :issue) :to-be nil)
      (expect (plist-get parts :pull) :to-be nil)))
  (it "extracts the issue number"
    (expect (plist-get (bisect-github-url
                        "https://github.com/fniessen/refcard-org-mode/issues/7")
                       :issue)
            :to-equal "7"))
  (it "extracts the PR number"
    (expect (plist-get (bisect-github-url
                        "https://github.com/fniessen/refcard-org-mode/pull/5")
                       :pull)
            :to-equal "5"))
  (it "extracts ref, path and extension from file urls"
    (let ((parts (bisect-github-url
                  "https://github.com/f/r/blob/master/images/org-mode-unicorn.png")))
      (expect (plist-get parts :ref) :to-equal "master")
      (expect (plist-get parts :path) :to-equal "images/org-mode-unicorn.png")
      (expect (plist-get parts :ext) :to-equal "png")))
  (it "extracts the line number from #L links"
    (let ((parts (bisect-github-url
                  "https://github.com/f/r/blob/main/src/code.py#L42")))
      (expect (plist-get parts :line) :to-equal "42")
      ;; known quirk (parity with doom.d): the ext regexp can't see past
      ;; the #L anchor, so :ext degrades to the whole path
      (expect (plist-get parts :ext) :to-equal "src/code.py#L42")))
  (it "errors on non-github urls"
    (expect (bisect-github-url "https://example.com/not/github")
            :to-throw 'error))
  (it "handles dots, dashes and underscores in org and repo"
    (let ((parts (bisect-github-url "https://github.com/foo-bar.baz/qu_ux-1.2")))
      (expect (plist-get parts :org) :to-equal "foo-bar.baz")
      (expect (plist-get parts :repo) :to-equal "qu_ux-1.2"))))

(describe "github-url->bug-reference"
  (it "formats a pull url as org/repo#N"
    (expect (github-url->bug-reference "https://github.com/agzam/foo/pull/4")
            :to-equal "agzam/foo#4"))
  (it "formats an issue url as org/repo#N"
    (expect (github-url->bug-reference "https://github.com/agzam/foo/issues/12")
            :to-equal "agzam/foo#12"))
  (it "returns nil for a bare repo url"
    (expect (github-url->bug-reference "https://github.com/agzam/foo")
            :to-be nil))
  (it "errors on non-github urls"
    (expect (github-url->bug-reference "https://example.com/x/y")
            :to-throw 'error)))

(describe "make-path"
  (it "joins and expands parts"
    (expect (make-path "/tmp" "a" "b") :to-equal "/tmp/a/b")))
