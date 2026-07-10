;;; tests/git/git-link-tests.el --- git/autoload/git-link.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/git/autoload/git-link.el")

(describe "git-https-url->ssh"
  (it "converts a plain https url"
    (expect (git-https-url->ssh "https://github.com/foo/bar")
            :to-equal "git@github.com:foo/bar.git"))
  (it "keeps a single .git suffix"
    (expect (git-https-url->ssh "https://github.com/foo/bar.git")
            :to-equal "git@github.com:foo/bar.git"))
  (it "handles trailing slashes and other hosts"
    (expect (git-https-url->ssh "https://gitlab.com/a/b/")
            :to-equal "git@gitlab.com:a/b.git"))
  (it "rejects non-https input"
    (expect (git-https-url->ssh "git@github.com:foo/bar.git")
            :to-throw 'error)))

;; browse-at-remote isn't installed in the test sandbox; the wrappers'
;; own glue (ref -> plist :url extraction) is what's under test.
(provide 'browse-at-remote)
(defvar git-link-tests--remote-ref nil)
(defvar git-link-tests--url-plist nil)
(defun browse-at-remote--remote-ref (&optional _file) git-link-tests--remote-ref)
(defun browse-at-remote--get-url-from-remote (_remote) git-link-tests--url-plist)

(describe "vc-git-link--homepage-url"
  (it "resolves the homepage from the remote ref"
    (let ((git-link-tests--remote-ref '("git@github.com:foo/bar.git" . "main"))
          (git-link-tests--url-plist '(:url "https://github.com/foo/bar")))
      (expect (vc-git-link--homepage-url)
              :to-equal "https://github.com/foo/bar")))
  (it "signals when no remote can be determined"
    (let ((git-link-tests--remote-ref nil))
      (expect (vc-git-link--homepage-url) :to-throw))))

(describe "vc-git-link-kill-homepage"
  (it "puts the homepage url on the kill ring"
    (let ((git-link-tests--remote-ref '("git@github.com:foo/bar.git" . "main"))
          (git-link-tests--url-plist '(:url "https://github.com/foo/bar"))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (vc-git-link-kill-homepage)
      (expect (car kill-ring) :to-equal "https://github.com/foo/bar"))))
