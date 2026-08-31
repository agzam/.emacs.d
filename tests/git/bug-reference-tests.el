;;; tests/git/bug-reference-tests.el --- git/autoload/bug-reference.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'bug-reference)

(load-module-file "modules/git/autoload/bug-reference.el")

;; NOTE buttercup :var slots are lexical - useless for these special vars;
;; dynamic lets keep the setqs inside `init-bug-reference-mode-settings'
;; from leaking into later suites.

(describe "init-bug-reference-mode-settings"
  (it "installs a regexp matching org/repo#N references"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (expect (string-match-p bug-reference-bug-regexp "see agzam/foo#12 there")
              :to-be-truthy)
      (expect (string-match-p bug-reference-bug-regexp "no reference here")
              :to-be nil)
      (expect bug-reference-url-format :to-be #'bug-reference-url-format-fn)))
  (it "wires an action onto the bug-reference button category"
    (init-bug-reference-mode-settings)
    (expect (get 'bug-reference 'action) :to-be #'bug-reference-button-action)))

(describe "bug-reference-url-format-fn"
  (it "builds the GitHub issue url from the match"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (with-temp-buffer
        (insert "fix agzam/spacehammer#101 soon")
        (goto-char (point-min))
        (re-search-forward bug-reference-bug-regexp)
        (expect (bug-reference-url-format-fn)
                :to-equal "https://github.com/agzam/spacehammer/issues/101")))))

;; NOTE ghub-get is stubbed per-test with cl-letf, never a top-level defun:
;; buttercup-run-discover shares one Emacs and tests/org/custom-tests.el
;; already defines a global ghub-get stub - two defuns would collide.

(describe "bug-reference-github-resolve-url"
  (it "swaps /issues/ for the API html_url when the number is a pull request"
    (let (calls)
      (cl-letf (((symbol-function 'ghub-get)
                 (lambda (resource &rest _)
                   (push resource calls)
                   '((html_url . "https://github.com/agzam/github-topics/pull/4")))))
        (expect (bug-reference-github-resolve-url
                 "https://github.com/agzam/github-topics/issues/4")
                :to-equal "https://github.com/agzam/github-topics/pull/4")
        (expect calls :to-equal '("/repos/agzam/github-topics/issues/4")))))
  (it "keeps the url when the number is a real issue"
    (cl-letf (((symbol-function 'ghub-get)
               (lambda (&rest _)
                 '((html_url . "https://github.com/agzam/spacehammer/issues/101")))))
      (expect (bug-reference-github-resolve-url
               "https://github.com/agzam/spacehammer/issues/101")
              :to-equal "https://github.com/agzam/spacehammer/issues/101")))
  (it "returns the url unchanged when the API call fails"
    (cl-letf (((symbol-function 'ghub-get)
               (lambda (&rest _) (error "offline"))))
      (expect (bug-reference-github-resolve-url
               "https://github.com/agzam/github-topics/issues/4")
              :to-equal "https://github.com/agzam/github-topics/issues/4")))
  (it "does not consult the API for non-issue urls"
    (let (calls)
      (cl-letf (((symbol-function 'ghub-get)
                 (lambda (&rest args) (push args calls) nil)))
        (expect (bug-reference-github-resolve-url "https://example.com/foo")
                :to-equal "https://example.com/foo")
        (expect calls :to-be nil)))))

(describe "bug-reference->github-url"
  (it "resolves an org/repo#N ref through the API to the accurate url"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (cl-letf (((symbol-function 'ghub-get)
                 (lambda (&rest _)
                   '((html_url . "https://github.com/agzam/github-topics/pull/4")))))
        (expect (bug-reference->github-url "agzam/github-topics#4")
                :to-equal "https://github.com/agzam/github-topics/pull/4"))))
  (it "falls back to the /issues/ form when the API is unreachable"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (cl-letf (((symbol-function 'ghub-get)
                 (lambda (&rest _) (error "offline"))))
        (expect (bug-reference->github-url "qlik-trial/stitch-menagerie-service#576")
                :to-equal
                "https://github.com/qlik-trial/stitch-menagerie-service/issues/576"))))
  (it "returns nil for a non-reference string"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (expect (bug-reference->github-url "just some words") :to-be nil))))

;; NOTE the point of these two: they run in a buffer holding no reference
;; and no overlay, with point nowhere near one.  That is the embark collect
;; buffer's state, and what the earlier point-reading actions died on.

(describe "bug-reference-visit-topic"
  (it "visits the topic of the reference it is handed, ignoring point"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format)
          visited)
      (init-bug-reference-mode-settings)
      (cl-letf (((symbol-function 'bug-reference-github-resolve-url) #'identity)
                ((symbol-function 'forge-visit-topic-via-url)
                 (lambda (url &rest _) (setq visited url))))
        (with-temp-buffer
          (insert "no reference anywhere in this buffer")
          (bug-reference-visit-topic "agzam/foo#12")))
      (expect visited :to-equal "https://github.com/agzam/foo/issues/12"))))

(describe "bug-reference-browse"
  (it "browses the reference it is handed, ignoring point"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format)
          browsed)
      (init-bug-reference-mode-settings)
      (cl-letf (((symbol-function 'bug-reference-github-resolve-url) #'identity)
                ((symbol-function 'browse-url-externally)
                 (lambda (url &rest _) (setq browsed url))))
        (with-temp-buffer
          (insert "no reference anywhere in this buffer")
          (bug-reference-browse "qlik-trial/stitch-agent-service#812")))
      (expect browsed
              :to-equal
              "https://github.com/qlik-trial/stitch-agent-service/issues/812"))))

(describe "bug-reference-button-action"
  (it "browses the reference url when its overlay-button is pushed"
    ;; Emacs 31 makes ref overlays actionless buttons; the wired action must
    ;; route `push-button' to the reference's url.
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format)
          browsed)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq browsed url))))
        (init-bug-reference-mode-settings)
        (with-temp-buffer
          (insert "see agzam/foo#12 here")
          (goto-char (point-min))
          (re-search-forward bug-reference-bug-regexp)
          (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put ov 'category 'bug-reference)
            (overlay-put ov 'button ov)
            (overlay-put ov 'bug-reference-url "https://example.test/12"))
          (push-button (match-beginning 0))))
      (expect browsed :to-equal "https://example.test/12"))))
