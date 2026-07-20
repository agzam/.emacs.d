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

(describe "bug-reference->github-url"
  (it "expands an org/repo#N ref string to its GitHub issue url"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (expect (bug-reference->github-url "qlik-trial/stitch-menagerie-service#576")
              :to-equal
              "https://github.com/qlik-trial/stitch-menagerie-service/issues/576")))
  (it "returns nil for a non-reference string"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (expect (bug-reference->github-url "just some words") :to-be nil))))

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
