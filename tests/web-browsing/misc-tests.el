;;; tests/web-browsing/misc-tests.el --- web-browsing/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/misc.el")

;; valued defvars: mark the embark vars special globally so the suite's
;; let-binds are dynamic for the module fn (the embark-tests lesson)
(defvar embark-url-patterns nil)
(defvar embark-url-config nil)

(describe "process-external-url"
  :var (calls)

  (before-each (setq calls nil))

  (it "dispatches to the matching url-type's RET action"
    (with-fake-feature 'embark
      (let ((embark-url-patterns '((yt-video "youtube\\.com/watch")))
            (embark-url-config
             `((nil :actions (("RET" . ,(lambda (url) (push (list 'shared url) calls)))))
               (yt-video :pattern "youtube\\.com/watch"
                         :actions (("RET" . ,(lambda (url) (push (list 'mpv url) calls))))))))
        (process-external-url "https://www.youtube.com/watch?v=abc")))
    (expect calls :to-equal '((mpv "https://www.youtube.com/watch?v=abc"))))

  (it "supports function patterns"
    (with-fake-feature 'embark
      (let ((embark-url-patterns
             `((gh-pr ,(lambda (url) (string-match-p "/pull/[0-9]+" url)))))
            (embark-url-config
             `((nil :actions (("RET" . ignore)))
               (gh-pr :actions (("RET" . ,(lambda (url) (push (list 'forge url) calls))))))))
        (process-external-url "https://github.com/org/repo/pull/1")))
    (expect calls :to-equal '((forge "https://github.com/org/repo/pull/1"))))

  (it "falls back to the shared RET when the matched type has none"
    ;; github-commit/compare rows declare empty :actions on purpose
    (with-fake-feature 'embark
      (let ((embark-url-patterns '((gh-commit "github\\.com/.+/commit/")))
            (embark-url-config
             `((nil :actions (("RET" . ,(lambda (url) (push (list 'eww url) calls)))))
               (gh-commit :actions ()))))
        (process-external-url "https://github.com/org/repo/commit/abc123")))
    (expect calls :to-equal '((eww "https://github.com/org/repo/commit/abc123"))))

  (it "falls back to the shared RET when no pattern matches"
    (with-fake-feature 'embark
      (let ((embark-url-patterns '((yt-video "youtube\\.com/watch")))
            (embark-url-config
             `((nil :actions (("RET" . ,(lambda (url) (push (list 'eww url) calls))))))))
        (process-external-url "https://example.com/article")))
    (expect calls :to-equal '((eww "https://example.com/article"))))

  (it "routes bug-reference-style strings to the forge action"
    ;; pin the regexp: the lab installs an org/repo#N one at runtime
    (with-fake-feature 'embark
      (let ((bug-reference-bug-regexp
             "\\(\\b[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+#\\([0-9]+\\)\\)"))
        (cl-letf (((symbol-function 'bug-reference-visit-topic)
                   (lambda (ref) (push (list 'forge ref) calls))))
          (process-external-url "agzam/foo#12"))))
    (expect calls :to-equal '((forge "agzam/foo#12")))))

(describe "browse-url-externally"
  (it "forces the default external browser regardless of handler setup"
    (let (seen-fn)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (&rest _) (setq seen-fn browse-url-browser-function))))
        (let ((browse-url-browser-function #'eww))
          (browse-url-externally "https://example.com")))
      (expect seen-fn :to-equal 'browse-url-default-browser))))
