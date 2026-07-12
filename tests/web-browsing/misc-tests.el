;;; tests/web-browsing/misc-tests.el --- web-browsing/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/misc.el")

(describe "process-external-url"
  :var (calls)

  (before-each (setq calls nil))

  (it "opens hacker news urls with hnreader"
    (cl-letf (((symbol-function 'hnreader-comment)
               (lambda (url) (push (list 'hn url) calls))))
      (process-external-url "https://news.ycombinator.com/item?id=1"))
    (expect calls :to-equal '((hn "https://news.ycombinator.com/item?id=1"))))

  (it "opens reddit urls with reddigg"
    (cl-letf (((symbol-function 'reddigg-view-comments)
               (lambda (url) (push (list 'reddit url) calls))))
      (process-external-url "https://www.reddit.com/r/emacs/comments/x/"))
    (expect calls :to-equal '((reddit "https://www.reddit.com/r/emacs/comments/x/"))))

  (it "plays youtube watch urls via mpv behind its transient"
    (cl-letf (((symbol-function 'mpv-transient)
               (lambda () (push '(transient) calls)))
              ((symbol-function 'mpv-play-url)
               (lambda (url) (push (list 'play url) calls))))
      (process-external-url "https://www.youtube.com/watch?v=abc"))
    (expect (reverse calls)
            :to-equal '((transient) (play "https://www.youtube.com/watch?v=abc"))))

  (it "opens github urls in forge when the git module is enabled"
    (let ((doom-modules-enabled '((:custom git))))
      (cl-letf (((symbol-function 'forge-visit-topic-via-url)
                 (lambda (url) (push (list 'forge url) calls))))
        (process-external-url "https://github.com/org/repo/pull/1")))
    (expect calls :to-equal '((forge "https://github.com/org/repo/pull/1"))))

  (it "falls back to eww for github urls when the git module is off"
    (let ((doom-modules-enabled '()))
      (cl-letf (((symbol-function 'eww-open-in-other-window)
                 (lambda (url &rest _) (push (list 'eww url) calls))))
        (process-external-url "https://github.com/org/repo/pull/1")))
    (expect calls :to-equal '((eww "https://github.com/org/repo/pull/1"))))

  (it "pushes bug-reference-style strings through bug-reference"
    ;; pin the regexp: the lab installs an org/repo#N one at runtime
    (let ((bug-reference-bug-regexp
           "\\(\\b[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+#\\([0-9]+\\)\\)"))
      (cl-letf (((symbol-function 'bug-reference-push-button)
                 (lambda () (interactive) (push '(bug-pushed) calls))))
        (process-external-url "agzam/foo#12")))
    (expect calls :to-equal '((bug-pushed))))

  (it "falls back to eww for everything else"
    (cl-letf (((symbol-function 'eww-open-in-other-window)
               (lambda (url &rest _) (push (list 'eww url) calls))))
      (process-external-url "https://example.com/article"))
    (expect calls :to-equal '((eww "https://example.com/article")))))

(describe "browse-url-externally"
  (it "forces the default external browser regardless of handler setup"
    (let (seen-fn)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (&rest _) (setq seen-fn browse-url-browser-function))))
        (let ((browse-url-browser-function #'eww))
          (browse-url-externally "https://example.com")))
      (expect seen-fn :to-equal 'browse-url-default-browser))))
