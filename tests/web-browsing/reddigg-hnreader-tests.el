;;; tests/web-browsing/reddigg-hnreader-tests.el --- web-browsing/autoload/reddigg-hnreader.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/reddigg-hnreader.el")

(describe "reddigg-copy-current-sub-url"
  (it "kills the absolute reddit url found near the top of the buffer"
    (with-temp-buffer
      (insert "#+TITLE: r/emacs\n"
              "some intro\n"
              "https://reddit.com/r/emacs/comments/abc/cool_post/\n"
              "body text\n")
      (let (kill-ring kill-ring-yank-pointer)
        (expect (reddigg-copy-current-sub-url)
                :to-equal "https://reddit.com/r/emacs/comments/abc/cool_post/")
        (expect (car kill-ring)
                :to-equal "https://reddit.com/r/emacs/comments/abc/cool_post/"))))

  (it "returns nil when the top of the buffer has no reddit url"
    (with-temp-buffer
      (insert "nothing to see\nhere\n")
      (let (kill-ring kill-ring-yank-pointer)
        (expect (reddigg-copy-current-sub-url) :to-be nil)))))

(describe "hnreader-copy-hn-story-url"
  (it "kills the story url from the trailing elisp link"
    (with-temp-buffer
      (insert "* HN comments\nsome thread\n"
              "[[elisp:(hnreader-comment \"https://news.ycombinator.com/item?id=42\")][View story in eww]]")
      (let (kill-ring kill-ring-yank-pointer)
        (expect (hnreader-copy-hn-story-url)
                :to-equal "https://news.ycombinator.com/item?id=42")
        (expect (car kill-ring)
                :to-equal "https://news.ycombinator.com/item?id=42")))))

(describe "hnreader-frontpage-item-no-rank-a"
  (it "strips ranking numbers and non-breaking spaces"
    (with-temp-buffer
      (hnreader-frontpage-item-no-rank-a
       (lambda (_thing _subtext)
         (insert "* 12. A Story Title\n  50\u00A0points\n"))
       'thing 'subtext)
      (expect (buffer-string)
              :to-equal "* A Story Title\n  50 points\n"))))

(describe "consult-line-collect-urls"
  ;; consult isn't installed in the test sandbox; satisfy the runtime
  ;; require and stub consult--read
  (before-all (provide 'consult))

  (it "offers only url-bearing lines and jumps to the selection"
    (with-temp-buffer
      (insert "no url here\n"
              "see https://one.example/x now\n"
              "plain line\n"
              "and https://two.example/y\n")
      (let (captured)
        (cl-letf (((symbol-function 'consult--read)
                   (lambda (cands &rest _)
                     (setq captured cands)
                     (car (nth 1 cands))))
                  ((symbol-function 'recenter) #'ignore))
          (consult-line-collect-urls))
        (expect (mapcar #'car captured)
                :to-equal '("2: https://one.example/x"
                            "4: https://two.example/y"))
        (expect (line-number-at-pos) :to-equal 4))))

  (it "honors the ignore-regexp filter"
    (with-temp-buffer
      (insert "keep https://one.example/x\n"
              "skip https://news.ycombinator.com/item?id=1\n")
      (let (captured)
        (cl-letf (((symbol-function 'consult--read)
                   (lambda (cands &rest _)
                     (setq captured cands)
                     (caar cands)))
                  ((symbol-function 'recenter) #'ignore))
          (consult-line-collect-urls "ycombinator\\.com"))
        (expect (mapcar #'car captured)
                :to-equal '("1: https://one.example/x"))))))
