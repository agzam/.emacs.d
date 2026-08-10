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


