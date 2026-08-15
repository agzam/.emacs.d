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

(describe "reddigg-hnreader-show-all-h"
  ;; the hook defers the unfolding by a timer, and the buffer can be killed
  ;; in between - reading a HN item and closing it at once does exactly that
  (it "does not error when the buffer dies before the timer runs"
    (let (timer-fn timer-arg)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_secs _rep fn arg) (setq timer-fn fn timer-arg arg) nil)))
        (let ((buf (get-buffer-create " *hn-hook-spec*")))
          (with-current-buffer buf
            (org-mode)
            (insert "* one\n** two\n")
            (reddigg-hnreader-show-all-h))
          (kill-buffer buf)))
      (expect timer-fn :to-be-truthy)
      (expect (funcall timer-fn timer-arg) :not :to-throw)))

  (it "unfolds a comments buffer once the timer runs"
    (let (timer-fn timer-arg)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_secs _rep fn arg) (setq timer-fn fn timer-arg arg) nil)))
        (let ((buf (get-buffer-create "*HNComments*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (erase-buffer)
                  (org-mode)
                  (insert "* one\n** two\n")
                  (org-overview)
                  (reddigg-hnreader-show-all-h))
                (funcall timer-fn timer-arg)
                (expect (with-current-buffer buf
                          (seq-some (lambda (o) (overlay-get o 'invisible))
                                    (with-current-buffer buf (overlays-in (point-min) (point-max)))))
                        :to-be nil))
            (kill-buffer buf))))))

  (it "leaves the front page folded"
    (let (timer-fn timer-arg)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_secs _rep fn arg) (setq timer-fn fn timer-arg arg) nil)))
        (let ((buf (get-buffer-create "*HN*")))
          (unwind-protect
              (let (unfolded)
                (with-current-buffer buf
                  (erase-buffer)
                  (org-mode)
                  (insert "* one\n** two\n")
                  (reddigg-hnreader-show-all-h))
                (cl-letf (((symbol-function 'org-fold-show-all)
                           (lambda (&rest _) (setq unfolded t))))
                  (funcall timer-fn timer-arg))
                (expect unfolded :to-be nil))
            (kill-buffer buf)))))))

(describe "hnreader-frontpage-item-no-rank-a"
  (it "strips ranking numbers and non-breaking spaces"
    (with-temp-buffer
      (hnreader-frontpage-item-no-rank-a
       (lambda (_thing _subtext)
         (insert "* 12. A Story Title\n  50\u00A0points\n"))
       'thing 'subtext)
      (expect (buffer-string)
              :to-equal "* A Story Title\n  50 points\n"))))


