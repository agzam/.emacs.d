;;; tests/general/yank-path-tests.el --- general/autoload/yank-path.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/yank-path.el")

(describe "yank-path--position-suffix"
  (it "returns :LINE outside a region"
    (with-temp-buffer
      (insert "one\ntwo\nthree")
      (goto-char (point-min))
      (forward-line 1)
      (expect (yank-path--position-suffix) :to-equal ":2")))
  (it "returns :START-END for an active region"
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert "one\ntwo\nthree\nfour")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (expect (yank-path--position-suffix) :to-equal ":1-4"))))

(describe "yank-path--path-at-point"
  (it "resolves a PATH:LINE-LINE spec under point"
    (let ((f (make-temp-file "yank-path-probe")))
      (unwind-protect
          (with-temp-buffer
            (insert (format "see %s:12-15 here" f))
            (goto-char (+ (point-min) 6))
            (expect (yank-path--path-at-point) :to-equal (list f 12 15)))
        (delete-file f))))
  (it "returns nil instead of guessing when the path does not resolve"
    (with-temp-buffer
      (insert "see /definitely/not/a/real/file:12 here")
      (goto-char (+ (point-min) 6))
      (expect (yank-path--path-at-point) :to-be nil))))
