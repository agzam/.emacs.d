;;; tests/writing/sdcv-tests.el --- writing/autoload/sdcv.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/sdcv.el")

(describe "region-or-word-at-point-str"
  (it "returns the word at point"
    (with-temp-buffer
      (insert "hello world")
      (goto-char 3)
      (expect (region-or-word-at-point-str) :to-equal "hello")))

  (it "prefers the active region"
    (with-temp-buffer
      (insert "hello world")
      (push-mark 1 t t)
      (goto-char 8)
      (expect (region-or-word-at-point-str) :to-equal "hello w"))))

(describe "sdcv-search-at-point"
  (it "searches the word at point with focus"
    (let (args)
      (cl-letf (((symbol-function 'sdcv-search)
                 (lambda (&rest a) (setq args a))))
        (with-temp-buffer
          (insert "lexeme")
          (goto-char 3)
          (sdcv-search-at-point))
        (expect args :to-equal '("lexeme" nil nil t))))))