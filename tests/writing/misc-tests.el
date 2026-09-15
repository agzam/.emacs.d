;;; tests/writing/misc-tests.el --- writing/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/misc.el")

(describe "insert-bracket-pair"
  (it "self-inserts an opening bracket (smartparens closes it in live sessions)"
    (with-temp-buffer
      (insert-bracket-pair)
      (expect (buffer-string) :to-equal "[")
      (expect (point) :to-equal 2))))
