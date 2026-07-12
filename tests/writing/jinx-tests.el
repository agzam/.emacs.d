;;; tests/writing/jinx-tests.el --- writing/autoload/jinx.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/jinx.el")

(describe "insert-comma"
  (it "backs over preceding spaces and re-attaches the comma"
    (with-temp-buffer
      (insert "foo bar")
      (goto-char 5)                     ; right before "bar"
      (insert-comma)
      (expect (buffer-string) :to-equal "foo, bar")))

  (it "inserts a bare comma before an existing space"
    (with-temp-buffer
      (insert "foo bar")
      (goto-char 4)                     ; right after "foo"
      (insert-comma)
      (expect (buffer-string) :to-equal "foo, bar")))

  (it "inserts comma-space mid-word"
    (with-temp-buffer
      (insert "foobar")
      (goto-char 4)
      (insert-comma)
      (expect (buffer-string) :to-equal "foo, bar"))))

(describe "insert-dash"
  (it "inserts a plain dash"
    (with-temp-buffer
      (insert "foo")
      (insert-dash)
      (expect (buffer-string) :to-equal "foo-")))

  (it "turns a quad dash into an em-dash"
    (with-temp-buffer
      (insert "foo ---")
      (insert-dash)
      (expect (buffer-string) :to-equal "foo — ")))

  (it "stays a plain dash near buffer start"
    (with-temp-buffer
      (insert "--")
      (insert-dash)
      (expect (buffer-string) :to-equal "---"))))

(describe "jinx-mode-off-h"
  :var (calls)

  (before-each (setq calls nil))

  (it "swallows any hook arity (wiktionary passes none, github-topics one)"
    (cl-letf (((symbol-function 'jinx-mode)
               (lambda (&optional arg) (push arg calls))))
      (jinx-mode-off-h)
      (jinx-mode-off-h (current-buffer))
      (expect calls :to-equal '(-1 -1)))))