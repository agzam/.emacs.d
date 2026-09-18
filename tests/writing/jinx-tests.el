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

(describe "jinx-autocorrect-last"
  :var (force-args)

  (before-each
    (setq force-args nil
          jinx-autocorrect--ts nil
          jinx-autocorrect--pos nil
          jinx-autocorrect--suggestions nil))

  (it "asks jinx--force-overlays for visible overlays positionally"
    ;; The stub mirrors jinx's own arglist, so a keyword call fails here
    ;; the same way it fails against the real function.
    (with-temp-buffer
      (insert "Fix the wrod.")
      (let ((ov (make-overlay 9 13)))
        (cl-letf (((symbol-function 'jinx--correct-guard)
                   (cons 'macro (lambda (&rest body) (macroexp-progn body))))
                  ((symbol-function 'jinx--force-overlays)
                   (lambda (start end &optional visible)
                     (setq force-args (list start end visible))
                     (list ov)))
                  ((symbol-function 'jinx--correct-suggestions)
                   (lambda (_word) '("word" "ward")))
                  ((symbol-function 'jinx--correct-replace)
                   (lambda (o word)
                     (goto-char (overlay-start o))
                     (delete-region (overlay-start o) (overlay-end o))
                     (insert word)
                     (delete-overlay o)))
                  ((symbol-function 'pulse-momentary-highlight-region) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (jinx-autocorrect-last)))
      (expect (length force-args) :to-equal 3)
      (expect (nth 2 force-args) :to-be t)
      (expect (buffer-string) :to-equal "Fix the word.")
      (expect jinx-autocorrect--pos :to-equal 9))))

(describe "jinx-mode-off-h"
  :var (calls)

  (before-each (setq calls nil))

  (it "swallows any hook arity (wiktionary passes none, github-topics one)"
    (cl-letf (((symbol-function 'jinx-mode)
               (lambda (&optional arg) (push arg calls))))
      (jinx-mode-off-h)
      (jinx-mode-off-h (current-buffer))
      (expect calls :to-equal '(-1 -1)))))