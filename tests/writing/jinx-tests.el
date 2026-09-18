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

(defvar jinx-tests-force-args nil
  "Arguments the stubbed `jinx--force-overlays' was last called with.")

(defun jinx-tests-word-overlay (word)
  "Overlay over the first occurrence of WORD in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward word)
    (make-overlay (match-beginning 0) (match-end 0))))

(defun jinx-tests-autocorrect (overlays suggestions)
  "Run `jinx-autocorrect-last' against stubbed jinx internals.
OVERLAYS is what `jinx--force-overlays' returns, SUGGESTIONS an alist of
misspelling to corrections."
  (setq jinx-tests-force-args nil)
  (cl-letf (((symbol-function 'jinx--correct-guard)
             (cons 'macro (lambda (&rest forms) (macroexp-progn forms))))
            ;; jinx's own arglist, so an arity drift fails here too
            ((symbol-function 'jinx--force-overlays)
             (lambda (start end &optional visible)
               (setq jinx-tests-force-args (list start end visible))
               (copy-sequence overlays)))
            ((symbol-function 'jinx--correct-suggestions)
             (lambda (word) (cdr (assoc word suggestions))))
            ((symbol-function 'jinx--correct-replace)
             (lambda (ov word)
               (goto-char (overlay-start ov))
               (delete-region (overlay-start ov) (overlay-end ov))
               (insert word)
               (delete-overlay ov)))
            ((symbol-function 'pulse-momentary-highlight-region) #'ignore)
            ((symbol-function 'message) #'ignore))
    (jinx-autocorrect-last)))

(describe "jinx-autocorrect-last"
  (before-each
    (setq jinx-autocorrect--ts nil
          jinx-autocorrect--pos nil
          jinx-autocorrect--suggestions nil))

  (it "asks jinx--force-overlays for visible overlays positionally"
    (with-temp-buffer
      (insert "Fix the wrod.")
      (jinx-tests-autocorrect (list (jinx-tests-word-overlay "wrod"))
                              '(("wrod" "word" "ward")))
      (expect jinx-tests-force-args :to-equal (list (point-min) (point-max) t))
      (expect (buffer-string) :to-equal "Fix the word.")
      (expect jinx-autocorrect--pos :to-equal 9)))

  (it "corrects the misspelling nearest to point, whatever order jinx returns"
    ;; `jinx--get-overlays' rotates its result around point, so the list
    ;; jinx hands over need not end on the word the user just typed.
    (with-temp-buffer
      (insert "Kubernets is fine. I made a mistke")
      (jinx-tests-autocorrect (list (jinx-tests-word-overlay "mistke")
                                    (jinx-tests-word-overlay "Kubernets"))
                              '(("mistke" "mistake")
                                ("Kubernets" "Kubernetes")))
      (expect (buffer-string) :to-equal "Kubernets is fine. I made a mistake"))))

(describe "jinx-mode-off-h"
  :var (calls)

  (before-each (setq calls nil))

  (it "swallows any hook arity (wiktionary passes none, github-topics one)"
    (cl-letf (((symbol-function 'jinx-mode)
               (lambda (&optional arg) (push arg calls))))
      (jinx-mode-off-h)
      (jinx-mode-off-h (current-buffer))
      (expect calls :to-equal '(-1 -1)))))