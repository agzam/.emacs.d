;;; tests/general/code-snippet-tests.el --- general/autoload/code-snippet.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)

(load-module-file "modules/general/autoload/code-snippet.el")

;; markdown-mode isn't installed in the batch tier, so the inline markdown
;; branch and every dispatch that needs `derived-mode-p' on it live in
;; tests/e2e/send-to-terminal.el.  The fence pairing reads buffer text only,
;; which is the whole point of not routing it through font-lock.

(defun snippet-in-org (text pos)
  "Snippet at POS in an org buffer holding TEXT."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert text)
    (goto-char pos)
    (code-snippet-at-point)))

(describe "code-snippet-at-point in org"
  (it "reads inline code without its tildes"
    (expect (snippet-in-org "run ~ls -la~ now" 8)
            :to-equal '("ls -la" 5 . 13)))

  (it "reads verbatim text without its equal signs"
    (expect (snippet-in-org "say =echo hi= now" 8)
            :to-equal '("echo hi" 5 . 14)))

  (it "stops at the closing delimiter rather than at the next word"
    ;; org's :end runs over the post-blank whitespace after the element
    (let ((snippet (snippet-in-org "run ~ls~   now" 6)))
      (expect (cddr snippet) :to-equal 9)))

  (it "finds nothing in plain prose"
    (expect (snippet-in-org "run ls -la now" 6) :to-be nil))

  (it "does not mistake a lone tilde for code"
    (expect (snippet-in-org "cd ~ and back" 4) :to-be nil)))

(defun fenced-snippet (text pos)
  "Fenced block at POS in a buffer holding TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char pos)
    (markdown-fenced-code-snippet)))

(defconst fenced-fixture "intro\n\n```sh\necho hi\nls -l\n```\n\noutro\n"
  "Prose either side of one fenced block, for the pairing specs.")

(describe "markdown-fenced-code-snippet"
  (it "returns the body between the fences, trailing newline trimmed"
    (expect (car (fenced-snippet fenced-fixture 20))
            :to-equal "echo hi\nls -l"))

  (it "spans the fences themselves"
    (expect (cdr (fenced-snippet fenced-fixture 20)) :to-equal '(8 . 31)))

  (it "counts a fence line itself as inside the block"
    (expect (car (fenced-snippet fenced-fixture 8))
            :to-equal "echo hi\nls -l"))

  (it "finds nothing before the block"
    (expect (fenced-snippet fenced-fixture 2) :to-be nil))

  (it "finds nothing after the block"
    (expect (fenced-snippet fenced-fixture 33) :to-be nil))

  (it "pairs tilde fences too"
    (expect (car (fenced-snippet "~~~\necho hi\n~~~\n" 5))
            :to-equal "echo hi"))

  (it "picks the block point is in when several follow each other"
    (let ((text "```\nfirst\n```\n\n```\nsecond\n```\n"))
      (expect (car (fenced-snippet text 6)) :to-equal "first")
      (expect (car (fenced-snippet text 21)) :to-equal "second")
      ;; between the two blocks belongs to neither
      (expect (fenced-snippet text 15) :to-be nil)))

  (it "leaves an unclosed fence alone"
    (expect (fenced-snippet "```\necho hi\n" 6) :to-be nil)))

(describe "embark-target-code-snippet"
  (it "reports the type, the code and the bounds the delimiters cover"
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "run ~ls -la~ now")
      (goto-char 8)
      (expect (embark-target-code-snippet)
              :to-equal '(code-snippet "ls -la" 5 . 13))))

  (it "reports nothing where there is no snippet"
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "run ls -la now")
      (goto-char 6)
      (expect (embark-target-code-snippet) :to-be nil))))
