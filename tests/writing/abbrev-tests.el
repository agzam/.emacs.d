;;; tests/writing/abbrev-tests.el --- writing/autoload/abbrev.el specs -*- lexical-binding: t; -*-

;; A contraction typed at speed lands its apostrophe after the word, so abbrev
;; meets "dont'" instead of "don't".  What that costs depends on one thing
;; only: the syntax class of the apostrophe in the buffer's syntax table.
;; Punctuation (`markdown-mode', `eca-chat-mode', `git-commit-mode') ends the
;; word, so the apostrophe fires the expansion and lands behind it; word
;; syntax (`text-mode', `org-mode') makes it part of the abbrev name, so
;; nothing matches.  Both regimes are built here from `text-mode', because
;; markdown-mode is an elpaca package and the batch tier has none; the real
;; modes are driven by real keys in tests/e2e/abbrev-contractions.el.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/abbrev.el")

(defun abbrev-tests-type (regime keys)
  "Type KEYS one `self-insert-command' at a time under syntax REGIME.
REGIME is `punctuation' or `word', naming the syntax class the buffer gives
the apostrophe.  The table is a copy: the modes it stands for share theirs."
  (with-temp-buffer
    (text-mode)
    (set-syntax-table (copy-syntax-table text-mode-syntax-table))
    (modify-syntax-entry ?' (if (eq regime 'punctuation) "." "w p"))
    (abbrev-mode 1)
    (dolist (char (string-to-list keys))
      (let ((last-command-event char))
        (call-interactively #'self-insert-command)))
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "late apostrophe in abbrev expansion"
  :var* ((real-table global-abbrev-table)
         (real-expand abbrev-expand-function))

  (before-each
    (setq global-abbrev-table (make-abbrev-table))
    (define-abbrev global-abbrev-table "dont" "don't")
    (define-abbrev global-abbrev-table "ive" "I've")
    (define-abbrev global-abbrev-table "alot" "a lot")
    ;; the wiring `modules/writing/config.el' installs
    (setq abbrev-expand-function #'expand-abbrev-tolerating-late-apostrophe)
    (add-hook 'post-self-insert-hook #'drop-redundant-apostrophe-h :append))

  (after-each
    (setq global-abbrev-table real-table
          abbrev-expand-function real-expand)
    (remove-hook 'post-self-insert-hook #'drop-redundant-apostrophe-h))

  (dolist (regime '(punctuation word))
    (describe (format "with an apostrophe of %s syntax" regime)

      (it "corrects the typo when the apostrophe lands after the word"
        (expect (abbrev-tests-type regime "dont' ") :to-equal "don't ")
        (expect (abbrev-tests-type regime "ive' ") :to-equal "I've "))

      (it "still corrects the plain typo"
        (expect (abbrev-tests-type regime "dont ") :to-equal "don't "))

      (it "leaves a correctly typed contraction alone"
        (expect (abbrev-tests-type regime "don't ") :to-equal "don't "))

      (it "keeps an apostrophe the expansion does not supply"
        (expect (abbrev-tests-type regime "alot' ") :to-equal "a lot' "))

      (it "keeps a possessive apostrophe on a word it knows nothing about"
        (expect (abbrev-tests-type regime "dogs' ") :to-equal "dogs' "))))

  (it "expands on the apostrophe itself where that ends the word"
    (expect (abbrev-tests-type 'punctuation "dont'") :to-equal "don't"))

  (it "waits for a word terminator where the apostrophe is a word character"
    ;; the name is still open, so nothing has fired yet
    (expect (abbrev-tests-type 'word "dont'") :to-equal "dont'")))
