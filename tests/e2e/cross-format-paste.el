;;; tests/e2e/cross-format-paste.el --- markdown<->org paste -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; `prisma-yank-mode' converts a kill to the format of the buffer it
;; lands in.  None of that chain is visible to a with-temp-buffer spec:
;; the mode arrives on doom-first-input, the kill picks up its format
;; from a real markdown or org buffer, and evil's paste command carries
;; it across.  Org -> Markdown needs no tree-sitter grammar - only the
;; markdown parser does - so it runs everywhere, CI included; the
;; opposite direction reports a loud skip when the grammar is missing.

(require 'cl-lib)

(defconst cross-format-paste-org "* Heading\n\nSome *bold* text.\n")
(defconst cross-format-paste-md "# Heading\n\nSome **bold** text.\n")

(defun cross-format-paste--press (buf text keys)
  "Fill BUF with TEXT, put point on line one, press KEYS.
Returns the errors the keys raised, newest first."
  (let (errs)
    (with-current-buffer buf
      (switch-to-buffer buf)
      (delete-other-windows)
      (erase-buffer)
      (insert text)
      (font-lock-ensure)
      (evil-force-normal-state)
      (goto-char (point-min)))
    (discard-input)
    (condition-case e
        (execute-kbd-macro keys)
      (error (push (list 'execute-kbd-macro keys e) errs)))
    errs))

(defun cross-format-paste--act (label src src-text dst dst-text keys want)
  "Yank all of SRC-TEXT in SRC, then press KEYS in DST holding DST-TEXT.
LABEL names the case, WANT is the DST contents the paste must leave."
  (let* ((errs (append (cross-format-paste--press src src-text (vconcat "yG"))
                       (cross-format-paste--press dst dst-text keys)))
         (got (with-current-buffer dst
                (buffer-substring-no-properties (point-min) (point-max)))))
    (list :label label
          :ok (and (null errs) (equal got want))
          :got got :want want :err (nreverse errs))))

(defun cross-format-paste-e2e ()
  "Paste across markdown and org through the keys that do it."
  (let* ((org (find-file-noselect (expand-file-name "case.org" e2e-work-dir)))
         (md (find-file-noselect (expand-file-name "case.md" e2e-work-dir)))
         (results '()))
    (unwind-protect
        (progn
          (push (cross-format-paste--act
                 "cross-format paste: org yank pasted into markdown"
                 org cross-format-paste-org
                 md "top\n" "p"
                 (concat "top\n" cross-format-paste-md))
                results)
          ;; the first act already ran a command, so the mode is due
          (push (list :label "cross-format paste: doom-first-input turned the mode on"
                      :ok (and (bound-and-true-p prisma-yank-mode) t)
                      :got (format "%s" (bound-and-true-p prisma-yank-mode))
                      :want "t")
                results)
          (push (cross-format-paste--act
                 "cross-format paste: bare C-u pastes the org kill verbatim"
                 org cross-format-paste-org
                 md "top\n" (vconcat "\C-u" "p")
                 (concat "top\n" cross-format-paste-org))
                results)
          (push (if (cl-every #'treesit-language-available-p
                              '(markdown markdown-inline))
                    (cross-format-paste--act
                     "cross-format paste: markdown yank pasted into org"
                     md cross-format-paste-md
                     org "top\n" "p"
                     (concat "top\n" cross-format-paste-org))
                  (list :label "cross-format paste: markdown -> org SKIPPED, \
no markdown tree-sitter grammar"
                        :ok t :got "skip" :want "skip"))
                results))
      (discard-input)
      (dolist (buf (list org md))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'cross-format-paste-e2e)
