;;; tests/e2e/markdown-special-edit.el --- C-c ' over markdown blocks -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The edit buffers, the keymaps that carry C-c ' there and back (evil's
;; prog-mode binding among them), and markdown-mode's after-commit fixup
;; exist only in a booted config.

(defun markdown-special-edit--snapshot ()
  "(MODE . TEXT) of the selected window's buffer, with a | at point in TEXT."
  (with-current-buffer (window-buffer (selected-window))
    (cons major-mode
          (concat (buffer-substring-no-properties (point-min) (point))
                  "|"
                  (buffer-substring-no-properties (point) (point-max))))))

(defun markdown-special-edit--act (label text steps)
  "Fill a markdown buffer with TEXT, point at its |, and run STEPS.
LABEL names the case.  Each step is (KEYS . WANT): the keys to press and
the snapshot they must leave."
  (let ((buf (find-file-noselect (expand-file-name "special.md" e2e-work-dir)))
        got want err)
    (unwind-protect
        (with-current-buffer buf
          (switch-to-buffer buf)
          (delete-other-windows)
          (erase-buffer)
          (insert (string-replace "|" "" text))
          (goto-char (1+ (string-search "|" text)))
          (font-lock-ensure)
          (evil-force-normal-state)
          (dolist (step steps)
            (unless err
              (discard-input)
              (condition-case e
                  (execute-kbd-macro (car step))
                (error (setq err e)))
              (push (markdown-special-edit--snapshot) got)
              (push (cdr step) want))))
      (dolist (b (buffer-list))
        (when (string-prefix-p "*edit-indirect " (buffer-name b))
          (kill-buffer b)))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))
    (setq got (nreverse got) want (nreverse want))
    (list :label label :got got :want want :err err
          :ok (and (null err) (equal got want)))))

(defun markdown-special-edit-e2e ()
  "Open, edit and close markdown code blocks and blockquotes with C-c '."
  (let ((toggle (kbd "C-c '"))
        (append-baz (vconcat "A baz" [escape])))
    (list
     (markdown-special-edit--act
      "markdown C-c ': drops the quote marks, C-c ' puts them back"
      "intro\n\n> fooo\n> |bar\n> zap\n> zop\n\noutro\n"
      `((,toggle markdown-mode . "fooo\n|bar\nzap\nzop\n")
        (,append-baz markdown-mode . "fooo\nbar ba|z\nzap\nzop\n")
        (,toggle markdown-mode
                 . "intro\n\n> fooo\n> bar ba|z\n> zap\n> zop\n\noutro\n")))
     ;; markdown-mode re-indents a commit by the line above it
     (markdown-special-edit--act
      "markdown C-c ': a quote under an indented line keeps its indent"
      "- item\n  more\n  > |quoted\n  > text\n\nend\n"
      `((,toggle markdown-mode . "|quoted\ntext\n")
        (,toggle markdown-mode . "- item\n  more\n  > |quoted\n  > text\n\nend\n")))
     (markdown-special-edit--act
      "markdown C-c ': nested and blank quote lines survive the round trip"
      "> |a\n>\n> > nested\n"
      `((,toggle markdown-mode . "|a\n\n> nested\n")
        (,toggle markdown-mode . "> |a\n>\n> > nested\n")))
     ;; only a commit returns point; after the save it sits at the quote start
     (markdown-special-edit--act
      "markdown C-c ': C-x C-s requotes the source, the edit buffer stays bare"
      "intro\n\n> fooo\n> |bar\n\noutro\n"
      `((,toggle markdown-mode . "fooo\n|bar\n")
        (,(vconcat append-baz (kbd "C-x C-s")) markdown-mode . "fooo\nbar ba|z\n")
        (,(kbd "C-c C-k") markdown-mode . "intro\n\n|> fooo\n> bar baz\n\noutro\n")))
     ;; a prog-mode edit buffer, where evil's own C-c ' binding lives
     (markdown-special-edit--act
      "markdown C-c ': a fenced elisp block opens in emacs-lisp-mode and closes"
      "intro\n\n```elisp\n(|foo)\n```\n"
      `((,toggle emacs-lisp-mode . "(|foo)\n")
        (,toggle markdown-mode . "intro\n\n```elisp\n(|foo)\n```\n")))
     (markdown-special-edit--act
      "markdown C-c ': a quote-like line in a code block edits as code"
      "```\n> |prompt\n```\n"
      `((,toggle fundamental-mode . "> |prompt\n")
        (,toggle markdown-mode . "```\n> |prompt\n```\n"))))))

(add-to-list 'e2e-scenarios #'markdown-special-edit-e2e)
