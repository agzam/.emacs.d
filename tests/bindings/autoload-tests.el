;;; tests/bindings/autoload-tests.el --- bindings/autoload.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/bindings/autoload.el")

;; The wrappers (require 'consult) before calling consult commands; consult
;; is not installed in the batch tier. Satisfy the require and stub the
;; commands per spec.
(provide 'consult)

(describe "delete-backward-word"
  (it "deletes a word without touching the kill-ring"
    (let ((kill-ring '("preexisting"))
          (kill-ring-yank-pointer nil))
      (with-temp-buffer
        (insert "foo bar")
        (delete-backward-word 1)
        (expect (buffer-string) :to-equal "foo "))
      (expect kill-ring :to-equal '("preexisting")))))

(describe "search wrappers"
  (it "search-cwd searches default-directory"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (let ((default-directory test-sandbox-dir))
          (search-cwd))
        (expect got :to-equal (list test-sandbox-dir nil)))))
  (it "search-emacsd searches user-emacs-directory"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (search-emacsd)
        (expect got :to-equal (list user-emacs-directory nil)))))
  (it "search-project falls back to default-directory outside projects"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (let ((default-directory test-sandbox-dir))
          (search-project))
        (expect got :to-equal (list test-sandbox-dir nil)))))
  (it "search-other-project searches the prompted project"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial))))
                ((symbol-function 'project-prompt-project-dir)
                 (lambda () "/tmp/other-project/")))
        (search-other-project)
        (expect got :to-equal '("/tmp/other-project/" nil)))))
  (it "search-notes-for-symbol-at-point searches org-directory for the symbol"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (with-temp-buffer
          (insert "needle")
          (goto-char (point-min))
          (call-interactively #'search-notes-for-symbol-at-point))
        (expect got :to-equal (list org-directory "needle")))))
  (it "search-buffer prefills the active region"
    (let (got)
      (cl-letf (((symbol-function 'consult-line)
                 (lambda (&rest args) (setq got args))))
        (with-temp-buffer
          (transient-mark-mode 1)
          (insert "needle in a haystack")
          (goto-char (point-min))
          (push-mark (point) t t)
          (goto-char (+ (point-min) 6))
          (search-buffer))
        (expect got :to-equal '("needle")))))
  (it "search-buffer passes nothing without a region"
    (let ((got 'untouched))
      (cl-letf (((symbol-function 'consult-line)
                 (lambda (&rest args) (setq got args))))
        (with-temp-buffer
          (insert "nothing marked")
          (search-buffer))
        (expect got :to-equal nil)))))

(describe "yank-buffer-path"
  ;; general/yank-path-tests.el loads yank-path.el in this same process,
  ;; installing its :override advice; lift it to test the underlying command.
  (before-each
    (advice-remove 'yank-buffer-path 'yank-path--buffer-path-a))
  (after-each
    (when (fboundp 'yank-path--buffer-path-a)
      (advice-add 'yank-buffer-path :override #'yank-path--buffer-path-a)))
  ;; file-truename on the fixtures: doom-defaults sets
  ;; find-file-visit-truename, and macOS temp dirs are symlinks
  ;; (/var -> /private/var).
  (it "kills the abbreviated file path"
    (let ((f (file-truename (make-temp-file "yank-probe")))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (unwind-protect
          (with-current-buffer (find-file-noselect f)
            (yank-buffer-path)
            (expect (car kill-ring) :to-equal (abbreviate-file-name f)))
        (when-let* ((buf (get-file-buffer f))) (kill-buffer buf))
        (delete-file f))))
  (it "resolves relative to ROOT when given"
    (let ((f (file-truename (make-temp-file "yank-probe")))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (unwind-protect
          (with-current-buffer (find-file-noselect f)
            (yank-buffer-path (file-name-directory f))
            (expect (car kill-ring)
                    :to-equal (file-name-nondirectory f)))
        (when-let* ((buf (get-file-buffer f))) (kill-buffer buf))
        (delete-file f))))
  (it "signals a user-error in non-file buffers"
    (with-temp-buffer
      (expect (yank-buffer-path) :to-throw 'user-error))))

(describe "backward-to-bol-or-indent"
  (it "cycles mid-line -> indentation -> bol -> back to indentation"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "  foo")
      (let ((backward-to-bol--last-pt nil))
        (goto-char (point-max))                 ; after "foo"
        (backward-to-bol-or-indent)
        (expect (point) :to-equal 3)            ; bot: before "f"
        (backward-to-bol-or-indent)
        (expect (point) :to-equal 1)            ; bol
        (backward-to-bol-or-indent)
        (expect (point) :to-equal 3)))))        ; bounce back to stored pt

(describe "forward-to-last-non-comment-or-eol"
  (it "jumps mid-line to the last non-comment char, then eol"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(foo) ;; bar")
      (let ((forward-to-eol--last-pt nil))
        (goto-char 2)
        (forward-to-last-non-comment-or-eol)
        (expect (point) :to-equal 6)            ; eot: after ")"
        (forward-to-last-non-comment-or-eol)
        (expect (point) :to-equal (line-end-position)))))

  (it "bounces back from eol to the stored position (doom rot fix)"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(foo) ;; bar")
      (let ((forward-to-eol--last-pt nil)
            (inside-comment 10))                ; on "b" of bar
        (goto-char inside-comment)
        (forward-to-last-non-comment-or-eol)
        (expect (point) :to-equal (line-end-position))
        ;; the doom.d original stored this in the backward mover's
        ;; variable, so this bounce always landed on eot instead
        (forward-to-last-non-comment-or-eol)
        (expect (point) :to-equal inside-comment)))))

(describe "backward-kill-to-bol-and-indent"
  (it "kills to bol (delete-region path without evil)"
    (with-temp-buffer
      (fundamental-mode)
      (insert "first\nsecond line")
      (goto-char (point-max))
      (backward-kill-to-bol-and-indent)
      (expect (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              :to-match "^[ \t]*$"))))

(describe "newline-below / newline-above"
  (it "newline-below opens an indented line after the current one"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(a)\n(b)")
      (goto-char 2)
      (newline-below)
      (expect (count-lines (point-min) (point-max)) :to-equal 3)
      (expect (line-number-at-pos) :to-equal 2)
      (expect (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              :to-equal "")))

  (it "newline-above opens an indented line before the current one"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(a)")
      (goto-char 2)
      (newline-above)
      (expect (count-lines (point-min) (point-max)) :to-equal 2)
      (expect (line-number-at-pos) :to-equal 1)
      (expect (buffer-substring-no-properties
               (point-min) (line-end-position))
              :to-equal ""))))

(describe "comment-current-line"
  (it "comments and uncomments in place, point stays on the line"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(foo)\n(bar)")
      (goto-char 3)
      (comment-current-line)
      (expect (line-number-at-pos) :to-equal 1)
      (expect (buffer-substring-no-properties 1 (line-end-position))
              :to-match "^;+ ?(foo)")
      (comment-current-line)
      (expect (buffer-substring-no-properties 1 (line-end-position))
              :to-equal "(foo)"))))
