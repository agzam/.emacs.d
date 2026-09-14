;;; tests/shell/terminal-tests.el --- shell/autoload/terminal.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; No evil in the batch tier; the state switch is driven for real against a
;; live eshell in tests/e2e/send-to-terminal.el.
(defvar evil-state nil)
(defun evil-insert-state (&optional _arg) (setq evil-state 'insert))

(load-module-file "modules/shell/autoload/terminal.el")

;; Neither eshell nor ghostel runs in the batch tier, and starting one would
;; test the shell rather than this file: a buffer carrying the major mode is
;; all `terminal-buffers' and `terminal-insert' ever look at.  The window
;; behaviour is stubbed here and driven for real in
;; tests/e2e/send-to-terminal.el.
(defun terminal-spec-buffer (mode)
  "A fresh buffer whose `major-mode' is MODE."
  (let ((buf (generate-new-buffer " *terminal-spec*")))
    (with-current-buffer buf (setq major-mode mode))
    buf))

(describe "terminal-buffers"
  :var (eshell-buf ghostel-buf plain-buf)
  (before-each
    (setq plain-buf (terminal-spec-buffer 'text-mode)
          eshell-buf (terminal-spec-buffer 'eshell-mode)
          ghostel-buf (terminal-spec-buffer 'ghostel-mode)))
  (after-each
    (mapc #'kill-buffer (list plain-buf eshell-buf ghostel-buf)))

  (it "keeps eshell and ghostel buffers and drops everything else"
    (let ((found (terminal-buffers)))
      (expect (memq eshell-buf found) :to-be-truthy)
      (expect (memq ghostel-buf found) :to-be-truthy)
      (expect (memq plain-buf found) :to-be nil)))

  (it "orders them the way `buffer-list' does, most recent first"
    (cl-letf (((symbol-function 'buffer-list)
               (lambda (&rest _) (list plain-buf ghostel-buf eshell-buf))))
      (expect (terminal-buffers) :to-equal (list ghostel-buf eshell-buf)))))

(describe "read-terminal-buffer"
  :var (one two)
  (before-each
    (setq one (terminal-spec-buffer 'eshell-mode)
          two (terminal-spec-buffer 'ghostel-mode)))
  (after-each (mapc #'kill-buffer (list one two)))

  (it "refuses when no terminal is live"
    (spy-on 'terminal-buffers :and-return-value nil)
    (expect (read-terminal-buffer) :to-throw 'user-error))

  (it "takes the only terminal without asking"
    (spy-on 'terminal-buffers :and-return-value (list one))
    (spy-on 'completing-read)
    (expect (read-terminal-buffer) :to-be one)
    (expect 'completing-read :not :to-have-been-called))

  (it "asks once more than one is live"
    (spy-on 'terminal-buffers :and-return-value (list two one))
    (spy-on 'completing-read :and-return-value (buffer-name one))
    (expect (read-terminal-buffer) :to-be one)
    (expect 'completing-read :to-have-been-called)))

(describe "terminal-buffer-table"
  :var (table)
  (before-each
    (setq table (terminal-buffer-table
                 (list (get-buffer-create " *zeta-spec*")
                       (get-buffer-create " *alpha-spec*")))))
  (after-each
    (mapc #'kill-buffer '(" *zeta-spec*" " *alpha-spec*")))

  (it "completes over the buffer names"
    (expect (all-completions "" table)
            :to-equal '(" *zeta-spec*" " *alpha-spec*")))

  (it "declares the buffer category so the picker annotates the rows"
    (expect (alist-get 'category (cdr (funcall table "" nil 'metadata)))
            :to-be 'buffer))

  (it "keeps the given order rather than sorting the names"
    (expect (alist-get 'display-sort-function
                       (cdr (funcall table "" nil 'metadata)))
            :to-be 'identity)))

(describe "terminal-insert"
  (it "appends at the prompt of an eshell buffer"
    (let ((buf (terminal-spec-buffer 'eshell-mode)))
      (unwind-protect
          (with-current-buffer buf
            (insert "$ ")
            (goto-char (point-min))
            (terminal-insert "ls -la")
            (expect (buffer-string) :to-equal "$ ls -la"))
        (kill-buffer buf))))

  (it "hands a ghostel buffer a bracketed paste instead of typed keys"
    (let ((buf (terminal-spec-buffer 'ghostel-mode))
          pasted)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-paste-string)
                     (lambda (s) (setq pasted s))))
            (with-current-buffer buf (terminal-insert "ls -la"))
            (expect pasted :to-equal "ls -la")
            ;; the text never lands in the buffer: the terminal echoes it back
            (expect (with-current-buffer buf (buffer-string)) :to-equal ""))
        (kill-buffer buf)))))

(describe "send-to-terminal-text"
  (it "prefers the region when one is active"
    (with-temp-buffer
      (insert "pick me")
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (expect (send-to-terminal-text) :to-equal "pick me")))

  (it "falls back to the code snippet at point"
    (spy-on 'code-snippet-at-point :and-return-value '("ls -la" 5 . 13))
    (with-temp-buffer
      (expect (send-to-terminal-text) :to-equal "ls -la")))

  (it "refuses when there is neither"
    (spy-on 'code-snippet-at-point :and-return-value nil)
    (with-temp-buffer
      (expect (send-to-terminal-text) :to-throw 'user-error))))

(describe "send-to-terminal"
  :var (term)
  (before-each
    (setq term (terminal-spec-buffer 'eshell-mode))
    (spy-on 'read-terminal-buffer :and-return-value term))
  (after-each (kill-buffer term))

  (it "inserts the text without running it"
    (spy-on 'pop-to-buffer)
    (send-to-terminal "ls -la")
    (expect (with-current-buffer term (buffer-string)) :to-equal "ls -la"))

  (it "brings the terminal up and moves to it"
    (spy-on 'pop-to-buffer)
    (send-to-terminal "ls -la")
    (expect 'pop-to-buffer :to-have-been-called-with term))

  (it "leaves the terminal in insert state, ready for RET"
    (spy-on 'pop-to-buffer)
    (spy-on 'evil-insert-state)
    (send-to-terminal "ls -la")
    (expect 'evil-insert-state :to-have-been-called)))
