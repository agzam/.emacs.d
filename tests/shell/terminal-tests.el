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
;; `start-new-terminal' calls into the sibling file; load the real definition
;; so the spies restore it rather than a stub of it.
(load-module-file "modules/shell/autoload/shell.el")

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

  (it "starts one when none is live, rather than refusing"
    (let (started)
      (spy-on 'terminal-buffers :and-call-fake
              (lambda () (and started (list one))))
      (spy-on 'shell-pop-choose :and-call-fake
              (lambda (&optional _arg) (setq started t)))
      (expect (read-terminal-buffer) :to-be one)
      (expect 'shell-pop-choose :to-have-been-called)))

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

(describe "start-new-terminal"
  (it "refuses when the picker started nothing"
    (spy-on 'shell-pop-choose)
    (spy-on 'terminal-buffers :and-return-value nil)
    (expect (start-new-terminal) :to-throw 'user-error))

  (it "hands over an eshell the moment it is there"
    (let ((term (terminal-spec-buffer 'eshell-mode)))
      (unwind-protect
          (progn
            (spy-on 'shell-pop-choose)
            (spy-on 'terminal-buffers :and-return-value (list term))
            (spy-on 'accept-process-output)
            (expect (start-new-terminal) :to-be term)
            (expect 'accept-process-output :not :to-have-been-called))
        (kill-buffer term))))

  ;; a ghostel spawns its shell asynchronously, and a paste that beats the
  ;; shell's line editor to the pty is read as literal escape sequences
  (it "waits for a ghostel prompt before handing the buffer over"
    (let ((term (terminal-spec-buffer 'ghostel-mode))
          (polls 0))
      (unwind-protect
          (progn
            (spy-on 'shell-pop-choose)
            (spy-on 'terminal-buffers :and-return-value (list term))
            (spy-on 'accept-process-output :and-call-fake
                    (lambda (&rest _)
                      (when (= 2 (cl-incf polls))
                        (with-current-buffer term
                          (insert "~ % ")
                          (put-text-property (point-min) (point-max)
                                             'ghostel-prompt t)))
                      t))
            (expect (start-new-terminal) :to-be term)
            (expect polls :to-equal 2))
        (kill-buffer term))))

  (it "gives up on a silent ghostel instead of blocking forever"
    (let ((term (terminal-spec-buffer 'ghostel-mode))
          (terminal-start-timeout 0.2))
      (unwind-protect
          (progn
            (spy-on 'shell-pop-choose)
            (spy-on 'terminal-buffers :and-return-value (list term))
            (spy-on 'accept-process-output :and-call-fake
                    (lambda (&rest _) (sleep-for 0.05) nil))
            (expect (start-new-terminal) :to-be term))
        (kill-buffer term)))))

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
