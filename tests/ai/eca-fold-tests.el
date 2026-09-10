;;; tests/ai/eca-fold-tests.el --- ai/autoload/eca-fold.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; occult is absent here; the only entry point that reaches it is stubbed
;; per spec, with the three-overlay fold it would build.
(with-fake-feature 'occult
  (load-module-file "modules/ai/autoload/eca-fold.el"))

;; eca is absent too, and the fold command reads the symbol eca puts after
;; a finished tool call's label.
(defvar eca-chat-mcp-tool-call-success-symbol "✅")

;; eca's geometry: the label overlay is empty and created before the label
;; is inserted, so it sits on the label's first character; the content
;; overlay starts at the line after the label and ends at the start of the
;; line after the content, so it is empty while the block is collapsed.
;; Overlays need no major mode, so a temp buffer reproduces the real thing.
(defun fold-test-block (label &optional prompt content)
  "Insert a block labelled LABEL and return its overlay.
With PROMPT the block carries the user message id eca gives a prompt.
With CONTENT the block is expanded and shows it."
  (let ((ov (make-overlay (point) (point) nil nil nil)))
    (overlay-put ov 'eca-chat--expandable-content-id (format "id-%s" (point)))
    (when prompt
      (overlay-put ov 'eca-chat--user-message-id (format "id-%s" (point))))
    (insert label "\n")
    (let ((start (point)))
      (when content
        (overlay-put ov 'eca-chat--expandable-content-toggle t)
        (insert content "\n"))
      (overlay-put ov 'eca-chat--expandable-content-ov-content
                   (make-overlay start (point))))
    ov))

(defun fold-test-prompt (label) (fold-test-block label t))

(defun fold-test-lines (region)
  "Turn REGION into (FIRST-LINE . LAST-LINE), which reads better."
  (cons (line-number-at-pos (car region))
        (line-number-at-pos (cdr region))))

(defun fold-test-occult (beg end)
  "Build the fold `occult-hide-region' would over BEG to END.
The parent owns the region, the head is empty at BEG and carries the
indicator, the body hides the tail behind the ellipsis."
  (let ((parent (make-overlay beg end))
        (head (make-overlay beg beg))
        (body (make-overlay (min end (+ beg 3)) end)))
    (overlay-put parent 'occult t)
    (overlay-put parent 'occult-head head)
    (overlay-put parent 'occult-body body)
    (overlay-put head 'before-string "📎 ")
    (overlay-put body 'before-string "...")
    parent))

(defmacro fold-test-hiding (hidden &rest body)
  "Run BODY with `occult-hide-region' stubbed, collecting regions in HIDDEN.
The stub keeps the real function's contract: it builds the fold,
deactivates the mark and returns t."
  (declare (indent 1))
  `(let (,hidden)
     (cl-letf (((symbol-function 'occult-hide-region)
                (lambda (beg end)
                  (setq ,hidden (append ,hidden (list (cons beg end))))
                  (fold-test-occult beg end)
                  (deactivate-mark)
                  t)))
       ,@body)))

(defun fold-test-chat ()
  "Two turns laid out as eca does: blocks one blank line apart, prose right after."
  (fold-test-prompt "first question")                 ; 1
  (insert "\n")                                        ; 2
  (fold-test-block "Thought 1s")                       ; 3
  (insert "\n")                                        ; 4
  (insert "I'll start with the spec.\n")               ; 5
  (fold-test-block "Reading spec.md ✅ 0s")            ; 6
  (insert "\n")                                        ; 7
  (fold-test-block "Thought 2s")                       ; 8
  (insert "\n")                                        ; 9
  (insert "Tree matches the spec.\n")                  ; 10
  (fold-test-block "Git status: tree state ✅ 0s")     ; 11
  (insert "\n")                                        ; 12
  (fold-test-prompt "second question")                 ; 13
  (insert "\n")                                        ; 14
  (fold-test-block "Thought 3s")                       ; 15
  (insert "\n")                                        ; 16
  (insert "Done.\n"))                                  ; 17

(describe "eca-chat--fold-block-region"
  (it "is the label line of a collapsed block"
    (with-temp-buffer
      (let ((block (fold-test-block "$ ls ✅ 0s")))
        (insert "next line\n")
        (expect (fold-test-lines (eca-chat--fold-block-region block))
                :to-equal '(1 . 1)))))

  (it "reaches the last content line of an expanded block"
    (with-temp-buffer
      (let ((block (fold-test-block "$ ls ✅ 0s" nil "line one\nline two")))
        (insert "next line\n")
        (expect (fold-test-lines (eca-chat--fold-block-region block))
                :to-equal '(1 . 3)))))

  (it "covers every line of a multi-line label"
    (with-temp-buffer
      (let ((block (fold-test-prompt "a question\nover two lines")))
        (insert "next line\n")
        (expect (fold-test-lines (eca-chat--fold-block-region block))
                :to-equal '(1 . 2))))))

(describe "eca-chat--fold-runs"
  (it "joins blocks separated only by blank lines and stops at prompts and prose"
    (with-temp-buffer
      (fold-test-chat)
      (expect (mapcar #'fold-test-lines (eca-chat--fold-runs))
              :to-equal '((3 . 3) (6 . 8) (11 . 11) (15 . 15)))))

  (it "takes an expanded block whole, nested blocks included"
    (with-temp-buffer
      (fold-test-block "Thought 1s")
      (insert "\n")
      (let ((parent (fold-test-block "Subagent: explorer ✅ 3s" nil "child output")))
        ;; a nested block lives inside its parent's content
        (save-excursion
          (goto-char (overlay-start (overlay-get parent 'eca-chat--expandable-content-ov-content)))
          (fold-test-block "$ rg foo ✅ 0s")))
      (insert "\nThe reply.\n")
      (expect (mapcar #'fold-test-lines (eca-chat--fold-runs))
              :to-equal '((1 . 5)))))

  (it "is empty without blocks"
    (with-temp-buffer
      (insert "welcome\n")
      (expect (eca-chat--fold-runs) :to-be nil))))

(describe "eca-chat-fold"
  (it "folds every stretch into one occult fold and counts them"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (expect (eca-chat-fold) :to-equal 4)
        (expect (mapcar #'fold-test-lines hidden)
                :to-equal '((3 . 3) (6 . 8) (11 . 11) (15 . 15))))))

  (it "leaves occult's fold as occult built it"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (dolist (ov (seq-filter (lambda (ov) (overlay-get ov 'occult))
                                (overlays-in (point-min) (point-max))))
          (expect (overlay-get (overlay-get ov 'occult-head) 'before-string)
                  :to-equal "📎 ")))))

  (it "keeps the reader's selection"
    (with-temp-buffer
      (fold-test-chat)
      (push-mark (point-min) t t)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (expect mark-active :to-be-truthy))))

  (it "ends the fold summaries before a tool call's status symbol"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold))
      (expect (local-variable-p 'occult-summary-end-regexp) :to-be-truthy)
      (let ((label "Reading spec.md ✅ 0s"))
        (expect (substring label 0 (string-match occult-summary-end-regexp label))
                :to-equal "Reading spec.md"))))

  (it "follows eca's own success symbol"
    (with-temp-buffer
      (fold-test-chat)
      (let ((eca-chat-mcp-tool-call-success-symbol "OK"))
        (fold-test-hiding hidden
          (eca-chat-fold)))
      (expect occult-summary-end-regexp :to-equal " OK"))))

(describe "eca-chat-fold-h"
  (it "does nothing when automatic folding is off"
    (with-temp-buffer
      (fold-test-chat)
      (let ((eca-chat-fold-automatically nil))
        (fold-test-hiding hidden
          (eca-chat-fold-h)
          (expect hidden :to-be nil))))))

(describe "eca-chat-refold-after-protect-a"
  (it "guards the re-protect that wipes the folds"
    (expect (advice-member-p 'eca-chat-refold-after-protect-a
                             'eca-chat--protect-non-prompt)
            :to-be-truthy))

  (it "rebuilds the folds the re-protect deleted, keeping its result"
    (with-temp-buffer
      (fold-test-chat)
      (let* ((runs (eca-chat--fold-runs))
             (folds (mapcar (lambda (run) (fold-test-occult (car run) (cdr run))) runs)))
        (fold-test-hiding hidden
          (expect (eca-chat-refold-after-protect-a
                   (lambda (&rest _) (mapc #'delete-overlay folds) 'protected)
                   (point-min))
                  :to-be 'protected)
          (expect (mapcar #'fold-test-lines hidden)
                  :to-equal (mapcar #'fold-test-lines runs))))))

  (it "leaves the folds the re-protect spared alone"
    (with-temp-buffer
      (fold-test-chat)
      (dolist (run (eca-chat--fold-runs))
        (fold-test-occult (car run) (cdr run)))
      (fold-test-hiding hidden
        (eca-chat-refold-after-protect-a #'ignore)
        (expect hidden :to-be nil))))

  (it "leaves a fold the reader opened alone"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-refold-after-protect-a #'ignore)
        (expect hidden :to-be nil))))

  (it "keeps the reader's selection while the chat streams"
    (with-temp-buffer
      (fold-test-chat)
      (let* ((run (car (eca-chat--fold-runs)))
             (fold (fold-test-occult (car run) (cdr run))))
        (push-mark (point-min) t t)
        (fold-test-hiding hidden
          (eca-chat-refold-after-protect-a (lambda (&rest _) (delete-overlay fold)))
          (expect (length hidden) :to-equal 1)
          (expect mark-active :to-be-truthy))))))
