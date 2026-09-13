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
;; eca sets this to where the newest answer starts, right after the prompt.
(defvar eca-chat--last-user-message-pos nil)
;; occult's own option; buffer-local in occult, a plain variable here.
(defvar occult-noise-regions-function nil)

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

(defun fold-test-injected ()
  "Insert the user message a finished background job arrives as.
eca gives it the same overlay properties as a typed prompt."
  (fold-test-block "Background job job-3 (`bb test`) completed with exit code 0.
Last 20 lines of output:
Ran 1609 specs, 0 failed."
                   t))

(defun fold-test-summary (label)
  "Return LABEL as the buffer's replacement rules would show it.
occult replaces what a match displays rather than the text itself; the
rules are the same either way, and occult's own suite covers the
rendering."
  (let ((text label))
    (pcase-dolist (`(,regexp . ,replacement) occult-summary-replace-alist)
      (setq text (replace-regexp-in-string regexp replacement text t)))
    text))

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

(defun fold-test-fold-noise (&optional beg end)
  "What `occult-fold-noise' does with the stretches the buffer names.
Every stretch reaching into BEG..END that no fold hides in full goes
through `occult-hide-region' with the mark left alone; the count comes
back.  occult's own suite covers the real one."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max)))
        (folded 0))
    (dolist (region (funcall occult-noise-regions-function))
      (when (and (< (car region) end)
                 (< beg (cdr region))
                 (not (seq-find (lambda (ov)
                                  (and (overlay-get ov 'occult)
                                       (<= (overlay-start ov) (car region))
                                       (<= (cdr region) (overlay-end ov))))
                                (overlays-in (car region) (cdr region)))))
        (let ((mark-active nil))
          (when (occult-hide-region (car region) (cdr region))
            (cl-incf folded)))))
    folded))

(defun fold-test-folds ()
  "The occult folds of the buffer, in buffer order."
  (sort (seq-filter (lambda (ov) (overlay-get ov 'occult))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun fold-test-open (fold)
  "Take FOLD apart the way `occult-toggle' does when the reader opens it."
  (delete-overlay (overlay-get fold 'occult-head))
  (delete-overlay (overlay-get fold 'occult-body))
  (delete-overlay fold))

(defun fold-test-open-at-line (line)
  "Open the fold whose first line is LINE."
  (fold-test-open (seq-find (lambda (ov) (= (line-number-at-pos (overlay-start ov)) line))
                            (fold-test-folds))))

(defmacro fold-test-hiding (hidden &rest body)
  "Run BODY with occult's folding stubbed, collecting regions in HIDDEN.
The `occult-hide-region' stub keeps the real function's contract: it
builds the fold, deactivates the mark and returns t.  `occult-fold-noise'
is `fold-test-fold-noise', and `occult-reveal-all' opens every fold."
  (declare (indent 1))
  `(let (,hidden)
     (cl-letf (((symbol-function 'occult-hide-region)
                (lambda (beg end)
                  (setq ,hidden (append ,hidden (list (cons beg end))))
                  (fold-test-occult beg end)
                  (deactivate-mark)
                  t))
               ((symbol-function 'occult-fold-noise) #'fold-test-fold-noise)
               ((symbol-function 'occult-reveal-all)
                (lambda () (mapc #'fold-test-open (fold-test-folds)))))
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
  ;; eca marks the newest answer's start right after the prompt block
  (setq-local eca-chat--last-user-message-pos (point))
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
      (expect (eca-chat--fold-runs) :to-be nil)))

  ;; A background job reports through the chat as a user message.  It is
  ;; not the reader talking, so it folds like any other noise.
  (it "folds a job report and joins it to the stretch beside it"
    (with-temp-buffer
      (fold-test-block "Thought 1s")                    ; 1
      (insert "\n")                                     ; 2
      (fold-test-injected)                              ; 3-5
      (insert "\n")                                     ; 6
      (fold-test-block "$ ls ✅ 0s")                    ; 7
      (insert "The reply.\n")
      (expect (mapcar #'fold-test-lines (eca-chat--fold-runs))
              :to-equal '((1 . 7)))))

  (it "keeps a typed prompt out of the fold"
    (with-temp-buffer
      (fold-test-block "Thought 1s")
      (insert "\n")
      (fold-test-prompt "Background jobs are noisy, fold them")
      (insert "\n")
      (fold-test-block "$ ls ✅ 0s")
      (expect (mapcar #'fold-test-lines (eca-chat--fold-runs))
              :to-equal '((1 . 1) (5 . 5)))))

  (it "treats every user message as the reader's when the pattern is nil"
    (with-temp-buffer
      (fold-test-block "Thought 1s")
      (insert "\n")
      (fold-test-injected)
      (insert "\n")
      (fold-test-block "$ ls ✅ 0s")
      (let ((eca-chat-fold-injected-prompt-regexp nil))
        (expect (mapcar #'fold-test-lines (eca-chat--fold-runs))
                :to-equal '((1 . 1) (7 . 7)))))))

(describe "eca-chat-fold"
  (it "is reachable as a command"
    (expect (commandp 'eca-chat-fold) :to-be-truthy))

  (it "folds every stretch into one occult fold and counts them"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (expect (eca-chat-fold) :to-equal 4)
        (expect (mapcar #'fold-test-lines hidden)
                :to-equal '((3 . 3) (6 . 8) (11 . 11) (15 . 15))))))

  (it "names the block stretches as the buffer's noise for occult"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold))
      (expect (local-variable-p 'occult-noise-regions-function) :to-be-truthy)
      (expect occult-noise-regions-function :to-be #'eca-chat--fold-runs)))

  (it "folds nothing on a rerun and only what was opened after one"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (expect (eca-chat-fold) :to-equal 0)
        (expect (length hidden) :to-equal 4)
        (fold-test-open-at-line 6)
        (expect (eca-chat-fold) :to-equal 1)
        (expect (fold-test-lines (car (last hidden))) :to-equal '(6 . 8)))))

  (it "arms eca-chat-auto-fold-mode"
    (with-temp-buffer
      (fold-test-chat)
      (expect eca-chat-auto-fold-mode :to-be nil)
      (fold-test-hiding hidden
        (eca-chat-fold))
      (expect eca-chat-auto-fold-mode :to-be t)
      (expect (memq 'eca-chat-auto-fold-h eca-chat-finished-hook) :to-be-truthy)))

  (it "leaves occult's fold as occult built it"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (dolist (ov (fold-test-folds))
          (expect (overlay-get (overlay-get ov 'occult-head) 'before-string)
                  :to-equal "📎 ")))))

  (it "keeps the reader's selection"
    (with-temp-buffer
      (fold-test-chat)
      (push-mark (point-min) t t)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (expect mark-active :to-be-truthy))))

  (it "takes the status, the time and the diff button out of the summaries"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold))
      (expect (local-variable-p 'occult-summary-replace-alist) :to-be-truthy)
      (expect (fold-test-summary "Reading spec.md ✅ 0s")
              :to-equal "Reading spec.md")
      (expect (fold-test-summary "Editing eca-chat.el +8 -5 ✅ 0s view diff")
              :to-equal "Editing eca-chat.el +8 -5")
      (expect (fold-test-summary "Called tool: grep ✅ 2m 30s")
              :to-equal "Called tool: grep")))

  (it "leaves a failed call marked as failed"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold))
      (expect (fold-test-summary "$ ls ❌ 1s") :to-equal "$ ls ❌ 1s")))

  (it "drops the block marker eca draws as a line prefix"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold))
      (expect (local-variable-p 'occult-summary-line-prefix) :to-be-truthy)
      (expect occult-summary-line-prefix :to-equal "")))

  (it "follows eca's own success symbol"
    (with-temp-buffer
      (fold-test-chat)
      (let ((eca-chat-mcp-tool-call-success-symbol "OK"))
        (fold-test-hiding hidden
          (eca-chat-fold))
        (expect (fold-test-summary "Reading spec.md OK 0s")
                :to-equal "Reading spec.md")))))

(describe "eca-chat-reveal"
  (it "is reachable as a command"
    (expect (commandp 'eca-chat-reveal) :to-be-truthy))

  (it "opens every fold and disarms the mode"
    (with-temp-buffer
      (fold-test-chat)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (expect (length (fold-test-folds)) :to-equal 4)
        (eca-chat-reveal)
        (expect (fold-test-folds) :to-be nil)
        (expect eca-chat-auto-fold-mode :to-be nil)
        (expect (memq 'eca-chat-auto-fold-h eca-chat-finished-hook) :to-be nil)
        ;; the next turn ends without folding
        (run-hooks 'eca-chat-finished-hook)
        (expect (length hidden) :to-equal 4)))))

;; The mode folds a turn once eca reports it finished, and only that turn:
;; a fold the reader opened in an earlier turn is theirs to keep open.  A
;; chat starts with it off; `eca-chat-fold' arms it.
(describe "eca-chat-auto-fold-mode"
  (it "folds on the buffer's own finished hook and leaves the global one alone"
    (with-temp-buffer
      (eca-chat-auto-fold-mode 1)
      (expect (local-variable-p 'eca-chat-finished-hook) :to-be-truthy)
      (expect (memq 'eca-chat-auto-fold-h eca-chat-finished-hook) :to-be-truthy)
      (expect (memq 'eca-chat-auto-fold-h (default-value 'eca-chat-finished-hook))
              :to-be nil)))

  (it "folds the newest answer when the turn finishes"
    (with-temp-buffer
      (fold-test-chat)
      (eca-chat-auto-fold-mode 1)
      (fold-test-hiding hidden
        (run-hooks 'eca-chat-finished-hook)
        (expect (mapcar #'fold-test-lines hidden) :to-equal '((15 . 15))))))

  (it "leaves a fold the reader opened in an earlier turn open"
    (with-temp-buffer
      (fold-test-chat)
      (eca-chat-auto-fold-mode 1)
      (fold-test-hiding hidden
        (eca-chat-fold)
        (fold-test-open-at-line 6)
        (run-hooks 'eca-chat-finished-hook)
        (expect (length hidden) :to-equal 4)
        (expect (seq-find (lambda (ov) (= (line-number-at-pos (overlay-start ov)) 6))
                          (fold-test-folds))
                :to-be nil))))

  (it "folds nothing while the turn streams"
    (with-temp-buffer
      (fold-test-chat)
      (eca-chat-auto-fold-mode 1)
      (fold-test-hiding hidden
        (fold-test-block "Reading config.el ✅ 0s")
        (expect hidden :to-be nil)
        (run-hooks 'eca-chat-finished-hook)
        (expect (mapcar #'fold-test-lines hidden) :to-equal '((15 . 15) (18 . 18))))))

  (it "folds the whole chat when eca has not marked an answer yet"
    (with-temp-buffer
      (fold-test-chat)
      (setq-local eca-chat--last-user-message-pos nil)
      (eca-chat-auto-fold-mode 1)
      (fold-test-hiding hidden
        (run-hooks 'eca-chat-finished-hook)
        (expect (length hidden) :to-equal 4))))

  (it "stops folding once turned off"
    (with-temp-buffer
      (fold-test-chat)
      (eca-chat-auto-fold-mode 1)
      (eca-chat-auto-fold-mode -1)
      (expect (memq 'eca-chat-auto-fold-h eca-chat-finished-hook) :to-be nil)
      (fold-test-hiding hidden
        (run-hooks 'eca-chat-finished-hook)
        (expect hidden :to-be nil)))))

(describe "the re-protect needs no repair"
  ;; eca's re-protect is a `put-text-property' over the history, and occult
  ;; keeps a fold through a change that leaves the characters alone (its own
  ;; suite covers that).  Nothing here may snapshot and rebuild folds again.
  (it "leaves eca-chat--protect-non-prompt unadvised"
    (expect (fboundp 'eca-chat-refold-after-protect-a) :to-be nil)
    (expect (advice-member-p 'eca-chat-refold-after-protect-a
                             'eca-chat--protect-non-prompt)
            :to-be nil)))
