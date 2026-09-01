;;; tests/e2e/model-picker-narrowing.el --- narrowing the model pickers by backend -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The batch suite stubs consult away and never opens a minibuffer.  What
;; only shows up here: `consult-narrow-key' reaching a picker that was
;; routed through the shim, vertico answering RET with the first candidate
;; in view rather than the prompt, and gptel's menu dispatching its picker
;; through the method - in the config as booted, driven by real keys.

(require 'cl-lib)

(defun model-picker-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "model picker narrowing: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun model-picker-e2e--read (keys thunk)
  "Call THUNK with KEYS queued for the read it performs; its value, or the
signal that ended it.  A stalled read is aborted rather than left to eat
the keys of the scenarios after this one."
  (let ((guard (run-with-timer 10 nil (lambda ()
                                        (when (active-minibuffer-window)
                                          (abort-minibuffers)))))
        (unread-command-events (listify-key-sequence (kbd keys))))
    (unwind-protect
        (condition-case err
            (funcall thunk)
          ((error quit) (list 'signalled err)))
      (cancel-timer guard)
      (setq unread-command-events nil))))

(defun model-picker-e2e--headers (keys thunk)
  "The group headers vertico shows once KEYS have gone into THUNK's read.
Captured from inside the live minibuffer, then the read is aborted."
  (let ((seen 'never-opened))
    (model-picker-e2e--read
     keys
     (lambda ()
       (run-with-timer 0.5 nil
                       (lambda ()
                         (when-let* ((win (active-minibuffer-window)))
                           (with-current-buffer (window-buffer win)
                             (setq seen (mapcar #'substring-no-properties vertico--groups)))
                           (abort-minibuffers))))
       (funcall thunk)))
    seen))

(defun model-picker-e2e--helper ()
  "The shim gives a plain completing-read the narrowing keys and list order."
  (let* ((read (lambda ()
                 (call-with-prefix-narrowing
                  "/" nil
                  (lambda ()
                    (completing-read "Model: "
                                     '("anthropic/x" "github-copilot/y" "ollama/z")
                                     nil t)))))
         (pick (lambda (keys) (model-picker-e2e--read keys read))))
    (list (model-picker-e2e--result "the narrowing key is <" consult-narrow-key "<")
          (model-picker-e2e--result "the list opens under one header per group, key spelled out"
                                    (model-picker-e2e--headers "" read)
                                    '("anthropic  (< a)" "github-copilot  (< g)" "ollama  (< o)"))
          (model-picker-e2e--result "< g leaves only that group's header"
                                    (model-picker-e2e--headers "< g" read)
                                    '("github-copilot  (< g)"))
          (model-picker-e2e--result "< g RET picks the first github-copilot candidate"
                                    (funcall pick "< g RET") "github-copilot/y")
          (model-picker-e2e--result "< o RET picks the first ollama candidate"
                                    (funcall pick "< o RET") "ollama/z")
          ;; vertico's own sort would lead with the shortest, ollama/z
          (model-picker-e2e--result "RET alone picks the first candidate in list order"
                                    (funcall pick "RET") "anthropic/x"))))

(defun model-picker-e2e--gptel ()
  "The menu's picker runs through the method and narrows by backend."
  (require 'gptel)
  (require 'gptel-transient)
  (let ((old-backend gptel-backend)
        (old-model gptel-model)
        results)
    (push (model-picker-e2e--result
           "the picker is a gptel-provider-variable"
           (eieio-object-class (get 'gptel--infix-provider 'transient--suffix))
           'gptel-provider-variable)
          results)
    (push (model-picker-e2e--result
           "the class carries the narrowing method"
           (and (cl-find-method #'transient-infix-read '(:around)
                                '(gptel-provider-variable))
                t)
           t)
          results)
    (push (model-picker-e2e--result
           "an Ollama backend is registered to narrow to"
           (and (assoc "Ollama" gptel--known-backends) t) t)
          results)
    (unwind-protect
        (with-temp-buffer
          ;; Copilot first, so the current model no longer leads the Claude
          ;; group when "c" is tried: that pick has to come from narrowing.
          (pcase-dolist (`(,key . ,backend) '(("g" . "Copilot") ("c" . "Claude-OAuth") ("o" . "Ollama")))
            (push (model-picker-e2e--result
                   (format "m < %s RET in gptel-menu lands on a %s model" key backend)
                   (condition-case err
                       (progn
                         (gptel-menu)
                         (execute-kbd-macro (kbd (format "m < %s RET" key)))
                         (list (gptel-backend-name gptel-backend)
                               (and (memq gptel-model (gptel-backend-models gptel-backend)) t)))
                     ((error quit) (list 'signalled err)))
                   (list backend t))
                  results)
            (push (model-picker-e2e--result
                   (format "q closes the menu the %s pick re-opened" backend)
                   (condition-case err
                       (progn (execute-kbd-macro (kbd "q"))
                              (bound-and-true-p transient--prefix))
                     ((error quit) (list 'signalled err)))
                   nil)
                  results)))
      ;; a failed step may strand the transient; never leak it into the
      ;; scenarios that run after this one
      (when (bound-and-true-p transient--prefix)
        (ignore-errors
          (if (fboundp 'transient--emergency-exit)
              (transient--emergency-exit)
            (transient-quit-all))))
      (setq gptel-backend old-backend
            gptel-model old-model))
    (nreverse results)))

;; The picker commands are faked in tests/ai/eca-tests.el; asserting the
;; advice sits on the real ones is what keeps that fake honest.
(defun model-picker-e2e--eca ()
  "Both eca pickers exist and carry the narrowing advice."
  (require 'eca-chat)
  (require 'eca-chat-inline)
  (mapcar (lambda (command)
            (model-picker-e2e--result
             (format "%s is advised" command)
             (list (fboundp command)
                   (and (advice-member-p 'eca-select-model-narrowing-a command) t))
             '(t t)))
          '(eca-chat-select-model eca-chat-inline-select-model)))

(defun model-picker-e2e ()
  "Check narrowing by backend in the model pickers."
  (append (model-picker-e2e--helper)
          (model-picker-e2e--gptel)
          (model-picker-e2e--eca)))

(add-to-list 'e2e-scenarios #'model-picker-e2e)
