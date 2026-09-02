;;; tests/e2e/model-picker-groups.el --- provider headers and narrowing in the model pickers -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The batch suite calls the setup hook by hand with consult stubbed away
;; and never opens a minibuffer.  What only shows up here: `this-command'
;; naming the picker when its minibuffer opens (from gptel's menu, through
;; transient), the minibuffer-local `completion-extra-properties' reaching
;; vertico's grouping, `consult-narrow-key' reaching consult's narrowing
;; on top of vertico's map, RET after narrowing landing on a candidate of
;; that provider, and none of it surviving into the next read.

(require 'cl-lib)

(defun model-picker-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "model picker: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun model-picker-e2e--snapshot ()
  "What the live minibuffer shows: its headers, the providers in view, the selection."
  (list :groups (sort (mapcar #'substring-no-properties vertico--groups) #'string<)
        :providers (sort (seq-uniq (mapcar #'model-picker-provider vertico--candidates))
                         #'string<)
        :selected (if (< vertico--index 0) 'prompt 'candidate)))

(defun model-picker-e2e--observe (done-p thunk)
  "The snapshot of THUNK's read once DONE-P holds inside its minibuffer.
Checked from `post-command-hook'; the read is aborted right after the
capture.  A read that never gets there is aborted by a timer rather than
left to eat the keys of the scenarios after this one."
  (let* ((seen 'never-captured)
         (capture (lambda ()
                    (when (and (minibufferp) (funcall done-p))
                      (setq seen (model-picker-e2e--snapshot))
                      (abort-minibuffers))))
         (guard (run-with-timer 10 nil
                                (lambda ()
                                  (when (active-minibuffer-window)
                                    (abort-recursive-edit))))))
    (add-hook 'post-command-hook capture)
    (unwind-protect
        (condition-case err
            (progn (funcall thunk) seen)
          (quit seen)
          (error (list 'signalled err)))
      (remove-hook 'post-command-hook capture)
      (cancel-timer guard))))

(defun model-picker-e2e--typed (input)
  "A DONE-P for `model-picker-e2e--observe': the minibuffer reads INPUT."
  (lambda () (equal (minibuffer-contents-no-properties) input)))

(defun model-picker-e2e--narrowed (key)
  "A DONE-P for `model-picker-e2e--observe': the read is narrowed by KEY."
  (lambda () (eq consult--narrow key)))

(defun model-picker-e2e--plain-read (command keys done-p)
  "Snapshot of a picker-shaped read COMMAND opens, KEYS in, once DONE-P holds."
  (model-picker-e2e--observe
   done-p
   (lambda ()
     (let ((this-command command)
           (unread-command-events (listify-key-sequence (kbd keys))))
       (completing-read "Select a model: "
                        '("anthropic/claude-opus-5" "anthropic/claude-opus-4-6"
                          "github-copilot/claude-opus-5" "github-copilot/gpt-5.4"
                          "ollama/solar")
                        nil t nil nil "anthropic/claude-opus-5")))))

(defmacro model-picker-e2e--in-gptel-menu (&rest body)
  "Run BODY with gptel loaded and a buffer to open its menu in.
Whatever BODY leaves of the menu is torn down, so nothing leaks into the
scenarios after this one, and the model it may have set is put back."
  (declare (indent 0))
  `(progn
     (require 'gptel)
     (require 'gptel-transient)
     (let ((old-backend gptel-backend)
           (old-model gptel-model))
       (unwind-protect
           (with-temp-buffer ,@body)
         (when (bound-and-true-p transient--prefix)
           (ignore-errors
             (if (fboundp 'transient--emergency-exit)
                 (transient--emergency-exit)
               (transient-quit-all))))
         (setq gptel-backend old-backend
               gptel-model old-model)))))

(defun model-picker-e2e--gptel-menu (keys done-p)
  "Snapshot of the picker behind gptel-menu's m, KEYS typed, once DONE-P holds."
  (model-picker-e2e--in-gptel-menu
    (model-picker-e2e--observe
     done-p
     (lambda ()
       (gptel-menu)
       (execute-kbd-macro (kbd keys))))))

(defun model-picker-e2e--gptel-pick (keys)
  "The backend and model gptel holds after KEYS go into its menu, or the signal."
  (model-picker-e2e--in-gptel-menu
    (let ((guard (run-with-timer 10 nil
                                 (lambda ()
                                   (when (active-minibuffer-window)
                                     (abort-recursive-edit))))))
      (unwind-protect
          (condition-case err
              (progn
                (gptel-menu)
                (execute-kbd-macro (kbd keys))
                (list (gptel-backend-name gptel-backend)
                      (and (memq gptel-model (gptel-backend-models gptel-backend)) t)))
            ((error quit) (list 'signalled err)))
        (cancel-timer guard)))))

(defun model-picker-e2e ()
  "Check the provider headers and narrowing in the model pickers."
  (require 'eca-chat)
  (require 'eca-chat-inline)
  ;; the hook is autoloaded and nothing here has opened a minibuffer yet
  (let ((fn (symbol-function 'model-picker-setup-h)))
    (when (autoloadp fn)
      (autoload-do-load fn 'model-picker-setup-h)))
  (list
   (model-picker-e2e--result "the setup hook is on minibuffer-setup-hook"
                             (and (memq #'model-picker-setup-h minibuffer-setup-hook) t) t)
   (model-picker-e2e--result "every picker command is a real command"
                             (seq-remove #'commandp model-picker-commands) nil)
   (model-picker-e2e--result "the narrowing key is <" consult-narrow-key "<")
   (model-picker-e2e--result
    "eca's picker: opu shows the opus models under one header per provider, key spelled out"
    (model-picker-e2e--plain-read 'eca-chat-select-model "opu" (model-picker-e2e--typed "opu"))
    '(:groups ("anthropic  (< a)" "github-copilot  (< g)")
      :providers ("anthropic" "github-copilot") :selected candidate))
   (model-picker-e2e--result
    "eca's picker: < g leaves github-copilot alone, first candidate selected"
    (model-picker-e2e--plain-read 'eca-chat-select-model "< g" (model-picker-e2e--narrowed ?g))
    '(:groups ("github-copilot  (< g)") :providers ("github-copilot") :selected candidate))
   (model-picker-e2e--result
    "the inline picker narrows the same way"
    (model-picker-e2e--plain-read 'eca-chat-inline-select-model "< o"
                                  (model-picker-e2e--narrowed ?o))
    '(:groups ("ollama  (< o)") :providers ("ollama") :selected candidate))
   (model-picker-e2e--result
    "gptel-menu: m opu shows the opus models under one header per backend"
    (model-picker-e2e--gptel-menu "m o p u" (model-picker-e2e--typed "opu"))
    '(:groups ("Claude-OAuth  (< a)" "Copilot  (< g)")
      :providers ("Claude-OAuth" "Copilot") :selected candidate))
   (model-picker-e2e--result
    "gptel-menu: m < g leaves Copilot alone, first candidate selected"
    (model-picker-e2e--gptel-menu "m < g" (model-picker-e2e--narrowed ?g))
    '(:groups ("Copilot  (< g)") :providers ("Copilot") :selected candidate))
   (model-picker-e2e--result
    "gptel-menu: m < g RET sets a Copilot model"
    (model-picker-e2e--gptel-pick "m < g RET")
    '("Copilot" t))
   ;; runs after the pickers on purpose: the minibuffer is reused, and the
   ;; local group function and narrowing must not outlive the read that set them
   (model-picker-e2e--result
    "a read another command opens has neither headers nor narrowing"
    (model-picker-e2e--plain-read 'execute-extended-command "opu" (model-picker-e2e--typed "opu"))
    '(:groups nil :providers ("anthropic" "github-copilot") :selected candidate))
   (model-picker-e2e--result
    "there < just types"
    (model-picker-e2e--plain-read 'execute-extended-command "< g" (model-picker-e2e--typed "<g"))
    '(:groups nil :providers nil :selected prompt))))

(add-to-list 'e2e-scenarios #'model-picker-e2e)
