;;; tests/ai/model-picker-tests.el --- ai/autoload/model-picker.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "modules/ai/autoload/model-picker.el")

;; consult owns these and is not installed here.  The module's bodiless
;; `defvar' marks them special within its own file only; binding them from
;; the specs, so the module's closures see the binding, takes these.
(defvar consult--narrow nil)
(defvar consult-narrow-key nil)
(defvar consult-narrow-map (make-sparse-keymap))

(describe "model-picker-provider"
  (it "reads eca's provider before the slash"
    (expect (model-picker-provider "anthropic/claude-opus-5") :to-equal "anthropic"))

  (it "reads gptel's backend before the colon"
    (expect (model-picker-provider "Claude-OAuth:claude-opus-5") :to-equal "Claude-OAuth"))

  ;; ollama tags carry a second colon; only the first separator counts
  (it "stops at the first separator"
    (expect (model-picker-provider "Ollama:llama3:latest") :to-equal "Ollama")
    (expect (model-picker-provider "ollama/qwen3-embedding:0.6b") :to-equal "ollama"))

  ;; gptel's table is an alist of (NAME BACKEND MODEL); completion hands
  ;; the predicate the entry, not its key
  (it "reads an alist entry by its key"
    (expect (model-picker-provider '("Copilot:gpt-4o" backend model)) :to-equal "Copilot"))

  (it "is nil for a bare model name"
    (expect (model-picker-provider "solar") :to-be nil)))

(describe "model-picker-narrowing-keys"
  (it "keys eca's providers by their initials"
    (expect (model-picker-narrowing-keys '("anthropic" "github-copilot" "ollama"))
            :to-equal '((?a . "anthropic") (?g . "github-copilot") (?o . "ollama"))))

  (it "gives gptel's backends the pinned keys, the same letters"
    (expect (model-picker-narrowing-keys '("Claude-OAuth" "Copilot" "Ollama"))
            :to-equal '((?a . "Claude-OAuth") (?g . "Copilot") (?o . "Ollama"))))

  ;; the pinned key is off the table even for a provider listed earlier
  (it "reserves a pinned key ahead of the others"
    (expect (model-picker-narrowing-keys '("Gemini" "Copilot"))
            :to-equal '((?e . "Gemini") (?g . "Copilot"))))

  (it "leaves an absent pin's key free"
    (expect (model-picker-narrowing-keys '("Groq")) :to-equal '((?g . "Groq"))))

  (it "moves on to the next free letter of a provider whose initial is taken"
    (expect (model-picker-narrowing-keys '("Ollama" "OpenAI"))
            :to-equal '((?o . "Ollama") (?p . "OpenAI"))))

  (it "drops a provider with no letter left"
    (expect (model-picker-narrowing-keys '("a" "a" "ab"))
            :to-equal '((?a . "a") (?b . "ab")))))

(describe "model-picker-group"
  :var (title)
  (before-each
    (setq title (lambda (candidate &optional narrow-key)
                  ;; consult parses the key itself; the built-in parser is
                  ;; equivalent for a plain key description
                  (cl-letf (((symbol-function 'consult--key-parse) #'key-parse))
                    (let ((consult-narrow-key narrow-key))
                      (funcall (model-picker-group '((?a . "Claude-OAuth") (?o . "Ollama")))
                               candidate nil))))))

  (it "titles a candidate by its provider and the key that narrows to it"
    (expect (funcall title "Claude-OAuth:claude-opus-5" "<")
            :to-equal "Claude-OAuth  (< a)"))

  (it "leaves the key off a provider without one"
    (expect (funcall title "Copilot:gpt-4o" "<") :to-equal "Copilot"))

  (it "leaves the key off while narrowing is disabled"
    (expect (funcall title "Ollama:solar" nil) :to-equal "Ollama"))

  (it "puts a bare model name in no group"
    (expect (funcall title "solar" "<") :to-be nil))

  ;; vertico asks for the candidate itself with TRANSFORM set
  (it "hands the candidate back untouched on transform"
    (expect (funcall (model-picker-group nil) "Ollama:solar" t) :to-equal "Ollama:solar")))

(describe "model-picker-narrow-predicate"
  :var (pred)
  (before-each
    (setq pred (model-picker-narrow-predicate '((?a . "anthropic") (?g . "github-copilot")))))

  (it "keeps the candidates of the key in force"
    (let ((consult--narrow ?a))
      (expect (funcall pred "anthropic/claude-opus-5") :to-be-truthy)
      (expect (funcall pred "github-copilot/claude-opus-5") :to-be nil)))

  (it "keeps an alist entry by its key"
    (let ((consult--narrow ?g))
      (expect (funcall pred '("github-copilot/gpt-5.4" b m)) :to-be-truthy)))

  (it "never keeps a bare model name"
    (let ((consult--narrow ?a))
      (expect (funcall pred "solar") :to-be nil))))

;; consult is not installed here: its setup is stubbed to record what it was
;; handed, and the require is faked away.
(defmacro model-picker-tests--with-consult-stub (&rest body)
  "Run BODY with `consult--narrow-setup' recording its arguments.
The narrowing config lands in `narrow', the keymap in `map'; both are
lexical in the spec that expands this."
  (declare (indent 0))
  `(with-fake-feature 'consult
     (cl-letf (((symbol-function 'consult--narrow-setup)
                (lambda (config keymap) (setq narrow config map keymap))))
       ,@body)))

(describe "model-picker-setup-h"
  :var (narrow map)
  (before-each
    (setq narrow nil map nil))

  (it "groups a picker's minibuffer by provider, local to it"
    (with-temp-buffer
      (let ((this-command 'eca-chat-select-model)
            (minibuffer-completion-table '("anthropic/x" "github-copilot/y"))
            (minibuffer-completion-predicate nil))
        (model-picker-tests--with-consult-stub (model-picker-setup-h)))
      (expect (local-variable-p 'completion-extra-properties) :to-be t)
      (expect (funcall (plist-get completion-extra-properties :group-function)
                       "github-copilot/y" nil)
              :to-equal "github-copilot"))
    (expect (plist-get (default-value 'completion-extra-properties) :group-function)
            :to-be nil))

  (it "derives the narrowing keys from the table and installs them"
    (with-temp-buffer
      (let ((this-command 'eca-chat-select-model)
            (minibuffer-completion-table '("anthropic/x" "github-copilot/y" "ollama/z"))
            (minibuffer-completion-predicate nil))
        (model-picker-tests--with-consult-stub (model-picker-setup-h)))
      (expect (plist-get narrow :keys)
              :to-equal '((?a . "anthropic") (?g . "github-copilot") (?o . "ollama")))
      (expect (functionp (plist-get narrow :predicate)) :to-be t)
      (expect (keymapp map) :to-be t)
      ;; the keys sit on top of the map that was there, which stays reachable
      (expect (memq map (current-local-map)) :to-be-truthy)))

  ;; gptel's table is an alist and gptel binds its annotation function
  ;; before reading; both must come through
  (it "reads an alist table and keeps what the command bound itself"
    (with-temp-buffer
      (let ((this-command 'gptel--infix-provider)
            (minibuffer-completion-table '(("Copilot:gpt-4o" b m) ("Ollama:solar" b m)))
            (minibuffer-completion-predicate nil)
            (completion-extra-properties '(:annotation-function ignore)))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect (plist-get narrow :keys) :to-equal '((?g . "Copilot") (?o . "Ollama")))
        (expect (plist-get completion-extra-properties :annotation-function) :to-be #'ignore)
        (expect (functionp (plist-get completion-extra-properties :group-function)) :to-be t))))

  (it "drops the default, so RET after narrowing takes the first candidate in view"
    (with-temp-buffer
      (let ((this-command 'eca-chat-select-model)
            (minibuffer-completion-table '("anthropic/x"))
            (minibuffer-completion-predicate nil)
            (minibuffer-default "anthropic/x"))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect minibuffer-default :to-be nil))))

  (it "leaves any other command's minibuffer alone"
    (with-temp-buffer
      (let ((this-command 'find-file)
            (minibuffer-default "keep"))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect minibuffer-default :to-equal "keep"))
      (expect narrow :to-be nil)
      (expect (local-variable-p 'completion-extra-properties) :to-be nil))))
