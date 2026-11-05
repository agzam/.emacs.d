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

(describe "model-picker-version"
  (it "splits a dotted version out of the family"
    (expect (model-picker-version "github-copilot/gpt-6.1-sol")
            :to-equal '("github-copilot/gpt-#-sol" 6 1)))

  ;; Anthropic separates the minor version with a dash
  (it "reads a dashed version"
    (expect (model-picker-version "Claude-OAuth:claude-opus-5-5")
            :to-equal '("Claude-OAuth:claude-opus-#" 5 5)))

  (it "reads a major version alone"
    (expect (model-picker-version "anthropic/claude-opus-5")
            :to-equal '("anthropic/claude-opus-#" 5)))

  ;; as a version part, 20250514 would put opus 4 above opus 4.1
  (it "drops a trailing date stamp"
    (expect (model-picker-version "anthropic/claude-opus-4-20250514")
            :to-equal '("anthropic/claude-opus-#" 4))
    (expect (model-picker-version "anthropic/claude-opus-4-1-20250805")
            :to-equal '("anthropic/claude-opus-#" 4 1)))

  (it "leaves an ollama size tag in the family"
    (expect (model-picker-version "Ollama:qwen3.5:4b") :to-equal '("Ollama:qwen#:4b" 3 5))
    (expect (model-picker-version "ollama/qwen3-embedding:0.6b")
            :to-equal '("ollama/qwen#-embedding:0.6b" 3)))

  ;; the dash would join the 8 of the 8b size to version 3.1
  (it "takes no number a letter follows"
    (expect (model-picker-version "Groq:llama-3.1-8b-instant")
            :to-equal '("Groq:llama-#-8b-instant" 3 1))
    (expect (model-picker-version "Copilot:gpt-4o") :to-be nil))

  (it "reads an alist entry by its key"
    (expect (model-picker-version '("Copilot:grok-4.7" backend model))
            :to-equal '("Copilot:grok-#" 4 7)))

  (it "is nil for a model without a version"
    (expect (model-picker-version "ollama/nomic-embed-text:latest") :to-be nil)))

(defun model-picker-tests--listed (provider &rest models)
  "MODELS as eca lists them under PROVIDER."
  (mapcar (lambda (model) (concat provider "/" model)) models))

(describe "model-picker-latest"
  (it "keeps the newest version of each model"
    (expect (model-picker-latest
             (model-picker-tests--listed "github-copilot"
                                         "gpt-6-sol" "gpt-6-luna" "gpt-6.1-sol" "gpt-5.6-luna"))
            :to-equal (model-picker-tests--listed "github-copilot" "gpt-6-luna" "gpt-6.1-sol")))

  (it "compares versions within one provider"
    (expect (model-picker-latest '("anthropic/claude-opus-5" "github-copilot/claude-opus-5.5"))
            :to-equal '("anthropic/claude-opus-5" "github-copilot/claude-opus-5.5")))

  (it "tells sizes apart"
    (expect (model-picker-latest
             (model-picker-tests--listed "ollama" "qwen3.5:4b" "qwen3.6:27b" "qwen3.8:27b"))
            :to-equal (model-picker-tests--listed "ollama" "qwen3.5:4b" "qwen3.8:27b")))

  (it "keeps the models without a version"
    (expect (model-picker-latest
             (model-picker-tests--listed "ollama" "nomic-embed-text:latest" "embeddinggemma:latest"))
            :to-equal
            (model-picker-tests--listed "ollama" "nomic-embed-text:latest" "embeddinggemma:latest")))

  (it "keeps gptel's alist entries whole"
    (expect (model-picker-latest '(("Copilot:grok-4.6" b m) ("Copilot:grok-4.7" b m)))
            :to-equal '(("Copilot:grok-4.7" b m))))

  (it "keeps one model per family of the Copilot catalog"
    (expect (model-picker-latest
             (model-picker-tests--listed
              "github-copilot"
              "claude-fable-5.1" "claude-haiku-4.5" "claude-haiku-5.5" "claude-opus-4.8"
              "claude-opus-5" "claude-opus-5.5" "claude-sonnet-5" "claude-sonnet-5.5"
              "gemini-3.8-flash" "gemini-4-argon" "gpt-5.3-codex" "gpt-5.6-luna" "gpt-5.6-sol"
              "gpt-5.6-terra" "gpt-6-astra" "gpt-6-luna" "gpt-6-sol" "gpt-6.1-sol"
              "gpt-6.1-terra" "grok-4.6" "grok-4.7" "mai-code-1.1-flash"))
            :to-equal
            (model-picker-tests--listed
             "github-copilot"
             "claude-fable-5.1" "claude-haiku-5.5" "claude-opus-5.5" "claude-sonnet-5.5"
             "gemini-3.8-flash" "gemini-4-argon" "gpt-5.3-codex" "gpt-6-astra" "gpt-6-luna"
             "gpt-6.1-sol" "gpt-6.1-terra" "grok-4.7" "mai-code-1.1-flash")))

  (it "keeps one model per family of the Anthropic catalog, dated ids among them"
    (expect (model-picker-latest
             (model-picker-tests--listed
              "anthropic"
              "claude-fable-5" "claude-fable-5-1" "claude-haiku-4-5-20251001" "claude-haiku-5-5"
              "claude-mythos-5" "claude-mythos-5-1" "claude-opus-4-5-20251101" "claude-opus-4-6"
              "claude-opus-4-7" "claude-opus-4-8" "claude-opus-5" "claude-opus-5-5"
              "claude-sonnet-4-5-20250929" "claude-sonnet-4-6" "claude-sonnet-5"
              "claude-sonnet-5-5"))
            :to-equal
            (model-picker-tests--listed
             "anthropic"
             "claude-fable-5-1" "claude-haiku-5-5" "claude-mythos-5-1" "claude-opus-5-5"
             "claude-sonnet-5-5"))))

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

  ;; the narrowing replaces the predicate, so the hiding has to sit in the table
  (it "hides from the table the models a newer version supersedes"
    (with-temp-buffer
      (let ((this-command 'eca-chat-select-model)
            (minibuffer-completion-table '("anthropic/claude-opus-4-8" "anthropic/claude-opus-5"
                                           "github-copilot/claude-opus-4.8"))
            (minibuffer-completion-predicate nil))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect minibuffer-completion-table
                :to-equal '("anthropic/claude-opus-5" "github-copilot/claude-opus-4.8")))))

  ;; gptel looks the pick up in its own alist, by the whole entry's key
  (it "hides superseded entries of gptel's alist table"
    (with-temp-buffer
      (let ((this-command 'gptel--infix-provider)
            (minibuffer-completion-table '(("Copilot:grok-4.6" b m) ("Copilot:grok-4.7" b m)))
            (minibuffer-completion-predicate nil))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect minibuffer-completion-table :to-equal '(("Copilot:grok-4.7" b m))))))

  (it "drops the default, so RET after narrowing takes the first candidate in view"
    (with-temp-buffer
      (let ((this-command 'eca-chat-select-model)
            (minibuffer-completion-table '("anthropic/x"))
            (minibuffer-completion-predicate nil)
            (minibuffer-default "anthropic/x"))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect minibuffer-default :to-be nil))))

  (it "groups the model picker of a ticket summary by backend"
    (with-temp-buffer
      (let ((this-command 'ticket-tldr-pick-model)
            (minibuffer-completion-table '(("Claude-OAuth:x" b m) ("Copilot:y" b m)))
            (minibuffer-completion-predicate nil))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect (plist-get narrow :keys)
                :to-equal '((?a . "Claude-OAuth") (?g . "Copilot"))))))

  (it "leaves any other command's minibuffer alone"
    (with-temp-buffer
      (let ((this-command 'find-file)
            (minibuffer-completion-table '("claude-opus-4-8" "claude-opus-5"))
            (minibuffer-default "keep"))
        (model-picker-tests--with-consult-stub (model-picker-setup-h))
        (expect minibuffer-default :to-equal "keep")
        (expect minibuffer-completion-table :to-equal '("claude-opus-4-8" "claude-opus-5")))
      (expect narrow :to-be nil)
      (expect (local-variable-p 'completion-extra-properties) :to-be nil))))
