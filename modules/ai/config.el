;;; modules/ai/config.el -*- lexical-binding: t; -*-

;; Ported from doom.d modules/custom/ai.  Dropped: whisper, gptel-quick,
;; ob-gptel, llm-tool-collection, ragmacs, shell-maker/acp/agent-shell,
;; claude-code (commented out or inert upstream; git-resurrectable).

(use-package gptel
  :defer t
  :config
  ;; Make AGENTS.md the default system prompt everywhere.  A function value
  ;; is re-read per request, so `bb setup.bb' regenerations apply without
  ;; reloading.  gptel-agent/gptel-plan presets set their own :system, so
  ;; AGENTS.md is not injected twice when those presets are active.
  (setf (alist-get 'default gptel-directives) #'eca-agents-md-content)
  (setq-default gptel--system-message #'eca-agents-md-content)

  (setf (alist-get 'chat gptel-directives)
        (concat
         "You are my Spanish conversation partner and tutor. Help me acquire Spanish through natural conversation, do not lecture.\n\n"
         "Talk to me in Spanish, calibrated to the level you infer from how I write, and nudge me slightly above my comfort zone. When you use a word likely new to me, add a short English gloss in parentheses. If I switch to English because I lack a word, give me the Spanish and continue. Always keep the conversation going by ending with a question or prompt.\n\n"
         "Correct my Spanish - grammar, word choice, and unnatural or non-idiomatic phrasing, not only outright errors. Surface the few most important issues per turn, not every nitpick, in this format:\n"
         "- mine: <what I wrote>\n"
         "- better: <corrected version>\n"
         "- why: <one short line, in English if the point is subtle>\n\n"
         "Revisit mistakes I repeat. Be concise and prioritize my practice over long explanations. Occasionally, not every message, add a brief memorable etymological or cultural note (e.g. why a word is feminine due to its Greek origin). Use neutral Latin American Spanish unless I say otherwise."))

  (setopt
   gptel-default-mode 'org-mode
   gptel-expert-commands t
   gptel-track-media t
   gptel-highlight-methods '(face))

  (after! gptel-transient
    ;; `gptel-tools' is gptel's transient prefix, not the vendored package.
    (transient-suffix-put 'gptel-menu (kbd "RET") :key "s-<return>")
    (transient-suffix-put 'gptel-tools (kbd "RET") :key "s-<return>"))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* ")

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream nil
    :models '("llama3:latest" "solar"))

  (gptel-make-gh-copilot "Copilot")

  (map! "C-c C-g" #'gptel-abort)

  (add-hook! 'gptel-mode-hook
    #'gptel-highlight-mode
    (defun gptel-mode-set-local-keys-h ()
      (map! :map gptel-mode-map
            :i "RET" nil
            :i "<return>" nil
            :i "s-<return>" #'gptel-send
            :i "s-RET" #'gptel-send
            :i ", m" #'gptel-menu
            :i ", SPC" #'insert-comma
            :n "q" #'bury-buffer
            (:localleader
             "m" #'gptel-menu
             "," #'gptel-menu
             (:prefix ("s" . "session")
              :desc "clear" "L" #'gptel-clear-buffer)))))

  (add-hook! 'kill-emacs-hook
    (defun persist-gptel-model-h ()
      ;; custom.el is the sanctioned in-config-dir exception.
      (customize-save-variable 'gptel-model gptel-model)))

  (add-hook! 'gptel-post-response-functions #'gptel-persist-history)

  (add-to-list
   'display-buffer-alist
   `(,(rx bos (or "*Claude" "*ChatGPT" "gptel-"))
     (display-buffer-in-quadrant)
     (init-width . 0.40)
     (direction . right)
     (window . root)))

  (add-to-list
   'display-buffer-alist
   `((lambda (buffer _action)
       (with-current-buffer buffer
         (and buffer-file-name
              (natnump (string-match-p
                        (concat org-default-folder "gptel/quick.org")
                        buffer-file-name)))))
     (display-buffer-reuse-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

;; extract-tool-defs-from-bb parses the MCP harness .bb files with parseedn;
;; doom.d gets it transitively from the clojure module - explicit here until
;; clojure ports.
(use-package parseedn :defer t)

(use-package eca
  :ensure (eca :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :defer t
  :config
  (setopt eca-chat-use-side-window nil
          eca-chat-custom-behavior nil
          eca-chat-parent-mode 'markdown-mode
          eca-api-response-timeout 15)

  (defcustom eca-archive-dir "~/Sync/org/eca/"
    "Directory where ECA chats are archived as Markdown."
    :type 'directory
    :group 'eca)

  ;; Save a readable copy of each chat after every finished turn.
  (add-hook 'eca-chat-finished-hook #'eca-archive-chat)

  ;; Let later capfs (cape-file, etc.) run when eca has no candidates.
  (defadvice! eca-chat-capf-non-exclusive-a (result)
    :filter-return #'eca-chat-completion-at-point
    (when result (append result '(:exclusive no))))

  ;; doom.d also unbinds these in evil-markdown-mode-map; no evil-markdown here.
  (map! :map markdown-mode-map
        "TAB" nil
        :n "<tab>" nil)
  (add-hook! 'eca-chat-mode-hook
    (defun eca-set-completions-at-point-h ()
      (cl-delete 'yasnippet-capf completion-at-point-functions))
    (defun eca-set-keybindings-h ()
      (map! :map eca-chat-mode-map
            "<return>" nil
            "RET" nil
            :i "<return>" nil
            :i "RET" nil
            :n "<return>" #'eca-chat--key-pressed-return
            :n "RET" #'eca-chat--key-pressed-return
            :i "s-<return>"  #'eca-chat--key-pressed-return
            "C-c C-y" #'eca-chat-tool-call-accept-all
            "C-c !" #'eca-chat-tool-call-accept-all-and-remember
            "C-c C-g" #'eca-chat-stop-prompt
            :i "M-RET" #'eca-chat--key-pressed-return
            :i "M-p" #'eca-chat--key-pressed-previous-prompt-history
            :i "M-n" #'eca-chat--key-pressed-next-prompt-history
            :n "M-p" #'eca-chat-go-to-prev-expandable-block
            :n "M-n" #'eca-chat-go-to-next-expandable-block
            :n "<tab>"  #'eca-chat-toggle-expandable-block
            :n "TAB" #'eca-chat-toggle-expandable-block
            :n ",," #'eca-transient-menu
            (:localleader
             "n" #'tab-line-switch-to-next-tab
             "p" #'tab-line-switch-to-prev-tab
             "b" #'eca-chat-cycle-agent
             "t" #'eca-chat-toggle-trust
             "f" #'eca-chat-flag-and-fork
             (:prefix ("w" . "workspace")
              "a" #'eca-chat-add-workspace-root
              "r" #'eca-chat-remove-workspace-root
              "w" #'eca-toggle-workspaces)))
      (map! :map eca-workspaces-mode-map
            (:localleader
             "w w" #'eca-toggle-workspaces)))
    (defun eca-chat-mode-markup-no-hiding-h ()
      (markdown-toggle-markup-hiding -1)))
  (add-hook 'eca-chat-mode-hook #'eca-compact-modeline-icons-h))

(use-package mcp
  :ensure (mcp :host github :repo "lizqwerscott/mcp.el")
  :after gptel
  :config
  (setopt mcp-hub-servers (mcp-servers-from-eca-config))
  (register-mcp-tools-lazy))

(use-package gptel-agent
  :ensure (gptel-agent :host github :repo "karthink/gptel-agent"
                       :files ("*.el" "agents"))
  :defer t
  :config
  (add-to-list 'gptel-agent-skill-dirs "~/.config/eca/skills/")

  (map! :map gptel-tool-call-actions-map
        "C-c C-y" #'gptel--accept-tool-calls
        "C-c C-r" #'gptel--reject-tool-calls)

  (defadvice! gptel-agent-inject-mcp-tools-a (&rest _)
    :after #'gptel-agent-update
    "Inject MCP tools and ECA directives into gptel-agent definitions."
    ;; Prepend AGENTS.md directives to every agent's :system prompt
    (when-let* ((directives (eca-agents-md-content)))
      (dolist (entry gptel-agent--agents)
        (when-let* ((system (plist-get (cdr entry) :system)))
          (plist-put (cdr entry) :system
                     (concat directives "\n\n" system)))))
    ;; Inject MCP server tool categories (using server name as category)
    (when (bound-and-true-p mcp-hub-servers)
      (let ((mcp-cats (mapcar #'car mcp-hub-servers)))
        (dolist (entry gptel-agent--agents)
          (when-let* ((tools (plist-get (cdr entry) :tools)))
            (dolist (cat mcp-cats)
              (when (and (assoc cat gptel--known-tools)
                         (not (member cat tools)))
                (nconc tools (list cat))))))))
    ;; Re-create presets with updated system prompts and tool lists
    (when-let* ((p (assoc-default "gptel-agent" gptel-agent--agents nil nil)))
      (apply #'gptel-make-preset 'gptel-agent p))
    (when-let* ((p (assoc-default "gptel-plan" gptel-agent--agents nil nil)))
      (apply #'gptel-make-preset 'gptel-plan p))))

;; The two vendored in-module packages have no upstream repo; elpaca builds
;; them in place from the module tree, artifacts land in the builds dir
;; (MIGRATION "Local :repo packages").  Backquoted orders are evaluated.
(elpaca `(gptel-anthropic-oauth
          :repo ,(file-name-as-directory
                  (expand-file-name "modules/ai/gptel-anthropic-oauth"
                                    user-emacs-directory)))
  (after! gptel
    (setopt gptel-backend (gptel-make-anthropic-oauth "Claude-OAuth" :stream t))))

;; Installed-not-loaded, matching doom.d (its preset block is commented out).
(elpaca `(gptel-tools
          :repo ,(file-name-as-directory
                  (expand-file-name "modules/ai/gptel-tools" user-emacs-directory))
          :files ("*.el")))
