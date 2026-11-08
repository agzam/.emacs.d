;;; tests/ai/config-tests.el --- ai/config.el local model specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; gptel is absent from the batch tier, so `modules/ai/config.el' can't be
;; loaded; its forms are read and walked as data instead.
(defvar ai-tests--forms
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/ai/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (push form forms))
      (nreverse forms)))
  "Every top-level form of the ai module config.")

(defun ai-tests--calls-in (head form)
  "Collect every subform of FORM whose car is HEAD."
  (when (proper-list-p form)
    (append (when (eq (car form) head) (list form))
            (mapcan (lambda (sub) (ai-tests--calls-in head sub)) form))))

(defun ai-tests--calls (head)
  "Collect every form of the ai config whose car is HEAD."
  (mapcan (lambda (form) (ai-tests--calls-in head form)) ai-tests--forms))

(defun ai-tests--unquote (x)
  "Return X without a wrapping `quote'."
  (if (and (consp x) (eq (car x) 'quote)) (cadr x) x))

(defun ai-tests--ollama-backend ()
  "The `gptel-make-ollama' call in the ai config."
  (car (ai-tests--calls 'gptel-make-ollama)))

(defun ai-tests--ollama-models ()
  "Model specs declared on the Ollama backend, as (NAME . PLIST) entries."
  (mapcar (lambda (spec) (cons (car spec) (cdr spec)))
          (ai-tests--unquote
           (plist-get (cddr (ai-tests--ollama-backend)) :models))))

(defun ai-tests--local-presets ()
  "Presets pointing at the Ollama backend, as (NAME . PLIST) entries."
  (let (presets)
    (dolist (call (ai-tests--calls 'gptel-make-preset) (nreverse presets))
      (let ((plist (cddr call)))
        (when (equal (plist-get plist :backend) "Ollama")
          (push (cons (ai-tests--unquote (cadr call)) plist) presets))))))

(describe "Ollama backend in ai/config.el"
  (it "is registered"
    (expect (ai-tests--ollama-backend) :not :to-be nil))

  (it "streams, so a local model does not look hung while it generates"
    (expect (plist-get (cddr (ai-tests--ollama-backend)) :stream) :to-be t))

  (it "declares at least one model"
    (expect (length (ai-tests--ollama-models)) :to-be-greater-than 0))

  (it "gives every model a context window"
    ;; gptel tracks context against this number; absent, it cannot warn.
    (dolist (model (ai-tests--ollama-models))
      (expect (plist-get (cdr model) :context-window) :to-be-truthy)))

  (it "marks every model tool-use capable"
    ;; Both pulled models report `tools' to Ollama; a model declared without
    ;; it silently drops out of every tool-carrying gptel request.
    (dolist (model (ai-tests--ollama-models))
      (expect (memq 'tool-use (plist-get (cdr model) :capabilities))
              :to-be-truthy))))

(describe "Copilot backend in ai/config.el"
  (it "loads Copilot's model catalog as it is created"
    ;; gptel's bundled Copilot list trails what the account can use
    (expect (seq-some (lambda (call)
                        (eq (car-safe (cadr call)) 'gptel-make-gh-copilot))
                      (ai-tests--calls 'gptel-copilot-update-models))
            :to-be-truthy)))

(describe "gptel tools in ai/config.el"
  (it "run without confirmation"
    (let ((setting (seq-some (lambda (call)
                               (plist-member (cdr call) 'gptel-confirm-tool-calls))
                             (ai-tests--calls 'setopt))))
      (expect setting :to-be-truthy)
      (expect (cadr setting) :to-be nil)))

  ;; gptel-menu outside a chat shows the global value, and selecting before
  ;; registration would miss every MCP tool
  (it "are all selected globally once the MCP tools register"
    (let* ((mcp (seq-find (lambda (form)
                            (and (eq (car-safe form) 'use-package)
                                 (eq (cadr form) 'mcp)))
                          ai-tests--forms))
           (config (cdr (memq :config mcp))))
      (expect (member '(setq-default gptel-tools (gptel-all-tools))
                      (member '(register-mcp-tools-lazy) config))
              :to-be-truthy)))

  (it "stay out of rewrites"
    (expect (member '(advice-add #'gptel--suffix-rewrite
                                 :around #'gptel-rewrite-without-tools-a)
                    (ai-tests--calls 'advice-add))
            :to-be-truthy)))

(describe "ticket summary keys in ai/config.el"
  ;; evil's normal state owns m, o, q and the g prefix; the summary takes
  ;; them back in its own map, gr rather than g so that gg still works
  (it "binds m, gr, o and q in normal state"
    (let ((form (seq-find (lambda (f)
                            (and (eq (car-safe f) 'map!)
                                 (memq 'ticket-tldr-mode-map f)))
                          ai-tests--forms)))
      (expect (car (map-form-groups form 'ticket-tldr-mode-map))
              :to-equal '(:n "m"  #'ticket-tldr-pick-model
                          :n "gr" #'ticket-tldr-regenerate
                          :n "o"  #'ticket-tldr-open-spec
                          :n "q"  #'quit-window)))))

(describe "eca chat localleader in ai/config.el"
  (let ((keys (lambda ()
                (map-form-key-pairs
                 (seq-some (lambda (body)
                             (seq-some (lambda (sub)
                                         (and (eq (car-safe sub) :localleader) (cdr sub)))
                                       body))
                           (map-form-groups ai-tests--forms 'eca-chat-mode-map))))))
    ;; t for terminal: the key outlives whichever terminal the runs use
    (it "binds t to the picker of the terminals the chat started"
      (expect (cdr (assoc "t" (funcall keys))) :to-equal '#'eca-chat-ghostel-buffers))

    (it "binds T to trust"
      (expect (cdr (assoc "T" (funcall keys))) :to-equal '#'eca-chat-toggle-trust))))

(describe "gptel-send in ai/config.el"
  (it "names the model in its Querying message"
    (expect (member '(advice-add #'gptel-send :around #'gptel-send-show-model-a)
                    (ai-tests--calls 'advice-add))
            :to-be-truthy)))

(describe "local gptel presets"
  (it "exist"
    (expect (ai-tests--local-presets) :not :to-be nil))

  (it "name a model the Ollama backend declares"
    ;; The shipped config once listed models nobody had pulled.  Swapping a
    ;; model in the backend list without updating its preset reintroduces the
    ;; same dead reference, so the two lists are checked against each other.
    (let ((declared (mapcar #'car (ai-tests--ollama-models))))
      (dolist (preset (ai-tests--local-presets))
        (expect (memq (ai-tests--unquote (plist-get (cdr preset) :model))
                      declared)
                :to-be-truthy)))))
