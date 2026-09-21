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
