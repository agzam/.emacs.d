;;; tests/e2e/agent-shell-keys.el --- Return presses agent-shell buttons -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; A GUI Return arrives as <return> and becomes the RET that agent-shell's
;; buttons bind only while no map binds <return>; an evil binding of it made
;; Return send the prompt from on top of a button.

(defun agent-shell-keys-e2e ()
  "Return in normal state on an agent-shell button runs the button."
  (require 'agent-shell)
  (let ((buffer (get-buffer-create " *agent-shell-keys-e2e*"))
        (pressed nil))
    (unwind-protect
        (with-current-buffer buffer
          (switch-to-buffer buffer)
          (delete-other-windows)
          (fundamental-mode)
          (use-local-map agent-shell-mode-map)
          (insert "before "
                  (agent-shell--make-button
                   :text "Allow" :action (lambda () (interactive) (setq pressed t)))
                  " after")
          (evil-normal-state)
          (evil-normalize-keymaps)
          (goto-char (point-min))
          (search-forward "Allo")
          (ignore-errors (execute-kbd-macro (kbd "<return>"))))
      (kill-buffer buffer))
    (list (list :label "agent-shell keys: GUI Return on a button presses it"
                :ok pressed :got (format "%S" pressed) :want "t"))))

(add-to-list 'e2e-scenarios #'agent-shell-keys-e2e)
