;;; tests/e2e/eca-workspaces-keys.el --- dashboard keys on its buttons -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; Evil rebuilds a buffer's keymaps from the maps active at point, and the
;; dashboard's buttons carry a `local-map' that hides the mode map there.  A
;; state change on a button then lost every dashboard key.  Only a dashboard
;; eca really rendered, under real evil, shows that.

(require 'cl-lib)

(defconst eca-workspaces-keys-e2e--session-id 424242
  "Id of the fake session the dashboard lists.")

(defconst eca-workspaces-keys-e2e--bindings
  '(("?" . eca-workspaces-menu)
    ("q" . eca-toggle-workspaces)
    ("n" . eca-workspaces-new-chat)
    (", ," . eca-workspaces-menu)
    (", n" . eca-workspaces-new-chat)
    (", R" . eca-workspaces-resume)
    (", g" . eca-workspaces-refresh)
    (", r" . eca-workspaces-rename-chat)
    (", f" . eca-workspaces-fork-chat)
    (", C" . eca-workspaces-compact-chat)
    (", m" . eca-workspaces-select-model)
    (", v" . eca-workspaces-select-variant)
    (", s" . eca-workspaces-stop-prompt)
    (", d" . eca-workspaces-delete)
    (", a" . eca-workspaces-tool-call-accept-next)
    (", A" . eca-workspaces-tool-call-accept-all)
    (", x" . eca-workspaces-tool-call-reject-next)
    (", w w" . eca-toggle-workspaces))
  "Keys the dashboard answers in normal state, and their commands.")

(defun eca-workspaces-keys-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "eca-workspaces keys: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun eca-workspaces-keys-e2e--resolve ()
  "What each dashboard key reaches from point."
  (mapcar (lambda (pair) (cons (car pair) (key-binding (kbd (car pair)))))
          eca-workspaces-keys-e2e--bindings))

(defun eca-workspaces-keys-e2e--menu (keys)
  "The transient KEYS open from the dashboard, quit again with q.
KEYS are pressed only while they reach the menu: anything else may wait
in the minibuffer for input that never comes."
  (when (eq (key-binding (kbd keys)) 'eca-workspaces-menu)
    (execute-kbd-macro (kbd keys))
    (when-let* ((prefix (bound-and-true-p transient--prefix)))
      (prog1 (oref prefix command)
        (execute-kbd-macro (kbd "q"))))))

(defun eca-workspaces-keys-e2e--folded-p ()
  "Whether the dashboard has the fake session folded."
  (and (memq eca-workspaces-keys-e2e--session-id eca-workspaces--folded-sessions) t))

(defun eca-workspaces-keys-e2e--on-button (buffer)
  "Put point inside BUFFER's workspace button and re-enter normal state.
Return whether point is on the button.  The state change is what used
to drop the dashboard maps."
  (select-window (get-buffer-window buffer))
  (goto-char (point-min))
  (forward-char 3)
  (evil-normal-state)
  (and (get-char-property (point) 'keymap) t))

(defun eca-workspaces-keys-e2e--cases (buffer)
  "Every case, run in the rendered dashboard BUFFER."
  (list
   (eca-workspaces-keys-e2e--result
    "the advice sits on a render function that exists"
    (and (fboundp 'eca-workspaces--render)
         (advice-member-p 'eca-workspaces-drop-button-local-map-a
                          'eca-workspaces--render)
         t)
    t)
   (eca-workspaces-keys-e2e--result
    "point sits inside the workspace button"
    (eca-workspaces-keys-e2e--on-button buffer) t)
   (eca-workspaces-keys-e2e--result
    "no button keeps a local-map"
    (text-property-not-all (point-min) (point-max) 'local-map nil) nil)
   (eca-workspaces-keys-e2e--result
    "every key reaches the dashboard after a state change on a button"
    (eca-workspaces-keys-e2e--resolve) eca-workspaces-keys-e2e--bindings)
   (eca-workspaces-keys-e2e--result
    "? opens the actions menu"
    (eca-workspaces-keys-e2e--menu "?") 'eca-workspaces--menu-prefix)
   (eca-workspaces-keys-e2e--result
    ", , opens the actions menu"
    (progn (eca-workspaces-keys-e2e--on-button buffer)
           (eca-workspaces-keys-e2e--menu ", ,"))
    'eca-workspaces--menu-prefix)
   (eca-workspaces-keys-e2e--result
    "RET on the button still folds and unfolds the workspace"
    (progn (eca-workspaces-keys-e2e--on-button buffer)
           (list (progn (execute-kbd-macro (kbd "RET"))
                        (eca-workspaces-keys-e2e--folded-p))
                 (progn (execute-kbd-macro (kbd "RET"))
                        (eca-workspaces-keys-e2e--folded-p))))
    '(t nil))
   (eca-workspaces-keys-e2e--result
    "q closes the dashboard window"
    (progn (eca-workspaces-keys-e2e--on-button buffer)
           (execute-kbd-macro (kbd "q"))
           (get-buffer-window buffer))
    nil)))

(defun eca-workspaces-keys-e2e ()
  "Check that the dashboard keys survive a state change on a button."
  (require 'eca)
  (let* ((id eca-workspaces-keys-e2e--session-id)
         (eca--sessions
          (list (cons id (make-eca--session
                          :id id
                          :workspace-folders
                          (list (expand-file-name "dashboard" e2e-work-dir))))))
         buffer)
    (unwind-protect
        (progn
          (delete-other-windows)
          (eca-workspaces)
          (setq buffer (get-buffer eca-workspaces-buffer-name))
          (with-current-buffer buffer
            (eca-workspaces-keys-e2e--cases buffer)))
      (discard-input)
      (when (bound-and-true-p transient--prefix)
        (ignore-errors (transient--emergency-exit)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-other-windows))))

(add-to-list 'e2e-scenarios #'eca-workspaces-keys-e2e)
