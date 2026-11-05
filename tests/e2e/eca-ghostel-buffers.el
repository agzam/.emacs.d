;;; tests/e2e/eca-ghostel-buffers.el --- a chat's eca-ghostel buffers behind , t -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; The batch suite stubs consult.  Only here does a key reach the picker
;; from a real `eca-chat-mode' buffer, vertico keep the order, consult
;; preview into the chat's window, and vertico-buffer open beside it.

(require 'cl-lib)

(defun eca-ghostel-buffers-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "eca-ghostel buffers: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun eca-ghostel-buffers-e2e--run (buffer origin)
  "An `eca-ghostel-list' entry for BUFFER, started from the chat named ORIGIN."
  (list :buffer buffer :status "exited 0 after 1.0s" :command "make test"
        :origin origin :started "12:00:00" :log nil))

(defun eca-ghostel-buffers-e2e--step (window)
  "What the picker shows now: its rows, what WINDOW previews, the windows.
The vertico window, when there is one, is the one showing the minibuffer."
  (let ((vertico (seq-find (lambda (w) (minibufferp (window-buffer w)))
                           (window-list nil 'no))))
    (list :rows (mapcar #'substring-no-properties vertico--candidates)
          :preview (buffer-name (window-buffer window))
          :layout (mapcar #'window-edges (window-list nil 'no))
          :vertico (and vertico
                        (list :leftmost (zerop (window-left-column vertico))
                              :full-height (window-full-height-p vertico)
                              :beside (= (nth 2 (window-edges vertico))
                                         (window-left-column window)))))))

(defun eca-ghostel-buffers-e2e--press (chat keys runs)
  "Press KEYS in CHAT while `eca-ghostel-list' answers RUNS.
RUNS `undefined' leaves the function unbound, as before the elisp-eval MCP
loads eca-ghostel.el.  Return each step the picker went through, what the
chat's window shows after, how many windows are left, and the error."
  (let* ((window (get-buffer-window chat))
         steps
         (capture (lambda ()
                    (when (minibufferp)
                      (push (eca-ghostel-buffers-e2e--step window) steps))))
         ;; keys that never leave the read would wait for the terminal
         (guard (run-with-timer 10 nil (lambda ()
                                         (when (active-minibuffer-window)
                                           (abort-recursive-edit))))))
    (cl-letf (((symbol-function 'eca-ghostel-list)
               (unless (eq runs 'undefined) (lambda () runs)))
              ;; the real one shows the buffer through `pop-to-buffer' too
              ((symbol-function 'eca-ghostel-visit) #'pop-to-buffer))
      (add-hook 'post-command-hook capture)
      (unwind-protect
          (let ((err (condition-case err
                         (progn (with-current-buffer chat
                                  (execute-kbd-macro (kbd keys)))
                                nil)
                       ((error quit) (error-message-string err)))))
            (list :steps (nreverse steps)
                  :after (buffer-name (window-buffer window))
                  :windows (length (window-list nil 'no))
                  :error err))
        (remove-hook 'post-command-hook capture)
        (cancel-timer guard)
        (discard-input)
        (delete-other-windows window)
        (set-window-buffer window chat)))))

(defun eca-ghostel-buffers-e2e--seen (got key)
  "The distinct KEY values across the steps in GOT, in order."
  (seq-uniq (mapcar (lambda (step) (plist-get step key)) (plist-get got :steps))))

(defun eca-ghostel-buffers-e2e--cases (chat)
  "Every case, run in the eca chat buffer CHAT."
  (let* ((name (buffer-name chat))
         ;; vertico's own sort, by length, would put alpha first and
         ;; mid-label second, so the second row tells the orders apart
         (runs (list (eca-ghostel-buffers-e2e--run "*eca-ghostel: zeta-newest*" name)
                     (eca-ghostel-buffers-e2e--run "*eca-ghostel: theirs*" "eca-chat - other")
                     (eca-ghostel-buffers-e2e--run "*eca-ghostel: alpha*" name)
                     (eca-ghostel-buffers-e2e--run "*eca-ghostel: mid-label*" name)))
         (mine '("*eca-ghostel: zeta-newest*" "*eca-ghostel: alpha*"
                 "*eca-ghostel: mid-label*")))
    (list
     (eca-ghostel-buffers-e2e--result
      ", t in normal state and C-, t in insert state reach the picker, , T trust"
      (with-current-buffer chat
        (prog1 (list (progn (evil-normal-state) (key-binding (kbd ", t")))
                     (key-binding (kbd ", T"))
                     (progn (evil-insert-state) (key-binding (kbd "C-, t"))))
          (evil-normal-state)))
      '(eca-chat-ghostel-buffers eca-chat-toggle-trust eca-chat-ghostel-buffers))
     (let ((got (eca-ghostel-buffers-e2e--press chat ", t C-n RET" runs)))
       (eca-ghostel-buffers-e2e--result
        ", t C-n RET previews each row in the chat's window and leaves the pick there"
        (list :rows (eca-ghostel-buffers-e2e--seen got :rows)
              :previews (eca-ghostel-buffers-e2e--seen got :preview)
              :after (plist-get got :after)
              :windows (plist-get got :windows)
              :error (plist-get got :error))
        (list :rows (list mine)
              :previews '("*eca-ghostel: zeta-newest*" "*eca-ghostel: alpha*")
              :after "*eca-ghostel: alpha*"
              :windows 1
              :error nil)))
     (let* ((got (eca-ghostel-buffers-e2e--press chat ", t ; b C-n C-n RET" runs))
            (in-buffer (cons :steps (list (seq-filter (lambda (step) (plist-get step :vertico))
                                                      (plist-get got :steps))))))
       (eca-ghostel-buffers-e2e--result
        ", t ; b puts vertico at the left, full height, and previews move no window"
        (list :vertico (eca-ghostel-buffers-e2e--seen in-buffer :vertico)
              :layouts (length (eca-ghostel-buffers-e2e--seen in-buffer :layout))
              :previews (eca-ghostel-buffers-e2e--seen in-buffer :preview)
              :after (plist-get got :after)
              :windows (plist-get got :windows)
              :error (plist-get got :error))
        (list :vertico '((:leftmost t :full-height t :beside t))
              :layouts 1
              :previews mine
              :after "*eca-ghostel: mid-label*"
              :windows 1
              :error nil)))
     (eca-ghostel-buffers-e2e--result
      ", t before eca-ghostel.el has loaded says there is nothing to show"
      (plist-get (eca-ghostel-buffers-e2e--press chat ", t" 'undefined) :error)
      "No eca-ghostel buffers from this chat"))))

(defun eca-ghostel-buffers-e2e ()
  "Check that , t in an eca chat lists the eca-ghostel buffers it started."
  (require 'eca)
  (require 'eca-chat)
  (let ((chat (generate-new-buffer "eca-chat - e2e ghostel buffers"))
        ;; real buffers, so that consult has something to preview
        (runs (mapcar #'get-buffer-create
                      '("*eca-ghostel: zeta-newest*" "*eca-ghostel: alpha*"
                        "*eca-ghostel: mid-label*"))))
    (unwind-protect
        (progn
          (switch-to-buffer chat)
          (delete-other-windows)
          ;; a chat that holds text skips the welcome screen, which needs a session
          (insert "chat\n")
          (eca-chat-mode)
          ;; the mode finishes its setup, mode hook included, from a timer
          (accept-process-output nil 0.2)
          (evil-normal-state)
          (cons (eca-ghostel-buffers-e2e--result
                 "eca-chat-ghostel-buffers is an autoloaded command"
                 (commandp 'eca-chat-ghostel-buffers) t)
                (eca-ghostel-buffers-e2e--cases chat)))
      (let ((kill-buffer-query-functions nil))
        (mapc #'kill-buffer (cons chat runs))))))

(add-to-list 'e2e-scenarios #'eca-ghostel-buffers-e2e)
