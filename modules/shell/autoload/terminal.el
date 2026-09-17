;;; modules/shell/autoload/terminal.el --- put text at a terminal's prompt -*- lexical-binding: t; -*-

(require 'seq)

(declare-function ghostel-paste-string "ghostel" (string))
(declare-function code-snippet-at-point "code-snippet" ())
(declare-function evil-insert-state "evil-states" (&optional arg))
(declare-function shell-pop-choose "shell" (&optional arg))

(defvar terminal-start-timeout 2
  "Seconds to wait for a freshly started terminal to show its first prompt.")

(defun terminal-buffers ()
  "Live eshell and ghostel buffers, most recently used first."
  (seq-filter (lambda (buf)
                (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                         'eshell-mode 'ghostel-mode))
              (buffer-list)))

(defun terminal-buffer-table (buffers)
  "Completion table over BUFFERS that keeps them in the order given."
  (let ((names (mapcar #'buffer-name buffers)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . buffer)
                     (cycle-sort-function . identity)
                     (display-sort-function . identity))
        (complete-with-action action names string pred)))))

(defun terminal-ready-p ()
  "Non-nil when the current terminal buffer can take input.
A ghostel shell spawns asynchronously, and text pasted before its line
editor runs is read as literal escape sequences; its first OSC 133 prompt
says the shell is listening.  An eshell prompt is there the moment the
buffer is."
  (or (not (derived-mode-p 'ghostel-mode))
      (text-property-not-all (point-min) (point-max) 'ghostel-prompt nil)))

(defun start-new-terminal ()
  "Start a terminal with `shell-pop-choose' and return it once it takes input."
  (shell-pop-choose)
  (let ((buffer (car (terminal-buffers))))
    (unless buffer
      (user-error "No terminal started"))
    (with-current-buffer buffer
      (with-timeout (terminal-start-timeout nil)
        (while (not (terminal-ready-p))
          (accept-process-output nil 0.05))))
    buffer))

(defun read-terminal-buffer ()
  "The terminal to send to: the only live one, one the user picks, or a new one."
  (pcase (terminal-buffers)
    ('() (start-new-terminal))
    (`(,only) only)
    (buffers (get-buffer
              (completing-read "Terminal: " (terminal-buffer-table buffers)
                               nil t)))))

(defun terminal-insert (text)
  "Insert TEXT at the current terminal buffer's prompt."
  (if (derived-mode-p 'ghostel-mode)
      ;; bracketed paste: the shell keeps a multi-line snippet on its edit
      ;; line instead of running each line as its newline arrives
      (ghostel-paste-string text)
    (goto-char (point-max))
    (insert text)))

(defun send-to-terminal-text ()
  "The text `send-to-terminal' reads off the buffer."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((car (code-snippet-at-point)))
   (t (user-error "No region and no code snippet at point"))))

;;;###autoload
(defun send-to-terminal (text)
  "Put TEXT at a live terminal's prompt, unrun, with point there in insert state.
Interactively TEXT is the region, or the code snippet at point."
  (interactive (list (send-to-terminal-text)))
  (let ((buffer (read-terminal-buffer)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (terminal-insert text)
      ;; insert state so RET reaches the shell rather than evil's motion
      (evil-insert-state))))
