;;; modules/hammerspoon/autoload/monroe.el -*- lexical-binding: t; -*-

;; Monroe glue for the Hammerspoon (spacehammer) fennel nREPL.  Renamed off
;; the spacehammer- package namespace (writing-port precedent:
;; spacehammer--hs-eval-fennel -> hammerspoon-eval-fennel).

(defvar hammerspoon-monroe--reconnect-timer nil
  "Timer for auto-reconnecting to the Hammerspoon nREPL.")

;;;###autoload
(defun hammerspoon-monroe--cleanup ()
  "Kill stale monroe processes and buffers."
  (dolist (p (process-list))
    (when (string-prefix-p "monroe" (process-name p))
      (delete-process p)))
  (dolist (b (buffer-list))
    (when (string-match-p "\\*monroe" (buffer-name b))
      (kill-buffer b))))

;;;###autoload
(defun hammerspoon-monroe-connect ()
  "Connect to Hammerspoon's nREPL server via Monroe."
  (interactive)
  (hammerspoon-monroe--cleanup)
  (monroe (format "localhost:%d" monroe-default-port)))

;;;###autoload
(defun hammerspoon-monroe--try-reconnect ()
  "Reconnect to the Hammerspoon nREPL silently, without popping the REPL buffer."
  (when hammerspoon-monroe--reconnect-timer
    (cancel-timer hammerspoon-monroe--reconnect-timer)
    (setq hammerspoon-monroe--reconnect-timer nil))
  (condition-case nil
      (let* ((host (format "localhost:%d" monroe-default-port))
             (win (cl-some (lambda (b)
                             (and (string-match-p "\\*monroe" (buffer-name b))
                                  (get-buffer-window b)))
                           (buffer-list))))
        (hammerspoon-monroe--cleanup)
        (with-current-buffer
            (get-buffer-create (concat "*monroe: " host "*"))
          (monroe-connect host)
          (goto-char (point-max))
          (monroe-mode)
          (when (and win (window-live-p win))
            (set-window-buffer win (current-buffer))))
        (message "Hammerspoon nREPL: reconnected"))
    (error
     (setq hammerspoon-monroe--reconnect-timer
           (run-with-timer 1 nil #'hammerspoon-monroe--try-reconnect)))))

;;;###autoload
(defun hammerspoon-monroe-sentinel-a (orig-fn process message)
  "Advise monroe sentinel to auto-reconnect on disconnect."
  (funcall orig-fn process message)
  (when (and (string-prefix-p "monroe/" (process-name process))
             (not (process-live-p process)))
    (message "Hammerspoon nREPL: connection lost, reconnecting...")
    (setq hammerspoon-monroe--reconnect-timer
          (run-with-timer 1.5 nil #'hammerspoon-monroe--try-reconnect))))

(defun hammerspoon-monroe--repl-buffer ()
  "REPL buffer of a live Hammerspoon monroe connection, or nil.
Liveness is judged by the connection process, which monroe keeps on a
separate *monroe-connection* buffer."
  (let* ((host (format "%s:%d" monroe-default-host monroe-default-port))
         (repl (get-buffer (format "*monroe: %s*" host)))
         (proc (get-buffer-process (format "*monroe-connection: %s*" host))))
    (when (and repl (process-live-p proc))
      repl)))

;;;###autoload
(defun hammerspoon-monroe-eval-sync (fennel-form &optional timeout)
  "Eval FENNEL-FORM in Hammerspoon, block until the result arrives.
Returns a plist (:value V :out O :err E), each a string or nil.
Values come serialized through hs.inspect, so payloads meant for
parsing are better printed to :out by the form itself.  Signals
`user-error' when disconnected, `error' after TIMEOUT (default 10s)."
  (let ((repl (hammerspoon-monroe--repl-buffer)))
    (unless repl
      (user-error "Hammerspoon nREPL is not connected (M-x hammerspoon-monroe-connect)"))
    (let (val-acc out-acc err-acc done)
      (with-current-buffer repl
        (monroe-send-eval-string
         fennel-form
         (lambda (response)
           ;; monroe's bencode layer yields alists with string keys
           (let ((value (cdr (assoc "value" response)))
                 (out (cdr (assoc "out" response)))
                 (err (cdr (assoc "err" response)))
                 (status (cdr (assoc "status" response))))
             (when value (setq val-acc (concat val-acc value)))
             (when out (setq out-acc (concat out-acc out)))
             (when err (setq err-acc (concat err-acc err)))
             (when (member "done" status) (setq done t))))))
      (let ((deadline (+ (float-time) (or timeout 10))))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (unless done
        (error "Hammerspoon eval timed out: %s" fennel-form))
      (list :value val-acc :out out-acc :err err-acc))))

;;;###autoload
(defun hammerspoon-monroe-eval-async (fennel-form)
  "Fire-and-forget eval of FENNEL-FORM in Hammerspoon.
Silently does nothing when disconnected - meant for cosmetic calls
(e.g. preview overlays) that must never interrupt the caller."
  (when-let* ((repl (hammerspoon-monroe--repl-buffer)))
    (with-current-buffer repl
      (monroe-send-eval-string fennel-form #'ignore))))
