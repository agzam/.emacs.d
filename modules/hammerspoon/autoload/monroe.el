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
