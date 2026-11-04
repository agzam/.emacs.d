;;; tests/e2e/gptel-chat-tools.el --- tools a new gptel chat starts with -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; gptel restores a chat file's saved tools in the mode body, and the
;; selection runs from the mode hook after it, so only a real
;; `gptel-mode' in a real buffer shows which of the two wins.

(require 'cl-lib)

(defun gptel-chat-tools-e2e ()
  "Start gptel chats the ways a chat starts and check the tools each carries."
  (require 'gptel)
  (let ((results '())
        (buffers '()))
    (cl-flet* ((record (label ok got want)
                 (push (list :label (format "gptel chat tools: %s" label)
                             :ok ok :got got :want want)
                       results)
                 ok)
               (known-names ()
                 (cl-loop for (_ . tools) in gptel--known-tools
                          append (mapcar #'car tools)))
               ;; the full list runs to dozens of names; the transcript
               ;; carries what a failure needs
               (selection (buf)
                 (with-current-buffer buf
                   (let ((names (mapcar #'gptel-tool-name gptel-tools)))
                     (list :own (local-variable-p 'gptel-tools)
                           :count (length names)
                           :missing (cl-set-difference (known-names) names
                                                       :test #'equal)
                           :bash (and (member "Bash" names) t)))))
               (every-tool ()
                 (list :own t :count (length (known-names)) :missing nil :bash t))
               (chat-file (name text)
                 (let ((file (expand-file-name name e2e-work-dir)))
                   (with-temp-file file (insert text))
                   (let ((buf (find-file-noselect file)))
                     (push buf buffers)
                     (with-current-buffer buf
                       (switch-to-buffer buf)
                       (delete-other-windows)
                       (font-lock-ensure)
                       (gptel-mode 1))
                     buf))))
      (unwind-protect
          (progn
            ;; the command swaps in another model when the backend lacks the
            ;; chosen one, and the kill-emacs hook would persist the swap
            (let ((buf (with-temp-buffer
                         (let ((gptel-model gptel-model)
                               (gptel-backend gptel-backend))
                           (gptel "*gptel-chat-tools-e2e*")))))
              (push buf buffers)
              (let ((got (selection buf))
                    (want (every-tool)))
                (record "a chat from the gptel command starts with every tool"
                        (equal got want) got want)))

            (let* ((buf (chat-file "fresh-chat.org" "* hello\n"))
                   (got (selection buf))
                   (want (every-tool)))
              (record "a chat file without saved tools starts with every tool"
                      (equal got want) got want))

            ;; the saved names resolve only once gptel-agent's tools exist
            (require 'gptel-agent)
            (let* ((buf (chat-file "saved-chat.org"
                                   (concat ":PROPERTIES:\n"
                                           ":GPTEL_TOOLS: Read Grep\n"
                                           ":END:\n"
                                           "* hello\n")))
                   (got (with-current-buffer buf
                          (list :own (local-variable-p 'gptel-tools)
                                :tools (mapcar #'gptel-tool-name gptel-tools))))
                   (want '(:own t :tools ("Read" "Grep"))))
              (record "a chat file keeps the tools it saved"
                      (equal got want) got want))

            (let ((got (default-value 'gptel-confirm-tool-calls)))
              (record "tool calls run without confirmation"
                      (null got) got nil)))
        (dolist (buf buffers)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (gptel-mode -1)
              (set-buffer-modified-p nil))
            (kill-buffer buf)))))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'gptel-chat-tools-e2e)
