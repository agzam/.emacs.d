;;; tests/e2e/gptel-chat-tools.el --- tools gptel starts with -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The selection is made as gptel loads, and a chat file restores its own
;; in the mode body, so only a booted config shows what gptel-menu, a chat
;; and a rewrite each end up with.

(require 'cl-lib)

(defun gptel-chat-tools-e2e ()
  "Check the tools gptel-menu shows, chats carry and rewrites send."
  (require 'gptel)
  (require 'gptel-rewrite)
  (unless (boundp 'gptel-improve-text-prompts-history)
    (load (expand-file-name "modules/ai/autoload/gptel.el" e2e-root)
          nil 'nomessage))
  (let ((results '())
        (buffers '()))
    (cl-flet* ((record (label ok got want)
                 (push (list :label (format "gptel tools: %s" label)
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
                 (list :own nil :count (length (known-names)) :missing nil :bash t))
               (visit (name text)
                 (let ((file (expand-file-name name e2e-work-dir)))
                   (with-temp-file file (insert text))
                   (let ((buf (find-file-noselect file)))
                     (push buf buffers)
                     (with-current-buffer buf
                       (switch-to-buffer buf)
                       (delete-other-windows)
                       (font-lock-ensure)
                       (evil-normal-state))
                     buf)))
               (chat-file (name text)
                 (let ((buf (visit name text)))
                   (with-current-buffer buf (gptel-mode 1))
                   buf))
               ;; point before mark: a rewrite ending at point queues a
               ;; timer that moves point in whichever buffer runs it
               (select-all ()
                 (goto-char (point-max))
                 (push-mark (point-max) t t)
                 (goto-char (point-min)))
               ;; a dry run builds the real payload and sends nothing
               (sent-tools (thunk)
                 (let ((real (symbol-function 'gptel-request))
                       fsm)
                   (cl-letf (((symbol-function 'gptel-request)
                              (lambda (prompt &rest args)
                                (setq fsm (apply real prompt :dry-run t args)))))
                     (funcall thunk))
                   (let ((info (and fsm (gptel-fsm-info fsm))))
                     (list :selected (length (default-value 'gptel-tools))
                           :requested (and fsm t)
                           :info (length (plist-get info :tools))
                           :payload (length (plist-get (plist-get info :data) :tools))))))
               (toolless-p (got)
                 (and (< 0 (plist-get got :selected))
                      (plist-get got :requested)
                      (= 0 (plist-get got :info) (plist-get got :payload)))))
      (unwind-protect
          (progn
            ;; what the menu draws is what the user reads
            (let* ((buf (visit "notes.txt" "plain notes\n"))
                   (drawn
                    (with-current-buffer buf
                      ;; the menu swaps in another model when the backend
                      ;; lacks the chosen one; never let it reach custom.el
                      (let ((gptel-model gptel-model)
                            (gptel-backend gptel-backend))
                        (execute-kbd-macro (kbd "SPC g g"))
                        (prog1 (when-let* ((b (get-buffer transient--buffer-name)))
                                 (with-current-buffer b
                                   (substring-no-properties (buffer-string))))
                          (execute-kbd-macro (kbd "C-g"))))))
                   (got (list :own (local-variable-p 'gptel-tools buf)
                              :menu (and drawn
                                         (string-match "Tools (\\([0-9]+\\) selected)"
                                                       drawn)
                                         (string-to-number (match-string 1 drawn)))))
                   (want (list :own nil :menu (length (known-names)))))
              (record "gptel-menu outside a chat shows every tool selected"
                      (equal got want) got want))

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

            ;; the global scope of gptel-menu picks what every chat starts with
            (let ((saved (default-value 'gptel-tools)))
              (unwind-protect
                  (progn
                    (setq-default gptel-tools nil)
                    (let* ((buf (chat-file "emptied-chat.org" "* hello\n"))
                           (got (with-current-buffer buf
                                  (list :own (local-variable-p 'gptel-tools)
                                        :count (length gptel-tools))))
                           (want '(:own nil :count 0)))
                      (record "a chat starts without tools once the global selection is emptied"
                              (equal got want) got want)))
                (setq-default gptel-tools saved)))

            ;; every rewrite path sends through this suffix
            (let* ((buf (visit "rewrite.txt" "teh txt to fix\n"))
                   (got (with-current-buffer buf
                          (select-all)
                          (sent-tools (lambda ()
                                        (gptel--suffix-rewrite "Fix the typos"))))))
              (record "a rewrite sends no tools"
                      (toolless-p got) got
                      "tools selected, a request built, none of them in it"))

            (pcase-dolist (`(,path ,file ,prompt)
                           `(("in place" "improve-in-place.txt"
                              ,(car gptel-improve-text-prompts-history))
                             ("aside" "improve-aside.txt"
                              ,(nth 3 gptel-improve-text-prompts-history))))
              (let* ((buf (visit file "teh txt to fix\n"))
                     (got (with-current-buffer buf
                            (select-all)
                            (let ((gptel-improve-text-prompt prompt))
                              (sent-tools #'gptel-improve-text)))))
                (record (format "improve-text %s sends no tools" path)
                        (toolless-p got) got
                        "tools selected, a request built, none of them in it")))

            (let ((got (default-value 'gptel-confirm-tool-calls)))
              (record "tool calls run without confirmation"
                      (null got) got nil)))
        (discard-input)
        ;; a failed step may strand the menu; never leak it into the
        ;; scenarios that run after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (dolist (buf buffers)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (bound-and-true-p gptel-mode) (gptel-mode -1))
              (deactivate-mark)
              (set-buffer-modified-p nil))
            (kill-buffer buf)))))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'gptel-chat-tools-e2e)
