;;; tests/e2e/gptel-chat-from-region.el --- SPC g n over a selection -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The chat quotes the lines evil hands a leader command, or goes on with
;; the gptel exchange they hold, as read by the org that renders the chat
;; and the parser gptel builds its request with.  Only real keys show it.

(require 'cl-lib)

(defun gptel-chat-from-region-e2e--chats ()
  "Every live gptel chat buffer."
  (seq-filter (lambda (b) (buffer-local-value 'gptel-mode b)) (buffer-list)))

(defun gptel-chat-from-region-e2e--seed (buf text line keys)
  "Fill BUF with TEXT, press KEYS from LINE, and return the chats they opened."
  (let ((before (gptel-chat-from-region-e2e--chats)))
    (with-current-buffer buf
      (erase-buffer)
      (insert text)
      (font-lock-ensure))
    (switch-to-buffer buf)
    (delete-other-windows)
    (with-current-buffer buf
      (evil-force-normal-state)
      (goto-char (point-min))
      (forward-line (1- line)))
    (discard-input)
    ;; V opens expreg-transient, which replays the SPC it lets through,
    ;; except inside a keyboard macro, which general assumes recorded it.
    ;; A new chat may swap in another model, which kill-emacs-hook
    ;; would persist into custom.el.
    (let ((general--simulate-as-is t)
          (gptel-model gptel-model)
          (gptel-backend gptel-backend))
      (mapc (lambda (key) (execute-kbd-macro (kbd key))) keys))
    (seq-difference (gptel-chat-from-region-e2e--chats) before)))

(defun gptel-chat-from-region-e2e--kill (chats)
  "Kill CHATS without a prompt to save them."
  (dolist (chat chats)
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (gptel-mode -1)
        (set-buffer-modified-p nil))
      (kill-buffer chat))))

(defun gptel-chat-from-region-e2e--request (chat)
  "The messages gptel would send from the end of CHAT."
  (with-current-buffer chat
    (let ((prompt (gptel--create-prompt-buffer (point-max))))
      (unwind-protect
          (with-current-buffer prompt
            (gptel--parse-buffer gptel-backend nil))
        (kill-buffer prompt)))))

(defun gptel-chat-from-region-e2e ()
  "Start chats with SPC g n over visual-line selections in real files."
  (require 'gptel)
  (let ((el (find-file-noselect (expand-file-name "chat-src.el" e2e-work-dir)))
        (md (find-file-noselect (expand-file-name "chat-src.md" e2e-work-dir)))
        (chats-before (gptel-chat-from-region-e2e--chats))
        ;; prisma's Markdown parser needs the tree-sitter grammar; without
        ;; it a quote falls back to an example block, an exchange stays raw
        (converts (cl-every #'treesit-language-available-p
                            '(markdown markdown-inline)))
        (results '()))
    (cl-flet ((record (label ok got want)
                (push (list :label (format "chat from region: %s" label)
                            :ok ok :got got :want want)
                      results)
                ok)
              (text-of (chat)
                (with-current-buffer chat
                  (buffer-substring-no-properties (point-min) (point-max))))
              (folded (chat text)
                (with-current-buffer chat
                  (save-excursion
                    (goto-char (point-min))
                    (and (search-forward text nil t)
                         (invisible-p (match-beginning 0))
                         t)))))
      (unwind-protect
          (catch 'stop
            (let ((new (gptel-chat-from-region-e2e--seed
                        el (concat ";; greetings\n"
                                   "(defun greet (name)\n"
                                   "  \"Greet NAME.\n"
                                   "* a star at the start of a line\"\n"
                                   "  (message \"hi %s\" name))\n")
                        3 '("V" "j" "SPC g n"))))
              (unless (record "SPC g n over the selection opens one chat"
                              (= 1 (length new))
                              (list :chats (mapcar #'buffer-name new)
                                    :selected (buffer-name (window-buffer (selected-window)))
                                    :source-state (buffer-local-value 'evil-state el))
                              "one new gptel-mode buffer")
                (throw 'stop nil))
              (let* ((chat (car new))
                     (got (with-current-buffer chat
                            (list :selected (eq chat (window-buffer (selected-window)))
                                  :mode major-mode
                                  :text (text-of chat)
                                  :at-end (= (point) (point-max))
                                  :state evil-state)))
                     (want (list :selected t
                                 :mode 'org-mode
                                 :text (concat "#+begin_src emacs-lisp\n"
                                               "  \"Greet NAME.\n"
                                               ",* a star at the start of a line\"\n"
                                               "#+end_src\n\n")
                                 :at-end t
                                 :state 'insert)))
                (record "code lands in a src block, no heading over it, point below"
                        (equal got want) got want)))
            (let ((got (list :source-shown (and (get-buffer-window el) t)
                             :source-state (buffer-local-value 'evil-state el)
                             :mark (buffer-local-value 'mark-active el)))
                  (want '(:source-shown t :source-state normal :mark nil)))
              (record "the selection is dropped and its file stays in view"
                      (equal got want) got want))
            (let* ((new (gptel-chat-from-region-e2e--seed
                         md (concat "# Notes\n\nSome **bold** words.\n\n"
                                    "```org\n* TODO x\n```\n\n## Next\n")
                         ;; expreg-transient lets j through, but not a count
                         1 '("V" "j" "j" "j" "j" "j" "j" "SPC g n")))
                   (got (mapcar #'text-of new))
                   (want (list (if converts
                                   (concat "* Notes\n\nSome *bold* words.\n\n"
                                           "#+begin_src org\n,* TODO x\n#+end_src\n\n")
                                 (concat "#+begin_example\n"
                                         "# Notes\n\nSome **bold** words.\n\n"
                                         "```org\n,* TODO x\n```\n"
                                         "#+end_example\n\n")))))
              (record (if converts
                          "Markdown lands as Org at its own heading levels, a fenced * line escaped"
                        "Markdown lands in an example block without the grammar")
                      (equal got want) got want))
            ;; the Markdown buffer once gptel-send answered its first line,
            ;; laid out the way gptel writes into a buffer that is not Org
            (let ((exchange-chat
                   (let* ((exchange
                           (concat "Why is the sky **blue**?\n\n"
                                   (propertize "``` reasoning\nLight scatters.\n```"
                                               'gptel 'ignore)
                                   "\n\n"
                                   (propertize "``` tool (Search :q \"sky\")" 'gptel 'ignore)
                                   (propertize (concat "\n(:name \"Search\" :args (:q \"sky\"))"
                                                       "\n\n* Rayleigh scattering")
                                               'gptel '(tool . "toolu_e2e"))
                                   (propertize "\n```\n" 'gptel 'ignore)
                                   "\n"
                                   (propertize "It is **Rayleigh** scattering." 'gptel 'response)
                                   "\n"))
                          (new (gptel-chat-from-region-e2e--seed
                                md exchange 1 (append '("V") (make-list 12 "j") '("SPC g n"))))
                          (chat (car new))
                          (got (and chat
                                    (list :chats (length new)
                                          :text (text-of chat)
                                          :folded (list (folded chat "Light scatters")
                                                        (folded chat ",* Rayleigh scattering")
                                                        (folded chat "It is"))
                                          :state (buffer-local-value 'evil-state chat))))
                          (want (list :chats 1
                                      :text (concat (if converts
                                                        "Why is the sky *blue*?"
                                                      "Why is the sky **blue**?")
                                                    "\n\n#+begin_reasoning\nLight scatters.\n"
                                                    "#+end_reasoning\n\n"
                                                    "#+begin_tool (Search :q \"sky\")\n"
                                                    "(:name \"Search\" :args (:q \"sky\"))\n\n"
                                                    ",* Rayleigh scattering\n#+end_tool\n\n"
                                                    (if converts
                                                        "It is *Rayleigh* scattering."
                                                      "It is **Rayleigh** scattering.")
                                                    "\n\n* ")
                                      :folded '(t t nil)
                                      :state 'insert)))
                     (unless (record "a selected exchange lands as an Org chat, blocks folded"
                                     (equal got want) got want)
                       (throw 'stop nil))
                     (execute-kbd-macro (kbd "Why?"))
                     (let* ((messages (gptel-chat-from-region-e2e--request chat))
                            (got (list :roles (mapcar (lambda (m) (plist-get m :role)) messages)
                                       :reasoning-sent (and (string-search "Light scatters"
                                                                           (format "%S" messages))
                                                            t)
                                       :parts (thread-last
                                                messages
                                                (seq-mapcat (lambda (m)
                                                              (let ((c (plist-get m :content)))
                                                                (and (vectorp c) (append c nil)))))
                                                (seq-keep (lambda (part) (plist-get part :type))))))
                            (want '(:roles ("user" "assistant" "user" "assistant" "user")
                                    :reasoning-sent nil
                                    :parts ("text" "tool_use" "tool_result" "text"))))
                       (record "gptel reads the prompt, tool call, reply and follow-up apart"
                               (equal got want) got want))
                     chat)))
              ;; without a selection SPC g n jumps to the chat used last,
              ;; though it has no answer yet and so no file
              (switch-to-buffer el)
              (delete-other-windows)
              (with-current-buffer el (evil-force-normal-state))
              (let ((before (gptel-chat-from-region-e2e--chats))
                    seeded agent)
                (cl-letf (((symbol-function 'gptel-chat-from-region)
                           (lambda (&rest _) (setq seeded t)))
                          ((symbol-function 'gptel-agent)
                           (lambda (&rest _) (interactive) (setq agent t))))
                  (execute-kbd-macro (kbd "SPC g n")))
                (let ((got (list :shown (buffer-name (window-buffer (selected-window)))
                                 :file (buffer-file-name exchange-chat)
                                 :new (length (seq-difference
                                               (gptel-chat-from-region-e2e--chats) before))
                                 :seeded seeded :agent agent))
                      (want (list :shown (buffer-name exchange-chat) :file nil :new 0
                                  :seeded nil :agent nil)))
                  (record "SPC g n with no selection jumps to the unsaved chat used last"
                          (equal got want) got want))))
            ;; with no chat open it starts a new one
            (gptel-chat-from-region-e2e--kill
             (seq-difference (gptel-chat-from-region-e2e--chats) chats-before))
            (switch-to-buffer el)
            (delete-other-windows)
            (with-current-buffer el (evil-force-normal-state))
            (let (seeded agent)
              (cl-letf (((symbol-function 'gptel-chat-from-region)
                         (lambda (&rest _) (setq seeded t)))
                        ((symbol-function 'gptel-agent)
                         (lambda (&rest _) (interactive) (setq agent t))))
                (execute-kbd-macro (kbd "SPC g n")))
              (record "SPC g n with no selection and no chat starts a gptel-agent session"
                      (and (not seeded) agent)
                      (list :seeded seeded :agent agent
                            :chats (mapcar #'buffer-name (gptel-chat-from-region-e2e--chats)))
                      '(:seeded nil :agent t :chats nil))))
        (discard-input)
        ;; a failed step may strand expreg's menu; never leak it into the
        ;; scenarios that run after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (gptel-chat-from-region-e2e--kill
         (seq-difference (gptel-chat-from-region-e2e--chats) chats-before))
        (dolist (buf (list el md))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (deactivate-mark)
              (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (delete-other-windows)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'gptel-chat-from-region-e2e)
