;;; tests/e2e/gptel-chat-from-region.el --- SPC g n over a selection -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The chat quotes the lines evil hands a leader command: code in a block
;; of its mode's language, Markdown converted by the real prisma, all read
;; by the org that renders the chat.  Only real keys over real files show it.

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

(defun gptel-chat-from-region-e2e ()
  "Start chats with SPC g n over visual-line selections in real files."
  (require 'gptel)
  (let ((el (find-file-noselect (expand-file-name "chat-src.el" e2e-work-dir)))
        (md (find-file-noselect (expand-file-name "chat-src.md" e2e-work-dir)))
        (chats-before (gptel-chat-from-region-e2e--chats))
        (results '()))
    (cl-flet ((record (label ok got want)
                (push (list :label (format "chat from region: %s" label)
                            :ok ok :got got :want want)
                      results)
                ok)
              (text-of (chat)
                (with-current-buffer chat
                  (buffer-substring-no-properties (point-min) (point-max))))
              (heading (buf lines)
                (format "* =%s:%s=\n" (abbreviate-file-name (buffer-file-name buf))
                        lines)))
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
                                 :text (concat (heading el "3-4")
                                               "#+begin_src emacs-lisp\n"
                                               "  \"Greet NAME.\n"
                                               ",* a star at the start of a line\"\n"
                                               "#+end_src\n\n")
                                 :at-end t
                                 :state 'insert)))
                (record "code lands in a src block under its source, point below"
                        (equal got want) got want)))
            (let ((got (list :source-shown (and (get-buffer-window el) t)
                             :source-state (buffer-local-value 'evil-state el)
                             :mark (buffer-local-value 'mark-active el)))
                  (want '(:source-shown t :source-state normal :mark nil)))
              (record "the selection is dropped and its file stays in view"
                      (equal got want) got want))
            ;; prisma's Markdown parser needs the tree-sitter grammar; without
            ;; it the quote falls back to an example block
            (let* ((converts (cl-every #'treesit-language-available-p
                                       '(markdown markdown-inline)))
                   (new (gptel-chat-from-region-e2e--seed
                         md (concat "# Notes\n\nSome **bold** words.\n\n"
                                    "```org\n* TODO x\n```\n\n## Next\n")
                         ;; expreg-transient lets j through, but not a count
                         1 '("V" "j" "j" "j" "j" "j" "j" "SPC g n")))
                   (got (mapcar #'text-of new))
                   (want (list (concat (heading md "1-7")
                                       (if converts
                                           (concat "** Notes\n\nSome *bold* words.\n\n"
                                                   "#+begin_src org\n,* TODO x\n#+end_src")
                                         (concat "#+begin_example\n"
                                                 "# Notes\n\nSome **bold** words.\n\n"
                                                 "```org\n,* TODO x\n```\n"
                                                 "#+end_example"))
                                       "\n\n"))))
              (record (if converts
                          "Markdown lands as Org: heading one level down, a fenced * line escaped"
                        "Markdown lands in an example block without the grammar")
                      (equal got want) got want))
            ;; without a selection SPC g n keeps its old path
            (switch-to-buffer el)
            (delete-other-windows)
            (with-current-buffer el (evil-force-normal-state))
            (let (seeded agent)
              (cl-letf (((symbol-function 'gptel-chat-from-region)
                         (lambda (&rest _) (setq seeded t)))
                        ((symbol-function 'gptel-agent)
                         (lambda (&rest _) (interactive) (setq agent t))))
                (execute-kbd-macro (kbd "SPC g n")))
              (record "SPC g n with no selection starts no seeded chat"
                      (and (not seeded) agent)
                      (list :seeded seeded :agent agent)
                      '(:seeded nil :agent t))))
        (discard-input)
        ;; a failed step may strand expreg's menu; never leak it into the
        ;; scenarios that run after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (dolist (chat (seq-difference (gptel-chat-from-region-e2e--chats)
                                      chats-before))
          (with-current-buffer chat
            (gptel-mode -1)
            (set-buffer-modified-p nil))
          (kill-buffer chat))
        (dolist (buf (list el md))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (deactivate-mark)
              (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (delete-other-windows)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'gptel-chat-from-region-e2e)
