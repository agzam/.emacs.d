;;; modules/ai/autoload/eca.el -*- lexical-binding: t; -*-

;;;###autoload
(defadvice! eca-chat-seed-code-and-trust-a (_session)
  "Start every new chat in the code agent with trust on.
A new chat copies its selection from the session defaults, which
upstream keeps sticky: switching one chat to plan, or resuming an
untrusted one, would decide how every chat opened afterwards starts.
Overriding that copy keeps the switch a per-chat choice."
  :after #'eca-chat--initialize-selection-state
  (setq-local eca-chat--selected-agent "code"
              eca-chat--selected-trust t))

;;;###autoload
(defun eca-chat-flag-and-fork ()
  "Add a flag at point and immediately fork the conversation from it.
Uses async RPCs to avoid `eca-api-request-sync' throw which drops
sibling notifications from the same process-filter batch."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let ((nearest-id nil)
        (nearest-pos -1)
        (session (eca-session)))
    (dolist (ov (overlays-in (point-min) (1+ (point))))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id))
                  (pos (overlay-start ov)))
        ;; Only consider message overlays (UUID content-ids).
        ;; Skip tool calls (toolu_*), widgets (eca-chat-*), flags.
        (when (and (> pos nearest-pos)
                   (not (overlay-get ov 'eca-chat--flag-text))
                   (not (overlay-get ov 'eca-chat--tool-call-status))
                   (string-match-p "\\`[0-9a-f]\\{8\\}-" id))
          (setq nearest-id id nearest-pos pos))))
    (if (not nearest-id)
        (user-error "No message found before point")
      (let ((origin-title (or eca-chat--title "chat"))
            (existing-flags
             (mapcar (lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
                     (seq-filter (lambda (ov) (overlay-get ov 'eca-chat--flag-text))
                                 (overlays-in (point-min) (point-max)))))
            (existing-bufs (eca-vals (eca--session-chats session)))
            (proc (eca--session-process session))
            (deadline (+ (float-time) 10))
            (rpc-err nil))
        ;; Phase 1 - add flag (async so the contentReceived notification
        ;; in the same pipe chunk is not dropped by throw)
        (eca-api-request-async session
          :method "chat/addFlag"
          :params (list :chatId eca-chat--id
                        :contentId nearest-id
                        :text (format-time-string "fork @ %H:%M"))
          :success-callback #'ignore
          :error-callback (lambda (err) (setq rpc-err err)))
        (let ((new-flag-id nil))
          (while (and (not new-flag-id) (not rpc-err)
                      (< (float-time) deadline))
            (accept-process-output proc 0.2)
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when-let* ((_ft (overlay-get ov 'eca-chat--flag-text))
                          (id (overlay-get ov 'eca-chat--expandable-content-id)))
                (unless (member id existing-flags)
                  (setq new-flag-id id)))))
          (when rpc-err (user-error "addFlag failed: %s" rpc-err))
          (unless new-flag-id (user-error "Timed out waiting for flag"))
          ;; Phase 2 - fork (async so the chat/opened notification is
          ;; not dropped either)
          (setq rpc-err nil)
          (eca-api-request-async session
            :method "chat/fork"
            :params (list :chatId eca-chat--id
                          :contentId new-flag-id)
            :success-callback #'ignore
            :error-callback (lambda (err) (setq rpc-err err)))
          (let ((new-buf nil))
            (while (and (not new-buf) (not rpc-err)
                        (< (float-time) deadline))
              (accept-process-output proc 0.2)
              (dolist (buf (eca-vals (eca--session-chats session)))
                (unless (memq buf existing-bufs)
                  (setq new-buf buf))))
            (when rpc-err (user-error "fork failed: %s" rpc-err))
            (if new-buf
                (progn
                  (setf (eca--session-last-chat-buffer session) new-buf)
                  (switch-to-buffer new-buf)
                  (let ((fork-title (format "Fork: %s" origin-title)))
                    (setq-local eca-chat--title fork-title)
                    (eca-api-request-async session
                      :method "chat/update"
                      :params (list :chatId eca-chat--id :title fork-title)
                      :success-callback #'ignore
                      :error-callback #'ignore)
                    (eca-chat--force-tab-line-update)))
              (user-error "Timed out waiting for forked chat"))))))))

;;;###autoload
(defadvice! eca-hide-buffer-name-a (orig-fn &rest args)
  "Prepend a space to make the buffer name hidden."
  :around 'eca-process--buffer-name
  :around 'eca-process--stderr-buffer-name
  :around 'eca--emacs-errors-buffer-name
  (concat " " (apply orig-fn args)))

;;;###autoload
(defun eca-chat-buffer-name (&optional title)
  "Build a chat buffer name from TITLE, falling back to \"Empty chat\"."
  (format "eca-chat - %s" (or title "Empty chat")))

;;;###autoload
(defun eca-compact-modeline-icons-h ()
  "Scale the trust/elapsed emoji so the chat mode line matches other windows.
Their color-emoji glyphs (Apple Color Emoji) are taller than the text
font, and doom-modeline's height bar is pinned to 1px, so nothing else
clamps the line - it floats up to the emoji height (~33px vs 26px
elsewhere).  The remap is buffer-local, so the minibuffer completion
annotations that reuse the elapsed face are untouched.  Runs from
`eca-chat-mode-hook', which fires twice per buffer, hence the guard
against re-adding a remap that would stack multiplicatively."
  (dolist (face '(eca-chat-trust-on-face
                  eca-chat-trust-off-face
                  eca-chat-elapsed-time-face))
    (unless (assq face face-remapping-alist)
      (face-remap-add-relative face :height 0.7))))

;;;###autoload
(defadvice! eca-chat-new-buffer-name-a (_session)
  "Override chat buffer naming to use a readable title."
  :override 'eca-chat-new-buffer-name
  (eca-chat-buffer-name))

;;;###autoload
(defun eca-chat-rename-buffer-on-title-change ()
  "Rename the current chat buffer to reflect the new title."
  (when (derived-mode-p 'eca-chat-mode)
    (let* ((title (or eca-chat--custom-title eca-chat--title "Empty chat"))
           (new-name (eca-chat-buffer-name title)))
      (unless (string= (buffer-name) new-name)
        (rename-buffer new-name t)))))

;;;###autoload
(defadvice! eca-chat-rename-after-content-a (session _params)
  "Rename chat buffer after content-received sets the title."
  :after 'eca-chat-content-received
  :after 'eca-chat-opened
  (when-let* ((chats (eca--session-chats session)))
    (dolist (pair chats)
      (when (buffer-live-p (cdr pair))
        (with-current-buffer (cdr pair)
          (eca-chat-rename-buffer-on-title-change))))))

;;;###autoload
(defadvice! eca-chat-exit-cleanup-a (orig-fn session)
  "Around advice to fix closed-buffer cleanup for renamed chat buffers."
  :around 'eca-chat-exit
  (funcall orig-fn session)
  ;; Clean up stale closed chat buffers with new naming scheme.
  (let ((latest nil))
    (dolist (b (buffer-list))
      (when (string-match-p "^eca-chat - .*:closed" (buffer-name b))
        (if latest
            (kill-buffer b)
          (setq latest b))))))

;;;###autoload
(define-minor-mode eca-workspaces-mode
  "Minor mode for keybindings in the eca-workspaces buffer."
  :keymap (make-sparse-keymap))

;;;###autoload
(defun eca-toggle-workspaces ()
  "Toggle the eca-workspaces side window."
  (interactive)
  (if-let* ((buf (get-buffer eca-workspaces-buffer-name))
            (win (get-buffer-window buf t)))
      (delete-window win)
    (eca-workspaces)
    (when-let* ((buf (get-buffer eca-workspaces-buffer-name)))
      (with-current-buffer buf
        (eca-workspaces-mode 1)))))

;;;###autoload
(defun eca-reauth ()
  "Re-trigger Anthropic Max OAuth login on the current session.
Workaround for the OAuth refresh race condition (eca#462).
After token rotation invalidates this session's refresh-token,
the CLI process stays alive, so re-running the max flow gets
a fresh token pair without losing the active chat."
  (interactive)
  (let ((session (eca-session)))
    (unless session
      (user-error "No active ECA session for current buffer"))
    (unless (eca-process-running-p session)
      (user-error "ECA session is not running"))
    (eca-providers--do-login session "anthropic" "max")))


;;;; Keeping a closed chat alive

;; Killing the buffer is how a chat gets put away, and upstream turns that into a
;; `chat/delete' prompt - a hard delete, no trash, no undo - on the same keystroke
;; used to tidy up. Answer it wrong once and the conversation is gone from the
;; server; the archived markdown that survives is a transcript, not the message
;; history the model reads, so nothing can bring the chat back. Closing now only closes.

;;;###autoload
(defadvice! eca-chat-kill-keeps-server-copy-a ()
  "Let the kill proceed without deleting the chat server-side."
  :override #'eca-chat--kill-buffer-query
  (setq-local eca-chat--kill-delete-server-side nil)
  t)

;;;###autoload
(defadvice! eca-chat-delete-confirm-a (&rest _)
  "Confirm before `eca-chat-delete' destroys the chat server-side.
It sits one key from `eca-chat-reset', which merely closes."
  :before-while #'eca-chat-delete
  (yes-or-no-p "Delete this chat from the server (cannot be undone)? "))


;;;; Session archiving + resume-from-archive

(defvar eca-archive-dir)                ; real defcustom lives in config.el

(defun eca-archive--slugify (string)
  "Return STRING as a filename-safe slug, or \"\" when blank.
Drops text properties, turns whitespace and characters illegal in
filenames into single hyphens, trims stray hyphens/dots, and caps the
length so titles stay readable but bounded."
  (let* ((s (replace-regexp-in-string
             "[[:cntrl:][:space:]/\\:*?\"<>|]+" "-"
             (substring-no-properties (or string ""))))
         (s (replace-regexp-in-string "-+" "-" s))
         (s (string-trim s "[-.]+" "[-.]+")))
    (if (< 72 (length s))
        (string-trim-right (substring s 0 72) "[-.]+")
      s)))

(defun eca-archive--chat-file-name (project title id-short)
  "Return the archive basename for a chat.
PROJECT and ID-SHORT identify the chat and keep the name unique; TITLE,
when it slugifies to something non-empty, is inserted for readability."
  (let ((slug (eca-archive--slugify title)))
    (if (string-empty-p slug)
        (format "%s_%s.md" project id-short)
      (format "%s__%s_%s.md" project slug id-short))))

;;;###autoload
(defun eca-archive-chat (&optional buffer)
  "Write BUFFER's chat transcript to `eca-archive-dir' as Markdown.
Keeps one file per chat - named after the project, chat title, and
short chat id - re-saved each finished turn.  When the title changes,
the previous file for this chat is removed.  Return the file path, or
nil if BUFFER is not an archivable chat."
  (interactive)
  (with-demoted-errors "eca-archive: %S"
    (with-current-buffer (or buffer (current-buffer))
      (when (and (derived-mode-p 'eca-chat-mode)
                 (stringp eca-chat--id)
                 (not (string-prefix-p "subagent-" eca-chat--id)))
        (let* ((session   (thread-last
                            eca--sessions
                            eca-vals
                            (seq-find (lambda (s)
                                        (memq (current-buffer)
                                              (eca-vals (eca--session-chats s)))))))
               (project   (replace-regexp-in-string
                           "\\`[.]+" ""
                           (if session (eca--session-project-name session) "unknown")))
               (workspace (when session (car (eca--session-workspace-folders session))))
               (model     (or eca-chat--selected-model "unknown"))
               (id        eca-chat--id)
               (id-short  (substring id 0 (min 8 (length id))))
               (title     (when (or eca-chat--custom-title eca-chat--title)
                            (substring-no-properties (eca-chat-title))))
               (dir       (expand-file-name eca-archive-dir))
               (file      (expand-file-name
                           (eca-archive--chat-file-name project title id-short) dir))
               (content   (buffer-substring-no-properties (point-min) (point-max))))
          (make-directory dir t)
          (with-temp-file file
            (insert (format "<!-- eca: %S -->\n\n"
                            (list :id id :workspace workspace :model model
                                  :project project :title title)))
            (insert content))
          ;; Keep one file per chat: now that FILE exists, drop any stale
          ;; name for this id (e.g. an earlier untitled save).
          (dolist (old (file-expand-wildcards
                        (expand-file-name (format "*_%s.md" id-short) dir)))
            (unless (file-equal-p old file)
              (ignore-errors (delete-file old))))
          (when (called-interactively-p 'interactive)
            (eca-info "Archived chat to %s" file))
          file)))))

(defun eca-archive--read-meta (file)
  "Return the metadata plist on FILE's first line, or nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (when (and (string-prefix-p "<!-- eca: " line)
                 (string-suffix-p " -->" line))
        (read (string-remove-suffix
               " -->" (string-remove-prefix "<!-- eca: " line)))))))

(defun eca-archive--parse-name (file)
  "Return (PROJECT . TITLE) read off FILE's name.
`eca-archive--chat-file-name' builds PROJECT__TITLE_ID.md, or
PROJECT_ID.md when the title slugified away.  Only archives written
before the metadata line carried both need this; their title keeps the
slug's hyphens rather than being guessed back into words."
  (let ((base (file-name-base file)))
    (cond
     ((string-match "\\`\\(.+?\\)__\\(.+\\)_[0-9a-f]+\\'" base)
      (cons (match-string 1 base) (match-string 2 base)))
     ((string-match "\\`\\(.+\\)_[0-9a-f]+\\'" base)
      (cons (match-string 1 base) nil))
     (t (cons base nil)))))

(defun eca-archive--entry (file)
  "Describe archive FILE as a plist, or nil when it records no chat.
Keys: :file :id :workspace :model :project :title :time."
  (when-let* ((meta (eca-archive--read-meta file))
              (id (plist-get meta :id)))
    (pcase-let ((`(,project . ,title) (eca-archive--parse-name file)))
      (list :file file
            :id id
            :workspace (plist-get meta :workspace)
            :model (plist-get meta :model)
            :project (or (plist-get meta :project) project)
            :title (or (plist-get meta :title) title)
            :time (file-attribute-modification-time (file-attributes file))))))

(defun eca-archive-entries ()
  "Every archived chat, most recently written first."
  (let ((dir (expand-file-name eca-archive-dir)))
    (sort (delq nil (mapcar #'eca-archive--entry
                            (and (file-directory-p dir)
                                 (directory-files dir t "\\.md\\'"))))
          (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time))))))

(defun eca-archive--id-short (entry)
  "The leading 8 characters of ENTRY's chat id."
  (let ((id (plist-get entry :id)))
    (substring id 0 (min 8 (length id)))))

(defun eca-archive--label (entry)
  "A one-line description of ENTRY for completion."
  (format "%s  %s"
          (or (plist-get entry :project) "?")
          (or (plist-get entry :title) "untitled")))

(defun eca-archive--table (entries)
  "Alist of label to entry over ENTRIES, keeping their order.
Two chats in one project can carry the same title, so a repeated label
gains the chat id - unique by construction - rather than shadowing the
entry it collides with."
  (let (table)
    (dolist (entry entries (nreverse table))
      (let ((label (eca-archive--label entry)))
        (when (assoc label table)
          (setq label (format "%s  [%s]" label (eca-archive--id-short entry))))
        (push (cons label entry) table)))))

(defun eca-archive--completion-table (labels)
  "Completion table over LABELS that keeps them in the given order.
The list arrives newest first and that is the useful order; completion
would otherwise sort it alphabetically."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . eca-archive-chat)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action labels string pred))))

(defun eca-archive-read-entry (&optional prompt)
  "Prompt with PROMPT for an archived chat and return its entry."
  (let* ((table (eca-archive--table (eca-archive-entries)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (label)
                  (when-let* ((entry (cdr (assoc label table))))
                    (format "  %s"
                            (format-time-string "%Y-%m-%d %H:%M"
                                                (plist-get entry :time))))))))
    (unless table
      (user-error "No archived chats under %s" eca-archive-dir))
    (cdr (assoc (completing-read (or prompt "Continue chat: ")
                                 (eca-archive--completion-table (mapcar #'car table))
                                 nil t)
                table))))

(defun eca-archive--session-for-root (root)
  "Return a running session whose workspace folders include ROOT."
  (when (and (stringp root) (not (string-empty-p root)))
    (let ((root (file-name-as-directory (expand-file-name root))))
      (thread-last
       eca--sessions
       eca-vals
       (seq-find
        (lambda (s)
          (thread-last
           (eca--session-workspace-folders s)
           (seq-some (lambda (f)
                       (string= (file-name-as-directory (expand-file-name f))
                                root))))))))))

(defvar eca-archive-session-timeout 60
  "Seconds to wait for a session to finish starting before giving up.")

(defun eca-archive--when-started (session callback &optional deadline)
  "Call CALLBACK with SESSION once it reports started.
Starting spans several round trips, so this polls on a timer instead of
blocking Emacs, and stops at DEADLINE rather than rescheduling forever."
  (let ((deadline (or deadline (time-add nil eca-archive-session-timeout))))
    (pcase (eca--session-status session)
      ('started (funcall callback session))
      ('stopped (user-error "ECA session stopped before it finished starting"))
      (_ (if (time-less-p deadline nil)
             (user-error "Timed out waiting for the ECA session to start")
           (run-at-time 0.3 nil #'eca-archive--when-started
                        session callback deadline))))))

(defun eca-archive--ensure-session (root callback)
  "Call CALLBACK with a started session for ROOT, starting one if needed.
Resuming usually happens after a restart, when nothing is running for
the chat's workspace yet; requiring the session to exist first would
push that chore back onto whoever wants to read an old chat."
  (if-let* ((session (eca-archive--session-for-root root)))
      (eca-archive--when-started session callback)
    ;; `eca-error' only messages, so aborting here has to signal
    (unless (and (stringp root) (file-directory-p root))
      (user-error "Archive records no usable workspace to start (%s)" root))
    (let ((session (eca-create-session
                    (list (file-name-as-directory (expand-file-name root))))))
      (eca-info "Starting ECA in %s to continue the chat..." root)
      (eca-process-start session
                         (lambda () (eca--initialize session))
                         (apply-partially #'eca--handle-message session))
      (eca-archive--when-started session callback))))

(defun eca-archive--attach-chat (session chat-id open-res from-buf)
  "Surface CHAT-ID's restored buffer in SESSION after OPEN-RES.
FROM-BUF is where the command ran, so an empty welcome buffer it left
behind can be reclaimed.  Mirrors the open path of `eca-chat-resume'."
  (let ((chat-buf (eca-get (eca--session-chats session) chat-id)))
    (setf (eca--session-last-chat-buffer session) chat-buf)
    (eca-chat--with-current-buffer chat-buf
      (eca-chat--apply-history-meta (plist-get open-res :meta))
      (eca-chat--refresh-load-older-control)
      (eca-chat--protect-non-prompt))
    (eca-chat-open session)
    (eca-chat--kill-empty-welcome-buffer session from-buf chat-buf)))

(defun eca-archive--gone (entry &optional detail)
  "Report that ENTRY's chat cannot be resumed, DETAIL saying why.
Resuming means the server replaying the messages the model actually
read.  When it no longer holds the chat there is nothing to resume, and
a new chat fed the transcript would be a different conversation wearing
its clothes - so this stops instead of pretending."
  ;; only ever called from a chat/open callback, i.e. inside the process
  ;; filter, where signalling buys nothing but an `error in process filter'
  ;; wrapper around the sentence worth reading.  Nothing may branch on this.
  (eca-error "Chat %s is no longer on the server%s; the archive %s is all that is left"
             (eca-archive--id-short entry)
             (if detail (format " (%s)" detail) "")
             (abbreviate-file-name (plist-get entry :file))))

(defun eca-archive--continue-in (session entry)
  "Reopen ENTRY's chat inside SESSION, in its own buffer."
  (eca-assert-session-running session)
  (let ((chat-id (plist-get entry :id))
        (from-buf (current-buffer)))
    (eca-api-request-async
     session
     :method "chat/open"
     :params (append (list :chatId chat-id)
                     (when eca-chat-history-page-size
                       (list :limit eca-chat-history-page-size)))
     :success-callback
     (lambda (open-res)
       (cond
        ((not (plist-get open-res :found?))
         (eca-archive--gone entry))
        ((not (buffer-live-p (eca-get (eca--session-chats session) chat-id)))
         (eca-archive--gone entry "the server found it but registered no buffer"))
        (t (eca-archive--attach-chat session chat-id open-res from-buf))))
     :error-callback
     (lambda (err) (eca-archive--gone entry (format "%s" err))))))

;;;###autoload
(defun eca-continue-from-file (&optional file)
  "Reopen an archived ECA chat, prompting for one unless FILE is given.
A file visited from `eca-archive-dir' is taken as the chat to reopen.
Starts the chat's workspace session when none is running, then restores
the chat itself - same chat, same history.  Says so and opens nothing
when the server no longer holds it."
  (interactive)
  (let* ((entry (cond
                 (file (eca-archive--entry file))
                 ((and buffer-file-name
                       (file-in-directory-p buffer-file-name
                                            (expand-file-name eca-archive-dir)))
                  (eca-archive--entry buffer-file-name))
                 (t (eca-archive-read-entry)))))
    (unless entry
      (user-error "No chat metadata found in %s" (or file buffer-file-name)))
    (eca-archive--ensure-session
     (plist-get entry :workspace)
     (lambda (session) (eca-archive--continue-in session entry)))))

;;;###autoload
(defun eca-mcp-restart-server (name &optional session)
  "Restart MCP server NAME: stop it, then start it once it has stopped.
Stop and start race (each runs on its own server thread), so the start
is deferred until the stop lands.  A one-shot timer reschedules itself
until then, so this never blocks Emacs and self-terminates after ~10s.
Interactively prompt for a server; SESSION defaults to the one hosting
NAME, else the current buffer's."
  (interactive
   (let ((session (eca-session)))
     (eca-assert-session-running session)
     (list (completing-read "Restart MCP server: "
                            (mapcar (lambda (s) (plist-get s :name))
                                    (eca-mcp-servers session))
                            nil t)
           session)))
  (let ((session (or session
                     (seq-find (lambda (s) (eca-get (eca--session-tool-servers s) name))
                               (eca-vals eca--sessions))
                     (eca-session))))
    (eca-assert-session-running session)
    (unless (eca-get (eca--session-tool-servers session) name)
      (user-error "No MCP server named %s" name))
    (eca-api-notify session :method "mcp/stopServer" :params (list :name name))
    (let ((deadline (+ (float-time) 10)))
      (cl-labels ((resume ()
                    (if (or (member (plist-get (eca-get (eca--session-tool-servers session) name)
                                               :status)
                                    '("stopped" "failed" "disabled"))
                            (< deadline (float-time)))
                        (progn
                          (eca-api-notify session :method "mcp/startServer" :params (list :name name))
                          (eca-info "Restarted MCP server %s" name))
                      (run-with-timer 0.1 nil #'resume))))
        (resume)))))
