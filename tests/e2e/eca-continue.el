;;; tests/e2e/eca-continue.el --- continuing an archived eca chat -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; The batch suite drives this flow against stand-ins: eca is not installed
;; in the sandbox, so the session struct, the request layer and the chat
;; commands are all faked there.  Fakes cannot notice that upstream renamed
;; something, and a keymap is never consulted at all.  Both gaps are this
;; scenario's job - it runs where eca is really installed and the leader map
;; is really built.

(require 'cl-lib)

(defun eca-continue-e2e--result (label ok got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label label :ok ok :got (format "%S" got) :want (format "%S" want)))

(defun eca-continue-e2e--equal (label got want)
  "Assert GOT equals WANT, reporting under LABEL."
  (eca-continue-e2e--result label (equal got want) got want))

;; The commands these keys reach are the whole point; a binding that silently
;; stops resolving puts us back where we started.  Resolved the way a
;; keypress resolves them - through the live keymaps, in the evil state the
;; keys are typed in - not by reading the keymap they were written into.
(defun eca-continue-e2e--bindings ()
  "Every key that reaches eca resolves to its command."
  (with-temp-buffer
    (fundamental-mode)
    (evil-normal-state)
    (mapcar (pcase-lambda (`(,keys . ,command))
              (eca-continue-e2e--equal
               (format "%s -> %s" keys command)
               (key-binding (kbd keys))
               command))
            '(("C-x g c" . eca)
              ("C-x g r" . eca-chat-resume)
              ("C-x g a" . eca-continue-from-file)
              ;; doom mirrors the C-x prefix onto the leader
              ("SPC x g c" . eca)
              ("SPC x g r" . eca-chat-resume)
              ("SPC x g a" . eca-continue-from-file)))))

(defun eca-continue-e2e--guards ()
  "The close guards are attached to the commands they protect."
  (list
   (eca-continue-e2e--result
    "kill query is overridden, so closing cannot delete server-side"
    (advice-member-p 'eca-chat-kill-keeps-server-copy-a 'eca-chat--kill-buffer-query)
    'installed 'installed)
   (eca-continue-e2e--result
    "eca-chat-delete asks first"
    (advice-member-p 'eca-chat-delete-confirm-a 'eca-chat-delete)
    'installed 'installed)))

;; Each of these is faked in tests/ai/eca-tests.el.  Asserting they exist,
;; here, is what stops those fakes from drifting into fiction.
(defun eca-continue-e2e--api ()
  "Every eca symbol the continue path leans on still exists."
  (let ((missing (seq-remove #'fboundp
                             '(eca-create-session
                               eca-process-start
                               eca-api-request-async
                               eca-assert-session-running
                               eca-vals
                               eca-get
                               eca--session-status
                               eca--session-chats
                               eca--session-workspace-folders
                               eca--session-last-chat-buffer
                               eca-chat--kill-buffer-query
                               eca-chat--apply-history-meta
                               eca-chat--kill-empty-welcome-buffer
                               eca-chat-delete
                               eca-chat-resume
                               eca-chat-open))))
    (list (eca-continue-e2e--equal "eca api the continue path depends on" missing nil)
          (eca-continue-e2e--equal "eca-chat-history-page-size is a real setting"
                                   (boundp 'eca-chat-history-page-size) t))))

;; The key reaches an autoloaded command, and everything else in the module
;; arrives with it.  Nothing else here would notice if that stopped being
;; true - the batch suite loads the file directly.
(defun eca-continue-e2e--autoload ()
  "The bound command loads the module that implements it."
  (let ((fn (symbol-function 'eca-continue-from-file)))
    (when (autoloadp fn)
      (autoload-do-load fn 'eca-continue-from-file)))
  (list (eca-continue-e2e--equal "eca-continue-from-file is a command"
                                 (commandp 'eca-continue-from-file) t)
        (eca-continue-e2e--equal "loading it brings in the archive helpers"
                                 (and (fboundp 'eca-archive-entries)
                                      (fboundp 'eca-archive-read-entry))
                                 t)))

(defun eca-continue-e2e--picker ()
  "The picker reads real archives and offers the newest first."
  (let ((dir (file-name-as-directory (make-temp-file "eca-e2e-archive" t)))
        (stamp (encode-time '(0 0 12 1 1 2020 nil -1 nil))))
    (unwind-protect
        (progn
          (cl-loop for (name meta) in
                   '(("proj_aaaaaaaa.md" (:id "aaaaaaaa" :workspace "/w/" :title "older"))
                     ("proj_bbbbbbbb.md" (:id "bbbbbbbb" :workspace "/w/" :title "newer")))
                   for minute from 0
                   do (let ((file (expand-file-name name dir)))
                        (with-temp-file file
                          (insert (format "<!-- eca: %S -->\n\nbody\n" meta)))
                        (set-file-times file (time-add stamp (* 60 minute)))))
          (let* ((eca-archive-dir dir)
                 (entries (eca-archive-entries))
                 (picked (cl-letf (((symbol-function 'completing-read)
                                    (lambda (_prompt collection &rest _)
                                      (car (all-completions "" collection)))))
                           (eca-archive-read-entry))))
            (list (eca-continue-e2e--equal
                   "archive entries come back newest first"
                   (mapcar (lambda (e) (plist-get e :title)) entries)
                   '("newer" "older"))
                  (eca-continue-e2e--equal
                   "the picker hands back the entry behind the label"
                   (plist-get picked :id) "bbbbbbbb"))))
      (delete-directory dir t))))

(defun eca-continue-e2e ()
  "Check the wiring that makes an archived chat reachable again."
  (require 'eca)
  (require 'eca-chat)
  (append (eca-continue-e2e--bindings)
          (eca-continue-e2e--autoload)
          (eca-continue-e2e--guards)
          (eca-continue-e2e--api)
          (eca-continue-e2e--picker)))

(add-to-list 'e2e-scenarios #'eca-continue-e2e)
