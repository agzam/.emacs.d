;;; tests/e2e/email-compose.el --- recipient completion in a message buffer -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup.  Typing in a recipient header reaches the address list through
;; message.el's completion alist, completion-preview's overlay, evil's
;; insert-state keys and vertico's minibuffer, which only real keys cross.

(require 'cl-lib)

(defvar mail-address-program)
(defvar mail-address-file)
(defvar mail-addresses)
(defvar mail-addresses-read-at)
(defvar mail-address-processes)
(defvar completion-preview--overlay)
(defvar vertico-repeat-history)

(defconst email-compose-e2e-runs
  '(("recipients" "all"
     ((:name "Sandy Guan" :address "sguan@splash.example"
             :name-addr "Sandy Guan <sguan@splash.example>" :count 19)
      (:name "Erin Brock" :address "erin@brock.example"
             :name-addr "Erin Brock <erin@brock.example>" :count 3)
      (:name "" :address "rhill@old.example" :name-addr "rhill@old.example" :count 139)
      (:name "Eric Natale" :address "eric@bunk1.example"
             :name-addr "Eric Natale <eric@bunk1.example>" :count 50)
      (:name "Douglas, Matthew" :address "madougla@teksys.example"
             :name-addr "\"Douglas, Matthew\" <madougla@teksys.example>" :count 2)))
    ("recipients" "3years"
     ((:name "Sandy Guan" :address "sguan@splash.example"
             :name-addr "Sandy Guan <sguan@splash.example>" :count 19)
      (:name "Erin Brock" :address "erin@brock.example"
             :name-addr "Erin Brock <erin@brock.example>" :count 3)))
    ("recipients" "1year"
     ((:name "Sandy Guan" :address "sguan@splash.example"
             :name-addr "Sandy Guan <sguan@splash.example>" :count 10)
      (:name "Erin Brock" :address "erin@brock.example"
             :name-addr "Erin Brock <erin@brock.example>" :count 3)))
    ("recipients" "30days"
     ((:name "Sandy Guan" :address "sguan@splash.example"
             :name-addr "Sandy Guan <sguan@splash.example>" :count 2)
      (:name "Erin Brock" :address "erin@brock.example"
             :name-addr "Erin Brock <erin@brock.example>" :count 1)))
    ("sender" "all"
     ((:name "Panther Creek" :address "pchs@school.example"
             :name-addr "Panther Creek <pchs@school.example>" :count 443)
      (:name "Chase" :address "no.reply.alerts@chase.example"
             :name-addr "Chase <no.reply.alerts@chase.example>" :count 200)
      (:name "Eric Natale" :address "eric@bunk1.example"
             :name-addr "Eric Natale <eric@bunk1.example>" :count 30)))
    ("sender" "3years" nil)
    ("sender" "1year" nil)
    ("sender" "30days" nil))
  "What the stand-in notmuch prints per output and date window.")

(defconst email-compose-e2e-ranked
  '("Sandy Guan <sguan@splash.example>" "Erin Brock <erin@brock.example>" "rhill@old.example"
    "Eric Natale <eric@bunk1.example>" "\"Douglas, Matthew\" <madougla@teksys.example>"
    "Panther Creek <pchs@school.example>")
  "`email-compose-e2e-runs' ranked: whom the account wrote to by frecency, then the rest.")

(defun email-compose-e2e ()
  "Recipients complete from the notmuch addresses in a real message buffer."
  ;; the autoload first, so the let below binds the module's own variables
  (autoload-do-load (symbol-function 'prepare-mail-addresses-h) 'prepare-mail-addresses-h)
  (let* ((dir (file-name-as-directory (expand-file-name "addresses" e2e-work-dir)))
         (log (expand-file-name "args" dir))
         (mail-address-program (expand-file-name "notmuch" dir))
         (mail-address-file (expand-file-name "mail-addresses.eld" dir))
         (mail-addresses nil)
         (mail-addresses-read-at nil)
         (mail-address-processes nil)
         ;; any file, so the first-buffer hooks have run, as in a session
         (scratch (find-file-noselect (expand-file-name "notes.txt" e2e-work-dir)))
         (results '())
         message)
    (make-directory dir t)
    (pcase-dolist (`(,output ,window ,entries) email-compose-e2e-runs)
      (with-temp-file (expand-file-name (format "%s-%s" output window) dir)
        (prin1 entries (current-buffer))))
    (with-temp-file mail-address-program
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"$*\" >> '" log "'\n"
              "case \"$*\" in *--output=recipients*) output=recipients;; *) output=sender;; esac\n"
              "case \"$*\" in *date:30days*) window=30days;; *date:1year*) window=1year;;"
              " *date:3years*) window=3years;; *) window=all;; esac\n"
              "cat '" dir "'\"$output-$window\"\n"))
    (set-file-modes mail-address-program #o755)
    (cl-flet* ((record (label ok &rest kv)
                 (push (append (list :label (format "email compose: %s" label) :ok ok) kv)
                       results))
               (read-done ()
                 (with-timeout (10 nil)
                   (while (seq-some (lambda (process) (buffer-live-p (process-buffer process)))
                                    mail-address-processes)
                     (accept-process-output nil 0.05)))
                 t)
               (asked ()
                 (if (file-exists-p log)
                     (with-temp-buffer
                       (insert-file-contents log)
                       (length (split-string (buffer-string) "\n" t)))
                   0))
               (compose ()
                 (message-mail)
                 (delete-other-windows)
                 (setq message (current-buffer)))
               (discard ()
                 (when (buffer-live-p message)
                   (with-current-buffer message
                     (set-buffer-modified-p nil))
                   (kill-buffer message)))
               ;; the text the inline preview shows after point, or nil
               (preview ()
                 (and (bound-and-true-p completion-preview-active-mode)
                      completion-preview--overlay
                      (substring-no-properties
                       (overlay-get completion-preview--overlay 'after-string))))
               ;; KEYS in one macro, as typed: every macro starts by running
               ;; post-command-hook, which hides the preview.  Returns
               ;; (COMMAND . PREVIEW) after each key.  A prompt left open
               ;; would wait for a real key
               (press (keys)
                 (let* ((buffer (current-buffer))
                        (states nil)
                        (note (lambda () (push (cons this-command (preview)) states)))
                        (guard (run-at-time 5 nil (lambda ()
                                                    (when (active-minibuffer-window)
                                                      (abort-recursive-edit))))))
                   (add-hook 'post-command-hook note 90 t)
                   (unwind-protect
                       (condition-case nil
                           (execute-kbd-macro (kbd keys))
                         (quit (record (format "%s left no prompt open" keys) nil)))
                     (with-current-buffer buffer
                       (remove-hook 'post-command-hook note t))
                     (cancel-timer guard))
                   (nreverse states)))
               ;; what the preview showed right after the last COMMAND
               (shown (states command)
                 (cdr (seq-find (lambda (state) (eq (car state) command)) (reverse states))))
               (header (name)
                 (save-excursion
                   (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field name)))))
      (unwind-protect
          (condition-case e
              (progn
                (compose)
                ;; the stand-in may exit before the command returns, but its
                ;; output is read only once Emacs waits
                (let ((started (length mail-address-processes))
                      (waiting (null mail-addresses)))
                  (read-done)
                  (record "a new message reads the addresses from notmuch without waiting for them"
                          (and (= 8 started) waiting
                               (equal mail-addresses email-compose-e2e-ranked)
                               (= 8 (asked))
                               (with-temp-buffer
                                 (insert-file-contents mail-address-file)
                                 (equal (read (current-buffer)) email-compose-e2e-ranked)))
                          :got (format "started %S, waiting %S, then %S after %d runs"
                                       started waiting mail-addresses (asked))))
                (record "the message starts in insert state with the preview on, at To"
                        (and (eq evil-state 'insert)
                             (bound-and-true-p completion-preview-mode)
                             (looking-back "^To: " (line-beginning-position)))
                        :got (format "%S %S %S" evil-state
                                     (bound-and-true-p completion-preview-mode)
                                     (buffer-substring (line-beginning-position) (point))))
                (let ((states (press "eri M-/ M-l")))
                  (record "three letters of a name preview the match of the highest frecency"
                          (equal (shown states 'self-insert-command) "n Brock <erin@brock.example>")
                          :got (format "%S" states))
                  (record "M-/ previews the next match"
                          (equal (shown states 'completion-preview-next-candidate)
                                 "c Natale <eric@bunk1.example>")
                          :got (format "%S" states))
                  (record "M-l inserts the previewed match as Name <address>, case restored"
                          (equal (header "To") "Eric Natale <eric@bunk1.example>")
                          :got (header "To")))
                (let ((shown (shown (press ", SPC gua M-l") 'self-insert-command)))
                  (record "a later word of the name previews and completes too"
                          (and (equal shown "n <sguan@splash.example>")
                               (equal (header "To")
                                      (concat "Eric Natale <eric@bunk1.example>, "
                                              "Sandy Guan <sguan@splash.example>")))
                          :got (format "%S, then %S" shown (header "To"))))
                (let ((shown (shown (press ", SPC mado M-l") 'self-insert-command)))
                  (record "the start of an address previews it, and completes to Name <address>"
                          (and (equal shown "ugla@teksys.example")
                               (string-suffix-p ", \"Douglas, Matthew\" <madougla@teksys.example>"
                                                (header "To")))
                          :got (format "%S, then %S" shown (header "To"))))
                ;; RET answers the list TAB opens.  History would rank the
                ;; list, and savehist would keep the pick in the real file
                (let ((minibuffer-history nil)
                      (vertico-repeat-history nil))
                  (press "C-c C-f C-c ex TAB RET"))
                (record "TAB lists every match of any part, best first, and RET takes it"
                        (equal (header "Cc") "Sandy Guan <sguan@splash.example>")
                        :got (header "Cc"))
                (let ((shown (shown (press "C-c C-f C-b school TAB") 'self-insert-command)))
                  (record "a word inside an address previews nothing, and TAB completes its one match"
                          (and (null shown)
                               (equal (header "Bcc") "Panther Creek <pchs@school.example>"))
                          :got (format "%S, then %S" shown (header "Bcc"))))
                (let ((subject (shown (press "C-c C-f C-s eri") 'self-insert-command))
                      (body (shown (press "C-c C-b eri") 'self-insert-command)))
                  (record "the subject and the body offer no addresses"
                          (and (equal (header "Subject") "eri")
                               (not (seq-some (lambda (shown)
                                                (and shown (string-match-p "[<@]" shown)))
                                              (list subject body))))
                          :got (format "subject %S, body %S" subject body)))
                (discard)
                ;; a new session: the saved list, until notmuch answers again
                (setq mail-addresses nil
                      mail-addresses-read-at nil)
                (compose)
                (let ((at-once mail-addresses))
                  (read-done)
                  (record "the first message of a session completes from the saved list at once"
                          (and (equal at-once email-compose-e2e-ranked) (= 16 (asked)))
                          :got (format "%S, %d runs" at-once (asked))))
                (discard)
                (compose)
                (read-done)
                (record "a message within the hour asks notmuch nothing"
                        (= 16 (asked))
                        :got (format "%d runs" (asked))))
            (error (record "flow signalled" nil :err e)))
        (discard)
        (dolist (process mail-address-processes)
          (when (process-live-p process)
            (delete-process process)))
        (when (active-minibuffer-window)
          (abort-recursive-edit))
        (when (buffer-live-p scratch)
          (kill-buffer scratch))
        (delete-other-windows)
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'email-compose-e2e)
