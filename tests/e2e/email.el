;;; tests/e2e/email.el --- Gnus over a fixture maildir -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; nnmaildir is pure elisp, so a maildir holding read and unread messages
;; boots Gnus for real: the group buffer, the summary buffer with its flag
;; mapping, a reply composed through the posting styles, and the one-buffer
;; thread view over a thread chained by References.  The fixture server
;; carries the real server name, so the group parameters, the inbox group
;; and the Gcc target of the module config are the ones under test.  Every
;; action goes through a real keypress.

(require 'cl-lib)
;; the let-bound Gnus variables must already be special when the scenario
;; is defined, or `gnus' loading gnus-start trips over lexical bindings
(require 'gnus)
(require 'gnus-start)
(require 'gnus-sum)

(defun email-e2e--write-message (file from subject id &optional date references)
  "Write a minimal RFC 822 message to FILE.
FROM, SUBJECT, ID, DATE and REFERENCES fill the headers."
  (with-temp-file file
    (insert "From: " from "\n"
            "To: to.plotnick@gmail.com\n"
            "Subject: " subject "\n"
            "Date: " (or date "Tue, 22 Sep 2026 10:00:00 +0000") "\n"
            "Message-ID: <" id "@fixture.example>\n"
            (if references (concat "References: " references "\n") "")
            "\n"
            "body of " subject "\n")))

(defun email-e2e--article (subject)
  "Number of the article with SUBJECT in the current summary."
  (mail-header-number
   (seq-find (lambda (header) (equal (mail-header-subject header) subject))
             gnus-newsgroup-headers)))

(defun email-e2e ()
  "Gnus reads the fixture maildir, replies, and shows a thread in one buffer."
  (let* ((root (expand-file-name "mail/" e2e-work-dir))
         (inbox (expand-file-name "inbox/" root))
         (results '())
         ;; gnus-started-hook subscribes every group under this root;
         ;; mail-groups would add gmane, and CI has no news server
         (gmail-maildir root)
         (mail-groups nil)
         (gnus-secondary-select-methods
          `((nnmaildir "gmail" (directory ,root) (get-new-mail nil))))
         (gnus-startup-file (expand-file-name "newsrc" e2e-work-dir))
         (gnus-init-file (expand-file-name "gnus-init" e2e-work-dir))
         (gnus-directory (expand-file-name "news/" e2e-work-dir))
         (gnus-interactive-exit nil)
         (gnus-expert-user t)
         reply)
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name sub inbox) t))
    (email-e2e--write-message (expand-file-name "new/1700000001.1.fixture" inbox)
                              "Someone <someone@example.com>" "fresh" "fresh")
    (email-e2e--write-message (expand-file-name "cur/1700000000.2.fixture:2,S" inbox)
                              "Other <other@example.com>" "seen" "seen")
    ;; one thread, oldest first, with only its last message unread
    (email-e2e--write-message (expand-file-name "cur/1700000002.3.fixture:2,S" inbox)
                              "Ann <ann@example.com>" "release plan" "plan"
                              "Mon, 21 Sep 2026 09:00:00 +0000")
    (email-e2e--write-message (expand-file-name "cur/1700000003.4.fixture:2,S" inbox)
                              "Bob <bob@example.com>" "Re: release plan (Bob)" "plan-bob"
                              "Mon, 21 Sep 2026 10:00:00 +0000"
                              "<plan@fixture.example>")
    (email-e2e--write-message (expand-file-name "new/1700000004.5.fixture" inbox)
                              "Ann <ann@example.com>" "Re: release plan (Ann)" "plan-ann"
                              "Mon, 21 Sep 2026 11:00:00 +0000"
                              "<plan@fixture.example> <plan-bob@fixture.example>")
    (cl-flet ((record (label ok &rest kv)
                (push (append (list :label (format "email: %s" label) :ok ok) kv)
                      results))
              (open-subjects ()
                (mapcar (lambda (message)
                          (mail-header-subject (mail-thread-message-header message)))
                        (seq-filter #'mail-thread-message-open-p mail-thread-messages)))
              (subject-at-point ()
                (mail-header-subject
                 (mail-thread-message-header (mail-thread-message-at-point)))))
      (unwind-protect
          (condition-case e
              (progn
                (discard-input)
                (open-mail-inbox)
                (record "opens the inbox summary"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal gnus-newsgroup-name "nnmaildir+gmail:inbox"))
                        :got (format "%s in %s" major-mode gnus-newsgroup-name))
                (record "maps maildir flags to unread and read"
                        (and (= 5 (length gnus-newsgroup-headers))
                             (= 2 (length gnus-newsgroup-unreads)))
                        :got (format "%d headers, %d unread"
                                     (length gnus-newsgroup-headers)
                                     (length gnus-newsgroup-unreads)))
                (gnus-summary-goto-subject (email-e2e--article "fresh"))
                (execute-kbd-macro "r")
                (setq reply (current-buffer))
                (record "r composes a reply in message-mode"
                        (derived-mode-p 'message-mode)
                        :got (format "%s" major-mode))
                (record "the reply is sent as agzam.ibragimov"
                        (let ((from (message-fetch-field "From")))
                          (and from (string-match-p "agzam\\.ibragimov@gmail\\.com" from)))
                        :got (message-fetch-field "From"))
                (record "the reply answers the original sender"
                        (equal (message-fetch-field "To") "Someone <someone@example.com>")
                        :got (message-fetch-field "To"))
                (record "the reply files a copy into the synced sent group"
                        (equal (message-fetch-field "Gcc") "nnmaildir+gmail:sent")
                        :got (message-fetch-field "Gcc"))
                (set-buffer-modified-p nil)
                (kill-buffer reply)
                (setq reply nil)
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-exit-no-update)
                ;; RET from the group buffer is the path the `display'
                ;; parameter governs; without it only unread mail shows
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                (execute-kbd-macro (kbd "RET"))
                (record "RET on the group line shows read mail too"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (= 5 (length gnus-newsgroup-headers)))
                        :got (format "%s, %d headers" major-mode
                                     (length gnus-newsgroup-headers)))
                ;; the thread view, entered from the middle message
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro (kbd "RET"))
                (record "RET in the summary opens the whole thread in one buffer"
                        (and (derived-mode-p 'mail-thread-mode)
                             (= 3 (length mail-thread-messages)))
                        :got (format "%s, %d messages" major-mode
                                     (length mail-thread-messages)))
                (record "the thread opens on the message the summary was on"
                        (equal (subject-at-point) "Re: release plan (Bob)")
                        :got (subject-at-point))
                (record "it unfolds the entry message and the unread one only"
                        (equal (open-subjects)
                               '("Re: release plan (Bob)" "Re: release plan (Ann)"))
                        :got (format "%S" (open-subjects)))
                (record "a folded message costs a line, not a render"
                        (not (string-match-p "body of release plan"
                                             (buffer-substring-no-properties
                                              (point-min) (point-max))))
                        :got (format "%d chars" (buffer-size)))
                (execute-kbd-macro (kbd "C-j"))
                (record "C-j moves to the next message"
                        (equal (subject-at-point) "Re: release plan (Ann)")
                        :got (subject-at-point))
                (execute-kbd-macro (kbd "C-k"))
                (execute-kbd-macro (kbd "C-k"))
                (record "C-k moves back to the first message"
                        (equal (subject-at-point) "release plan")
                        :got (subject-at-point))
                (execute-kbd-macro (kbd "TAB"))
                (record "TAB renders the message it unfolds"
                        (and (string-match-p "body of release plan"
                                             (buffer-substring-no-properties
                                              (point-min) (point-max)))
                             (equal (subject-at-point) "release plan"))
                        :got (format "%S open" (open-subjects)))
                (execute-kbd-macro "q")
                (record "q returns to the summary it came from"
                        (eq (window-buffer (selected-window))
                            (get-buffer "*Summary nnmaildir+gmail:inbox*"))
                        :got (buffer-name (window-buffer (selected-window))))
                ;; the thread buffer keeps no MIME handles, so the
                ;; article view is where an attachment is opened
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro (kbd "RET"))
                (execute-kbd-macro (kbd "RET"))
                (record "RET in the thread reads the message in the article buffer"
                        (and (derived-mode-p 'gnus-article-mode)
                             (string-match-p "body of Re: release plan (Bob)"
                                             (buffer-substring-no-properties
                                              (point-min) (point-max))))
                        :got (format "%s, %s" major-mode gnus-article-current)))
            (error (record "flow signalled" nil :err e)))
        (when (buffer-live-p reply)
          (with-current-buffer reply
            (set-buffer-modified-p nil))
          (kill-buffer reply))
        ;; the thread view loads with its first use, so its buffer name
        ;; is void when the flow died before reaching it
        (when-let* ((name (bound-and-true-p mail-thread-buffer-name))
                    (thread (get-buffer name)))
          (kill-buffer thread))
        (when (gnus-alive-p)
          ;; a live summary makes gnus-group-exit ask whether to update it
          (when-let* ((summary (get-buffer "*Summary nnmaildir+gmail:inbox*")))
            (with-current-buffer summary
              (gnus-summary-exit-no-update)))
          (with-current-buffer gnus-group-buffer
            (gnus-group-exit)))
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'email-e2e)
