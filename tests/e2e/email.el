;;; tests/e2e/email.el --- Gnus over a fixture maildir -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; nnmaildir is pure elisp, so a maildir holding one unread and one read
;; message boots Gnus for real: the group buffer, the summary buffer with
;; its flag mapping, and a reply composed through the posting styles.
;; The fixture server carries the real server name, so the group
;; parameters, the inbox group and the Gcc target of the module config
;; are the ones under test.  The reply itself goes through
;; evil-collection's summary binding by a real keypress.

(require 'cl-lib)
;; the let-bound Gnus variables must already be special when the scenario
;; is defined, or `gnus' loading gnus-start trips over lexical bindings
(require 'gnus)
(require 'gnus-start)
(require 'gnus-sum)

(defun email-e2e--write-message (file from subject id)
  "Write a minimal RFC 822 message to FILE with FROM, SUBJECT and message ID."
  (with-temp-file file
    (insert "From: " from "\n"
            "To: to.plotnick@gmail.com\n"
            "Subject: " subject "\n"
            "Date: Tue, 22 Sep 2026 10:00:00 +0000\n"
            "Message-ID: <" id "@fixture.example>\n"
            "\n"
            "body of " subject "\n")))

(defun email-e2e ()
  "Gnus reads the fixture maildir and replies as agzam.ibragimov."
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
    (cl-flet ((record (label ok &rest kv)
                (push (append (list :label (format "email: %s" label) :ok ok) kv)
                      results)))
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
                        (and (= 2 (length gnus-newsgroup-headers))
                             (= 1 (length gnus-newsgroup-unreads)))
                        :got (format "%d headers, %d unread"
                                     (length gnus-newsgroup-headers)
                                     (length gnus-newsgroup-unreads)))
                (gnus-summary-goto-article (car gnus-newsgroup-unreads))
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
                             (= 2 (length gnus-newsgroup-headers)))
                        :got (format "%s, %d headers" major-mode
                                     (length gnus-newsgroup-headers))))
            (error (record "flow signalled" nil :err e)))
        (when (buffer-live-p reply)
          (with-current-buffer reply
            (set-buffer-modified-p nil))
          (kill-buffer reply))
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
