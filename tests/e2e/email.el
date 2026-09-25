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

(defun email-e2e--write-message (file from subject id &optional date references xref body)
  "Write a minimal RFC 822 message to FILE.
FROM, SUBJECT, ID, DATE, REFERENCES and XREF fill the headers; BODY
follows the body's first line."
  (with-temp-file file
    (insert "From: " from "\n"
            "To: to.plotnick@gmail.com\n"
            "Subject: " subject "\n"
            "Date: " (or date "Tue, 22 Sep 2026 10:00:00 +0000") "\n"
            "Message-ID: <" id "@fixture.example>\n"
            (if references (concat "References: " references "\n") "")
            (if xref (concat "Xref: " xref "\n") "")
            "\n"
            "body of " subject "\n"
            (or body ""))))

(defun email-e2e--write-alternative (file subject id plain html)
  "Write to FILE a multipart/alternative message with PLAIN and HTML parts.
SUBJECT and ID fill the headers."
  (with-temp-file file
    (insert "From: Carol <carol@example.com>\n"
            "To: to.plotnick@gmail.com\n"
            "Subject: " subject "\n"
            "Date: Tue, 22 Sep 2026 12:00:00 +0000\n"
            "Message-ID: <" id "@fixture.example>\n"
            "MIME-Version: 1.0\n"
            "Content-Type: multipart/alternative; boundary=\"alt\"\n"
            "\n"
            "--alt\n"
            "Content-Type: text/plain; charset=utf-8\n"
            "\n"
            plain "\n"
            "--alt\n"
            "Content-Type: text/html; charset=utf-8\n"
            "\n"
            html "\n"
            "--alt--\n")))

(defun email-e2e--article (subject)
  "Number of the article with SUBJECT in the current summary."
  (mail-header-number
   (seq-find (lambda (header) (equal (mail-header-subject header) subject))
             gnus-newsgroup-headers)))

(defun email-e2e--idle (seconds &optional done)
  "Sit idle for SECONDS, or until DONE returns non-nil; return DONE's value.
Idle timers never run inside a `sit-for' called from a running command.
An untimed `read-event' is idle, and a timer ends it."
  (let ((poll (run-at-time 0.05 0.05
                           (lambda ()
                             (when (and done (funcall done))
                               (throw 'email-e2e--idle t))))))
    (unwind-protect
        (catch 'email-e2e--idle
          (with-timeout (seconds nil)
            (while t (read-event))))
      (cancel-timer poll))
    (and done (funcall done))))

(defun email-e2e ()
  "Gnus reads the fixture maildir, replies, and shows a thread in one buffer."
  (let* ((root (expand-file-name "mail/" e2e-work-dir))
         (inbox (expand-file-name "inbox/" root))
         ;; the group a queued deletion is moved into has to exist when
         ;; Gnus starts, or nnmaildir refuses the article
         (trash (expand-file-name "trash/" root))
         ;; a label of its own, so the inbox holds what the flow counts
         (html (expand-file-name "html/" root))
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
    (dolist (dir (list inbox trash html))
      (dolist (sub '("cur" "new" "tmp"))
        (make-directory (expand-file-name sub dir) t)))
    ;; the sender's colors would paint white on white, and the paragraph
    ;; is longer than any terminal line
    (email-e2e--write-alternative
     (expand-file-name "cur/1700000005.6.fixture:2,S" html) "html letter" "html"
     "PLAIN alternative\n\n> quoted once\n> > quoted twice"
     (concat "<p>HTML alternative</p>"
             "<p>paragraph start " (string-join (make-list 60 "wrapping") " ")
             " paragraph end</p>"
             "<p style=\"color:#ffffff;background-color:#ffffff\">white on white</p>"
             "<blockquote><p>quoted once</p>"
             "<blockquote><p>quoted twice</p></blockquote></blockquote>"))
    (email-e2e--write-message (expand-file-name "new/1700000001.1.fixture" inbox)
                              "Someone <someone@example.com>" "fresh" "fresh")
    (email-e2e--write-message (expand-file-name "cur/1700000000.2.fixture:2,S" inbox)
                              "Other <other@example.com>" "seen" "seen")
    ;; one thread, oldest first, with only its last message unread
    (email-e2e--write-message (expand-file-name "cur/1700000002.3.fixture:2,S" inbox)
                              "Ann <ann@example.com>" "release plan" "plan"
                              "Mon, 21 Sep 2026 09:00:00 +0000")
    ;; two lines one level deep, which gnus-cite would paint too, and a
    ;; single line two levels deep, which it would skip
    (email-e2e--write-message (expand-file-name "cur/1700000003.4.fixture:2,S" inbox)
                              "Bob <bob@example.com>" "Re: release plan (Bob)" "plan-bob"
                              "Mon, 21 Sep 2026 10:00:00 +0000"
                              "<plan@fixture.example>" nil
                              (concat "> the plan, first line\n"
                                      "> the plan, second line\n"
                                      "> > the draft before it\n"))
    ;; the last one carries the Xref header every gmane article has
    (email-e2e--write-message (expand-file-name "new/1700000004.5.fixture" inbox)
                              "Ann <ann@example.com>" "Re: release plan (Ann)" "plan-ann"
                              "Mon, 21 Sep 2026 11:00:00 +0000"
                              "<plan@fixture.example> <plan-bob@fixture.example>"
                              "news.gmane.io gmane.emacs.devel:346572")
    (cl-flet ((record (label ok &rest kv)
                (push (append (list :label (format "email: %s" label) :ok ok) kv)
                      results))
              (open-subjects ()
                (mapcar (lambda (message)
                          (mail-header-subject (mail-thread-message-header message)))
                        (seq-filter #'mail-thread-message-open-p mail-thread-messages)))
              (subject-at-point ()
                (mail-header-subject
                 (mail-thread-message-header (mail-thread-message-at-point))))
              (thread-text ()
                (with-current-buffer mail-thread-buffer-name
                  (buffer-substring-no-properties (point-min) (point-max))))
              (unread-p (article)
                (with-current-buffer "*Summary nnmaildir+gmail:inbox*"
                  (and (memq article gnus-newsgroup-unreads) t)))
              (messages-in (dir)
                (mapcan (lambda (sub)
                          (directory-files (expand-file-name sub dir) t "\\`[^.]"))
                        '("cur" "new")))
              (message-ids (files)
                (mapcar (lambda (file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (mail-fetch-field "Message-ID")))
                        files))
              (glyph-at-point ()
                (char-after (line-beginning-position)))
              ;; what redisplay draws: an overlay face wins over the text's
              (quote-faces ()
                (mapcar (lambda (text)
                          (save-excursion
                            (goto-char (point-min))
                            (when (search-forward text nil t)
                              (list (get-char-property (line-beginning-position) 'face)
                                    (get-char-property (1- (line-end-position)) 'face)))))
                        '("the plan, first line" "the plan, second line"
                          "the draft before it")))
              (cite-overlays ()
                (seq-filter (lambda (overlay)
                              (string-prefix-p "gnus-cite"
                                               (format "%s" (overlay-get overlay 'face))))
                            (overlays-in (point-min) (point-max))))
              ;; each quoted line of the HTML letter with the faces drawn
              ;; at its start and end
              (html-quotes ()
                (mapcar (lambda (text)
                          (save-excursion
                            (goto-char (point-min))
                            (when (search-forward text nil t)
                              (list (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position))
                                    (get-char-property (line-beginning-position) 'face)
                                    (get-char-property (1- (line-end-position)) 'face)))))
                        '("quoted once" "quoted twice")))
              (sender-colors ()
                (save-excursion
                  (goto-char (point-min))
                  (if (not (search-forward "white on white" nil t))
                      :missing
                    (let ((face (get-char-property (1- (point)) 'face)))
                      (seq-filter (lambda (spec)
                                    (and (keywordp (car-safe spec))
                                         (or (plist-get spec :foreground)
                                             (plist-get spec :background))))
                                  (if (keywordp (car-safe face)) (list face) (ensure-list face)))))))
              ;; buffer lines and screen lines the long paragraph takes
              (paragraph-lines ()
                (save-excursion
                  (goto-char (point-min))
                  (if (not (search-forward "paragraph start" nil t))
                      :missing
                    (let ((start (line-beginning-position)))
                      (search-forward "paragraph end")
                      (list (count-lines start (point))
                            (count-screen-lines start (point))))))))
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
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                ;; gR belongs to evil-collection until the module takes
                ;; it back, and it used to ask nnmaildir for a
                ;; server-wide scan - every label in the store
                (let (called)
                  (cl-letf (((symbol-function 'refresh-mail-groups)
                             (lambda () (interactive) (setq called 'routine-groups)))
                            ((symbol-function 'gnus-group-get-new-news)
                             (lambda (&rest _) (interactive "P") (setq called 'whole-server))))
                    (execute-kbd-macro (kbd "gR")))
                  (record "gR refreshes the routine groups, not the whole server"
                          (eq called 'routine-groups)
                          :got (format "%s" called)))
                ;; RET from the group buffer is the path the `display'
                ;; parameter governs; without it only unread mail shows
                (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                (execute-kbd-macro (kbd "RET"))
                (record "RET on the group line shows read mail too"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (= 5 (length gnus-newsgroup-headers)))
                        :got (format "%s, %d headers" major-mode
                                     (length gnus-newsgroup-headers)))
                ;; the article buffer shows a shorter message without
                ;; Xref, the way it does after reading mail; the render
                ;; must not look the thread's Xref up in it
                (gnus-summary-goto-subject (email-e2e--article "fresh"))
                (gnus-summary-select-article)
                (setq ann (email-e2e--article "Re: release plan (Ann)"))
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
                (record "RET renders the entry message and leaves the unread one to the fill"
                        (and (equal (open-subjects) '("Re: release plan (Bob)"))
                             (not (string-match-p "body of Re: release plan (Ann)"
                                                  (thread-text))))
                        :got (format "%S open" (open-subjects)))
                (record "a folded message costs a line, not a render"
                        (not (string-match-p "body of release plan" (thread-text)))
                        :got (format "%d chars" (buffer-size)))
                (redisplay t)
                (record "the thread view faces each quoted line by depth, markers included"
                        (equal (quote-faces)
                               '((message-cited-text-1 message-cited-text-1)
                                 (message-cited-text-1 message-cited-text-1)
                                 (message-cited-text-2 message-cited-text-2)))
                        :got (format "%S" (quote-faces)))
                ;; q before Emacs goes idle, so the fill never gets a turn
                (execute-kbd-macro "q")
                (email-e2e--idle 0.5)
                (record "q stops the fill: the unread message stays unrendered and unread"
                        (and (not (string-match-p "body of Re: release plan (Ann)"
                                                  (thread-text)))
                             (unread-p ann))
                        :got (format "unread %S, %d chars" (unread-p ann)
                                     (length (thread-text))))
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro (kbd "RET"))
                (execute-kbd-macro (kbd "C-j"))
                (record "C-j reaches the unread message before its body arrives"
                        (and (equal (subject-at-point) "Re: release plan (Ann)")
                             (not (string-match-p "body of Re: release plan (Ann)"
                                                  (thread-text))))
                        :got (subject-at-point))
                (email-e2e--idle 5 (lambda ()
                                     (string-match-p "body of Re: release plan (Ann)"
                                                     (thread-text))))
                (record "the unread message renders once Emacs is idle"
                        (equal (open-subjects)
                               '("Re: release plan (Bob)" "Re: release plan (Ann)"))
                        :got (format "%S open" (open-subjects)))
                (record "the message with an Xref header renders while another is shown"
                        (string-match-p "body of Re: release plan (Ann)" (thread-text))
                        :got (format "%d chars" (length (thread-text))))
                (record "the fill marks the message it rendered read"
                        (not (unread-p ann))
                        :got (format "unread %S" (unread-p ann)))
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
                        :got (format "%s, %s" major-mode gnus-article-current))
                (redisplay t)
                (record "the article buffer faces the quotes the same way, with no gnus-cite overlay"
                        (and (equal (quote-faces)
                                    '((message-cited-text-1 message-cited-text-1)
                                      (message-cited-text-1 message-cited-text-1)
                                      (message-cited-text-2 message-cited-text-2)))
                             (null (cite-overlays)))
                        :got (format "%S, %d gnus-cite overlays"
                                     (quote-faces) (length (cite-overlays))))
                ;; replying cites every line once more, and message-mode
                ;; paints it with the same faces by the same depth rule
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro "R")
                (setq reply (current-buffer))
                (font-lock-ensure)
                (record "a cited reply paints the quotes a level deeper with the same faces"
                        (and (derived-mode-p 'message-mode)
                             (equal (quote-faces)
                                    '((message-cited-text-2 message-cited-text-2)
                                      (message-cited-text-2 message-cited-text-2)
                                      (message-cited-text-3 message-cited-text-3))))
                        :got (format "%s, %S" major-mode (quote-faces)))
                (set-buffer-modified-p nil)
                (kill-buffer reply)
                (setq reply nil)
                ;; deferred deletion and archive: nothing reaches the
                ;; store until x
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "seen"))
                (execute-kbd-macro "d")
                (record "d queues the message at point for the trash and draws D"
                        (and (eq (alist-get (email-e2e--article "seen") mail-marks) 'delete)
                             (eq (glyph-at-point) ?D)
                             (eq (car-safe (get-text-property (line-beginning-position) 'face))
                                 'dired-flagged)
                             (= 5 (length (messages-in inbox))))
                        :got (format "%S, line starts with %c, face %S, %d files"
                                     mail-marks (glyph-at-point)
                                     (get-text-property (line-beginning-position) 'face)
                                     (length (messages-in inbox))))
                (execute-kbd-macro "u")
                (record "u takes it back and clears the column"
                        (and (null mail-marks) (eq (glyph-at-point) ?\s))
                        :got (format "%S, line starts with %c" mail-marks (glyph-at-point)))
                (execute-kbd-macro "d")
                (gnus-summary-goto-subject (email-e2e--article "release plan"))
                (execute-kbd-macro "A")
                (record "A queues the whole thread at point for archive"
                        (equal (mail-marked-articles 'archive)
                               (sort (mapcar #'email-e2e--article
                                             '("release plan" "Re: release plan (Bob)"
                                               "Re: release plan (Ann)"))
                                     #'<))
                        :got (format "%S" mail-marks))
                (execute-kbd-macro "x")
                (record "x moves the queued deletion into the trash maildir"
                        (equal (message-ids (messages-in trash)) '("<seen@fixture.example>"))
                        :got (format "%S" (message-ids (messages-in trash))))
                (record "x deletes the archived thread's files from the label"
                        (equal (message-ids (messages-in inbox)) '("<fresh@fixture.example>"))
                        :got (format "%S" (message-ids (messages-in inbox))))
                (record "the executed lines leave the summary"
                        (and (null mail-marks)
                             (= 1 (count-lines (point-min) (point-max)))
                             (equal (mail-header-subject
                                     (gnus-summary-article-header
                                      (progn (goto-char (point-min))
                                             (gnus-summary-article-number))))
                                    "fresh"))
                        :got (format "%S, %d lines" mail-marks
                                     (count-lines (point-min) (point-max))))
                ;; an HTML letter with a plain alternative: both views
                ;; show the HTML part, quoted like plain mail and wrapped
                ;; at the window edge instead of cut there
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-exit-no-update)
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-jump-to-group "nnmaildir+gmail:html")
                (execute-kbd-macro (kbd "RET"))
                (gnus-summary-goto-subject (email-e2e--article "html letter"))
                (execute-kbd-macro (kbd "RET"))
                (redisplay t)
                (record "the thread view shows the HTML part, its quote levels drawn as > lines"
                        (and (derived-mode-p 'mail-thread-mode)
                             (string-match-p "HTML alternative" (thread-text))
                             (not (string-match-p "PLAIN alternative" (thread-text)))
                             (equal (html-quotes)
                                    '(("> quoted once" message-cited-text-1 message-cited-text-1)
                                      ("> > quoted twice" message-cited-text-2 message-cited-text-2))))
                        :got (format "%s, %S" major-mode (html-quotes)))
                (record "the thread view drops the sender's colors"
                        (null (sender-colors))
                        :got (format "%S" (sender-colors)))
                (record "the thread view wraps a long HTML paragraph instead of cutting it"
                        (pcase (paragraph-lines) (`(1 ,screen) (< 1 screen)))
                        :got (format "%S buffer and screen lines" (paragraph-lines)))
                (execute-kbd-macro (kbd "RET"))
                (redisplay t)
                (record "the article buffer shows the HTML part, its quote levels drawn as > lines"
                        (and (derived-mode-p 'gnus-article-mode)
                             (string-match-p "HTML alternative" (buffer-string))
                             (not (string-match-p "PLAIN alternative" (buffer-string)))
                             (equal (html-quotes)
                                    '(("> quoted once" message-cited-text-1 message-cited-text-1)
                                      ("> > quoted twice" message-cited-text-2 message-cited-text-2))))
                        :got (format "%s, %S" major-mode (html-quotes)))
                (record "the article buffer drops the sender's colors"
                        (null (sender-colors))
                        :got (format "%S" (sender-colors)))
                (record "the article buffer wraps a long HTML paragraph instead of cutting it"
                        (pcase (paragraph-lines) (`(1 ,screen) (< 1 screen)))
                        :got (format "%S buffer and screen lines, truncate-lines %S"
                                     (paragraph-lines) truncate-lines)))
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
          (dolist (name '("*Summary nnmaildir+gmail:inbox*" "*Summary nnmaildir+gmail:html*"))
            (when-let* ((summary (get-buffer name)))
              (with-current-buffer summary
                (gnus-summary-exit-no-update))))
          (with-current-buffer gnus-group-buffer
            (gnus-group-exit)))
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'email-e2e)
