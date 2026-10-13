;;; tests/email/delay-tests.el --- delayed send specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; compose.el's require of prisma must no-op; nothing here converts
(provide 'prisma)

(load-module-file "modules/email/autoload/compose.el")
(load-module-file "modules/email/autoload/delay.el")

(defmacro delay-tests-with-message (headers &rest forms)
  "Run FORMS in a message buffer whose extra HEADERS sit above the body."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (message-mode))
     (insert "From: a@example.com\nTo: bob@example.com\nSubject: s\n" ,headers
             mail-header-separator "\nHello\n")
     ,@forms))

(defun delay-tests-headers ()
  "The current message's headers."
  (save-restriction
    (message-narrow-to-headers)
    (buffer-string)))

(defun delay-tests-draft (&optional sent)
  "A modified message buffer that visits a draft file, SENT via mail or not."
  (let ((buffer (generate-new-buffer "*delay-tests draft*")))
    (with-current-buffer buffer
      (delay-mode-hooks (message-mode))
      (insert "unsent")
      (setq buffer-file-name "/nonexistent/delay-tests/draft")
      (when sent
        (setq message-sent-message-via '(mail))))
    buffer))

(describe "send-later"
  :var (calls)
  (before-each (setq calls nil))
  (it "runs the checks, then queues the message for the default delay, its format saved and its old deadline gone"
    (delay-tests-with-message "X-Gnus-Delayed: Mon, 12 Oct 2026 10:00:00 +0000\n"
      (setq mail-format 'plain)
      (let ((gnus-delay-default-delay "1m"))
        (cl-letf (((symbol-function 'gnus-alive-p) #'always)
                  ((symbol-function 'message-fix-before-sending) (lambda () (push 'checked calls)))
                  ((symbol-function 'gnus-delay-article)
                   (lambda (delay) (push (list delay (delay-tests-headers)) calls)))
                  ((symbol-function 'message-send-and-exit) (lambda (&rest _) (push 'sent calls))))
          (send-later))))
    (expect (length calls) :to-be 2)
    (expect (cadr calls) :to-be 'checked)
    (pcase-let ((`(,delay ,headers) (car calls)))
      (expect delay :to-equal "1m")
      (expect headers :to-match "^X-Mail-Format: plain$")
      (expect headers :not :to-match "X-Gnus-Delayed")))
  (it "sends at once with a prefix argument, the old deadline gone"
    (delay-tests-with-message "X-Gnus-Delayed: Mon, 12 Oct 2026 10:00:00 +0000\n"
      (cl-letf (((symbol-function 'gnus-alive-p) #'always)
                ((symbol-function 'gnus-delay-article) (lambda (_) (push 'queued calls)))
                ((symbol-function 'message-send-and-exit)
                 (lambda (&rest _) (push (delay-tests-headers) calls))))
        (send-later t)))
    (expect (length calls) :to-be 1)
    (expect (car calls) :to-match "^To: bob@example.com$")
    (expect (car calls) :not :to-match "X-Gnus-Delayed"))
  (it "sends at once when Gnus is not running, since nothing would send the queue"
    (delay-tests-with-message ""
      (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
                ((symbol-function 'gnus-delay-article) (lambda (_) (push 'queued calls)))
                ((symbol-function 'message-send-and-exit) (lambda (&rest _) (push 'sent calls))))
        (send-later)))
    (expect calls :to-equal '(sent)))
  (it "refuses a post that fails the news checks the queue would skip"
    (delay-tests-with-message ""
      (cl-letf (((symbol-function 'gnus-alive-p) #'always)
                ((symbol-function 'message-fix-before-sending) #'ignore)
                ((symbol-function 'message-news-p) #'always)
                ((symbol-function 'message-check-news-syntax) #'ignore)
                ((symbol-function 'gnus-delay-article) (lambda (_) (push 'queued calls))))
        (expect (send-later) :to-throw 'user-error)))
    (expect calls :to-be nil)))

(describe "queued-mail-recipient"
  (it "reads To from the queued message's head"
    (let ((nntp-server-buffer (get-buffer-create " *delay-tests nntp*")))
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-request-head)
                     (lambda (&rest _)
                       (with-current-buffer nntp-server-buffer
                         (erase-buffer)
                         (insert "Subject: s\nTo: bob@example.com\n"))
                       t)))
            (expect (queued-mail-recipient 3 "nndraft:delayed") :to-equal "bob@example.com"))
        (kill-buffer nntp-server-buffer)))))

(describe "send-queued-article-a"
  :var (expired asked)
  (before-each (setq expired nil asked nil))
  (it "passes a message of any other group straight through"
    (let ((mail-queue-sent nil))
      (cl-letf (((symbol-function 'queued-mail-recipient) (lambda (&rest _) (error "Looked up"))))
        (expect (send-queued-article-a (lambda (article group _) (list article group)) 3 "nndraft:drafts")
                :to-equal '(3 "nndraft:drafts")))
      (expect mail-queue-sent :to-be nil)))
  (it "notes where a queued message went"
    (let ((mail-queue-sent nil))
      (cl-letf (((symbol-function 'queued-mail-recipient) (lambda (&rest _) "bob@example.com")))
        (send-queued-article-a #'ignore 3 (mail-queue-group)))
      (expect mail-queue-sent :to-equal '("bob@example.com"))))
  (it "keeps a failed message queued, kills its buffer without a question, and says whose it was"
    ;; batch Emacs never asks to save a modified file buffer, so the
    ;; flag at the kill stands in for the question
    (let ((mail-queue-sent nil)
          buffer modified)
      (let ((kill-buffer-query-functions (list (lambda () (setq asked t) t)))
            (kill-buffer-hook (list (lambda ()
                                      (when (eq (current-buffer) buffer)
                                        (setq modified (list (buffer-modified-p))))))))
        (cl-letf (((symbol-function 'queued-mail-recipient) (lambda (&rest _) "bob@example.com"))
                  ((symbol-function 'gnus-request-expire-articles) (lambda (&rest args) (setq expired args))))
          (expect (send-queued-article-a (lambda (&rest _)
                                           (setq buffer (delay-tests-draft))
                                           (error "550 refused"))
                                         3 (mail-queue-group))
                  :to-throw 'error '("Delayed mail to bob@example.com failed: 550 refused"))))
      (expect (buffer-live-p buffer) :to-be nil)
      (expect modified :to-equal '(nil))
      (expect asked :to-be nil)
      (expect expired :to-be nil)
      (expect mail-queue-sent :to-be nil)))
  (it "takes a message that left before failing out of the queue, so it cannot go twice"
    (let ((mail-queue-sent nil)
          buffer)
      (cl-letf (((symbol-function 'queued-mail-recipient) (lambda (&rest _) "bob@example.com"))
                ((symbol-function 'gnus-request-expire-articles) (lambda (&rest args) (setq expired args))))
        (send-queued-article-a (lambda (&rest _)
                                 (setq buffer (delay-tests-draft t))
                                 (error "Gcc failed"))
                               3 (mail-queue-group)))
      (expect (buffer-live-p buffer) :to-be nil)
      (expect expired :to-equal (list '(3) (mail-queue-group) t))
      (expect mail-queue-sent :to-equal '("bob@example.com (then: Gcc failed)"))))
  (it "treats a quit like a failure"
    ;; a quit that escaped would end the whole run, so it is caught here
    (let ((mail-queue-sent nil))
      (cl-letf (((symbol-function 'queued-mail-recipient) (lambda (&rest _) "bob@example.com")))
        (expect (condition-case err
                    (send-queued-article-a (lambda (&rest _) (signal 'quit nil)) 3 (mail-queue-group))
                  (quit 'escaped)
                  (error (cadr err)))
                :to-equal "Delayed mail to bob@example.com failed: Quit")))))

(describe "run-mail-queue"
  (it "sends with no confirmation, quietly, every question refused, and gives the windows back"
    (let ((before (window-buffer))
          seen)
      (expect (run-mail-queue (lambda ()
                                (setq seen (list message-confirm-send inhibit-message
                                                 (car minibuffer-setup-hook)))
                                (switch-to-buffer (get-buffer-create " *delay-tests elsewhere*"))
                                (push "bob@example.com" mail-queue-sent)))
              :to-equal '(("bob@example.com") nil))
      (expect seen :to-equal (list nil t #'refuse-mail-prompt))
      (expect (window-buffer) :to-be before)))
  (it "fails a send that would ask a question instead of waiting for the answer"
    (expect (run-mail-queue (lambda () (read-multiple-choice "Kill draft?" '((?y "yes") (?n "no")))))
            :to-equal '(nil "Queued mail would ask \"Kill draft?\"")))
  (it "says why the run stopped"
    (expect (run-mail-queue (lambda () (error "Connection refused")))
            :to-equal '(nil "Connection refused"))))

(describe "report-mail-queue"
  :var (said warned)
  (before-each
    (setq said nil warned nil mail-queue-failure nil mail-queue-retry-time 0))
  (it "says where the mail went"
    (cl-letf (((symbol-function 'message) (lambda (text &rest args) (push (apply #'format text args) said))))
      (report-mail-queue '(("bob@example.com" "carol@example.com") nil) t))
    (expect said :to-equal '("Sent delayed mail to bob@example.com, carol@example.com"))
    (expect mail-queue-retry-time :to-equal 0))
  (it "holds the demon off for the backoff after a failure, and warns once per failure"
    (cl-letf (((symbol-function 'display-warning) (lambda (_type text &rest _) (push text warned))))
      (report-mail-queue '(nil "Delayed mail to bob failed: refused") t)
      (expect (time-less-p (time-add nil 290) mail-queue-retry-time) :to-be-truthy)
      (report-mail-queue '(nil "Delayed mail to bob failed: refused") t)
      (expect (length warned) :to-be 1)
      (expect (car warned) :to-match "It stays in nndraft:delayed and goes again in 5 minutes")
      (report-mail-queue '(nil nil) t)
      (report-mail-queue '(nil "Delayed mail to bob failed: refused") t)
      (expect (length warned) :to-be 2)))
  (it "leaves the demon's schedule alone for a run at quit"
    (cl-letf (((symbol-function 'display-warning) (lambda (_type text &rest _) (push text warned))))
      (report-mail-queue '(nil "Delayed mail to bob failed: refused")))
    (expect mail-queue-retry-time :to-equal 0)
    (expect (car warned) :to-match "It stays in nndraft:delayed\\.")))

(describe "send-due-mail"
  :var (ran)
  (before-each (setq ran nil mail-queue-retry-time 0))
  (it "runs gnus-delay's queue while Gnus runs and no send failed lately"
    (cl-letf (((symbol-function 'gnus-alive-p) #'always)
              ((symbol-function 'active-minibuffer-window) #'ignore)
              ((symbol-function 'gnus-delay-send-queue) (lambda () (setq ran t)))
              ((symbol-function 'report-mail-queue) #'ignore))
      (send-due-mail))
    (expect ran :to-be t))
  (it "holds the next runs off after a failed one"
    (cl-letf (((symbol-function 'gnus-alive-p) #'always)
              ((symbol-function 'active-minibuffer-window) #'ignore)
              ((symbol-function 'gnus-delay-send-queue) (lambda () (error "Connection refused")))
              ((symbol-function 'display-warning) #'ignore))
      (send-due-mail))
    (expect (time-less-p nil mail-queue-retry-time) :to-be-truthy))
  (it "waits out the backoff after a failure"
    (setq mail-queue-retry-time (time-add nil 60))
    (cl-letf (((symbol-function 'gnus-alive-p) #'always)
              ((symbol-function 'active-minibuffer-window) #'ignore)
              ((symbol-function 'gnus-delay-send-queue) (lambda () (setq ran t))))
      (send-due-mail))
    (expect ran :to-be nil))
  (it "waits while a minibuffer is in use"
    (cl-letf (((symbol-function 'gnus-alive-p) #'always)
              ((symbol-function 'active-minibuffer-window) #'always)
              ((symbol-function 'gnus-delay-send-queue) (lambda () (setq ran t))))
      (send-due-mail))
    (expect ran :to-be nil))
  (it "does nothing without Gnus"
    (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
              ((symbol-function 'active-minibuffer-window) #'ignore)
              ((symbol-function 'gnus-delay-send-queue) (lambda () (setq ran t))))
      (send-due-mail))
    (expect ran :to-be nil)))

(describe "send-queued-mail"
  :var (sent)
  (before-each (setq sent nil))
  (it "sends every queued message, due or not, its deadline header dropped"
    ;; `gnus-group-entry' is a macro over this table
    (let ((gnus-newsrc-hashtb (make-hash-table :test #'equal)))
      (puthash "nndraft:delayed" t gnus-newsrc-hashtb)
      (cl-letf (((symbol-function 'gnus-alive-p) #'always)
                ((symbol-function 'gnus-activate-group) #'ignore)
                ((symbol-function 'nndraft-articles) (lambda () '(4 7)))
                ((symbol-function 'gnus-draft-send)
                 (lambda (article group &rest _)
                   (push (list article group (and (memq #'remove-delay-header message-send-hook) t))
                         sent))))
        (expect (send-queued-mail) :to-equal '(nil nil))))
    (expect (reverse sent) :to-equal '((4 "nndraft:delayed" t) (7 "nndraft:delayed" t))))
  (it "has nothing to send without Gnus or without the queue group"
    (let ((gnus-newsrc-hashtb (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'gnus-alive-p) #'always)
                ((symbol-function 'gnus-draft-send) (lambda (&rest args) (push args sent))))
        (expect (send-queued-mail) :to-equal '(nil nil)))
      (puthash "nndraft:delayed" t gnus-newsrc-hashtb)
      (cl-letf (((symbol-function 'gnus-alive-p) #'ignore)
                ((symbol-function 'gnus-draft-send) (lambda (&rest args) (push args sent))))
        (expect (send-queued-mail) :to-equal '(nil nil))))
    (expect sent :to-be nil)))

(describe "send-queued-mail-before-quit"
  (it "lets Emacs quit once the queue is sent, asking nothing"
    (cl-letf (((symbol-function 'send-queued-mail) (lambda () '(("bob@example.com") nil)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Asked"))))
      (expect (send-queued-mail-before-quit) :to-be t)))
  (it "asks before quitting when a send failed, and stays on no"
    (let (question)
      (cl-letf (((symbol-function 'send-queued-mail)
                 (lambda () '(nil "Delayed mail to bob failed: refused")))
                ((symbol-function 'yes-or-no-p) (lambda (prompt) (setq question prompt) nil)))
        (expect (send-queued-mail-before-quit) :to-be nil))
      (expect question :to-equal "Delayed mail to bob failed: refused.  Quit anyway? "))))

(describe "start-mail-queue"
  (it "has the demon run the queue every 2 steps after 1 idle step, and quitting Emacs send it"
    (let ((kill-emacs-query-functions nil)
          handler)
      (cl-letf (((symbol-function 'gnus-demon-add-handler) (lambda (&rest args) (setq handler args))))
        (start-mail-queue))
      (expect handler :to-equal '(send-due-mail 2 1))
      (expect kill-emacs-query-functions :to-equal '(send-queued-mail-before-quit)))))

(describe "stop-mail-queue"
  (it "stops the demon's runs and sends everything still queued"
    (let ((kill-emacs-query-functions '(send-queued-mail-before-quit))
          removed reported)
      (cl-letf (((symbol-function 'gnus-demon-remove-handler) (lambda (fn &rest _) (setq removed fn)))
                ((symbol-function 'send-queued-mail) (lambda () '(("bob@example.com") nil)))
                ((symbol-function 'report-mail-queue) (lambda (&rest args) (setq reported args))))
        (stop-mail-queue))
      (expect removed :to-be 'send-due-mail)
      (expect kill-emacs-query-functions :to-be nil)
      (expect reported :to-equal '((("bob@example.com") nil))))))

;;; delay-tests.el ends here
