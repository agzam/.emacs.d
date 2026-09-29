;;; tests/email/quit-tests.el --- leaving mail in one step -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'gnus-sum)

(load-module-file "modules/email/autoload/quit.el")

(defvar quit-tests-calls nil
  "What `quit-mail' asked of Gnus and its summaries, newest first.")

(defvar quit-tests-buffers nil
  "Buffers a spec made, killed after it.")

(defun quit-tests-buffer (name mode)
  "A buffer called NAME in MODE, killed after the spec."
  (let ((buffer (generate-new-buffer name)))
    (push buffer quit-tests-buffers)
    (with-current-buffer buffer
      (funcall mode))
    buffer))

(defun quit-tests-message (text)
  "A message buffer visiting a draft file of its own, holding TEXT if any.
A Gnus message buffer visits its file in the drafts group the same way."
  (let ((buffer (quit-tests-buffer "*unsent*" #'message-mode)))
    (with-current-buffer buffer
      (setq buffer-file-name (make-temp-name
                              (expand-file-name "quit-tests-draft" temporary-file-directory)))
      (when text
        (insert text)))
    buffer))

(defmacro quit-tests-with-gnus (buffers &rest body)
  "Run BODY with Gnus running over BUFFERS, its exit and `q' logged.
The logs land in `quit-tests-calls'; a spec's buffers and files go after it."
  (declare (indent 1))
  `(let ((gnus-group-buffer (buffer-name (quit-tests-buffer "*Group*" #'fundamental-mode))))
     (setq quit-tests-calls nil)
     (unwind-protect
         (let ((gnus-buffers ,buffers))
           (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                     ((symbol-function 'gnus-group-exit)
                      (lambda () (push (list 'exit gnus-interactive-exit) quit-tests-calls)))
                     ((symbol-function 'quit-mail-summary)
                      (lambda () (push (list 'summary (current-buffer)) quit-tests-calls))))
             ,@body))
       (dolist (buffer quit-tests-buffers)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and buffer-file-name (file-exists-p buffer-file-name))
               (delete-file buffer-file-name))
             (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (setq quit-tests-buffers nil))))

(describe "quit-mail"
  (it "saves an unsent Gnus message to its draft, then exits Gnus without asking"
    (let ((unsent (quit-tests-message "Hello Ann,\n\nsee you at noon")))
      (quit-tests-with-gnus (list unsent)
        (quit-mail)
        (let ((file (buffer-file-name unsent)))
          (expect (file-exists-p file) :to-be-truthy)
          (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
                  :to-match "see you at noon"))
        (expect (buffer-modified-p unsent) :not :to-be-truthy)
        (expect quit-tests-calls :to-equal '((exit nil))))))
  (it "leaves an untouched message to Gnus's exit, which drops it"
    (let ((untouched (quit-tests-message nil)))
      (quit-tests-with-gnus (list untouched)
        (quit-mail)
        (expect (file-exists-p (buffer-file-name untouched)) :not :to-be-truthy)
        (expect quit-tests-calls :to-equal '((exit nil))))))
  (it "leaves a message started outside Gnus alone"
    (let ((elsewhere (quit-tests-message "a note to self")))
      (quit-tests-with-gnus nil
        (quit-mail)
        (expect (buffer-modified-p elsewhere) :to-be-truthy)
        (expect (file-exists-p (buffer-file-name elsewhere)) :not :to-be-truthy))))
  (it "closes each summary the way q does, before Gnus exits"
    (let ((summary (quit-tests-buffer "*Summary quit-tests*" #'gnus-summary-mode)))
      (quit-tests-with-gnus (list summary)
        (quit-mail)
        (expect (reverse quit-tests-calls) :to-equal `((summary ,summary) (exit nil))))))
  (it "keeps Gnus running when a summary's question is cancelled"
    ;; C-g at "Run ... first?" in a summary with a queue
    (let ((summary (quit-tests-buffer "*Summary quit-tests*" #'gnus-summary-mode)))
      (quit-tests-with-gnus (list summary)
        ;; quit is no error, so :to-throw would let it end the run
        (cl-letf (((symbol-function 'quit-mail-summary) (lambda () (signal 'quit nil))))
          (expect (condition-case nil (progn (quit-mail) 'finished) (quit 'quit))
                  :to-be 'quit))
        (expect quit-tests-calls :to-be nil))))
  (it "does nothing when Gnus is not running"
    (let ((unsent (quit-tests-message "Hello Ann")))
      (quit-tests-with-gnus (list unsent)
        (cl-letf (((symbol-function 'gnus-alive-p) #'ignore))
          (quit-mail))
        (expect quit-tests-calls :to-be nil)
        (expect (buffer-modified-p unsent) :to-be-truthy)))))

;;; quit-tests.el ends here
