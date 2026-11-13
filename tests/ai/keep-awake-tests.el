;;; tests/ai/keep-awake-tests.el --- ai/autoload/keep-awake.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/ai/autoload/keep-awake.el")

;; eca, agent-shell and eca-ghostel are absent here: specs stub the parts
;; of their API that keep-awake reads.
(defvar eca--sessions)

(describe "keep-awake-check"
  (before-each
    (setq keep-awake--first-seen nil)
    (spy-on 'keep-awake--renew))

  (it "renews the lease while some agent work is busy"
    (spy-on 'keep-awake--busy-items :and-return-value '(chat))
    (keep-awake-check)
    (expect 'keep-awake--renew :to-have-been-called))

  (it "lets the lease run out once nothing is busy"
    (spy-on 'keep-awake--busy-items :and-return-value nil)
    (keep-awake-check)
    (expect 'keep-awake--renew :not :to-have-been-called))

  (it "stops counting work busy for longer than the max age"
    (setq keep-awake--first-seen `((chat . ,(- (float-time) keep-awake-max-age 1))))
    (spy-on 'keep-awake--busy-items :and-return-value '(chat))
    (keep-awake-check)
    (expect 'keep-awake--renew :not :to-have-been-called))

  (it "still renews for fresh work next to stuck work"
    (setq keep-awake--first-seen `((chat . ,(- (float-time) keep-awake-max-age 1))))
    (spy-on 'keep-awake--busy-items :and-return-value '(chat run))
    (keep-awake-check)
    (expect 'keep-awake--renew :to-have-been-called))

  (it "keeps the first sighting while work stays busy"
    (setq keep-awake--first-seen '((chat . 100.0)))
    (spy-on 'keep-awake--busy-items :and-return-value '(chat))
    (keep-awake-check)
    (expect (alist-get 'chat keep-awake--first-seen) :to-equal 100.0))

  (it "forgets work once it stops, so work that returns starts a new age"
    (setq keep-awake--first-seen `((chat . ,(- (float-time) keep-awake-max-age 1))))
    (spy-on 'keep-awake--busy-items :and-return-value nil)
    (keep-awake-check)
    (expect keep-awake--first-seen :to-be nil)))

(describe "keep-awake--busy-items"
  (it "counts working ECA chats and running ECA jobs, not waiting chats or finished jobs"
    (let ((eca--sessions '((1 . session))))
      (cl-letf (((symbol-function 'eca-vals) (lambda (map) (mapcar #'cdr map)))
                ((symbol-function 'eca-chat-buffers) (lambda (_) '(working asking idle)))
                ((symbol-function 'eca-chat-status)
                 (lambda (chat)
                   (alist-get chat '((working . running)
                                     (asking . waiting-answer)
                                     (idle . idle)))))
                ((symbol-function 'eca--session-jobs)
                 (lambda (_) (vector '(:id "job-1" :chatId "c1" :status "running")
                                     '(:id "job-2" :chatId "c1" :status "completed")))))
        (expect (keep-awake--eca-items) :to-equal '(working ("c1" "job-1"))))))

  (it "counts the agent-shell buffers waiting on their agent"
    (let ((busy (generate-new-buffer " *keep-awake-busy-shell*"))
          (idle (generate-new-buffer " *keep-awake-idle-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list busy idle)))
                    ((symbol-function 'shell-maker-busy)
                     (lambda () (eq (current-buffer) busy))))
            (expect (keep-awake--agent-shell-items) :to-equal (list busy)))
        (kill-buffer busy)
        (kill-buffer idle))))

  (it "counts the eca-ghostel runs whose command still runs"
    (cl-letf (((symbol-function 'eca-ghostel-list)
               (lambda ()
                 '((:buffer "*eca-ghostel: tests*" :status "running 12.0s")
                   (:buffer "*eca-ghostel: lint*" :status "exited 0 after 3.1s")))))
      (expect (keep-awake--ghostel-items) :to-equal '("*eca-ghostel: tests*"))))

  (it "joins the work of every source"
    (spy-on 'keep-awake--eca-items :and-return-value '(chat))
    (spy-on 'keep-awake--agent-shell-items :and-return-value '(shell))
    (spy-on 'keep-awake--ghostel-items :and-return-value '("run"))
    (expect (keep-awake--busy-items) :to-equal '(chat shell "run")))

  (it "finds nothing when no agent package is loaded"
    (let ((eca--sessions nil))
      (cl-letf (((symbol-function 'eca-chat-buffers) nil)
                ((symbol-function 'agent-shell-buffers) nil)
                ((symbol-function 'eca-ghostel-list) nil))
        (expect (keep-awake--busy-items) :to-be nil)))))

(describe "keep-awake--renew"
  (before-each (setq keep-awake--process nil))
  (after-each (setq keep-awake--process nil))

  (it "starts caffeinate bound to this Emacs and timed out after the lease"
    (spy-on 'make-process :and-return-value 'lease)
    (keep-awake--renew)
    (expect (plist-get (spy-calls-args-for 'make-process 0) :command)
            :to-equal (list "caffeinate" "-i"
                            "-t" (number-to-string keep-awake-lease)
                            "-w" (number-to-string (emacs-pid))))
    (expect keep-awake--process :to-be 'lease))

  (it "ends the lease it replaces only after the new one started"
    (spy-on 'make-process :and-return-value 'new)
    (spy-on 'process-live-p :and-return-value t)
    (spy-on 'delete-process
            :and-call-fake (lambda (_) (expect 'make-process :to-have-been-called)))
    (setq keep-awake--process 'old)
    (keep-awake--renew)
    (expect 'delete-process :to-have-been-called-with 'old)
    (expect keep-awake--process :to-be 'new))

  (it "leaves alone a lease that already ran out"
    (spy-on 'make-process :and-return-value 'new)
    (spy-on 'process-live-p :and-return-value nil)
    (spy-on 'delete-process)
    (setq keep-awake--process 'old)
    (keep-awake--renew)
    (expect 'delete-process :not :to-have-been-called)))

(describe "keep-awake-mode"
  (before-each (spy-on 'keep-awake-check))
  (after-each
    (setq keep-awake--process nil)
    (keep-awake-mode -1))

  (it "checks for agent work on a repeating timer"
    (keep-awake-mode 1)
    (expect (timerp keep-awake--timer) :to-be-truthy)
    (expect (timer--repeat-delay keep-awake--timer) :to-equal keep-awake-interval))

  (it "stops the checks and ends the lease when turned off"
    (spy-on 'process-live-p :and-return-value t)
    (spy-on 'delete-process)
    (keep-awake-mode 1)
    (setq keep-awake--process 'lease)
    (keep-awake-mode -1)
    (expect keep-awake--timer :to-be nil)
    (expect 'delete-process :to-have-been-called-with 'lease)
    (expect keep-awake--process :to-be nil))

  (it "keeps the current lease when turned on again"
    (spy-on 'process-live-p :and-return-value t)
    (spy-on 'delete-process)
    (keep-awake-mode 1)
    (setq keep-awake--process 'lease)
    (keep-awake-mode 1)
    (expect 'delete-process :not :to-have-been-called)
    (expect keep-awake--process :to-be 'lease)))

;;; keep-awake-tests.el ends here
