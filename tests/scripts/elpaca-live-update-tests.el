;;; tests/scripts/elpaca-live-update-tests.el --- live update driver specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "scripts/elpaca-live-update.el")

;; Elpaca is absent from the buttercup sandbox, and must stay absent outside
;; each spec: other suites assert on that absence, and every suite loads into
;; the one shared process.  So the entry points the driver reaches for are
;; stubbed per-spec via `cl-letf', never with global defuns.
(defmacro with-elpaca-stubs (&rest body)
  "Run BODY with the driver's elpaca entry points stubbed inert."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'elpaca-subscribe) (lambda (&rest _) nil))
             ((symbol-function 'elpaca-unsubscribe) (lambda (&rest _) nil))
             ((symbol-function 'elpaca--queued) (lambda (&optional _) nil))
             ((symbol-function 'elpaca-update-all) (lambda (&optional _) nil))
             ;; keep specs out of the real persistent append-log
             ((symbol-function 'elpaca-update-report-open-session)
              (lambda (_label) nil)))
     (let ((elpaca-log-functions nil))
       ,@body)))
(defvar elpaca-log-functions)

(defun elpaca-live-update-tests--reset ()
  "Cancel any timers a spec armed and null the driver's run state."
  (dolist (timer (list elpaca-live-update--poll elpaca-live-update--watchdog))
    (when (timerp timer) (cancel-timer timer)))
  (setq elpaca-live-update--poll nil
        elpaca-live-update--watchdog nil
        elpaca-live-update--log nil
        elpaca-live-update--persist nil
        elpaca-live-update--batcher nil
        elpaca-live-update--saved-log-fns 'unset
        elpaca-live-update--last-emit nil))

(defun elpaca-live-update-tests--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(describe "elpaca-live-update-start"
  (it "hands back the in-flight run's progress file instead of starting anew"
    (unwind-protect
        (let (started)
          (with-elpaca-stubs
            (cl-letf (((symbol-function 'elpaca-update-all)
                       (lambda (&optional _) (setq started t))))
              (setq elpaca-live-update--poll (run-at-time 3600 nil #'ignore)
                    elpaca-live-update--log "/tmp/in-flight.log")
              (expect (elpaca-live-update-start "/tmp/fresh.log")
                      :to-equal "/tmp/in-flight.log")
              (expect started :to-be nil))))
      (elpaca-live-update-tests--reset)))

  (it "dismantles a stale wreck, starts fresh, and returns the new logfile"
    (let ((stale (run-at-time 3600 nil #'ignore))
          (logfile (make-temp-file "elpaca-live-update-tests")))
      (cancel-timer stale) ; non-nil poll, timer no longer armed = the wreck
      (unwind-protect
          (with-elpaca-stubs
            (setq elpaca-live-update--poll stale)
            (expect (elpaca-live-update-start logfile) :to-equal logfile)
            (expect (memq elpaca-live-update--poll timer-list) :to-be-truthy)
            (let ((content (elpaca-live-update-tests--slurp logfile)))
              (expect content :to-match "0 package(s) queued")
              (expect content :to-match "fetching \\+ merging")))
        (elpaca-live-update-tests--reset)
        (delete-file logfile))))

  (it "returns nil and lands UPDATE-ERROR in the logfile when kickoff signals"
    (let ((logfile (make-temp-file "elpaca-live-update-tests")))
      (unwind-protect
          (with-elpaca-stubs
            (cl-letf (((symbol-function 'elpaca-update-report-snapshot)
                       (lambda () (error "boom"))))
              (expect (elpaca-live-update-start logfile) :to-be nil)
              (expect (elpaca-live-update-tests--slurp logfile)
                      :to-match "^UPDATE-ERROR boom")))
        (elpaca-live-update-tests--reset)
        (delete-file logfile)))))

(describe "elpaca-live-update--emit"
  (it "never signals on an unwritable progress file"
    (unwind-protect
        (progn
          (setq elpaca-live-update--log "/nonexistent-dir/nowhere/progress.log"
                elpaca-live-update--persist nil)
          (expect (elpaca-live-update--emit "hello %s" "world")
                  :not :to-throw))
      (elpaca-live-update-tests--reset))))

(describe "elpaca-live-update--advance"
  (it "lands the terminal UPDATE-ERROR marker and stops the poll on a tick error"
    (let ((logfile (make-temp-file "elpaca-live-update-tests"))
          (poll (run-at-time 3600 nil #'ignore))
          (watchdog (run-at-time 3600 nil #'ignore)))
      (unwind-protect
          (with-elpaca-stubs
            (cl-letf (((symbol-function 'elpaca-update-report-pending)
                       (lambda () (error "heal phase exploded"))))
              (setq elpaca-live-update--log logfile
                    elpaca-live-update--poll poll
                    elpaca-live-update--watchdog watchdog
                    elpaca-live-update--persist nil
                    elpaca-live-update--batcher (elpaca-update-report-batcher))
              (elpaca-live-update--advance)
              (expect (elpaca-live-update-tests--slurp logfile)
                      :to-match "^UPDATE-ERROR heal phase exploded")
              (expect elpaca-live-update--poll :to-be nil)
              (expect (memq poll timer-list) :to-be nil)))
        (elpaca-live-update-tests--reset)
        (when (timerp poll) (cancel-timer poll))
        (when (timerp watchdog) (cancel-timer watchdog))
        (delete-file logfile)))))

;;; tests/scripts/elpaca-live-update-tests.el ends here
