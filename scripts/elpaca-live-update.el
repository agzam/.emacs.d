;;; scripts/elpaca-live-update.el --- in-session streamed Elpaca update -*- lexical-binding: t; -*-
;; Loaded into the RUNNING Emacs by `bb update' via emacsclient.  Updates
;; every package in the live session, so it picks up changes without a
;; restart - but asynchronously and with elpaca's log buffer suppressed.
;; That buffer's per-event refresh (`elpaca-log-interval', ~50fps under a
;; bulk update) is what freezes the frame; invoked programmatically it never
;; pops, and this belt-and-suspenders binds `elpaca-log-functions' to nil for
;; the run anyway.  Per-package progress is teed to a file the caller tails;
;; the call returns immediately, elpaca finishes on its own async timers.
;; Caveat inherent to any updater: already-loaded code whose functions
;; changed only fully takes effect after that feature reloads or Emacs
;; restarts; new and not-yet-loaded packages update live.

(require 'elpaca)
(require 'cl-lib)

(defvar elpaca-live-update--log nil "File the caller tails for progress.")
(defvar elpaca-live-update--total 0 "Packages expected to reach a terminal status.")
(defvar elpaca-live-update--done 0 "Packages finished so far.")
(defvar elpaca-live-update--failed 0 "Packages failed so far.")
(defvar elpaca-live-update--saved-log-fns 'unset "Saved `elpaca-log-functions'.")
(defvar elpaca-live-update--poll nil "Completion poll timer.")
(defvar elpaca-live-update--watchdog nil "Safety timer.")

(defun elpaca-live-update--emit (fmt &rest args)
  "Append a formatted line to the progress file."
  (when elpaca-live-update--log
    (write-region (concat (apply #'format fmt args) "\n")
                  nil elpaca-live-update--log 'append 'silent)))

(defun elpaca-live-update--progress ()
  "Running count of packages that have reached a terminal status this run."
  (+ elpaca-live-update--done elpaca-live-update--failed))

(defun elpaca-live-update--on-finished (e)
  "Report E finishing."
  (cl-incf elpaca-live-update--done)
  (elpaca-live-update--emit "[%d/%d] ok   %s" (elpaca-live-update--progress)
                            elpaca-live-update--total (elpaca<-id e)))

(defun elpaca-live-update--on-failed (e)
  "Report E failing."
  (cl-incf elpaca-live-update--failed)
  (elpaca-live-update--emit "[%d/%d] FAIL %s" (elpaca-live-update--progress)
                            elpaca-live-update--total (elpaca<-id e)))

(defun elpaca-live-update--terminal-p ()
  "Non-nil once every queued package has reached a terminal status."
  (cl-every (lambda (q)
              (cl-every (lambda (cell)
                          (memq (elpaca<-status (cdr cell)) '(finished failed)))
                        (elpaca-q<-elpacas q)))
            elpaca--queues))

(defun elpaca-live-update--cleanup ()
  "Cancel timers, drop subscribers, restore `elpaca-log-functions'."
  (when elpaca-live-update--poll (cancel-timer elpaca-live-update--poll))
  (when elpaca-live-update--watchdog (cancel-timer elpaca-live-update--watchdog))
  (setq elpaca-live-update--poll nil elpaca-live-update--watchdog nil)
  (elpaca-unsubscribe 'finished #'elpaca-live-update--on-finished)
  (elpaca-unsubscribe 'failed #'elpaca-live-update--on-failed)
  (unless (eq elpaca-live-update--saved-log-fns 'unset)
    (setq elpaca-log-functions elpaca-live-update--saved-log-fns
          elpaca-live-update--saved-log-fns 'unset)))

(defun elpaca-live-update--finish ()
  "Emit the summary marker and clean up."
  (elpaca-live-update--cleanup)
  (elpaca-live-update--emit "%s %d package(s) processed, %d failed"
                            (if (< 0 elpaca-live-update--failed)
                                "UPDATE-FAILED" "UPDATE-DONE")
                            elpaca-live-update--total elpaca-live-update--failed))

;;;###autoload
(defun elpaca-live-update-start (logfile &optional packages)
  "Asynchronously update Elpaca packages, teeing progress to LOGFILE.
Without PACKAGES, update everything; otherwise update only that list of
package symbols.  Runs in the live session with the log buffer suppressed (no
frame freeze) and returns immediately; the caller tails LOGFILE for progress
and a terminal UPDATE-DONE / UPDATE-FAILED line."
  (when elpaca-live-update--poll
    (error "elpaca-live-update: an update is already in progress"))
  (condition-case err
      (progn
        (setq elpaca-live-update--log logfile
              elpaca-live-update--total (if packages (length packages)
                                          (length (elpaca--queued)))
              elpaca-live-update--done 0
              elpaca-live-update--failed 0)
        (write-region "" nil logfile nil 'silent) ; truncate
        (elpaca-live-update--emit "update: %d package(s) queued"
                                  elpaca-live-update--total)
        ;; Suppress the log buffer for the async run (its refresh is the freeze).
        (setq elpaca-live-update--saved-log-fns elpaca-log-functions
              elpaca-log-functions nil)
        (elpaca-subscribe 'finished #'elpaca-live-update--on-finished)
        (elpaca-subscribe 'failed #'elpaca-live-update--on-failed)
        (setq elpaca-live-update--watchdog
              (run-at-time (string-to-number
                            (or (getenv "ELPACA_UPDATE_WATCHDOG") "1800"))
                           nil (lambda ()
                                 (elpaca-live-update--emit "UPDATE-TIMEOUT")
                                 (elpaca-live-update--cleanup))))
        ;; interactive=t => elpaca processes the queue (async), returns at once.
        (if packages
            (dolist (p packages) (elpaca-update p t))
          (elpaca-update-all t))
        (setq elpaca-live-update--poll
              (run-at-time 1 1 (lambda ()
                                 (when (elpaca-live-update--terminal-p)
                                   (elpaca-live-update--finish)))))
        t)
    (error
     (elpaca-live-update--emit "UPDATE-ERROR %s" (error-message-string err))
     (elpaca-live-update--cleanup)
     nil)))
