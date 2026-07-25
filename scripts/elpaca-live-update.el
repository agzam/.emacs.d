;;; scripts/elpaca-live-update.el --- in-session streamed Elpaca update -*- lexical-binding: t; -*-
;; Loaded into the RUNNING Emacs by `bb update' via emacsclient.  Updates
;; every package in the live session, so it picks up changes without a
;; restart - but asynchronously and with elpaca's log buffer suppressed.
;; That buffer's per-event refresh (`elpaca-log-interval', ~50fps under a
;; bulk update) is what freezes the frame; invoked programmatically it never
;; pops, and this belt-and-suspenders binds `elpaca-log-functions' to nil for
;; the run anyway.  Progress is teed to a file the caller tails - unchanged
;; packages batched into `pulled:' lines, changed ones broken out with their
;; commits; the call returns immediately, elpaca finishes on its own async
;; timers.
;; Caveat inherent to any updater: already-loaded code whose functions
;; changed only fully takes effect after that feature reloads or Emacs
;; restarts; new and not-yet-loaded packages update live.

(require 'elpaca)
(require 'cl-lib)

;; Defined in lisp/functions.el, which init loads before this driver can run.
(declare-function broken-elpaca-builds "functions")
(declare-function rebuild-broken-elpaca-builds "functions")
;; The changelog and local-rebuild helpers sit next to this file.  The live
;; driver is `load'ed by `bb update' via emacsclient, so resolve them relative
;; to this file, loading each only if the running session lacks it.
(dolist (lib '("elpaca-update-report" "elpaca-local"))
  (unless (featurep (intern lib))
    (load (expand-file-name lib (file-name-directory (or load-file-name buffer-file-name)))
          nil 'nomessage)))

(defvar elpaca-live-update--log nil "File the caller tails for progress.")
(defvar elpaca-live-update--total 0 "Packages expected to reach a terminal status.")
(defvar elpaca-live-update--failed 0 "Packages failed so far.")
(defvar elpaca-live-update--saved-log-fns 'unset "Saved `elpaca-log-functions'.")
(defvar elpaca-live-update--poll nil "Completion poll timer.")
(defvar elpaca-live-update--watchdog nil "Safety timer.")
(defvar elpaca-live-update--snapshot nil
  "Pre-update map of source dir -> HEAD sha, for the changelog.")
(defvar elpaca-live-update--reported nil
  "Hash of source dirs whose changelog block has already been emitted.")
(defvar elpaca-live-update--batcher nil
  "Shared `pulled:'-line batcher (see `elpaca-update-report-batcher').")
(defvar elpaca-live-update--locals-rebuilt nil
  "Non-nil once the post-update local-package rebuild phase has run.")
(defvar elpaca-live-update--integrity-rebuilt nil
  "Non-nil once the post-update broken-build heal phase has run.")
(defvar elpaca-live-update--persist nil
  "Persistent append-log path this run tees to, or nil when disabled.")
(defvar elpaca-live-update--heartbeat-interval 5
  "Seconds of silence before a heartbeat line is emitted.")
(defvar elpaca-live-update--last-emit nil
  "`float-time' of the most recent emitted line; the silence heartbeat keys on it.")

(defun elpaca-live-update--emit (fmt &rest args)
  "Append a formatted line to the progress file, mirroring the persistent log."
  (when elpaca-live-update--log
    (let ((line (apply #'format fmt args)))
      (setq elpaca-live-update--last-emit (float-time))
      (write-region (concat line "\n")
                    nil elpaca-live-update--log 'append 'silent)
      (elpaca-update-report-tee elpaca-live-update--persist line))))

(defun elpaca-live-update--on-finished (e)
  "Batch E into the running `pulled:' line, or break out its commit block."
  (if-let* ((changes (elpaca-update-report-block-once
                      e elpaca-live-update--snapshot elpaca-live-update--reported)))
      (progn
        (elpaca-update-report-flush elpaca-live-update--batcher
                                    #'elpaca-live-update--emit)
        (elpaca-live-update--emit
         "pulled: %s\n%s"
         (elpaca-update-report--paint "1" (symbol-name (elpaca<-id e))) changes))
    (elpaca-update-report-note elpaca-live-update--batcher #'elpaca-live-update--emit
                               (symbol-name (elpaca<-id e)))))

(defun elpaca-live-update--on-failed (e)
  "Report E failing, after flushing any pending `pulled:' names."
  (cl-incf elpaca-live-update--failed)
  (elpaca-update-report-flush elpaca-live-update--batcher #'elpaca-live-update--emit)
  (elpaca-live-update--emit
   "%s" (elpaca-update-report--paint
         "31" (concat "failed: " (symbol-name (elpaca<-id e))))))

(defun elpaca-live-update--cleanup ()
  "Cancel timers, drop subscribers, restore `elpaca-log-functions'."
  (when elpaca-live-update--poll (cancel-timer elpaca-live-update--poll))
  (when elpaca-live-update--watchdog (cancel-timer elpaca-live-update--watchdog))
  (setq elpaca-live-update--poll nil elpaca-live-update--watchdog nil)
  (elpaca-unsubscribe 'finished #'elpaca-live-update--on-finished)
  (elpaca-unsubscribe 'failed #'elpaca-live-update--on-failed)
  (unless (eq elpaca-live-update--saved-log-fns 'unset)
    (setq elpaca-log-functions elpaca-live-update--saved-log-fns
          elpaca-live-update--saved-log-fns 'unset))
  (setq elpaca-update-report-color nil
        elpaca-live-update--batcher nil))

(defun elpaca-live-update--finish ()
  "Flush the trailing `pulled:' names, emit the summary marker, and clean up.
A build still broken after the heal phase (deleted repos can only be
repaired by a fresh process) flips the marker and points at `bb repair'.
The marker line stays plain - no color prefix - so the caller's `^UPDATE-'
progress regex still anchors."
  (let ((updated (if (hash-table-p elpaca-live-update--reported)
                     (hash-table-count elpaca-live-update--reported) 0))
        (broken (broken-elpaca-builds)))
    (elpaca-update-report-flush elpaca-live-update--batcher #'elpaca-live-update--emit)
    (elpaca-live-update--cleanup)
    (pcase-dolist (`(,id . ,reason) broken)
      (elpaca-live-update--emit
       "STILL BROKEN %s (%s) - run bb repair, then restart Emacs" id reason))
    (elpaca-live-update--emit "%s %d package(s) processed, %d updated, %d failed"
                              (if (or (< 0 elpaca-live-update--failed) broken)
                                  "UPDATE-FAILED" "UPDATE-DONE")
                              elpaca-live-update--total updated
                              elpaca-live-update--failed)))

(defun elpaca-live-update--run-local-rebuilds ()
  "Rebuild on-disk-changed build-in-place locals, if any.
Announcing what it waits on also refreshes the silence clock, so the wait
is timed afresh.  Queuing rebuilds makes the queue non-terminal again, so
the poll keeps ticking until they settle; with nothing queued the next
tick advances to the heal phase."
  (when-let* ((rebuilt (elpaca-local-rebuild-changed
                        (lambda (fmt &rest args)
                          (elpaca-update-report-flush elpaca-live-update--batcher
                                                      #'elpaca-live-update--emit)
                          (apply #'elpaca-live-update--emit fmt args)))))
    (elpaca-live-update--emit "waiting for %d local rebuild(s): %s"
                              (length rebuilt)
                              (mapconcat #'symbol-name rebuilt ", "))))

(defun elpaca-live-update--run-integrity-rebuilds ()
  "Heal half-built/stale builds the git update skipped, if any.
A merge that moves no HEAD never rebuilds, so damage from an earlier
interrupted run survives a plain update - rebuild those in place (works
in-session; disk becomes consistent, RAM catches up on restart)."
  (when-let* ((broken (rebuild-broken-elpaca-builds
                       (lambda (fmt &rest args)
                         (elpaca-update-report-flush elpaca-live-update--batcher
                                                     #'elpaca-live-update--emit)
                         (apply #'elpaca-live-update--emit fmt args)))))
    (elpaca-live-update--emit "waiting for %d integrity rebuild(s): %s"
                              (length broken)
                              (mapconcat (lambda (b) (symbol-name (car b)))
                                         broken ", "))))

(defun elpaca-live-update--advance ()
  "Poll tick: surface silent stretches, run the rebuild phase, then finish.
Heartbeats keep the run from ever going silent: whenever nothing has been
emitted for `elpaca-live-update--heartbeat-interval' - a slow compile, or a
genuinely wedged package - a line says exactly what is still pending, via the
same silence-keyed helper the headless driver uses.  Once the queue settles
two more phases run before the terminal marker, each re-arming the poll when
it queues work: on-disk-changed locals (a git update skips a build-in-place
checkout whose merge moved no HEAD), then broken-build healing (half-built or
stale builds a plain update also skips)."
  (let ((pending (elpaca-update-report-pending)))
    ;; Self-heal a reload mid-run: seed the silence clock when unset.
    (unless elpaca-live-update--last-emit
      (setq elpaca-live-update--last-emit (float-time)))
    ;; A slow trickle of finished packages must not sit invisible in the
    ;; batcher waiting for the line to fill out - flush it on staleness.
    (elpaca-update-report-flush-stale elpaca-live-update--batcher
                                      #'elpaca-live-update--emit
                                      elpaca-live-update--last-emit)
    (if pending
        (when-let* ((line (elpaca-update-report-heartbeat-line
                           pending elpaca-live-update--last-emit
                           elpaca-live-update--heartbeat-interval)))
          (elpaca-live-update--emit "%s" line))
      (cond
       ((not elpaca-live-update--locals-rebuilt)
        (setq elpaca-live-update--locals-rebuilt t)
        (elpaca-live-update--run-local-rebuilds))
       ((not elpaca-live-update--integrity-rebuilt)
        (setq elpaca-live-update--integrity-rebuilt t)
        (elpaca-live-update--run-integrity-rebuilds))
       (t (elpaca-live-update--finish))))))

;;;###autoload
(defun elpaca-live-update-start (logfile &optional packages color)
  "Asynchronously update Elpaca packages, teeing progress to LOGFILE.
Without PACKAGES, update everything; otherwise update only that list of
package symbols.  With COLOR non-nil, shas and dates are wrapped in ANSI SGR
codes (the caller passes this only when its own stdout is a terminal).  Runs
in the live session with the log buffer suppressed (no frame freeze) and
returns immediately; the caller tails LOGFILE for progress and a terminal
UPDATE-DONE / UPDATE-FAILED line."
  (when elpaca-live-update--poll
    (error "elpaca-live-update: an update is already in progress"))
  (condition-case err
      (progn
        (setq elpaca-live-update--log logfile
              elpaca-live-update--total (if packages (length packages)
                                          (length (elpaca--queued)))
              elpaca-live-update--failed 0
              elpaca-live-update--batcher (elpaca-update-report-batcher)
              elpaca-live-update--locals-rebuilt nil
              elpaca-live-update--integrity-rebuilt nil
              elpaca-live-update--persist (elpaca-update-report-open-session "live")
              elpaca-live-update--heartbeat-interval
              (max 1 (string-to-number (or (getenv "ELPACA_UPDATE_HEARTBEAT") "5")))
              elpaca-live-update--last-emit (float-time)
              ;; honor the caller's tty-color? verdict (cleared in --cleanup)
              elpaca-update-report-color color)
        (write-region "" nil logfile nil 'silent) ; truncate
        (elpaca-live-update--emit "update: %d package(s) queued"
                                  elpaca-live-update--total)
        ;; Baseline every repo's HEAD before any merge, so each package can be
        ;; diffed OLD..NEW once it lands (fetch never moves local HEAD).
        (setq elpaca-live-update--snapshot (elpaca-update-report-snapshot)
              elpaca-live-update--reported (make-hash-table :test 'equal))
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
        ;; Announce the phase before kicking off: the first `pulled:' line can
        ;; be seconds away, and the tail must never open on dead air.
        (elpaca-live-update--emit
         "update: fetching + merging + rebuilding %d package(s)..."
         elpaca-live-update--total)
        ;; interactive=t => elpaca processes the queue (async), returns at once.
        (if packages
            (dolist (p packages) (elpaca-update p t))
          (elpaca-update-all t))
        (setq elpaca-live-update--poll
              (run-at-time 1 1 #'elpaca-live-update--advance))
        t)
    (error
     (elpaca-live-update--emit "UPDATE-ERROR %s" (error-message-string err))
     (elpaca-live-update--cleanup)
     nil)))
