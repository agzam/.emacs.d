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
(defvar elpaca-live-update--persist nil
  "Persistent append-log path this run tees to, or nil when disabled.")
(defvar elpaca-live-update--heartbeat-interval 10
  "Seconds the pending set may sit unchanged before a heartbeat line is emitted.")
(defvar elpaca-live-update--last-sig nil
  "Last pending-work signature, for detecting a stalled or slow queue.")
(defvar elpaca-live-update--last-change nil
  "`float-time' at which the pending signature last changed.")

(defun elpaca-live-update--emit (fmt &rest args)
  "Append a formatted line to the progress file."
  (when elpaca-live-update--log
    (write-region (concat (apply #'format fmt args) "\n")
                  nil elpaca-live-update--log 'append 'silent)))

(defun elpaca-live-update--flush-pulled ()
  "Flush pending unchanged-package names as one `pulled:' line."
  (elpaca-update-report-flush elpaca-live-update--batcher #'elpaca-live-update--emit))

(defun elpaca-live-update--note-pulled (name)
  "Batch NAME into the pending `pulled:' line, flushing on overflow first."
  (elpaca-update-report-note elpaca-live-update--batcher #'elpaca-live-update--emit name))

(defun elpaca-live-update--changes-for (e)
  "Return E's changelog block, once per shared source dir, else nil."
  (when-let* (((hash-table-p elpaca-live-update--snapshot))
              (dir (ignore-errors (elpaca<-source-dir e)))
              ((not (gethash dir elpaca-live-update--reported)))
              (changes (elpaca-update-report-block e elpaca-live-update--snapshot)))
    (puthash dir t elpaca-live-update--reported)
    changes))

(defun elpaca-live-update--on-finished (e)
  "Batch E into the running `pulled:' line, or break out its commit block."
  (if-let* ((changes (elpaca-live-update--changes-for e)))
      (progn
        (elpaca-live-update--flush-pulled)
        (elpaca-live-update--emit
         "pulled: %s\n%s"
         (elpaca-update-report--paint "1" (symbol-name (elpaca<-id e))) changes))
    (elpaca-live-update--note-pulled (symbol-name (elpaca<-id e)))))

(defun elpaca-live-update--on-failed (e)
  "Report E failing, after flushing any pending `pulled:' names."
  (cl-incf elpaca-live-update--failed)
  (elpaca-live-update--flush-pulled)
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
The marker line stays plain - no color prefix - so the caller's `^UPDATE-'
progress regex still anchors."
  (let ((updated (if (hash-table-p elpaca-live-update--reported)
                     (hash-table-count elpaca-live-update--reported) 0)))
    (elpaca-live-update--flush-pulled)
    (elpaca-live-update--cleanup)
    (elpaca-live-update--emit "%s %d package(s) processed, %d updated, %d failed"
                              (if (< 0 elpaca-live-update--failed)
                                  "UPDATE-FAILED" "UPDATE-DONE")
                              elpaca-live-update--total updated
                              elpaca-live-update--failed)))

(defun elpaca-live-update--heartbeat (pending elapsed)
  "Emit a `still working' line naming PENDING packages unchanged for ELAPSED secs."
  (elpaca-live-update--flush-pulled)
  (elpaca-live-update--emit
   "still working (%ds, %d pending): %s"
   (round elapsed) (length pending)
   (elpaca-update-report-format-pending pending)))

(defun elpaca-live-update--run-local-rebuilds ()
  "Rebuild on-disk-changed build-in-place locals; finish now when none need it.
Announces what it waits on and resets the heartbeat clock so the wait is timed
afresh.  Queuing rebuilds makes the queue non-terminal again, so the poll keeps
ticking until they settle."
  (if-let* ((rebuilt (elpaca-local-rebuild-changed
                      (lambda (fmt &rest args)
                        (elpaca-live-update--flush-pulled)
                        (apply #'elpaca-live-update--emit fmt args)))))
      (progn
        (elpaca-live-update--emit "waiting for %d local rebuild(s): %s"
                                  (length rebuilt)
                                  (mapconcat #'symbol-name rebuilt ", "))
        (setq elpaca-live-update--last-sig nil))
    (elpaca-live-update--finish)))

(defun elpaca-live-update--advance ()
  "Poll tick: surface slow/stalled work, run the rebuild phase, then finish.
Heartbeats keep the run from ever going silent: whenever the pending set sits
unchanged past `elpaca-live-update--heartbeat-interval' - a slow compile, or a
genuinely wedged package - it says exactly what it is waiting on.  Once the
queue settles it rebuilds on-disk-changed locals as a second phase (a git
update skips a build-in-place checkout whose merge moved no HEAD), which makes
the queue non-terminal again, so the poll keeps ticking and only writes the
terminal marker once that too has settled."
  (let* ((now (float-time))
         (pending (elpaca-update-report-pending))
         (sig (elpaca-update-report-format-pending pending)))
    ;; Self-heal a reload mid-run: seed the heartbeat clock when unset.
    (unless elpaca-live-update--last-change
      (setq elpaca-live-update--last-sig sig
            elpaca-live-update--last-change now))
    (cond
     ((not (equal sig elpaca-live-update--last-sig))
      (setq elpaca-live-update--last-sig sig
            elpaca-live-update--last-change now))
     ((and pending
           (>= (- now elpaca-live-update--last-change)
               elpaca-live-update--heartbeat-interval))
      (elpaca-live-update--heartbeat pending (- now elpaca-live-update--last-change))
      (setq elpaca-live-update--last-change now)))
    (when (null pending)
      (if elpaca-live-update--locals-rebuilt
          (elpaca-live-update--finish)
        (setq elpaca-live-update--locals-rebuilt t)
        (elpaca-live-update--run-local-rebuilds)))))

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
              elpaca-live-update--persist (elpaca-update-report-open-session "live")
              elpaca-live-update--heartbeat-interval
              (max 1 (string-to-number (or (getenv "ELPACA_UPDATE_HEARTBEAT") "10")))
              elpaca-live-update--last-sig nil
              elpaca-live-update--last-change (float-time)
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
