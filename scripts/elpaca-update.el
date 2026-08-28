;;; scripts/elpaca-update.el --- headless Elpaca update -*- lexical-binding: t; -*-
;; Fetch, merge and rebuild every Elpaca package in a headless --batch Emacs,
;; the way `doom sync -u' did under straight - so the running Emacs is never
;; frozen by elpaca's (nominally async, in practice UI-blocking) update, and
;; nothing draws a frame.  Driven by `bb update', which boots the full config
;; under --batch and then loads this file last:
;;   emacs -Q --batch --init-directory <root> \
;;     -l <root>/early-init.el -l <root>/init.el -l <root>/scripts/elpaca-update.el
;; Two non-obvious batch facts this relies on: --batch does NOT auto-load the
;; user init (so bb loads early-init + init explicitly), and it never runs
;; `after-init-hook' (so we drive elpaca directly instead of hooking its
;; settle).  `elpaca-wait' pumps the async queue to completion under --batch.
;; Changes land on disk; the live Emacs keeps its in-RAM code until restart.

(require 'cl-lib)
(require 'elpaca)

;; Defined in lisp/functions.el, which init loads before this driver can run.
(declare-function broken-elpaca-builds "functions")
(declare-function rebuild-broken-elpaca-builds "functions")
;; Shared audit-trail helpers sit next to this file.
(load (expand-file-name "elpaca-update-report"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
(load (expand-file-name "elpaca-local"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)

(defvar elpaca-update--snapshot nil
  "Pre-update map of source dir -> HEAD sha, captured before merging.")

;; Open the persistent append-log up front so even the earliest lines land in
;; it; nil (disabled or unwritable) just turns teeing into a no-op.
(defvar elpaca-update--persist (elpaca-update-report-open-session "headless")
  "Persistent append-log path this run tees to, or nil when disabled.")

(defun elpaca-update--out (text)
  "Print TEXT to stdout and mirror it into the persistent append-log.
The changelog and summary print at the very end, right before `kill-emacs'
flushes stdout - but without this tee the persistent log recorded headless
sessions with no verdict at all."
  (princ text)
  (elpaca-update-report-tee elpaca-update--persist (string-trim-right text "\n+")))

(defun elpaca-update--changelog ()
  "Print the commits pulled in for every changed package; return the count."
  (let ((seen (make-hash-table :test 'equal)) (n 0))
    (dolist (cell (elpaca--queued))
      (when-let* ((changes (elpaca-update-report-block-once
                            (cdr cell) elpaca-update--snapshot seen)))
        (when (zerop n) (elpaca-update--out "\nupdated packages:\n"))
        (cl-incf n)
        (elpaca-update--out (format "  %s\n%s\n"
                                    (elpaca-update-report--paint
                                     "1" (symbol-name (elpaca<-id (cdr cell))))
                                    changes))))
    n))

(defun elpaca-update--report (updated healed remaining)
  "Print a per-package status summary; return non-nil when nothing is wrong.
UPDATED is the count of packages that actually changed this run.  HEALED
is the (ID . REASON) list of broken builds this run rebuilt; REMAINING is
the post-heal re-scan - anything still broken fails the run alongside
failed/blocked statuses."
  (let* ((statuses (mapcar (lambda (e) (cons (car e) (elpaca<-status (cdr e))))
                           (elpaca--queued)))
         (failed (cl-remove-if-not (lambda (s) (memq (cdr s) '(failed blocked)))
                                   statuses)))
    (elpaca-update--out
     (format "\nupdate: %d processed, %d updated, %d failed/blocked%s\n"
             (length statuses) updated (length failed)
             (if healed (format ", %d healed" (length healed)) "")))
    (dolist (f failed) (elpaca-update--out (format "  %s: %s\n" (car f) (cdr f))))
    (dolist (h healed) (elpaca-update--out (format "  healed %s (%s)\n" (car h) (cdr h))))
    (dolist (r remaining)
      (elpaca-update--out
       (format "  STILL BROKEN %s (%s) - run bb repair\n" (car r) (cdr r))))
    (and (null failed) (null remaining))))

;; Live progress: --batch elpaca-wait spins silently, so stream each package to
;; stdout as it settles the same way the live driver tees to its logfile -
;; unchanged names batched into `pulled:' lines, failures broken out.  The
;; changelog at the end still prints the commits per changed package.
(defvar elpaca-update--batcher (elpaca-update-report-batcher)
  "Batcher accumulating settled package names into `pulled:' lines.")

(defvar elpaca-update--last-emit nil
  "`float-time' of the most recent emitted line; the silence heartbeat keys on it.")

(defun elpaca-update--emit (fmt &rest args)
  "Stream one progress line to stderr and mirror it to the persistent log.
stderr rather than stdout: under --batch stdout is block-buffered unless it is
a terminal, so progress `princ'd there would be invisible until the process
exits whenever `bb update' runs piped, redirected or from a compilation
buffer.  The end-of-run changelog and summary stay on stdout - they print just
before `kill-emacs', which flushes."
  (let ((line (apply #'format fmt args)))
    (setq elpaca-update--last-emit (float-time))
    (elpaca-update-report-progress line)
    (elpaca-update-report-tee elpaca-update--persist line)))

(defun elpaca-update--on-finished (e)
  "Batch E's id into the running `pulled:' line as it reaches `finished'."
  (elpaca-update-report-note elpaca-update--batcher #'elpaca-update--emit
                             (symbol-name (elpaca<-id e))))

(defun elpaca-update--on-failed (e)
  "Break out E as `failed:', flushing any pending `pulled:' names first."
  (elpaca-update-report-flush elpaca-update--batcher #'elpaca-update--emit)
  (elpaca-update--emit "%s" (elpaca-update-report--paint
                             "31" (concat "failed: " (symbol-name (elpaca<-id e))))))

;; Heartbeat: `elpaca-wait' pumps the queue by spinning in `sit-for', dead
;; silent through a slow compile or a wedged package.  A repeating timer fires
;; during that spin and reports what is still pending whenever nothing has been
;; emitted for the interval - keyed on SILENCE, not on the pending set stalling.
;; The first-run activation build settles packages steadily yet streams no
;; per-package line, so a stall-keyed heartbeat sat mute through it; a
;; silence-keyed one speaks up while a streaming phase (whose per-package lines
;; keep resetting the clock) stays quiet.  Default 10s, override with
;; ELPACA_UPDATE_HEARTBEAT.
(defvar elpaca-update--hb-timer nil "Repeating heartbeat timer, cancelled at the end.")
(defvar elpaca-update--hb-interval
  (max 1 (string-to-number (or (getenv "ELPACA_UPDATE_HEARTBEAT") "5")))
  "Seconds of silence before a heartbeat line is printed.")

(defun elpaca-update--heartbeat ()
  "Keep output flowing: flush a stale `pulled:' batch, else report pending work.
The stale flush wins ties - once it emits, the silence clock resets and the
heartbeat line stays quiet, so the heartbeat only ever speaks over a batcher
that is genuinely empty."
  (elpaca-update-report-flush-stale elpaca-update--batcher #'elpaca-update--emit
                                    elpaca-update--last-emit)
  (when-let* ((pending (elpaca-update-report-pending))
              (line (elpaca-update-report-heartbeat-line
                     pending elpaca-update--last-emit elpaca-update--hb-interval)))
    (elpaca-update--emit "%s" line)))

;; Watchdog: fetching+rebuilding every package is slow, but never hang the
;; caller forever.  Fires during `elpaca-wait's sit-for.  Default 30 min,
;; override with ELPACA_UPDATE_WATCHDOG.
(defvar elpaca-update--watchdog
  (run-at-time
   (string-to-number (or (getenv "ELPACA_UPDATE_WATCHDOG") "1800")) nil
   (lambda ()
     (elpaca-update--out
      "\nupdate: TIMEOUT - elpaca did not settle within the watchdog window\n")
     (kill-emacs 124)))
  "One-shot watchdog timer, cancelled once the run reaches a verdict.")

(condition-case err
    (progn
      ;; Start the heartbeat before the first wait so even a slow activation
      ;; reports what it is chewing on.
      (setq elpaca-update--last-emit (float-time)
            elpaca-update--hb-timer (run-at-time 2 2 #'elpaca-update--heartbeat))
      (elpaca-update--emit "update: activating installed packages (first run can be slow)...")
      (elpaca-wait)                ; settle the boot queue (activate everything)
      (setq elpaca-update--snapshot (elpaca-update-report-snapshot)) ; baseline HEADs
      (elpaca-update--emit "update: fetching + merging + rebuilding %d package(s)..."
                           (length (elpaca--queued)))
      ;; Stream each package as it settles - otherwise the wait below is a
      ;; multi-minute silent hang with no clue which package is in flight.
      (elpaca-subscribe 'finished #'elpaca-update--on-finished)
      (elpaca-subscribe 'failed #'elpaca-update--on-failed)
      ;; queue fetch+merge+rebuild for every package elpaca clones
      (elpaca-local-update-remotes nil nil #'elpaca-update--emit)
      (elpaca-wait)                ; settle the update queue (streamed as it goes)
      ;; A git update only rebuilds a package when its merge moved HEAD; a
      ;; build-in-place local checkout edited on disk (new files, edits) won't
      ;; have been.  Catch those and rebuild them before reporting - kept under
      ;; the same subscriptions, since a local rebuild can itself take a while.
      (when (elpaca-local-rebuild-changed
             (lambda (fmt &rest args)
               (elpaca-update-report-flush elpaca-update--batcher #'elpaca-update--emit)
               (apply #'elpaca-update--emit fmt args)))
        (elpaca-wait))
      ;; Heal what the update cannot see: a half-built or stale build whose
      ;; merge moved no HEAD is skipped by the rebuild step - and an earlier
      ;; interrupted run leaves exactly that behind.
      (let ((healed (rebuild-broken-elpaca-builds
                     (lambda (fmt &rest args)
                       (elpaca-update-report-flush elpaca-update--batcher
                                                   #'elpaca-update--emit)
                       (apply #'elpaca-update--emit fmt args)))))
        (when healed (elpaca-wait))
        (elpaca-unsubscribe 'finished #'elpaca-update--on-finished)
        (elpaca-unsubscribe 'failed #'elpaca-update--on-failed)
        (when (timerp elpaca-update--hb-timer) (cancel-timer elpaca-update--hb-timer))
        (when (timerp elpaca-update--watchdog) (cancel-timer elpaca-update--watchdog))
        (elpaca-update-report-flush elpaca-update--batcher #'elpaca-update--emit)
        ;; changelog prints the blocks and returns the changed count for the summary
        (kill-emacs (if (elpaca-update--report (elpaca-update--changelog)
                                               healed (broken-elpaca-builds))
                        0 1))))
  (error
   (when (timerp elpaca-update--hb-timer) (cancel-timer elpaca-update--hb-timer))
   (elpaca-update--out (format "\nupdate: ERROR - %s\n" (error-message-string err)))
   (kill-emacs 1)))
