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
;; Shared audit-trail helpers sit next to this file.
(load (expand-file-name "elpaca-update-report"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
(load (expand-file-name "elpaca-local"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)

(defvar elpaca-update--snapshot nil
  "Pre-update map of source dir -> HEAD sha, captured before merging.")

;; Color when bb signals our stdout is a terminal.
(setq elpaca-update-report-color (and (getenv "ELPACA_UPDATE_COLOR") t))

;; Open the persistent append-log up front so even the earliest lines land in
;; it; nil (disabled or unwritable) just turns teeing into a no-op.
(defvar elpaca-update--persist (elpaca-update-report-open-session "headless")
  "Persistent append-log path this run tees to, or nil when disabled.")

(defun elpaca-update--changelog ()
  "Print the commits pulled in for every changed package; return the count."
  (let ((seen (make-hash-table :test 'equal)) (n 0))
    (when (hash-table-p elpaca-update--snapshot)
      (dolist (cell (elpaca--queued))
        (let* ((e (cdr cell))
               (dir (ignore-errors (elpaca<-source-dir e))))
          (when (and dir (not (gethash dir seen)))
            (when-let* ((changes (elpaca-update-report-block e elpaca-update--snapshot)))
              (puthash dir t seen)
              (when (zerop n) (princ "\nupdated packages:\n"))
              (cl-incf n)
              (princ (format "  %s\n%s\n"
                             (elpaca-update-report--paint
                              "1" (symbol-name (elpaca<-id e)))
                             changes)))))))
    n))

(defun elpaca-update--report (updated)
  "Print a per-package status summary; return non-nil when none failed.
UPDATED is the count of packages that actually changed this run."
  (let* ((statuses (mapcar (lambda (e) (cons (car e) (elpaca<-status (cdr e))))
                           (elpaca--queued)))
         (failed (cl-remove-if-not (lambda (s) (memq (cdr s) '(failed blocked)))
                                   statuses)))
    (princ (format "\nupdate: %d processed, %d updated, %d failed/blocked\n"
                   (length statuses) updated (length failed)))
    (dolist (f failed) (princ (format "  %s: %s\n" (car f) (cdr f))))
    (null failed)))

;; Live progress: --batch elpaca-wait spins silently, so stream each package to
;; stdout as it settles the same way the live driver tees to its logfile -
;; unchanged names batched into `pulled:' lines, failures broken out.  The
;; changelog at the end still prints the commits per changed package.
(defvar elpaca-update--batcher (elpaca-update-report-batcher)
  "Batcher accumulating settled package names into `pulled:' lines.")

(defun elpaca-update--emit (fmt &rest args)
  "Print one progress line to stdout and mirror it to the persistent log."
  (let ((line (apply #'format fmt args)))
    (princ (concat line "\n"))
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
;; during that spin and says what is still pending - but only once the pending
;; set has sat unchanged for the interval, so an actively-settling queue (whose
;; `finished' events already stream) stays quiet.  Default 10s, override with
;; ELPACA_UPDATE_HEARTBEAT.
(defvar elpaca-update--hb-timer nil "Repeating heartbeat timer, cancelled at the end.")
(defvar elpaca-update--hb-sig nil "Pending signature when it last changed.")
(defvar elpaca-update--hb-change nil "`float-time' when the pending signature last changed.")
(defvar elpaca-update--hb-interval
  (max 1 (string-to-number (or (getenv "ELPACA_UPDATE_HEARTBEAT") "10")))
  "Seconds the pending set may sit unchanged before a heartbeat line is printed.")

(defun elpaca-update--heartbeat ()
  "Print pending work when nothing has settled for `elpaca-update--hb-interval'."
  (when-let* ((pending (elpaca-update-report-pending))
              (sig (elpaca-update-report-format-pending pending))
              (now (float-time)))
    (unless elpaca-update--hb-change (setq elpaca-update--hb-change now))
    (cond
     ((not (equal sig elpaca-update--hb-sig))
      (setq elpaca-update--hb-sig sig elpaca-update--hb-change now))
     ((>= (- now elpaca-update--hb-change) elpaca-update--hb-interval)
      (elpaca-update-report-flush elpaca-update--batcher #'elpaca-update--emit)
      (elpaca-update--emit "still working (%ds, %d pending): %s"
                           (round (- now elpaca-update--hb-change))
                           (length pending) sig)
      (setq elpaca-update--hb-change now)))))

;; Watchdog: fetching+rebuilding every package is slow, but never hang the
;; caller forever.  Fires during `elpaca-wait's sit-for.  Default 30 min,
;; override with ELPACA_UPDATE_WATCHDOG.
(run-at-time
 (string-to-number (or (getenv "ELPACA_UPDATE_WATCHDOG") "1800")) nil
 (lambda ()
   (princ "\nupdate: TIMEOUT - elpaca did not settle within the watchdog window\n")
   (kill-emacs 124)))

(condition-case err
    (progn
      ;; Start the heartbeat before the first wait so even a slow activation
      ;; reports what it is chewing on.
      (setq elpaca-update--hb-timer (run-at-time 2 2 #'elpaca-update--heartbeat))
      (elpaca-update--emit "update: activating installed packages (first run can be slow)...")
      (elpaca-wait)                ; settle the boot queue (activate everything)
      (setq elpaca-update--snapshot (elpaca-update-report-snapshot)) ; baseline HEADs
      (elpaca-update--emit "update: fetching + merging + rebuilding %d package(s)..."
                           (length (elpaca--queued)))
      ;; Stream each package as it settles - otherwise the wait below is a
      ;; multi-minute silent hang with no clue which package is in flight.
      (elpaca-subscribe 'finished #'elpaca-update--on-finished)
      (elpaca-subscribe 'failed #'elpaca-update--on-failed)
      (elpaca-update-all)          ; queue fetch+merge+rebuild for every package
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
      (elpaca-unsubscribe 'finished #'elpaca-update--on-finished)
      (elpaca-unsubscribe 'failed #'elpaca-update--on-failed)
      (when (timerp elpaca-update--hb-timer) (cancel-timer elpaca-update--hb-timer))
      (elpaca-update-report-flush elpaca-update--batcher #'elpaca-update--emit)
      ;; changelog prints the blocks and returns the changed count for the summary
      (kill-emacs (if (elpaca-update--report (elpaca-update--changelog)) 0 1)))
  (error
   (when (timerp elpaca-update--hb-timer) (cancel-timer elpaca-update--hb-timer))
   (princ (format "\nupdate: ERROR - %s\n" (error-message-string err)))
   (kill-emacs 1)))
