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

(defvar elpaca-update--snapshot nil
  "Pre-update map of source dir -> HEAD sha, captured before merging.")

;; Color when bb signals our stdout is a terminal.
(setq elpaca-update-report-color (and (getenv "ELPACA_UPDATE_COLOR") t))

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
      (elpaca-wait)                ; settle the boot queue (activate everything)
      (setq elpaca-update--snapshot (elpaca-update-report-snapshot)) ; baseline HEADs
      (elpaca-update-all)          ; queue fetch+merge+rebuild for every package
      (elpaca-wait)                ; settle the update queue
      ;; changelog prints the blocks and returns the changed count for the summary
      (kill-emacs (if (elpaca-update--report (elpaca-update--changelog)) 0 1)))
  (error
   (princ (format "\nupdate: ERROR - %s\n" (error-message-string err)))
   (kill-emacs 1)))
