;;; scripts/elpaca-repair.el --- heal botched package state -*- lexical-binding: t; -*-
;; The doom-sync equivalent for broken packages.  Load on top of a full
;; --batch boot (same pattern as elpaca-update.el):
;;   emacs -Q --batch --init-directory <root> \
;;     -l <root>/early-init.el -l <root>/init.el -l <root>/scripts/elpaca-repair.el
;; Driven by `bb repair'.  The fresh boot itself re-clones deleted repos and
;; rebuilds deleted builds (elpaca's normal activation); on top of that this
;; rebuilds every package the integrity scans flag - half-built (linked
;; sources, no autoloads) and stale (source newer than .elc) - then re-scans
;; and reports.  The running Emacs keeps its in-RAM state: restart after.
;; The driver only runs when elpaca is present, so the test sandbox can load
;; this file for the report function alone.

(require 'cl-lib)

;; Defined in lisp/functions.el, which init loads before the driver can run.
(declare-function broken-elpaca-builds "functions")
(declare-function rebuild-broken-elpaca-builds "functions")

(defun elpaca-repair-report (statuses rebuilt remaining)
  "Render the repair verdict and report text as (OK . TEXT).
STATUSES is an alist of (PACKAGE . STATUS).  REBUILT is an alist of
\(PACKAGE . REASON) for packages this run rebuilt; REASON is `half-built'
or `stale'.  REMAINING is the same shape from the post-rebuild re-scan -
anything still broken, or any failed/blocked status, flips the verdict."
  (let* ((failed (cl-remove-if-not
                  (lambda (s) (memq (cdr s) '(failed blocked)))
                  statuses))
         (ok (and (null failed) (null remaining))))
    (cons ok
          (concat
           (if ok "REPAIR-OK\n" "REPAIR-FAILED\n")
           (format "packages: %d processed, %d failed/blocked, %d rebuilt\n"
                   (length statuses) (length failed) (length rebuilt))
           (mapconcat (lambda (f) (format "  %s: %s\n" (car f) (cdr f)))
                      failed "")
           (mapconcat (lambda (r) (format "  rebuilt %s (%s)\n" (car r) (cdr r)))
                      rebuilt "")
           (mapconcat (lambda (r) (format "  STILL BROKEN %s (%s)\n" (car r) (cdr r)))
                      remaining "")
           (when (and ok (null rebuilt))
             "nothing to repair\n")))))

(when (featurep 'elpaca)
  ;; arms the deeper `dirty' scan in broken-elpaca-builds (merged sources
  ;; the build never saw - what a killed update leaves behind)
  (load (expand-file-name "elpaca-local"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil 'nomessage)
  (defvar elpaca-repair--watchdog
    (run-at-time
     (string-to-number (or (getenv "ELPACA_REPAIR_WATCHDOG") "900")) nil
     (lambda ()
       (princ "\nrepair: TIMEOUT - elpaca did not settle within the watchdog window\n")
       (kill-emacs 124))))
  (princ "repair: settling the boot queue (re-clones/rebuilds anything missing; can take a while)...\n")
  (elpaca-wait)
  ;; scan + queue live in lisp/functions.el, shared with the update drivers
  (let ((broken (rebuild-broken-elpaca-builds
                 (lambda (fmt &rest args)
                   (princ (concat "repair: " (apply #'format fmt args) "\n"))))))
    (when broken (elpaca-wait))
    (when (timerp elpaca-repair--watchdog)
      (cancel-timer elpaca-repair--watchdog))
    (pcase-let ((`(,ok . ,text)
                 (elpaca-repair-report
                  (mapcar (lambda (e) (cons (car e) (elpaca<-status (cdr e))))
                          (elpaca--queued))
                  broken
                  (broken-elpaca-builds))))
      (princ text)
      (kill-emacs (if ok 0 1)))))
