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

(defun elpaca-update--report ()
  "Print a per-package status summary; return non-nil when none failed."
  (let* ((statuses (mapcar (lambda (e) (cons (car e) (elpaca<-status (cdr e))))
                           (elpaca--queued)))
         (failed (cl-remove-if-not (lambda (s) (memq (cdr s) '(failed blocked)))
                                   statuses)))
    (princ (format "\nupdate: %d packages processed, %d failed/blocked\n"
                   (length statuses) (length failed)))
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
      (elpaca-update-all)          ; queue fetch+merge+rebuild for every package
      (elpaca-wait)                ; settle the update queue
      (kill-emacs (if (elpaca-update--report) 0 1)))
  (error
   (princ (format "\nupdate: ERROR - %s\n" (error-message-string err)))
   (kill-emacs 1)))
