;;; scripts/smoke-check.el --- smoke-boot probe -*- lexical-binding: t; -*-
;; The AGENTS.md smoke-boot pattern, formalized.  Load on top of a full boot:
;;   emacs -nw --init-directory <root> -l scripts/smoke-check.el
;; Once elpaca settles, writes a result marker (SMOKE_RESULT_FILE env, or
;; /tmp/emacs-lab-smoke-result) with elpaca statuses and any *Warnings*,
;; then kills Emacs.  `bb smoke' drives this and reads the marker.

(defvar smoke-result-file
  (or (getenv "SMOKE_RESULT_FILE") "/tmp/emacs-lab-smoke-result"))

(defun smoke-write-result ()
  "Dump elpaca statuses + warnings to `smoke-result-file' and exit."
  (let* ((statuses (mapcar (lambda (entry)
                             (cons (car entry) (elpaca<-status (cdr entry))))
                           (elpaca--queued)))
         (failed (cl-remove-if-not
                  (lambda (s) (memq (cdr s) '(failed blocked)))
                  statuses))
         (warnings (when-let* ((buf (get-buffer "*Warnings*")))
                     (with-current-buffer buf (buffer-string))))
         (ok (and (null failed) (not init-file-had-error))))
    (with-temp-file smoke-result-file
      (insert (if ok "SMOKE-OK\n" "SMOKE-FAILED\n"))
      (insert (format "packages: %d queued, %d failed/blocked\n"
                      (length statuses) (length failed)))
      (when init-file-had-error
        (insert "init.el signaled an error during startup\n"))
      (dolist (f failed)
        (insert (format "  %s: %s\n" (car f) (cdr f))))
      (when warnings
        (insert "*Warnings*:\n" warnings)))
    (kill-emacs (if ok 0 1))))

;; -l files load after `after-init-hook', so on a warm cache elpaca may
;; already be done by the time this file runs - check, don't just hook.
(if (bound-and-true-p elpaca-after-init-time)
    (smoke-write-result)
  (add-hook 'elpaca-after-init-hook #'smoke-write-result 99))

;; Watchdog: if elpaca never settles (network hang, blocked queue), still
;; produce a result instead of hanging the caller.
(defvar smoke-watchdog-seconds
  (string-to-number (or (getenv "SMOKE_WATCHDOG") "600")))

(run-at-time
 smoke-watchdog-seconds nil
 (lambda ()
   (with-temp-file smoke-result-file
     (insert (format "SMOKE-TIMEOUT\nelpaca-after-init-hook never fired within %ss\n"
                     smoke-watchdog-seconds)))
   (kill-emacs 124)))
