;;; scripts/smoke-check.el --- smoke-boot probe -*- lexical-binding: t; -*-
;; The AGENTS.md smoke-boot pattern, formalized.  Load on top of a full boot:
;;   emacs -nw --init-directory <root> -l scripts/smoke-check.el
;; Once elpaca settles, writes a result marker (SMOKE_RESULT_FILE env, or
;; /tmp/emacs-lab-smoke-result) with elpaca statuses and any *Warnings*,
;; then kills Emacs.  `bb smoke' drives this and reads the marker.

(require 'cl-lib)

(defvar smoke-result-file
  (or (getenv "SMOKE_RESULT_FILE") "/tmp/emacs-lab-smoke-result"))

(defvar smoke-tolerated-packages '(khalendario)
  "Own packages whose repos are private.
Without ssh keys (CI) their anonymous https clone can only fail, so their
failed/blocked status is reported but does not flip the verdict.  On a dev
machine the local checkout makes them build normally, keeping this list
inert there.")

;; Emacs 30.1's tty redisplay can segfault while padding frame glyph rows
;; (fill_up_frame_row_with_spaces <- build_frame_matrix_from_window_tree,
;; symbolized from the exact CI binary) when elpaca's install UI redraws
;; during boot.  Rendering was never this probe's signal - packages,
;; config load and warnings are - so skip redisplay wholesale rather than
;; tiptoe around an upstream C bug.  The marker is file-based; nothing
;; here needs a drawn frame.
(setq inhibit-redisplay t)

(defun smoke-report (statuses init-error warnings tolerated)
  "Render the verdict and result-marker text as (OK . TEXT).
STATUSES is an alist of (PACKAGE . STATUS); INIT-ERROR mirrors
`init-file-had-error'; WARNINGS is the *Warnings* buffer text or nil.
A failed/blocked package listed in TOLERATED is reported but does not
flip the verdict - any other failure, or INIT-ERROR, does."
  (let* ((failed (cl-remove-if-not
                  (lambda (s) (memq (cdr s) '(failed blocked)))
                  statuses))
         (fatal (cl-remove-if (lambda (s) (memq (car s) tolerated)) failed))
         (waved (cl-remove-if-not (lambda (s) (memq (car s) tolerated)) failed))
         (ok (and (null fatal) (not init-error))))
    (cons ok
          (concat
           (if ok "SMOKE-OK\n" "SMOKE-FAILED\n")
           (format "packages: %d queued, %d failed/blocked%s\n"
                   (length statuses) (length fatal)
                   (if waved
                       (format ", %d tolerated (private)" (length waved))
                     ""))
           (when init-error "init.el signaled an error during startup\n")
           (mapconcat (lambda (f) (format "  %s: %s\n" (car f) (cdr f)))
                      fatal "")
           (mapconcat (lambda (f) (format "  %s: %s (tolerated)\n" (car f) (cdr f)))
                      waved "")
           (when warnings (concat "*Warnings*:\n" warnings))))))

(defun smoke-write-result ()
  "Dump elpaca statuses + warnings to `smoke-result-file' and exit."
  (pcase-let ((`(,ok . ,text)
               (smoke-report
                (mapcar (lambda (entry)
                          (cons (car entry) (elpaca<-status (cdr entry))))
                        (elpaca--queued))
                init-file-had-error
                (when-let* ((buf (get-buffer "*Warnings*")))
                  (with-current-buffer buf (buffer-string)))
                smoke-tolerated-packages)))
    (with-temp-file smoke-result-file
      (insert text))
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
