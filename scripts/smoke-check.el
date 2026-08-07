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

(defun smoke-report (statuses init-error warnings tolerated &optional half-built logs)
  "Render the verdict and result-marker text as (OK . TEXT).
STATUSES is an alist of (PACKAGE . STATUS); INIT-ERROR mirrors
`init-file-had-error'; WARNINGS is the *Warnings* buffer text or nil.
A failed/blocked package listed in TOLERATED is reported but does not
flip the verdict - any other failure, INIT-ERROR, or an entry in
HALF-BUILT (see `half-built-elpaca-packages': finished status hiding a
build dir with no autoloads) does.  LOGS is an alist of (PACKAGE .
LINES); LINES print indented under the matching fatal entry, so the CI
log answers why a package failed (git stderr, build error) without a
second run."
  (let* ((failed (cl-remove-if-not
                  (lambda (s) (memq (cdr s) '(failed blocked)))
                  statuses))
         (fatal (cl-remove-if (lambda (s) (memq (car s) tolerated)) failed))
         (waved (cl-remove-if-not (lambda (s) (memq (car s) tolerated)) failed))
         (ok (and (null fatal) (null half-built) (not init-error))))
    (cons ok
          (concat
           (if ok "SMOKE-OK\n" "SMOKE-FAILED\n")
           (format "packages: %d queued, %d failed/blocked%s%s\n"
                   (length statuses) (length fatal)
                   (if waved
                       (format ", %d tolerated (private)" (length waved))
                     "")
                   (if half-built
                       (format ", %d half-built" (length half-built))
                     ""))
           (when init-error "init.el signaled an error during startup\n")
           (mapconcat (lambda (f)
                        (concat
                         (format "  %s: %s\n" (car f) (cdr f))
                         (mapconcat (lambda (line) (format "      %s\n" line))
                                    (alist-get (car f) logs) "")))
                      fatal "")
           (mapconcat (lambda (f) (format "  %s: %s (tolerated)\n" (car f) (cdr f)))
                      waved "")
           (mapconcat (lambda (b) (format "  %s: half-built (no autoloads in %s)\n"
                                          (car b) (cdr b)))
                      half-built "")
           (when warnings (concat "*Warnings*:\n" warnings))))))

(defun smoke-package-events (id &optional n)
  "Readable `:info' lines from package ID's last N elpaca events, oldest first.
Multi-line infos (git stderr) are split so the report can indent each
line.  Nil when elpaca (or its event log) never loaded."
  (when (and (boundp 'elpaca--event-log) (fboundp 'elpaca-event<-id))
    (let (infos)
      (cl-loop for ev in elpaca--event-log
               while (< (length infos) (or n 4))
               when (eq (elpaca-event<-id ev) id)
               do (when-let* ((info (plist-get (elpaca-event<-payload ev) :info)))
                    (push info infos)))
      (mapcan (lambda (info) (split-string info "\n" t)) infos))))

(defun smoke-write-result ()
  "Dump elpaca statuses + warnings to `smoke-result-file' and exit.
Elpaca may be absent entirely (init died in the bootstrap); the report
then carries the init error and *Warnings* alone."
  (let* ((statuses (when (fboundp 'elpaca--queued)
                     (mapcar (lambda (entry)
                               (cons (car entry) (elpaca<-status (cdr entry))))
                             (elpaca--queued))))
         (logs (cl-loop for (id . status) in statuses
                        when (memq status '(failed blocked))
                        collect (cons id (smoke-package-events id)))))
    (pcase-let ((`(,ok . ,text)
                 (smoke-report
                  statuses
                  init-file-had-error
                  (when-let* ((buf (get-buffer "*Warnings*")))
                    (with-current-buffer buf (buffer-string)))
                  smoke-tolerated-packages
                  (when (fboundp 'half-built-elpaca-packages)
                    (half-built-elpaca-packages))
                  logs)))
      (with-temp-file smoke-result-file
        (insert text))
      (kill-emacs (if ok 0 1)))))

(defun smoke-should-report-now-p (settled init-error queue-armed)
  "Non-nil when the verdict is already decided at probe load time.
SETTLED: elpaca finished before this file loaded (warm cache).
INIT-ERROR mirrors `init-file-had-error'; QUEUE-ARMED whether init.el
got far enough to put `elpaca-process-queues' on `after-init-hook'.
An init error with the queue never armed means `elpaca-after-init-hook'
cannot fire - waiting only runs out the watchdog."
  (or settled (and init-error (not queue-armed))))

;; -l files load after `after-init-hook', so on a warm cache elpaca may
;; already be done by the time this file runs - check, don't just hook.
;; An init.el error can also abort before elpaca queue processing is
;; armed; report right away instead of idling into the watchdog.
(if (smoke-should-report-now-p
     (bound-and-true-p elpaca-after-init-time)
     init-file-had-error
     (memq 'elpaca-process-queues after-init-hook))
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
