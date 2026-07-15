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
(defvar elpaca-live-update--pulled nil
  "Names of unchanged packages accumulated for the next `pulled:' line.")
(defconst elpaca-live-update--pulled-width 72
  "Flush the pending `pulled:' line once it would grow past this many columns.")

(defun elpaca-live-update--emit (fmt &rest args)
  "Append a formatted line to the progress file."
  (when elpaca-live-update--log
    (write-region (concat (apply #'format fmt args) "\n")
                  nil elpaca-live-update--log 'append 'silent)))

(defun elpaca-live-update--flush-pulled ()
  "Emit and clear any pending unchanged-package names as one `pulled:' line."
  (when elpaca-live-update--pulled
    (elpaca-live-update--emit "pulled: %s"
                              (string-join (nreverse elpaca-live-update--pulled) ", "))
    (setq elpaca-live-update--pulled nil)))

(defun elpaca-live-update--note-pulled (name)
  "Add NAME to the pending `pulled:' line, flushing first if it would overflow.
Batching keeps the stream alive - names still appear promptly - without
spending a whole line on each of the (usually many) unchanged packages."
  (when (and elpaca-live-update--pulled     ; 8 = width of the "pulled: " prefix
             (< elpaca-live-update--pulled-width
                (+ 8 (length (string-join (cons name elpaca-live-update--pulled) ", ")))))
    (elpaca-live-update--flush-pulled))
  (push name elpaca-live-update--pulled))

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

(defun elpaca-live-update--terminal-p ()
  "Non-nil once every queued package has reached a terminal status."
  (cl-every (lambda (q)
              (cl-every (lambda (cell)
                          (memq (elpaca<-status (cdr cell)) '(finished failed)))
                        (elpaca-q<-elpacas q)))
            elpaca--queues))

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
        elpaca-live-update--pulled nil))

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
              elpaca-live-update--pulled nil)
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
              (run-at-time 1 1 (lambda ()
                                 (when (elpaca-live-update--terminal-p)
                                   (elpaca-live-update--finish)))))
        t)
    (error
     (elpaca-live-update--emit "UPDATE-ERROR %s" (error-message-string err))
     (elpaca-live-update--cleanup)
     nil)))
