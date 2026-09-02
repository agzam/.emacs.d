;;; scripts/e2e-check.el --- end-to-end flow probe -*- lexical-binding: t; -*-
;; Load on top of a full boot:
;;   emacs -nw --init-directory <root> -l scripts/e2e-check.el
;; Once elpaca settles, runs every scenario registered by tests/e2e/*.el,
;; writes a transcript marker (E2E_RESULT_FILE env, or
;; /tmp/emacs-lab-e2e-result) and kills Emacs.  `bb e2e' drives this and
;; reads the marker.
;;
;; The buttercup suites call commands directly in a `with-temp-buffer',
;; which is a replica of a buffer rather than one: no major mode has been
;; turned on, nothing is fontified, and the keymaps that reach the command
;; are never consulted.  Bugs that live in what mode activation does to a
;; buffer, or in the wiring between a target and its keymap, are invisible
;; to every spec written that way.  This tier exists for exactly those:
;; real files in their own modes, fontified, acted on by real keypresses.

(require 'cl-lib)

(defvar e2e-result-file
  (or (getenv "E2E_RESULT_FILE") "/tmp/emacs-lab-e2e-result"))

(defvar e2e-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the config being probed, derived from this file's location.")

(defvar e2e-scenarios nil
  "Functions returning lists of result plists, newest first.
Scenario files under tests/e2e/ add themselves here; each is free to set
up whatever the flow it covers needs (stubs, fixtures) and runs its cases
through `e2e-act-case'.")

(defvar e2e-work-dir nil
  "Throwaway directory holding the case files, created per run.")

;; Emacs 30.1's tty redisplay can segfault while padding frame glyph rows
;; when a package's UI redraws during boot (see scripts/smoke-check.el).
;; Nothing here needs a drawn frame - the marker is file-based.
(setq inhibit-redisplay t)

(defun e2e-act-case (case)
  "Run CASE through a real `embark-act' and return a result plist.
CASE is a plist of :label, :ext (picks the major mode through
`auto-mode-alist'), :text, :search (point lands after it), :keys to
press once embark is up, :type the embark target expected under point,
:want the buffer contents the action must produce, and an optional
:probe called in the buffer whose value joins the transcript.
The buffer is displayed on purpose: embark acts in the window showing
the target, so an undisplayed one sends the action to whatever the
selected window holds."
  (let* ((file (expand-file-name (format "case.%s" (plist-get case :ext))
                                 e2e-work-dir))
         (buf (find-file-noselect file))
         (keys (plist-get case :keys))
         err)
    (unwind-protect
        (with-current-buffer buf
          (switch-to-buffer buf)
          (delete-other-windows)
          (erase-buffer)
          (insert (plist-get case :text))
          (font-lock-ensure)
          (goto-char (point-min))
          (search-forward (plist-get case :search))
          (let* ((type (plist-get (car (embark--targets)) :type))
                 (cmd (ignore-errors
                        (lookup-key (embark--action-keymap type nil) (kbd keys))))
                 (probe (when-let* ((f (plist-get case :probe))) (funcall f))))
            (condition-case e
                (let ((unread-command-events (listify-key-sequence (kbd keys))))
                  (embark-act))
              (error (setq err e)))
            ;; embark leaves which-key's own buffer current, so read the
            ;; result from inside the case buffer rather than assume it
            (let ((got (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max)))))
              (list :label (plist-get case :label)
                    :mode (buffer-local-value 'major-mode buf)
                    :keys keys :probe probe :cmd cmd
                    :type type :want-type (plist-get case :type)
                    :got got :want (plist-get case :want) :err err
                    :ok (and (null err)
                             (eq type (plist-get case :type))
                             (equal got (plist-get case :want)))))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(defun e2e-format-result (r)
  "One transcript entry for result plist R."
  (concat
   (format "%s %s\n" (if (plist-get r :ok) "PASS" "FAIL") (plist-get r :label))
   (when (plist-get r :type)
     (format "     mode=%s target=%s (want %s) keys=%S -> %s%s\n"
             (plist-get r :mode) (plist-get r :type) (plist-get r :want-type)
             (plist-get r :keys) (plist-get r :cmd)
             (if-let* ((probe (plist-get r :probe)))
                 (format " probe=%s" probe)
               "")))
   (when (plist-get r :got) (format "     got:  %S\n" (plist-get r :got)))
   (unless (equal (plist-get r :got) (plist-get r :want))
     (format "     want: %S\n" (plist-get r :want)))
   (when (plist-get r :err) (format "     error: %S\n" (plist-get r :err)))))

(defun e2e-load-scenarios ()
  "Load every scenario file, returning a failure entry per file that died.
A scenario that cannot even load must fail the run loudly; silently
running fewer cases is the failure mode this whole tier exists to end."
  (let ((dir (expand-file-name "tests/e2e/" e2e-root))
        failures)
    (dolist (file (and (file-directory-p dir)
                       (directory-files dir t "\\.el\\'")))
      (condition-case e
          (load file nil 'nomessage)
        (error (push (list :label (format "loading %s" (file-name-nondirectory file))
                           :err e :ok nil)
                     failures))))
    failures))

(defun e2e-run-scenarios ()
  "Results of every registered scenario, in registration order."
  (mapcan
   (lambda (scenario)
     (condition-case e
         (funcall scenario)
       (error (list (list :label (format "%s signalled" scenario) :err e :ok nil)))))
   (reverse e2e-scenarios)))

(defun e2e-report (results)
  "Render the verdict and marker text for RESULTS as (OK . TEXT).
No results at all is a failure: an empty run reads as a pass otherwise."
  (let* ((failed (cl-remove-if (lambda (r) (plist-get r :ok)) results))
         (ok (and results (null failed))))
    (cons ok
          (concat (if ok "E2E-OK\n" "E2E-FAILED\n")
                  (format "%d cases, %d failed\n\n" (length results) (length failed))
                  (mapconcat #'e2e-format-result results "")
                  (when-let* ((buf (get-buffer "*Warnings*")))
                    (concat "\n*Warnings*:\n"
                            (with-current-buffer buf (buffer-string))))))))

(defun e2e-write-result (results)
  "Write the RESULTS marker and exit."
  (pcase-let ((`(,ok . ,text) (e2e-report results)))
    (with-temp-file e2e-result-file (insert text))
    (kill-emacs (if ok 0 1))))

(defun e2e-prewarm ()
  "Force cold-start side effects before any scenario runs.
On a fresh machine the first text-mode buffer makes jinx compile its
native module: the compilation buffer pops up mid-scenario and the
window it steals eats the keys of whichever act is running.  The
compile is synchronous (jinx.el `call-process'), so triggering it here
absorbs pop and wait both.  Best-effort: a config without jinx runs on."
  (ignore-errors
    (when (require 'jinx nil t)
      (with-temp-buffer
        (text-mode)
        (jinx-mode 1)
        (jinx-mode -1))))
  ;; The Linux pty leaves stray events queued from terminal init (a
  ;; switch-frame event and a NUL byte that runs `set-mark-command');
  ;; the first scenario that reads input would drain them into its own
  ;; key stream - a NUL-set mark flips evil to visual state and every
  ;; key after it dispatches under the wrong maps.  Flush them here.
  (discard-input))

(defun e2e-run ()
  "Load the scenarios, run them, write the marker, exit."
  (let (results)
    (unwind-protect
        (progn
          (setq e2e-work-dir
                (file-name-as-directory (make-temp-file "emacs-lab-e2e" t)))
          (e2e-prewarm)
          (setq results (append (e2e-load-scenarios) (e2e-run-scenarios))))
      (when (and e2e-work-dir (file-directory-p e2e-work-dir))
        (delete-directory e2e-work-dir t))
      (e2e-write-result results))))

(defvar e2e-watchdog-seconds
  (string-to-number (or (getenv "E2E_WATCHDOG") "600")))

;; Armed before the run is dispatched: on a warm cache `e2e-run' executes
;; right here during load and never returns (it kills Emacs), so anything
;; placed after the dispatch would not evaluate.  The timer fires from
;; inside a stalled minibuffer read too - that is what bounds a scenario
;; whose keys never exit the read.
(run-at-time
 e2e-watchdog-seconds nil
 (lambda ()
   (with-temp-file e2e-result-file
     (insert (format "E2E-TIMEOUT\nno verdict within %ss (boot never settled or a scenario stalled)\n"
                     e2e-watchdog-seconds)))
   (kill-emacs 124)))

;; -l files load after `after-init-hook', so on a warm cache elpaca may
;; already be done by the time this runs.  An init.el error that aborted
;; before the queue was armed means `elpaca-after-init-hook' can never
;; fire; run anyway and let the scenarios report against the half-built
;; config instead of idling into the watchdog.
(if (or (bound-and-true-p elpaca-after-init-time)
        (and init-file-had-error
             (not (memq 'elpaca-process-queues after-init-hook))))
    (e2e-run)
  (add-hook 'elpaca-after-init-hook #'e2e-run 99))
