;;; general/autoload/avy.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A which-key-style guide for avy's dispatch actions - the keys that turn a
;; jump into a yank, teleport, kill or embark call.  avy is not required here
;; so the file stays loadable in the bare -Q batch test env; its dynamic vars
;; are forward-declared so the references stay dynamic under byte-compilation.
;;; Code:

;; avy's dynamic vars are only special once avy loads.
(defvar avy-action)
(defvar avy-dispatch-alist)
(defvar avy-single-candidate-jump)

(defvar avy-dispatch-guide-delay 0.5
  "Seconds of hesitation before the dispatch guide appears.
Matches `which-key-idle-delay' so both popups answer to the same pause.")

(defvar avy-dispatch-guide--timer nil
  "Pending guide timer, cancelled when the selection ends.")

(defun avy-dispatch-action-name (action)
  "Short label for ACTION, a value from `avy-dispatch-alist'."
  (let ((name (if (symbolp action) (symbol-name action) "custom")))
    (if (string-prefix-p "avy-action-" name)
        (substring name (length "avy-action-"))
      name)))

;;;###autoload
(defun avy-dispatch-guide ()
  "Echo avy's dispatch keys, or the action already armed.
Once a dispatch key is pressed avy repaints the candidates with no sign
of what it will do with the one picked, so that state gets its own line."
  (message
   "%s"
   (if avy-action
       (format "%s: pick a candidate"
               (propertize (avy-dispatch-action-name avy-action)
                           'face 'avy-lead-face))
     (mapconcat (lambda (entry)
                  (format "%s %s"
                          (propertize (char-to-string (car entry))
                                      'face 'avy-lead-face)
                          (avy-dispatch-action-name (cdr entry))))
                avy-dispatch-alist "  "))))

;;;###autoload
(defun avy-dispatch-guide-a (fn candidates &rest args)
  "Guide the dispatch keys while FN picks among CANDIDATES, given ARGS.
Around advice for `avy--process-1', the one place all three selection
styles funnel through.  It blocks in `read-key' with no command loop
running, so a timer is the only thing that can still paint - which buys
the which-key ergonomics for free: hesitate and the keys appear, keep
typing and they never do."
  (let ((shown nil))
    (unwind-protect
        (progn
          ;; the cases where `avy--process-1' really enters a read loop
          (when (and candidates
                     (not (and (null (cdr candidates))
                               avy-single-candidate-jump)))
            (setq avy-dispatch-guide--timer
                  (run-with-timer avy-dispatch-guide-delay nil
                                  (lambda ()
                                    (setq shown t)
                                    (avy-dispatch-guide)))))
          (apply fn candidates args))
      (when (timerp avy-dispatch-guide--timer)
        (cancel-timer avy-dispatch-guide--timer))
      (setq avy-dispatch-guide--timer nil)
      (when shown (message nil)))))
