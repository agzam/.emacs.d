;;; lisp/elpaca-failures.el --- fail every dependent of a failed package -*- lexical-binding: t; -*-

;; `elpaca-resolve' fails the packages waiting on a failed dependency one by
;; one through `elpaca--fail', which ends by signalling the build error that
;; aborts a build step.  The signal escapes the loop after the first waiter:
;; every other waiter keeps a condition that is already gone from the table,
;; nothing resolves it, and the queue never completes - a boot idles until
;; the watchdog, and the failed package's own reason is cut off by the same
;; signal before it is noted.  Here each waiter fails in its own call, with
;; the signal contained, and the caller's own signal still aborts its step.

(defvar elpaca--conditions)

(defun elpaca-resolve-fail-every-waiter-a (fn type value &optional reason)
  "Apply FN to TYPE, VALUE and REASON once per waiter when REASON is set.
Around `elpaca-resolve'.  Without REASON the condition is satisfied and
FN handles all waiters at once, as it always did."
  (if (not reason)
      (funcall fn type value)
    (let ((key (cons type value)))
      (dolist (waiter (gethash key elpaca--conditions))
        (puthash key (list waiter) elpaca--conditions)
        (condition-case nil
            (funcall fn type value reason)
          (elpaca-build-error nil)))
      (remhash key elpaca--conditions))))

(advice-add #'elpaca-resolve :around #'elpaca-resolve-fail-every-waiter-a)

(provide 'elpaca-failures)
;;; elpaca-failures.el ends here
