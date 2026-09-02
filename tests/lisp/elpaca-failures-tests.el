;;; tests/lisp/elpaca-failures-tests.el --- lisp/elpaca-failures.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; elpaca is absent here.  The advice is exercised against a stand-in for
;; `elpaca-resolve' that behaves as the real one does on failure: it drops
;; the key from the table, then fails the waiters in order, and the first
;; failure's signal escapes.
(define-error 'elpaca-build-error "Elpaca build error")
(defvar elpaca--conditions (make-hash-table :test #'equal))

(load-module-file "lisp/elpaca-failures.el")

(defvar elpaca-failures-tests--handled nil
  "Waiters the stand-in handled, newest first.")

(defun elpaca-failures-tests--resolve (type value &optional reason)
  "A stand-in `elpaca-resolve': handles waiters, escapes on the first failure."
  (let ((key (cons type value)))
    (when-let* ((waiting (gethash key elpaca--conditions)))
      (remhash key elpaca--conditions)
      (dolist (waiter waiting)
        (push waiter elpaca-failures-tests--handled)
        (when reason
          (signal 'elpaca-build-error (list waiter reason)))))))

(describe "elpaca-resolve-fail-every-waiter-a"
  (before-each
    (setq elpaca-failures-tests--handled nil)
    (clrhash elpaca--conditions)
    (puthash '(finished . ghub) '(forge magit-gha-badge gh-notify) elpaca--conditions))

  (it "fails every waiter, not only the first"
    (elpaca-resolve-fail-every-waiter-a #'elpaca-failures-tests--resolve
                                        'finished 'ghub "ghub failed")
    (expect (reverse elpaca-failures-tests--handled)
            :to-equal '(forge magit-gha-badge gh-notify)))

  (it "contains each waiter's signal, so the caller's own failure goes on to be noted"
    (expect (elpaca-resolve-fail-every-waiter-a #'elpaca-failures-tests--resolve
                                                'finished 'ghub "ghub failed")
            :not :to-throw))

  (it "leaves no waiter registered on the failed condition"
    (elpaca-resolve-fail-every-waiter-a #'elpaca-failures-tests--resolve
                                        'finished 'ghub "ghub failed")
    (expect (gethash '(finished . ghub) elpaca--conditions) :to-be nil))

  (it "hands a satisfied condition through untouched"
    (elpaca-resolve-fail-every-waiter-a #'elpaca-failures-tests--resolve 'finished 'ghub)
    (expect (reverse elpaca-failures-tests--handled)
            :to-equal '(forge magit-gha-badge gh-notify)))

  (it "is installed around elpaca-resolve"
    (expect (advice-member-p 'elpaca-resolve-fail-every-waiter-a 'elpaca-resolve)
            :to-be-truthy)))

;;; elpaca-failures-tests.el ends here
