;;; tests/e2e/consult-zoxide-actions.el --- Embark actions on zoxide rows -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;; consult-zoxide joins Embark and consult-dir only when the config asks,
;; and only a booted Emacs shows the request arriving through elpaca.

(require 'cl-lib)

(defun consult-zoxide-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "consult-zoxide: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun consult-zoxide-e2e--press (keys entries)
  "Press KEYS in a zoxide prompt listing ENTRIES, an alist of (PATH . SCORE).
Return the Embark actions run, as (ACTION TYPE), and the paths zoxide was
told to remove.  The zoxide binary is stubbed, so no database is touched."
  (let* ((acted nil)
         (removed nil)
         (record (lambda (action target &rest _)
                   (push (list action (plist-get target :type)) acted)))
         ;; keys that never leave the prompt would stall the run to the watchdog
         (guard (run-with-timer 10 nil (lambda ()
                                         (when (active-minibuffer-window)
                                           (abort-recursive-edit))))))
    (advice-add 'embark--act :before record)
    (unwind-protect
        (cl-letf (((symbol-function 'consult-zoxide--call)
                   (lambda (_destination command &rest args)
                     (pcase command
                       ("query" (pcase-dolist (`(,path . ,score) entries)
                                  (insert (format "%5.1f %s\n" score path))))
                       ("remove" (setq removed args)))
                     0)))
          (condition-case err
              (let ((unread-command-events (listify-key-sequence (kbd keys))))
                (consult-zoxide-read))
            (quit nil)
            (error (push (list 'signalled err) acted)))
          ;; `embark-quit-after-action' runs the action once the prompt is gone
          (let ((deadline (+ (float-time) 5)))
            (while (and (null removed) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (list (nreverse acted) removed))
      (cancel-timer guard)
      (advice-remove 'embark--act record))))

(defun consult-zoxide-e2e ()
  "Check the Embark actions and the consult-dir source of zoxide rows."
  (require 'embark)
  (require 'consult-zoxide)
  (let* ((root (expand-file-name "zoxide/" e2e-work-dir))
         (alpha (expand-file-name "alpha" root))
         (beta (expand-file-name "beta" root)))
    (make-directory alpha t)
    (make-directory beta t)
    (unwind-protect
        (let ((pressed (consult-zoxide-e2e--press
                        "C-; \\" `((,alpha . 12.0) (,beta . 4.0)))))
          (require 'consult-dir)
          (list
           (consult-zoxide-e2e--result
            "C-; \\ on the selected row removes it from zoxide"
            pressed `(((consult-zoxide-remove consult-zoxide-dir)) (,alpha)))
           (consult-zoxide-e2e--result
            "consult-dir lists the zoxide source"
            (and (memq 'consult-zoxide-directory-source consult-dir-sources) t)
            t)))
      (delete-directory root t))))

(add-to-list 'e2e-scenarios #'consult-zoxide-e2e)
