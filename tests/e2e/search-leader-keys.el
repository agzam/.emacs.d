;;; tests/e2e/search-leader-keys.el --- buffer search under SPC s -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; A leader key bound to a nil command keeps its label in the keymap: which-key
;; lists it, and the key does nothing.  Only a real keypress tells them apart.

(require 'cl-lib)

(defun search-leader-keys-e2e ()
  "Press each buffer search leader key and record the consult call it makes."
  ;; Loaded up front: a first `require' under the stubs would redefine them.
  (require 'consult)
  (let* ((file (expand-file-name "search-leader-case.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         call)
    (unwind-protect
        (cl-letf (((symbol-function 'consult-line)
                   (lambda (&rest args) (setq call (cons 'consult-line args))))
                  ((symbol-function 'consult-line-multi)
                   (lambda (query &optional _initial)
                     (interactive "P")
                     (setq call (list 'consult-line-multi query)))))
          (with-current-buffer buf
            (switch-to-buffer buf)
            (delete-other-windows)
            (erase-buffer)
            (insert "needle\n")
            (goto-char (point-min)))
          (mapcar (pcase-lambda (`(,keys ,label ,want))
                    (let (err)
                      (setq call nil)
                      (with-current-buffer buf (evil-force-normal-state))
                      (discard-input)
                      (condition-case e
                          (execute-kbd-macro (kbd keys))
                        (error (setq err e)))
                      (list :label (format "search leader: %s %s" keys label)
                            :ok (and (null err) (equal call want))
                            :got (format "%S" call) :want (format "%S" want)
                            :err err)))
                  '(("SPC s b" "searches the buffer" (consult-line))
                    ("SPC s B" "searches all open buffers"
                     (consult-line-multi all-buffers)))))
      (discard-input)
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-other-windows))))

(add-to-list 'e2e-scenarios #'search-leader-keys-e2e)
