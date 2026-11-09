;;; tests/e2e/input-method-switching.el --- C-<f12> walks the input methods -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The remap of `toggle-input-method' only exists once general has bound
;; it, evil decides which state holds the method back, and quail only
;; translates keys that come through the command loop.

(require 'cl-lib)

;; Bound below before the autoload file loads: a lexical binding there
;; makes its `defvar' fail and leaves the command undefined.
(defvar input-method-recent)
(defvar cycle-input-method--walk)

(defun input-method-switching-e2e ()
  "Press C-<f12> between typed words in a text buffer and read what landed."
  (let* ((file (expand-file-name "input-methods.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (input-method-recent nil)
         (cycle-input-method--walk nil)
         (results '()))
    (cl-flet ((press (keys label want)
                (condition-case e
                    (execute-kbd-macro keys)
                  ((error quit)
                   (push (list :label (format "input methods: %s (keys)" label)
                               :ok nil :err e)
                         results)))
                (let ((got (with-current-buffer buf
                             (buffer-substring-no-properties (point-min) (point-max)))))
                  (push (list :label (format "input methods: %s" label)
                              :ok (equal got want) :got got :want want)
                        results))))
      (unwind-protect
          (progn
            (switch-to-buffer buf)
            (delete-other-windows)
            (with-current-buffer buf
              (erase-buffer)
              (deactivate-input-method)
              (setq evil-input-method nil)
              (evil-force-normal-state))
            (discard-input)
            (press (vconcat "i" (kbd "C-<f12>") "ghbdtn" (kbd "C-<f12>") " ok" [escape])
                   "one press types Russian, the next one English"
                   "привет ok")
            (press (vconcat "A" (kbd "C-<f12> C-<f12>") " an~o" [escape])
                   "a second press right away reaches Spanish"
                   "привет ok año")
            (press (vconcat (kbd "C-<f12>") "A" " x" [escape])
                   "a press in normal state turns the held method off"
                   "привет ok año x"))
        (with-current-buffer buf
          (evil-force-normal-state)
          (set-buffer-modified-p nil))
        (kill-buffer buf)
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'input-method-switching-e2e)
