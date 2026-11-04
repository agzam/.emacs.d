;;; tests/e2e/window-numbers.el --- window numbers on the leader menu -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.  Only a booted
;; config has evil's leader, winum's selectors and the which-key advice wired
;; together; the batch suite stubs all three.  Without child frames (a tty
;; before Emacs 31) posframe cannot draw, and the numbers must stay away.

(defun window-numbers-e2e--case (label got want)
  "A harness result for LABEL comparing GOT with WANT."
  (list :label (format "window numbers: %s" label) :ok (equal got want)
        :got (format "%S" got) :want (format "%S" want)))

(defun window-numbers-e2e--shown ()
  "Window numbers whose labels are on display, ascending."
  (sort (seq-keep (lambda (f)
                    (when-let* ((buf (car (frame-parameter f 'posframe-buffer)))
                                ((frame-visible-p f))
                                ((string-match "\\` \\*window-number-\\([0-9]+\\)\\*\\'" buf)))
                      (string-to-number (match-string 1 buf))))
                  (frame-list))
        #'<))

(defun window-numbers-e2e ()
  "The leader menus number the windows, and the next command hides them."
  (let ((buf (find-file-noselect (expand-file-name "window-numbers.txt" e2e-work-dir)))
        (frame (selected-frame)))
    (unwind-protect
        (progn
          ;; :after-call loads winum on the first window switch, and its
          ;; config installs the advice once which-key is in
          (require 'winum)
          (require 'which-key)
          (select-frame frame)
          (unless (fboundp 'keymap-selects-windows-p)
            (load (expand-file-name "modules/general/autoload/windows.el" e2e-root)
                  nil 'nomessage))
          (switch-to-buffer buf)
          (delete-other-windows)
          (split-window-right)
          (evil-normal-state)
          ;; redisplay is inhibited, so winum would never renumber on its own
          (run-window-configuration-change-hook)
          (let ((want (and (posframe-workable-p) '(1 2))))
            (list
             (window-numbers-e2e--case
              "SPC and SPC w list the selectors, SPC f does not"
              (mapcar (lambda (keys) (keymap-selects-windows-p (key-binding (kbd keys))))
                      '("SPC" "SPC w" "SPC f"))
              '(t t nil))
             (window-numbers-e2e--case
              "which-key's popup carries the advice"
              (and (advice-member-p #'which-key-window-numbers-a
                                    #'which-key--create-buffer-and-show)
                   t)
              t)
             (window-numbers-e2e--case
              "the SPC menu numbers both windows (none without child frames)"
              (progn (which-key--create-buffer-and-show (kbd "SPC"))
                     (window-numbers-e2e--shown))
              want)
             (window-numbers-e2e--case
              "moving on to SPC f takes them down"
              (progn (which-key--create-buffer-and-show (kbd "SPC f"))
                     (window-numbers-e2e--shown))
              nil)
             (window-numbers-e2e--case
              "the SPC w menu numbers them again"
              (progn (which-key--create-buffer-and-show (kbd "SPC w"))
                     (window-numbers-e2e--shown))
              want)
             (window-numbers-e2e--case
              "SPC 2 lands in window 2 and takes them down"
              (progn (execute-kbd-macro (kbd "SPC 2"))
                     (list (winum-get-number) (window-numbers-e2e--shown)))
              '(2 nil)))))
      (when (fboundp 'hide-window-numbers) (hide-window-numbers))
      (when (fboundp 'posframe-delete)
        (dolist (n '(1 2)) (posframe-delete (format " *window-number-%d*" n))))
      (which-key--hide-popup)
      (delete-other-windows)
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(add-to-list 'e2e-scenarios #'window-numbers-e2e)
