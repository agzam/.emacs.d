;;; tests/e2e/translate-field.el --- typing into translate-transient -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The field is a real buffer edited in a recursive edit that transient
;; suspends its keys for; evil states, the input method and the RET the
;; suffix queues for the resumed menu only meet in a booted config.

(require 'cl-lib)

;; Bound below; special before the files defining them load (see
;; input-method-switching.el).
(defvar input-method-recent)
(defvar cycle-input-method--walk)
(defvar translate-history)

(defun translate-field-e2e ()
  "Open the menu off the leader, type into the field, translate with RET."
  (require 'google-translate)
  ;; load before stubbing: the autoload firing under a stub would redefine it
  (unless (fboundp 'translate-field-edit)
    (load (expand-file-name "modules/writing/autoload/google-translate.el" e2e-root)
          nil 'nomessage))
  (let* ((file (expand-file-name "translate-field.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (google-translate-default-source-language "auto")
         (google-translate-default-target-language "en")
         (input-method-recent nil)
         (cycle-input-method--walk nil)
         (translate-history nil)
         (results '())
         sent snapshots)
    (cl-labels ((record (label ok got want)
                  (push (list :label (format "translate field: %s" label)
                              :ok ok :got got :want want)
                        results))
                (snapshot ()
                  (when (derived-mode-p 'translate-field-mode)
                    (push (list :state (bound-and-true-p evil-state)
                                :source google-translate-default-source-language
                                :method (input-method-in-effect)
                                :text (buffer-string)
                                :prefix (substring-no-properties line-prefix)
                                :preview (and completion-preview-mode t)
                                :mode-line mode-line-format
                                :height (window-body-height)
                                ;; the menu window sets `no-other-window'
                                :menu-below (eq (window-in-direction 'below nil t)
                                                transient--window)
                                :menu (when-let* ((b (get-buffer transient--buffer-name)))
                                        (with-current-buffer b
                                          (buffer-substring-no-properties
                                           (point-min) (point-max)))))
                          snapshots)))
                (run (keys)
                  (setq sent nil snapshots nil)
                  (switch-to-buffer buf)
                  (delete-other-windows)
                  (with-current-buffer buf (evil-force-normal-state))
                  (discard-input)
                  ;; a key that errors ends the macro with the field still
                  ;; open; leave it instead of idling into the watchdog
                  (let ((guard (run-with-timer
                                10 nil
                                (lambda ()
                                  (when (< 0 (recursion-depth))
                                    (if (active-minibuffer-window)
                                        (abort-recursive-edit)
                                      (exit-recursive-edit)))))))
                    (unwind-protect
                        (condition-case e
                            (execute-kbd-macro keys)
                          ((error quit) (record "keys ran" nil e nil)))
                      (cancel-timer guard)))
                  (setq snapshots (nreverse snapshots))))
      (unwind-protect
          (cl-letf (((symbol-function 'google-translate-translate)
                     (lambda (src tgt text &rest _) (setq sent (list src tgt text)))))
            (with-current-buffer buf (erase-buffer) (insert "word at point"))
            (add-hook 'post-command-hook #'snapshot)

            (run (vconcat (kbd "SPC x t t") "icat" (kbd "C-<f12>") "rjn" (kbd "RET")))
            (let ((first (car snapshots))
                  (switched (cl-find "ru" snapshots
                                     :key (lambda (s) (plist-get s :source))
                                     :test #'equal)))
              (record "i opens a one-line field in insert state between the buffer and the menu"
                      (and first
                           (eq (plist-get first :state) 'insert)
                           (plist-get first :preview)
                           (null (plist-get first :mode-line))
                           (eql (plist-get first :height) 1)
                           (plist-get first :menu-below))
                      first "insert, completion-preview, no mode line, 1 line, menu below")
              (record "C-<f12> in the field makes Russian the source, prefix and menu show it"
                      (and switched
                           (equal (plist-get switched :method) "russian-computer")
                           (equal (plist-get switched :prefix) "ru → en: ")
                           (string-search "\"ru\"" (or (plist-get switched :menu) ""))
                           t)
                      switched "source ru, russian-computer, prefix ru → en, menu \"ru\"")
              (record "one RET translates what the field holds"
                      (equal sent '("ru" "en" "catкот"))
                      sent '("ru" "en" "catкот"))
              (record "the menu and the field are gone, the text joined the history"
                      (and (null transient--prefix)
                           (null (get-buffer "*translate*"))
                           (equal (car translate-history) "catкот"))
                      (list transient--prefix (get-buffer "*translate*")
                            (car translate-history))
                      (list nil nil "catкот")))

            (setq google-translate-default-source-language "auto")
            (run (vconcat (kbd "SPC x t t") "ihol" [escape] "Aa" (kbd "RET")))
            (record "escape gives normal state and A appends, as in any buffer"
                    (and (cl-find 'normal snapshots
                                  :key (lambda (s) (plist-get s :state)))
                         (equal sent '("auto" "en" "hola")))
                    (list (mapcar (lambda (s) (plist-get s :state)) snapshots) sent)
                    "a normal-state snapshot, then (\"auto\" \"en\" \"hola\")")

            (run (vconcat (kbd "SPC x t t") "ix" [escape] "q"))
            (let ((menu transient--prefix))
              (when menu (execute-kbd-macro (kbd "C-g")))
              (record "q in normal state closes the field back to the menu"
                      (and menu (null sent) (null (get-buffer "*translate*")))
                      (list (and menu t) sent (get-buffer "*translate*"))
                      (list t nil nil))))
        (remove-hook 'post-command-hook #'snapshot)
        (discard-input)
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-other-windows)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'translate-field-e2e)
