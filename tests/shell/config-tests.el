;;; tests/shell/config-tests.el --- modules/shell/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar shell-pop-shell-type nil)
(defvar ghostel-module-auto-install nil)

(defun shell-config-tests--body (package keyword)
  "Forms under KEYWORD in PACKAGE's `use-package' block of the shell config."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/shell/config.el" test-config-root))
    (let (block)
      (condition-case nil
          (while (not block)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) package))
                (setq block form))))
        (end-of-file nil))
      (use-package-body-forms (cddr block) keyword))))

(defun shell-config-tests--run (package keyword)
  "Evaluate PACKAGE's KEYWORD forms, leaving out its keymaps and hooks."
  (eval (macroexpand-all `(progn ,@(shell-config-tests--body package keyword))
                         '((map! . ignore) (add-hook! . ignore)))
        t))

(describe "shell-pop default"
  (it "pops a ghostel terminal"
    (let ((shell-pop-shell-type nil))
      (shell-config-tests--run 'shell-pop :init)
      (expect (car shell-pop-shell-type) :to-equal "ghostel")
      (expect (cadr shell-pop-shell-type) :to-equal "*ghostel*")
      (spy-on 'ghostel)
      (funcall (eval (nth 2 shell-pop-shell-type) t))
      (expect 'ghostel :to-have-been-called))))

(describe "ghostel native module"
  (before-each
    (spy-on 'run-with-idle-timer))

  (it "downloads a missing module instead of prompting"
    (let ((ghostel-module-auto-install 'ask))
      (shell-config-tests--run 'ghostel :config)
      (expect ghostel-module-auto-install :to-be 'download)))

  (it "is fetched once Emacs idles"
    (let ((process-environment (cons "CI" process-environment)))
      (shell-config-tests--run 'ghostel :config)
      (expect 'run-with-idle-timer
              :to-have-been-called-with 2 nil #'fetch-ghostel-module)))

  (it "is never fetched in CI"
    (let ((process-environment (cons "CI=true" process-environment)))
      (shell-config-tests--run 'ghostel :config)
      (expect 'run-with-idle-timer :not :to-have-been-called))))

;;; config-tests.el ends here
