;;; tests/general/scratch-tests.el --- general/autoload/scratch.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/scratch.el")

;; scratch-dir derives from the sandboxed doom-data-dir (helper.el), so
;; persistence specs never touch real state.

(defun scratch-tests-cleanup ()
  "Kill scratch buffers (silencing the persist hook) and wipe scratch-dir.
Also removes any untracked *scratch* - the persistent scratch shares the
built-in name now, and a leftover would flip create-fresh specs into the
adoption path."
  (dolist (buf scratch-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (remove-hook 'kill-buffer-hook #'persist-scratch-buffer-h t))
      (kill-buffer buf)))
  (setq scratch-buffers nil)
  (when-let* ((stray (get-buffer "*scratch*")))
    (kill-buffer stray))
  (when (file-directory-p scratch-dir)
    (delete-directory scratch-dir t)))

(defun scratch-tests-restore-batch-scratch ()
  "Re-create the pristine *scratch* the batch session started in.
Later suites run with it as the current buffer, like before this file."
  (set-buffer (get-buffer-create "*scratch*")))

(describe "scratch buffers"
  (before-each (scratch-tests-cleanup))
  (after-each (scratch-tests-cleanup))
  (after-all (scratch-tests-restore-batch-scratch))

  (it "applies MODE to a fresh scratch"
    (let ((buf (scratch-buffer-create nil #'text-mode default-directory nil)))
      (expect (buffer-name buf) :to-equal "*scratch*")
      (expect (buffer-local-value 'major-mode buf) :to-be 'text-mode)))

  (it "namespaces project scratches by name"
    (expect (buffer-name (scratch-buffer-create t nil default-directory "proj"))
            :to-equal "*scratch (proj)*"))

  (it "persists and restores content, point and mode"
    (with-current-buffer (scratch-buffer-create nil #'text-mode
                                                default-directory nil)
      (insert "hello world")
      (goto-char 3)
      (persist-scratch-buffer-h))
    ;; plain kill: the persisted file must survive for the restore leg
    (kill-buffer "*scratch*")
    (with-current-buffer (scratch-buffer-create nil nil default-directory nil)
      (expect (buffer-string) :to-equal "hello world")
      (expect (point) :to-be 3)
      (expect major-mode :to-be 'text-mode)))

  (it "switch-to-scratch-buffer takes the current window"
    (let ((before (current-buffer)))
      (unwind-protect
          (progn
            (switch-to-scratch-buffer)
            (expect (buffer-name) :to-equal "*scratch*"))
        (switch-to-buffer before))))

  (it "toggle closes the window when the scratch is visible"
    (let ((before (current-buffer))
          closed)
      (unwind-protect
          (progn
            (switch-to-scratch-buffer)
            (cl-letf (((symbol-function 'delete-window)
                       (lambda (&optional win) (setq closed (or win t)))))
              (toggle-scratch-buffer))
            (expect closed :to-be-truthy))
        (switch-to-buffer before)))))

(describe "startup-scratch-buffer"
  (before-each (scratch-tests-cleanup))
  (after-each (scratch-tests-cleanup))
  (after-all (scratch-tests-restore-batch-scratch))

  (it "buries a pristine *scratch* and takes its name"
    (with-current-buffer (get-buffer-create "*scratch*")
      (erase-buffer)
      (set-buffer-modified-p nil))
    (let ((buf (startup-scratch-buffer)))
      (expect (buffer-name buf) :to-equal "*scratch*")
      ;; the persistent machinery owns it now
      (expect (memq buf scratch-buffers) :to-be-truthy)))

  (it "adopts a modified *scratch* instead of killing it"
    (let ((existing (get-buffer-create "*scratch*")))
      (with-current-buffer existing
        (insert "precious"))
      (let ((buf (startup-scratch-buffer)))
        (expect buf :to-be existing)
        (expect (with-current-buffer buf (buffer-string))
                :to-equal "precious")
        (expect (memq buf scratch-buffers) :to-be-truthy)))))
