;;; tests/bindings/autoload-tests.el --- bindings/autoload.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/bindings/autoload.el")

;; The wrappers (require 'consult) before calling consult commands; consult
;; is not installed in the batch tier. Satisfy the require and stub the
;; commands per spec.
(provide 'consult)

(describe "delete-backward-word"
  (it "deletes a word without touching the kill-ring"
    (let ((kill-ring '("preexisting"))
          (kill-ring-yank-pointer nil))
      (with-temp-buffer
        (insert "foo bar")
        (delete-backward-word 1)
        (expect (buffer-string) :to-equal "foo "))
      (expect kill-ring :to-equal '("preexisting")))))

(describe "search wrappers"
  (it "search-cwd searches default-directory"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (let ((default-directory test-sandbox-dir))
          (search-cwd))
        (expect got :to-equal (list test-sandbox-dir nil)))))
  (it "search-emacsd searches user-emacs-directory"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (search-emacsd)
        (expect got :to-equal (list user-emacs-directory nil)))))
  (it "search-project falls back to default-directory outside projects"
    (let (got)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (dir &optional initial) (setq got (list dir initial)))))
        (let ((default-directory test-sandbox-dir))
          (search-project))
        (expect got :to-equal (list test-sandbox-dir nil)))))
  (it "search-buffer prefills the active region"
    (let (got)
      (cl-letf (((symbol-function 'consult-line)
                 (lambda (&rest args) (setq got args))))
        (with-temp-buffer
          (transient-mark-mode 1)
          (insert "needle in a haystack")
          (goto-char (point-min))
          (push-mark (point) t t)
          (goto-char (+ (point-min) 6))
          (search-buffer))
        (expect got :to-equal '("needle")))))
  (it "search-buffer passes nothing without a region"
    (let ((got 'untouched))
      (cl-letf (((symbol-function 'consult-line)
                 (lambda (&rest args) (setq got args))))
        (with-temp-buffer
          (insert "nothing marked")
          (search-buffer))
        (expect got :to-equal nil)))))

(describe "yank-buffer-path"
  ;; general/yank-path-tests.el loads yank-path.el in this same process,
  ;; installing its :override advice; lift it to test the underlying command.
  (before-each
    (advice-remove 'yank-buffer-path 'yank-path--buffer-path-a))
  (after-each
    (when (fboundp 'yank-path--buffer-path-a)
      (advice-add 'yank-buffer-path :override #'yank-path--buffer-path-a)))
  ;; file-truename on the fixtures: doom-defaults sets
  ;; find-file-visit-truename, and macOS temp dirs are symlinks
  ;; (/var -> /private/var).
  (it "kills the abbreviated file path"
    (let ((f (file-truename (make-temp-file "yank-probe")))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (unwind-protect
          (with-current-buffer (find-file-noselect f)
            (yank-buffer-path)
            (expect (car kill-ring) :to-equal (abbreviate-file-name f)))
        (when-let* ((buf (get-file-buffer f))) (kill-buffer buf))
        (delete-file f))))
  (it "resolves relative to ROOT when given"
    (let ((f (file-truename (make-temp-file "yank-probe")))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (unwind-protect
          (with-current-buffer (find-file-noselect f)
            (yank-buffer-path (file-name-directory f))
            (expect (car kill-ring)
                    :to-equal (file-name-nondirectory f)))
        (when-let* ((buf (get-file-buffer f))) (kill-buffer buf))
        (delete-file f))))
  (it "signals a user-error in non-file buffers"
    (with-temp-buffer
      (expect (yank-buffer-path) :to-throw 'user-error))))
