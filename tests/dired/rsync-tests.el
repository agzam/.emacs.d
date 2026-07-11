;;; tests/dired/rsync-tests.el --- dired/autoload/rsync.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/dired/autoload/rsync.el")

(defun rsync-tests--make-file (dir name bytes)
  "Create NAME under DIR holding BYTES bytes; return its path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert (make-string bytes ?x)))
    path))

(describe "dired--calculate-size"
  (it "sums a flat directory recursively"
    (let ((dir (make-temp-file "rsync-size" t)))
      (unwind-protect
          (progn
            (rsync-tests--make-file dir "a" 100)
            (rsync-tests--make-file dir "b" 20)
            (expect (dired--calculate-size dir) :to-equal 120))
        (delete-directory dir t))))

  (it "returns the plain size for a single file"
    (let ((dir (make-temp-file "rsync-size" t)))
      (unwind-protect
          (expect (dired--calculate-size (rsync-tests--make-file dir "a" 42))
                  :to-equal 42)
        (delete-directory dir t)))))

(describe "dired-should-use-rsync-p"
  (it "is nil below the (default 50M) threshold"
    (let ((dir (make-temp-file "rsync-thresh" t)))
      (unwind-protect
          (expect (dired-should-use-rsync-p
                   (list (rsync-tests--make-file dir "a" 10)))
                  :to-be nil)
        (delete-directory dir t))))

  (it "is non-nil once the total crosses the threshold"
    (let ((dir (make-temp-file "rsync-thresh" t)))
      (unwind-protect
          (let ((dired-rsync-size-threshold 15))
            (expect (dired-should-use-rsync-p
                     (list (rsync-tests--make-file dir "a" 10)
                           (rsync-tests--make-file dir "b" 10)))
                    :to-be-truthy))
        (delete-directory dir t)))))

(describe "dired-do-rename-wrapper-a"
  (it "falls through to the original rename below the threshold"
    (let ((dir (make-temp-file "rsync-rename" t))
          orig-arg shelled)
      (unwind-protect
          (let ((file (rsync-tests--make-file dir "small" 10)))
            (cl-letf (((symbol-function 'dired-get-marked-files)
                       (lambda (&rest _) (list file)))
                      ((symbol-function 'async-shell-command)
                       (lambda (cmd) (setq shelled cmd))))
              (dired-do-rename-wrapper-a (lambda (arg) (setq orig-arg (list arg))) 4)
              (expect orig-arg :to-equal '(4))
              (expect shelled :to-be nil)))
        (delete-directory dir t))))

  (it "routes oversized payloads through rsync and skips the original"
    (let ((dir (make-temp-file "rsync-rename" t))
          orig-called shelled)
      (unwind-protect
          (let ((file (rsync-tests--make-file dir "big" 64))
                (dired-rsync-size-threshold 32))
            (cl-letf (((symbol-function 'dired-get-marked-files)
                       (lambda (&rest _) (list file)))
                      ((symbol-function 'dired-dwim-target-directory)
                       (lambda () "/tmp/"))
                      ((symbol-function 'read-directory-name)
                       (lambda (&rest _) "/tmp/dest/"))
                      ((symbol-function 'async-shell-command)
                       (lambda (cmd) (setq shelled cmd)))
                      ((symbol-function 'revert-buffer)
                       (lambda (&rest _))))
              (dired-do-rename-wrapper-a (lambda (_arg) (setq orig-called t)))
              (expect orig-called :to-be nil)
              (expect shelled :to-match "\\`rsync -av --progress --remove-source-files ")
              (expect shelled :to-match (regexp-quote file))
              (expect shelled :to-match (regexp-quote "/tmp/dest/"))))
        (delete-directory dir t)))))
