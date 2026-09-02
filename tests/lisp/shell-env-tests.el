;;; tests/lisp/shell-env-tests.el --- shell environment import specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "lisp/shell-env.el")

(defmacro shell-env-tests--sandboxed (&rest body)
  "Run BODY against /bin/sh and a temp cache file, on a graphical frame.
The test Emacs's own environment and `exec-path' survive untouched."
  (declare (indent 0))
  `(let* ((shell-env-tests--dir (make-temp-file "shell-env-tests" t))
          (shell-environment-cache-file
           (expand-file-name "state/shell-env" shell-env-tests--dir))
          (shell-file-name "/bin/sh")
          (shell-environment-arguments nil)
          (process-environment (copy-sequence process-environment))
          (exec-path exec-path))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
       (unwind-protect (progn ,@body)
         (when-let* ((process (get-process "shell-env")))
           (delete-process process))
         (delete-directory shell-env-tests--dir t)))))

(defun shell-env-tests--seed-cache (&rest values)
  "Write VALUES into the cache file the way the shell does."
  (make-directory (file-name-directory shell-environment-cache-file) t)
  (with-temp-file shell-environment-cache-file
    (insert (mapconcat (lambda (v) (concat v "\0")) values ""))))

(defun shell-env-tests--wait ()
  "Block until the background shell has exited and its sentinel has run."
  (when-let* ((process (get-process "shell-env")))
    (while (process-live-p process)
      (accept-process-output process 1))))

(describe "shell-environment-incomplete-p"
  (it "is non-nil on a graphical frame, which launchd started"
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (expect (shell-environment-incomplete-p) :to-be-truthy)))
  (it "is nil on a terminal frame, which a shell started"
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (expect (shell-environment-incomplete-p) :to-be nil)))
  (it "ignores executables that the launchd PATH also provides"
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'executable-find)
               (lambda (&rest _) "/opt/homebrew/bin/rg")))
      (expect (shell-environment-incomplete-p) :to-be-truthy))))

(describe "shell-environment-command"
  (it "runs the login shell interactively"
    (let ((shell-file-name "/bin/zsh")
          (shell-environment-arguments '("-l" "-i")))
      (expect (seq-take (shell-environment-command) 4)
              :to-equal '("/bin/zsh" "-l" "-i" "-c"))))
  (it "has printf write the variables NUL-separated into the cache file"
    (let ((shell-environment-arguments nil)
          (shell-environment-cache-file "/tmp/state dir/shell-env"))
      (expect (car (last (shell-environment-command)))
              :to-equal
              "printf '%s\\000%s\\000' \"$PATH\" \"$MANPATH\" > /tmp/state\\ dir/shell-env"))))

(describe "import-shell-environment"
  (it "waits for the shell and applies what it wrote when no session cached anything"
    (shell-env-tests--sandboxed
      (import-shell-environment)
      ;; the launchd PATH, not the test Emacs's own: the shell started clean
      (expect (getenv "PATH") :to-equal "/usr/bin:/bin:/usr/sbin:/sbin")
      (expect exec-path
              :to-equal (list "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" exec-directory))
      (expect (getenv "MANPATH") :to-equal "")
      (expect (get-process "shell-env") :to-be nil)))

  (it "applies the cache at once, then whatever the shell rewrites in the background"
    (shell-env-tests--sandboxed
      (shell-env-tests--seed-cache "/cached/bin" "/cached/man")
      (import-shell-environment)
      (expect (getenv "PATH") :to-equal "/cached/bin")
      (expect (getenv "MANPATH") :to-equal "/cached/man")
      (expect (process-live-p (get-process "shell-env")) :to-be-truthy)
      (shell-env-tests--wait)
      (expect (getenv "PATH") :to-equal "/usr/bin:/bin:/usr/sbin:/sbin")
      (expect (getenv "MANPATH") :to-equal "")))

  (it "keeps the cached values when the background shell fails"
    (shell-env-tests--sandboxed
      (shell-env-tests--seed-cache "/cached/bin" "/cached/man")
      (let ((shell-environment-arguments '("-c" "exit 3" "--")))
        (import-shell-environment)
        (shell-env-tests--wait))
      (expect (getenv "PATH") :to-equal "/cached/bin")
      (expect (getenv "MANPATH") :to-equal "/cached/man")))

  (it "is a no-op on a terminal frame"
    (shell-env-tests--sandboxed
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
        (let ((path (getenv "PATH")))
          (import-shell-environment)
          (expect (getenv "PATH") :to-equal path)
          (expect (get-process "shell-env") :to-be nil)
          (expect (file-exists-p shell-environment-cache-file) :to-be nil))))))

;;; tests/lisp/shell-env-tests.el ends here
