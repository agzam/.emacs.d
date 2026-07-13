;;; tests/lisp/functions-tests.el --- lisp/functions.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "lisp/functions.el")

(describe "system-dist-name"
  (it "prefixes the macOS product version"
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "15.5\n")))
      (let ((system-type 'darwin))
        (expect (system-dist-name) :to-equal "macOS 15.5"))))
  (it "returns the bare distro name on linux"
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "Arch\n")))
      (let ((system-type 'gnu/linux))
        (expect (system-dist-name) :to-equal "Arch")))))

(defvar active-modules)

(describe "reload-config"
  (it "re-runs the init.el layers in order, then processes elpaca queues"
    ;; let*: the hook lambdas must lexically capture `calls'
    (let* ((calls nil)
           (active-modules '(mod-a mod-b))
           (custom-file "/probe/custom.el")
           (doom-before-reload-hook (list (lambda () (push '(hook before) calls))))
           (doom-after-reload-hook (list (lambda () (push '(hook after) calls)))))
      (cl-letf (((symbol-function 'load)
                 (lambda (file &rest _)
                   (push (list 'load (file-name-nondirectory file)) calls)))
                ((symbol-function 'load-module)
                 (lambda (name) (push (list 'module name) calls)))
                ((symbol-function 'elpaca-process-queues)
                 (lambda () (push '(elpaca) calls)))
                ((symbol-function 'message) #'ignore))
        (reload-config))
      (expect (nreverse calls) :to-equal
              '((hook before)
                (load "doom-defaults") (load "functions")
                (module mod-a) (module mod-b)
                (load "config") (load "custom.el")
                (elpaca)
                (hook after)))))
  (it "skips custom-file when unset"
    (let ((calls nil)
          (active-modules nil)
          (custom-file nil)
          (doom-before-reload-hook nil)
          (doom-after-reload-hook nil))
      (cl-letf (((symbol-function 'load)
                 (lambda (file &rest _)
                   (push (file-name-nondirectory file) calls)))
                ((symbol-function 'load-module) #'ignore)
                ((symbol-function 'elpaca-process-queues) #'ignore)
                ((symbol-function 'message) #'ignore))
        (reload-config))
      (expect (nreverse calls) :to-equal '("doom-defaults" "functions" "config")))))

(describe "display-buffer-in-quadrant"
  (it "creates a window for a new buffer and reuses it on redisplay"
    (let ((buf (generate-new-buffer "quadrant-probe")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let ((w1 (display-buffer-in-quadrant
                       buf '((direction . right)))))
              (expect (window-live-p w1) :to-be-truthy)
              (expect (window-buffer w1) :to-be buf)
              ;; second display finds the existing window, no new split
              (let ((count (length (window-list))))
                (display-buffer-in-quadrant buf '((direction . right)))
                (expect (length (window-list)) :to-equal count))))
        (delete-other-windows)
        (kill-buffer buf)))))
