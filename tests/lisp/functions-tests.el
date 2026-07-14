;;; tests/lisp/functions-tests.el --- lisp/functions.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'transient)

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

(describe "transient-remap-suffix-key"
  ;; Two throwaway prefixes mirror gptel's two keying shapes: a key defined on
  ;; the suffix itself (like `gptel--suffix-send') and a key defined inline in
  ;; the layout (like gptel-tools' confirm).  transient is built-in, so no
  ;; package is needed here.
  (before-each
    (transient-define-suffix probe-suffix-on-def ()
      :key "RET" :description "send"
      (interactive))
    (transient-define-prefix probe-menu-inherited ()
      "probe" [(probe-suffix-on-def)])
    (transient-define-prefix probe-menu-in-layout ()
      "probe" [("RET" "confirm" ignore)]))
  (after-each
    (dolist (s '(probe-suffix-on-def probe-menu-inherited probe-menu-in-layout
                 probe-menu-no-ret))
      (when (fboundp s) (fmakunbound s))
      (put s 'transient--prefix nil)
      (put s 'transient--layout nil)))

  (it "remaps whether the key lives on the suffix or in the layout"
    (transient-remap-suffix-key 'probe-menu-inherited "RET" "s-<return>")
    (transient-remap-suffix-key 'probe-menu-in-layout "RET" "s-<return>")
    (expect (ignore-errors (transient-get-suffix 'probe-menu-inherited "s-<return>"))
            :to-be-truthy)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-in-layout "s-<return>"))
            :to-be-truthy)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-inherited "RET")) :to-be nil)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-in-layout "RET")) :to-be nil))

  (it "is idempotent - a second run (as reload-config triggers) does not error"
    (transient-remap-suffix-key 'probe-menu-inherited "RET" "s-<return>")
    (expect (transient-remap-suffix-key 'probe-menu-inherited "RET" "s-<return>")
            :not :to-throw)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-inherited "s-<return>"))
            :to-be-truthy))

  (it "no-ops on a prefix without the FROM key"
    (transient-define-prefix probe-menu-no-ret ()
      "probe" [("x" "x" ignore)])
    (expect (transient-remap-suffix-key 'probe-menu-no-ret "RET" "s-<return>")
            :not :to-throw)))
