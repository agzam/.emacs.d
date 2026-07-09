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
