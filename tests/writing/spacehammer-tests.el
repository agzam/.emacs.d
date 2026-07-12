;;; tests/writing/spacehammer-tests.el --- writing/autoload/spacehammer.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/spacehammer.el")

(defmacro with-fake-screen (frame-x frame-width screen-width &rest body)
  "Stub frame/display geometry around BODY."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'frame-position) (lambda () (cons ,frame-x 0)))
             ((symbol-function 'frame-pixel-width) (lambda () ,frame-width))
             ((symbol-function 'display-pixel-width) (lambda () ,screen-width)))
     ,@body))

(describe "frame-facing-direction"
  (it "faces right when the frame sits on the left half"
    (with-fake-screen 0 800 2000
      (expect (frame-facing-direction) :to-be 'right)))

  (it "faces left when the frame sits on the right half"
    (with-fake-screen 1400 800 2000
      (expect (frame-facing-direction) :to-be 'left))))

(describe "spacehammer-display-edit-buffer"
  (it "injects the facing direction ahead of the caller's alist"
    (let (seen)
      (cl-letf (((symbol-function 'display-buffer-in-quadrant)
                 (lambda (_buf alist) (setq seen alist))))
        (with-fake-screen 1400 800 2000
          (spacehammer-display-edit-buffer (current-buffer) '((window . root))))
        (expect (alist-get 'direction seen) :to-be 'left)
        (expect (alist-get 'window seen) :to-be 'root)))))

(describe "hammerspoon-eval-fennel"
  (it "refuses to run off macOS"
    (let ((system-type 'gnu/linux))
      (expect (hammerspoon-eval-fennel "(+ 1 2)") :to-throw 'user-error)))

  (it "requires the hs CLI"
    (let ((system-type 'darwin))
      (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
        (expect (hammerspoon-eval-fennel "(+ 1 2)") :to-throw 'user-error))))

  (it "escapes double quotes before shipping the form to hs"
    (let ((system-type 'darwin) seen)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/local/bin/hs"))
                ((symbol-function 'process-lines)
                 (lambda (&rest args) (setq seen args) '("ok"))))
        (expect (hammerspoon-eval-fennel "(print \"hi\")") :to-equal '("ok"))
        (expect (nth 0 seen) :to-equal "/usr/local/bin/hs")
        (expect (nth 2 seen) :to-match "\\\\\"hi\\\\\"")))))