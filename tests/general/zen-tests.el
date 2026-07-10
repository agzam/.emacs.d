;;; tests/general/zen-tests.el --- zen (writeroom) helpers specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/zen.el")

;; special declaration: writeroom isn't installed in the batch tier
(defvar writeroom-mode)

(describe "zen-text-scale-h"
  (it "scales up on enable and back to zero on disable"
    (let ((sets nil))
      (cl-letf (((symbol-function 'text-scale-set)
                 (lambda (n) (push n sets)))
                ((symbol-function 'visual-fill-column-adjust) #'ignore))
        (zen-text-scale-h 1)
        (zen-text-scale-h -1)
        (expect (nreverse sets) :to-equal (list zen-text-scale 0)))))

  (it "refits the centered column after scaling"
    (let ((adjusts 0))
      (cl-letf (((symbol-function 'text-scale-set) #'ignore)
                ((symbol-function 'visual-fill-column-adjust)
                 (lambda () (cl-incf adjusts))))
        (zen-text-scale-h 1)
        (expect adjusts :to-equal 1))))

  (it "does nothing when zen-text-scale is 0"
    (let ((zen-text-scale 0)
          (sets nil))
      (cl-letf (((symbol-function 'text-scale-set)
                 (lambda (n) (push n sets)))
                ((symbol-function 'visual-fill-column-adjust) #'ignore))
        (zen-text-scale-h 1)
        (expect sets :to-be nil)))))

(describe "zen-mixed-pitch-h"
  (it "passes the toggle arg through in prose modes"
    (let ((calls nil))
      (cl-letf (((symbol-function 'mixed-pitch-mode)
                 (lambda (arg) (push arg calls))))
        (with-temp-buffer
          (org-mode)
          (zen-mixed-pitch-h 1)
          (zen-mixed-pitch-h -1))
        (expect (nreverse calls) :to-equal '(1 -1)))))

  (it "stays out of non-prose buffers"
    (let ((calls nil))
      (cl-letf (((symbol-function 'mixed-pitch-mode)
                 (lambda (arg) (push arg calls))))
        (with-temp-buffer
          (zen-mixed-pitch-h 1))
        (expect calls :to-be nil)))))

(describe "turn-off-writeroom-before-split-a"
  (it "kills writeroom before the split"
    (let ((calls nil))
      (cl-letf (((symbol-function 'writeroom-mode)
                 (lambda (arg) (push arg calls))))
        (let ((writeroom-mode t))
          (turn-off-writeroom-before-split-a))
        (expect calls :to-equal '(-1)))))

  (it "no-ops when writeroom is off"
    (let ((calls nil))
      (cl-letf (((symbol-function 'writeroom-mode)
                 (lambda (arg) (push arg calls))))
        (let ((writeroom-mode nil))
          (turn-off-writeroom-before-split-a))
        (expect calls :to-be nil)))))
