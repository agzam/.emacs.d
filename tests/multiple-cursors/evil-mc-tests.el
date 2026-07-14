;;; tests/multiple-cursors/evil-mc-tests.el --- multiple-cursors specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/multiple-cursors/autoload/evil-mc.el")

(defvar evil-mc-frozen)

(describe "mc-toggle-cursors"
  (it "errors when no cursors exist"
    (cl-letf (((symbol-function 'evil-mc-has-cursors-p) (lambda () nil)))
      (expect (mc-toggle-cursors) :to-throw 'user-error)))

  (it "freezes cursors when they exist and aren't frozen"
    (cl-letf (((symbol-function 'evil-mc-has-cursors-p) (lambda () t)))
      (let ((evil-mc-frozen nil))
        (mc-toggle-cursors)
        (expect evil-mc-frozen :to-be t))))

  (it "resumes cursors when they are frozen"
    (cl-letf (((symbol-function 'evil-mc-has-cursors-p) (lambda () t)))
      (let ((evil-mc-frozen t))
        (mc-toggle-cursors)
        (expect evil-mc-frozen :to-be nil)))))
