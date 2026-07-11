;;; tests/modeline/segments-tests.el --- modeline/autoload/segments.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; doom-modeline isn't installed in the batch tier; apply-custom-modeline
;; requires it at call time - stub the feature and capture the def call.
;; Segment existence against the real package is probe territory
;; (doom-modeline--prepare-segments hard-errors on unknown names).
(provide 'doom-modeline)

(load-module-file "modules/modeline/autoload/segments.el")

(describe "apply-custom-modeline"
  (it "redefines doom-modeline's `main' with the segment split"
    (let (got)
      (cl-letf (((symbol-function 'doom-modeline-def-modeline)
                 (lambda (&rest args) (setq got args))))
        (apply-custom-modeline)
        (expect got :to-equal (cons 'main modeline-segments)))))

  (it "keeps dead segments out of the layout"
    ;; persp-name (persp-mode nowhere) and modals (modal-icon dropped with
    ;; it) were removed on port; this pins against silent reintroduction.
    (let ((all (apply #'append modeline-segments)))
      (expect all :not :to-contain 'persp-name)
      (expect all :not :to-contain 'modals)
      (expect (car modeline-segments) :to-contain 'bar))))
