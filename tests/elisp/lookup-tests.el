;;; tests/elisp/lookup-tests.el --- elisp/autoload/lookup.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/elisp/autoload/lookup.el")

(describe "elisp-lookup-documentation"
  (it "opens a named symbol in helpful"
    (let ((looked nil))
      (cl-letf (((symbol-function 'helpful-symbol) (lambda (s) (setq looked s)))
                ((symbol-function 'cl-find-class) (lambda (_) nil)))
        (elisp-lookup-documentation "car")
        (expect looked :to-be 'car))))

  (it "falls back to helpful-at-point when nothing is named"
    (let ((called nil))
      (cl-letf (((symbol-function 'helpful-at-point)
                 (lambda () (interactive) (setq called t) t)))
        (elisp-lookup-documentation nil)
        (expect called :to-be-truthy)))))
