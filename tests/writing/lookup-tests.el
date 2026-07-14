;;; tests/writing/lookup-tests.el --- writing/autoload/lookup.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/lookup.el")

(describe "prose-lookup-documentation"
  (it "uses offline sdcv when its binary is present"
    (let (sdcv wik)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (b) (when (equal b "sdcv") "/bin/sdcv")))
                ((symbol-function 'sdcv-search-at-point) (lambda () (setq sdcv t)))
                ((symbol-function 'wiktionary-bro-dwim) (lambda () (setq wik t))))
        (expect (prose-lookup-documentation) :to-be 'deferred)
        (expect sdcv :to-be t)
        (expect wik :to-be nil))))

  (it "falls back to the in-Emacs Wiktionary reader without sdcv"
    (let (wik)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
                ((symbol-function 'wiktionary-bro-dwim) (lambda () (setq wik t))))
        (expect (prose-lookup-documentation) :to-be 'deferred)
        (expect wik :to-be t)))))
