;;; tests/tree-sitter/fold-tests.el --- tree-sitter/autoload/fold.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/tree-sitter/autoload/fold.el")

;; treesitter-context isn't installed in the batch tier; the advice only
;; ever runs with the package loaded, so define its vars and stub its fns.
(defvar treesitter-context--fold-supported-mode nil)
(defvar treesitter-context-fold-mode nil)

(describe "fold-all-with-treesitter-context-a"
  (before-each
    (setq treesitter-context--fold-supported-mode '(fundamental-mode)
          treesitter-context-fold-mode t))

  ;; stub walks defuns as lines: one step back per call, nil at bob
  (defun fold-tests--beginning-of-defun (_n)
    (when (> (point) (point-min))
      (forward-line -1)
      t))

  (it "folds every defun on :close-all without calling through"
    (let (folds orig-called)
      (cl-letf (((symbol-function 'treesit-beginning-of-defun)
                 #'fold-tests--beginning-of-defun)
                ((symbol-function 'treesitter-context-fold-hide)
                 (lambda () (push (line-number-at-pos) folds)))
                ((symbol-function 'treesitter-context-fold-show)
                 (lambda () (push 'shown folds))))
        (with-temp-buffer
          (insert "one\ntwo\nthree\n")
          (fold-all-with-treesitter-context-a
           (lambda (&rest _) (setq orig-called t))
           nil :close-all))
        (expect orig-called :to-be nil)
        (expect folds :to-equal '(1 2 3)))))

  (it "shows every defun on :open-all"
    (let (folds)
      (cl-letf (((symbol-function 'treesit-beginning-of-defun)
                 #'fold-tests--beginning-of-defun)
                ((symbol-function 'treesitter-context-fold-hide)
                 (lambda () (push 'hidden folds)))
                ((symbol-function 'treesitter-context-fold-show)
                 (lambda () (push 'shown folds))))
        (with-temp-buffer
          (insert "one\ntwo\n")
          (fold-all-with-treesitter-context-a #'ignore nil :open-all))
        (expect folds :to-equal '(shown shown)))))

  (it "calls through for other fold actions"
    (let (passed)
      (with-temp-buffer
        (fold-all-with-treesitter-context-a
         (lambda (lst action) (setq passed (list lst action)))
         'the-list :close))
      (expect passed :to-equal '(the-list :close))))

  (it "calls through when the mode is unsupported"
    (setq treesitter-context--fold-supported-mode '(prog-mode))
    (let (passed)
      (with-temp-buffer
        (fold-all-with-treesitter-context-a
         (lambda (&rest _) (setq passed t))
         nil :close-all))
      (expect passed :to-be t)))

  (it "calls through when fold-mode is off"
    (setq treesitter-context-fold-mode nil)
    (let (passed)
      (with-temp-buffer
        (fold-all-with-treesitter-context-a
         (lambda (&rest _) (setq passed t))
         nil :close-all))
      (expect passed :to-be t))))
