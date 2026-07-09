;;; tests/general/evil-tests.el --- general/autoload/evil.el specs -*- lexical-binding: t; -*-

;; evil-ex-visual-star-search-a is evil-bound and stays smoke-covered;
;; the window movers are testable with stubs for the evil edge cases.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/evil.el")

(describe "window-move-right"
  (it "wraps to the far left when already at the right edge"
    (delete-other-windows)
    (let (called)
      (cl-letf (((symbol-function 'evil-window-move-far-left)
                 (lambda () (setq called t))))
        (window-move-right))
      (expect called :to-be-truthy)))
  (it "swaps with the right window otherwise (no self-recursion)"
    (delete-other-windows)
    (unwind-protect
        (let (swapped)
          (split-window nil nil 'right)
          ;; selected window is the left one: not at the right edge
          (cl-letf (((symbol-function 'windmove-swap-states-right)
                     (lambda () (setq swapped t))))
            (window-move-right))
          (expect swapped :to-be-truthy))
      (delete-other-windows))))

(describe "window-move-left"
  (it "wraps to the far right when already at the left edge"
    (delete-other-windows)
    (let (called)
      (cl-letf (((symbol-function 'evil-window-move-far-right)
                 (lambda () (setq called t))))
        (window-move-left))
      (expect called :to-be-truthy))))
