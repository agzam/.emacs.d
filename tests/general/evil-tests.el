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

(describe "window-move-down"
  (it "wraps to the very top when already at the bottom edge"
    (delete-other-windows)
    (let (called)
      (cl-letf (((symbol-function 'evil-window-move-very-top)
                 (lambda () (setq called t))))
        (window-move-down))
      (expect called :to-be-truthy)))
  (it "swaps with the window below otherwise"
    (delete-other-windows)
    (unwind-protect
        (let (swapped)
          (split-window nil nil 'below)
          ;; selected window is the top one: not at the bottom edge
          (cl-letf (((symbol-function 'windmove-swap-states-down)
                     (lambda () (setq swapped t))))
            (window-move-down))
          (expect swapped :to-be-truthy))
      (delete-other-windows))))

(describe "window-move-up"
  (it "wraps to the very bottom when already at the top edge"
    (delete-other-windows)
    (let (called)
      (cl-letf (((symbol-function 'evil-window-move-very-bottom)
                 (lambda () (setq called t))))
        (window-move-up))
      (expect called :to-be-truthy))))

;; dynamic declarations: evil isn't loaded in the batch tier (see the
;; defcustom-let gotcha in MIGRATION.org)
(defvar evil-split-window-below)
(defvar evil-vsplit-window-right)

(describe "window-split-and-follow"
  (it "inverts evil-split-window-below for the call"
    (let ((evil-split-window-below nil)
          seen)
      (cl-letf (((symbol-function 'evil-window-split)
                 (lambda () (interactive) (setq seen evil-split-window-below))))
        (window-split-and-follow))
      (expect seen :to-be t))))

(describe "window-vsplit-and-follow"
  (it "inverts evil-vsplit-window-right for the call"
    (let ((evil-vsplit-window-right t)
          seen)
      (cl-letf (((symbol-function 'evil-window-vsplit)
                 (lambda () (interactive) (setq seen evil-vsplit-window-right))))
        (window-vsplit-and-follow))
      (expect seen :to-be nil))))
