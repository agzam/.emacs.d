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

;; window-move-* is the Doom +evil--window-swap port: swap when a window
;; exists in the direction, throw far when only perpendicular neighbors
;; exist, wrap from a full-span edge window, split-and-follow when sole.
(describe "window-move-right"
  (before-each (delete-other-windows))
  (after-each (delete-other-windows))

  (it "swaps with the right window and follows"
    (let ((buf-a (get-buffer-create "*wm-a*"))
          (buf-b (get-buffer-create "*wm-b*"))
          (left (selected-window))
          (right (split-window nil nil 'right)))
      (set-window-buffer left buf-a)
      (set-window-buffer right buf-b)
      (window-move-right)
      (expect (selected-window) :to-be right)
      (expect (window-buffer right) :to-be buf-a)
      (expect (window-buffer left) :to-be buf-b)))

  (it "throws far right when the only neighbors are above/below"
    ;; the reported SPC w L bug: a top/bottom stack errored
    ;; "No window right from selected window" under the windmove version
    (let (called)
      (split-window nil nil 'below)
      (cl-letf (((symbol-function 'evil-window-move-far-right)
                 (lambda () (setq called t))))
        (window-move-right))
      (expect called :to-be-truthy)))

  (it "wraps a full-span right-edge window to the far left"
    (let (called)
      (select-window (split-window nil nil 'right))
      (cl-letf (((symbol-function 'evil-window-move-far-left)
                 (lambda () (setq called t))))
        (window-move-right))
      (expect called :to-be-truthy)))

  (it "splits and follows when the window is the only one"
    (let ((buf-a (get-buffer-create "*wm-a*")))
      (set-window-buffer (selected-window) buf-a)
      (window-move-right)
      (expect (length (window-list)) :to-equal 2)
      (expect (current-buffer) :to-be buf-a)
      ;; the followed window sits on the right of the vacated one
      (expect (window-in-direction 'left) :not :to-be nil)))

  (it "refuses to move a dedicated window"
    (set-window-dedicated-p (selected-window) t)
    (unwind-protect
        (expect (window-move-right) :to-throw 'user-error)
      (set-window-dedicated-p (selected-window) nil))))

(describe "window-move-left"
  (before-each (delete-other-windows))
  (after-each (delete-other-windows))

  (it "swaps with the left window and follows"
    (let ((buf-a (get-buffer-create "*wm-a*"))
          (buf-b (get-buffer-create "*wm-b*"))
          (left (selected-window))
          (right (split-window nil nil 'right)))
      (set-window-buffer left buf-a)
      (set-window-buffer right buf-b)
      (select-window right)
      (window-move-left)
      (expect (selected-window) :to-be left)
      (expect (window-buffer left) :to-be buf-b)
      (expect (window-buffer right) :to-be buf-a)))

  (it "wraps a full-span left-edge window to the far right"
    (let (called)
      (split-window nil nil 'right)
      (cl-letf (((symbol-function 'evil-window-move-far-right)
                 (lambda () (setq called t))))
        (window-move-left))
      (expect called :to-be-truthy))))

(describe "window-move-down"
  (before-each (delete-other-windows))
  (after-each (delete-other-windows))

  (it "throws to the very bottom when the only neighbors are left/right"
    (let (called)
      (split-window nil nil 'right)
      (cl-letf (((symbol-function 'evil-window-move-very-bottom)
                 (lambda () (setq called t))))
        (window-move-down))
      (expect called :to-be-truthy)))

  (it "wraps a full-span bottom-edge window to the very top"
    (let (called)
      (select-window (split-window nil nil 'below))
      (cl-letf (((symbol-function 'evil-window-move-very-top)
                 (lambda () (setq called t))))
        (window-move-down))
      (expect called :to-be-truthy)))

  (it "swaps with the window below and follows"
    (let ((bottom (split-window nil nil 'below)))
      (window-move-down)
      (expect (selected-window) :to-be bottom))))

(describe "window-move-up"
  (before-each (delete-other-windows))
  (after-each (delete-other-windows))

  (it "wraps a full-span top-edge window to the very bottom"
    (let (called)
      (split-window nil nil 'below)
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
