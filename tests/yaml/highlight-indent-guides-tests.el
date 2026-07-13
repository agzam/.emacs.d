;;; tests/yaml/highlight-indent-guides-tests.el --- yaml module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/yaml/autoload/highlight-indent-guides.el")

;; highlight-indent-guides isn't installed in the batch tier; the hooks only
;; ever run with the package loaded, so declare its vars special and stub
;; the calls (fold-tests precedent).
(defvar highlight-indent-guides-mode)
(defvar org-indent-mode)

(describe "indent-guides-init-faces-h"
  (it "refreshes the guide faces on a graphical frame"
    (let (called)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'highlight-indent-guides-auto-set-faces)
                 (lambda (&rest _) (setq called t))))
        (indent-guides-init-faces-h)
        (expect called :to-be t))))

  (it "skips the refresh on a terminal frame (256 colors can't derive them)"
    (let (called)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
                ((symbol-function 'highlight-indent-guides-auto-set-faces)
                 (lambda (&rest _) (setq called t))))
        (indent-guides-init-faces-h)
        (expect called :to-be nil))))

  (it "ignores hook arguments (it rides doom-load-theme-hook)"
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
              ((symbol-function 'highlight-indent-guides-auto-set-faces) #'ignore))
      (expect (indent-guides-init-faces-h 'theme 'extra) :not :to-throw))))

(describe "indent-guides-disable-maybe-h"
  (it "turns the guides off when org-indent-mode owns the buffer"
    (let ((highlight-indent-guides-mode t)
          (org-indent-mode t)
          arg)
      (cl-letf (((symbol-function 'highlight-indent-guides-mode)
                 (lambda (n) (setq arg n))))
        (indent-guides-disable-maybe-h)
        (expect arg :to-be -1))))

  (it "leaves the guides alone when they are already off"
    (let ((highlight-indent-guides-mode nil)
          (org-indent-mode t)
          called)
      (cl-letf (((symbol-function 'highlight-indent-guides-mode)
                 (lambda (&rest _) (setq called t))))
        (indent-guides-disable-maybe-h)
        (expect called :to-be nil))))

  (it "leaves the guides alone outside org-indent-mode"
    (let ((highlight-indent-guides-mode t)
          (org-indent-mode nil)
          called)
      (cl-letf (((symbol-function 'highlight-indent-guides-mode)
                 (lambda (&rest _) (setq called t))))
        (indent-guides-disable-maybe-h)
        (expect called :to-be nil)))))
