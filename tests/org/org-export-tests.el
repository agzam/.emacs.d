;;; tests/org/org-export-tests.el --- org/autoload/org-export.el specs -*- lexical-binding: t; -*-
;; The shell/clipboard side is smoke-only; here we stub the sinks and assert
;; org-export-to-clipboard-as-rich-text dispatches on major-mode.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/org/autoload/org-export.el")

(describe "org-export-to-clipboard-as-rich-text"
  (it "routes markdown buffers through org--yank-html-buffer"
    (let (yanked)
      (cl-letf (((symbol-function 'markdown) (lambda (&rest _) 'md-buffer))
                ((symbol-function 'org--yank-html-buffer)
                 (lambda (buf) (setq yanked buf)))
                ((symbol-function 'ox-clip-formatted-copy)
                 (lambda (&rest _) (error "ox-clip path should not run"))))
        (with-temp-buffer
          (setq major-mode 'markdown-mode)
          (org-export-to-clipboard-as-rich-text (point-min) (point-max))))
      (expect yanked :to-be 'md-buffer)))

  (it "routes gfm buffers through org--yank-html-buffer"
    (let (yanked)
      (cl-letf (((symbol-function 'markdown) (lambda (&rest _) 'gfm-buffer))
                ((symbol-function 'org--yank-html-buffer)
                 (lambda (buf) (setq yanked buf))))
        (with-temp-buffer
          (setq major-mode 'gfm-mode)
          (org-export-to-clipboard-as-rich-text (point-min) (point-max))))
      (expect yanked :to-be 'gfm-buffer)))

  (it "falls back to ox-clip-formatted-copy for other modes"
    (let (copied)
      (cl-letf (((symbol-function 'ox-clip-formatted-copy)
                 (lambda (beg end) (setq copied (list beg end)))))
        (with-temp-buffer
          (insert "hello")
          (setq major-mode 'text-mode)
          (org-export-to-clipboard-as-rich-text (point-min) (point-max))))
      (expect copied :to-equal '(1 6)))))
