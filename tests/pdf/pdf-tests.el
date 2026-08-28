;;; tests/pdf/pdf-tests.el --- pdf-tools specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; pdf-tools isn't installed in the batch tier; the helpers only read these
;; vars / call these fns at runtime, so declare/stub them.
(defvar pdf-view-continuous nil)
(defvar pdf-view-themed-minor-mode nil)

(load-module-file "modules/pdf/autoload/pdf.el")

(describe "pdf-toggle-continuous-scroll"
  (it "flips pdf-view-continuous inside a pdf-view buffer"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let ((pdf-view-continuous nil))
        (pdf-toggle-continuous-scroll)
        (expect pdf-view-continuous :to-be t)
        (pdf-toggle-continuous-scroll)
        (expect pdf-view-continuous :to-be nil))))

  (it "is a no-op outside pdf-view-mode"
    (with-temp-buffer
      (let ((pdf-view-continuous nil))
        (pdf-toggle-continuous-scroll)
        (expect pdf-view-continuous :to-be nil)))))

(describe "pdf-evil-goto-first-line"
  (it "jumps to the first page when continuous"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let ((pdf-view-continuous t) called)
        (cl-letf (((symbol-function 'pdf-view-first-page)
                   (lambda () (setq called 'page)))
                  ((symbol-function 'image-scroll-down)
                   (lambda (&rest _) (setq called 'scroll))))
          (pdf-evil-goto-first-line)
          (expect called :to-be 'page)))))

  (it "scrolls within the page when not continuous"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let ((pdf-view-continuous nil) called)
        (cl-letf (((symbol-function 'pdf-view-first-page)
                   (lambda () (setq called 'page)))
                  ((symbol-function 'image-scroll-down)
                   (lambda (&rest _) (setq called 'scroll))))
          (pdf-evil-goto-first-line)
          (expect called :to-be 'scroll))))))

(describe "pdf-evil-goto-last-line"
  (it "jumps to the last page when continuous"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let ((pdf-view-continuous t) called)
        (cl-letf (((symbol-function 'pdf-view-last-page)
                   (lambda () (setq called 'page)))
                  ((symbol-function 'image-scroll-up)
                   (lambda (&rest _) (setq called 'scroll))))
          (pdf-evil-goto-last-line)
          (expect called :to-be 'page)))))

  (it "scrolls within the page when not continuous"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let ((pdf-view-continuous nil) called)
        (cl-letf (((symbol-function 'pdf-view-last-page)
                   (lambda () (setq called 'page)))
                  ((symbol-function 'image-scroll-up)
                   (lambda (&rest _) (setq called 'scroll))))
          (pdf-evil-goto-last-line)
          (expect called :to-be 'scroll))))))

(describe "pdf-view-current-progress"
  (it "reports the page as a percentage"
    (cl-letf (((symbol-function 'pdf-view-current-page) (lambda () 5))
              ((symbol-function 'pdf-info-number-of-pages) (lambda () 10)))
      (expect (pdf-view-current-progress) :to-equal "at 50.0%"))))

(describe "adjust-pdf-colors-on-theme-change-h"
  (it "re-enables themed mode only in themed pdf-view buffers"
    (let ((themed (generate-new-buffer " test-themed-pdf"))
          (plain (generate-new-buffer " test-plain-pdf"))
          (other (generate-new-buffer " test-not-pdf"))
          reenabled)
      (unwind-protect
          (progn
            (with-current-buffer themed
              (setq major-mode 'pdf-view-mode)
              (setq-local pdf-view-themed-minor-mode t))
            (with-current-buffer plain
              (setq major-mode 'pdf-view-mode)
              (setq-local pdf-view-themed-minor-mode nil))
            (with-current-buffer other
              (setq-local pdf-view-themed-minor-mode t))
            (cl-letf (((symbol-function 'pdf-view-themed-minor-mode)
                       (lambda (&rest _) (push (current-buffer) reenabled))))
              (adjust-pdf-colors-on-theme-change-h nil))
            (expect (memq themed reenabled) :to-be-truthy)
            (expect (memq plain reenabled) :to-be nil)
            (expect (memq other reenabled) :to-be nil))
        (kill-buffer themed)
        (kill-buffer plain)
        (kill-buffer other)))))

(defun pdf-tests--read-form (marker)
  "Read the form that starts at MARKER in modules/pdf/config.el.
The spec reads source: config.el cannot be loaded in the batch tier, where
neither `use-package' nor general (behind `map!') is available."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/pdf/config.el" test-config-root))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward marker)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(describe "epdfinfo program location"
  (let (form path)
    (before-all
      (setq form (pdf-tests--read-form "(setopt pdf-info-epdfinfo-program")
            path (expand-file-name (eval (nth 2 form) t))))

    (it "keeps the server binary out of the package directories"
      ;; The regression: pdf-tools-install leaves the binary in the elpaca
      ;; build dir, elpaca wipes that dir on the next build, and every
      ;; `bb update' ends with PDFs opening in fundamental-mode.  The source
      ;; tree is no home either - `pdf-tools-install' installs from there to
      ;; this path, and install refuses to copy a file onto itself.
      (expect (nth 1 form) :to-be 'pdf-info-epdfinfo-program)
      (expect (file-in-directory-p path doom-data-dir) :to-be-truthy))

    (it "names a directory that exists before the build runs"
      ;; `pdf-tools-build-server' cl-check-types its target directory, so a
      ;; path through a directory nothing creates errors out before autobuild
      ;; gets its own mkdir -p.
      (expect (file-directory-p (file-name-directory path)) :to-be-truthy))))

;;; pdf-tests.el ends here
