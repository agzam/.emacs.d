;;; tests/pdf/org-noter-tests.el --- pdf/autoload/org-noter.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
;; org-noter-transient is a transient-define-prefix; the parser needs transient.
(require 'transient)

(load-module-file "modules/pdf/autoload/org-noter.el")

(describe "org-noter-sync"
  (it "syncs the page/chapter in a pdf-view buffer"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'org-noter-sync-current-page-or-chapter)
                   (lambda () (setq called 'pdf)))
                  ((symbol-function 'org-noter-sync-current-note)
                   (lambda () (setq called 'org))))
          (org-noter-sync)
          (expect called :to-be 'pdf)))))

  (it "syncs the note in an org buffer"
    (with-temp-buffer
      (setq major-mode 'org-mode)
      (let (called)
        (cl-letf (((symbol-function 'org-noter-sync-current-page-or-chapter)
                   (lambda () (setq called 'pdf)))
                  ((symbol-function 'org-noter-sync-current-note)
                   (lambda () (setq called 'org))))
          (org-noter-sync)
          (expect called :to-be 'org))))))

;; In a pdf-view buffer the scroll/page helpers act on the document directly
;; (no session); the org-buffer branch rides org-noter--with-valid-session and
;; is exercised in the live probe with a real session.
(describe "pdf-view-mode scroll/page helpers"
  (it "scroll-down advances the pdf"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'pdf-view-scroll-up-or-next-page)
                   (lambda (&rest _) (setq called t))))
          (org-noter-pdf-scroll-down)
          (expect called :to-be t)))))

  (it "scroll-up rewinds the pdf"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'pdf-view-scroll-down-or-previous-page)
                   (lambda (&rest _) (setq called t))))
          (org-noter-pdf-scroll-up)
          (expect called :to-be t)))))

  (it "next-page turns the page forward"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'pdf-view-next-page)
                   (lambda (&rest _) (setq called t))))
          (org-noter-pdf-next-page)
          (expect called :to-be t)))))

  (it "prev-page turns the page back"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'pdf-view-previous-page)
                   (lambda (&rest _) (setq called t))))
          (org-noter-pdf-prev-page)
          (expect called :to-be t)))))

  (it "top-of-the-page delegates to pdf-evil-goto-first-line"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'pdf-evil-goto-first-line)
                   (lambda () (setq called t))))
          (org-noter-top-of-the-page)
          (expect called :to-be t)))))

  (it "bottom-of-the-page delegates to pdf-evil-goto-last-line"
    (with-temp-buffer
      (setq major-mode 'pdf-view-mode)
      (let (called)
        (cl-letf (((symbol-function 'pdf-evil-goto-last-line)
                   (lambda () (setq called t))))
          (org-noter-bottom-of-the-page)
          (expect called :to-be t))))))

(describe "org-noter-transient layout"
  (it "wires the documented suffixes; module-owned ones are defined"
    (let ((cmds (transient-layout-commands
                 (get 'org-noter-transient 'transient--layout))))
      ;; every named suffix appears (org-noter/-insert-note are the package's)
      (dolist (sym '(org-noter org-noter-insert-note org-noter-sync
                     org-noter-anchor-to-current-page
                     org-noter-pdf-scroll-down org-noter-pdf-scroll-up
                     org-noter-pdf-prev-page org-noter-pdf-next-page
                     org-noter-top-of-the-page org-noter-bottom-of-the-page))
        (expect (memq sym cmds) :to-be-truthy))
      ;; the module owns these, so they must resolve
      (dolist (sym '(org-noter-sync org-noter-anchor-to-current-page
                     org-noter-pdf-scroll-down org-noter-pdf-scroll-up
                     org-noter-pdf-prev-page org-noter-pdf-next-page
                     org-noter-top-of-the-page org-noter-bottom-of-the-page))
        (expect (fboundp sym) :to-be-truthy)))))

;;; org-noter-tests.el ends here
