;;; modules/pdf/autoload/pdf.el -*- lexical-binding: t; -*-
;;; Commentary:
;; pdf-view helpers.  In doom.d these lived (mislabelled) inside the
;; org-noter autoload file; split out here.  `pdf-view-continuous' is a
;; long-standing pdf-tools variable (per-line "roll onto next page"), distinct
;; from `pdf-view-roll-minor-mode' (the multi-page smooth roll).
;;; Code:

;;;###autoload
(defun adjust-pdf-colors-on-theme-change-h (_)
  "Keep pdf-view-themed buffers in sync with main color-theme changes."
  (thread-last
    (buffer-list)
    (seq-filter
     (lambda (b)
       (with-current-buffer b (eq major-mode 'pdf-view-mode))))
    (seq-do
     (lambda (b)
       (with-current-buffer b
         (when pdf-view-themed-minor-mode
           (pdf-view-themed-minor-mode +1)))))))

;;;###autoload
(defun pdf-view-current-progress ()
  "Show current progress in a PDF document."
  (interactive)
  (message "at %.1f%%"
           (* 100 (/ (float (pdf-view-current-page))
                     (float (pdf-info-number-of-pages))))))

;;;###autoload
(defun pdf-toggle-continuous-scroll ()
  "Toggle between single page and scrollable document."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (setq pdf-view-continuous (not pdf-view-continuous))))

;;;###autoload
(defun pdf-evil-goto-first-line ()
  "Go to the first page (continuous) or the top of the page."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (funcall
     (if pdf-view-continuous
         #'pdf-view-first-page
       #'image-scroll-down))))

;;;###autoload
(defun pdf-evil-goto-last-line ()
  "Go to the last page (continuous) or the bottom of the page."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (funcall
     (if pdf-view-continuous
         #'pdf-view-last-page
       #'image-scroll-up))))

;;; pdf.el ends here
