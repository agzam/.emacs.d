;;; modules/pdf/autoload/emureader.el -*- lexical-binding: t; -*-

;;;###autoload
(defun keep-pdf-and-epub-viewers ()
  "Drop emureader's PDF and EPUB `auto-mode-alist' entries.
The package adds them on activation and again when it loads."
  (setq auto-mode-alist
        (seq-remove (lambda (entry)
                      (and (eq (cdr entry) 'emureader-mode)
                           (member (car entry) '("\\.pdf\\'" "\\.epub\\'"))))
                    auto-mode-alist)))

;;;###autoload
(defun toggle-emureader ()
  "Reopen the visited document in emureader, or out of it in its usual mode."
  (interactive)
  (let ((file (or buffer-file-name (user-error "This buffer visits no file"))))
    (if (derived-mode-p 'emureader-mode)
        (let ((major-mode-remap-alist nil))
          (kill-buffer)
          (find-file file))
      (kill-buffer)
      (let ((magic-mode-alist nil)
            (auto-mode-alist '(("" . emureader-mode))))
        (find-file file)))))

;;;###autoload
(defun fit-emureader-page ()
  "Fit the whole page in the selected window, like `pdf-view-fit-page-to-window'."
  (interactive)
  (emureader-fit-to-height)
  (when (< (window-body-width nil t)
           (car (emureader--get-current-doc-image-size)))
    (emureader-fit-to-width)))

;;;###autoload
(defun show-emureader-progress ()
  "Show how far into the document the current page is."
  (interactive)
  (message "at %.1f%%"
           (* 100 (/ (float (emureader-current-doc-pagenumber))
                     emureader-current-doc-pagecount))))

;;;###autoload
(defun refresh-emureader-theme-colors-h (&rest _)
  "Re-render themed emureader buffers in the colors of the current theme."
  (seq-do (lambda (buffer)
            (with-current-buffer buffer
              (when (bound-and-true-p emureader-themed-mode)
                (emureader-themed-mode 1))))
          (match-buffers '(derived-mode . emureader-mode))))

;;;###autoload
(defun emureader-saveplace-skip-foreign-a (&rest _)
  "Let emureader's save-place reader see only records it wrote.
It reads saveplace-pdf-view's records as bookmarks and errors on them."
  (let ((place (cdr (assoc buffer-file-name save-place-alist))))
    (and (vectorp place) (stringp (car-safe (aref place 0))))))
