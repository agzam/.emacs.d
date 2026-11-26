;;; modules/pdf/autoload/emureader.el -*- lexical-binding: t; -*-

;;;###autoload
(defun keep-pdf-viewer ()
  "Drop emureader's PDF `auto-mode-alist' entry, so PDFs keep pdf-view's.
The package adds it on activation and again when it loads."
  (setq auto-mode-alist
        (seq-remove (lambda (entry)
                      (and (eq (cdr entry) 'emureader-mode)
                           (equal (car entry) "\\.pdf\\'")))
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
(defun current-emureader-page-text ()
  "Return the text of the page emureader shows, one line per paragraph.
emureader draws pages as images, so mutool reads the text."
  (let ((file buffer-file-name)
        (page (emureader-current-doc-pagenumber))
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (unless (zerop (call-process "mutool" nil '(t nil) nil
                                   "draw" "-q" "-F" "txt" "-O" "dehyphenate"
                                   "-o" "-" file (number-to-string page)))
        (error "Could not read page %d of %s with mutool" page file))
      (let ((paragraphs (split-string (buffer-string) "\n[ \t\f]*\n" t "[ \t\n\f]+")))
        (mapconcat (lambda (paragraph)
                     (string-join (split-string paragraph "[ \t]*\n[ \t]*" t) " "))
                   paragraphs "\n\n")))))

;;;###autoload
(defun emureader-saveplace-skip-foreign-a (&rest _)
  "Let emureader's save-place reader see only records it wrote.
It reads saveplace-pdf-view's records as bookmarks and errors on them."
  (let ((place (cdr (assoc buffer-file-name save-place-alist))))
    (and (vectorp place) (stringp (car-safe (aref place 0))))))

;;;###autoload
(defun skip-large-file-prompt-for-documents-a (_size _op-type filename &rest _)
  "Ask about the size of FILENAME only if no document viewer opens it."
  (not (memq (let ((case-fold-search t))
               (assoc-default filename auto-mode-alist #'string-match-p))
             '(emureader-mode pdf-view-mode))))
