;;; modules/pdf/autoload/buffer-to-pdf.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Browser fallback for the buffer-to-pdf package, whose commands go through
;; `x-export-frames' - a Cairo-only function with no NS equivalent, so on
;; macOS they refuse to run.  Off Cairo the buffer is htmlized and printed by
;; a headless Chromium instead; the advice in config.el picks the path.
;;; Code:

(defvar chromium-candidates
  '("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    "/Applications/Chromium.app/Contents/MacOS/Chromium")
  "Chromium-family binaries to try before searching the executable path.")

;;;###autoload
(defun chromium-executable ()
  "Return a Chromium-family binary that can print HTML to PDF, or nil."
  (or (seq-find #'file-executable-p chromium-candidates)
      (seq-some #'executable-find '("chromium" "google-chrome" "brave"))))

;;;###autoload
(defun normalize-hex-colors (html)
  "Rewrite the 9- and 12-digit hex colors in HTML as #rrggbb.
Emacs reports frame colors with up to 16 bits per channel and htmlize lets
them through (its \"already #rrggbb\" test is unanchored); browsers parse
only the 8-bit form and drop the rest of the declaration."
  (replace-regexp-in-string
   "#\\(?:[[:xdigit:]]\\{12\\}\\|[[:xdigit:]]\\{9\\}\\)"
   (lambda (color)
     (let* ((digits (substring color 1))
            (channel (/ (length digits) 3)))
       (concat "#"
               (substring digits 0 2)
               (substring digits channel (+ channel 2))
               (substring digits (* 2 channel) (+ (* 2 channel) 2)))))
   html t t))

;;;###autoload
(defun page-size-css (orientation)
  "Return the CSS `size' value for ORIENTATION of `buffer-to-pdf-orientations'."
  (let* ((spec (alist-get orientation buffer-to-pdf-orientations))
         (parameters (if (functionp spec) (funcall spec) spec))
         (pixels (lambda (dimension)
                   (let ((value (alist-get dimension parameters)))
                     (if (consp value) (cdr value) value)))))
    (unless parameters
      (user-error "Unknown orientation `%s'" orientation))
    (format "%dpx %dpx" (funcall pixels 'width) (funcall pixels 'height))))

(defun print-stylesheet (page-size)
  "Return the CSS that lays htmlize output onto a PAGE-SIZE page.
Full bleed (no page margin, padding on the body instead) so the theme
background covers the sheet, and soft wrapping so long lines are not clipped.
A bound `buffer-to-pdf-monochrome' flattens the face colors, as it does on
the Cairo path."
  (concat
   (format "
      @page { size: %s; margin: 0; }
      html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
      body { margin: 0; padding: 10mm; }
      pre { white-space: pre-wrap; word-break: break-word; }
"
           page-size)
   (when-let* ((monochrome (bound-and-true-p buffer-to-pdf-monochrome)))
     (format "
      body { background-color: %s !important; color: %s !important; }
      body * { background-color: transparent !important; color: inherit !important; }
"
             (car monochrome) (cdr monochrome)))))

;;;###autoload
(defun buffer-html-for-print (buffer page-size)
  "Return BUFFER as htmlized HTML, styled for a PAGE-SIZE page."
  (require 'htmlize)
  (let ((htmlized (with-current-buffer buffer (htmlize-buffer))))
    (unwind-protect
        (normalize-hex-colors
         (with-current-buffer htmlized
           (goto-char (point-min))
           (when (search-forward "</style>" nil t)
             (goto-char (match-beginning 0))
             (insert (print-stylesheet page-size)))
           (buffer-string)))
      (kill-buffer htmlized))))

;;;###autoload
(defun exported-pdf-path (buffer)
  "Return the file `buffer-to-pdf' writes for BUFFER.
The package's own naming rule, so both export paths land on one file and the
advice can open it without asking either of them."
  (expand-file-name
   (format "%s.pdf" (if-let* ((file (buffer-file-name buffer)))
                        (file-name-base file)
                      (buffer-name buffer)))
   buffer-to-pdf-directory))

;;;###autoload
(defun show-pdf (path)
  "Display the PDF at PATH in another window.
Re-exporting a buffer overwrites the same file, and nothing here auto-reverts,
so a window still showing the previous render has to be refreshed by hand."
  (when-let* ((stale (get-file-buffer path)))
    (with-current-buffer stale (revert-buffer :ignore-auto :noconfirm)))
  (find-file-other-window path))

;;;###autoload
(defun print-buffer-with-browser (buffer orientation)
  "Write BUFFER to a PDF with a headless Chromium, sized by ORIENTATION.
Same faces and colors as on screen, but wrapping and page breaks are the
browser's, not redisplay's."
  (let* ((browser (or (chromium-executable)
                      (user-error "Found no Chromium-family browser to print with")))
         (pdf (exported-pdf-path buffer))
         (html (make-temp-file "buffer-to-pdf" nil ".html"
                               (buffer-html-for-print buffer (page-size-css orientation)))))
    (message "Printing %s through %s..." (buffer-name buffer)
             (file-name-nondirectory browser))
    (unwind-protect
        (with-temp-buffer
          (unless (eq 0 (call-process browser nil t nil
                                      "--headless" "--disable-gpu"
                                      "--no-pdf-header-footer"
                                      (concat "--print-to-pdf=" pdf)
                                      html))
            (error "Printing failed: %s" (string-trim (buffer-string))))
          (message "Wrote %s" pdf)
          pdf)
      (delete-file html))))

;;; buffer-to-pdf.el ends here
