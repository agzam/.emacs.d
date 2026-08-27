;;; modules/writing/autoload/markdown.el -*- lexical-binding: t; -*-

;;;###autoload
(defun markdown-wrap-collapsible ()
  "Wrap region in a collapsible section."
  (interactive)
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (content (delete-and-extract-region beg end))
           ;; typically content inside collapsible needs indentation
           (content (with-temp-buffer
                      (insert content)
                      (indent-rigidly (point-min) (point-max) 4)
                      (buffer-substring (point-min) (point-max)))))
      (insert
       (format "<details>\n  <summary></summary>\n\n%s\n\n</details>" content))
      (goto-char beg)
      (search-forward "<summary>")
      (when evil-mode
        (evil-insert-state)))))

;;;###autoload
(defun markdown-wrap-code-generic ()
  "Wrap region in a plain fenced code block."
  (interactive)
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (content (buffer-substring beg end)))
      (delete-region beg end)
      (deactivate-mark)
      (insert
       (format "```\n%s\n```\n" content))
      (search-backward "```" nil :noerror 2)
      (forward-char 3)
      (when evil-mode
        (evil-insert-state)))))

;;;###autoload
(defun markdown-toggle-blockquote ()
  "Add or strip the `> ' marker on every line of the region.
Strips when each non-blank line already carries the marker, so the same
key round-trips.  Blank lines get a bare `>' to keep the quote one block."
  (interactive)
  (when (region-active-p)
    (let* ((beg (save-excursion
                  (goto-char (region-beginning))
                  (line-beginning-position)))
           (end (save-excursion
                  (goto-char (region-end))
                  ;; a line-wise selection ends at the next line's bol
                  (if (and (bolp) (< beg (point)))
                      (1- (point))
                    (line-end-position))))
           (lines (split-string (buffer-substring-no-properties beg end) "\n"))
           (content (seq-remove #'string-blank-p lines))
           (quoted (and content
                        (seq-every-p
                         (lambda (line) (string-match-p "\\`[ \t]*>" line))
                         content))))
      (delete-region beg end)
      (goto-char beg)
      (insert (mapconcat
               (lambda (line)
                 (cond
                  (quoted (replace-regexp-in-string "\\`[ \t]*> ?" "" line))
                  ((string-blank-p line) ">")
                  (t (concat "> " line))))
               lines "\n"))
      (deactivate-mark)
      (goto-char beg))))

;;;###autoload
(defun markdown-wrap-code-clojure ()
  "Wrap region in a clojure fenced code block."
  (interactive)
  (funcall 'markdown-wrap-code-generic)
  (insert "clojure")
  (search-forward "```" nil :noerror))

;; Declared so the `let' below binds dynamically; grip-mode is not loaded when
;; this file is compiled.
(defvar grip-preview-in-webkit)
(declare-function grip-browse-preview "grip-mode")

;;;###autoload
(defun grip-preview-in-browser ()
  "Show the grip preview in the external browser instead of the xwidget.
The embedded webkit cannot print, so saving to PDF needs a real browser."
  (interactive)
  (require 'grip-mode)
  (let ((grip-preview-in-webkit nil))
    (grip-browse-preview)))

(defvar markdown-stored-links nil
  "Stores markdown links as (label file heading-text)")

;;;###autoload
(defun markdown-store-link ()
  "Store current markdown heading as a link."
  (interactive)
  (save-excursion
    (unless (markdown-heading-at-point)
      (markdown-back-to-heading))
    (let* ((heading-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
           (heading-text (replace-regexp-in-string "^#+ " "" heading-line))
           (file-path (buffer-file-name))
           (label (format "%s#%s"
                          (file-name-nondirectory file-path)
                          (replace-regexp-in-string " " "-" (downcase heading-text)))))
      (add-to-list 'markdown-stored-links (cons label (list file-path heading-text)))
      (message "Stored link to: %s" label))))

;;;###autoload
(defun markdown-insert-stored-link ()
  "Insert a link to a heading stored via `markdown-store-link'."
  (interactive)
  (if (not (seq-empty-p markdown-stored-links))
      (let* ((sel (thread-first
                    (completing-read
                     "Heading: "
                     markdown-stored-links
                     nil :require-match)
                    (assoc markdown-stored-links)))
             (file-path (nth 1 sel))
             (heading-text (nth 2 sel))
             (lbl (replace-regexp-in-string " " "-" (downcase heading-text)))
             (label (if (equal (file-truename file-path)
                               (file-truename (buffer-file-name)))
                        (format "#%s" lbl)
                      (format "%s#%s" (file-relative-name file-path) lbl))))
        (markdown-insert-inline-link heading-text label))))