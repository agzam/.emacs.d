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

;; Neither edit-indirect nor markdown-mode is loaded when this file is
;; compiled; the defvars also make the `let' below bind dynamically.
(defvar edit-indirect-guess-mode-function)
(defvar edit-indirect--overlay)
(declare-function edit-indirect-region "edit-indirect")
(declare-function markdown-edit-code-block "markdown-mode")
(declare-function markdown-get-enclosing-fenced-block-construct "markdown-mode")

(defun markdown-blockquote-bounds ()
  "Return (BEG . END) spanning the whole blockquote lines at point, or nil."
  (save-excursion
    (forward-line 0)
    (when (looking-at-p "[ \t]*>")
      (while (and (not (bobp))
                  (save-excursion (forward-line -1) (looking-at-p "[ \t]*>")))
        (forward-line -1))
      (let ((beg (point)))
        (while (and (not (eobp)) (looking-at-p "[ \t]*>"))
          (forward-line 1))
        (cons beg (point))))))

(defun markdown-blockquote-unquote ()
  "Strip one level of `>' marks from every line of the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "[ \t]*> ?")
        (replace-match ""))
      (forward-line 1))))

(defun markdown-blockquote-requote (text indent)
  "Return TEXT with INDENT and a `>' mark before each of its lines."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (not (eobp))
      (insert indent)
      (if (looking-at "[ \t]*$")
          (replace-match ">" t t)
        (insert "> "))
      (forward-line 1))
    (buffer-string)))

(defun markdown-blockquote-point (start)
  "Return point as (LINES . CHARS-TO-EOL), LINES counted from START.
Counting from the line end keeps it valid when the marks come or go."
  (cons (count-lines start (line-beginning-position))
        (- (line-end-position) (point))))

(defun markdown-blockquote-goto (start pos)
  "Go to POS, a `markdown-blockquote-point' value taken against START."
  (goto-char start)
  (forward-line (car pos))
  (goto-char (max (line-beginning-position) (- (line-end-position) (cdr pos)))))

(defvar-local markdown-blockquote-start nil
  "Marker where the blockquote of this edit buffer starts in its source.")

;;;###autoload
(defun markdown-edit-blockquote ()
  "Edit the blockquote at point in a separate buffer, without its `>' marks."
  (interactive "*")
  (require 'edit-indirect)
  (pcase-let* ((`(,beg . ,end) (or (markdown-blockquote-bounds)
                                   (user-error "Not in a blockquote")))
               (pos (markdown-blockquote-point beg))
               (start (copy-marker beg))
               (indent (save-excursion
                         (goto-char beg)
                         (skip-chars-forward " \t")
                         (buffer-substring-no-properties beg (point))))
               (mode (if (derived-mode-p 'gfm-mode) 'gfm-mode 'markdown-mode))
               (edit-indirect-guess-mode-function (lambda (&rest _) (funcall mode))))
    (with-current-buffer (edit-indirect-region beg end t)
      ;; an edit buffer already open for this quote comes back as is
      (unless (overlay-get edit-indirect--overlay 'blockquote-indent)
        (overlay-put edit-indirect--overlay 'blockquote-indent indent)
        (markdown-blockquote-unquote)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)
        (markdown-blockquote-goto (point-min) pos)
        (setq markdown-blockquote-start start)))))

;;;###autoload
(defun markdown-edit-special ()
  "Edit the code block or the blockquote at point in a separate buffer."
  (interactive)
  ;; code first: lines inside a fence that start with `>' are code
  (cond ((markdown-get-enclosing-fenced-block-construct)
         (call-interactively #'markdown-edit-code-block))
        ((markdown-blockquote-bounds)
         (call-interactively #'markdown-edit-blockquote))
        (t (user-error "Not in a code block or blockquote"))))

;;;###autoload
(defun markdown-blockquote-commit-a (commit)
  "Run COMMIT, then go to the source spot matching a blockquote edit buffer's."
  (if-let* ((start markdown-blockquote-start))
      (let ((pos (markdown-blockquote-point (point-min))))
        (funcall commit)
        ;; the commit collapses every point inside the quote to its start
        (with-current-buffer (marker-buffer start)
          (markdown-blockquote-goto start pos))
        (set-marker start nil))
    (funcall commit)))

;;;###autoload
(defun markdown-blockquote-commit-h (beg end)
  "Requote the text a blockquote edit buffer committed to BEG..END."
  (when-let* ((ov (seq-find (lambda (o) (overlay-get o 'blockquote-indent))
                            (overlays-in beg end)))
              ;; Requote from the edit buffer, not in place: markdown-mode's
              ;; after-commit fixup runs first and re-indents the region.
              (quoted (markdown-blockquote-requote
                       (with-current-buffer (overlay-get ov 'edit-indirect-buffer)
                         (buffer-substring-no-properties (point-min) (point-max)))
                       (overlay-get ov 'blockquote-indent))))
    ;; requoting runs `looking-at', so the match data come after it
    (save-excursion
      (set-match-data (list beg end))
      (replace-match quoted t t))))

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