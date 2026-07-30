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

(defvar page-padding 60
  "Pixels of margin around the text on a page.
Mirrors the `internal-border-width' the package gives its export frames, so
both paths frame the text the same way.")

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
(defun page-pixels (orientation)
  "Return the text area of ORIENTATION as a (WIDTH . HEIGHT) pixel pair.
ORIENTATION is a key of `buffer-to-pdf-orientations', whose dimensions
describe the text, not the sheet - the package adds its border outside them."
  (let* ((spec (alist-get orientation buffer-to-pdf-orientations))
         (parameters (if (functionp spec) (funcall spec) spec))
         (pixels (lambda (dimension)
                   (let ((value (alist-get dimension parameters)))
                     (if (consp value) (cdr value) value)))))
    (unless parameters
      (user-error "Unknown orientation `%s'" orientation))
    (cons (funcall pixels 'width) (funcall pixels 'height))))

;;;###autoload
(defun default-font-metrics ()
  "Return the default face's (FAMILY SIZE CELL-WIDTH CELL-HEIGHT) in pixels."
  (list (face-attribute 'default :family)
        (font-get (face-attribute 'default :font) :size)
        (frame-char-width)
        (frame-char-height)))

;;;###autoload
(defun line-spacing-pixels (buffer)
  "Return the pixels Emacs puts between BUFFER's lines.
`line-spacing' is either a pixel count or a fraction of the character cell,
and the frame parameter stands in when the buffer has no value of its own."
  (let* ((cell (frame-char-height))
         (spacing (or (buffer-local-value 'line-spacing buffer)
                      (frame-parameter nil 'line-spacing)
                      0)))
    (if (floatp spacing) (round (* spacing cell)) spacing)))

(defun print-stylesheet (width height word-wrap spacing)
  "Return the CSS laying htmlized output onto a page of WIDTH by HEIGHT text.
Hands the frame's own font, cell height and column count to the browser:
left to its defaults it picks another typeface at another size and breaks
lines nowhere near where Emacs does.  The text box is sized in `ch' so the
wrap column survives the browser's slightly different glyph advance, and
loses a column to the continuation glyph the way a fringeless Emacs window
does.  WORD-WRAP mirrors the buffer's setting: nil means Emacs breaks
mid-word, which CSS only does under `break-all'.  SPACING is the buffer's
`line-spacing' in pixels; it goes into a unitless line-height, which children
inherit as a factor and so scale with a heading's larger font, the way Emacs
does.  A length there - pixels or calc - would compute once on the `pre' and
leave every heading squashed into a default-sized line.  A bound
`buffer-to-pdf-monochrome' flattens the face colors, as on the Cairo path."
  (pcase-let ((`(,family ,size ,cell-width ,cell-height) (default-font-metrics)))
    (concat
     (format "
      /* No page margin: it sits outside the canvas, so anything painted stops
         at it and every sheet comes out with a white frame.  The inset is the
         text block's own padding instead, cloned onto each fragment so every
         page keeps it. */
      @page { size: %dpx %dpx; margin: 0; }
      html, body { margin: 0; padding: 0; background-color: %s;
                   -webkit-print-color-adjust: exact; print-color-adjust: exact; }
      body, pre { font-family: \"%s\", monospace; font-size: %dpx;
                  line-height: %.4f; }
      pre { width: %dch; padding: %dpx; white-space: pre-wrap; word-break: %s;
            background-color: %s;
            -webkit-box-decoration-break: clone; box-decoration-break: clone; }
"
             (+ width (* 2 page-padding)) (+ height (* 2 page-padding))
             (face-attribute 'default :background)
             family size (/ (float (+ cell-height spacing)) size)
             (1- (/ width cell-width)) page-padding
             (if word-wrap "normal" "break-all")
             (face-attribute 'default :background))
     (when-let* ((monochrome (bound-and-true-p buffer-to-pdf-monochrome)))
       (format "
      body { background-color: %s !important; color: %s !important; }
      body * { background-color: transparent !important; color: inherit !important; }
"
               (car monochrome) (cdr monochrome))))))

;;;###autoload
(defun composed-text (composition)
  "Return the characters COMPOSITION puts on screen, or nil.
Only static compositions - what `compose-region' makes, as org-superstar and
`prettify-symbols-mode' do - are spelled out.  Automatic ones are the shaper's
doing, and their buffer text is what belongs in a document: substituting them
would rewrite ligatures into whatever glyphs the font happened to pick."
  (when (< 3 (length composition))
    (let ((components (nth 2 composition)))
      (cond ((vectorp components)
             (apply #'string (seq-filter #'characterp (append components nil))))
            ((characterp components) (string components))
            ((stringp components) components)))))

;;;###autoload
(defun buffer-copy-as-displayed (buffer)
  "Return a copy of BUFFER with what redisplay draws spelled out as text.
htmlize reads buffer text, so a composed glyph reaches the page as the
asterisks it was composed from and `line-prefix' indentation not at all.
Substituting both up front is what puts org-superstar's bullets and
org-indent's indentation into the export."
  (let ((copy (generate-new-buffer " *buffer-to-pdf-copy*")))
    (with-current-buffer copy
      (insert-buffer-substring buffer)
      ;; Folding is overlays here, and the spec decides what counts as hidden;
      ;; without both, an export shows everything the buffer has folded away.
      (setq-local buffer-invisibility-spec
                  (buffer-local-value 'buffer-invisibility-spec buffer)))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (let ((clone (make-overlay (overlay-start overlay) (overlay-end overlay) copy)))
          (dolist (property '(invisible display face before-string after-string priority))
            (when-let* ((value (overlay-get overlay property)))
              (overlay-put clone property value))))))
    (with-current-buffer copy
      ;; Substitute in place, so the cloned overlays ride the edits.
      (let ((position (point-min)))
        (while (< position (point-max))
          (let* ((composition (find-composition position nil nil t))
                 (glyphs (and composition (composed-text composition))))
            (if glyphs
                (pcase-let ((`(,from ,to . ,_) composition))
                  (let ((face (get-text-property from 'face)))
                    (goto-char from)
                    (delete-region from to)
                    (insert (propertize glyphs 'face face))
                    (setq position (point))))
              (setq position (or (next-single-property-change position 'composition)
                                 (point-max)))))))
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((prefix (get-text-property (point) 'line-prefix)))
          (unless (string-empty-p prefix)
            (insert prefix)))
        (forward-line 1)))
    copy))

;;;###autoload
(defun buffer-html-for-print (buffer orientation)
  "Return BUFFER as htmlized HTML, styled for an ORIENTATION page."
  (require 'htmlize)
  (pcase-let* ((`(,width . ,height) (page-pixels orientation))
               (wrap (buffer-local-value 'word-wrap buffer))
               (source (buffer-copy-as-displayed buffer))
               (htmlized (with-current-buffer source (htmlize-buffer))))
    (unwind-protect
        (normalize-hex-colors
         (with-current-buffer htmlized
           (goto-char (point-min))
           (when (search-forward "</style>" nil t)
             (goto-char (match-beginning 0))
             (insert (print-stylesheet width height wrap
                                       (line-spacing-pixels buffer))))
           (buffer-string)))
      (kill-buffer htmlized)
      (kill-buffer source))))

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
so a window still showing the previous render has to be refreshed by hand.
Saved places are kept out of it: an export's page count changes with every
run, and `saveplace-pdf-view' restoring a page past the new end aborts the
visit with \"No such page\"."
  (when-let* ((stale (get-file-buffer path)))
    (with-current-buffer stale (revert-buffer :ignore-auto :noconfirm)))
  (let ((save-place-alist nil))
    (find-file-other-window path)))

;;;###autoload
(defun print-buffer-with-browser (buffer orientation)
  "Write BUFFER to a PDF with a headless Chromium, sized by ORIENTATION.
Faces, font and wrap column match the screen and the text stays selectable,
but only what htmlize can see survives: compositions such as org-superstar's
bullets, and anything drawn from a display property, do not."
  (let* ((browser (or (chromium-executable)
                      (user-error "Found no Chromium-family browser to print with")))
         (pdf (exported-pdf-path buffer))
         (html (make-temp-file "buffer-to-pdf" nil ".html"
                               (buffer-html-for-print buffer orientation))))
    (message "Printing %s..." (buffer-name buffer))
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
