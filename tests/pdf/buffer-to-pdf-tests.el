;;; tests/pdf/buffer-to-pdf-tests.el --- buffer-to-pdf wiring specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
;; show-pdf binds save-place-alist; without saveplace loaded it is not special
;; and a let would bind it lexically, out of the function's sight.
(require 'saveplace)

(defun buffer-to-pdf-tests--read-form (file marker)
  "Read the form that starts at MARKER in FILE, relative to the config root.
The specs read source: config.el cannot be loaded in the batch tier, where
neither `use-package' nor general (behind `map!') is available."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file test-config-root))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward marker)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(defun buffer-to-pdf-tests--desc-keys (form)
  "Collect the keys FORM binds, each being the element after a `map!' :desc."
  (let (keys)
    (while form
      (cond
       ((eq (car form) :desc)
        (push (nth 2 form) keys)
        (setq form (nthcdr 3 form)))
       ((proper-list-p (car form))
        (setq keys (nconc (buffer-to-pdf-tests--desc-keys (car form)) keys)
              form (cdr form)))
       (t (setq form (cdr form)))))
    keys))

(defvar buffer-to-pdf-tests--use-package
  (buffer-to-pdf-tests--read-form "modules/pdf/config.el" "(use-package buffer-to-pdf"))

(describe "buffer-to-pdf"
  (it "pins an explicit recipe - the package is on no ELPA to resolve"
    (let ((recipe (cadr (memq :ensure buffer-to-pdf-tests--use-package))))
      (expect (plist-get (cdr recipe) :host) :to-be 'github)
      (expect (plist-get (cdr recipe) :repo) :to-equal "protesilaos/buffer-to-pdf")))

  (it "writes its documents outside the config directory"
    (let* ((setting (seq-find (lambda (form) (eq (car-safe form) 'setopt))
                              buffer-to-pdf-tests--use-package))
           (directory (expand-file-name (eval (nth 2 setting) t))))
      (expect (nth 1 setting) :to-be 'buffer-to-pdf-directory)
      (expect (string-prefix-p (expand-file-name test-config-root) directory)
              :to-be nil)))

  (it "keeps the package's own command as the entry point"
    ;; The regression: a separate command left `M-x buffer-to-pdf' - the name
    ;; in the package's README, and the one muscle memory reaches for - doing
    ;; nothing but erroring on this build.
    (let ((advice (buffer-to-pdf-tests--read-form
                   "modules/pdf/config.el" "(defadvice! buffer-to-pdf-fallback-a"))
          (binding (buffer-to-pdf-tests--read-form
                    "modules/pdf/config.el"
                    "(map! :leader :desc \"Export buffer to PDF\"")))
      (expect (cadr (memq :around advice)) :to-equal '(function buffer-to-pdf))
      (expect (memq 'buffer-to-pdf (flatten-tree binding)) :to-be-truthy)
      (expect (memq 'export-buffer-to-pdf (flatten-tree binding)) :to-be nil)))

  (it "dispatches on the build and shows the result"
    (let ((advice (flatten-tree
                   (buffer-to-pdf-tests--read-form
                    "modules/pdf/config.el" "(defadvice! buffer-to-pdf-fallback-a"))))
      ;; Probe the function, not `system-configuration-features': the package's
      ;; own test reads "cairo" against a string spelling it CAIRO, and only
      ;; passes while `case-fold-search' happens to be t (upstream issue #2).
      (expect (memq 'x-export-frames advice) :to-be-truthy)
      (expect (memq 'system-configuration-features advice) :to-be nil)
      (expect (memq 'print-buffer-with-browser advice) :to-be-truthy)
      (expect (memq 'show-pdf advice) :to-be-truthy)))

  (it "claims a leader key the bindings tree leaves free"
    (let ((file-keys (buffer-to-pdf-tests--desc-keys
                      (buffer-to-pdf-tests--read-form
                       "modules/bindings/config.el" "(:prefix-map (\"f\" . \"file\")")))
          (module-keys (buffer-to-pdf-tests--desc-keys
                        (buffer-to-pdf-tests--read-form
                         "modules/pdf/config.el"
                         "(map! :leader :desc \"Export buffer to PDF\""))))
      (expect (member "p" file-keys) :to-be nil)
      (expect (member "f p" module-keys) :to-be-truthy))))

;;; The browser fallback.  htmlize and the buffer-to-pdf package are absent
;;; from the batch tier, so the specs stub them.

;; A path inside the helper's sandbox, not a directory of its own: the specs
;; only ever build names from it, and a per-run temp directory leaks one behind
;; on every `bb test'.
(defvar buffer-to-pdf-directory (expand-file-name "pdf-exports/" test-sandbox-dir))

(defvar buffer-to-pdf-monochrome nil)

(defun buffer-to-pdf-tests--window-orientation ()
  "Stand in for the `current-window' entry, which is a function."
  '((width . (text-pixels . 640)) (height . (text-pixels . 480))))

(defvar buffer-to-pdf-orientations
  '((landscape . ((width . (text-pixels . 1024)) (height . (text-pixels . 768))))
    (current-window . buffer-to-pdf-tests--window-orientation)))

(load-module-file "modules/pdf/autoload/buffer-to-pdf.el")

(describe "normalize-hex-colors"
  (it "truncates the 12-digit colors Emacs frames report"
    ;; The live regression: htmlize passes #fbfbf8f8efee straight through and
    ;; the browser drops the whole declaration, printing an unthemed page.
    (expect (normalize-hex-colors "body { background-color: #fbfbf8f8efee; }")
            :to-equal "body { background-color: #fbf8ef; }"))

  (it "truncates 9-digit colors too"
    (expect (normalize-hex-colors "#abcdefabc") :to-equal "#abdeab"))

  (it "leaves usable colors alone"
    (expect (normalize-hex-colors "a { color: #655370 } b { color: red }")
            :to-equal "a { color: #655370 } b { color: red }")))

(describe "page-pixels"
  (it "reads the pixel dimensions of an orientation"
    (expect (page-pixels 'landscape) :to-equal '(1024 . 768)))

  (it "calls orientations that are functions"
    (expect (page-pixels 'current-window) :to-equal '(640 . 480)))

  (it "refuses an unknown orientation"
    (expect (page-pixels 'origami) :to-throw 'user-error)))

(describe "line-spacing-pixels"
  (it "reads a fraction of the character cell"
    (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 20)))
      (with-temp-buffer
        (setq-local line-spacing 0.35)
        (expect (line-spacing-pixels (current-buffer)) :to-equal 7))))

  (it "takes a pixel count as it stands"
    (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 20)))
      (with-temp-buffer
        (setq-local line-spacing 3)
        (expect (line-spacing-pixels (current-buffer)) :to-equal 3))))

  (it "adds nothing when neither buffer nor frame asks for it"
    (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 20))
              ((symbol-function 'frame-parameter) (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq-local line-spacing nil)
        (expect (line-spacing-pixels (current-buffer)) :to-equal 0)))))

(describe "print-stylesheet"
  ;; Fira Code at 16, in a 10x20 pixel cell - the metrics the calibration was
  ;; measured against.
  (before-each (spy-on 'default-font-metrics
                       :and-return-value '("Fira Code" 16 10 20)))

  (it "bleeds the page and insets the text itself"
    ;; The regression: a page margin sits outside the canvas, so the theme
    ;; background stopped at it and every sheet printed with a white frame.
    ;; The sheet keeps the border in its size, the text block carries it as
    ;; padding, and cloning the decoration puts it on every page fragment.
    (let ((css (print-stylesheet 1024 768 nil 7)))
      (expect css :to-match "@page { size: 1144px 888px; margin: 0; }")
      (expect css :to-match "padding: 60px")
      (expect css :to-match "box-decoration-break: clone")))

  (it "hands the frame's own font to the browser"
    ;; Left alone the browser renders another typeface at another size and
    ;; wraps ~5% off; this is what closes the gap to ~1%.
    (let ((css (print-stylesheet 1024 768 nil 7)))
      (expect css :to-match "font-family: \"Fira Code\", monospace")
      (expect css :to-match "font-size: 16px")))

  (it "carries line-spacing, unitless so headings scale with their font"
    ;; The regression: a length here computes once on the pre, losing the
    ;; buffer's line-spacing and squashing every heading into a default line.
    ;; (20 cell + 7 spacing) / 16 = 1.6875, measured at 27px plain and 35px
    ;; on a 130% heading - Emacs gives 27 and 35.
    (expect (print-stylesheet 1024 768 nil 7) :to-match "line-height: 1\\.6875")
    (expect (print-stylesheet 1024 768 nil 0) :to-match "line-height: 1\\.2500"))

  (it "sizes the text box in columns, one short for the continuation glyph"
    (expect (print-stylesheet 1024 768 nil 7) :to-match "width: 101ch"))

  (it "breaks mid-word unless the buffer wraps on words"
    (expect (print-stylesheet 1024 768 nil 7) :to-match "word-break: break-all")
    (expect (print-stylesheet 1024 768 t 7) :to-match "word-break: normal"))

  (it "keeps the browser from dropping the theme colors"
    (expect (print-stylesheet 1024 768 nil 7) :to-match "print-color-adjust: exact"))

  (it "flattens the faces for the monochrome commands"
    (let ((buffer-to-pdf-monochrome (cons "white" "black")))
      (expect (print-stylesheet 1024 768 nil 7)
              :to-match "background-color: white !important; color: black"))))

(describe "composed-text"
  (it "spells out a static composition"
    ;; (FROM TO COMPONENTS RELATIVE-P MOD-FUNC WIDTH), what compose-region makes.
    (expect (composed-text '(1 2 [?◉] t nil 1)) :to-equal "◉")
    (expect (composed-text '(1 2 ?○ t nil 1)) :to-equal "○"))

  (it "leaves automatic compositions alone"
    ;; Those are (FROM TO GSTRING) - the shaper's ligatures.  Substituting them
    ;; would rewrite source text into whatever glyphs the font chose.
    (expect (composed-text '(1 3 [gstring])) :to-be nil)))

(describe "buffer-copy-as-displayed"
  (it "puts composed glyphs into the text, with the face they were drawn in"
    (with-temp-buffer
      (insert "* heading\n")
      (put-text-property 1 2 'face 'org-level-1)
      (compose-region 1 2 [?◉])
      (let ((copy (buffer-copy-as-displayed (current-buffer))))
        (unwind-protect
            (with-current-buffer copy
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "◉ heading\n")
              (expect (get-text-property (point-min) 'face) :to-be 'org-level-1))
          (kill-buffer copy)))))

  (it "writes line prefixes into the text"
    (with-temp-buffer
      (insert "one\ntwo\n")
      (put-text-property 5 8 'line-prefix "  ")
      (let ((copy (buffer-copy-as-displayed (current-buffer))))
        (unwind-protect
            (expect (with-current-buffer copy
                      (buffer-substring-no-properties (point-min) (point-max)))
                    :to-equal "one\n  two\n")
          (kill-buffer copy)))))

  (it "carries folding across, overlays and invisibility spec both"
    ;; The regression: org folds with overlays here, and a copy without them
    ;; exports every drawer and subtree the buffer has folded away - one export
    ;; went from 4 pages to 12.
    (with-temp-buffer
      (insert "visible\nhidden\n")
      (setq-local buffer-invisibility-spec '((org-fold-outline . " ↴")))
      (overlay-put (make-overlay 9 15) 'invisible 'org-fold-outline)
      (let ((copy (buffer-copy-as-displayed (current-buffer))))
        (unwind-protect
            (progn
              (expect (buffer-local-value 'buffer-invisibility-spec copy)
                      :to-equal '((org-fold-outline . " ↴")))
              (expect (with-current-buffer copy
                        (seq-find (lambda (overlay)
                                    (eq 'org-fold-outline (overlay-get overlay 'invisible)))
                                  (overlays-in (point-min) (point-max))))
                      :to-be-truthy))
          (kill-buffer copy)))))

  (it "leaves an ordinary buffer as it is"
    (with-temp-buffer
      (insert "(defun foo () 42)\n")
      (let ((copy (buffer-copy-as-displayed (current-buffer))))
        (unwind-protect
            (expect (with-current-buffer copy
                      (buffer-substring-no-properties (point-min) (point-max)))
                    :to-equal "(defun foo () 42)\n")
          (kill-buffer copy))))))

(describe "exported-pdf-path"
  (it "names a file buffer after its file, as the package does"
    (let ((buffer (generate-new-buffer "unrelated-name")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (setq buffer-file-name "/tmp/notes.org"))
            (expect (exported-pdf-path buffer)
                    :to-equal (expand-file-name "notes.pdf" buffer-to-pdf-directory)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))))

  (it "falls back to the buffer name"
    (let ((buffer (generate-new-buffer "*probe*")))
      (unwind-protect
          (expect (exported-pdf-path buffer)
                  :to-equal (expand-file-name "*probe*.pdf" buffer-to-pdf-directory))
        (kill-buffer buffer)))))

(describe "show-pdf"
  (it "refreshes a window still showing the previous render"
    ;; Re-exporting overwrites the same file and nothing auto-reverts here, so
    ;; without this the second export would look like it did nothing.
    (let ((stale (generate-new-buffer "old.pdf"))
          reverted opened)
      (unwind-protect
          (cl-letf (((symbol-function 'get-file-buffer) (lambda (_) stale))
                    ((symbol-function 'revert-buffer)
                     (lambda (&rest _) (setq reverted (current-buffer))))
                    ((symbol-function 'find-file-other-window)
                     (lambda (path) (setq opened path))))
            (show-pdf "/tmp/old.pdf")
            (expect reverted :to-be stale)
            (expect opened :to-equal "/tmp/old.pdf"))
        (kill-buffer stale))))

  (it "just opens a PDF nothing is visiting"
    (let (opened)
      (cl-letf (((symbol-function 'get-file-buffer) #'ignore)
                ((symbol-function 'revert-buffer)
                 (lambda (&rest _) (error "Nothing to revert")))
                ((symbol-function 'find-file-other-window)
                 (lambda (path) (setq opened path))))
        (show-pdf "/tmp/fresh.pdf")
        (expect opened :to-equal "/tmp/fresh.pdf"))))

  (it "keeps saved places out of a regenerated file"
    ;; The regression: an export's page count changes with every run, and
    ;; saveplace-pdf-view restoring a page past the new end aborts the visit
    ;; outright - "No such page: 5".
    (let ((save-place-alist '(("/tmp/fresh.pdf" . 5)))
          during)
      (cl-letf (((symbol-function 'get-file-buffer) #'ignore)
                ((symbol-function 'find-file-other-window)
                 (lambda (_) (setq during save-place-alist))))
        (show-pdf "/tmp/fresh.pdf")
        (expect during :to-be nil)
        (expect save-place-alist :to-equal '(("/tmp/fresh.pdf" . 5)))))))

(describe "chromium-executable"
  (it "prefers an installed application bundle"
    (cl-letf (((symbol-function 'file-executable-p)
               (lambda (path) (string-match-p "Brave" path))))
      (expect (chromium-executable) :to-match "Brave")))

  (it "falls back to exec-path"
    (cl-letf (((symbol-function 'file-executable-p) #'ignore)
              ((symbol-function 'executable-find)
               (lambda (name) (when (equal name "chromium") "/usr/bin/chromium"))))
      (expect (chromium-executable) :to-equal "/usr/bin/chromium"))))

(describe "print-buffer-with-browser"
  (let (invocation)
    (before-each
      (setq invocation nil)
      (spy-on 'chromium-executable :and-return-value "/opt/chrome")
      (spy-on 'buffer-html-for-print :and-return-value "<html></html>"))

    (it "prints the buffer into buffer-to-pdf-directory"
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest args) (setq invocation args) 0)))
        (let ((buffer (generate-new-buffer "probe")))
          (unwind-protect
              (expect (print-buffer-with-browser buffer 'landscape)
                      :to-equal (expand-file-name "probe.pdf" buffer-to-pdf-directory))
            (kill-buffer buffer))))
      (expect (car invocation) :to-equal "/opt/chrome")
      (expect (member (concat "--print-to-pdf="
                              (expand-file-name "probe.pdf" buffer-to-pdf-directory))
                      invocation)
              :to-be-truthy)
      ;; The rendered page is scratch: it must not outlive the run.
      (expect (file-exists-p (car (last invocation))) :to-be nil))

    (it "reports what the browser said when it fails"
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _) (insert "cannot create target") 1)))
        (expect (print-buffer-with-browser (current-buffer) 'landscape)
                :to-throw 'error)))))

;;; buffer-to-pdf-tests.el ends here
