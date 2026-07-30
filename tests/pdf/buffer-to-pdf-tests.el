;;; tests/pdf/buffer-to-pdf-tests.el --- buffer-to-pdf wiring specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

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

(defvar buffer-to-pdf-directory (file-name-as-directory
                                 (make-temp-file "buffer-to-pdf-tests" t)))

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

(describe "page-size-css"
  (it "reads the pixel dimensions of an orientation"
    (expect (page-size-css 'landscape) :to-equal "1024px 768px"))

  (it "calls orientations that are functions"
    (expect (page-size-css 'current-window) :to-equal "640px 480px"))

  (it "refuses an unknown orientation"
    (expect (page-size-css 'origami) :to-throw 'user-error)))

(describe "print-stylesheet"
  (it "sizes the page and keeps the browser from dropping the theme colors"
    (let ((css (print-stylesheet "1024px 768px")))
      (expect css :to-match "@page { size: 1024px 768px")
      (expect css :to-match "print-color-adjust: exact")))

  (it "flattens the faces for the monochrome commands"
    (let ((buffer-to-pdf-monochrome (cons "white" "black")))
      (expect (print-stylesheet "1024px 768px")
              :to-match "background-color: white !important; color: black"))))

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
        (expect opened :to-equal "/tmp/fresh.pdf")))))

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
