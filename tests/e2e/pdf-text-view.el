;;; tests/e2e/pdf-text-view.el --- pdf-text reading-view flow -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The buttercup suite proves the pure transforms; what only shows up
;; here is the wiring: mode activation over a real displayed buffer,
;; q through pdf-text-mode-map, RET through the buffer-local evil
;; binding that outranks evil-org's, and org folding driven by TAB and
;; C-c / with the full config's org hooks live.
;;
;; Two legs.  The companion leg runs in any frame: the buffer is built
;; exactly the way pdf-view-as-text builds it (mode, then insert-pages),
;; only the epdfinfo source is canned.  The pdf-view leg needs a
;; window-system frame - pdf-view-mode refuses tty frames outright - and
;; a built epdfinfo, so under the -nw harness (and CI) it reports a loud
;; skip; in a GUI session `pdf-text-view-pdf-flow-cases' runs the full
;; round trip against generated fixture PDFs: themed-mode blending on
;; open, localleader x / RET / q, companion reuse and mtime staleness,
;; pdf-text-sync-mode in both directions, and the no-text-layer refusal
;; on a scan-like fixture.

(require 'cl-lib)

;; The package autoloads only pdf-view-as-text; the companion leg calls the
;; transforms directly, so pull the whole package in.
(require 'pdf-text)

(defun pdf-text-view--content-stream (lines)
  "One PDF text-drawing stream painting LINES top-down."
  (concat "BT /F1 12 Tf 72 720 Td "
          (mapconcat (lambda (l) (format "(%s) Tj" l)) lines " 0 -16 Td ")
          " ET"))

(defun pdf-text-view--make-pdf (path pages)
  "Write a minimal PDF at PATH; PAGES is a list of line lists.
Hand-built so the fixture needs no tracked binary and no external
generator; xref offsets are recorded as the objects are laid out.
ASCII only, so string length equals byte offset."
  (let* ((n (length pages))
         (font (+ 3 (* 2 n)))
         (objs
          (append
           (list (cons 1 "<< /Type /Catalog /Pages 2 0 R >>")
                 (cons 2 (format "<< /Type /Pages /Count %d /Kids [%s] >>"
                                 n (mapconcat (lambda (i) (format "%d 0 R" (+ 3 i)))
                                              (number-sequence 0 (1- n)) " "))))
           (cl-loop for i from 0 below n
                    collect
                    (cons (+ 3 i)
                          (format (concat "<< /Type /Page /Parent 2 0 R"
                                          " /MediaBox [0 0 612 792]"
                                          " /Resources << /Font << /F1 %d 0 R >> >>"
                                          " /Contents %d 0 R >>")
                                  font (+ 3 n i))))
           (cl-loop for i from 0 below n
                    for stream = (pdf-text-view--content-stream (nth i pages))
                    collect (cons (+ 3 n i)
                                  (format "<< /Length %d >>\nstream\n%s\nendstream"
                                          (length stream) stream)))
           (list (cons font "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"))))
         (doc "%PDF-1.4\n")
         offsets)
    (dolist (o objs)
      (push (cons (car o) (length doc)) offsets)
      (setq doc (concat doc (format "%d 0 obj\n%s\nendobj\n" (car o) (cdr o)))))
    (let ((xref-pos (length doc))
          (size (1+ font)))
      (setq doc (concat doc
                        (format "xref\n0 %d\n0000000000 65535 f \n" size)
                        (mapconcat (lambda (i)
                                     (format "%010d 00000 n \n" (cdr (assq i offsets))))
                                   (number-sequence 1 font) "")
                        (format "trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n"
                                size xref-pos)))
      (let ((coding-system-for-write 'binary))
        (write-region doc nil path nil 'silent)))))

(defvar pdf-text-view--fixture-pages
  '(("Alpha page one" "modern informa-" "tion retrieval")
    ("Bravo page two marker")
    ("Charlie page three"))
  "Per-page line lists; the wrapped hyphenated word exercises unfill.")

(defvar pdf-text-view--fixture-outline
  '(((depth . 1) (type . goto-dest) (title . "Alpha") (page . 1))
    ((depth . 1) (type . goto-dest) (title . "Bravo") (page . 2))
    ((depth . 2) (type . goto-dest) (title . "Bravo detail") (page . 2))
    ((depth . 1) (type . goto-dest) (title . "Charlie") (page . 3)))
  "Canned `pdf-info-outline' shape; the generated fixture PDF itself
carries no outline, so the GUI leg doubles as the degrade-to-flat
path while the companion leg exercises headings and folding.")

(defun pdf-text-view--case (label ok got want &optional err)
  (list :label label :ok (and ok (null err)) :got got :want want :err err))

(defun pdf-text-view--companion-buffer ()
  "Companion buffer constructed the way `pdf-view-as-text' constructs it.
Pages come from the same fixture lines gettext would yield, through the
real render pipeline with the canned outline interleaved and the same
overview fold; the source PDF buffer is dead on purpose, so a
dispatched sync-back has to answer with its own user-error."
  (let ((buf (get-buffer-create "*pdf-text: e2e-companion*"))
        (pages (pdf-text--interleave-outline
                (pdf-text-render-pages
                 (mapcar (lambda (lines) (string-join lines "\n"))
                         pdf-text-view--fixture-pages))
                pdf-text-view--fixture-outline)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pdf-text-mode)
        (pdf-text--insert-pages pages)
        (setq pdf-text--pdf-buffer
              (let ((b (generate-new-buffer " e2e-dead-pdf")))
                (kill-buffer b)
                b))
        (org-cycle-overview)))
    buf))

(defun pdf-text-view-companion-cases ()
  "Drive pdf-text-mode-map with real keys; runs in any frame."
  (let (results buf)
    (unwind-protect
        (progn
          (setq buf (pdf-text-view--companion-buffer))
          (switch-to-buffer buf)
          (delete-other-windows)
          (push (pdf-text-view--case
                 "companion state: visual-line, read-only, 2 feeds, jinx off, unfill joined"
                 (and (buffer-local-value 'visual-line-mode buf)
                      (buffer-local-value 'buffer-read-only buf)
                      (eql 2 (how-many "\f" (point-min) (point-max)))
                      (not (buffer-local-value 'jinx-mode buf))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "modern information retrieval" nil t)))
                 (format "vl=%s ro=%s feeds=%s jinx=%s text=%S"
                         (buffer-local-value 'visual-line-mode buf)
                         (buffer-local-value 'buffer-read-only buf)
                         (how-many "\f" (point-min) (point-max))
                         (buffer-local-value 'jinx-mode buf)
                         (buffer-substring-no-properties
                          (point-min) (min 45 (point-max))))
                 "vl=t ro=t feeds=2 jinx=nil, page 1 starts with the joined paragraph")
                results)
          (push (pdf-text-view--case
                 "page map round-trips at every page start"
                 (cl-every (lambda (page)
                             (goto-char (pdf-text--page-start page))
                             (eql page (pdf-text-page-at-point)))
                           '(1 2 3))
                 "walked pages 1-3" "page-at-point equals the page jumped to")
                results)
          (let ((marker-pos (save-excursion
                              (goto-char (point-min))
                              (search-forward "Bravo page two marker")
                              (1- (point)))))
            (push (pdf-text-view--case
                   "overview fold hides chapter bodies"
                   (invisible-p marker-pos)
                   (format "invisible=%s" (invisible-p marker-pos))
                   "body under * Bravo invisible after org-cycle-overview")
                  results)
            (goto-char (pdf-text--page-start 2)) ; the * Bravo heading line
            (let (err
                  (detail-pos (save-excursion
                                (goto-char (point-min))
                                (search-forward "Bravo detail")
                                (1- (point)))))
              ;; the config's TAB is the local-fold toggle
              ;; (org-cycle-only-current-subtree-h), not org's 3-state
              ;; cycle: it peeks children open, never the whole subtree.
              ;; A GUI Tab press sends the <tab> function key (the C-i
              ;; char would hit evil's motion-state jump instead); a tty
              ;; Tab is the C-i char itself.
              (condition-case e (execute-kbd-macro
                                 (if (display-graphic-p) (kbd "<tab>") (kbd "TAB")))
                (error (setq err e)))
              (push (pdf-text-view--case
                     "TAB peeks the heading open to its children"
                     (and (not (invisible-p detail-pos))
                          (invisible-p marker-pos))
                     (format "child-invisible=%s body-invisible=%s"
                             (invisible-p detail-pos) (invisible-p marker-pos))
                     "child heading visible, body under it still folded"
                     err)
                    results))
            (let (err)
              (condition-case e (execute-kbd-macro (kbd "zO"))
                (error (setq err e)))
              (push (pdf-text-view--case
                     "zO opens the full subtree down to the body"
                     (not (invisible-p marker-pos))
                     (format "invisible=%s" (invisible-p marker-pos))
                     "body visible after outline-show-subtree"
                     err)
                    results))
            (let (err
                  (alpha-pos (save-excursion
                               (goto-char (point-min))
                               (search-forward "Alpha page one")
                               (1- (point)))))
              (condition-case e
                  (execute-kbd-macro
                   (vconcat (kbd "C-c / /") "marker" (kbd "RET")))
                (error (setq err e)))
              (push (pdf-text-view--case
                     "sparse tree folds the buffer down to the match"
                     (and (not (invisible-p marker-pos))
                          (invisible-p alpha-pos))
                     (format "marker-invisible=%s alpha-invisible=%s"
                             (invisible-p marker-pos) (invisible-p alpha-pos))
                     "match visible, non-matching chapter body folded"
                     err)
                    results)))
          (goto-char (pdf-text--page-start 3))
          (let (err)
            (condition-case e (execute-kbd-macro (kbd "RET"))
              (error (setq err e)))
            (push (pdf-text-view--case
                   "RET dispatches pdf-text-show-in-pdf through the mode map"
                   (and err (string-match-p "source PDF buffer is gone"
                                            (error-message-string err)))
                   (format "err=%s" (and err (error-message-string err)))
                   "the command's own user-error: The source PDF buffer is gone")
                  results))
          (let (err)
            ;; GUI frames send the function key, and evil-org's direct
            ;; <return> binding preempts the translation to RET - the
            ;; buffer-local <return> binding must win on its own.
            (condition-case e (execute-kbd-macro (kbd "<return>"))
              (error (setq err e)))
            (push (pdf-text-view--case
                   "<return> dispatches the sync-back through the local binding"
                   (and err (string-match-p "source PDF buffer is gone"
                                            (error-message-string err)))
                   (format "err=%s" (and err (error-message-string err)))
                   "the command's own user-error through the direct <return> binding")
                  results))
          (let (err)
            (condition-case e (execute-kbd-macro (kbd "q"))
              (error (setq err e)))
            (push (pdf-text-view--case
                   "q buries the companion buffer"
                   (and (not (eq (current-buffer) buf)) (buffer-live-p buf))
                   (format "current=%s companion-live=%s"
                           (buffer-name) (buffer-live-p buf))
                   "current != companion, companion still live"
                   err)
                  results)))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (nreverse results)))

(defun pdf-text-view-pdf-flow-cases ()
  "Full pdf-view round trip over a generated fixture PDF.
Needs a window-system frame and a built epdfinfo; elsewhere returns a
single loud skip entry."
  (if (not (display-graphic-p))
      (list (pdf-text-view--case
             "pdf-view flow SKIPPED: tty frame (pdf-view-mode requires a window system)"
             t "skip" "skip"))
    (let* ((borrowed (and (boundp 'e2e-work-dir) e2e-work-dir))
           (dir (or borrowed (file-name-as-directory
                              (make-temp-file "pdf-text-e2e" t))))
           (fixture (expand-file-name "pdf-text-fixture.pdf" dir))
           results pdf-buf text-buf)
      (pdf-text-view--make-pdf fixture pdf-text-view--fixture-pages)
      (unwind-protect
          (progn
            (setq pdf-buf (find-file-noselect fixture))
            (switch-to-buffer pdf-buf)
            (delete-other-windows)
            (if (not (with-current-buffer pdf-buf (derived-mode-p 'pdf-view-mode)))
                (push (pdf-text-view--case
                       (format "pdf-view flow SKIPPED: no epdfinfo (PDF opened in %s)"
                               (buffer-local-value 'major-mode pdf-buf))
                       t "skip" "skip")
                      results)
              (let* ((themed (buffer-local-value 'pdf-view-themed-minor-mode pdf-buf))
                     (opts (with-current-buffer pdf-buf (pdf-info-getoptions)))
                     (fg (downcase (or (plist-get opts :render/foreground) "")))
                     (want-fg (downcase (pdf-util-hexcolor
                                         (face-foreground 'default nil)))))
                (push (pdf-text-view--case
                       "opening a PDF blends it into the current theme"
                       (and themed
                            (eql 1 (plist-get opts :render/usecolors))
                            (equal fg want-fg))
                       (format "themed=%s usecolors=%s fg=%s"
                               themed (plist-get opts :render/usecolors) fg)
                       (format "themed=t usecolors=1 fg=%s" want-fg))
                      results))
              (pdf-view-goto-page 2)
              (let (err)
                ;; the config rebinds doom-localleader-key to ","
                (condition-case e
                    (execute-kbd-macro (kbd (concat doom-localleader-key " x")))
                  (error (setq err e)))
                (setq text-buf (current-buffer))
                (push (pdf-text-view--case
                       "localleader x opens pdf-text on the viewed page"
                       (and (eq major-mode 'pdf-text-mode)
                            (eql 2 (ignore-errors (pdf-text-page-at-point))))
                       (format "buffer=%s mode=%s page=%s"
                               (buffer-name) major-mode
                               (ignore-errors (pdf-text-page-at-point)))
                       "mode=pdf-text-mode page=2"
                       err)
                      results))
              (when (eq (buffer-local-value 'major-mode text-buf) 'pdf-text-mode)
                (with-current-buffer text-buf
                  (push (pdf-text-view--case
                         "epdfinfo text reflowed: wrap-hyphenated word joined"
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "modern information retrieval" nil t))
                         (save-excursion
                           (goto-char (point-min))
                           (buffer-substring-no-properties
                            (point) (min 60 (point-max))))
                         "text containing \"modern information retrieval\"")
                        results)
                  (goto-char (pdf-text--page-start 3)))
                (let (err)
                  (condition-case e (execute-kbd-macro (kbd "RET"))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "RET jumps pdf-view to the page at point"
                         (and (eq (current-buffer) pdf-buf)
                              (eql 3 (ignore-errors (pdf-view-current-page))))
                         (format "buffer=%s page=%s" (buffer-name)
                                 (and (derived-mode-p 'pdf-view-mode)
                                      (ignore-errors (pdf-view-current-page))))
                         (format "buffer=%s page=3" (buffer-name pdf-buf))
                         err)
                        results))
                ;; the RET jump left pdf-buf selected on page 3: the next
                ;; invocation must reuse the companion without re-extracting
                ;; (chars tick untouched) and land on the now-viewed page
                (let (err (tick (buffer-chars-modified-tick text-buf)))
                  (condition-case e
                      (execute-kbd-macro (kbd (concat doom-localleader-key " x")))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "re-invocation reuses the companion at the viewed page"
                         (and (eq (current-buffer) text-buf)
                              (eql tick (buffer-chars-modified-tick text-buf))
                              (eql 3 (ignore-errors (pdf-text-page-at-point))))
                         (format "buffer=%s tick=%s->%s page=%s"
                                 (buffer-name) tick
                                 (buffer-chars-modified-tick text-buf)
                                 (ignore-errors (pdf-text-page-at-point)))
                         "same buffer, unchanged tick, page 3"
                         err)
                        results))
                (switch-to-buffer pdf-buf)
                (let (err (tick (buffer-chars-modified-tick text-buf)))
                  (set-file-times fixture (encode-time '(0 0 0 1 1 2020)))
                  (condition-case e
                      (execute-kbd-macro (kbd (concat doom-localleader-key " x")))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "a changed file mtime forces re-extraction"
                         (and (eq (current-buffer) text-buf)
                              (not (eql tick (buffer-chars-modified-tick text-buf)))
                              (eql 3 (ignore-errors (pdf-text-page-at-point))))
                         (format "buffer=%s tick=%s->%s page=%s"
                                 (buffer-name) tick
                                 (buffer-chars-modified-tick text-buf)
                                 (ignore-errors (pdf-text-page-at-point)))
                         "same buffer, advanced tick, page 3"
                         err)
                        results))
                ;; sync mode, both directions over the live pair; the
                ;; staleness case just re-rendered, and a fresh render
                ;; starts the sync on its own (pdf-text-sync-default),
                ;; so no keypress precedes the first assertion
                (push (pdf-text-view--case
                       "a fresh render starts pdf-text-sync-mode by itself"
                       (buffer-local-value 'pdf-text-sync-mode text-buf)
                       (format "sync=%s"
                               (buffer-local-value 'pdf-text-sync-mode text-buf))
                       "pdf-text-sync-mode on in the companion"
                       nil)
                      results)
                (let (err)
                  (condition-case e (execute-kbd-macro (kbd "gg"))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "sync: point on page 1 flips the pdf window there"
                         (eql 1 (with-current-buffer pdf-buf
                                  (ignore-errors (pdf-view-current-page))))
                         (format "pdf-page=%s"
                                 (with-current-buffer pdf-buf
                                   (ignore-errors (pdf-view-current-page))))
                         "pdf-view on page 1"
                         err)
                        results))
                (let (err)
                  (condition-case e
                      (with-selected-window (get-buffer-window pdf-buf t)
                        (pdf-view-goto-page 2))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "sync: flipping the pdf page moves point in the companion"
                         (eql 2 (with-current-buffer text-buf
                                  (pdf-text-page-at-point)))
                         (format "text-page=%s"
                                 (with-current-buffer text-buf
                                   (pdf-text-page-at-point)))
                         "companion point on page 2"
                         err)
                        results))
                (let (err)
                  (condition-case e
                      (execute-kbd-macro (kbd (concat doom-localleader-key " s")))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "localleader s disables the sync again"
                         (not (buffer-local-value 'pdf-text-sync-mode text-buf))
                         (format "sync=%s"
                                 (buffer-local-value 'pdf-text-sync-mode text-buf))
                         "pdf-text-sync-mode off"
                         err)
                        results))
                (switch-to-buffer text-buf)
                (let (err)
                  (condition-case e (execute-kbd-macro (kbd "q"))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "q buries the text buffer"
                         (and (not (eq (current-buffer) text-buf))
                              (buffer-live-p text-buf))
                         (format "current=%s text-live=%s"
                                 (buffer-name) (buffer-live-p text-buf))
                         "current != text buffer, text buffer still live"
                         err)
                        results))
                ;; a text-less (scanned) fixture must refuse, leaving nothing
                (let ((scanfix (expand-file-name "pdf-text-scan.pdf" dir))
                      scan-buf err)
                  (pdf-text-view--make-pdf scanfix '(() () ()))
                  (setq scan-buf (find-file-noselect scanfix))
                  (switch-to-buffer scan-buf)
                  (condition-case e
                      (execute-kbd-macro (kbd (concat doom-localleader-key " x")))
                    (error (setq err e)))
                  (push (pdf-text-view--case
                         "a scan refuses with a no-text-layer error, no companion"
                         (and err
                              (string-match-p "no text layer"
                                              (error-message-string err))
                              (not (get-buffer "*pdf-text: pdf-text-scan.pdf*"))
                              (eq (current-buffer) scan-buf))
                         (format "err=%s companion=%s current=%s"
                                 (and err (error-message-string err))
                                 (get-buffer "*pdf-text: pdf-text-scan.pdf*")
                                 (buffer-name))
                         "user-error mentioning no text layer, no companion buffer")
                        results)
                  (when (buffer-live-p scan-buf)
                    (let ((kill-buffer-query-functions nil))
                      (with-current-buffer scan-buf (set-buffer-modified-p nil))
                      (kill-buffer scan-buf)))))))
        (dolist (b (list text-buf pdf-buf))
          (when (buffer-live-p b)
            (let ((kill-buffer-query-functions nil))
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
        (unless borrowed (delete-directory dir t)))
      (nreverse results))))

(defun pdf-text-view-e2e ()
  "Companion-buffer leg everywhere; pdf-view leg where a GUI frame allows."
  (append (pdf-text-view-companion-cases)
          (pdf-text-view-pdf-flow-cases)))

(add-to-list 'e2e-scenarios #'pdf-text-view-e2e)
