;;; tests/completion/completion-preview-tests.el --- pager specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'completion-preview)

(load-module-file "modules/completion/autoload/completion-preview.el")

(defun set-preview-overlay (beg end &rest props)
  "Install a completion-preview-shaped overlay with PROPS in this buffer."
  (let ((ov (make-overlay beg end)))
    (while props
      (overlay-put ov (pop props) (pop props)))
    (setq-local completion-preview--overlay ov)
    ov))

(defun face-heights (face)
  "Return the list of :height values present in FACE, a face spec.
Handles a bare attribute plist, a single face symbol, or a list of faces."
  (let ((faces (if (keywordp (car-safe face)) (list face) (ensure-list face))))
    (delq nil (mapcar (lambda (f) (and (consp f) (plist-get f :height))) faces))))

(describe "completion-preview--superscript"
  (it "renders multi-digit numbers with superscript glyphs"
    (expect (completion-preview--superscript 42) :to-equal "⁴²")
    (expect (completion-preview--superscript 105) :to-equal "¹⁰⁵")))

(describe "completion-preview--echo-string"
  (it "is nil when no preview overlay exists"
    (with-temp-buffer
      (expect (completion-preview--echo-string) :to-be nil)))
  (it "shows the current page, highlighting the active candidate"
    (with-temp-buffer
      (insert "pre")
      (set-preview-overlay
       1 4
       'completion-preview-common "pre"
       'completion-preview-suffixes '("fix" "amble" "sume" "tend" "cision"
                                      "view" "fer")
       'completion-preview-index 1)
      (let* ((str (completion-preview--echo-string))
             (hl-pos (string-match "preamble" str)))
        (expect str :to-match "prefix")
        (expect str :to-match "presume")
        (expect (memq 'highlight
                      (ensure-list (get-text-property hl-pos 'face str)))
                :to-be-truthy)
        ;; 7 candidates, page size 5, on page 1: only a right arrow.
        (expect str :to-match " →")
        (expect (string-match-p "← " str) :to-be nil))))
  (it "wraps the whole line, numbers included, in the outer echo height"
    ;; The outer height must compose onto the numbers too: the inner ratio and
    ;; the outer scale are both float :heights on the number, so shrinking the
    ;; line shrinks the numbers with it (the whole point of the pager's design).
    (with-temp-buffer
      (insert "pre")
      (set-preview-overlay
       1 4
       'completion-preview-common "pre"
       'completion-preview-suffixes '("fix" "sume")
       'completion-preview-index 0)
      (let* ((completion-preview-echo-height 0.5)
             (completion-preview-echo-number-height 2.0)
             (str (completion-preview--echo-string))
             (num-heights (face-heights (get-text-property 0 'face str)))
             (body-pos (string-match "prefix" str))
             (body-heights (face-heights (get-text-property body-pos 'face str))))
        (expect (member completion-preview-echo-height num-heights) :to-be-truthy)
        (expect (member completion-preview-echo-number-height num-heights) :to-be-truthy)
        (expect body-heights :to-equal (list completion-preview-echo-height)))))
  (it "pages past the fold with a left arrow"
    (with-temp-buffer
      (insert "pre")
      (set-preview-overlay
       1 4
       'completion-preview-common "pre"
       'completion-preview-suffixes '("fix" "amble" "sume" "tend" "cision"
                                      "view" "fer")
       'completion-preview-index 5)
      (let ((str (completion-preview--echo-string)))
        (expect str :to-match "preview")
        (expect str :to-match "prefer")
        (expect str :to-match "← ")
        (expect (string-match-p " →" str) :to-be nil)))))

(describe "completion-preview-insert-indexed"
  (it "inserts the visible remainder of the Nth candidate and finishes"
    (with-temp-buffer
      (insert "pre")
      (let (exit-args)
        (set-preview-overlay
         1 4
         'completion-preview-base ""
         'completion-preview-beg 1
         'completion-preview-end 4
         'completion-preview-common "pre"
         'completion-preview-suffixes '("fix" "sume")
         'completion-preview-index 0
         'completion-preview-props
         (list :exit-function (lambda (&rest args) (setq exit-args args))))
        (completion-preview-insert-indexed 2)
        (expect (buffer-string) :to-equal "presume")
        (expect exit-args :to-equal '("presume" finished)))))
  (it "does nothing when the index is off the page"
    (with-temp-buffer
      (insert "pre")
      (set-preview-overlay
       1 4
       'completion-preview-base ""
       'completion-preview-beg 1
       'completion-preview-end 4
       'completion-preview-common "pre"
       'completion-preview-suffixes '("fix")
       'completion-preview-index 0
       'completion-preview-props nil)
      (completion-preview-insert-indexed 4)
      (expect (buffer-string) :to-equal "pre"))))

(describe "completion-preview-accept-or-slurp"
  (it "accepts the preview when one is active"
    (spy-on 'completion-preview-insert)
    (with-temp-buffer
      (setq-local completion-preview-active-mode t)
      (completion-preview-accept-or-slurp))
    (expect 'completion-preview-insert :to-have-been-called))
  (it "falls back to slurp when no preview is active"
    (let (called)
      (cl-letf (((symbol-function 'sp-forward-slurp-sexp)
                 (lambda () (interactive) (setq called t))))
        (with-temp-buffer
          (completion-preview-accept-or-slurp)))
      (expect called :to-be-truthy))))
