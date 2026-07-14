;;; modules/completion/autoload/completion-preview.el -*- lexical-binding: t; -*-

;; Echo-area pager for the built-in completion-preview-mode: shows the
;; current page of candidates in the echo area, supports M+number insertion.
;; Wiring (hooks/advice/keys) lives in ../config.el.

(defvar completion-preview-echo-max 5
  "Maximum number of completion-preview candidates shown per echo-area page.")

(defvar completion-preview--echo-shown nil
  "Non-nil while the echo-area candidate list is on screen.")

(defface completion-preview-echo-number '((t :foreground "orange"))
  "Face for the index number shown before each echo-list candidate."
  :group 'completion-preview)

(defvar completion-preview-echo-height 0.75
  "Outer height multiplier applied to the whole echo-list string, numbers included.
The jank control: shrinking the entire line (superscript index glyphs and all)
keeps the tall numbers within the echo-area line so the mode line does not jump.
Composes multiplicatively with `completion-preview-echo-number-height'.")

(defvar completion-preview-echo-number-height 1.2
  "Height of the superscript index numbers relative to the candidate text.
A ratio: values above 1.0 make the numbers larger than the text.  It is then
scaled down together with everything else by `completion-preview-echo-height',
so the on-screen number height is the product of the two.")

(defconst completion-preview--superscripts ["⁰" "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹"]
  "Superscript glyphs for digits 0-9.")

;;;###autoload
(defun completion-preview-accept-or-slurp ()
  "Accept the completion preview if one is shown, else `sp-forward-slurp-sexp'.
Lets M-l double as an accept key without losing its slurp binding."
  (interactive)
  (if (bound-and-true-p completion-preview-active-mode)
      (completion-preview-insert)
    (call-interactively #'sp-forward-slurp-sexp)))

(defun completion-preview--superscript (n)
  "Return the natural number N rendered with superscript digits."
  (mapconcat (lambda (c) (aref completion-preview--superscripts (- c ?0)))
             (number-to-string n) ""))

(defun completion-preview--echo-string ()
  "Return the echo string for the current preview page, or nil when inactive.
Shows the page of up to `completion-preview-echo-max' candidates containing
the current one (highlighted), each prefixed with its 1-based on-page index.
The numbers carry an inner `completion-preview-echo-number-height' ratio and
the whole string is then wrapped in `completion-preview-echo-height'; the two
float heights compose multiplicatively."
  (when (bound-and-true-p completion-preview--overlay)
    (let* ((ov completion-preview--overlay)
           (common (or (overlay-get ov 'completion-preview-common) ""))
           (sufs (overlay-get ov 'completion-preview-suffixes))
           (idx (or (overlay-get ov 'completion-preview-index) 0))
           (total (length sufs))
           (size completion-preview-echo-max)
           (start (* (/ idx size) size))
           (end (min total (+ start size)))
           (cands (cl-loop for i from start below end
                           for num = (propertize
                                      (completion-preview--superscript (1+ (- i start)))
                                      'face `((:height ,completion-preview-echo-number-height)
                                              completion-preview-echo-number))
                           for cand = (substring-no-properties
                                       (concat common (nth i sufs)))
                           collect (concat num (if (= i idx)
                                                   (propertize cand 'face 'highlight)
                                                 cand))))
           (str (concat (and (< 0 start) "← ")
                        (mapconcat #'identity cands "  ")
                        (and (< end total) " →"))))
      (add-face-text-property 0 (length str)
                              `(:height ,completion-preview-echo-height) nil str)
      str)))

;;;###autoload
(defun completion-preview-echo-candidates (&rest _)
  "Echo the current page of completion-preview candidates."
  (if-let* ((str (completion-preview--echo-string)))
      (progn
        (setq completion-preview--echo-shown t)
        (let ((message-log-max nil)) (message "%s" str)))
    (completion-preview-echo-clear)))

;;;###autoload
(defun completion-preview-echo-clear (&rest _)
  "Clear the echoed candidate list once the preview is gone."
  (when (and completion-preview--echo-shown
             (not (bound-and-true-p completion-preview--overlay)))
    (setq completion-preview--echo-shown nil)
    (let ((message-log-max nil)) (message nil))))

;;;###autoload
(defun completion-preview-insert-indexed (n)
  "Complete with the Nth (1-based) candidate of the visible echo page.
Company-style M+number insertion: one press inserts."
  (when (bound-and-true-p completion-preview--overlay)
    (let* ((ov completion-preview--overlay)
           (base (or (overlay-get ov 'completion-preview-base) ""))
           (beg (overlay-get ov 'completion-preview-beg))
           (end (overlay-get ov 'completion-preview-end))
           (sufs (overlay-get ov 'completion-preview-suffixes))
           (common (or (overlay-get ov 'completion-preview-common) ""))
           (idx (or (overlay-get ov 'completion-preview-index) 0))
           (efn (plist-get (overlay-get ov 'completion-preview-props) :exit-function))
           (size completion-preview-echo-max)
           (target (+ (* (/ idx size) size) (1- n))))
      (when (< target (length sufs))
        (let* ((cand (concat common (nth target sufs)))
               (skip (- end beg))
               (visible (if (<= 0 skip (length cand)) (substring cand skip) "")))
          (completion-preview-active-mode -1)
          (goto-char end)
          (insert-and-inherit visible)
          (when (functionp efn)
            (funcall efn (concat base cand) 'finished)))))))

;;;###autoload
(defun completion-preview-next-candidate-guard-a (orig &rest args)
  "Hide the preview instead of throwing if cycling hits a stale overlay.
Overlay positions go stale in buffers rewritten under it (eca-chat streaming)."
  (condition-case nil
      (apply orig args)
    (args-out-of-range
     (when (bound-and-true-p completion-preview-active-mode)
       (completion-preview-active-mode -1)))))
