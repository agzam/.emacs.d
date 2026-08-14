;;; modules/git/autoload/smerge.el -*- lexical-binding: t; -*-

;; Single-key smerge driving.  Bare smerge keys hide behind C-c ^, and
;; the evil-collection bindings (]] [[ gu gl gr) are two-keystroke; a
;; transient gives visible modal review: panel up = single keys active,
;; q = back to normal editing.  Non-suffix keys pass through, so
;; motions work while the panel is up.

(require 'transient)

;;;###autoload
(transient-define-prefix smerge-transient ()
  "Drive smerge conflict resolution with single keys."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-stay
  ;; title-less two-row columns keep the panel three lines tall
  [:description
   (lambda ()
     (with-current-buffer (or transient--original-buffer (current-buffer))
       (format "smerge: %d conflict(s) left"
               (save-excursion
                 (goto-char (point-min))
                 (count-matches "^<<<<<<< " (point-min) (point-max))))))
   [("n" "next" smerge-next)
    ("p" "prev" smerge-prev)]
   [("u" "original (upper)" smerge-keep-upper)
    ("l" "rewrite (lower)" smerge-keep-lower)]
   [("RET" "side at point" smerge-keep-current)
    ("a" "all sides" smerge-keep-all)]
   [("r" "auto-resolve" smerge-resolve)
    ("R" "refine words" smerge-refine)]
   [("E" "ediff" smerge-ediff :transient nil)
    ("q" "quit" transient-quit-one)]])
