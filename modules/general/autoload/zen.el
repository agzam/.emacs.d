;;; modules/general/autoload/zen.el --- writeroom (zen) helpers -*- lexical-binding: t; -*-

(defvar zen-text-scale 2.0
  "Text-scale level while writeroom is active; 0 disables scaling.")

(defvar zen-mixed-pitch-modes '(markdown-mode org-mode)
  "Major modes that get variable-pitch prose inside writeroom.")

;;;###autoload
(defun zen-text-scale-h (arg)
  "Scale text by `zen-text-scale' inside writeroom; refit the column.
ARG is writeroom's local-effects argument: 1 on enable, -1 on disable."
  (when (/= zen-text-scale 0)
    (text-scale-set (if (= arg 1) zen-text-scale 0))
    (visual-fill-column-adjust)))

;;;###autoload
(defun zen-mixed-pitch-h (arg)
  "Toggle variable-pitch prose inside writeroom per ARG.
Only in `zen-mixed-pitch-modes'; code buffers stay monospace."
  (when (apply #'derived-mode-p zen-mixed-pitch-modes)
    (mixed-pitch-mode arg)))

;;;###autoload
(defun turn-off-writeroom-before-split-a (&rest _)
  "Disable writeroom before a window split; it hangs Emacs mid-split."
  (when (bound-and-true-p writeroom-mode)
    (writeroom-mode -1)))
