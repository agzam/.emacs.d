;;; modules/pdf/autoload/nov.el -*- lexical-binding: t; -*-
;;; Commentary:
;; nov.el (EPUB) helpers.
;;; Code:

;;;###autoload
(defun nov-back-or-quit ()
  "Go back in nov history, or kill the buffer when there is none."
  (interactive)
  (if nov-history
      (nov-history-back)
    (kill-current-buffer)))

(defun nov--text-scale-adjust (inc)
  "Adjust nov-mode text scale by INC, remapping the shr faces nov uses."
  ;; First, scale the default face (standard behavior).
  (text-scale-increase inc)
  ;; Then remap all the shr-* faces that nov uses.
  (let ((scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
    (dolist (face '(shr-text shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6
                    shr-code shr-strike shr-mark shr-italic shr-bold
                    shr-link shr-preformatted))
      (face-remap-add-relative face :height scale-factor))))

;;;###autoload
(defun nov-text-scale-increase ()
  "Increase nov-mode text scale, remapping shr faces when variable-pitch."
  (interactive)
  (if nov-variable-pitch
      (nov--text-scale-adjust 0.5)
    (text-scale-increase 0.5)))

;;;###autoload
(defun nov-text-scale-decrease ()
  "Decrease nov-mode text scale, remapping shr faces when variable-pitch."
  (interactive)
  (if nov-variable-pitch
      (nov--text-scale-adjust -0.5)
    (text-scale-decrease 0.5)))

;;; nov.el ends here
