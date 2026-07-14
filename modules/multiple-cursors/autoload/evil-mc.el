;;; modules/multiple-cursors/autoload/evil-mc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mc-toggle-cursors ()
  "Toggle the frozen state of evil-mc cursors."
  (interactive)
  (unless (evil-mc-has-cursors-p)
    (user-error "No cursors exist to be toggled"))
  (setq evil-mc-frozen (not (and (evil-mc-has-cursors-p)
                                 evil-mc-frozen)))
  (message (if evil-mc-frozen "evil-mc paused" "evil-mc resumed")))
