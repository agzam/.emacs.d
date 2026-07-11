;;; modules/tree-sitter/autoload/fold.el -*- lexical-binding: t; -*-

;;;###autoload
(defun fold-all-with-treesitter-context-a (orig-fn lst action)
  "Route evil's fold-all/open-all through treesitter-context folds.
Advises `evil-fold-action': its alist carries no close-all/open-all
entries for these modes, so walk the defuns and fold each one."
  (if (and (member major-mode treesitter-context--fold-supported-mode)
           treesitter-context-fold-mode
           (member action '(:close-all :open-all)))
      (save-excursion
        (goto-char (point-max))
        (while (treesit-beginning-of-defun 1)
          (pcase action
            (:close-all (treesitter-context-fold-hide))
            (:open-all  (treesitter-context-fold-show)))))
    (funcall orig-fn lst action)))
