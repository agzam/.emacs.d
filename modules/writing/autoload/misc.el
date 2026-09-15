;;; modules/writing/autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-bracket-pair ()
  "Insert a `[]' pair for link scaffolding via smartparens.
Simulate a `[' keypress so smartparens auto-closes the pair and
leaves point inside, without clobbering a space already before point."
  (interactive)
  (self-insert-command 1 ?\[))
