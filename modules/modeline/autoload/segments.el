;;; modules/modeline/autoload/segments.el -*- lexical-binding: t; -*-

(defvar modeline-segments
  '((bar window-number buffer-info)
    (major-mode lsp misc-info process pdf-pages matches selection-info
                input-method buffer-position))
  "LHS and RHS segment lists for the `main' doom-modeline layout.
Segments must exist in doom-modeline's registries at apply time -
`doom-modeline--prepare-segments' hard-errors on unknown names.")

;;;###autoload
(defun apply-custom-modeline ()
  "Redefine doom-modeline's `main' layout from `modeline-segments'.
Replacing `main' styles every buffer through the default value;
doom-modeline's own special modelines (pdf, message, ...) still apply
buffer-locally on top."
  (require 'doom-modeline)
  (apply #'doom-modeline-def-modeline 'main modeline-segments))
