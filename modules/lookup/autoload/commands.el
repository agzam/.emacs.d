;;; modules/lookup/autoload/commands.el -*- lexical-binding: t; -*-

;; Lab-native thin wrappers for the SPC s lookup keys that Doom implemented
;; over its +docsets / +dictionary machinery.  Each renders inside Emacs: the
;; docset entry via eww (`dash-docs-browser-func'), the dictionary/thesaurus
;; in their own buffers.  The wrapped commands live in the completion and
;; writing modules and load lazily on first use.

;;;###autoload
(defun lookup-in-all-docsets ()
  "Search documentation across every installed Dash docset.
Widens `consult-dash' past the buffer's active docsets; the chosen entry
opens in eww, never a system browser."
  (interactive)
  (require 'dash-docs)
  (let ((dash-docs-common-docsets (dash-docs-installed-docsets)))
    (consult-dash (thing-at-point 'symbol t))))

;;;###autoload
(defun lookup-dictionary-definition ()
  "Define the word at point with sdcv (offline StarDict), shown in Emacs."
  (interactive)
  (sdcv-search-at-point))

;;;###autoload
(defun lookup-synonyms ()
  "Show a thesaurus entry for the word at point, in an Emacs buffer."
  (interactive)
  (mw-thesaurus-lookup-dwim))
