;;; modules/writing/autoload/lookup.el --- prose K: define the word -*- lexical-binding: t; -*-
;;; Commentary:
;; K in org/markdown looks up the word at point rather than code docs.  sdcv
;; (offline StarDict) renders in an Emacs buffer; when the sdcv binary is
;; missing it defers to the in-Emacs Wiktionary reader.  Neither leaves Emacs.
;;; Code:

;;;###autoload
(defun prose-lookup-documentation (&optional _identifier)
  "Define the word at point for prose, inside Emacs.
Uses sdcv (offline StarDict) when its binary is present, else
`wiktionary-bro-dwim'.  A `lookup-documentation' handler; returns
\\='deferred since the backend owns its own window."
  (if (executable-find "sdcv")
      (sdcv-search-at-point)
    (wiktionary-bro-dwim))
  'deferred)

;;; lookup.el ends here
