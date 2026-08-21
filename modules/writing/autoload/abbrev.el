;;; modules/writing/autoload/abbrev.el -*- lexical-binding: t; -*-

(defvar abbrev-redundant-apostrophe-position nil
  "Position an expansion ended at, when the apostrophe being typed is its own.
Set while the abbrev expands, read once that apostrophe lands after it.")

;;;###autoload
(defun expand-abbrev-tolerating-late-apostrophe ()
  "Expand the abbrev before point, tolerating an apostrophe typed after it.
A contraction typed at speed lands its apostrophe behind the word
\(\"dont'\"): the typo is already complete when the key arrives.  Where the
apostrophe is punctuation (`markdown-mode', `eca-chat-mode') it terminates
the word and triggers the expansion itself; where it is a word constituent
\(`text-mode', `org-mode') it joins the abbrev name and nothing matches.
Both ways the expansion carries an apostrophe of its own, so the typed one
is dropped: here for the word-constituent case, in
`drop-redundant-apostrophe-h' for the punctuation one."
  (setq abbrev-redundant-apostrophe-position nil)
  (let ((sym (or (abbrev--default-expand)
                 (and (eq (char-before) ?')
                      (let ((expanded (save-restriction
                                        (narrow-to-region (point-min) (1- (point)))
                                        (goto-char (point-max))
                                        (abbrev--default-expand))))
                        ;; point sits before the apostrophe either way: drop it
                        ;; when the expansion brought its own, else step over it
                        (if (and expanded (string-search "'" (symbol-value expanded)))
                            (delete-char 1)
                          (forward-char 1))
                        expanded)))))
    (when (and sym
               (eq last-command-event ?')
               (string-search "'" (symbol-value sym)))
      (setq abbrev-redundant-apostrophe-position (point)))
    sym))

;;;###autoload
(defun drop-redundant-apostrophe-h ()
  "Delete an apostrophe that only triggered the expansion carrying it."
  (when (eql abbrev-redundant-apostrophe-position (1- (point)))
    (delete-char -1))
  (setq abbrev-redundant-apostrophe-position nil))
