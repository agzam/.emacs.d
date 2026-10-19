;;; modules/dired/autoload/listing.el --- listing layout fixups -*- lexical-binding: t; -*-
;;; Commentary:
;; Placement of the dot entries: at the head of Dired's own listing, and
;; nowhere in dired-subtree's inline sub-listings, which this file reads in.
;;; Code:

(defun dired-take-entry-line (name)
  "Cut the listing line naming NAME out of the buffer, or return nil.
Reads the name off the line rather than through `dired-get-filename',
which costs a regexp match and an unquoting pass on every line scanned."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (and (dired-move-to-filename)
                          (equal name (buffer-substring-no-properties
                                       (point) (line-end-position))))))
      (forward-line 1))
    (unless (eobp)
      (delete-and-extract-region (line-beginning-position)
                                 (line-beginning-position 2)))))

;;;###autoload
(defun dired-dot-entries-first-h ()
  "Move the \".\" and \"..\" lines to the head of the listing.
`ls' sorts them like any other directory, so sorting by time drops them
into the middle, where the eye stops looking for them."
  (let ((inhibit-read-only t)
        (top (save-excursion
               (goto-char (point-min))
               (while (and (not (eobp)) (not (dired-move-to-filename)))
                 (forward-line 1))
               (copy-marker (line-beginning-position)))))
    (save-excursion
      ;; both land at TOP, so pulling ".." first leaves "." above it
      (dolist (name '(".." "."))
        (when-let* ((line (dired-take-entry-line name)))
          (goto-char top)
          (insert line))))
    (set-marker top nil)))

;;;###autoload
(defun dired-subtree-readin-a (dir-name)
  "Return the listing of DIR-NAME for `dired-subtree-insert' to splice in.
Overrides `dired-subtree--readin', which drops the dot entries by their
position and so cuts a real entry when sorting by time moves them."
  (with-temp-buffer
    (insert-directory dir-name dired-listing-switches nil t)
    (goto-char (point-min))
    (when (dired-between-files)
      (delete-region (point) (line-beginning-position 2)))
    (dired-take-entry-line ".")
    (dired-take-entry-line "..")
    (while (not (eobp))
      (insert "  ")
      (forward-line 1))
    (string-trim-right (buffer-string) "\n")))

;;; listing.el ends here
