;;; modules/dired/autoload/listing.el --- listing layout fixups -*- lexical-binding: t; -*-
;;; Commentary:
;; Rearrangements applied to a listing once it has been read, both to
;; dired's own buffer and to dired-subtree's inline sub-listings.
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
(defun dired-subtree-drop-dot-entries-a (listing)
  "Drop \".\" and \"..\" from a subtree LISTING.
dired-subtree drops them itself, but only when they lead the listing,
which sorting by time stops guaranteeing.  A subtree has no use for
either: \".\" repeats the directory line right above it."
  (with-temp-buffer
    (insert listing)
    (dired-take-entry-line ".")
    (dired-take-entry-line "..")
    (string-trim-right (buffer-string) "\n")))

;;; listing.el ends here
