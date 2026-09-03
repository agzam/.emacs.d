;;; tests/e2e/dired-subtree-dots.el --- dot entries in dired and its subtrees -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The buttercup suite drives `dired-dot-entries-first-h' and
;; `dired-subtree-drop-dot-entries-a' over canned listing text.  Only a
;; booted Emacs shows the wiring: real `ls' output through the configured
;; switches, dired-subtree building an inline listing, and the treemacs
;; icons advice walking the lines that come back - the last of which used
;; to die on a subtree holding no files at all.

(require 'dired)
(require 'seq)

(defun dired-subtree-dots--names ()
  "Every filename in the current buffer's listing, top to bottom."
  (save-excursion
    (goto-char (point-min))
    (let (names)
      (while (not (eobp))
        (when (dired-move-to-filename)
          (push (dired-get-filename 'no-dir t) names))
        (forward-line 1))
      (nreverse names))))

(defun dired-subtree-dots--raw-names (dir)
  "Names `ls' hands back for DIR, in listing order."
  (with-temp-buffer
    (insert-directory (file-name-as-directory dir) dired-listing-switches nil t)
    (dired-subtree-dots--names)))

(defun dired-subtree-dots--fixture ()
  "Build the tree under `e2e-work-dir' and return its root.
`zdir' is stamped older than the subdirectory it holds, so `ls -t' lists
that subdirectory first - the case where dired-subtree's own dot
stripping, which only fires when \".\" leads the listing, misses."
  (let* ((root (expand-file-name "dired-dots/" e2e-work-dir))
         (zdir (expand-file-name "zdir/" root))
         (now (current-time)))
    (when (file-directory-p root) (delete-directory root t))
    (make-directory (expand-file-name "sub" zdir) t)
    (make-directory (expand-file-name "empty" root) t)
    (with-temp-file (expand-file-name "old.txt" zdir) (insert "x"))
    (set-file-times (expand-file-name "sub" zdir) now)
    (set-file-times (expand-file-name "old.txt" zdir) (time-subtract now 50))
    (set-file-times zdir (time-subtract now 100))
    (set-file-times (expand-file-name "empty" root) (time-subtract now 200))
    (set-file-times root (time-subtract now 300))
    root))

(defun dired-subtree-dots--case (label ok got want &optional err)
  (list :label (format "dired dots: %s" label)
        :ok (and ok (null err)) :got (format "%S" got) :want want :err err))

(defun dired-subtree-dots-e2e ()
  "Dot entries at the head of a listing, and gone from its subtrees."
  (let* ((root (dired-subtree-dots--fixture))
         (zdir (expand-file-name "zdir" root))
         (empty (expand-file-name "empty" root))
         (buf (dired-noselect root))
         results)
    (unwind-protect
        (with-current-buffer buf
          (push (dired-subtree-dots--case
                 "the subtree advice reached dired-subtree--readin"
                 (and (advice-member-p #'dired-subtree-drop-dot-entries-a
                                       'dired-subtree--readin)
                      t)
                 (and (advice-member-p #'dired-subtree-drop-dot-entries-a
                                       'dired-subtree--readin)
                      t)
                 "t")
                results)
          ;; the rest only means anything while ls really does bury the dots
          (let ((raw (dired-subtree-dots--raw-names zdir)))
            (push (dired-subtree-dots--case
                   "the fixture buries \".\" in zdir's ls output"
                   (not (equal "." (car raw))) raw "\".\" not first")
                  results))
          (let ((got (dired-subtree-dots--names)))
            (push (dired-subtree-dots--case
                   "\".\" and \"..\" head the time-sorted listing"
                   (equal (seq-take got 2) '("." "..")) got "(\".\" \"..\" ...)")
                  results))
          (dired-goto-file zdir)
          (let ((want '("." ".." "zdir" "sub" "old.txt" "empty")) err)
            (condition-case e (dired-subtree-insert) (error (setq err e)))
            (let ((got (dired-subtree-dots--names)))
              (push (dired-subtree-dots--case
                     "an expanded subtree carries no dot entries"
                     (equal got want) got (format "%S" want) err)
                    results)))
          (dired-goto-file empty)
          (let (err)
            (condition-case e (dired-subtree-insert) (error (setq err e)))
            (push (dired-subtree-dots--case
                   "expanding an empty directory does not signal"
                   (null err) err "nil" err)
                  results))
          (nreverse results))
      (kill-buffer buf)
      (delete-directory root t))))

(add-to-list 'e2e-scenarios #'dired-subtree-dots-e2e)
