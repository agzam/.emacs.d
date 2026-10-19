;;; tests/e2e/dired-subtree-dots.el --- dot entries in dired and its subtrees -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el in a booted Emacs; the name stays clear of
;; *-tests.el so buttercup skips it.  The buttercup suite covers the listing
;; functions over canned text; this file covers real `ls' output, dired-subtree
;; splicing it in, and the treemacs icons advice walking what comes back.

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
Its timestamps make `ls -t' list zdir/sub above \".\", and solo/only
between \"..\" and \".\": the orders that defeat dired-subtree's own
dot stripping, which assumes the dot entries lead the listing."
  (let* ((root (expand-file-name "dired-dots/" e2e-work-dir))
         (zdir (expand-file-name "zdir/" root))
         (solo (expand-file-name "solo/" root))
         (now (current-time)))
    (when (file-directory-p root) (delete-directory root t))
    (make-directory (expand-file-name "sub" zdir) t)
    (make-directory (expand-file-name "only" solo) t)
    (make-directory (expand-file-name "empty" root) t)
    (with-temp-file (expand-file-name "old.txt" zdir) (insert "x"))
    (set-file-times (expand-file-name "sub" zdir) now)
    (set-file-times (expand-file-name "old.txt" zdir) (time-subtract now 50))
    (set-file-times zdir (time-subtract now 100))
    (set-file-times (expand-file-name "empty" root) (time-subtract now 200))
    (set-file-times (expand-file-name "only" solo) (time-subtract now 400))
    (set-file-times solo (time-subtract now 500))
    (set-file-times root (time-subtract now 300))
    root))

(defun dired-subtree-dots--case (label ok got want &optional err)
  (list :label (format "dired dots: %s" label)
        :ok (and ok (null err)) :got (format "%S" got) :want want :err err))

(defun dired-subtree-dots--expand (file)
  "Expand the subtree on FILE's line; return the error, if any."
  (if (dired-goto-file file)
      (condition-case e (ignore (dired-subtree-insert)) (error e))
    `(error ,(format "no line for %s" file))))

(defun dired-subtree-dots-e2e ()
  "Dot entries at the head of a listing, and gone from its subtrees."
  (let* (;; the config makes dired list truenames, and `dired-goto-file'
         ;; misses a name that differs, as one under a symlinked tmp dir does
         (root (file-truename (dired-subtree-dots--fixture)))
         (zdir (expand-file-name "zdir" root))
         (solo (expand-file-name "solo" root))
         (empty (expand-file-name "empty" root))
         (buf (dired-noselect root))
         results)
    (unwind-protect
        (with-current-buffer buf
          (let ((got (and (advice-member-p #'dired-subtree-readin-a
                                           'dired-subtree--readin)
                          t)))
            (push (dired-subtree-dots--case
                   "the subtree reader overrides dired-subtree--readin"
                   got got "t")
                  results))
          ;; the rest only means anything while ls really does bury the dots
          (let ((raw (dired-subtree-dots--raw-names zdir)))
            (push (dired-subtree-dots--case
                   "the fixture buries \".\" in zdir's ls output"
                   (not (equal "." (car raw))) raw "\".\" not first")
                  results))
          (let ((raw (dired-subtree-dots--raw-names solo)))
            (push (dired-subtree-dots--case
                   "the fixture lists solo's only child between \"..\" and \".\""
                   (equal raw '(".." "only" ".")) raw "(\"..\" \"only\" \".\")")
                  results))
          (let ((got (dired-subtree-dots--names)))
            (push (dired-subtree-dots--case
                   "\".\" and \"..\" head the time-sorted listing"
                   (equal (seq-take got 2) '("." "..")) got "(\".\" \"..\" ...)")
                  results))
          (let* ((err (dired-subtree-dots--expand zdir))
                 (got (dired-subtree-dots--names))
                 (want '("." ".." "zdir" "sub" "old.txt" "empty" "solo")))
            (push (dired-subtree-dots--case
                   "an expanded subtree carries no dot entries"
                   (equal got want) got (format "%S" want) err)
                  results))
          (let* ((err (dired-subtree-dots--expand solo))
                 (got (seq-take (member "solo" (dired-subtree-dots--names)) 2)))
            (push (dired-subtree-dots--case
                   "a subtree keeps the only child listed between the dots"
                   (equal got '("solo" "only")) got "(\"solo\" \"only\")" err)
                  results))
          (let ((err (dired-subtree-dots--expand empty)))
            (push (dired-subtree-dots--case
                   "expanding an empty directory does not signal"
                   (null err) err "nil" err)
                  results))
          (nreverse results))
      (kill-buffer buf)
      (delete-directory root t))))

(add-to-list 'e2e-scenarios #'dired-subtree-dots-e2e)
