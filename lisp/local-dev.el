;;; lisp/local-dev.el --- clone missing own-package checkouts -*- lexical-binding: t; -*-
;;; Commentary:
;; Own packages (the `local-dev-packages' registry in init.el) build in place
;; from a checkout under ~/GitHub/agzam/, so edits land without a push+pull.
;; `local-checkout-recipe' (init.el) redirects elpaca to that checkout when it
;; exists; this file fills the gap when it does NOT - a freshly added own
;; package, or a machine that has never checked it out.  Elpaca's fallback is
;; an anonymous https clone: it fails on the private repos (git never sees an
;; Emacs/GITHUB_TOKEN token) and lands the public ones in elpaca's own sources
;; dir rather than the dev folder.  So before elpaca resolves any recipe, clone
;; each missing checkout over ssh (works for private, uses the user's keys)
;; into its dev folder; elpaca then builds in place exactly as on a machine
;; that already had it.
;;
;; CI is the extreme of that machine: it has no checkout, no ssh key, and a
;; restored elpaca cache whose clones and builds elpaca never refreshes on its
;; own.  It clones over https and drops the cached build of everything it
;; cloned (`drop-local-dev-builds'), so a config commit always meets the
;; own-package commit it was written for.
;;; Code:

(require 'cl-lib)

(defvar local-dev-clone-url-format "git@github.com:%s.git"
  "Format string turning an \"OWNER/REPO\" into a clone url.
Ssh by default: it reaches the private own packages with the user's keys.  CI
has no key and binds the https form instead, which its credential rewrite
authenticates.")

(defun local-dev--derive-repo (dir)
  "Derive a GitHub \"OWNER/REPO\" from checkout DIR's last two path components.
E.g. \"~/GitHub/agzam/remoto.el\" -> \"agzam/remoto.el\".  Naming the local
folder after the repo is the convention; the odd ones out are handled by an
overrides table (see `local-dev-clone-spec')."
  (let* ((clean (directory-file-name (expand-file-name dir)))
         (repo  (file-name-nondirectory clean))
         (owner (file-name-nondirectory
                 (directory-file-name (file-name-directory clean)))))
    (concat owner "/" repo)))

(defun local-dev-clone-spec (name dir overrides)
  "Return (URL . BRANCH) for cloning own package NAME's checkout into DIR.
The url follows `local-dev-clone-url-format'.  OVERRIDES is an alist of
\(NAME :repo REPO :branch BRANCH).  A missing :repo is derived from DIR (see
`local-dev--derive-repo'); a missing :branch is nil, meaning the remote's
default branch."
  (let* ((ov     (cdr (assq name overrides)))
         (repo   (or (plist-get ov :repo) (local-dev--derive-repo dir)))
         (branch (plist-get ov :branch)))
    (cons (format local-dev-clone-url-format repo) branch)))

(defun local-dev--clone (url dir branch)
  "Clone URL (BRANCH, or the remote default when nil) into DIR.
Parent directories are created first.  ssh runs in BatchMode and git with
prompts disabled, so a missing key or an offline host fails fast rather than
hanging a headless update or blocking startup.  Return non-nil on success."
  (make-directory (file-name-directory (directory-file-name dir)) t)
  (let ((process-environment
         (append '("GIT_TERMINAL_PROMPT=0"
                   "GIT_SSH_COMMAND=ssh -oBatchMode=yes -oConnectTimeout=10")
                 process-environment)))
    (eq 0 (apply #'call-process
                 "git" nil nil nil "clone"
                 (append (and branch (list "--branch" branch))
                         (list url dir))))))

(defun ensure-local-dev-checkouts (&optional packages overrides emit)
  "Clone every missing own-package checkout in PACKAGES over ssh.
PACKAGES is an alist of (NAME . DIR), defaulting to `local-dev-packages';
OVERRIDES defaults to `local-dev-clone-overrides'.  A checkout that already
exists is left untouched - the only reason to skip.  EMIT, when non-nil, is
called as (EMIT FMT &rest ARGS) to report progress; it defaults to `message'.
Return the list of freshly cloned package names.

Meant to run before elpaca resolves recipes, so a checkout cloned here is
picked up by `local-checkout-recipe' and built in place - no https fallback,
nothing landing in elpaca's sources dir.  A failed clone is reported and
skipped, never aborting the caller."
  (let ((packages  (or packages  (bound-and-true-p local-dev-packages)))
        (overrides (or overrides (bound-and-true-p local-dev-clone-overrides)))
        (emit (or emit (lambda (fmt &rest args) (apply #'message fmt args))))
        cloned)
    (dolist (cell packages (nreverse cloned))
      (let* ((name (car cell))
             (dir  (expand-file-name (cdr cell))))
        (unless (file-directory-p dir)
          (pcase-let ((`(,url . ,branch) (local-dev-clone-spec name dir overrides)))
            (funcall emit "local-dev: cloning %s -> %s" name dir)
            (if (local-dev--clone url dir branch)
                (push name cloned)
              (funcall emit
                       "local-dev: WARNING could not clone %s (%s); leaving it to elpaca"
                       name url)
              ;; Any dir present now is this attempt's debris (the enclosing
              ;; check saw none): a partial clone would make
              ;; `local-checkout-recipe' build in place from junk, so drop it
              ;; whole - elpaca's own clone can still run.
              (when (file-directory-p dir)
                (delete-directory dir t)))))))))

(defun drop-local-dev-builds (names &optional builds-dir emit)
  "Delete elpaca's build directory for each own package in NAMES.
BUILDS-DIR defaults to `elpaca-builds-directory'; without one nothing is
deleted.  EMIT reports progress like `ensure-local-dev-checkouts'.  Return the
names whose build was deleted.

Elpaca reads an existing build directory as built and only activates it, so a
build restored from a cache outlives the checkout it was made from - silently,
until the config calls something that build predates.  Dropping it runs the
full build steps against the checkout present now."
  (let ((builds-dir (or builds-dir (bound-and-true-p elpaca-builds-directory)))
        (emit (or emit (lambda (fmt &rest args) (apply #'message fmt args))))
        dropped)
    (when builds-dir
      (dolist (name names)
        (let ((build (expand-file-name (symbol-name name) builds-dir)))
          (when (file-directory-p build)
            (funcall emit "local-dev: dropping cached build of %s" name)
            (delete-directory build t)
            (push name dropped)))))
    (nreverse dropped)))

(provide 'local-dev)
;;; lisp/local-dev.el ends here
