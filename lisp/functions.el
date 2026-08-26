;;; lisp/functions.el -*- lexical-binding: t; -*-

;;;###autoload
(defun display-buffer-in-quadrant (buffer alist)
  "Display BUFFER in a side window while preserving existing window dimensions.
When displaying BUFFER for the first time, creates a new window using a quarter
- 25% of the frame width. If BUFFER is already displayed, reuses the existing
window without modifying its dimensions.

The side is determined by the direction entry, e.g., (direction . right)
Initial width controlled by init-width entry, e.g., (init-width . 0.10)
- would occupy the 10% of the frame width

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'. It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((existing (get-buffer-window buffer))
        (init-w (alist-get 'init-width alist 0.25)))
    (if existing
        (display-buffer-reuse-window buffer alist)
      (when-let ((window (display-buffer-in-direction buffer alist)))
        (with-selected-window window
          (window-resize window
                         (- (round (* init-w (frame-pixel-width)))
                            (window-width window t))
                         t nil t))
        window))))

(defun system-dist-name ()
  "Returns system distribution name.

e.g. Ubuntu, Arch; or macOS 15.5, etc."
  (let ((cmd+prefix (alist-get
                     system-type
                     '((gnu/linux . ("lsb_release -si 2>/dev/null"))
                       (darwin . ("sw_vers -productVersion" "macOS "))))))
    (thread-last
      (car cmd+prefix)
      shell-command-to-string
      string-trim
      (concat (cadr cmd+prefix)))))

(defun transient-remap-suffix-key (prefix from to)
  "In transient PREFIX, rebind the suffix on key FROM to key TO.
No-op when FROM is absent, so `reload-config' re-running against an
already-remapped layout can't error.  FROM and TO are transient's key
strings like \"RET\", not `kbd' output, which it will not match."
  (when (ignore-errors (transient-get-suffix prefix from))
    (transient-suffix-put prefix from :key to)))

(defun transient-set-suffix-key (prefix command key)
  "In transient PREFIX, put COMMAND's suffix on KEY.
Locating the suffix by its command is what makes exchanging two keys
work: a lookup by key finds whichever suffix sits there now, so the
second half of a swap would move the suffix the first half just placed.
No-op when PREFIX has no such suffix."
  (when (ignore-errors (transient-get-suffix prefix command))
    (transient-suffix-put prefix command :key key)))

;; Doom hook names kept: vendored doom-keybinds.el registers on the before
;; hook (which-key replacement-alist reset).
(defvar doom-before-reload-hook nil
  "Hooks run by `reload-config' before reloading.")
(defvar doom-after-reload-hook nil
  "Hooks run by `reload-config' after reloading.")

(defun reload-config ()
  "Reload the config in place, the lab analogue of Doom's doom/reload.

Re-runs the layers init.el runs, in the same order: lisp/doom-defaults,
lisp/functions, every module in `active-modules' (regenerating stale
loaddefs), the root config.el, then custom.el.  Package declarations
re-queue with elpaca and any new ones install on the spot.

Not covered - restart instead: the elpaca bootstrap, the macro layer
(doom-compat/doom-keybinds) and `active-modules' changes.  Edited
autoload/*.el files keep their old in-memory definitions (loaddefs never
re-defines loaded functions); `load-file' those directly."
  (interactive)
  (let ((start-time (current-time)))
    (doom-run-hooks 'doom-before-reload-hook)
    (dolist (file '("lisp/doom-defaults" "lisp/functions"))
      (load (expand-file-name file user-emacs-directory) nil 'nomessage))
    (mapc #'load-module active-modules)
    (load (expand-file-name "config" user-emacs-directory) nil 'nomessage)
    (when custom-file
      (load custom-file 'noerror 'nomessage))
    (elpaca-process-queues)
    (doom-run-hooks 'doom-after-reload-hook)
    (message "Config reloaded in %.02fs" (float-time (time-since start-time)))))

(defun build-dir-half-built-p (dir autoloads-file)
  "Non-nil when build DIR exists without a working AUTOLOADS-FILE.
An interrupted elpaca update/rebuild stops after the link step: the dir
holds only linked sources, elpaca still activates it on the next boot
(its autoloads load is noerror), and the package's commands silently
come up void.  A dir with either artifact present got past linking.
A dangling autoloads symlink is broken regardless of leftover .elc
files: builds link sources by absolute path, so a relocated sources
tree (renamed config dir, moved CI workspace) severs every link while
the compiled files - real files - stay behind and mask the damage."
  (and (file-directory-p dir)
       (let ((autoloads (expand-file-name autoloads-file dir)))
         (and (not (file-exists-p autoloads))
              (or (file-symlink-p autoloads)
                  (not (directory-files dir nil "\\.elc\\'" t)))))))

(defun half-built-elpaca-packages ()
  "Return (ID . BUILD-DIR) for every queued elpaca package left half-built.
See `build-dir-half-built-p' for what qualifies.  The autoloads file
name honors the recipe's :autoloads override (org's org-loaddefs.el)."
  (when (fboundp 'elpaca--queued)
    (let (broken)
      (pcase-dolist (`(,id . ,e) (elpaca--queued))
        (let ((dir (elpaca<-build-dir e))
              (autoloads (or (plist-get (elpaca<-recipe e) :autoloads)
                             (format "%s-autoloads.el" (elpaca<-package e)))))
          (when (and dir (stringp autoloads)
                     (build-dir-half-built-p dir autoloads))
            (push (cons id dir) broken))))
      (nreverse broken))))

(defun stale-elc-files (dir)
  "Return .elc files in DIR whose sibling .el is newer.
Aftermath of an interrupted elpaca update: sources merged, the recompile
never ran, and Emacs silently loads the older bytecode from then on."
  (when (file-directory-p dir)
    (seq-filter
     (lambda (elc)
       (let ((el (concat (file-name-sans-extension elc) ".el")))
         (and (file-exists-p el) (file-newer-than-file-p el elc))))
     (directory-files dir t "\\.elc\\'"))))

(defun stale-elpaca-builds ()
  "Return (ID . STALE-ELCS) for packages built into elpaca's builds dir.
Build-in-place local checkouts are skipped: mid-development edits make
their bytecode stale by definition, and `bb update' rebuilds those."
  (when (and (fboundp 'elpaca--queued) (boundp 'elpaca-builds-directory))
    (let (stale)
      (pcase-dolist (`(,id . ,e) (elpaca--queued))
        (when-let* ((dir (elpaca<-build-dir e))
                    ((file-in-directory-p dir elpaca-builds-directory))
                    (elcs (stale-elc-files dir)))
          (push (cons id elcs) stale)))
      (nreverse stale))))

(defun broken-elpaca-builds ()
  "Return (ID . REASON) for every broken elpaca build.
REASON is `half-built' (no autoloads) or `stale' (source newer than
.elc); when the deeper `elpaca-local' checks are loaded - the update and
repair drivers load them, the boot warn hook stays cheap without - also
`dirty': the build never saw a source the repo now holds (a merge that
landed right before the process died, the killed-mid-update case).
Custom :build/:autoloads recipes are exempt from the dirty scan - their
build marker lives elsewhere and would read dirty forever (org)."
  (let* ((base (append (mapcar (lambda (b) (cons (car b) 'half-built))
                               (half-built-elpaca-packages))
                       (mapcar (lambda (s) (cons (car s) 'stale))
                               (stale-elpaca-builds))))
         (dirty
          (when (and (fboundp 'elpaca-local-build-dirty-p)
                     (fboundp 'elpaca--queued))
            (let (acc)
              (pcase-dolist (`(,id . ,e) (elpaca--queued))
                (let ((recipe (elpaca<-recipe e)))
                  (when (and (not (assq id base))
                             (not (plist-member recipe :build))
                             (not (plist-member recipe :autoloads))
                             (not (elpaca-local-package-p e))
                             (elpaca-local-build-dirty-p e))
                    (push (cons id 'dirty) acc))))
              (nreverse acc)))))
    (append base dirty)))

(defun rebuild-broken-elpaca-builds (&optional emit)
  "Queue an `elpaca-rebuild' for every broken build; return the (ID . REASON) list.
EMIT, when non-nil, is called as (EMIT FMT &rest ARGS) per package.
Queued non-interactively `elpaca-rebuild' never kicks the queue (the
go-jira stall, see `elpaca-local-rebuild-changed'), so one
`elpaca-process-queues' follows; callers wait for elpaca to settle."
  (let ((broken (broken-elpaca-builds)))
    (pcase-dolist (`(,id . ,reason) broken)
      (when emit (funcall emit "rebuild (%s): %s" reason id))
      (elpaca-rebuild id))
    (when (and broken (fboundp 'elpaca-process-queues))
      (elpaca-process-queues))
    broken))
