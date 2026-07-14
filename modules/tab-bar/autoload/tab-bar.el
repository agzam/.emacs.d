;;; modules/tab-bar/autoload/tab-bar.el -*- lexical-binding: t; -*-

;; Renames from the doom.d +tab-bar-* family, collision-checked live:
;; tab-bar-rename-tab and tab-bar-duplicate-tab are real built-ins, so
;; their wrappers become tab-bar-rename / tab-bar-duplicate; the movers
;; follow the built-in tab-move alias family (tab-move-left/right).

(require 'tab-bar)
(require 'transient)
;; project-root is a plain cl-defgeneric (no autoload cookie of its own)
(require 'project)

(defun tab-bar-rename-dups (&optional tabs)
  "Uniquify duplicate tab names with a numeric suffix.
Tabs in TABS (default `tab-bar-tabs') sharing a base name - existing
numeric suffixes ignored - keep the first occurrence bare; the rest
get \" 2\", \" 3\", ...  Mutates the tab alists in place."
  (dolist (group (seq-group-by
                  (lambda (tab)
                    (when-let* ((name (alist-get 'name tab)))
                      (replace-regexp-in-string " [[:digit:]]+$" "" name)))
                  (or tabs (tab-bar-tabs))))
    (pcase-let ((`(,base . ,dups) group))
      (when (and base (length> dups 1))
        (seq-do-indexed
         (lambda (tab i)
           (setf (alist-get 'name tab)
                 (if (< 0 i) (format "%s %d" base (1+ i)) base))
           (setf (alist-get 'explicit-name tab) t))
         dups)))))

;;;###autoload
(defun tab-move-left ()
  "Move the current tab one position to the left."
  (interactive)
  (tab-bar-move-tab -1))

;;;###autoload
(defun tab-move-right ()
  "Move the current tab one position to the right."
  (interactive)
  (tab-bar-move-tab 1))

;;;###autoload
(defun tab-bar-add-new-tab ()
  "Open a fresh rightmost tab on the scratch buffer, then offer templates."
  (interactive)
  (let ((tab-bar-new-tab-to 'rightmost))
    (tab-bar-new-tab))
  (switch-to-scratch-buffer)
  (tab-bar-new-tab-transient))

;;;###autoload
(defun tab-bar-kill-tab ()
  "Close the current tab and re-uniquify the remaining names."
  (interactive)
  (tab-bar-close-tab)
  (run-with-timer 0.3 nil #'tab-bar-rename-dups))

;;;###autoload
(defun tab-bar-duplicate ()
  "Duplicate the current tab; keep names uniquified.
Wraps built-in `tab-bar-duplicate-tab' (hence the distinct name)."
  (interactive)
  (tab-bar-duplicate-tab)
  (tab-bar-rename-dups))

;;;###autoload
(defun tab-bar-rename ()
  "Rename the current tab interactively; keep names uniquified.
Wraps built-in `tab-bar-rename-tab' (hence the distinct name)."
  (interactive)
  (call-interactively #'tab-bar-rename-tab)
  (tab-bar-rename-dups))

;;;###autoload
(defun tab-bar-name-fn ()
  "Name the tab after its project (or directory), git branch appended.
Worktree checkouts get named after the worktree container directory."
  (require 'magit)
  (let* ((buf-fname (buffer-file-name))
         (buf-name (buffer-name))
         (buf-dir (when buf-fname (file-name-directory buf-fname)))
         (branch (when (or buf-fname
                           (eq major-mode 'dired-mode))
                   (magit-get-current-branch)))
         (check-fn (lambda (opt)
                     (let ((rev-parse-res (or (magit-rev-parse opt) "")))
                       (if (string= rev-parse-res ".") t
                         (string= ".git" (substring rev-parse-res -4))))))
         (worktree? (and branch
                         (not (and (funcall check-fn "--git-dir")
                                   (funcall check-fn "--git-common-dir")))))
         (project-root (when-let* ((proj (project-current nil)))
                         (expand-file-name (project-root proj))))
         (buf-dir-project (when buf-dir (project-current nil buf-dir)))
         (label (cond
                 ((member major-mode '(gh-notify-mode)) buf-name)

                 (worktree?
                  (file-name-nondirectory
                   (expand-file-name ".." project-root)))

                 ((and branch (not worktree?))
                  (file-name-nondirectory
                   (expand-file-name "." project-root)))

                 (buf-dir-project
                  (file-name-nondirectory
                   (directory-file-name (project-root buf-dir-project))))

                 (buf-dir buf-dir)

                 ((not (string-match-p "\\*Minibuf" buf-name))
                  buf-name))))
    (concat label (when branch (format "󠀠 ▸ %s" branch)))))

;;;###autoload
(defun tab-bar-fmt-show-index-fn (name _tab idx)
  "Prefix NAME with a styled IDX when `tab-bar-tab-hints' is on."
  (if tab-bar-tab-hints
      (concat
       (propertize (number-to-string idx)
                   'display '(raise -0.5)
                   'face '(:height 1.2
                           :weight bold
                           :foreground "orange"))
       name)
    name))

;;;###autoload
(defun tab-bar-move-buffer-to-tab ()
  "Send the current buffer to another tab and follow it there."
  (interactive)
  (let ((buf (current-buffer))
        (pos (point)))
    (if (length= (window-list) 1)
        (bury-buffer)
      (delete-window))
    (call-interactively #'tab-bar-select-tab-by-name)
    (split-window-sensibly)
    (switch-to-buffer buf)
    (goto-char pos)))

;;;###autoload
(defun tab-bar-kill-project-buffers ()
  "Kill the current project's buffers and close the tab."
  (interactive)
  (require 'project)
  (project-kill-buffers)
  (tab-bar-kill-tab))

;;;###autoload
(defun tab-bar-find-buffer-in-tabs ()
  "Pick a buffer with consult and jump to the tab that owns it."
  (interactive)
  (require 'consult)
  (let ((sel-buf nil))
    (cl-letf (((symbol-function 'consult--buffer-action)
               (lambda (b) (setq sel-buf b))))
      (consult-buffer)
      (when-let* ((tab (tab-bar-get-buffer-tab sel-buf)))
        (if (eq 'current-tab (car tab))
            (select-window (get-buffer-window sel-buf))
          (tab-bar-switch-to-tab (alist-get 'name tab)))))))

;;;###autoload
(defun quicksave-session ()
  "Save the desktop session into the canonical state dir."
  (interactive)
  (desktop-save (car desktop-path)))

;;;###autoload
(defun restore-desktop-and-tabs ()
  "Manually restore the saved desktop (buffers + tabs).
Requiring the heavyweights first keeps `desktop-read' from
resurrecting their buffers before the modes exist."
  (interactive)
  (require 'org-roam-mode)
  (require 'magit)
  (tab-bar-mode 1)
  (desktop-read doom-state-dir))

;;; Transients

;;;###autoload
(transient-define-prefix tab-bar-new-tab-transient ()
  "New Tab"
  ["Choose a template"
   [("ort" "work note" (lambda () (interactive) (open-journal 'work)))
    ("orT" "personal note" (lambda () (interactive) (open-journal 'personal)))
    ("orr" "roam find" vulpea-find)
    ("orb" "backlinks" vulpea-backlinks)]

   [("gt" "gptel" open-gptel)
    ("gn" "gh-notify" gh-notify)]

   ;; apps/chat column: elfeed dropped (web-browsing), notmuch waits on its
   ;; module; telega restored with the chat port
   [("t" "telega" telega)]

   [("ed" "config" find-in-config-dir)
    ("D" "dotfile.org" (lambda ()
                         (interactive)
                         (find-file
                          (expand-file-name
                           "agzam/dotfile.org/dotfile.org"
                           (or (bound-and-true-p magit-clone-default-directory)
                               "~/GitHub/")))))]

   ;; darwin-only, like the jira module: go-jira-browse-default-board is only
   ;; autoloaded there (:if (featurep :system 'macos)), so the column hides
   ;; where the command doesn't exist.
   [:if (lambda () (eq system-type 'darwin))
    ("jb" "def. jira board" go-jira-browse-default-board)]

   [("p" "projects" (lambda ()
                      (interactive)
                      (dired (project-prompt-project-dir))))
    ("SPC" "zoxide history" zoxide-find)
    ("b" "buffers" consult-buffer)
    ("fd" "zoxide" zoxide-find)
    ("fr" "recent" consult-recent-file)]
   [("d" "kill tab" tab-bar-kill-tab)]])

(defun tab-bar-hints-off-h ()
  "Turn tab index hints back off; runs once per transient exit."
  (setq tab-bar-tab-hints nil)
  (remove-hook 'transient-exit-hook #'tab-bar-hints-off-h))

;;;###autoload
(transient-define-prefix tab-bar-transient ()
  "Layouts"
  ["Layouts"
   ["" "" "" ""
    ("<tab>" "recent" tab-bar-switch-to-recent-tab)]
   [("j" "prev" tab-bar-switch-to-prev-tab :transient t)
    ("k" "next" tab-bar-switch-to-next-tab :transient t)
    ("<" "move left" tab-move-left :transient t)
    (">" "move right" tab-move-right :transient t)
    ("w" "move window to new tab" tab-bar-move-window-to-tab)]
   [("t" "new tab" tab-bar-add-new-tab)
    ("n" "new tab" tab-bar-add-new-tab)
    ("D" "Duplicate" tab-bar-duplicate)
    ("r" "rename" tab-bar-rename)
    ("l" "select" tab-bar-select-tab-by-name)]
   [("b" "move buffer to tab" tab-bar-move-buffer-to-tab)
    ("f" "find tab with current buffer" tab-bar-find-buffer-in-tabs)
    ("K" "kill project buffers" tab-bar-kill-project-buffers)]
   ;; window-undo/redo instead of tab-bar-history-back/forward: the
   ;; stock commands restore stale cursor positions and mismanage the
   ;; forward ring
   [("[" "layout undo" window-undo :transient t)
    ("]" "layout redo" window-redo :transient t)
    ("dd" "kill tab" tab-bar-kill-tab :transient t)
    ("u" "undo kill tab" tab-undo)
    ("SPC" "templates" tab-bar-new-tab-transient)]
   [("dr" "restore" restore-desktop-and-tabs)
    ("ds" "save" quicksave-session)]]
  [:hide always
   :class transient-columns
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'tab-bar-transient
      (mapcar
       (lambda (n)
         (list (number-to-string n)
               (format "Goto: %s" n)
               (lambda ()
                 (interactive)
                 (tab-bar-select-tab n))))
       (number-sequence 1 9))))]
  (interactive)
  (setq tab-bar-tab-hints t)
  (add-hook 'transient-exit-hook #'tab-bar-hints-off-h)
  (transient-setup 'tab-bar-transient))
