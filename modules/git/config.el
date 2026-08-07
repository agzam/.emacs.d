;;; modules/git/config.el -*- lexical-binding: t; -*-

;; NOTE Own packages (remoto, github-topics) declare their GitHub recipes;
;; `local-checkout-recipe' (init.el) redirects them to ~/GitHub/agzam/
;; checkouts on machines that have them.

(use-package magit
  :ensure (magit :host github :repo "magit/magit")
  :commands magit-file-delete
  :defer-incrementally (with-editor git-commit package eieio transient)
  :init
  (setopt magit-clone-default-directory (expand-file-name "~/GitHub/"))
  :config
  ;; otherwise starts magit in evil-emacs-state
  (dolist (m '(magit-status-mode
               magit-refs-mode
               magit-revision-mode))
    (evil-set-initial-state m nil))

  ;; Magit is hardcoded to prefer ~/.git-credential-cache/ over the XDG
  ;; location; override when the legacy dir doesn't exist.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setopt magit-credential-cache-daemon-socket
            (doom-glob (or (getenv "XDG_CACHE_HOME")
                           "~/.cache/")
                       "git/credential/socket")))
  (setopt
   magit-save-repository-buffers 'dontask
   magit-clone-set-remote.pushDefault nil
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-delete-by-moving-to-trash nil
   ;; do not push renamed/deleted branch to remote automatically
   magit-branch-rename-push-target nil
   magit-diff-refine-hunk 'all
   ;; parent/related refs in commit buffers are rarely helpful and cost runtime
   magit-revision-insert-related-refs nil)

  ;; Disable some hooks on Mac - Magit is a bit slow otherwise
  (when (featurep :system 'macos)
    (remove-hook! 'magit-status-sections-hook
      #'magit-insert-unpulled-from-upstream)
    (remove-hook! 'magit-status-headers-hook
      #'magit-insert-head-branch-header
      #'magit-insert-push-branch-header))

  ;; `git log ORIG_HEAD..HEAD': log only changes since the last pull
  (transient-append-suffix 'magit-log "l"
    '("p" "orig_head..head" magit-log-orig_head--head))
  (transient-append-suffix 'magit-log "l"
    '("R" "other..current" magit-log-other--current))
  (transient-append-suffix 'magit-log "l"
    '("m" "origin/main..current" magit-log--origin-main))
  (transient-append-suffix 'magit-diff "d"
    '("R" "Diff range (reversed)" magit-diff-range-reversed))
  (transient-append-suffix 'magit-diff "d"
    '("m" "origin/main..current" magit-diff--origin-main))
  (transient-append-suffix 'magit-worktree 'magit-worktree-branch
    '("i" "create from issue" magit-worktree-branch-from-issue))
  (transient-append-suffix 'magit-worktree "k"
    '("K" "delete merged" magit-worktree-delete-merged))
  (transient-append-suffix 'magit-worktree '(0 1 -1)
    '("f m" "move file to worktree" magit-worktree-move-file))
  (transient-append-suffix 'magit-file-dispatch ", c"
    '(", m" "move to worktree" magit-worktree-move-file))
  (transient-append-suffix 'magit-rebase 'magit-rebase-subset
    '("O" "rebase on origin/main" magit-rebase-origin-main))

  ;; poor UX to have the target file at the bottom of the window
  (advice-add #'magit-status-here :after #'doom-recenter-a)

  (dolist (v '((magit-pull "--rebase")
               (magit-show-refs "--sort=-committerdate")
               (magit-fetch "--prune")))
    (add-to-list 'transient-values v))

  ;; who cares if tags not displayed in magit-refs buffer?
  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags)

  ;; 'Path' column in submodule list is a bit too short
  (setf
   (car
    (alist-get
     "Path"
     magit-submodule-list-columns nil nil #'string=))
   50)

  ;; doom.d's ("o" "Dummy" nil) magit-dispatch shim is gone: current
  ;; magit-dispatch has its own "o" for forge to anchor on, and modern
  ;; transient hard-errors on nil-command suffixes (live probe caught it)

  (map! :map magit-blame-read-only-mode-map
        :n "RET" #'magit-show-commit)

  (add-hook! 'magit-credential-hook #'update-ssh-auth-sock-h))

(use-package evil-collection-magit
  :when (modulep! :editor evil +everywhere)
  :ensure nil  ; ships inside evil-collection
  :defer t
  :init (defvar evil-collection-magit-use-z-for-folds t)
  :config
  ;; q is enough; ESC is way too easy for a vimmer to accidentally press,
  ;; especially when traversing modes in magit buffers.
  (evil-define-key* 'normal magit-status-mode-map [escape] nil)

  (evil-define-key* 'normal magit-revision-mode-map
    "q" #'magit-log-bury-buffer)

  (map! (:map magit-mode-map
         :nv "z" #'magit-stash
         :nv "W" #'magit-worktree
         :nv "q" #'magit-quit
         :nv "Q" #'magit-quit-all
         :nv "gr" #'magit-refresh
         :nv "gR" #'magit-refresh-all
         :nv "l" #'evil-forward-char
         :nv "h" #'evil-backward-char
         "M-l" #'magit-log
         ;; evil-collection's magit maps swallow global g-sequences;
         ;; both need restating here
         :n "gi" #'gptel-inline-dwim
         :n "gI" #'ibuffer-sidebar-jump)
        (:map magit-status-mode-map
         :nv "gz" #'magit-refresh)
        (:map magit-diff-mode-map
         :nv "gd" #'magit-jump-to-diffstat-or-diff)
        (:map magit-section-mode-map
         :nv "]" #'magit-section-forward-sibling
         :nv "[" #'magit-section-backward-sibling))

  ;; A more intuitive behavior for TAB in magit buffers:
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-process-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)

  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let* ((desc (assoc (car key) evil-collection-magit-rebase-commands-w-descriptions)))
        (setcar desc (cdr key))))
    (evil-define-key* evil-collection-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up))

  (transient-append-suffix 'magit-dispatch '(0 -1 -1)
    '("*" "Worktree" magit-worktree)))

(use-package forge
  :ensure (forge :host github :repo "magit/forge")
  ;; forge-database-file lives in doom-compat.el's quarantine section;
  ;; doom.d's `:after emacsql-sqlite-builtin' gate dropped - that feature no
  ;; longer exists (emacsql 4.x merged the backends), :after-call defers.
  :after-call magit-status
  :commands (forge-create-pullreq forge-create-issue)
  :config
  ;; evil-collection-magit moves magit-dispatch's "o" (submodule) to "'",
  ;; so forge's own ("N" "Forge") insertion at anchor "o" fails - the very
  ;; breakage doom.d's dummy-"o" shim papered over. Insert directly instead
  ;; (before Submodule when the remap is in effect, else next to Run).
  (unless (ignore-errors (transient-get-suffix 'magit-dispatch "N"))
    (transient-insert-suffix 'magit-dispatch
        (if (ignore-errors (transient-get-suffix 'magit-dispatch "'")) "'" "!")
      '("N" "Forge" forge-dispatch)))

  ;; All forge list modes are derived from `forge-topic-list-mode'
  (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
  (map! :map forge-topic-mode-map
        "0" #'evil-digit-argument-or-evil-beginning-of-line
        "$" #'evil-end-of-line
        "v" #'evil-visual-char
        "l" #'evil-forward-char
        "h" #'evil-backward-char
        "w" #'evil-forward-word-begin
        "b" #'evil-backward-word-begin
        (:localleader
         "l" #'forge-copy-url-at-point-as-kill
         (:prefix ("y" . "yank")
                  "y" #'git-link-forge-topic)))

  ;; forge-topic uses markdown to display images, sometimes they get too big
  (setq markdown-max-image-size '(700 . nil))

  ;; remove after https://github.com/magit/forge/discussions/861 addressed
  (defadvice! forge-graphql-tolerate-http-errors-a (fn req errors headers status)
    "Skip forge GraphQL batches that fail at the HTTP level instead of crashing.
ghub passes `(error http NNN)' (e.g. 403) to an errorback that expects a
GraphQL `errors' alist, which otherwise dies with `listp, http'."
    :around #'ghub--graphql-handle-failure
    (if (eq (car-safe errors) 'error)
        (progn
          (ghub--graphql-set-mode-line req)
          (message "forge: skipped a GraphQL batch (HTTP %s)" (nth 2 errors))
          (if-let* ((cb (ghub--req-callback req)))
              (ghub--graphql-run-callback req cb nil)
            (ghub--signal-error errors)))
      (funcall fn req errors headers status))))

(use-package git-link
  :ensure t
  :defer t
  :after magit
  :config
  (setq browse-at-remote-add-line-number-if-no-region-selected t))

(use-package gh-notify
  :ensure (gh-notify :host github :repo "anticomputer/gh-notify")
  :after (magit forge)
  :defer t
  :commands (gh-notify)
  :config
  (setq gh-notify-redraw-on-visit t
        gh-notify-show-state t)

  (map! :map gh-notify-mode-map
        :n "RET" #'gh-notify-visit-notification
        :n "q" #'kill-buffer-and-window
        (:after code-review
         :n "s-r" #'gh-notify-code-review-forge-pr-at-point))

  (map! :map gh-notify-mode-map
        "C-c C-o" #'gh-notify-forge-browse-topic-at-point
        :ni "r" #'gh-notify-mark-read-and-move-next
        :ni "u" #'gh-notify-mark-read-and-move-next
        :ni "U" #'gh-notify-mark-read-and-move-prev)

  (map! :localleader :map gh-notify-mode-map
        "C-l" nil
        "l" #'gh-notify-retrieve-notifications
        "r" #'gh-notify-reset-filter
        "t" #'gh-notify-toggle-timing
        "y" #'gh-notify-copy-url
        "s" #'gh-notify-display-state
        "i" #'gh-notify-ls-issues-at-point
        "P" #'gh-notify-ls-pullreqs-at-point
        "p" #'gh-notify-forge-refresh
        "g" #'gh-notify-forge-visit-repo-at-point
        "m" #'gh-notify-mark-notification
        "M" #'gh-notify-mark-all-notifications
        "u" #'gh-notify-unmark-notification
        "U" #'gh-notify-unmark-all-notifications
        (:prefix ("/" . "limit")
                 "d" #'gh-notify-toggle-global-ts-sort
                 "u" #'gh-notify-limit-unread
                 "U" (cmd! (gh-notify-limit-unread 2))
                 "'" #'gh-notify-limit-repo
                 "\"" #'gh-notify-limit-repo-none
                 "p" #'gh-notify-limit-pr
                 "i" #'gh-notify-limit-issue
                 "*" #'gh-notify-limit-marked
                 "a" #'gh-notify-limit-assign
                 "y" #'gh-notify-limit-author
                 "m" #'gh-notify-limit-mention
                 "t" #'gh-notify-limit-team-mention
                 "s" #'gh-notify-limit-subscribed
                 "c" #'gh-notify-limit-comment
                 "r" #'gh-notify-limit-review-requested
                 "/" #'gh-notify-limit-none))

  ;; always recenter when getting back to gh-notify buffer from forge-buffers
  (advice-add 'gh-notify--filter-notifications :after 'recenter)

  (defadvice! gh-notify-render-notification-a (fn notification)
    "Modify gh-notify columns for every row."
    :around #'gh-notify-render-notification
    (replace-regexp-in-string
     "\\[subscribed\\]" ""
     (funcall fn notification)))

  (defadvice! gh-notify-visit-notification-other-window-a (fn arg)
    "Always open topics in other-window."
    :around #'gh-notify-visit-notification
    (let* ((lexical-binding t)
           (magit-display-buffer-function #'magit-display-buffer-traditional))
      (funcall-interactively fn arg))))

(use-package code-review
  ;; @tarsius broke Code-Review, ag91's fork keeps it alive:
  ;; https://github.com/wandersoncferreira/code-review/issues/245
  :ensure (code-review :host github :repo "ag91/code-review")
  :defer t
  :after (magit forge)
  :init
  ;; code-review-db-database-file lives in doom-compat.el's quarantine section
  (map! :map (magit-status-mode-map
              forge-pullreq-mode-map
              forge-topic-mode-map)
        :n "s-r" #'code-review-forge-pr-at-point)
  :config
  ;; ghub v5.1.0 renamed `ghub-graphql' -> `ghub-query' but code-review
  ;; (ag91 fork) still calls the old name. Load the upstream compat shim.
  (unless (fboundp 'ghub-graphql)
    (require 'ghub-legacy))

  ;; some update keeps popping up ghub-debug buffers - super annoying
  (defadvice! code-review-build-buffer-no-debug-a (orig-fn &rest args)
    :around #'code-review--build-buffer
    (let ((buf-regx "\\*http api\\.github\\.com:443\\*"))
      (add-to-list 'display-buffer-alist
                   `(,buf-regx
                     (display-buffer-no-window)
                     (allow-no-window . t)))
      (apply orig-fn args)
      ;; let's make sure we can investigate the log buffers later
      (run-with-timer
       5 nil
       (lambda ()
         (setq display-buffer-alist
               (assoc-delete-all buf-regx display-buffer-alist))))))

  ;; doom.d quoted these features - (after! 'evil-escape ...) waits on the
  ;; feature `quote', so both blocks never ran there; fixed on port
  (after! evil-escape
    (add-to-list 'evil-escape-excluded-major-modes 'code-review-mode))

  (after! evil-collection
    (dolist (binding evil-collection-magit-mode-map-bindings)
      (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
        (dolist (state states)
          (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
    (evil-set-initial-state 'code-review-mode evil-default-state))

  (map! :map code-review-mode-map
        :nv (kbd "<escape>") nil
        :nv "," nil
        :n "q" #'kill-buffer-and-window
        "C-c C-o" #'code-review-browse-pr)

  (map! :map code-review-feedback-section-map
        "k" nil)

  (map! :localleader
        :map code-review-mode-map
        "," #'code-review-transient-api))

(after! bug-reference
  (setq bug-reference-default-org "agzam")
  (map! :map bug-reference-map
        "C-c C-o" #'bug-reference-push-button)

  (add-hook! bug-reference-mode #'init-bug-reference-mode-settings))

(add-hook! (org-mode markdown-mode) #'bug-reference-mode)

(after! diff-mode
  (setq diff-add-log-use-relative-names t))

;; ox-gfm ships no Version header and its v1.0 tag sits on a commit a
;; depth-1 clone never fetches, so consult-gh's (ox-gfm "1.0") requirement
;; reads "version 0" on every cold build; declare the version explicitly.
(elpaca (ox-gfm :version (lambda (_) "1.0")))

;; doom.d's consult-gh config block is `:disabled' - only the package ports
;; (autoloaded commands for SPC g c); its embark-glue autoloads skipped too
(use-package consult-gh
  :ensure (consult-gh :host github :repo "armindarvish/consult-gh" :files ("*.el"))
  :defer t)

(use-package git-auto-commit-mode
  :ensure t
  :defer t)

(use-package github-topics
  :ensure (github-topics :host github :repo "agzam/github-topics")
  :defer t
  :commands (github-topics-find-prs)
  :config
  (setopt github-topics-default-orgs '(qlik-trial stitchdata singer-io))

  (add-to-list
   'display-buffer-alist
   '("\\*Searching GitHub.*"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (init-width . 0.3)
     (window . root))))

(use-package remoto
  :ensure (remoto :host github :repo "agzam/remoto.el" :files ("*.el"))
  :defer t
  :init
  ;; remoto installs a /github: virtual-filesystem handler at load time, which
  ;; is the only reason it needs to load eagerly. Bootstrap a tiny handler that
  ;; pulls remoto in on first /github: (or /gh:) access, then steps aside for
  ;; remoto's own handler - keeping path-opening while deferring the load cost.
  (defun remoto-bootstrap-file-handler (operation &rest args)
    (setq file-name-handler-alist
          (rassq-delete-all #'remoto-bootstrap-file-handler file-name-handler-alist))
    (require 'remoto)
    (apply operation args))
  (unless (rassq #'remoto-bootstrap-file-handler file-name-handler-alist)
    (push (cons (rx bos "/" (or "github" "gh") ":")
                #'remoto-bootstrap-file-handler)
          file-name-handler-alist)))

(use-package browse-at-remote
  :ensure t
  :defer t)

;; direct deps of this module's autoloads (fetch-github-raw-file,
;; forge-visit-topic-via-url); doom.d only got them transitively
(use-package request
  :ensure t
  :defer t)

(use-package deferred
  :ensure t
  :defer t)
