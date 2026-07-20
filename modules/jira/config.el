;;; modules/jira/config.el -*- lexical-binding: t; -*-

;; go-jira is an own package (agzam); the plain GitHub recipe is portable,
;; init.el's `local-checkout-recipe' redirects it to ~/GitHub/agzam/go-jira.el
;; when that checkout exists (elpaca then builds in place).  Darwin-only,
;; mirroring doom.d's `(:if (featurep :system 'macos) jira)': the browse
;; autoload drives the macOS browser via JXA (browser-get-tabs/-activate-tab).

(use-package go-jira
  :ensure (go-jira :host github :repo "agzam/go-jira.el" :files ("*.el"))
  ;; go-jira-view-mode derives from org-mode; `:after org' both guarantees the
  ;; parent mode and installs the popup+eldoc hooks below as soon as org loads
  ;; (the package's own autoloads still expose the commands independently).
  :after org
  :config
  (setopt go-jira-default-search-format-string
          "project = SAC AND status NOT IN (Closed, Done) AND text ~ \"%s\""
          ;; doom.d kept these in custom.el (`go-jira-set-default-board'
          ;; customize-saves them); tracked here alongside the SAC search
          ;; string so the default board opens out of the box.  A later
          ;; interactive re-select still wins - custom.el loads last.
          go-jira-default-board-id 3018
          go-jira-default-board-name "SAC Pipeline (Pod 2)")

  ;; doom.d bound go-jira-browse-ticket-mode-map / -browser-ticket-mode-get-url
  ;; here; the package rewrite dropped that mode (browsing now shells out to the
  ;; OS browser via `go-jira-browse-ticket-url'), so that block is gone.

  (setopt go-jira-status-face-alist
          '(("On Hold"     . (:foreground "#adbaba" :weight bold))
            ("In Progress" . (:foreground "#13702a"))
            ("Code Review" . (:foreground "#c678dd"))
            ("Validation"  . (:foreground "#7878dd"))))

  (add-hook! '(org-mode-hook
               markdown-mode-hook
               prog-mode-hook
               text-mode-hook)
             #'go-jira-enable-popup+eldoc)

  ;; Both maps are bound from their mode hooks: go-jira-board-view-mode-map
  ;; lives in go-jira-board.el, which only loads on the first board command,
  ;; so it may not exist yet in `:config'.  The hook fires once the mode (and
  ;; thus its map) is live.
  (add-hook! 'go-jira-view-mode-hook
    (defun go-jira-view-mode-h ()
      (map! :map go-jira-view-mode-map
            :nv "E" #'go-jira-edit
            :nv "q" #'kill-buffer-and-window
            :nv "gr" #'go-jira-view-mode-refresh)))

  (add-hook! 'go-jira-board-view-mode-hook
    (defun go-jira-board-view-mode-h ()
      (map! :map go-jira-board-view-mode-map
            :nv "q" #'kill-buffer-and-window
            :nv "E" #'go-jira-edit
            :nv "gr" #'go-jira-board-refresh)))

  (after! embark
    (map! :map go-jira-embark-jira-ticket-map
          (:prefix ("j" . "jira")
           :desc "change status" "s" #'go-jira-change-status
           :desc "assign"        "a" #'go-jira-assign
           :desc "PRs"           "p" #'go-jira-find-pull-requests-on-github)
          (:prefix ("b" . "browse")
           :desc "view" "b" #'go-jira-view-ticket
           :desc "in browser" "o" #'go-jira-browse-ticket-url)
          (:prefix ("f" . "find")
           :desc "GH PRs"        "g" #'go-jira-find-pull-requests-on-github
           :desc "Slack Threads" "s" #'go-jira-search-slack-threads)
          (:prefix ("c" . "convert")
           :desc "link" "l" #'go-jira-ticket->link
           :desc "link+desc" "d" #'go-jira-ticket->num+description
           :desc "git branch" "g" #'go-jira-ticket->git-branch-name))))
