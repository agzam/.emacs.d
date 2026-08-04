;;; modules/org/config.el -*- lexical-binding: t; -*-

;; doom.d's org module ported to vanilla: org, org-roam(+ui), the vulpea trio
;; + consult-vulpea, evil-org, org-modern-indent, org-appear, org-superstar,
;; org-cliplink, plus the former long tail - anki-editor(+ui, +anki-gen),
;; ob-async/http/mermaid, org-edit-indirect, org-pomodoro (+mp3s), toc-org,
;; orgit(-forge), ox-clip/ox-gfm, org-download.  org-roam/vulpea db paths sit
;; inline with their packages (own dbs under doom-local-dir).  Still parked in
;; doom.d (see MIGRATION.org): verb, org-contrib.

;; org builds via elpaca's own menu (elpaca-menu-org generates
;; org-version/loaddefs) - doom.d's straight :pre-build fake is obsolete.
;; The manual needs makeinfo (absent on CI); Doom never built it either.
(setq elpaca-menu-org-make-manual nil)

(defvar org-default-folder (expand-file-name "~/Sync/org/"))

(use-package org
  :defer t
  :init
  ;; Trim org-modules to the link backends actually used. The default list
  ;; pulls ol-gnus/ol-rmail/ol-mhe/ol-bbdb/ol-w3m/ol-irc; ol-gnus alone drags
  ;; the entire gnus stack in whenever org loads.
  (setq org-modules '(ol-eww ol-info ol-docview))
  :config
  (setopt org-directory org-default-folder)
  (setopt
   org-ctrl-k-protect-subtree t
   org-ellipsis " ↴"
   org-fold-catch-invisible-edits 'smart
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil
   org-log-into-drawer t
   org-log-states-order-reversed nil
   org-cycle-emulate-tab nil
   org-edit-src-content-indentation 0
   org-fontify-quote-and-verse-blocks t)
  ;; plain setq: org 10's defcustom type rejects fractional widths that
  ;; org-display-inline-image--width still supports
  (setq org-image-actual-width '(0.7))

  (add-to-list
   'auto-mode-alist
   `(,(format "\\%s.*\.txt\\'" (replace-regexp-in-string "~" "" org-default-folder)) . org-mode))

  (setopt
   org-confirm-babel-evaluate nil
   org-todo-keywords '((sequence "TODO(t!)" "ONGOING(o!)" "REVIEW" "|" "DONE(d!)" "CANCELED(c@/!)"))
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t)

  (setopt org-link-make-description-function #'org-link-make-description-fn)

  ;; Better context for sparse-tree/occur matches — 'lineage reveals
  ;; the full ancestor chain *and* sibling headings at each level,
  ;; so backlink sparse trees show surrounding structure for orientation.
  (add-to-list 'org-fold-show-context-detail '(occur-tree . lineage))

  ;; doom.d rebuilt these on every org-mode-hook run (org-init-keybinds-h);
  ;; once after load is enough.  Deviations: the vertico guard is gone (the
  ;; lab registry says :custom completion); org-noter ("n") is autoloaded from
  ;; the pdf module (its owner).  org-cliplink "i L" restored with its 2026-07
  ;; pull-forward.
  (map! :map org-mode-map
        :n "[[" #'org-previous-visible-heading
        :n "]]" #'org-next-visible-heading
        [remap imenu] #'consult-outline
        "C-c C-f f" #'vulpea-find
        "C-c C-i" #'vulpea-insert
        :n "zk" #'text-scale-increase

        ;; tilde instead of backtick
        :iv "`" (cmd! (self-insert-command 1 126))

        (:localleader
         "." #'consult-org-heading
         (:prefix ("b" . "babel")
                  "k" #'org-babel-remove-result)
         (:prefix ("d" . "date")
                  "t" #'org-goto-datetree-date)
         (:prefix ("g" . "goto")
          :desc "final heading" "L" #'org-goto-bottommost-heading)
         (:prefix ("i" . "insert")
                  "l" #'org-insert-link
                  "L" #'org-cliplink
                  "c" #'yank-from-clipboard)
         (:prefix ("l" . "links")
                  "i" #'org-id-store-link
                  "n" #'org-next-link
                  "p" #'org-previous-link
                  "x" #'org-remove-link-at-point)
         ;; org-noter-transient is autoloaded from the pdf module (its owner).
         "n" #'org-noter-transient
         (:prefix ("o" . "open/Org")
                  "l" #'org-id-store-link
                  "L" #'org-store-link-id-optional)
         (:prefix ("r" . "roam")
          "b" #'vulpea-backlinks
          "i" #'vulpea-insert
          "l" #'vulpea-ui-sidebar-toggle
          :desc "org-roam-ui in xwidget" "w" #'org-roam-toggle-ui-xwidget
          :desc "org-roam-ui in browser" "W" #'org-roam-ui-in-browser
          "f" #'vulpea-find
          "F" #'vulpea-forward-links
          :desc "work note" "n" (cmd! (open-journal 'work (org-read-date nil t)))
          :desc "personal note" "N" (cmd! (open-journal 'personal (org-read-date nil t)))
          (:prefix ("r" . "refile")
                   "n" #'org-roam-refile-to-node))
         (:prefix ("s" . "tree/subtree")
                  "a" #'org-toggle-archive-tag
                  "A" #'org-archive-subtree
                  "j" #'consult-org-heading
                  "n" #'org-narrow-to-subtree
                  "N" #'widen
                  "S" #'org-sort
                  "x" #'org-cut-subtree)
         (:prefix ("t" . "toggle")
                  "l" #'org-toggle-link-display)))

  (map! :map org-agenda-mode-map
        :n "RET" #'org-agenda-switch-to)

  ;; bug-reference-mode's org hook lives in the git module
  (add-hook!
   'org-mode-hook
   #'org-indent-mode
   #'yas-minor-mode-on
   #'org-roam-count-overlay-mode
   ;; fboundp: flycheck waits on :checkers - unguarded, the void call
   ;; aborted the rest of org-mode-hook (live probe caught it)
   (defun flycheck-disable-h ()
     (when (fboundp 'flycheck-mode) (flycheck-mode -1))))

  (add-hook! 'org-capture-mode-hook #'recenter)

  (setopt org-export-with-smart-quotes nil
          ;; "" not nil: the defcustom type is string (setopt warns on nil);
          ;; the postamble %v spec renders either as nothing
          org-html-validation-link ""
          org-latex-prefer-user-labels t
          org-ascii-text-width 900 ; don't wrap text
          org-ascii-links-to-notes nil)
  (add-to-list 'org-export-backends 'md)

  (setopt org-capture-bookmark nil)

  (after! org-attach
    (add-hook! 'org-attach-after-change-hook
      (defun org-attach-save-file-list-to-property (dir)
        (when-let* ((files (org-attach-file-list dir)))
          (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))))

    (advice-add 'org--image-yank-media-handler
                :around #'yank-media--tiff-as-png-a))

  ;; all of these ship inside org itself
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (js . t)
     (python . t)
     (clojure . t)
     (sql . t)
     (sqlite . t)))

  ;; youtube videos played in mpv (web-browsing); plain browser when
  ;; that module is off
  (org-link-set-parameters
   "yt" :follow (lambda (path)
                  (let ((url (concat "https:" path)))
                    (if (fboundp 'mpv-open) (mpv-open url) (browse-url url))))
   :export (lambda (link _desc _format)
             (format
              (concat
               "<iframe width=\"560\" height=\"315\" src=\"https:%s\" title=\"YouTube video player\" frameborder=\"0\" "
               "allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" "
               "referrerpolicy=\"strict-origin-when-cross-origin\" allowfullscreen></iframe>")
              link)))

  (setf (alist-get "c" org-structure-template-alist nil nil #'string=) "src")

  (setopt org-capture-templates
          `(("Q" "quote" entry
             (file ,(concat org-directory "quotes.org"))
             "* %c %?\n:PROPERTIES:\n:ID: %(org-id-new) \n:END:"
             :jump-to-captured t)
            ("p" "person" entry
             (file ,(concat org-directory "people.org"))
             "* %(person-w-name-based-id)\n%?"
             :jump-to-captured t)
            ("c" "colleague" entry
             (file ,(concat org-directory "coworkers.org"))
             "* %(person-w-name-based-id)\n%?"
             :jump-to-captured t))))

(use-package org-tempo
  :ensure nil  ; ships inside org
  :after org
  :config
  (add-to-list 'org-modules 'org-tempo t))

(use-package org-roam
  ;; extensions flattened into the build root (MELPA-style) - doom.d's manual
  ;; extensions/ load-path hack dissolves.  org-roam-ui hard-requires
  ;; org-roam-dailies at load, so they must be reachable.
  :ensure (org-roam :host github :repo "org-roam/org-roam"
                    :files (:defaults "extensions/*"))
  :after org
  :init
  (setopt
   org-roam-v2-ack t
   org-roam-directory org-default-folder
   ;; own db, kept out of user-emacs-directory and off the live Doom session's
   org-roam-db-location (concat doom-local-dir "org-roam.db")
   org-roam-dailies-directory "daily/" ; kept for org-roam-ui graph rendering

   ;; org-mode doesn't know how to properly export with roam links
   org-export-with-broken-links t
   org-roam-file-exclude-regexp '("data/" ".sync/"))
  :config
  (after! xwidget
    (map! :localleader :map xwidget-webkit-mode-map
          (:prefix ("r" . "roam")
                   "w" #'org-roam-toggle-ui-xwidget)))

  (setopt
   org-roam-completion-everywhere nil
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setopt org-roam-capture-templates
          `(("d" "default" plain
             "%?"
             :if-new
             (file+head
              "${slug}.org"
              "\n#+title: ${title}\n#+startup: overview\n\n")
             :unnarrowed t
             :jump-to-captured t)))

  (add-to-list 'org-default-properties "roam_aliases")
  (add-to-list 'org-default-properties "roam_refs"))

(use-package org-roam-ui
  :ensure (org-roam-ui :host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :after org-roam
  :init
  (setq org-roam-ui-port 8088
        org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*org-roam-ui\\*"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.4)))

  (add-hook! 'org-mode-hook
    (defun org-roam-ui-on ()
      (unless org-roam-ui-mode
        ;; demoted: while the Doom trial runs in parallel, its session may
        ;; already hold port 8088
        (with-demoted-errors "org-roam-ui: %S"
          (org-roam-ui-mode +1))))))

(use-package evil-org
  :ensure (evil-org :host github :repo "hlissner/evil-org-mode")
  :after org
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)

  ;; magit-blame fights evil-org bindings in org buffers.  Registered here,
  ;; not in the git module (its doom.d home): the hook can only matter once
  ;; evil-org exists, and this keeps blame in non-org buffers from tripping
  ;; over a void function.
  (add-hook! 'magit-blame-mode-hook
    (defun turn-off-evil-org-h ()
      (evil-org-mode -1)))

  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             #'org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'org-clear-babel-results-h)
  (let-alist evil-org-movement-bindings
    (let ((Cright  (concat "C-" .right))
          (Cleft   (concat "C-" .left))
          (Cup     (concat "C-" .up))
          (Cdown   (concat "C-" .down))
          (CSright (concat "C-S-" .right))
          (CSleft  (concat "C-S-" .left))
          (CSup    (concat "C-S-" .up))
          (CSdown  (concat "C-S-" .down)))
      (map! :map evil-org-mode-map
            :ni [C-return]   #'org-insert-item-below
            :ni [C-S-return] #'org-insert-item-above
            ;; navigate table cells (from insert-mode)
            :i Cright (cmds! (org-at-table-p) #'org-table-next-field
                             #'recenter-top-bottom)
            :i Cleft  (cmds! (org-at-table-p) #'org-table-previous-field)
            :i Cdown  (cmds! (org-at-table-p) #'org-table-next-row
                             #'org-down-element)
            :ni CSright   #'org-shiftright
            :ni CSleft    #'org-shiftleft
            :ni CSup      #'org-shiftup
            :ni CSdown    #'org-shiftdown
            ;; more intuitive RET keybinds
            :n [return]   #'org-dwim-at-point
            :n "RET"      #'org-dwim-at-point
            :i [S-return] #'org-shift-return
            :i "S-RET"    #'org-shift-return
            ;; more vim-esque org motion keys (not covered by evil-org-mode)
            :m "]h"  #'org-forward-heading-same-level
            :m "[h"  #'org-backward-heading-same-level
            :m "]l"  #'org-next-link
            :m "[l"  #'org-previous-link
            :m "]c"  #'org-babel-next-src-block
            :m "[c"  #'org-babel-previous-src-block
            :n "gQ"  #'org-fill-paragraph
            ;; sensible vim-esque folding keybinds
            :n "za"  #'org-toggle-fold
            :n "zA"  #'org-shifttab
            :n "zc"  #'org-close-fold
            :n "zC"  #'outline-hide-subtree
            :n "zm"  #'org-hide-next-fold-level
            :n "zM"  #'org-close-all-folds
            :n "zn"  #'org-tree-to-indirect-buffer
            :n "zo"  #'org-open-fold
            :n "zO"  #'outline-show-subtree
            :n "zr"  #'org-show-next-fold-level
            :n "zR"  #'org-open-all-folds
            :n "zi"  #'org-link-preview

            :map org-read-date-minibuffer-local-map
            Cleft    (cmd! (org-funcall-in-calendar '(calendar-backward-day 1)))
            Cright   (cmd! (org-funcall-in-calendar '(calendar-forward-day 1)))
            Cup      (cmd! (org-funcall-in-calendar '(calendar-backward-week 1)))
            Cdown    (cmd! (org-funcall-in-calendar '(calendar-forward-week 1)))
            CSleft   (cmd! (org-funcall-in-calendar '(calendar-backward-month 1)))
            CSright  (cmd! (org-funcall-in-calendar '(calendar-forward-month 1)))
            CSup     (cmd! (org-funcall-in-calendar '(calendar-backward-year 1)))
            CSdown   (cmd! (org-funcall-in-calendar '(calendar-forward-year 1)))))))

(use-package org-appear
  :ensure (org-appear :host github :repo "awth13/org-appear" :branch "org-9.7-fixes")
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setopt org-appear-delay 1
          org-appear-autolinks t
          org-appear-autoemphasis t
          org-appear-autosubmarkers t))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setopt org-superstar-leading-bullet ?\s
          org-superstar-leading-fallback ?\s
          org-hide-leading-stars nil
          org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                            ("[ ]"  . 9744)
                                            ("DONE" . 9745)
                                            ("[X]"  . 9745))
          org-superstar-item-bullet-alist '((?* . ?⋆)
                                            (?+ . ?◦)
                                            (?- . ?•))))

;; Pulled ahead of the org long tail (2026-07): five title-retrieval sites
;; in general/autoload/url.el and web-browsing's org branch call into it.
;; :commands covers org-cliplink-retrieve-title-synchronously explicitly -
;; upstream only cookies the interactive commands, and the url.el helpers
;; call the retriever without requiring the feature first.
(use-package org-cliplink
  :commands (org-cliplink
             org-cliplink-capture
             org-cliplink-retrieve-title-synchronously))

(use-package org-modern-indent
  :ensure (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  :defer t
  :hook (org-mode . org-modern-indent-mode))

;; consult search commands won't reveal the org fold context
;; see: minad/consult#563
(after! consult
  (defadvice! evil-ex-store-pattern-a (_ _)
    "Remember the search pattern after consult-line."
    :after #'consult-line
    (setopt evil-ex-search-pattern
            (list (car consult--line-history) t t)))

  (defadvice! org-show-entry-consult-a (fn &rest args)
    :around #'consult-line
    :around #'consult-org-heading
    :around #'consult--grep
    :around #'compile-goto-error
    (when-let* ((pos (apply fn args)))
      (when (derived-mode-p 'org-mode)
        (org-fold-show-entry)))))

(use-package vulpea
  :ensure (vulpea :host github :repo "d12frosted/vulpea")
  ;; vulpea-journal-note isn't autoloaded upstream: loading vulpea chains
  ;; vulpea-ui -> vulpea-journal through the :after gates below, which
  ;; defines it before the autoload machinery re-dispatches.
  :commands (open-journal vulpea-journal-note)
  :config
  (setopt vulpea-db-sync-directories (list org-default-folder)
          vulpea-buffer-alias-property "ROAM_ALIASES"
          vulpea-db-parse-method 'single-temp-buffer
          vulpea-db-sync-scan-on-enable 'async
          ;; own db, kept out of user-emacs-directory and off the live Doom's
          vulpea-db-location (concat doom-local-dir "vulpea.db"))

  ;; Hide journal file-level nodes ("April 2026 personal notes") from
  ;; vulpea-find/vulpea-insert. All headings inside journal files
  ;; (day entries, content nodes) remain findable.
  (setopt vulpea-find-default-filter
          (lambda (note)
            (not (and (seq-intersection (vulpea-note-tags note)
                                        '("work-notes" "personal-notes"))
                      (= (vulpea-note-level note) 0)))))
  (setopt vulpea-insert-default-filter
          (lambda (note)
            (not (and (seq-intersection (vulpea-note-tags note)
                                        '("work-notes" "personal-notes"))
                      (= (vulpea-note-level note) 0)))))
  (map! :map org-mode-map
        :i "[[" #'vulpea-insert
        :i "[ SPC" #'insert-bracket-pair)

  (vulpea-db-autosync-mode +1)

  (add-hook 'before-save-hook #'org-drawer-lint-before-save-h)

  ;; doom.d wrapped regexp-quoted strings in rx literals, so the entry
  ;; matched literal backslashes and never fired - fixed on port
  (add-to-list
   'display-buffer-alist
   `(,(rx bos (or "*org-roam*" "*vulpea-ui-sidebar"))
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package vulpea-ui
  :ensure (vulpea-ui :host github :repo "d12frosted/vulpea-ui")
  :after vulpea
  :config
  (setopt vulpea-ui-backlinks-show-preview t
          vulpea-ui-outline-max-depth 1
          vulpea-ui-default-widget-collapsed nil
          vulpea-ui-sidebar-auto-hide nil)
  (map! :map vulpea-ui-sidebar-mode-map
        (:localleader
         (:prefix ("r" . "roam")
                  "l" #'vulpea-ui-sidebar-toggle))))

(use-package vulpea-journal
  :ensure (vulpea-journal :host github :repo "d12frosted/vulpea-journal")
  :after (vulpea vulpea-ui)
  :config
  (setopt vulpea-directory org-default-folder
          vulpea-journal-default-template #'journal-template)

  ;; Before vulpea-journal runs: sync buffer type → global,
  ;; so navigation stays within the same journal type.
  ;; Detects type from filetags if the buffer-local var isn't set.
  (defadvice! vulpea-journal-sync-type-a (&optional _date)
    :before #'vulpea-journal
    (when-let* ((type (or vulpea-journal--buffer-type
                          (vulpea-journal--detect-buffer-type))))
      (setq vulpea-journal--type type)))

  ;; After vulpea-journal visits: propagate global type → buffer-local
  ;; in the destination buffer.
  (defadvice! vulpea-journal-propagate-type-a (&optional _date)
    :after #'vulpea-journal
    (setq-local vulpea-journal--buffer-type vulpea-journal--type))

  ;; Sidebar calendar clicks go through vulpea-journal-ui--visit-date,
  ;; NOT vulpea-journal.  Derive type from the sidebar's own note.
  (defadvice! vulpea-journal-ui-visit-detect-type-a (_date)
    :before #'vulpea-journal-ui--visit-date
    (when-let* ((note (and (boundp 'vulpea-ui--current-note)
                           vulpea-ui--current-note))
                (type (vulpea-journal--type-from-note note)))
      (setq vulpea-journal--type type)))

  ;; vulpea-journal--get-tag resolves the template to find the journal
  ;; tag, but it runs in sidebar context where the type is unknown.
  ;; Derive from the sidebar's current note instead.
  (defadvice! vulpea-journal-get-tag-context-a (orig-fn)
    :around #'vulpea-journal--get-tag
    (let* ((vulpea-journal--type
            (or vulpea-journal--buffer-type
                (vulpea-journal--detect-buffer-type)
                (when (boundp 'vulpea-ui--current-note)
                  (vulpea-journal--type-from-note vulpea-ui--current-note))
                vulpea-journal--type)))
      (funcall orig-fn)))

  ;; Show journal widgets for both work and personal notes.
  ;; The default predicate only matches one tag at a time.
  (defadvice! vulpea-journal-ui-view-p-a (note)
    :override #'vulpea-journal-ui--journal-view-p
    (or (and note
             (seq-intersection (vulpea-note-tags note)
                               '("work-notes" "personal-notes")))
        (vulpea-journal-ui--get-active-date)))

  ;; Insert journal entries in chronological order, not at the end.
  ;; The upstream always uses :after 'last; we find the right sibling
  ;; by comparing CREATED dates so entries stay sorted.
  (defadvice! vulpea-journal-create-sorted-a (date tpl)
    :override #'vulpea-journal--create-heading-note
    (let* ((file (vulpea-journal--file-for-date date))
           (entry-title (vulpea-journal--entry-title-for-date date))
           (date-str (format-time-string "[%Y-%m-%d]" date))
           (entry-level (plist-get tpl :entry-level))
           (container (vulpea-journal--ensure-container file date tpl))
           ;; find existing siblings at the entry level
           (siblings (vulpea-journal--query-file-notes file entry-level))
           ;; last sibling whose CREATED is before our date
           (predecessor
            (seq-find
             (lambda (it)
               (let ((created (cdr (assoc "CREATED" (vulpea-note-properties it)))))
                 (and created (string< created date-str))))
             (reverse siblings))))
      (vulpea-create
       entry-title
       nil
       :parent container
       :properties `(("CREATED" . ,date-str))
       :after (if predecessor
                  (vulpea-note-id predecessor)
                nil))))

  (vulpea-journal-setup))

(use-package consult-vulpea
  :ensure (consult-vulpea :host github :repo "fabcontigiani/consult-vulpea")
  :after (consult vulpea)
  :config
  (consult-vulpea-mode 1))

;;; former long tail - ported 2026-07

(use-package anki-editor
  ;; the fork that ships anki-editor-ui.el; louietan's original doesn't
  :ensure (anki-editor :host github :repo "anki-editor/anki-editor")
  :commands (anki-editor-mode anki-editor-push-notes)
  :config
  (setopt anki-editor-create-decks t      ; create a deck if it doesn't exist
          anki-editor-org-tags-as-anki-tags t)

  (defvar anki-editor-mode-map (make-sparse-keymap))
  (map! :map anki-editor-mode-map
        :localleader
        (:prefix ("a" . "anki")
                 "p" (cmd! (anki-editor-push-notes 'tree))))
  (add-to-list 'minor-mode-map-alist '(anki-editor-mode anki-editor-mode-map)))

(use-package anki-editor-ui
  :ensure nil  ; ships inside anki-editor
  :after anki-editor)

(use-package anki-gen
  :ensure (anki-gen :host github :repo "agzam/anki-gen.el")
  :after org)

;; doom.d installed ob-async but never required it (dead); :after org loads it
;; so ":async" header-args actually dispatch
(use-package ob-async
  :after org)

(use-package ob-http
  :after org
  :commands org-babel-execute:http)

(use-package ob-mermaid
  :after org
  :config
  ;; needs the mermaid-js/mermaid-cli "mmdc" binary
  (when-let* ((mmdc (executable-find "mmdc")))
    (setopt ob-mermaid-cli-path mmdc)))

(use-package org-edit-indirect
  :ensure (org-edit-indirect :host github :repo "agzam/org-edit-indirect.el")
  :defer t
  :hook (org-mode . org-edit-indirect-mode))
;; doom.d set edit-indirect-guess-mode-function to edit-indirect-guess-mode-fn+,
;; which was never defined anywhere - dropped rather than port a void ref.

(use-package org-pomodoro
  :after org
  :config
  ;; C-x p p (doom.d's global bind) now belongs to popup-other; pomodoro is
  ;; M-x / embark reachable.  Menu-bar clock indicator dropped.
  (setopt org-pomodoro-start-sound-p t
          org-pomodoro-killed-sound-p t
          org-pomodoro-audio-player (format "%s -volume 50" (executable-find "mplayer"))
          org-pomodoro-start-sound
          (expand-file-name "modules/org/pomodoro__race-start.mp3" user-emacs-directory)
          org-pomodoro-short-break-sound
          (expand-file-name "modules/org/pomodoro__break-over.mp3" user-emacs-directory)))

(use-package toc-org
  :after org
  :config
  (setopt toc-org-hrefify-default "gh"))

;; org<->magit link types; need both stacks loaded (magit/forge own the git
;; module).  orgit-forge back-fills the forge deferral.
(use-package orgit
  :after (org magit))

(use-package orgit-forge
  :after (orgit forge))

(use-package ox-gfm
  :after org
  :config
  (setopt org-export-with-toc nil))

;; consumer: org-export-to-clipboard-as-rich-text (org/autoload/org-export.el)
(use-package ox-clip
  :commands (ox-clip-formatted-copy ox-clip-image-to-clipboard))

;; consumer: org-attach-file-and-insert-link (org/autoload/org-attach.el);
;; its own autoloads (org-download-clipboard/-screenshot) stay reachable
(use-package org-download
  :defer t)

;; El Khalendario - explicit Org <-> Google Calendar sync.  Placement is
;; pluggable and left to the consumer; flat-daily files pulled events
;; under level-1 "YYYY-MM-DD Weekday" headings - the exact shape
;; journal-template gives daily notes - so events land under their day.
;; A per-file #+GCAL_PLACEMENT:/#+GCAL_CALENDAR: still overrides this.
(use-package khalendario
  :ensure (khalendario :host github :repo "agzam/khalendario.el")
  :defer t
  :init
  (setq khalendario-placement-strategy #'khalendario-placement-flat-daily)
  (map! :map org-mode-map
        :localleader
        (:prefix ("k" . "khalendario")
         :desc "pull events" "f" #'khalendario-pull
         :desc "push entry/region" "p" #'khalendario-push
         :desc "push buffer" "b" #'khalendario-push-buffer
         :desc "sync file" "s" #'khalendario-sync
         :desc "delete at point" "d" #'khalendario-delete-at-point
         :desc "verify auth" "v" #'khalendario-verify-auth)))
