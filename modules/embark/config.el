;;; modules/embark/config.el -*- lexical-binding: t; -*-

;; Ported from doom.d modules/custom/embark; +names de-plussed, link/eww/mpv
;; action references updated to the lab renames (see Decisions log).
;; consult-gh-embark glue stays out - consult-gh is package-only (git module).
;; Doom kept [remap describe-bindings], C-;, SPC a and the minibuffer rows
;; inside embark's :config, where only an already-loaded live session
;; resolves them; here they bind at startup ([remap] + minibuffer rows in
;; :init below; C-;, SPC a and C-c C-s embark-collect live in root
;; config.el - root layer wins on the collect key).

(use-package embark
  :commands (embark-act)
  :defer t
  :init
  (setopt which-key-use-C-h-commands nil
          prefix-help-command #'embark-prefix-help-command)

  (map! [remap describe-bindings] #'embark-bindings
        (:map minibuffer-local-map
              "C-c C-;" #'embark-export
              :desc "Export to writable buffer" "C-c C-e" #'vertico-embark-export-write))

  :config
  ;; also pulls embark-consult in: embark.el requires it once consult loads
  (require 'consult)

  (setopt embark-cycle-key "C-;"
          embark-help-key "M-h"
          embark-confirm-act-all nil
          embark-quit-after-action t)

  (setopt embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  (defadvice! embark-select-next-line-a (orig-fn &rest args)
    "embark-select always moves to the next item upon selection."
    :around #'embark-select
    (apply orig-fn args)
    (when (minibufferp)
      (vertico-next)))

  (defcustom embark-url-config
    '((nil :actions (("b e" . eww-open-in-other-window)
                     ("b o" . browse-url-externally)
                     ("c m" . link-plain->link-markdown)
                     ("c o" . link-plain->link-org-mode)
                     ("RET" . eww-open-in-other-window)))
      ;; youtube-sub-extractor waits on the writing module
      (yt-video
       :pattern "\\(youtube\\.com/watch\\|youtu\\.be/\\)"
       :actions (("b b" . mpv-open)
                 ("RET" . mpv-open)
                 ("b t" . youtube-sub-extractor-extract-subs)))
      (github-repo
       :pattern "github\\.com/[^/]+/[^/]+/?$"
       :actions (("b b" . remoto-browse)
                 ("RET" . remoto-browse)
                 ("b f" . forge-visit-topic-via-url)
                 ("c s" . git-https-url->ssh)
                 ("g c" . magit-clone-simple)))
      (github-pulls
       :pattern "github\\.com/[^/]+/[^/]+/pulls\\(?:\\?.*\\)?$"
       :actions (("b b" . forge-browse-topics)
                 ("RET" . forge-browse-topics)))
      (github-issues
       :pattern "github\\.com/[^/]+/[^/]+/issues\\(?:\\?.*\\)?$"
       :actions (("b b" . forge-browse-topics)
                 ("RET" . forge-browse-topics)))
      (github-pr
       :pattern "github\\.com/[^/]+/[^/]+/pull/[0-9]+"
       :actions (("b b" . forge-visit-topic-via-url)
                 ("RET" . forge-visit-topic-via-url)
                 ("c b" . link->link-bug-reference)
                 ("g c" . magit-clone-simple)))
      (github-issue
       :pattern "github\\.com/[^/]+/[^/]+/issues/[0-9]+"
       :actions (("b b" . forge-visit-topic-via-url)
                 ("RET" . forge-visit-topic-via-url)
                 ("c b" . link->link-bug-reference)
                 ("g c" . magit-clone-simple)))
      (github-file
       :pattern "github\\.com/[^/]+/[^/]+/blob/[^/]+/.+"
       :actions (("b b" . fetch-github-raw-file)
                 ("RET" . fetch-github-raw-file)))
      (github-compare-link
       :pattern "github\\.com/[^/]+/[^/]+/compare/.+"
       :actions ())
      (github-commit
       :pattern "github\\.com/[^/]+/[^/]+/commit/[0-9a-f]+"
       :actions ())
      ;; reddigg + hnreader use-package blocks are commented out in
      ;; web-browsing (broken forks) - rows runtime-void until they return
      (reddit-link
       :pattern "https\\:\\/\\/www.reddit.com\\/.*"
       :actions (("b b" . reddigg-view-comments)
                 ("RET" . reddigg-view-comments)))
      (hackernews-link
       :pattern "https\\:\\/\\/news.ycombinator.com\\/.*"
       :actions (("b b" . hnreader-comment)
                 ("RET" . hnreader-comment)))
      (circle-ci-log
       :pattern "https\\:\\/\\/circleci.com\\/api\\/.*"
       :actions (("b b" . open-circleci-log)
                 ("RET" . open-circleci-log))))
    "Complete url configuration with patterns and actions."
    :type '(alist :key-type (choice (const nil) symbol)
            :value-type plist)
    :group 'embark)

  (embark-setup-url-types)

  ;; vulpea-note: consult-vulpea sets :category 'vulpea-note on candidates,
  ;; but neither it nor vulpea register an embark action for it.
  ;; Without this, embark-collect buffers inherit `vertico-exit' as the
  ;; default action (via embark--command) which crashes outside a minibuffer.
  (setf (alist-get 'vulpea-note embark-default-action-overrides)
        (lambda (candidate)
          (when-let* ((id (get-text-property 0 'vulpea-note-id candidate))
                      (note (vulpea-db-get-by-id id)))
            (vulpea-visit note)
            ;; When coming from vulpea-backlinks, reveal all headings
            ;; containing links to the target note via sparse tree.
            (when (bound-and-true-p vulpea-backlinks--target-id)
              (vulpea-backlinks-sparse-tree
               vulpea-backlinks--target-id)))))

  ;; target finders + keymaps for the custom target types (the url-type
  ;; keymaps come out of `embark-setup-url-types' above)
  (add-to-list 'embark-target-finders 'embark-target-org-block)
  (dolist (finder '(embark-target-markdown-link-at-point
                    embark-target-bug-reference-link-at-point
                    embark-target-RFC-number-at-point))
    (add-to-list 'embark-target-finders finder))

  (defvar-keymap embark-org-block-map
    :doc "Embark actions for org blocks"
    :parent embark-general-map)
  (add-to-list 'embark-keymap-alist '(org-block . embark-org-block-map))

  (defvar-keymap embark-markdown-link-map
    :doc "Keymap for Embark markdown link actions."
    :parent embark-general-map)
  (add-to-list 'embark-keymap-alist '(markdown-link embark-markdown-link-map))

  (defvar-keymap embark-bug-reference-link-map
    :doc "Keymap for Embark bug-reference link actions."
    :parent embark-general-map)
  (add-to-list 'embark-keymap-alist '(bug-reference-link embark-bug-reference-link-map))

  (defvar-keymap embark-rfc-number-map
    :doc "Keymap for Embark RFC number link actions."
    :parent embark-general-map)
  (add-to-list 'embark-keymap-alist '(rfc-number embark-rfc-number-map))

  (map!
   (:map embark-general-map
         "C-<return>" #'embark-dwim
         "m" #'embark-select
         "/" #'embark-project-search)
   ;; doom.d also bound "x p" awesome-switch-to-prev-app-and-type on
   ;; embark-general-map - hammerspoon-era, void even there; dropped

   (:map embark-file-map
         "x" #'embark-open-externally
         "o" nil
         (:prefix ("o" . "open")
                  "j" (embark-split-action find-file window-split-and-follow)
                  "l" (embark-split-action find-file window-vsplit-and-follow)
                  "h" (embark-split-action find-file split-window-horizontally)
                  "k" (embark-split-action find-file split-window-vertically)
                  "a" (embark-ace-action find-file)))

   (:map embark-org-block-map
         (:prefix ("c" . "convert")
                  "c" #'embark-org-block-convert-to-src
                  "e" #'embark-org-block-convert-to-example
                  "q" #'embark-org-block-convert-to-quote))

   (:map embark-command-map
         "h" #'helpful-command)

   (:map
    embark-buffer-map
    "o" nil
    (:prefix ("o" . "open")
             "j" (embark-split-action switch-to-buffer window-split-and-follow)
             "a" (embark-ace-action switch-to-buffer)))

   (:map
    embark-function-map
    "o" nil
    (:prefix ("d" . "definition")
             "j" (embark-split-action embark-find-definition window-split-and-follow)
             "l" (embark-split-action embark-find-definition window-vsplit-and-follow)
             "h" (embark-split-action embark-find-definition split-window-horizontally)
             "k" (embark-split-action embark-find-definition split-window-vertically)
             "a" (embark-ace-action embark-find-definition)))

   (:map
    embark-org-heading-map
    (:prefix ("r" . "roam")
     :desc "add ref" "u" #'roam-ref-add-for-active-tab))

   (:map
    embark-url-map
    "RET" #'eww-open-in-other-window
    (:prefix
     ("b" . "browse")
     :desc "browser" "o" #'browse-url
     :desc "eww" "e" #'eww-open-in-other-window)
    (:prefix
     ("c" . "convert")
     :desc "markdown link" "m" #'link-plain->link-markdown
     :desc "org-mode link" "o" #'link-plain->link-org-mode
     :desc "bug-reference" "b" #'link-plain->link-bug-reference))

   (:map embark-markdown-link-map
         "b" (cmd! (browse-url-externally (markdown-link-url)))
         "v" #'forge-visit-topic-via-url
         (:prefix
          ("c" . "convert")
          :desc "org-mode link" "o" #'link-markdown->link-org-mode
          :desc "plain" "p" #'link-markdown->link-plain
          :desc "strip" "s" #'link-markdown->just-text
          :desc "bug-reference" "b" #'link-markdown->link-bug-reference))

   (:map embark-org-link-map
         "b" #'org-open-at-point
         "V" #'open-link-in-vlc
         "v" #'forge-visit-topic-via-url
         (:prefix
          ("c" . "convert")
          :desc "markdown link" "m" #'link-org->link-markdown
          :desc "plain" "p" #'link-org->link-plain
          :desc "strip" "s" #'link-org->just-text
          :desc "bug-reference" "b" #'link-org->link-bug-reference
          :desc "roam heading" "r" #'link-org->roam-heading))

   (:map embark-bug-reference-link-map
         "v" #'forge-visit-topic-via-url
         (:prefix ("b" . "browse")
          :desc "browser" "o" #'bug-reference-push-button
          :desc "forge-visit" "b" #'forge-visit-topic-via-url)
         (:prefix
          ("c" . "convert")
          :desc "markdown link" "m" #'link-bug-reference->link-markdown
          :desc "org-mode link" "o" #'link-bug-reference->link-org-mode
          :desc "plain" "p" #'link-bug-reference->link-plain))

   (:map embark-rfc-number-map
    :desc "browse" "b" #'browse-rfc-number-at-point)

   (:map
    embark-collect-mode-map
    :n "[" #'embark-previous-symbol
    :n "]" #'embark-next-symbol
    :n "TAB" #'embark-collect-outline-cycle
    :n "m" #'embark-select)

   (:map
    (embark-command-map embark-symbol-map)
    (:after edebug
            (:prefix ("D" . "debug")
                     "f" #'edebug-instrument-symbol
                     "F" #'edebug-remove-instrumentation)))

   (:map embark-region-map
         ;; otherwise, this shit opens another instance of Emacs
         "b" (cmd! (browse-url (buffer-substring-no-properties (region-beginning) (region-end)))))

   (:map
    (embark-identifier-map
     embark-region-map
     embark-sentence-map
     embark-paragraph-map)
    (:prefix
     ("x" . "text")
     (:when (modulep! :custom writing)
       (:prefix ("l" . "language")
        :desc "define" "d" #'define-it-at-point
        :desc "sdcv" "l" #'sdcv-search-pointer
        :desc "Merriam Webster" "m" #'mw-thesaurus-lookup-dwim
        :desc "wiktionary" "w" #'wiktionary-bro-dwim)
       (:prefix ("g" . "translate")
        :desc "en->ru" "e" #'google-translate-query-translate-reverse
        :desc "ru->en" "r" #'google-translate-query-translate
        :desc "es->en" "s" #'google-translate-es->en
        :desc "en->es" "S" #'google-translate-en->es
        :desc "translate" "g" #'google-translate-at-point)))))

  (add-hook! 'embark-collect-mode-hook
    (defun visual-line-mode-off-h ()
      (visual-line-mode -1)))

  ;; don't ask when killing buffers
  (setq embark-pre-action-hooks
        (cl-remove
         '(kill-buffer embark--confirm)
         embark-pre-action-hooks :test #'equal))

  (defadvice! embark-prev-next-recenter-a ()
    :after #'embark-previous-symbol
    :after #'embark-next-symbol
    (recenter)))

(use-package embark-consult
  :defer t)
