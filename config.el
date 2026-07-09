;;; config.el --- user config layer -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/config.el.  Loaded LAST (after all modules), mirroring
;; Doom's user-config ordering, so these bindings and settings win.
;; TODO markers note the pieces awaiting their module ports:
;; theme (colors), doom-font machinery, sexp-transient (smartparens/avy/
;; edit-indirect), undo-fu.
;;; Code:

(setq user-full-name "Ag Ibragimov"
      user-mail-address "agzam.ibragimov@gmail.com")

;; TODO: (load! "lisp/sexp-transient") - hard-requires smartparens, avy and
;; edit-indirect at load; restore once those port.  SPC k is void until then.

;; TODO: (setopt doom-theme 'ag-themes-spacemacs-light) - ag-themes live in
;; the colors module; default theme until it ports.

(defun tty-bar-cursor-h ()
  "Thin cursor for insert state in the terminal."
  (send-string-to-terminal "\033[5 q"))

(defun tty-default-cursor-h ()
  "Restore the terminal's default cursor shape."
  (send-string-to-terminal "\033[0 q"))

(unless (display-graphic-p)
  (add-hook 'evil-insert-state-entry-hook #'tty-bar-cursor-h)
  (add-hook 'evil-normal-state-entry-hook #'tty-default-cursor-h))

;; TODO: Doom's font machinery (doom-font & friends) isn't vendored; set the
;; faces directly until the UI port.  Original:
;;   doom-font (font-spec :family "Fira Code" :size 16)
;;   doom-variable-pitch-font (font-spec :family "Noto Sans" :size 18)
(push '(font . "Fira Code-16") default-frame-alist)
(custom-set-faces
 '(variable-pitch ((t (:family "Noto Sans" :height 1.125)))))

(defun fonts-relative-height-h (&rest _)
  "Restore reverted PR doomemacs/doomemacs#8733 locally.
Converts baked-in absolute `:height' of `fixed-pitch' and siblings
into ratios so `text-scale-mode' and `doom/increase-font-size'
cascade into `:inherit fixed-pitch' faces (e.g. `org-block').
Unsafe with global `variable-pitch-mode'; see issue #8756."
  (dolist (frame (frame-list))
    (when (display-multi-font-p frame)
      (let ((dh (face-attribute 'default :height frame)))
        (when (and (integerp dh) (< 0 dh))
          (dolist (face '(fixed-pitch fixed-pitch-serif variable-pitch))
            (let ((fh (face-attribute face :height frame)))
              (when (integerp fh)
                (set-face-attribute face frame :height
                                    (/ (float fh) dh))))))))))

(add-hook 'after-setting-font-hook #'fonts-relative-height-h)
(add-hook 'enable-theme-functions  #'fonts-relative-height-h)

(setopt display-line-numbers-type t)
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)

(setq-default
 line-spacing 0.35
 garbage-collection-messages nil
 left-fringe-width 6
 right-fringe-width 0
 messages-buffer-max-lines 10000
 fill-column 70)

(setopt
 scroll-margin 1
 default-input-method "russian-computer"
 tab-width 4
 apropos-sort-by-scores t
 split-width-threshold 160
 split-height-threshold 80
 switch-to-buffer-obey-display-actions t
 mouse-autoselect-window t
 other-window-scroll-default #'get-lru-window

 ;; per https://github.com/emacs-lsp/lsp-mode#performance
 read-process-output-max (* 10 1024 1024) ;; 10mb
 gc-cons-threshold 200000000)

(after! man
  ;; open man pages in the same window
  (setq Man-notify-method 'pushy))

(after! dumb-jump
  ;; https://github.com/jacktasia/dumb-jump#emacs-options
  (setq dumb-jump-force-searcher 'rg))

(after! which-key
  (setopt
   which-key-use-C-h-commands nil
   which-key-show-early-on-C-h t
   which-key-idle-delay 0.5
   which-key-idle-secondary-delay 0.2
   which-key-show-prefix 'echo)

  ;; replace 'evil-' in which-key HUD with a tiny triangle
  ;; borrowed from https://tecosaur.github.io/emacs-config/config.html
  (setopt which-key-allow-multiple-replacements t)
  (after! which-key
    (dolist (r '((("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
                 (("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))
      (add-to-list 'which-key-replacement-alist r)))

  (which-key-mode))

(when (modulep! :custom general)
  (add-hook! 'window-setup-hook
    (defun position-frame-on-load-h ()
      ;; Emacs 29 changed font for the modeline
      ;; https://github.com/hlissner/doom-emacs/issues/5891#issuecomment-992758572
      ;; (custom-set-faces! rewritten: Doom's themes lib isn't vendored)
      (custom-set-faces '(mode-line-active ((t (:inherit mode-line)))))

      (init-visual-line-keys)
      (fringe-mode '(6 . 0)))))

(after! custom
  ;; in customize dialogs keep the elisp names
  (setopt custom-unlispify-tag-names nil))

(add-hook! 'next-error-hook #'recenter)

;; (Two advice-remove calls neutralizing Doom's comment-continuation advice
;; are gone: those advices don't exist here.)

(defalias 'elisp-mode 'emacs-lisp-mode)
(defalias 'clj-mode 'clojure-mode)

;; disable global-hl-line
;; oddly, that's the way: https://github.com/hlissner/doom-emacs/issues/4206
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(after! evil
  (setopt evil-jumps-cross-buffers t
          evil-move-cursor-back nil
          evil-want-fine-undo t
          evil-escape-key-sequence "kj"

          ;; This is a delay between escape and another key in things
          ;; like ESC x = M-x, ESC f = M-f, ESC b = M-b.
          ;; I don't care about it since I'm almost always use Evil-mode
          evil-esc-delay 0

          ;; this one is a delay between evil-escape-key-sequence keys
          ;; - used for jumping to normal state from insert
          evil-escape-delay 0.1)

  (map! :map 'evil-visual-state-map "u" #'undo)

  (defadvice! fwd-o-bkwd-paragraph-o-heading-recenter-a (&rest _args)
    :after #'org-forward-paragraph
    :after #'org-backward-paragraph
    :after #'evil-forward-paragraph
    :after #'evil-backward-paragraph
    :after #'org-previous-visible-heading
    :after #'org-next-visible-heading
    :after #'forward-paragraph
    :after #'backward-paragraph
    (when (called-interactively-p 'any)
      (recenter)
      (let ((face (make-face (gensym "pulse-"))))
        (set-face-background face "LightGreen")
        (pulse-momentary-highlight-one-line (point) face)))))

(after! better-jumper
  (setopt better-jumper-context 'window))

(after! time
  (setopt world-clock-list
          '(("America/Los_Angeles" "Pacific")
            ("America/Chicago" "Central")
            ("America/New_York" "Eastern")
            ("America/Sao_Paulo" "São Paulo")
            ("Europe/Paris" "Paris")
            ("Europe/Copenhagen" "Copenhagen")
            ("Europe/Istanbul" "Istanbul")
            ("Europe/Kiev" "Kiev")
            ("Europe/Moscow" "Moscow")
            ("Asia/Tashkent" "Tashkent")
            ("Asia/Shanghai" "Shanghai")
            ("Asia/Seoul" "Seoul"))))

(after! flycheck
  (ignore-errors
    (define-key flycheck-mode-map flycheck-keymap-prefix nil))
  (setq flycheck-keymap-prefix nil)

  (global-flycheck-mode -1)  ; I don't know why Doom enables is by default

  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package)
  (setopt flycheck-temp-prefix "/tmp/.flycheck"))

(after! grep
  (setopt grep-program "rg"))

(add-hook! 'prog-mode-hook
           #'hs-minor-mode
           #'visual-line-mode)

(after! writeroom-mode
  (setq writeroom-maximize-window t))

(after! general
  (general-auto-unbind-keys))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

(define-key! :keymaps default-minibuffer-maps
  "C-u" #'universal-argument)

;; needed additional binding, because can't emit backslash from Hammerspoon
(map! "C-<f12>" #'toggle-input-method)

(map! :map grep-mode-map
      :n "q" #'kill-buffer-and-window
      :n "[" #'compilation-previous-file
      :n "]" #'compilation-next-file
      (:localleader
       "f" #'next-error-follow-minor-mode))

;; disable nonsensical keys
(dolist (key '("s-n" "s-p" "s-q" "s-m" "s-," "s-h"
               "C-x C-c"
               "C-<tab>" "C-S-<tab>" "<f11>"
               "M-k" "M-j"))
  (global-set-key (kbd key) nil))

;;; Globals
(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp
      :v "s" #'evil-surround-region
      "s-b" #'consult-buffer
      "s-=" #'text-scale-increase
      "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter))
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter))
      :n "zk" #'text-scale-increase
      :n "zj" #'text-scale-decrease
      :n "z0" #'text-scale-set
      :n "z ;" "za"
      :n "s-e" #'scroll-line-down-other-window
      :n "s-y" #'scroll-line-up-other-window
      :n "s-u" #'scroll-line-down-other-window
      :nv "C-e" (cmd! () (ultra-scroll-down 45))
      :nv "C-y" (cmd! () (ultra-scroll-up 45))
      :i "M-/" #'completion-preview-next-candidate
      :i "M-?" #'completion-preview-prev-candidate
      :i "M-l" #'completion-preview-accept-or-slurp
      :i "C-/" #'completion-at-point
      :n "gi" #'ibuffer-sidebar-jump
      :i "C-v" #'evil-paste-after
      :i "TAB" #'completion-at-point
      "C-x m" #'mpv-transient
      "C-;"  #'embark-act
      (:when (featurep :system 'linux)
        :i "C-M-S-s-y" #'nerd-dictation-toggle)
      (:when (modulep! :custom ai)
        (:prefix ("C-x g" . "gptel")
         :desc "gptel-menu" "g" #'gptel-menu
         :desc "new gptel" "n" #'gptel+
         :desc "check text" "e" #'gptel-improve-text-transient
         :desc "quick" "q" #'gptel-quick-question-buffer
         "m" #'gptel-mode
         "s" #'gptel-send
         "c" #'eca)))

(map! (:map (prog-mode-map text-mode-map markdown-mode-map)
       :desc "external browser" "C-c C-o"
       (cmd!
        (let ((git-link-extensions-rendered-plain nil))
          (git-link-kill :browse)))))

(map! (:map minibuffer-mode-map
            "M-l" #'sp-forward-slurp-sexp
            "M-h" #'sp-forward-barf-sexp)
      (:map minibuffer-local-map
            "C-c C-s" #'embark-collect
            (:prefix
             ";"
             "." #'evil-insert-state
             :desc "insert ;" "SPC" (cmd! (insert ";")))))

(map! :after rfc-mode
      :map rfc-mode-map
      :n "q" #'quit-window
      :n "[[" #'rfc-mode-previous-section
      :n "]]" #'rfc-mode-next-section
      :n "C-K" #'rfc-mode-previous-section
      :n "C-j" #'rfc-mode-next-section)

;;;;;;;;;;;;;;;;;;;;;;;
;; Leader keybidings ;;
;;;;;;;;;;;;;;;;;;;;;;;

(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command
      "*" #'search-in-project
      "TAB" #'alternate-buffer
      "v" #'expreg-transient
      "k" #'sexp-transient
      "/" #'consult-ripgrep
      :nv ";" (cmd! (call-interactively
                     (if (evil-visual-state-p)
                         #'comment-or-uncomment-region
                       #'comment-line)))
      (:when (modulep! :custom shell)
        :desc "pop shell" "'" #'shell-pop
        :desc "choose shell" "\"" #'shell-pop-choose)

      (:prefix ("b" . "buffers/browser")
       :desc "proj. buffers" "b" #'consult-project-buffer
       :desc "all buffers" "B" #'consult-buffer
       :desc "scratch" "s" #'switch-to-scratch-buffer
       :desc "Messages" "m" #'switch-to-messages-buffer
       :desc "kill" "d" #'kill-current-buffer
       :desc "kill with window" "k" #'kill-buffer-and-window
       :desc "diff with file" "D" #'diff-current-buffer-with-file
       :desc "kill some buffers" "s-d" #'kill-matching-buffers-rudely
       (:when (modulep! :custom web-browsing)
         :desc "browser history" "h" #'browser-hist-search
         :desc "browser tabs" "t" #'browser-goto-tab
         :desc "browser copy link" "l" #'browser-copy-tab-link
         :desc "insert url" "y" #'navegosa-insert-link
         :desc "act on url" "a" #'browser-tab-act
         :desc "in eww" "e" #'browser-active-tab->eww))

      (:prefix ("e" . "edit")
       :desc "edit indirect" "i" #'edit-indirect-region)

      (:prefix ("f" . "files")
               (:when (modulep! :custom search)
                 :desc "zoxide dir" "d" #'zoxide-find)
               :desc "dired" "j" #'dired-jump
               (:when (featurep :system 'macos)
                 :desc "open in app" "O" #'macos-open-in-default-program)
               "e" nil
               (:prefix ("e" . "doom/emacs")
                :desc "config dir" "d" #'find-in-config-dir
                :desc "elpaca sources" "i" (cmd! (dired elpaca-sources-directory))
                (:when (featurep :system 'linux)
                  :desc "awesomewm config" "a" (cmd! (dired "~/.config/awesome/")))))

      (:prefix ("g" . "goto/git")
       :desc "magit file" "f" #'magit-file-dispatch
       :desc "jump list" "j" #'evil-show-jumps
       :desc "git status" "s" #'magit-status
       :desc "blame" "b" #'magit-blame-addition
       :desc "clone" "C" #'git-clone
       (:prefix ("c" . "consult-gh")
                "o" #'consult-gh-orgs
                "r" #'consult-gh-search-repos
                "f" #'consult-gh-find-file
                "i" #'consult-gh-issue-list
                "p" #'consult-gh-pr-list)
       (:prefix ("l" . "git link")
        :desc "blame link" "b" #'git-link-blame
        :desc "copy link" "l" #'git-link-kill
        :desc "main branch" "m" #'git-link-main-branch))

      (:prefix ("h" . "help")
               "a" #'helpful-at-point
               "f" #'helpful-function
               "h" #'consult-symbol
               "c" #'consult-info
               "C" #'describe-key-briefly
               "p" nil
               (:prefix ("p" . "packages")
                        "l" #'list-packages
                        "f" #'find-library-other-window
                        "d" #'doom/describe-package)
               "s" #'find-function-other-window
               "v" #'helpful-variable
               "j" #'info-display-manual
               ;; "r" nil: SPC h inherits help-map, where r is a command
               ;; (info-emacs-manual) - unbind it so the prefix can exist
               "r" nil
               (:prefix ("r" . "reload")
                :desc "reload config" "r" #'reload-config))

      (:prefix ("i" . "insert")
       :desc "snippet" "s" #'consult-yasnippet
       :desc "file path" "f" #'insert-file-path)

      (:prefix ("j" . "jump")
       "j" #'avy-goto-char-timer
       :desc "xwidget" "x" #'xwidget-webkit-url-get-create)

      (:when (modulep! :custom tab-bar)
        :desc "tab-bar" "l" #'tab-bar-transient)

      (:prefix ("n" . "narrow")
       "F" #'narrow-to-defun-indirect-buffer
       "R" #'narrow-to-region-indirect-buffer
       "f" #'narrow-to-defun
       "r" #'narrow-to-region
       "l" #'consult-focus-lines
       :desc "widen" "w" (cmd! () (consult-focus-lines nil :show) (widen)))

      (:prefix ("o" . "open/Org")
       :desc "store link" "l" #'org-store-link
       :desc "link without id" "L" #'org-store-link-id-optional
       (:when (modulep! :custom notmuch)
         :desc "notmuch" "m" #'notmuch)
       (:when (modulep! :custom web-browsing)
         :desc "elfeed" "e" #'elfeed)
       (:when (modulep! :custom git)
         (:prefix ("g" . "git")
                  "h" #'gh-notify))
       (:prefix ("c" . "chat")
                "t" #'telega
                (:when (modulep! :custom ai)
                  :desc "gptel" "g" #'gptel+))
       "r" nil
       (:prefix ("r" . "roam")
        "r" #'vulpea-find
        "b" #'vulpea-backlinks
        :desc "work today" "t" (cmd! (vulpea-journal+ 'work))
        :desc "personal today" "T" (cmd! (vulpea-journal+ 'personal))
        :desc "work note" "n" (cmd! (vulpea-journal+ 'work (org-read-date nil t)))
        :desc "personal note" "N" (cmd! (vulpea-journal+ 'personal (org-read-date nil t)))
        :desc "org-roam-ui in xwidget" "w" #'org-roam-toggle-ui-xwidget
        :desc "org-roam-ui in browser" "W" #'org-roam-ui-browser+
        "C-b" #'browser-create-roam-node-for-active-tab))

      (:prefix ("p" . "projects")
               "b" #'consult-project-buffer
               "f" #'project-find-file
               "k" #'project-kill-buffers
               :desc "project buffers list" "i" #'project-list-buffers
               :desc "find dir" "d" #'project-find-dir
               (:when (modulep! :custom dired)
                 :desc "treemacs" "T" #'treemacs-project-toggle+
                 :desc "dired locate" "t" #'dired-jump-find-in-project)
               (:when (modulep! :custom shell)
                 :desc "project shell" "'" #'shell-pop-in-project-root))

      (:prefix ("r" . "reset/resume/ring/roam")
       "d" (cmd! (call-interactively #'redraw-display))
       "r" #'vulpea-find
       :desc "yank from kill-ring" "y" #'consult-yank-from-kill-ring
       (:after vertico
        :desc "vertico repeat" "l" #'vertico-repeat-or-unsuspend
        :desc "vertico history" "L" #'vertico-repeat-select))

      (:prefix ("s" . "search/symbol")
       :desc "in buffer"  "s" #'consult-line
       :desc "search" "/" #'consult-omni-transient
       :desc "eww search" "e" #'eww-search-words
       :desc "find-name-dired" "f" #'find-name-dired
       :desc "GitHub" "g" #'search-github-with-lang
       :desc "imenu" "j" #'imenu
       :desc "dir" "d" (cmd! (consult-ripgrep default-directory)))

      (:prefix ("t" . "toggle yo")
       :desc "v-line nav" "w" #'toggle-visual-line-navigation
       :desc "prefix wrap" ">" #'visual-wrap-prefix-mode
       :desc "minor modes" "m" #'consult-minor-mode-menu
       :desc "iBuffer side" "i" #'ibuffer-sidebar-toggle-sidebar
       :desc "Dired side" "d" #'dired-sidebar-toggle-sidebar
       :desc "line numbers" "l" #'display-line-numbers-mode)

      (:prefix ("T" . "toggle global")
       :desc "numbers" "N" #'global-display-line-numbers-mode
       :desc "variable-pitch" "f" #'variable-pitch-mode
       :desc "prefix wrap" ">" #'global-visual-wrap-prefix-mode
       (:when (modulep! :custom colors)
         :desc "next color theme" "n" #'colors/cycle-themes-down
         :desc "prev color theme" "p" #'colors/cycle-themes-up))

      (:prefix ("w" . "windows")
               "TAB" #'evil-window-prev
               "." #'window-transient
               "c" #'window-cleanup+
               "g" #'golden-ratio
               "D" #'ace-delete-window
               "M" #'ace-swap-window
               "W" #'ace-window
               "_" #'delete-other-windows-horizontally
               "m" #'toggle-maximize-buffer
               "|" #'delete-other-windows-vertically
               "=" #'balance-windows-area
               "u" #'window-undo
               "r" #'window-redo)
      "x" nil
      (:prefix ("x" ."text")
               "x" #'jinx-correct-word
               (:when (modulep! :custom writing)
                 (:prefix ("l" . "language")
                  :desc "define" "d" #'define-it-at-point
                  :desc "sdcv" "l" #'sdcv-search-pointer
                  :desc "Merriam Webster" "m" #'mw-thesaurus-lookup-dwim
                  :desc "wiktionary" "w" #'wiktionary-bro-dwim)
                 (:prefix ("t" . "translate")
                  :desc "en->ru" "e" #'google-translate-query-translate-reverse
                  :desc "ru->en" "r" #'google-translate-query-translate
                  :desc "es->en" "s" #'google-translate-es->en
                  :desc "en->es" "S" #'google-translate-en->es
                  :desc "translate" "t" #'translate-transient
                  :desc "popup" "p" #'google-translate-posframe-at-point))
               (:when (modulep! :custom ai)
                 (:prefix ("g" . "gptel")
                  :desc "gptel-menu" "g" #'gptel-menu
                  :desc "new gptel" "n" #'gptel+
                  :desc "check text" "e" #'gptel-improve-text-transient
                  :desc "quick" "q" #'gptel-quick-question-buffer
                  :desc "search" "/" #'gptel-log-find
                  "m" #'gptel-mode
                  "s" #'gptel-send
                  "c" #'eca)))

      (:prefix ("z" . "zoom")
       :desc "frame" "f" #'frame-zoom-transient))

(map! :map special-mode-map
      "SPC" nil
      "h" #'evil-backward-char)

(map! :map comint-mode-map
      "M-r" #'consult-history
      "C-c C-l" #'comint-clear-buffer)

(map! :map (org-mode-map markdown-mode-map)
      :i "-" #'insert-dash)

(after! (:and evil evil-maps)
  ;; often conflicts with doom-local-leader
  (map! (:map evil-motion-state-map "C-u" nil)
        (:map evil-insert-state-map "C-u" nil)
        (:map evil-window-map
              "d" #'evil-window-delete
              "S" #'window-split-and-follow
              "V" #'window-vsplit-and-follow
              "L" #'window-move-right
              "H" #'window-move-left
              "J" #'window-move-down
              "K" #'window-move-up)))

(map! :after ibuffer
      :map ibuffer-mode-map
      [remap imenu] #'ibuffer-jump-to-buffer
      :n "su" #'ibuffer-filter-by-unsaved-file-buffers
      :n "sF" #'ibuffer-filter-by-file-buffers
      :n "s*" #'ibuffer-filter-by-non-special-buffers)

(map! :map occur-mode-map
      :n "f" #'occur-mode-display-occurrence)

(map! :after transient
      (:map transient-map
            "q" #'transient-quit-one
            "<escape>" #'transient-quit-one))

(map! :after helpful
      :map helpful-mode-map
      :n "q" #'kill-buffer-and-window)

(add-hook! 'helpful-mode-hook #'visual-wrap-prefix-mode)

(map! :after calendar
      :map calendar-mode-map
      :n "gd" #'calendar-goto-date)

(after! epa
  (setopt epg-pinentry-mode 'loopback))

(use-package ligature
  :defer t
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(after! undo-fu
  (setopt undo-limit 80000000 ; 80Mb
          undo-strong-limit 120000000 ; 120Mb
          ;; 400Mb
          undo-outer-limit 400000000))

(unless (member "--debug-init" command-line-args)
  (setopt warning-minimum-level :error))

;;; config.el ends here
