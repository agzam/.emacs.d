;;; modules/writing/config.el -*- lexical-binding: t; -*-

;; Ported from doom.d modules/custom/writing.  Dropped (git-resurrectable):
;; after! writegood-mode (rode Doom's :checkers grammar, not installed here),
;; after! grip-mode (parks with :lang markdown), lsp-marksman require (parks
;; with lsp), menu-bar-item-set-clock-or-pomodoro (org-pomodoro long-tail).
;; translate-at-point-smart lives here now (below); it un-parked with the pdf
;; module (nov's localleader T), its only consumer.
;; NOTE Own packages (google-translate, occult, prisma, wiktionary-bro,
;; spacehammer) declare GitHub recipes; `local-checkout-recipe' (init.el)
;; redirects them to local checkouts on machines that have them -
;; spacehammer's checkout is ~/.hammerspoon itself.

;; doom.d gated the install on darwin; here it installs everywhere (smoke
;; determinism) - its commands are only ever invoked by Hammerspoon anyway.
(use-package spacehammer
  :ensure (spacehammer :host github :repo "agzam/spacehammer" :files ("*.el"))
  :defer t
  :commands spacehammer-edit-with-emacs
  :config
  (add-hook! 'spacehammer-edit-with-emacs-hook
             #'spacehammer-edit-with-emacs-h)
  (add-hook! 'spacehammer-before-finish-edit-with-emacs-hook
             #'spacehammer-before-finish-edit-with-emacs-h)

  (add-to-list
   'display-buffer-alist
   `("\\* spacehammer-edit.*"
     (display-buffer-reuse-window
      spacehammer-display-edit-buffer)
     (window . root))))

(use-package mw-thesaurus
  :defer t
  :commands mw-thesaurus-lookup-dwim
  :config
  (map! :map mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
  (add-to-list
   'display-buffer-alist
   `(,mw-thesaurus-buffer-name
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.3))))

(use-package sdcv
  :defer t
  :commands (sdcv-search-pointer sdcv-search)
  :hook (sdcv-mode . visual-line-mode)
  :config
  (map! :map sdcv-mode-map
        :n "q" #'sdcv-quit
        :n "n" #'sdcv-next-dictionary
        :n "p" #'sdcv-previous-dictionary
        :n "TAB" #'outline-cycle-buffer
        :n "<backtab>" #'outline-show-all
        :ni "RET" #'sdcv-search-pointer
        :n "a" #'sdcv-search-at-point)
  (setopt sdcv-word-pronounce nil)
  (add-to-list
   'display-buffer-alist
   `(,sdcv-buffer-name
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

;; Deferred, unlike doom.d's startup (require 'google-translate): the module
;; autoload file requires the package instead, so the transient and helpers
;; still find its defcustoms on first use.
(use-package google-translate
  :ensure (google-translate :host github :repo "agzam/google-translate"
                            :branch "improvements")
  :defer t
  :custom
  (google-translate-backend-method 'curl)
  :config
  ;; Google rotates the real TKK; this static pair keeps the API usable.
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (setopt google-translate-pop-up-buffer-set-focus t
          google-translate-default-source-language "auto"
          google-translate-default-target-language "en")
  (setopt google-translate-listen-program (executable-find "ffplay")
          google-translate-listen-program-args '("-nodisp" "-autoexit" "-loglevel" "quiet"))
  (setopt google-translate-input-method-auto-toggling t
          google-translate-preferable-input-methods-alist
          '((nil . ("en"))
            (spanish-prefix . ("es"))
            (russian-computer . ("ru"))))

  (add-hook! 'google-translate-mode-hook
    (defun google-translate-mode-h ()
      (variable-pitch-mode +1)
      (pop-to-buffer "*Google Translate*")
      (map! :map google-translate-mode-map
            (:localleader
             "l" #'google-translate-buffer-listen-source
             "L" #'google-translate-buffer-listen-translation))))

  (add-to-list
   'display-buffer-alist
   '("\\*Google Translate\\*"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (init-width . 0.3)
     (window . root))))

(use-package google-translate-posframe
  ;; rides the google-translate build (fork-only file)
  :ensure nil
  :after google-translate)

(use-package define-it
  :defer t
  :commands define-it-at-point
  :config
  (setopt define-it-show-google-translate nil
          define-it-show-header nil)

  ;; it doesn't pop to the buffer automatically when the definition arrives
  (defadvice! define-it-pop-to-buffer-a (&rest _)
    :after #'define-it--in-buffer
    (pop-to-buffer (format define-it--buffer-name-format define-it--current-word)))

  (add-to-list
   'display-buffer-alist
   '("\\*define-it:"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package separedit
  :ensure (separedit :host github :repo "twlz0ne/separedit.el")
  :defer t
  :commands (separedit separedit-dwim)
  :init
  (map! :map prog-mode-map :inv "C-c '"
        (cmd! () (cond
                  ((bound-and-true-p org-src-mode) (org-edit-src-exit))
                  ((eq major-mode 'separedit-double-quote-string-mode) (separedit-commit))
                  (t (separedit-dwim)))))
  (map! :map (separedit-double-quote-string-mode-map
              separedit-single-quote-string-mode-map)
        :inv "C-c '" #'separedit-commit)
  (map! :map minibuffer-local-map "C-c '" #'separedit)
  :config
  (setopt separedit-default-mode 'markdown-mode))

(after! ispell
  ;; Don't spellcheck org blocks
  (dolist (r '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
               ("#\\+BEGIN_SRC" . "#\\+END_SRC")
               ("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))
    (cl-pushnew r ispell-skip-region-alist :test #'equal))
  (setopt ispell-program-name "enchant-2")
  (add-to-list 'ispell-dictionary-alist
               '(nil "[[:alpha:]]"
                 "[^[:alpha:]]"
                 "['’]" nil ("-B") nil utf-8))

  (defadvice! change-dict-after-toggle-input (fn &optional arg interactive)
    :around #'toggle-input-method
    :around #'set-input-method
    (funcall fn arg interactive)
    (let ((dic+lan (pcase current-input-method
                     ("russian-computer" '("ru_RU" "russian"))
                     ((or "spanish-keyboard"
                          "spanish-prefix"
                          "spanish-postfix")
                      '("es_MX" "spanish"))
                     (_ '("en_US" "american-english")))))
      (setq ispell-alternate-dictionary
            (format "/usr/share/dict/%s" (cadr dic+lan)))
      (ispell-change-dictionary (car dic+lan)))))

(after! abbrev
  ;; Auto-correct typos/contractions ("dont" -> "don't") from this module's
  ;; abbrev_defs; add more with `C-x a i g' - abbrev saves back into the
  ;; tracked file in place (sanctioned exception, see AGENTS.md).
  ;; cape-abbrev reads it too.
  (setopt abbrev-file-name (expand-file-name "abbrev_defs" (dir!))
          save-abbrevs 'silently)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file abbrev-file-name))
  (add-hook! '(text-mode-hook git-commit-setup-hook) #'abbrev-mode))

(after! quail
  (quail-define-package
   "Emoji" "UTF-8" "😎" t
   "Emoji input mode for people that really, really like Emoji"
   '(("\t" . quail-completion))
   t t nil nil nil nil nil nil nil t)
  (quail-define-rules
   (":)" ?😀)
   (":(" ?😕)
   (":P" ?😋)
   (":D" ?😂)
   (":party:" ?🎉)
   (":spock:" ?🖖)
   (":thumb:" ?👍)))

(after! markdown-mode
  (setopt markdown-enable-math nil)
  (map! :map (markdown-mode-map)
        (:localleader
         (:prefix ("s" . "wrap")
                  "<" #'markdown-wrap-collapsible
                  "C" #'markdown-wrap-code-clojure
                  "c" #'markdown-wrap-code-generic)
         (:prefix ("o" . "open/links")
                  "o" #'markdown-open
                  "l" #'markdown-store-link))
        :i "[[" #'markdown-insert-stored-link
        :i "[ SPC" #'insert-bracket-pair))

(use-package youtube-sub-extractor
  :commands (youtube-sub-extractor-extract-subs)
  :config
  (map! :map youtube-sub-extractor-subtitles-mode-map
        :desc "copy timestamp URL" :n "RET" #'youtube-sub-extractor-copy-ts-link
        :desc "browse at timestamp" :n "C-c C-o" #'youtube-sub-extractor-browse-ts-link
        :n "q" #'kill-buffer-and-window))

(use-package wiktionary-bro
  :ensure (wiktionary-bro :host github :repo "agzam/wiktionary-bro.el")
  :commands (wiktionary-bro-dwim)
  :config
  (when (featurep 'evil)
    (dolist (state '(normal visual insert))
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap wiktionary-bro-mode-map state t t)
       state)))

  (map! :map wiktionary-bro-mode-map
        :n "q" #'kill-current-buffer)

  (add-hook! 'wiktionary-bro-mode-hook
    #'jinx-mode-off-h
    (defun wiktionary-bro-use-eww-open-in-other-window-h ()
      (setq-local browse-url-browser-function #'eww-open-in-other-window)))

  (add-to-list
   'display-buffer-alist
   '((major-mode . wiktionary-bro-mode)
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (init-width . 0.30)
     (window . root))))

(use-package jinx
  :ensure (jinx :host github :repo "minad/jinx")
  :defer t
  :hook (doom-first-buffer . global-jinx-mode)
  :config
  ;; Nothing else here ever loads vertico-multiform (in doom.d Doom itself
  ;; did) - require it, or the jinx grid never activates.
  (after! vertico
    (require 'vertico-multiform)
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))
    (vertico-multiform-mode 1))

  (setopt jinx-languages "en_US ru_RU es_MX")

  (map! :map jinx-mode-map
        :i ", SPC" #'insert-comma
        :i ",," #'jinx-autocorrect-last
        :i ",." (cmd! (jinx-autocorrect-last :prompt))))

;; github-topics renders prose-heavy PR listings; spellcheck off there.
;; The git module port deferred this hook here - jinx is this module's.
(add-hook 'github-topics-prs-buffer-hook #'jinx-mode-off-h)

(defadvice! forward-paragraph-fix-a (&rest _)
  "Move to first character of paragraph not the space before it"
  :after #'forward-paragraph
  (when (called-interactively-p 'any)
    (skip-chars-forward " \t\n")))

(defadvice! backward-paragraph-fix-a (ofn &rest _)
  "Move to first character of paragraph not the space after it"
  :around #'backward-paragraph
  (if (and (called-interactively-p 'any))
      (progn
        (skip-chars-backward " \n")
        (funcall ofn)
        (when (looking-at-p "[[:blank:]]*$")
          (skip-chars-forward " \t\n")))
    (funcall ofn)))

(defadvice! flash-on-recenter-a (&rest _)
  :after #'recenter-top-bottom
  (when (called-interactively-p 'any)
    (let ((face (make-face (gensym "pulse-"))))
      (set-face-background face "LightGreen")
      (pulse-momentary-highlight-one-line (point) face))))

;; Inert until the lsp module ports (nothing loads lsp-mode here yet).
;; Harper in lsp-mode is still rough upstream; registered as add-on anyway.
(after! lsp-mode
  (dolist (mode '((markdown-mode . "markdown")
                  (org-mode . "org")))
    (add-to-list 'lsp-language-id-configuration mode))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("harper-ls" "-s"))
    :major-modes '(markdown-mode org-mode)
    :initialization-options '(:userDictPath ""
                              :fileDictPath ""
                              :linters (:SpellCheck t
                                        :SpelledNumbers :json-false
                                        :AnA t
                                        :SentenceCapitalization t
                                        :UnclosedQuotes t
                                        :WrongQuotes :json-false
                                        :LongSentences t
                                        :RepeatedWords t
                                        :Spaces t
                                        :Matcher t
                                        :CorrectNumberSuffix t)
                              :codeActions (:ForceStable :json-false)
                              :markdown (:IgnoreLinkTitle :json-false)
                              :diagnosticSeverity "hint"
                              :isolateEnglish :json-false)
    :activation-fn (lsp-activate-on "markdown" "org")
    :add-on? 't
    :server-id 'harper-ls)))

(use-package occult
  :ensure (occult :host github :repo "agzam/occult.el")
  :defer t
  :commands (occult-toggle occult-hide-region occult-reveal-all)
  :init
  (after! evil
    (defadvice! occult-evil-close-fold-a (fn &rest args)
      "In visual state, hide selected region with occult."
      :around #'(evil-close-fold evil-close-fold-rec
                 org-close-fold)
      (if (evil-visual-state-p)
          (occult-hide-region (region-beginning) (region-end))
        (apply fn args)))

    (defadvice! occult-evil-open-fold-a (fn &rest args)
      "Reveal occult fold at point, or fall through to native."
      :around #'(evil-open-fold evil-open-fold-rec
                 org-open-fold)
      (if (cl-find-if (lambda (o) (overlay-get o 'occult))
                      (overlays-at (point)))
          (occult-toggle)
        (apply fn args)))

    (defadvice! occult-evil-open-folds-a (fn &rest args)
      "Also reveal all occult folds."
      :around #'(evil-open-folds org-open-all-folds)
      (apply fn args)
      (occult-reveal-all))

    (defadvice! occult-evil-toggle-fold-a (fn &rest args)
      "Delegate to occult-toggle when applicable."
      :around #'(evil-toggle-fold org-toggle-fold)
      (if (or (evil-visual-state-p)
              (cl-find-if (lambda (o) (overlay-get o 'occult))
                          (overlays-at (point))))
          (occult-toggle)
        (apply fn args)))))

;; Install-only: `paste-convert-kill' requires prisma at call time.  doom.d's
;; `:after (markdown org)' gate was rot - no `markdown' feature exists, so
;; the block never fired there either.
(use-package prisma
  :ensure (prisma :host github :repo "agzam/prisma.el")
  :defer t)

(after! evil
  ;; cross-format yank/paste: kills remember their origin format,
  ;; md<->org conversion happens at paste time; bare C-u pastes verbatim
  (advice-add 'evil-yank :after #'yank-remember-format-a)
  (advice-add 'evil-paste-after :around #'paste-maybe-convert-a)
  (advice-add 'evil-paste-before :around #'paste-maybe-convert-a))