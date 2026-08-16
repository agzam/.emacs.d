;;; modules/pdf/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/pdf.  Deltas from the Doom original:
;; - Continuous scroll now rides UPSTREAM pdf-tools.  vedang/pdf-tools ships
;;   lisp/pdf-roll.el (autoloaded `pdf-view-roll-minor-mode', lighter
;;   " Continuous") as of v1.2.0 (2025-12), so the dalanicolai fork + its
;;   pdf-roll branch AND the separate image-roll package are both gone - the
;;   roll code was absorbed into pdf-roll.el, which only requires pdf-view.
;;   The recipe tracks vedang's default branch; :files keeps build/{Makefile,
;;   server} so epdfinfo still compiles at first PDF open (runtime, not at
;;   elpaca build time - CI smoke never opens a PDF).  The fork-era
;;   <wheel-up>/<wheel-down> remaps in pdf-view-roll-minor-mode-map are
;;   dropped: the upstream mode drives the wheel itself via
;;   mwheel-scroll-{up,down}-function.  `pdf-view-continuous' (the s c toggle)
;;   is a distinct, long-standing per-line variable and survives untouched.
;; - calibredb dropped (unused).
;; - hide-mode-line added explicitly (Doom pulled it in via :ui; the lab had
;;   no consumer until now) for the annotation-list mode-line.
;; - nov's `jinx-mode-off' -> `jinx-mode-off-h' (writing-module rename);
;;   `translate-at-point-smart' un-parked into the writing module.
;; - `org-noter-anchor-to-current-page+' -> `-page' (plus-affix sweep).
;; - Doom's `SPC n e' org-noter row is pruned here (its
;;   (modulep! :lang org +noter) guard is false - org is a :custom module),
;;   so this module fills the slot itself.
;; - buffer-to-pdf added (Prot's; nothing like it in the Doom original) -
;;   this module now writes PDFs as well as reads them.
;; - pdf-text added (autoload/pdf-text.el; nothing like it in the Doom
;;   original): pdf-view-as-text extracts the document through epdfinfo into
;;   a reflowed pdf-text-mode companion buffer, nov's reading UX as model.
;;; Code:

(use-package pdf-tools
  :ensure (pdf-tools :host github :repo "vedang/pdf-tools"
                     :files (:defaults "README" ("build" "Makefile") ("build" "server")))
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (defadvice! pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (file-executable-p pdf-info-epdfinfo-program)
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors.  This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo.  Use `M-x pdf-tools-install' to build it")))

  (pdf-tools-install-noverify)

  ;; For consistency with other special modes.
  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setopt pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)

  (map! :map pdf-view-mode-map
        :gn "q" #'kill-current-buffer
        :nm "J" #'pdf-view-next-page
        :nm "K" #'pdf-view-previous-page
        :n "gg" #'pdf-evil-goto-first-line
        :n "G"  #'pdf-evil-goto-last-line
        :nm "[" #'pdf-history-backward
        :nm "]" #'pdf-history-forward
        :nm "o" #'pdf-outline
        :nm "C-e" #'pdf-view-scroll-up-or-next-page
        :nm "C-y" #'pdf-view-scroll-down-or-previous-page
        :nm "zk" #'pdf-view-enlarge
        :nm "zj" #'pdf-view-shrink
        :localleader
        "t" #'pdf-view-themed-minor-mode
        "," #'pdf-view-current-progress
        (:prefix ("s" . "slice/scroll")
                 "a" #'pdf-view-auto-slice-minor-mode
                 "b" #'pdf-view-set-slice-from-bounding-box
                 "m" #'pdf-view-set-slice-using-mouse
                 "r" #'pdf-view-reset-slice
                 "s" #'pdf-view-roll-minor-mode
                 "c" #'pdf-toggle-continuous-scroll)
        (:prefix ("f" . "fit")
                 "h" #'pdf-view-fit-height-to-window
                 "p" #'pdf-view-fit-page-to-window
                 "w" #'pdf-view-fit-width-to-window)
        (:prefix ("z" . "zoom")
                 "k" #'pdf-view-enlarge
                 "j" #'pdf-view-shrink
                 "0" #'pdf-view-scale-reset)
        "x" #'pdf-view-as-text
        "n" #'org-noter-transient))

(use-package hide-mode-line
  ;; The mode-line serves no useful purpose in annotation windows.
  :hook (pdf-annot-list-mode . hide-mode-line-mode))

(use-package saveplace-pdf-view
  :after pdf-view)

(after! pdf-view
  (defadvice! pdf-view-midnight-minor-mode-a (fn &rest args)
    "Toggling midnight-mode uses current theme colors."
    :around #'pdf-view-midnight-minor-mode
    (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                     ,(face-attribute 'default :background)))
    (funcall fn args))

  (defadvice! pdf-view-next-page-at-top-of-the-page-a (&optional _)
    "Always start at the top of the page."
    :after #'pdf-view-next-page
    :after #'pdf-view-next-page-command
    (image-scroll-down))

  (add-hook! 'enable-theme-functions #'adjust-pdf-colors-on-theme-change-h))

(use-package org-noter
  :ensure (org-noter :host github :repo "org-noter/org-noter")
  :after (org pdf-tools)
  :config
  (setopt
   org-noter-notes-search-path (list org-directory)
   org-noter-auto-save-last-location nil
   org-noter-separate-notes-from-heading t
   org-noter-always-create-frame nil
   org-noter-insert-note-no-questions nil
   org-noter-disable-narrowing t
   org-noter-notes-window-behavior '(only-prev)
   org-noter-kill-frame-at-session-end nil)

  (defadvice! org-noter--setup-windows-ignore-a (_ session)
    "Cease org-noter's windows and frame shenanigans."
    :around #'org-noter--setup-windows
    (when (org-noter--valid-session session)
      (with-selected-frame (org-noter--session-frame session)
        (let* ((doc-buffer (org-noter--session-doc-buffer session))
               (notes-buffer (org-noter--session-notes-buffer session)))
          (switch-to-buffer-other-window doc-buffer)
          (with-current-buffer notes-buffer
            (unless org-noter-disable-narrowing
              (org-noter--narrow-to-root (org-noter--parse-root session)))
            (setq notes-window (org-noter--get-notes-window 'start))
            (org-noter--set-notes-scroll notes-window))))))

  (defadvice! org-noter--set-notes-scroll-ignore-a (&rest _)
    :override #'org-noter--set-notes-scroll)

  (defadvice! org-noter--create-session-a (orig-fn &rest args)
    :around #'org-noter--create-session
    (cl-letf (((symbol-function
                #'org-noter--set-notes-scroll)
               #'ignore))
      (apply orig-fn args))))

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  ;; doom.d set nov-text-width to 100 then clobbered it with t in the same
  ;; setopt (rot); t (fill the window, paired with visual-line-mode) was the
  ;; effective value, kept.
  (setopt nov-variable-pitch nil
          nov-text-width t)
  (add-hook! 'nov-mode-hook
    #'jinx-mode-off-h
    #'visual-line-mode
    (defun nov-mode--keys-h ()
      (map! :map nov-mode-map
            :n "q" #'nov-back-or-quit
            "l" #'evil-forward-char
            "v" #'evil-visual-char
            "V" #'evil-visual-line
            "n" #'evil-ex-search-next
            :n "t" nil
            "g" nil
            "SPC" nil
            :n "i" nil
            "DEL" nil
            [remap text-scale-increase] #'nov-text-scale-increase
            [remap text-scale-decrease] #'nov-text-scale-decrease
            :n "]]" #'forward-paragraph
            :n "[[" #'backward-paragraph
            (:localleader
             "t" #'google-translate-posframe-at-point
             "T" #'translate-at-point-smart))
      (map! :map nov-button-map
            "l" #'evil-forward-char
            "v" #'evil-visual-char
            "V" #'evil-visual-line
            "n" #'evil-ex-search-next
            "g" nil
            "SPC" nil
            :n "i" nil
            "DEL" nil))))

;; pdf-text: reflowed plain-text companion view (autoload/pdf-text.el).
;; general defers :map bindings until the keymap exists, so binding here
;; works although pdf-text-mode-map is defined in a lazily-loaded file.
(add-hook 'pdf-text-mode-hook #'jinx-mode-off-h)

(map! :map pdf-text-mode-map
      :n "q" #'bury-buffer
      :n "RET" #'pdf-text-show-in-pdf
      (:localleader
       "p" #'pdf-text-show-in-pdf
       "t" #'google-translate-posframe-at-point
       "T" #'translate-at-point-smart))

;; Doom's `SPC n e' org-noter row is pruned in the lab (its
;; (modulep! :lang org +noter) guard is structurally false - org registers as
;; a :custom module).  The pdf module owns org-noter, so fill the slot here.
(map! :leader :desc "Org noter" "n e" #'org-noter)

;; Export the current buffer to a PDF that looks like the screen.
(use-package buffer-to-pdf
  :ensure (buffer-to-pdf :host github :repo "protesilaos/buffer-to-pdf")
  :defer t
  :config
  (setopt buffer-to-pdf-directory (expand-file-name "~/Downloads/"))

  (defadvice! buffer-to-pdf-fallback-a (fn buffer orientation)
    "Print through a browser where Emacs cannot export frames, and show it.
`x-export-frames' is compiled in only for Cairo builds, so on macOS every
command in this package would otherwise do nothing but error.  Probing the
function beats the package's own `system-configuration-features' test, which
reads \"cairo\" against a string that says CAIRO."
    :around #'buffer-to-pdf
    (if (fboundp 'x-export-frames)
        (funcall fn buffer orientation)
      (print-buffer-with-browser buffer orientation))
    (show-pdf (exported-pdf-path buffer))))

(map! :leader :desc "Export buffer to PDF" "f p" #'buffer-to-pdf)

;;; config.el ends here
