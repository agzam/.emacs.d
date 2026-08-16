;;; modules/osx/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/osx, folding in the useful subset of
;; Doom's os/macos module (deltas in MIGRATION.org).  Darwin-only: init.el
;; splices it into `active-modules' conditionally (Doom's :if equivalent).
;;; Code:

;; doom-defaults frees the right option key for character composition
;; ('none); restore Meta so right-Opt+Backspace stays M-DEL.  The mac-*
;; name aliases ns-right-alternate-modifier in the NS build and is the
;; real variable on the emacs-mac port - one setq covers both.
(setopt mac-right-option-modifier 'meta)

;;
;;; Doom os/macos subset (minus the emacs-mac-port-only scroll vars)

(setopt locate-command "mdfind"  ; Spotlight backs M-x locate
        ;; visit files opened outside of Emacs in the existing frame
        ns-pop-up-frames nil
        ;; trash instead of rm, as a layer of precaution
        delete-by-moving-to-trash (not noninteractive))

;; Match the macOS titlebar to the theme.  Install unconditionally so tty
;; boots (smoke, CI-less probes) still pre-build it; activate per Doom's
;; guard - only sessions that can ever show NS frames.
(use-package ns-auto-titlebar
  :config
  (when (or (daemonp) (display-graphic-p))
    (ns-auto-titlebar-mode +1)))

;; Keychain-backed auth
(after! auth-source
  (add-to-list 'auth-sources 'macos-keychain-generic t)
  (add-to-list 'auth-sources 'macos-keychain-internet t))

;; Deviation from doom.d: merge applescript into the org module's babel
;; list instead of clobbering it (this config runs after org's).
(use-package ob-applescript
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(applescript . t))
  (org-babel-do-load-languages
   'org-babel-load-languages org-babel-load-languages))

(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'" . applescript-mode))
  :commands (applescript-mode))

;;; config.el ends here
