;;; modules/colors/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/colors.  Deltas from the Doom original:
;; - package! recipes folded into :ensure; theme packages that only lived in
;;   packages.el get minimal :defer t blocks; ef-themes stays out (disabled
;;   upstream too)
;; - circadian-setup fires on doom-init-ui-hook, not window-setup-hook:
;;   elpaca activates packages after window-setup on cold boots, so a
;;   window-setup hook registered from :hook would never run
;; - cycle commands renamed (colors/ prefix dropped) and anchored on
;;   (car custom-enabled-themes) - doom.d anchored the ring on doom-theme,
;;   which circadian's timed switches never update
;;; Code:

(setopt pulse-delay 0.05)

;; doom-defaults turns window dividers on at UI init; never wanted here.
(remove-hook 'doom-init-ui-hook #'window-divider-mode)

(use-package spacemacs-theme
  :ensure (spacemacs-theme :host github :repo "nashamri/spacemacs-theme")
  :defer t)

(use-package base16-theme
  :defer t)

(use-package doom-themes
  :defer t)

(use-package ag-themes
  ;; Tracked *-autoloads.el/-pkg.el in the repo are stale straight/package.el
  ;; artifacts; excluded so elpaca's generated autoloads don't collide.
  :ensure (ag-themes :host github :repo "agzam/ag-themes.el"
                     :files ("*.el" (:exclude "*-autoloads.el" "*-pkg.el")))
  :after-call doom-init-ui-hook)

(use-package circadian
  :hook (doom-init-ui . circadian-setup)
  :config
  ;; North of TX
  (setopt calendar-latitude 33.16
          calendar-longitude -96.93)
  (setopt circadian-themes
          '(("6:00" . ag-themes-spacemacs-light)
            ("19:00" . ag-themes-doom-feather-light)
            ("20:00" . ag-themes-base16-ocean)
            ("21:00" . ag-themes-base16-ashes)
            ("23:00" . ag-themes-doom-plain-dark))))

(use-package rainbow-mode
  :defer t)

(use-package beacon
  :after-call doom-first-file-hook
  :config
  (setopt beacon-blink-delay 0.1
          beacon-blink-duration 0.7
          beacon-size 60
          beacon-color "DarkGoldenrod2"
          beacon-blink-when-window-scrolls nil)
  (when (and (display-graphic-p)
             (not (eq system-type 'darwin)))
    (beacon-mode +1)))
