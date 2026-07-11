;;; modules/modeline/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/modeline.  Deltas from the Doom original:
;; - the custom layout redefines doom-modeline's `main' once
;;   (autoload/segments.el) instead of stamping an `agcustom' modeline
;;   buffer-locally from window-state-change-hook and re-defining it on
;;   every change; rendering is identical (ground-truthed live), minus the
;;   hook churn and the local-modeline guard it needed
;; - persp-name segment dropped: persp-mode runs in neither config
;;   (revisit workspace-name with the tab-bar port)
;; - dropped as dead: doom-modeline-mu4e nil (default value, package never
;;   installed), doom-modeline-modal-icon t (default, and the modals
;;   segment isn't in the layout), the :unless (modulep! :ui modeline)
;;   guard (never true here)
;; - anti-flash blanking hoisted to top level: use-package :init runs at
;;   elpaca queue time, after after-init-time is set - the doom.d guard
;;   made it dead code in this boot order
;;; Code:

;; Blank the stock modeline until doom-modeline takes over at UI init.
(unless after-init-time
  (setq-default mode-line-format nil))

(use-package doom-modeline
  :hook ((doom-modeline-mode . column-number-mode)
         (doom-init-ui . doom-modeline-mode))
  :after-call (doom-first-input-hook doom-first-file-hook)
  :config
  (setopt doom-modeline-buffer-encoding nil
          doom-modeline-buffer-file-name-style 'relative-to-project
          doom-modeline-buffer-modification-icon t
          doom-modeline-buffer-state-icon t
          doom-modeline-icon (display-graphic-p)
          doom-modeline-major-mode-color-icon nil
          doom-modeline-major-mode-icon nil
          mode-line-compact nil
          doom-modeline-height 1
          doom-modeline-bar-width 4)
  ;; plain defvar (C source), not a defcustom - icon glyph churn stutters
  ;; redisplay without it
  (setq inhibit-compacting-font-caches t)

  (apply-custom-modeline)

  ;; keep modeline short
  (defadvice! doom-modeline--font-height-a ()
    :override #'doom-modeline--font-height
    1))
