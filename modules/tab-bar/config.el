;;; modules/tab-bar/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/tab-bar (no packages; tab-bar is
;; preloaded, so the after! block runs at module load).  Deltas from the
;; Doom original are in MIGRATION.org's Decisions log.
;;; Code:

(after! tab-bar
  (setopt tab-bar-show t
          tab-bar-new-tab-group nil
          tab-bar-close-button-show nil
          tab-bar-separator " ❘ "
          tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
          tab-bar-auto-width t
          tab-bar-auto-width-max '((150) 10)
          ;; ring capacity for window-undo/window-redo; stock default of 10
          ;; is too shallow for a day of window juggling
          tab-bar-history-limit 100)

  (add-hook! 'tab-bar-tab-name-format-functions
             #'tab-bar-fmt-show-index-fn)
  (remove-hook!
    'tab-bar-tab-name-format-functions
    #'tab-bar-tab-name-format-hints)

  ;; also feeds the window-undo/window-redo engine (general module), which
  ;; overrides this mode's recording hooks
  (tab-bar-history-mode +1)

  (unless (featurep :system 'macos)
    (setopt tab-bar-tab-name-function #'tab-bar-name-fn))

  (map!
   "s-[" #'tab-bar-switch-to-prev-tab
   "s-]" #'tab-bar-switch-to-next-tab
   "s-j" #'tab-bar-switch-to-prev-tab
   "s-k" #'tab-bar-switch-to-next-tab)

  ;; tabs sometimes disappear from the frame; force the tab-bar line back
  ;; on before switching
  (defadvice! tab-bar-switch-a (&optional _)
    :before #'tab-bar-switch-to-prev-tab
    :before #'tab-bar-switch-to-next-tab
    :before #'tab-bar-transient
    (set-frame-parameter nil 'tab-bar-lines 1)))

;; Desktop persistence.  Restore is deliberately manual (the transient's
;; "dr" -> restore-desktop-and-tabs): desktop.el's own after-init restore
;; hook registers long after after-init has run here, so it never fires.
;; Enabling on doom-after-init-hook (elpaca-after-init) arms the exit save
;; once every package is activated.  Presetting `desktop-dirname' keeps the
;; `desktop-save' t exit path from prompting for a directory - unset, it
;; read-directory-name'd on every quit and doom.d never actually wrote a
;; desktop file.  desktop-path itself is quarantined in doom-compat.el.
(add-hook! 'doom-after-init-hook
  (defun init-desktop-mode-h ()
    (setq desktop-dirname doom-state-dir)
    (desktop-save-mode 1)))

(setopt desktop-save t)
