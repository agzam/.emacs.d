;;; modules/hammerspoon/config.el -*- lexical-binding: t; -*-

;; Monroe is the nREPL client for the spacehammer fennel REPL (jeejah on
;; localhost:7888).  Moved out of the clojure module: CIDER owns the actual
;; Clojure REPLs, monroe's only job on this machine is Hammerspoon.  The
;; module sits in the darwin-only tail of `active-modules', so no
;; system-type gating is needed here.

(use-package monroe
  :defer t
  :commands (monroe monroe-connect)
  :init
  (setq monroe-default-host "localhost"
        monroe-default-port 7888)

  (advice-add 'monroe-sentinel :around #'hammerspoon-monroe-sentinel-a)

  (add-hook! 'fennel-mode-hook
    (defun monroe-fennel-mode-setup-h ()
      (monroe-interaction-mode 1)))

  (map! :after fennel-mode
        :map fennel-mode-map
        :i "C-j" #'monroe-eval-expression-at-point
        (:localleader
         (:prefix ("j" . "jack-in")
                  "c" #'hammerspoon-monroe-connect)
         (:prefix ("e" . "eval")
                  "c" #'monroe-eval-expression-at-point
                  "e" #'monroe-eval-expression-at-point
                  "d" #'monroe-eval-defun
                  "b" #'monroe-eval-buffer
                  "r" #'monroe-eval-region)
         (:prefix ("s" . "repl")
                  "s" #'monroe-switch-to-repl
                  "q" #'monroe-disconnect)
         (:prefix ("h" . "help")
                  "d" #'monroe-describe))))
