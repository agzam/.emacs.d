;;; early-init.el --- emacs-lab startup knobs -*- lexical-binding: t; -*-
;;; Commentary:
;; Startup optimizations borrowed from Doom Emacs (MIT, doomemacs/doomemacs
;; early-init.el @8e4fbba), trimmed to the safe subset.  Doom's redisplay,
;; mode-line and tool-bar suppression hacks are deliberately left out so a
;; broken trial init fails loudly.
;;; Code:

;; Elpaca replaces package.el entirely.
(setq package-enable-at-startup nil)

;; GC deferred during startup; gcmh takes over after (see init.el).
(setq gc-cons-percentage 1.0)
(if noninteractive
    (setq gc-cons-threshold (* 128 1024 1024))
  (setq gc-cons-threshold most-positive-fixnum)
  ;; Checking bytecode mtimes is wasted startup IO; Elpaca rebuilds on update.
  (setq load-prefer-newer nil))

(add-hook 'emacs-startup-hook
          (defun lab--startup-restore-h ()
            "Fallback sane GC until gcmh activates; restore `load-prefer-newer'."
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  load-prefer-newer t))
          95)

(setq read-process-output-max (* 64 1024))

;; Emacs 31: silence unactionable if-let/when-let obsoletion and lexbind spam
;; (vendored Doom code and plenty of packages still trip these).
(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)
(setq warning-suppress-types '((defvaralias) (lexical-binding)))
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; Don't pop warning buffers during async native compiles.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-warning-on-missing-source nil)

(setq auto-mode-case-fold nil
      ad-redefinition-action 'accept)

;; DEBUG=1 in the environment = --debug-init.
(let ((debug (getenv-internal "DEBUG")))
  (when (and (stringp debug) (not (string= debug "")))
    (setq init-file-debug t
          debug-on-error t)))

;; `file-name-handler-alist' is consulted on every load/require; drop it
;; during startup and merge it back at depth 101.
(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Keep the gzip handler only if the built-in lisp is compressed.
     (if (locate-file-internal "calc-loaddefs.el" load-path)
         nil
       (list (rassq 'jka-compr-handler old-value))))
    (put 'file-name-handler-alist 'initial-value (copy-sequence old-value))
    (define-advice command-line-1 (:around (fn args-left) lab-restore-fnha)
      ;; CLI file args may be TRAMP paths etc.
      (let ((file-name-handler-alist
             (if args-left (copy-sequence old-value) file-name-handler-alist)))
        (funcall fn args-left)))
    (add-hook 'emacs-startup-hook
              (defun lab--restore-file-name-handler-alist-h ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 (delete-dups
                  (append (default-toplevel-value 'file-name-handler-alist)
                          old-value))))
              101)))

(unless noninteractive
  ;; Frame resize to accommodate non-default fonts is a startup killer.
  (setq frame-inhibit-implied-resize t)
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)
  ;; Disable UI elements before the first frame is drawn.
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;;; early-init.el ends here
