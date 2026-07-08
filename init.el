;;; init.el --- emacs-lab: Elpaca + vendored Doom macros trial -*- lexical-binding: t; -*-
;;; Commentary:
;; Parallel trial config, fully isolated from ~/.doom.d.  Launch with:
;;   emacs --init-directory ~/.config/emacs-lab
;;; Code:

;;; Elpaca bootstrap (installer 0.12, verbatim from upstream README)

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; use-package via Elpaca, with Doom-parity semantics

(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)

;; Doom's use-package! defers by default; keep that contract for ported code.
(setq use-package-always-defer t
      use-package-always-ensure t)

;;; Doom compat layer

;; map! needs general.el at init time.
(elpaca (general :wait t))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'doom-compat)

;; Mirror of the doom! block in ~/.doom.d/init.el - powers `modulep!' so
;; vendored bindings prune themselves exactly like they did under Doom.
(setq doom-modules-enabled
      (append
       '((:ui vi-tilde-fringe) (:ui window-select) (:ui zen)
         (:editor evil +everywhere) (:editor file-templates) (:editor multiple-cursors)
         (:emacs electric) (:emacs ibuffer) (:emacs tramp) (:emacs undo) (:emacs vc)
         (:term eshell)
         (:checkers syntax +childframe) (:checkers grammar)
         (:tools eval +overlay) (:tools lookup) (:tools lsp +peek)
         (:lang emacs-lisp) (:lang json) (:lang markdown +grip) (:lang sh)
         (:config default +bindings +smartparens))
       (when (eq system-type 'darwin) '((:os macos)))))

(require 'doom-keybinds)

;; GC returns to normal under gcmh once the first buffer is visited.
(use-package gcmh
  :hook (doom-first-buffer . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

;;; Modules

;; Doom-style layout: modules/NAME/{autoload.el,autoload/*.el,config.el}.
;; Function files load first (sorted - raw directory order differs between
;; Mac and Linux), then config.el; config.el may `load!' extra +files.
;; The module list itself is explicit, in this exact order.
(defvar lab-modules '("evil" "bindings" "completion")
  "Modules under modules/, loaded in this exact order.")

(defun lab-load-module (name)
  "Load modules/NAME: autoload.el, autoload/*.el (sorted), then config.el."
  (let ((dir (expand-file-name (format "modules/%s/" name) user-emacs-directory)))
    (dolist (lib (sort (nconc (doom-glob dir "autoload.el")
                              (doom-glob dir "autoload/*.el"))
                       #'string<))
      (load lib nil 'nomessage))
    (load (expand-file-name "config" dir) nil 'nomessage)))

(mapc #'lab-load-module lab-modules)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
