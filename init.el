;;; init.el --- Elpaca + vendored Doom macros -*- lexical-binding: t; -*-
;;; Commentary:
;; Parallel trial config, fully isolated from ~/.doom.d.  Launch with:
;;   emacs --init-directory ~/.config/emacs-lab
;;; Code:

;;; Elpaca bootstrap (installer 0.12, verbatim from upstream README)

(defvar elpaca-installer-version 0.12)
;; Deviation from the stock installer: repos/builds live outside the config
;; dir (see early-init.el quarantine).
(defvar elpaca-directory (expand-file-name "elpaca/" doom-local-dir))
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

;; Doom leaves use-package-always-defer nil: :after-only blocks (vertico
;; extensions, posframes, evil-traces) need demand-after-parents semantics,
;; which always-defer silently disables - packages built but never loaded.
;; Laziness comes from explicit :defer/:hook/:commands keywords instead.
(setq use-package-always-ensure t)

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
         (:config default +bindings +smartparens)
         ;; :custom entries appear here as their modules get ported.
         (:custom general) (:custom completion))
       (when (eq system-type 'darwin) '((:os macos)))))

;; Leader prefixes are read at bind time; set before doom-keybinds loads so
;; no module-level binding (or the which-key label) can capture the "SPC m"
;; default - root config.el is too late.
(setq doom-localleader-key ","
      doom-localleader-alt-key "C-,")

(require 'doom-keybinds)

;; Baseline editor/UI defaults vendored from Doom core (lisp/doom-emacs.el);
;; loads before modules so module and user layers override it.
(require 'doom-defaults)

;; Standalone helpers, loaded before modules (mirrors Doom init.el ordering).
(load (expand-file-name "lisp/functions" user-emacs-directory) nil 'nomessage)

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
(defvar active-modules '("evil" "bindings" "general" "completion")
  "Modules under modules/, loaded in this exact order.
bindings (Doom's :config default) precedes the :custom ports, like in doom!.")

(defun generate-module-loaddefs (name auto-dir)
  "Return the loaddefs file for module NAME, regenerating it if stale.
Generation runs in a batch subprocess: `loaddefs-generate' must be able to
macroexpand cookied definers (`transient-define-prefix'), and requiring those
in this session would load them before Elpaca activates the real versions."
  (let* ((out (expand-file-name (format "autoloads/%s.el" name) doom-cache-dir))
         (tmp (concat out ".tmp")))
    (when (or (not (file-exists-p out))
              (cl-some (lambda (f) (file-newer-than-file-p f out))
                       (doom-glob auto-dir "*.el")))
      (make-directory (file-name-directory out) t)
      (let ((res (doom-call-process
                  (concat invocation-directory invocation-name)
                  "-Q" "--batch" "--eval"
                  (format
                   "%S"
                   `(progn
                      ;; Definer macros the autoload files may use at cookie sites.
                      (require 'transient)
                      (loaddefs-generate ,auto-dir ,tmp)
                      ;; Paths are emitted relative to the output directory;
                      ;; rewrite absolute so nothing depends on load-path.
                      (let ((rel (file-relative-name
                                  (directory-file-name ,auto-dir)
                                  (file-name-directory ,tmp)))
                            (abs (directory-file-name ,auto-dir)))
                        (with-temp-file ,tmp
                          (insert-file-contents ,tmp)
                          (while (search-forward (concat "\"" rel "/") nil t)
                            (replace-match (concat "\"" abs "/") t t))))
                      (rename-file ,tmp ,out t))))))
        (unless (eq 0 (car res))
          (error "loaddefs generation for module %s failed: %s" name (cdr res)))))
    out))

(defun load-module (name)
  "Load modules/NAME Doom-style: lazy autoloads first, then config.el.
autoload/*.el are exposed via generated loaddefs and only load in full on
first call - they may require packages that aren't installed yet.  A single
autoload.el is loaded eagerly and must be load-safe."
  (let* ((dir (expand-file-name (format "modules/%s/" name) user-emacs-directory))
         (auto-dir (expand-file-name "autoload/" dir))
         (auto-file (expand-file-name "autoload.el" dir)))
    (when (file-directory-p auto-dir)
      (load (generate-module-loaddefs name auto-dir) nil 'nomessage))
    (when (file-exists-p auto-file)
      (load auto-file nil 'nomessage))
    (load (expand-file-name "config" dir) nil 'nomessage)))

(mapc #'load-module active-modules)

;; User config layer - loads last so its bindings and settings win,
;; exactly like $DOOMDIR/config.el in Doom.
(load (expand-file-name "config" user-emacs-directory) nil 'nomessage)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
