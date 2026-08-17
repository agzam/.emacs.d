;;; init.el --- Elpaca + vendored Doom macros -*- lexical-binding: t; -*-
;;; Commentary:
;; Personal config (Elpaca + a vendored Doom macro layer) at ~/.emacs.d,
;; isolated from ~/.doom.d (the retired porting reference).  All machine
;; state lives under ~/.emacs.d/.local/ (git-ignored).
;;; Code:

;;; Elpaca bootstrap (installer 0.12, verbatim from upstream README)

;; Disable treeless clones everywhere - with newer Git versions (seen on
;; git 2.55) the clone succeeds but materializes an empty worktree, so
;; elpaca finds no elisp files
(setq elpaca-order-defaults (list :type 'git :protocol 'https :inherit t :depth 1))

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
(setopt use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Own packages declare plain GitHub recipes (portable across machines);
;; where a checkout below exists, `local-checkout-recipe' redirects the recipe
;; to it and elpaca builds in place - edits picked up on `elpaca-rebuild',
;; nothing cloned (MIGRATION "Local :repo packages").  A checkout this machine
;; lacks (a newly added or private own package) is cloned over ssh into its dev
;; folder by `ensure-local-dev-checkouts' below, before elpaca resolves - so it
;; too builds in place instead of failing elpaca's anonymous https clone.
(defvar local-dev-packages
  '((remoto . "~/GitHub/agzam/remoto.el")
    (github-topics . "~/GitHub/agzam/github-topics")
    (ag-themes . "~/GitHub/agzam/ag-themes.el")
    (browser-hist . "~/GitHub/agzam/browser-hist.el")
    (consult-hn . "~/GitHub/agzam/consult-hn")
    (consult-zoxide . "~/GitHub/agzam/consult-zoxide.el")
    (reddigg . "~/GitHub/agzam/emacs-reddigg")
    (hnreader . "~/GitHub/agzam/emacs-hnreader")
    (navegosa . "~/GitHub/agzam/navegosa.el")
    (google-translate . "~/GitHub/agzam/google-translate")
    (occult . "~/GitHub/agzam/occult.el")
    (prisma . "~/GitHub/agzam/prisma.el")
    (wiktionary-bro . "~/GitHub/agzam/wiktionary-bro.el")
    (slacko . "~/GitHub/agzam/slacko.el")
    (go-jira . "~/GitHub/agzam/go-jira.el")
    (khalendario . "~/GitHub/agzam/khalendario.el")
    ;; the hammerspoon config IS the spacehammer checkout (doom.d symlinked it)
    (spacehammer . "~/.hammerspoon"))
  "Alist of own packages -> local checkout preferred over the GitHub recipe.")

(defvar local-dev-clone-overrides
  ;; :repo where the GitHub repo can't be derived from the checkout dir name,
  ;; :branch where a non-default branch is pinned.
  '((spacehammer :repo "agzam/spacehammer"))  ; checkout is ~/.hammerspoon
  "Per-package (NAME :repo R :branch B) clone overrides for `ensure-local-dev-checkouts'.")

(defun local-checkout-recipe (recipe)
  "Local `:repo' override for RECIPE when its `local-dev-packages' dir exists.
The trailing slash is load-bearing: elpaca strips \".el$\" from local paths
otherwise (elpaca-git-repo-dir), demoting a remoto.el-style checkout from
build-in-place to clone."
  (when-let* ((name (intern-soft (plist-get recipe :package)))
              (path (alist-get name local-dev-packages))
              (dir (expand-file-name path))
              ((file-directory-p dir)))
    (list :host nil :fetcher nil :repo (file-name-as-directory dir))))

(add-hook 'elpaca-recipe-functions #'local-checkout-recipe)

;; Clone any own package this machine hasn't checked out yet into its dev
;; folder over ssh, before elpaca resolves recipes, so `local-checkout-recipe'
;; redirects to it and elpaca builds in place.  Skipped under CI (no ssh key;
;; it only needs the public repos, which clone anonymously over https).
(require 'local-dev)
(unless (getenv "CI")
  (ensure-local-dev-checkouts))

;; Import the shell PATH; a compositor-launched Emacs inherits a minimal one.
(elpaca (exec-path-from-shell :wait t))
(require 'shell-env)
(import-shell-environment)

;;; Doom compat layer

;; map! needs general.el at init time.
(elpaca (general :wait t))

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
         (:custom git) (:custom general) (:custom completion) (:custom embark) (:custom colors)
         (:custom modeline) (:custom tab-bar) (:custom elisp) (:custom search) (:custom dired) (:custom ai)
         (:custom web-browsing) (:custom tree-sitter) (:custom lsp) (:custom clojure)
         (:custom python) (:custom lua) (:custom java) (:custom rust)
         (:custom org) (:custom shell)
         (:custom writing) (:custom chat) (:custom yaml) (:custom pdf))
       (when (eq system-type 'darwin) '((:os macos) (:custom osx) (:custom jira)))))

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

;; Elpaca activates a package whose build dir merely exists, even when an
;; interrupted update left it without autoloads (commands silently void,
;; transients refuse to open) or with stale bytecode (old code keeps
;; running).  Surface both loudly right after the queue settles instead.
(add-hook! 'doom-after-init-hook
  (defun warn-broken-elpaca-builds-h ()
    (when-let* ((broken (half-built-elpaca-packages)))
      (warn "Half-built elpaca package(s): %s - autoloads missing; fix with `bb repair' (or M-x elpaca-rebuild)"
            (mapconcat (lambda (b) (symbol-name (car b))) broken ", ")))
    (when-let* ((stale (stale-elpaca-builds)))
      (warn "Stale bytecode in elpaca package(s): %s - source newer than .elc; fix with `bb repair' (or M-x elpaca-rebuild)"
            (mapconcat (lambda (s) (symbol-name (car s))) stale ", ")))))

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
(defvar active-modules
  `(evil bindings lookup git general multiple-cursors completion embark colors modeline tab-bar elisp search dired ai web-browsing tree-sitter lsp clojure python lua java rust org shell writing chat yaml pdf
    ;; darwin-only tail - Doom's (:if (featurep :system 'macos) ...)
    ;; hammerspoon: monroe glue for the spacehammer fennel nREPL.
    ;; jira after osx: its browse autoload rides the web-browsing + git
    ;; module fns (loaded above), and it defers on org (loaded above).
    ,@(when (eq system-type 'darwin) '(osx hammerspoon jira)))
  "Modules under modules/, loaded in this exact order.
bindings (Doom's :config default) precedes the :custom ports, like in doom!;
lookup (Doom's :tools lookup core) rides right behind it; lsp < clojure <
python/lua/java/rust < org keeps the doom! :custom order (python/lua/java/rust
ride behind lsp and clojure - they call lsp! and the lsp lookup handlers;
java/rust need no extra deps beyond lsp; fennel's monroe glue now lives in
the darwin-tail hammerspoon module, autoloaded so order-independent).
pdf trails org and writing: org-noter defers on org, and the
nov (epub) config rides writing's translate + jinx autoloads.")

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
(put 'narrow-to-region 'disabled nil)
