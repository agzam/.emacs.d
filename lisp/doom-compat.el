;;; doom-compat.el --- vendored Doom Emacs macro layer -*- lexical-binding: t; -*-
;;; Commentary:
;; Macros and helpers vendored from Doom Emacs (MIT License, Copyright (c)
;; 2014-2026 Henrik Lissner), primarily lisp/doom-lib.el, lisp/doom-emacs.el
;; and modules/doom/compat/+use-package.el @ doomemacs/doomemacs 8e4fbba.
;; Trimmed of Doom's module/profile system; Doom names are kept so config
;; ported from ~/.doom.d works with minimal diffs.
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; * Directories

(defvar doom-emacs-dir user-emacs-directory)
(defvar doom-user-dir user-emacs-directory)
(defvar doom-local-dir (expand-file-name ".local/" user-emacs-directory))
(defvar doom-data-dir (expand-file-name "etc/" doom-local-dir))
(defvar doom-state-dir (expand-file-name "state/" doom-local-dir))
(defvar doom-profile-state-dir doom-state-dir)
(defvar doom-cache-dir (expand-file-name ".cache/" doom-local-dir))

(dolist (dir (list doom-local-dir doom-data-dir doom-state-dir doom-cache-dir))
  (make-directory dir t))

;;; * File quarantine

;; Nothing writes into `user-emacs-directory'; grep stays clean.  Extend this
;; list as ported modules introduce new state files.
(setq auto-save-list-file-prefix (concat doom-cache-dir "autosave/")
      savehist-file (concat doom-state-dir "savehist")
      save-place-file (concat doom-state-dir "saveplace")
      nov-save-place-file (concat doom-state-dir "nov-places")
      recentf-save-file (concat doom-state-dir "recentf")
      bookmark-default-file (concat doom-state-dir "bookmarks")
      project-list-file (concat doom-state-dir "projects")
      multisession-directory (concat doom-state-dir "multisession/")
      tramp-persistency-file-name (concat doom-cache-dir "tramp")
      tramp-auto-save-directory (concat doom-cache-dir "tramp-autosave/")
      url-configuration-directory (concat doom-data-dir "url/")
      url-cache-directory (concat doom-cache-dir "url/")
      eshell-directory-name (concat doom-state-dir "eshell/")
      nsm-settings-file (concat doom-data-dir "network-security.data")
      transient-levels-file (concat doom-state-dir "transient/levels.el")
      transient-values-file (concat doom-state-dir "transient/values.el")
      transient-history-file (concat doom-state-dir "transient/history.el")
      projectile-cache-file (concat doom-cache-dir "projectile.cache")
      projectile-known-projects-file (concat doom-state-dir "projectile.projects")
      forge-database-file (concat doom-data-dir "forge/forge-database.sqlite")
      code-review-db-database-file (concat doom-data-dir "code-review-db-file.sqlite")
      org-id-locations-file (concat doom-cache-dir "org-id-locations")
      ;; org 9.7+ defaults org-persist to XDG cache; keep the lab's out of the
      ;; live Doom session's (both run org 10 but different builds)
      org-persist-directory (concat doom-cache-dir "org-persist/")
      org-publish-timestamp-directory (concat doom-cache-dir "org-timestamps/")
      org-clock-persist-file (concat doom-state-dir "org-clock-save.el")
      ;; org-roam/vulpea db locations live with their packages in the org
      ;; module (both target doom-local-dir - lab builds its own, never Doom's)
      ;; treemacs defaults both under user-emacs-directory/.cache/
      treemacs-persist-file (concat doom-cache-dir "treemacs-persist")
      treemacs-last-error-persist-file (concat doom-cache-dir "treemacs-last-error-persist")
      ;; lsp module: session/state files default under user-emacs-directory,
      ;; server installs under its .cache/ (Doom put sessions in cache, server
      ;; downloads in data - same split here); dap keeps breakpoints and
      ;; VSCode extension downloads there too
      lsp-session-file (concat doom-cache-dir "lsp-session")
      lsp-server-install-dir (concat doom-data-dir "lsp/")
      lsp-eslint-library-choices-file (concat doom-cache-dir "lsp-eslint-choices")
      dap-breakpoints-file (concat doom-cache-dir "dap-breakpoints")
      dap-utils-extension-path (concat doom-data-dir "dap-extension/")
      lsp-java-workspace-dir (concat doom-data-dir "java-workspace/")
      ;; ai module: token caches + eca server binary default inside
      ;; user-emacs-directory; lab keeps its own copies - sharing Doom's
      ;; OAuth tokens.el invites refresh-token rotation races.
      gptel-anthropic-oauth-cache-dir (concat doom-cache-dir "anthropic-oauth/")
      gptel-gh-github-token-file (concat doom-cache-dir "copilot-chat/github-token")
      gptel-gh-token-file (concat doom-cache-dir "copilot-chat/token")
      gptel-crowdsourced-prompts-file (concat doom-cache-dir "gptel-crowdsourced-prompts.csv")
      eca-server-install-path (concat doom-data-dir "eca/eca")
      eca-server-version-file-path (concat doom-data-dir "eca/eca-version")
      ;; search module: tldr pages default under user-emacs-directory;
      ;; slacko's extracted-token cache defaults there too (secret-bearing -
      ;; ai-module token-cache precedent)
      tldr-directory-path (concat doom-cache-dir "tldr/")
      slacko-creds-gpg-file (concat doom-cache-dir "slacko-creds.gpg")
      ;; tab-bar module: desktop files live with the rest of the session
      ;; state (the module presets desktop-dirname to match)
      desktop-path (list doom-state-dir)
      ;; git module: the request package derives its curl cookie jar from
      ;; request-storage-directory, which defaults inside user-emacs-directory
      request-storage-directory (concat doom-cache-dir "request/")
      ;; tree-sitter module: keep the .dylib grammar blobs (un-greppable
      ;; build artifacts) in the cache.  Two knobs, both load-bearing:
      ;; `treesit-ensure-installed' (Emacs 31) installs into the first
      ;; writable `treesit-extra-load-path' entry, but raw
      ;; `treesit-install-language-grammar' calls (markdown-ts-mode's
      ;; grammar setup, M-x with RET) ignore that variable - their default
      ;; out-dir is the car of the history list, else
      ;; user-emacs-directory/tree-sitter.
      treesit-extra-load-path (list (expand-file-name "tree-sitter" doom-cache-dir))
      treesit--install-language-grammar-out-dir-history
      (list (expand-file-name "tree-sitter" doom-cache-dir))
      ;; chat module: emojify downloads its emoji-image sets under
      ;; user-emacs-directory/emojis/ by default (Doom's :ui emoji relocates
      ;; the same way)
      emojify-emojis-dir (concat doom-data-dir "emojis/"))

(defvar doom-disabled-packages nil)

;; Doom polyfill: makes (featurep :system 'macos) etc. work.
(provide :system
         (cond ((eq system-type 'darwin) '(macos bsd))
               ((eq system-type 'berkeley-unix) '(bsd))
               ((eq system-type 'gnu/linux) '(linux))
               ((memq system-type '(cygwin windows-nt ms-dos)) '(windows))))

;;; * Module registry (modulep! shim)

(defvar doom-modules-enabled nil
  "List of (:category module +flags...) entries treated as enabled.
Set in init.el; powers the `modulep!' shim.")

(defun doom-module--entry (category module)
  (cl-find-if (lambda (entry)
                (and (eq (car entry) category)
                     (eq (cadr entry) module)))
              doom-modules-enabled))

(defmacro modulep! (category &optional module &rest flags)
  "Lab shim of Doom's `modulep!'; absolute form only.
+flag requires the flag present, -flag requires its + form absent (Doom
semantics; the SPC c lsp rows ride on -eglot).  Relative flag checks
(modulep! +flag) must be resolved at port time."
  (unless (keywordp category)
    (error "modulep! shim only supports (modulep! :category module +flags...)"))
  `(when-let* ((entry (doom-module--entry ,category ',module)))
     (cl-every (lambda (flag)
                 (let ((name (symbol-name flag)))
                   (if (string-prefix-p "-" name)
                       (not (memq (intern (concat "+" (substring name 1)))
                                  (cddr entry)))
                     (memq flag (cddr entry)))))
               ',flags)))

;;; * Logging stub

(defmacro doom-log (&rest _)
  "No-op stand-in for Doom's logger."
  nil)

;;; * Helpers

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun doom-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defalias 'doom-partial #'apply-partially)

(defun doom-rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (doom--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "doom--setq-%s-for-%s-h"
                                          var mode))))))

(defsubst doom--dir (dir segments)
  (let ((segments (delq nil segments))
        file-name-handler-alist)
    (if segments
        (expand-file-name
         (if (cdr segments)
             (apply #'file-name-concat segments)
           (car segments))
         dir)
      (expand-file-name dir))))

(defun doom-lookup-key (keys &rest keymaps)
  "Like `lookup-key', but search active keymaps if KEYMAP is omitted."
  (if keymaps
      (cl-some (doom-rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(defun doom-run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with error handling."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (run-hooks hook)
      (error
       (lwarn hook :error "Error running hook %S because: %s" hook e)))))

(defun doom-run-hook-on (hook-var trigger-hooks &optional predicate)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            (when (and (not running?)
                       ;; Not during startup; for this config "startup" ends
                       ;; when elpaca has processed all queues.
                       (bound-and-true-p elpaca-after-init-time)
                       (or (daemonp)
                           (and (boundp hook)
                                (symbol-value hook)))
                       (or (null predicate)
                           (funcall predicate)))
              (setq running? t)
              (doom-run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          ;; `find-file-hook' triggers too late; advise `after-find-file'.
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))

;;; * Sugars

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or (bound-and-true-p byte-compile-current-file)
      load-file-name
      (buffer-file-name (buffer-base-buffer))  ; for `eval'
      (let ((file (car (last current-load-list))))
        (if (stringp file) file))
      (error "file!: cannot deduce the current file path")))

(defmacro dir! (&rest segments)
  "Return the directory of the file in which this macro was called.

Appends SEGMENTS to the path, relative to the call site."
  (let* ((file-name-handler-alist nil)
         (dir (file-name-directory (macroexpand '(file!)))))
    (if segments
        `(doom--dir ,dir (list ,@segments))
      dir)))

(defmacro load! (filename &optional path noerror)
  "Load FILENAME relative to the current executing file.
Simplified from Doom's version (no doom error hierarchy)."
  `(load (file-name-concat ,(or path `(dir!)) ,filename) ,noerror 'nomessage))

(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file."
  `(let ((default-directory (dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))

(put 'defun* 'lisp-indent-function 'defun)
(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

BINDINGS is either a list of (PLACE VALUE) bindings as `cl-letf*' would accept,
or a list of (or a single) `defun', `defun*', `defmacro', or `defadvice' forms."
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice
               (if (keywordp (cadr rest))
                   (cl-destructuring-bind (target where fn) rest
                     `(when-let* ((fn ,fn))
                        (advice-add ,target ,where fn)
                        (unwind-protect ,body (advice-remove ,target fn))))
                 (let* ((fn (pop rest))
                        (argspec (pop rest)))
                   (when (< (length argspec) 3)
                     (setq argspec
                           (list (nth 0 argspec)
                                 (nth 1 argspec)
                                 (or (nth 2 argspec) (gensym (format "%s-a" (symbol-name fn)))))))
                   (let ((name (nth 2 argspec)))
                     `(progn
                        (define-advice ,fn ,argspec ,@rest)
                        (unwind-protect ,body
                          (advice-remove #',fn #',name)
                          ,(if name `(fmakunbound ',name))))))))
              (`defun
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  (cl-letf (((symbol-function #',(car rest))
                             (lambda! ,(cadr rest) ,@(cddr rest))))
                    ,body)))
              (`defun*
               `(cl-labels ((,@rest)) ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro quiet!! (&rest forms)
  "Run FORMS without generating any output (for real)."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@forms)
     (letf! ((standard-output (lambda (&rest _)))
             (defun message (&rest _))
             (defun load (file &optional noerror _nomessage nosuffix must-suffix)
               (funcall load file noerror t nosuffix must-suffix))
             (defun write-region (start end filename &optional append visit lockname mustbenew)
               (unless visit (setq visit 'no-message))
               (funcall write-region start end filename append visit lockname mustbenew)))
       ,@forms)))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

Silences `message', `load', `write-region' and `standard-output'. In
interactive sessions this inhibits echo-area output, but not *Messages*."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@forms)
     ,(if noninteractive
          `(quiet!! ,@forms)
        `(let ((inhibit-message t)
               (save-silently t))
           (prog1 ,@forms (message ""))))))

;;; * Closure factories

(defmacro lambda! (arglist &rest body)
  "Returns (cl-function (lambda ARGLIST BODY...))
ARGLIST accepts anything `cl-defun' will. Implicitly adds `&allow-other-keys'
if `&key' is present in ARGLIST."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((nlistp (cdr-safe arg)) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         (allow-other-keys arglist))
      ,@body)))

(setplist 'doom--fn-crawl '(%2 2 %3 3 %4 4 %5 5 %6 6 %7 7 %8 8 %9 9))
(defun doom--fn-crawl (data args)
  (cond ((symbolp data)
         (when-let*
             ((pos (cond ((eq data '%*) 0)
                         ((memq data '(% %1)) 1)
                         ((get 'doom--fn-crawl data)))))
           (when (and (= pos 1)
                      (aref args 1)
                      (not (eq data (aref args 1))))
             (error "%% and %%1 are mutually exclusive"))
           (aset args pos data)))
        ((and (not (eq (car-safe data) 'fn!))
              (or (listp data)
                  (vectorp data)))
         (let ((len (length data))
               (i 0))
           (while (< i len)
             (doom--fn-crawl (elt data i) args)
             (cl-incf i))))))

(defmacro fn! (&rest args)
  "Return a lambda with implicit, positional arguments (%1..%9, %*)."
  `(lambda ,(let ((argv (make-vector 10 nil)))
              (doom--fn-crawl args argv)
              `(,@(let ((i (1- (length argv)))
                        (n -1)
                        sym arglist)
                    (while (> i 0)
                      (setq sym (aref argv i))
                      (unless (and (= n -1) (null sym))
                        (cl-incf n)
                        (push (or sym (intern (format "_%%%d" i)))
                              arglist))
                      (cl-decf i))
                    arglist)
                ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interactive commands for keybinds."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        (let ((command ,command))
          (or (command-remapping command)
              command))
        ,@args))))

(defmacro cmds! (&rest branches)
  "Returns a dispatcher that runs a command in BRANCHES.
BRANCHES is a flat list of CONDITION COMMAND pairs, with an optional trailing
fallback COMMAND."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (ignore it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

(defalias 'λ!  #'cmd!)
(defalias 'λ!! #'cmd!!)

;;; * after!

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE has loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features. It may use
:or/:any and :and/:all operators; a plain list implies :and."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

;;; * Hooks

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again. HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "doom-transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil lambda lambda!))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.
Takes the same arguments as `add-hook!'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _) (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (doom--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;;; * Advice definers

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

;;; * Library functions

(defun doom-glob (&rest segments)
  "Return file list matching the glob created by joining SEGMENTS."
  (declare (side-effect-free t))
  (let* (case-fold-search
         file-name-handler-alist
         (path (apply #'file-name-concat segments)))
    (if (string-suffix-p "/" path)
        (cl-loop for file in (file-expand-wildcards (substring path 0 -1))
                 if (file-directory-p file)
                 collect file)
      (file-expand-wildcards path))))

(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.
Returns (STATUS . OUTPUT)."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defun doom-project-root (&optional dir)
  "Return the project root of DIR (default `default-directory'), or nil.
Lab version on top of project.el (Doom used projectile)."
  (when-let* ((project (project-current nil dir)))
    (project-root project)))

;; set-lookup-handlers! lives in modules/lookup/autoload/lookup.el now (the
;; loaddefs autoload would lose to a stub defun here - `autoload' never
;; overrides an fboundp symbol).

(defun doom-region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

(defun doom-region-beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))

(defun doom-region-end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-end)
           (marker-position evil-visual-end))
      (region-end)))

(defun doom-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (buffer-substring-no-properties
          (doom-region-beginning)
          (doom-region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defun doom-recenter-a (&rest _)
  "Generic advice for recentering the window (typically :after other fns)."
  (recenter))

;;; * Doom lifecycle hooks

(defvar doom-first-input-hook nil
  "Transient hooks run before the first user input.")
(defvar doom-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(defvar doom-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

(defvar doom-switch-buffer-hook nil
  "Hooks run after changing the current buffer.")
(defvar doom-switch-window-hook nil
  "Hooks run after changing the focused window.")
(defvar doom-switch-frame-hook nil
  "Hooks run after changing the focused frame (debounced).")

(defvar doom-load-theme-hook nil
  "Hooks run after the theme is loaded.")

;; Bridges: Doom init hooks map onto elpaca's end-of-init.
(defvar doom-init-ui-hook nil)
(defvar doom-init-modules-hook nil)
(defvar doom-after-init-hook nil)

(defun doom-compat--after-init-h ()
  (doom-run-hooks 'doom-init-ui-hook 'doom-init-modules-hook 'doom-after-init-hook))

;; Switch-frame machinery vendored from doom-emacs.el ('+last-focus frame
;; parameter renamed to 'doom--last-focus).
(defvar doom-switch-frame-hook-debounce-delay 2.0
  "The delay for which `doom-switch-frame-hook' won't trigger again.

This exists to prevent switch-frame hooks getting triggered too aggressively
due to misbehaving desktop environments, packages incorrectly frame switching
in non-interactive code, or the user rapidly un-and-refocusing the frame.")

(defun doom--run-switch-frame-hooks-fn (_)
  (remove-hook 'pre-redisplay-functions #'doom--run-switch-frame-hooks-fn)
  (let ((gc-cons-threshold most-positive-fixnum))
    (dolist (fr (visible-frame-list))
      (let ((state (frame-focus-state fr)))
        (when (and state (not (eq state 'unknown)))
          (let ((last-update (frame-parameter fr 'doom--last-focus)))
            (when (or (null last-update)
                      (> (float-time (time-subtract (current-time) last-update))
                         doom-switch-frame-hook-debounce-delay))
              (with-selected-frame fr
                (unwind-protect
                    (let ((inhibit-redisplay t))
                      (run-hooks 'doom-switch-frame-hook))
                  (set-frame-parameter fr 'doom--last-focus (current-time)))))))))))

(let (last-focus-state)
  (defun doom-run-switch-frame-hooks-fn ()
    "Trigger `doom-switch-frame-hook' once per frame focus change."
    (or (equal last-focus-state
               (setq last-focus-state
                     (mapcar #'frame-focus-state (frame-list))))
        ;; Defer until next redisplay
        (add-hook 'pre-redisplay-functions #'doom--run-switch-frame-hooks-fn))))

(unless noninteractive
  (add-hook 'elpaca-after-init-hook #'doom-compat--after-init-h -90)

  (defun doom--run-switch-buffer-hooks-h (&optional _)
    (let ((gc-cons-threshold most-positive-fixnum))
      (run-hooks 'doom-switch-buffer-hook)))
  (add-hook 'window-buffer-change-functions #'doom--run-switch-buffer-hooks-h)

  (defun doom--run-switch-window-hooks-h (&optional _)
    (let ((gc-cons-threshold most-positive-fixnum))
      (run-hooks 'doom-switch-window-hook)))
  (add-hook 'window-selection-change-functions #'doom--run-switch-window-hooks-h)

  (add-function :after after-focus-change-function #'doom-run-switch-frame-hooks-fn)

  (defun doom--run-load-theme-hooks-h (_theme)
    (run-hooks 'doom-load-theme-hook))
  (add-hook 'enable-theme-functions #'doom--run-load-theme-hooks-h)

  (doom-run-hook-on 'doom-first-file-hook   '(find-file-hook dired-initial-position-hook))
  (doom-run-hook-on 'doom-first-input-hook  '(pre-command-hook))
  (doom-run-hook-on 'doom-first-buffer-hook '(find-file-hook doom-switch-buffer-hook)))

;;; * Incremental lazy-loading

(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup.")

(defvar doom-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.
nil disables it; 0 loads everything at doom-after-init.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) between incrementally loading packages.")

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally (when idle, if NOW)."
  (let* ((gc-cons-threshold most-positive-fixnum)
         (first-idle-timer (or doom-incremental-first-idle-timer
                               doom-incremental-idle-timer)))
    (if (not now)
        (cl-callf append doom-incremental-packages packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (unless (featurep req)
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) first-idle-timer)
                     (not
                      (while-no-input
                        (let ((default-directory doom-emacs-dir)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if packages
                (progn
                  (run-at-time (if idle-time
                                   doom-incremental-idle-timer
                                 first-idle-timer)
                               nil #'doom-load-packages-incrementally
                               packages t)
                  (setq packages nil)))))))))

(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.
If this is a daemon session, load them all immediately instead."
  (when (numberp doom-incremental-first-idle-timer)
    (if (zerop doom-incremental-first-idle-timer)
        (mapc #'require (cdr doom-incremental-packages))
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))

(add-hook 'doom-after-init-hook #'doom-load-packages-incrementally-h 100)

;;; * use-package extensions (:defer-incrementally, :after-call)

(defvar doom--deferred-packages-alist '(t))

(with-eval-after-load 'use-package-core
  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((doom-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "doom--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (condition-case e
                       (let ((default-directory doom-emacs-dir))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let* ((deferral-list (assq ',name doom--deferred-packages-alist)))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (cl-callf2 delq deferral-list doom--deferred-packages-alist)
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name doom--deferred-packages-alist)
             (push '(,name) doom--deferred-packages-alist))
           (nconc (assq ',name doom--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))

(provide 'doom-compat)
;;; doom-compat.el ends here
