;;; doom-keybinds.el --- vendored Doom leader keys + map! -*- lexical-binding: t; -*-
;;; Commentary:
;; Vendored from Doom Emacs (MIT License, Copyright (c) 2014-2026 Henrik
;; Lissner), modules/doom/compat/+keybinds.el @ doomemacs/doomemacs 8e4fbba.
;; Changes: Doom lifecycle hooks remapped to doom-compat bridges, module
;; system references removed, which-key comes from Emacs core.  `map!' :desc on
;; a terminal key embeds the label in the keymap as a (DESC . DEF) menu item
;; (built-in which-key reads it directly, so embark's prefix-stripped
;; sub-keymaps keep their labels); prefix labels stay global replacements so
;; layered leader prefixes across modules still merge instead of clobbering,
;; but outside the leader tree they go through `which-key-label-prefix', which
;; only matches prefix bindings.
;;; Code:

(require 'doom-compat)

(defcustom doom-leader-key "SPC"
  "The leader prefix key for Evil users."
  :type 'key-sequence
  :group 'doom)

(defcustom doom-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users."
  :type 'key-sequence
  :group 'doom)

(defcustom doom-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands."
  :type 'key-sequence
  :group 'doom)

(defcustom doom-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users."
  :type 'key-sequence
  :group 'doom)

(defcustom doom-leader-key-states '(normal visual motion)
  "which evil modes to activate the leader key for"
  :type '(repeat symbol)
  :group 'doom)

(defcustom doom-leader-alt-key-states '(emacs insert)
  "which evil modes to activate the alternative leader key for"
  :type '(repeat symbol)
  :group 'doom)

(defvar doom-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")


(require 'general)
;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)
(defalias 'kbd! #'general-simulate-key)
;; Prevent "X starts with non-prefix key Y" errors when binding over defaults.
;; (Doom deferred this to after module init; here modules load after this file.)
(general-auto-unbind-keys)

;; HACK: `map!' uses this instead of `define-leader-key!' because it consumes
;;   20-30% more startup time, so we reimplement it ourselves.
(defmacro doom--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (doom-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key doom-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let* ((desc (cadr (memq :which-key udef))))
              (cl-callf2 append
                  `((which-key-add-key-based-replacements
                      (general--concat t doom-leader-alt-key ,key)
                      ,desc)
                    (which-key-add-key-based-replacements
                      (general--concat t doom-leader-key ,key)
                      ,desc))
                  wkforms))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.

See `doom-leader-key' and `doom-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'doom-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `doom-localleader-key' and `doom-localleader-alt-key' to change the
localleader prefix."
  (if (modulep! :editor evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix doom-localleader-key
        :non-normal-prefix doom-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix doom-localleader-alt-key
      ,@args)))

;; PERF: We use a prefix commands instead of general's
;;   :prefix/:non-normal-prefix properties because general is incredibly slow
;;   binding keys en mass with them in conjunction with :states -- an effective
;;   doubling of Doom's startup time!
(define-prefix-command 'doom/leader 'doom-leader-map)
(define-key doom-leader-map [override-state] 'all)

;; Bind `doom-leader-key' and `doom-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'doom-after-init-hook
  (defun doom-init-leader-keys-h ()
    "Bind `doom-leader-key' and `doom-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal doom-leader-alt-key "C-c")
                   (set-keymap-parent doom-leader-map mode-specific-map))
                  ((equal doom-leader-alt-key "C-x")
                   (set-keymap-parent doom-leader-map ctl-x-map)))
            (define-key map (kbd doom-leader-alt-key) 'doom/leader))
        (evil-define-key* doom-leader-key-states map (kbd doom-leader-key) 'doom/leader)
        (evil-define-key* doom-leader-alt-key-states map (kbd doom-leader-alt-key) 'doom/leader))
      (general-override-mode +1))))

(use-package which-key
  :ensure nil  ; built-in since Emacs 30
  :hook (doom-first-input . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  (add-hook! 'doom-before-reload-hook
    (defun doom-reset-which-key-replacements-h ()
      (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements doom-leader-key "<leader>")
  (which-key-add-key-based-replacements doom-localleader-key "<localleader>"))

(defun which-key-label-prefix (key desc)
  "Label the prefix keymap bound at KEY as DESC.
Narrower than `which-key-add-key-based-replacements', which matches the
key alone: embark resolves a pressed prefix to its bare sub-keymap and
hands that to which-key with no prefix context, so a top-level \"b\"
label would rename whatever command sits on \"b\" one level down."
  (cl-pushnew (cons (cons (format "\\`%s\\'" (regexp-quote (key-description (kbd key))))
                          ;; which-key renders a bare prefix keymap as
                          ;; "prefix", one carrying a (DESC . KEYMAP) label as
                          ;; "group:DESC"; anchored both ends so the label
                          ;; replaces the description whole
                          "\\`\\(?:group:.*\\|prefix\\)\\'")
                    (cons nil desc))
              which-key-replacement-alist
              :test #'equal))


;;; ** `map!' macro

(defvar doom-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun doom--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list \\='normal \\='visual \\='insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (doom-keyword-name keyword)
           if (assq l doom-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar doom--map-forms nil)
(defvar doom--map-fn nil)
(defvar doom--map-batch-forms nil)
(defvar doom--map-state '(:dummy t))
(defvar doom--map-parent-state nil)
(defvar doom--map-evil-p nil)
(after! evil (setq doom--map-evil-p t))

(defun doom--map-process (rest)
  (let ((doom--map-fn doom--map-fn)
        doom--map-state
        doom--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (doom--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (doom--map-commit)
                  (setq doom--map-fn 'doom--define-leader-key))
                 (:localleader
                  (doom--map-commit)
                  (setq doom--map-fn 'define-localleader-key!))
                 (:after
                  (doom--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (doom--map-set :keymaps `(backquote ,(ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (doom--map-nested (list (intern (doom-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "doom-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            doom--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (doom--map-set (if doom--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (if doom--map-fn
                          ;; leader/localleader labels ride on the whole
                          ;; <leader>-anchored sequence, which nothing else can
                          ;; match; `doom--define-leader-key' reads them off
                          ;; the :which-key def
                          (setq rest (append (list :desc desc "" nil) rest))
                        (push `(after! which-key
                                 (which-key-label-prefix
                                  ,(doom--map-append-keys :prefix) ,desc))
                              doom--map-forms)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          doom--map-forms)))
                 (_
                  (condition-case _
                      (doom--map-def (pop rest) (pop rest)
                                     (doom--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((doom--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (doom--map-commit)
    (macroexp-progn (nreverse (delq nil doom--map-forms)))))

(defun doom--map-append-keys (prop)
  (let ((a (plist-get doom--map-parent-state prop))
        (b (plist-get doom--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun doom--map-nested (wrapper rest)
  (doom--map-commit)
  (let ((doom--map-parent-state (doom--map-state)))
    (push (if wrapper
              (append wrapper (list (doom--map-process rest)))
            (doom--map-process rest))
          doom--map-forms)))

(defun doom--map-set (prop &optional value)
  (unless (equal (plist-get doom--map-state prop) value)
    (doom--map-commit))
  (setq doom--map-state (plist-put doom--map-state prop value)))

(defun doom--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (doom-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ;; Label a prefix through a global which-key replacement rather than
            ;; rebinding the prefix key to a fresh labeled keymap: the same
            ;; leader prefixes are built up across several modules, and a fresh
            ;; keymap would drop the keys another module already bound here.
            ((and (equal key "") (null def))
             (setq def (cons 'list `(:ignore t :which-key ,desc))))
            ;; Embed a terminal key's label in the keymap itself as a
            ;; (DESC . DEF) menu item, so which-key still shows it when embark
            ;; displays a sub-keymap with the prefix stripped -- a global
            ;; replacement keyed on the whole key sequence can't match there.
            ((setq def `(cons ,desc ,def))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state doom--map-batch-forms)))
  t)

(defun doom--map-commit ()
  (when doom--map-batch-forms
    (cl-loop with attrs = (doom--map-state)
             for (state . defs) in doom--map-batch-forms
             if (or doom--map-evil-p (not state))
             collect `(,(or doom--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) doom--map-forms))
    (setq doom--map-batch-forms nil)))

(defun doom--map-state ()
  (let ((plist
         (append (list :prefix (doom--map-append-keys :prefix)
                       :infix  (doom--map-append-keys :infix)
                       :keymaps
                       (append (plist-get doom--map-parent-state :keymaps)
                               (plist-get doom--map-state :keymaps)))
                 doom--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

Properties
  :leader [...]                   an alias for (:prefix doom-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

  These must be placed right before the key string.

  Do
    (map! :leader :desc \"Description\" :n \"C-c\" \\=#'dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" \\=#'dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" \\=#'dosomething)"
  (when (or (bound-and-true-p byte-compile-current-file)
            (not noninteractive))
    (doom--map-process rest)))

(provide 'doom-keybinds)
;;; doom-keybinds.el ends here
