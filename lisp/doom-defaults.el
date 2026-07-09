;;; doom-defaults.el --- vendored Doom baseline defaults -*- lexical-binding: t; -*-
;;; Commentary:
;; The settings half of Doom's lisp/doom-emacs.el (MIT License, Copyright (c)
;; 2014-2026 Henrik Lissner) @ doomemacs/doomemacs 8e4fbba, near-verbatim.
;; Loads after doom-compat (uses its macros, dirs and lifecycle hooks) and
;; before modules, so module and user layers override it, as under Doom.
;;
;; Deviations from upstream:
;; - dropped: Emacs 29/30 backports (30.1 is the floor here), doom-theme/
;;   doom-font variables + theme/font loaders (fonts/colors ports pending),
;;   menu/tool/scroll-bar suppression (early-init.el owns it; the macOS
;;   menu-bar fix is kept), winner-mode (windows.el layout engine deliberately
;;   avoids it), hl-line (config.el disables Doom's global-hl-line-mode),
;;   fallback-buffer guards + kill-current-buffer advice + doom-quit-p (need
;;   the buffers lib), Man/completion-list mode-line hiding (needs
;;   mode-line-invisible-mode), incremental loader + switch-{buffer,window,
;;   frame} hooks (live in doom-compat), Doom's startup entry point
;; - confirm-kill-emacs simplified to yes-or-no-p until the buffers lib ports
;; - scroll-conservatively/scroll-margin left to ultra-scroll (modules/general)
;; - state-file paths (auto-save prefix, recentf, savehist, saveplace,
;;   transient, ...) come from doom-compat's quarantine; the backup dir here
;;   derives from doom-cache-dir
;; - savehist-mode enable lives in modules/completion; only settings here
;; - upstream's saveplace block enables savehist-mode and configures
;;   `after-load 'savehist' (copy-paste bug); corrected to saveplace
;; - doom-visible-buffers and temp/special buffer predicates inlined
;;; Code:

(require 'cl-lib)
(require 'doom-compat)


;;
;;; * Global defaults

;; Background native compilation consumes several CPU cores and takes minutes to
;; complete. Not worth the extra stress when on battery power.
(setq native-comp-async-on-battery-power nil)  ; introduced in Emacs 31.1


;;; ** Stricter security defaults

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (featurep :system 'windows))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))


;;; ** Runtime optimizations

;; PERF: Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses.
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. This increases memory usage, however.
(setq inhibit-compacting-font-caches t)

;; Inhibits fontification while receiving input, which should help a little
;; with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; PGTK builds only: lowering the frame-event timeout from the default 0.1
;; makes childframes and the packages that abuse them feel snappier.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))


;;; ** Trusting this config

;; Trust the contents of the config dir; we work on it from inside it.
(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-truename user-emacs-directory)))

;; Ensure .dir-locals.el in the config dir is always respected.
(add-to-list 'safe-local-variable-directories user-emacs-directory)

;; Ensure that, if the user does want package.el, it doesn't litter inside the
;; config dir. Elpaca replaces it, but `list-packages' remains usable.
(with-eval-after-load 'package
  (setq package-user-dir (file-name-concat doom-local-dir "elpa/")
        package-gnupghome-dir (expand-file-name "gpg" package-user-dir))
  (let ((s (if (gnutls-available-p) "s" "")))
    (add-to-list 'package-archives `("melpa" . ,(format "http%s://melpa.org/packages/" s)))
    (add-to-list 'package-archives `("org"   . ,(format "http%s://orgmode.org/elpa/"   s))))
  ;; Refresh package.el the first time you call `package-install'.
  (add-transient-hook! 'package-install (package-refresh-contents)))


;;
;;; * Text Editor config

;;; ** Encodings

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated. (The user layer sets its own input method.)
(setq default-input-method nil)
;; ...And the clipboard on Windows is often a wider encoding (UTF-16), so leave
;; Emacs to its own devices there.
(unless (or (featurep :system 'windows) (featurep :system 'wsl))
  (setq selection-coding-system 'utf-8))


;;; ** File handling

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))


;;; ** Backup & autosave files

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist `(("." . ,(concat doom-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe.
      auto-save-include-big-deletions t
      ;; `auto-save-list-file-prefix' is set in doom-compat's quarantine.
      ;; Hash the file names: paths built from `buffer-file-name' get too long
      ;; for some filesystems, and TRAMP would otherwise prompt to save its
      ;; auto-saves in `temporary-file-directory'.
      auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "tramp-\\2-") sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "\\2-") sha1)))

(add-hook! 'auto-save-hook
  (defun doom-ensure-auto-save-prefix-exists-h ()
    (with-file-modes #o700
      (make-directory auto-save-list-file-prefix t))))

(add-hook! 'after-save-hook
  (defun doom-guess-mode-h ()
    "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window))) ; only visible buffers
             (set-auto-mode)
             (not (eq major-mode 'fundamental-mode)))))))

(defadvice! doom--shut-up-autosave-a (fn &rest args)
  "If a file has autosaved data, `after-find-file' will pause for 1 second to
tell you about it. Very annoying. This prevents that."
  :around #'after-find-file
  (letf! ((#'sit-for #'ignore))
    (apply fn args)))

;; HACK: Make sure backup files (like undo-tree's) don't have ridiculously long
;;   file names that some filesystems will refuse.
(defadvice! doom-make-hashed-backup-file-name-a (fn file)
  "A few places use the backup file name so paths don't get too long."
  :around #'make-backup-file-name-1
  (let ((alist backup-directory-alist)
        backup-directory)
    (while alist
      (let ((elt (car alist)))
        (if (string-match (car elt) file)
            (setq backup-directory (cdr elt)
                  alist nil)
          (setq alist (cdr alist)))))
    (let ((file (funcall fn file)))
      (if (or (null backup-directory)
              (not (file-name-absolute-p backup-directory)))
          file
        (expand-file-name (sha1 (file-name-nondirectory file))
                          (file-name-directory file))))))


;;; ** Formatting

;; Favor spaces over tabs. It can be changed on a per-mode basis anyway (and
;; is, where tabs are the canonical style, like `go-mode').
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation. (modules/completion overrides with 'complete.)
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; An archaic default in the age of widescreen 4k displays? I disagree. We still
;; frequently split our terminals and editor frames, or have them side-by-side,
;; using up more of that newly available horizontal real-estate.
(setq-default fill-column 80)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs
;; otherwise, so off it goes.
(setq truncate-partial-width-windows nil)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line as "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this, but we should.
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes. It is more sensible for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)


;;; ** Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse.
(setq kill-do-not-save-duplicates t)


;;
;;; * User Interface config

;; A simple confirmation prompt when killing Emacs.
;; DEVIATION: Doom uses `doom-quit-p' (no prompt unless real buffers are open);
;; that needs the buffers lib, so a plain prompt until it ports.
(setq confirm-kill-emacs #'yes-or-no-p)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Larger column width for function name in profiler reports
(after! profiler
  (setf (caar profiler-report-cpu-line-format) 80
        (caar profiler-report-memory-line-format) 80))


;;; ** {menu,tool,scroll} bars

;; early-init.el pushes the frame parameters that disable these; the mode
;; variables must be nil too so the modes can be re-enabled in one toggle.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; HACK: The menu-bar needs special treatment on macOS: in GUI frames the menu
;;   bar lives outside the frame, on the macOS menu bar, which is acceptable,
;;   but disabling it also makes macOS treat Emacs GUI frames like
;;   non-application windows (e.g. it won't capture input focus on activation),
;;   so keep it enabled there.
(when (eq system-type 'darwin)
  ;; NOTE: Don't try to undo the hack below, as it may change without warning.
  ;;   Instead, toggle `menu-bar-mode' (or put it on a hook) as normal. This
  ;;   hack will always try to respect the state of `menu-bar-mode'.
  ;; (early-init.el pushes the menu-bar-lines entry; absent in bare batch)
  (when-let* ((entry (assq 'menu-bar-lines default-frame-alist)))
    (setcdr entry 'tty))
  (defun doom--init-menu-bar-on-macos-h (&optional frame)
    (if (eq (frame-parameter frame 'menu-bar-lines) 'tty)
        (set-frame-parameter frame 'menu-bar-lines
                             (if (display-graphic-p frame) 1 0))))
  (add-hook 'after-make-frame-functions #'doom--init-menu-bar-on-macos-h))


;;; ** Scrolling

;; DEVIATION: scroll-conservatively and scroll-margin are owned by the
;; ultra-scroll setup (modules/general) and the user layer.
(setq hscroll-margin 2
      hscroll-step 1
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;; ** Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;; ** Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;; ** Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, can upset tiling window
;; managers, and can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUI dialogs are inconsistent across systems, so use Emacs prompts
;;   instead of GUI popups.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)


;;; ** Minibuffer

;; Hide irrelevant commands in M-x menu.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area.
(setq resize-mini-windows 'grow-only
      tooltip-resize-echo-area t)

;; Typing yes/no is obnoxious when y/n will do
(setq use-short-answers t)
;; HACK: By default, SPC = yes when `y-or-n-p' prompts you. This seems too easy
;;   to hit by accident, especially with SPC as our default leader key.
(define-key y-or-n-p-map " " nil)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; ** Line Numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes (the user layer removes
;; these again - kept for Doom parity so that removal stays meaningful).
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)


;;
;;; * Keybind config

(cond
 ((featurep :system 'macos)
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 ((featurep :system 'windows)
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; HACK: Emacs can't distinguish C-i from TAB, or C-m from RET, in either GUI or
;;   TTY frames. This is a byproduct of its history with the terminal. Emacs has
;;   separate input events for many contentious keys like TAB and RET (like
;;   [tab] and [return]), which are only triggered in GUI frames, so here, I
;;   create one for C-i. Won't work in TTY frames.
(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   key-translation-map fallback
   (cmd! (if (when-let* ((keys (this-single-command-raw-keys)))
               (and (display-graphic-p)
                    (not (cl-loop for event in events
                                  if (cl-position event keys)
                                  return t))
                    ;; Use FALLBACK if nothing is bound to KEY, otherwise
                    ;; we've broken all pre-existing FALLBACK keybinds.
                    (key-binding
                     (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                              key) nil t)))
             key fallback))))


;;; ** Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like diff-hl and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'doom-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))


;;
;;; * MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar doom-inhibit-local-var-hooks nil)

(defun doom-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless (or doom-inhibit-local-var-hooks
              delay-mode-hooks
              ;; Don't trigger local-vars hooks in temporary (internal) buffers
              (string-prefix-p
               " " (buffer-name (or (buffer-base-buffer)
                                    (current-buffer)))))
    (setq-local doom-inhibit-local-var-hooks t)
    ;; Show some rudimentary documentation for anyone wanting to understand
    ;; where these hooks came from.
    (let* ((hook-var (intern (format "%s-local-vars-hook" major-mode))))
      (unless (boundp hook-var)
        (set hook-var nil))
      (unless (get hook-var 'variable-documentation)
        (put hook-var 'variable-documentation
             (format (concat "Hooks to run after file/dir local variables are set in `%s', well after `%s-hook'.\n\n"
                             "These hooks are defined and executed by `doom-run-local-var-hooks-h'.")
                     major-mode major-mode)))
      (doom-run-hooks hook-var))))

;; If the user has disabled `enable-local-variables', then
;; `hack-local-variables-hook' is never triggered, so we trigger it at the end
;; of `after-change-major-mode-hook':
(defun doom-run-local-var-hooks-maybe-h ()
  "Run `doom-run-local-var-hooks-h' if `enable-local-variables' is disabled."
  (unless enable-local-variables
    (doom-run-local-var-hooks-h)))

(unless noninteractive
  (add-hook 'after-change-major-mode-hook #'doom-run-local-var-hooks-maybe-h 100)
  (add-hook 'hack-local-variables-hook #'doom-run-local-var-hooks-h))


;;
;;; * Built-in packages

;;;###package autorevert
;; revert buffers when their files/state have changed
(add-hook 'doom-first-file-hook #'doom-auto-revert-mode)
(autoload 'doom-auto-revert-mode "autorevert" nil t)
(with-eval-after-load 'autorevert
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; PERF: `auto-revert-mode' and `global-auto-revert-mode' would, normally,
  ;;   abuse the heck out of file watchers _or_ aggressively poll your buffer
  ;;   list every X seconds. Doom does this lazily instead: visible buffers are
  ;;   reverted when a file is saved or Emacs is refocused; buried buffers when
  ;;   they are switched to.
  (define-minor-mode doom-auto-revert-mode
    "A more performant alternative to `global-auto-revert-mode'."
    :global t
    :group 'autorevert
    (when global-auto-revert-mode
      (setq doom-auto-revert-mode nil))
    (let ((fn (if doom-auto-revert-mode #'add-hook #'remove-hook)))
      (funcall fn 'doom-switch-buffer-hook #'doom-auto-revert-buffer-h)
      (funcall fn 'doom-switch-window-hook #'doom-auto-revert-buffer-h)
      (funcall fn 'doom-switch-frame-hook #'doom-auto-revert-buffers-h)
      (funcall fn 'after-save-hook #'doom-auto-revert-buffers-h)))

  (defvar auto-revert-mode)
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode
                (active-minibuffer-window)
                (and buffer-file-name
                     auto-revert-remote-files
                     (file-remote-p buffer-file-name nil t)))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    ;; DEVIATION: inlined (doom-visible-buffers) - buffers lib isn't ported.
    (dolist (frame (visible-frame-list))
      (dolist (window (window-list frame))
        (with-current-buffer (window-buffer window)
          (doom-auto-revert-buffer-h))))))


;;;###package comint
(with-eval-after-load 'comint
  (setq-default comint-buffer-maximum-size 2048)  ; double the default

  ;; UX: Temporarily disable undo history between command executions. Otherwise,
  ;;   undo could destroy output while it's being printed or delete buffer
  ;;   contents past the boundaries of the current prompt.
  (add-hook 'comint-exec-hook #'buffer-disable-undo)
  (defadvice! doom--comint-enable-undo-a (process _string)
    :after #'comint-output-filter
    (unless buffer-read-only  ; don't affect output-only buffers like `compilation-mode'
      (with-current-buffer (process-buffer process)
        (when-let* ((start-marker comint-last-output-start))
          (when (and (< start-marker
                        (or (if process (process-mark process))
                            (point-max-marker)))
                     (eq (char-before start-marker) ?\n))
            (buffer-enable-undo)
            (setq buffer-undo-list nil))))))

  ;; Protect prompts from accidental modifications.
  (setq-default comint-prompt-read-only t)

  ;; UX: Prior output in shell and comint shells (like ielm) should be
  ;;   read-only. Otherwise, it's trivial to make edits in visual modes (like
  ;;   evil's) and leave the buffer in a half-broken state.
  (defadvice! doom--comint-protect-output-in-visual-modes-a (process _string)
    :after #'comint-output-filter
    (with-current-buffer (process-buffer process)
      (let ((start-marker comint-last-output-start)
            (end-marker (process-mark process)))
        (when (and start-marker (< start-marker end-marker))
          (let ((inhibit-read-only t))
            ;; Make all past output read-only (disallow buffer modifications)
            (add-text-properties comint-last-input-start (1- end-marker) '(read-only t))
            ;; Disallow interleaving.
            (remove-text-properties start-marker (1- end-marker) '(rear-nonsticky))
            ;; Make sure that at `max-point' you can always append. Important for
            ;; bad REPLs that keep writing after giving us prompt (e.g. sbt).
            (add-text-properties (1- end-marker) end-marker '(rear-nonsticky t))
            ;; Protect fence (newline of input, just before output).
            (when (eq (char-before start-marker) ?\n)
              (remove-text-properties (1- start-marker) start-marker '(rear-nonsticky))
              (add-text-properties (1- start-marker) start-marker '(read-only t))))))))

  ;; UX: If the user is anywhere but the last prompt, typing should move them
  ;;   there instead of unhelpfully spew read-only errors at them.
  (defun doom--comint-move-cursor-to-prompt-h ()
    (and (eq this-command 'self-insert-command)
         comint-last-prompt
         (> (cdr comint-last-prompt) (point))
         (goto-char (cdr comint-last-prompt))))

  (add-hook! 'comint-mode-hook
    (defun doom--comint-init-move-cursor-to-prompt-h ()
      (unless buffer-read-only  ; don't affect output-only buffers like `compilation-mode'
        (add-hook 'pre-command-hook #'doom--comint-move-cursor-to-prompt-h
                  nil t)))))


;;;###package compile
(with-eval-after-load 'compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-max-output-line-length nil  ; slows down verbose processes
        compilation-scroll-output 'first-error)
  ;; DEVIATION: upstream keeps a pre-28 ansi-color fallback; not needed here.
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and grind Emacs' GC to a halt or crash.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook! 'compilation-filter-hook
    (defun doom-comint-truncate-buffer-h (&optional _string)
      "Rate-limit `comint-truncate-buffer' in compilation-mode buffers."
      (if (> (buffer-size)
             ;; HACK: Approximate this because counting lines is prohibitively
             ;;   expensive in longer buffers.
             (* 80 comint-buffer-maximum-size))
          (let ((gc-cons-threshold most-positive-fixnum)
                (gc-cons-percentage 1.0))
            (with-silent-modifications
              (comint-truncate-buffer)))))))


;;;###package ediff
(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar doom--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (add-hook! 'ediff-before-setup-hook
    (defun doom-ediff-save-wconf-h ()
      (setq doom--ediff-saved-wconf (current-window-configuration))))
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun doom-ediff-restore-wconf-h ()
      (when (window-configuration-p doom--ediff-saved-wconf)
        (set-window-configuration doom--ediff-saved-wconf)))))


;;;###package ffap
;; Emacs 30.2 made this the default; keep it for the 30.1 floor.
(setq ffap-machine-p-known 'accept) ; don't ping domains


;;;###package paren
;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(add-hook 'doom-first-buffer-hook #'show-paren-mode)


;;;###package project
;; (project-list-file is set in doom-compat's quarantine)
(with-eval-after-load 'project
  ;; Not valid vc backends, but used to inform (global) file index exclusions.
  (add-to-list 'project-vc-backend-markers-alist '(Jujutsu . ".jj"))
  (add-to-list 'project-vc-backend-markers-alist '(Sapling . ".sl"))
  (add-to-list 'project-vc-extra-root-markers ".jj"))


;;;###package recentf
;; Keep track of recently opened files
(doom-load-packages-incrementally '(easymenu tree-widget timer recentf))
;; (recentf-save-file is set in doom-compat's quarantine)
(add-hook 'doom-first-file-hook #'recentf-mode)
(autoload 'recentf-open-files "recentf" nil t)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 200) ; default is 20

  ;; Anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))

  ;; PERF: Text properties inflate the size of recentf's files, and there is no
  ;;   reason to persist them (must be first in `recentf-filename-handlers'!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; UX: Reorder the recent files list by frecency (i.e. every time you touch a
  ;;   buffer, bump it to the top of the list).
  (add-hook! '(doom-switch-window-hook write-file-functions)
    (defun doom--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))
  (add-hook! 'dired-mode-hook
    (defun doom--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory)))

  ;; The most sensible time to clean and save your recent files list is when you
  ;; quit Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup 'never)
  (when (daemonp)
    (setq recentf-auto-cleanup 600
          recentf-autosave-interval 1200))
  ;; Use a negative depth value because we need `recentf-cleanup' to run before
  ;; `recentf-save-list' to be effective, which `recentf-mode' will only add to
  ;; `kill-emacs-hook' once it is enabled.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -50)

  (defadvice! doom--shut-up-recentf-load-a (fn &rest args)
    "Otherwise `load-file' calls in `recentf-load-list' pollute *Messages*."
    :around #'recentf-load-list
    (quiet! (apply fn args))))


;;;###package savehist
;; persist variables across sessions
;; (savehist-file comes from the quarantine; modules/completion enables the
;; mode - only the settings live here)
(doom-load-packages-incrementally '(custom))
(with-eval-after-load 'savehist
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (add-hook! 'savehist-save-hook
    (defun doom-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring' to reduce savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))
    (defun doom-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))


;;;###package saveplace
;; persistent point location in buffers
;; (save-place-file comes from the quarantine)
;; DEVIATION: upstream enables savehist-mode and configures after 'savehist
;; here (copy-paste bug); corrected to saveplace.
(add-hook 'doom-first-input-hook #'save-place-mode)
(with-eval-after-load 'saveplace
  (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! doom--inhibit-saveplace-in-long-files-a (fn &rest args)
    :around #'save-place-to-alist
    (unless (bound-and-true-p so-long-minor-mode)
      (apply fn args)))

  (defadvice! doom--inhibit-saveplace-if-point-not-at-bol-a (&rest _)
    "If something else has moved point, don't try to move it again."
    :before-while #'save-place-find-file-hook
    (bobp))

  (defadvice! doom--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so this replaces calls to `pp' with the much faster `prin1'."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)) (funcall fn))))


;;;###package so-long
(defvar doom-file-lines-threshold-alist
  `(("." . ,(cond ((fboundp 'igc-info) 25000)
                  ((featurep 'native-compile) 20000)
                  (15000))))
  "An alist mapping regexps (like `auto-mode-alist') to line number thresholds.

If a file is opened and discovered to have more lines than this,
`so-long-minor-mode' is enabled to prevent Emacs from hanging, crashing or
becoming unusably slow.  Used by `doom-so-long-p'.")

(when (fboundp 'buffer-line-statistics)
  (add-hook 'doom-first-file-hook #'global-so-long-mode)
  (with-eval-after-load 'so-long
    (unless (featurep 'native-compile)
      (setq so-long-threshold 5000))

    ;; HACK: I exploit so-long to implement a "large file" minor mode that
    ;;   activates if a file is too large or has lines whose width exceed
    ;;   `so-long-threshold' (particularly minified files), and disables
    ;;   non-essential functionality to speed Emacs up.
    (defun doom-so-long-p ()
      "A `so-long-predicate' to determine if the current buffer is too large.

This is determined by the longest line (whether it exceeds `so-long-threshold')
and whether the line count of the buffer exceeds that matching entry in
`doom-file-lines-threshold-alist' (defaulting to 20k lines)."
      (unless
          ;; HACK: Prevent so-long in places where we don't want it, like
          ;;   special buffers (e.g. magit status) or temp buffers.
          ;;   DEVIATION: doom-temp-buffer-p/doom-special-buffer-p inlined.
          (let ((bname (buffer-name (or (buffer-base-buffer) (current-buffer)))))
            (or (string-prefix-p " " bname)
                (string-match-p "\\`\\*.+\\*\\'" bname)))
        (let ((stats (buffer-line-statistics)))
          (or (> (cadr stats) so-long-threshold)
              (and buffer-file-name
                   (when-let* ((maxlines
                                (assoc-default buffer-file-name doom-file-lines-threshold-alist
                                               #'string-match-p)))
                     (> (car stats) maxlines)))))))
    (setq so-long-predicate #'doom-so-long-p
          so-long-function #'turn-on-so-long-minor-mode
          so-long-revert-function #'turn-off-so-long-minor-mode)

    (add-to-list 'so-long-target-modes 'conf-mode)
    (add-to-list 'so-long-target-modes 'text-mode)

    ;; Don't disable syntax highlighting and line numbers, or make the buffer
    ;; read-only, in `so-long-minor-mode', so we can have a basic editing
    ;; experience in them, at least. It will remain off in `so-long-mode',
    ;; however, because long files have a far bigger impact on Emacs performance.
    (cl-callf2 delq 'font-lock-mode so-long-minor-modes)
    (cl-callf2 delq 'display-line-numbers-mode so-long-minor-modes)
    (setf (alist-get 'buffer-read-only so-long-variable-overrides nil t) nil)
    ;; ...but at least reduce the level of syntax highlighting
    (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
    ;; ...and insist that save-place not operate in large/long files
    (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
    ;; But disable everything else that may be unnecessary/expensive for large
    ;; or wide buffers.
    (cl-callf append so-long-minor-modes
      '(spell-fu-mode
        eldoc-mode
        better-jumper-local-mode
        ws-butler-mode
        auto-composition-mode
        undo-tree-mode
        highlight-indent-guides-mode
        hl-fill-column-mode
        ;; These are redundant on Emacs 29+
        flycheck-mode
        smartparens-mode
        smartparens-strict-mode))))


;;;###package transient
;; (transient-{levels,values,history}-file come from the quarantine)
(with-eval-after-load 'transient
  (setq transient-default-level 5)
  ;; Pop up transient windows at the bottom of the window where it was invoked.
  ;; This is more ergonomic for users with large displays or many splits.
  (setq transient-display-buffer-action
        '(display-buffer-below-selected
          (dedicated . t)
          (inhibit-same-window . t))
        transient-show-during-minibuffer-read t)
  ;; Universal ESC behavior for popups.
  (define-key transient-map [escape] #'transient-quit-one))

(provide 'doom-defaults)
;;; doom-defaults.el ends here
