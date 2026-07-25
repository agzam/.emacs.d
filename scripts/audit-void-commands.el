;;; scripts/audit-void-commands.el --- void-command audit -*- lexical-binding: t; -*-
;; Load on top of a full --batch boot (same pattern as elpaca-update.el):
;;   emacs -Q --batch --init-directory <root> \
;;     -l <root>/early-init.el -l <root>/init.el \
;;     -l <root>/scripts/audit-void-commands.el
;; Driven by `bb audit'.  Finds commands that error only when finally
;; pressed: bound or referenced symbols that are neither defined nor
;; autoloaded once the whole config is up.  Two surfaces:
;;   1. suffix :command entries of every in-tree `transient-define-prefix'
;;   2. the leader keymap tree (doom-leader-map)
;; A suffix (or group) carrying any :if* predicate is exempt from the
;; verdict - transient skips init for it when the predicate fails, so it
;; cannot break the prefix; it is still printed as a note since the guard
;; owner must ensure the predicate implies availability.  A prefix whose
;; definer never ran in this boot (defined inside an unloaded package's
;; `after!'/:config block) is listed as skipped, not silently passed.
;; Exit 0 when nothing unguarded is void, 1 otherwise.

(require 'cl-lib)
(require 'elpaca)
(require 'transient)

(elpaca-wait)                       ; all package autoloads registered

(defvar audit-root (file-name-as-directory user-emacs-directory))
(defvar audit-load-errors nil)
(defvar audit-findings nil)
(defvar audit-notes nil)
(defvar audit-skipped nil)

;; Autoload files load lazily in normal use; here every one is loaded in
;; full so their transients and helpers exist for inspection.  A file that
;; cannot load is itself a finding (broken require chain).
(dolist (f (doom-glob audit-root "modules/*/autoload/*.el"))
  (condition-case err
      (load f nil 'nomessage)
    (error (push (cons (file-relative-name f audit-root) err)
                 audit-load-errors))))

(defun audit-scan-prefix-names (files)
  "Return (SYMBOL . FILE) for every transient-define-prefix in FILES."
  (let (names)
    (dolist (f files)
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (while (re-search-forward
                "^\\s-*(transient-define-prefix \\([^ ()\n]+\\)" nil t)
          (push (cons (intern (match-string 1)) (file-relative-name f audit-root))
                names))))
    (nreverse names)))

(defun audit-plist-shaped-p (x)
  (and (consp x) (keywordp (car x))))

(defun audit-plist-guarded-p (plist)
  "Non-nil when PLIST carries any :if* key (transient skips init on those)."
  (let (found)
    (while (and plist (not found))
      (when (and (keywordp (car plist))
                 (string-prefix-p ":if" (symbol-name (car plist))))
        (setq found t))
      (setq plist (cddr plist)))
    found))

(defun audit-layout-commands (tree)
  "Collect (COMMAND . GUARDED) conses from a parsed transient layout TREE.
Handles the parsed shapes: group vectors [CLASS PLIST CHILDREN...],
suffix lists (CLASS :key ... :command CMD ...), plain child lists."
  (let (cmds)
    (cl-labels
        ((walk (node guarded)
           (cond
            ((vectorp node)
             ;; a plist-shaped element guards the whole group's subtree
             (let ((guarded (or guarded
                                (cl-some (lambda (el)
                                           (and (audit-plist-shaped-p el)
                                                (audit-plist-guarded-p el)))
                                         (append node nil)))))
               (cl-loop for el across node
                        unless (audit-plist-shaped-p el)
                        do (walk el guarded))))
            ((and (consp node) (symbolp (car node)) (keywordp (cadr node)))
             ;; suffix entry: trailing plist after the class symbol
             (let* ((plist (cdr node))
                    (guarded (or guarded (audit-plist-guarded-p plist)))
                    (cmd (plist-get plist :command)))
               (when (and cmd (symbolp cmd))
                 (push (cons cmd guarded) cmds))))
            ((proper-list-p node)
             (dolist (el node) (walk el guarded))))))
      (walk tree nil))
    (nreverse cmds)))

(defun audit-command-problem (cmd)
  "Return a problem keyword for CMD, or nil when it is a live command."
  (cond ((not (fboundp cmd)) 'void)
        ((not (commandp cmd)) 'non-interactive)))

;; 1. transients
(let ((prefixes (audit-scan-prefix-names
                 (append (doom-glob audit-root "modules/*/autoload/*.el")
                         (doom-glob audit-root "modules/*/autoload.el")
                         (doom-glob audit-root "modules/*/config.el")
                         (doom-glob audit-root "lisp/*.el")
                         (doom-glob audit-root "config.el")))))
  (pcase-dolist (`(,sym . ,file) prefixes)
    (if-let* ((layout (get sym 'transient--layout)))
        (pcase-dolist (`(,cmd . ,guarded) (audit-layout-commands layout))
          (when-let* ((problem (audit-command-problem cmd)))
            (if guarded
                (push (format "%s [%s]: %s is %s behind an :if guard"
                              sym file cmd problem)
                      audit-notes)
              (push (format "%s [%s]: %s is %s" sym file cmd problem)
                    audit-findings))))
      (push (format "%s [%s]" sym file) audit-skipped))))

;; 2. leader keymap tree
(defun audit-walk-keymap (map path)
  "Push findings for void/non-interactive bindings reachable from MAP."
  (map-keymap
   (lambda (key binding)
     (let ((desc (condition-case nil
                     (key-description (vector key))
                   (error (format "%s" key))))
           (target binding))
       ;; evil-make-overriding-map bookkeeping entry, not a binding
       (unless (string-match-p "override-state" desc)
         ;; unwrap menu-item / (DESC . DEF) conses down to the definition
         (while (and (consp target)
                     (or (eq (car target) 'menu-item)
                         (stringp (car target))))
           (setq target (if (eq (car target) 'menu-item)
                            (nth 2 target)
                          (cdr target))))
         (cond
          ((keymapp target)
           (audit-walk-keymap
            (if (symbolp target) (symbol-function target) target)
            (concat path " " desc)))
          ((and target (symbolp target)
                (not (memq target '(ignore undefined digit-argument))))
           (when-let* ((problem (audit-command-problem target)))
             (push (format "leader \"%s%s\": %s is %s"
                           (string-trim path) (concat " " desc) target problem)
                   audit-findings)))))))
   map))

(when (boundp 'doom-leader-map)
  (audit-walk-keymap doom-leader-map ""))

;; report
(when audit-load-errors
  (princ "autoload files that failed to load in full:\n")
  (pcase-dolist (`(,f . ,err) audit-load-errors)
    (princ (format "  %s: %s\n" f (error-message-string err)))))
(when audit-skipped
  (princ (format "skipped (definer not run this boot): %d prefix(es)\n"
                 (length audit-skipped)))
  (dolist (s (nreverse audit-skipped))
    (princ (format "  %s\n" s))))
(when audit-notes
  (princ (format "guarded references (verify the guard implies availability): %d\n"
                 (length audit-notes)))
  (dolist (n (nreverse audit-notes))
    (princ (format "  %s\n" n))))
(if (or audit-findings audit-load-errors)
    (progn
      (princ (format "VOID-COMMANDS: %d finding(s)\n" (length audit-findings)))
      (dolist (f (nreverse audit-findings))
        (princ (format "  %s\n" f)))
      (kill-emacs 1))
  (princ "VOID-COMMANDS: none\n")
  (kill-emacs 0))
