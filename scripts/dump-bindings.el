;;; scripts/dump-bindings.el --- key-audit binding dump -*- lexical-binding: t; -*-
;; Migration-scoped tooling (see MIGRATION.org "Key clusters").  Walks the
;; resolved leader tree plus the evil g/z/[/] prefix trees and writes one
;; EDN-compatible line per binding: ("group" "KEY DESC" "command"), plus
;; trailing flags: :void when the bound symbol has no function definition
;; (bound but broken - the SPC b s class) and :void-suffix when a transient
;; prefix's layout references undefined commands (bound but broken inside -
;; the SPC z f class).
;; Standalone vanilla elisp - safe to evaluate in any session, Doom included.
;;
;; Boot mode (KEYDUMP_OUT set), driven by `bb keydump':
;;   KEYDUMP_OUT=<file> [KEYDUMP_REQUIRE=evil,...] \
;;     emacs -nw --init-directory <root> -l scripts/dump-bindings.el
;; Mirrors smoke-check.el: dumps once elpaca settles, then kills Emacs.

(require 'cl-lib)

(defun dump-bindings--unwrap (def)
  "Resolve DEF, unwrapping menu-items, (STRING . DEF), and prefix symbols.
Prefix-command symbols come back as their keymap so the walk can descend."
  (cond
   ((and (consp def) (eq (car def) 'menu-item))
    (dump-bindings--unwrap (nth 2 def)))
   ((and (consp def) (stringp (car def)))
    (dump-bindings--unwrap (cdr def)))
   ((and def (symbolp def) (fboundp def)
         (keymapp (symbol-function def)))
    (symbol-function def))
   (t def)))

(defun dump-bindings--name (def)
  "Printable name for binding DEF, nil when DEF isn't dumpable."
  (cond
   ((and def (symbolp def)) (symbol-name def))
   ((or (stringp def) (vectorp def)) (concat "<kmacro> " (key-description def)))
   ((functionp def) "<lambda>")))

(defvar dump-bindings-load-autoloads nil
  "When non-nil, force-load autoloaded commands before layout inspection.
Transient layouts only exist once the defining file loads, so autoloaded
prefixes dump clean no matter how rotten their suffixes.  Boot mode sets
this; leave nil when dumping from a live session.")

(defun dump-bindings--transient-suffixes (node)
  "Collect suffix command symbols from a parsed transient layout NODE.
Groups are vectors [CLASS PLIST (CHILDREN)], suffixes (CLASS :command CMD
...) lists."
  (cond
   ((vectorp node)
    (mapcan #'dump-bindings--transient-suffixes (append node nil)))
   ((proper-list-p node)
    (if-let* ((cmd (plist-get (cdr node) :command)))
        (list cmd)
      (mapcan #'dump-bindings--transient-suffixes node)))))

(defun dump-bindings--void-suffix-p (def)
  "Non-nil when DEF is a transient prefix whose layout references void commands."
  (when (and (symbolp def) (fboundp def))
    (when (and dump-bindings-load-autoloads
               (autoloadp (symbol-function def)))
      (autoload-do-load (symbol-function def) def))
    (cl-some (lambda (cmd) (and (symbolp cmd) (not (fboundp cmd))))
             (dump-bindings--transient-suffixes
              (get def 'transient--layout)))))

(defun dump-bindings--walk (group keymap prefix acc)
  "Collect (GROUP keydesc name void? void-suffix?) entries from KEYMAP.
Keys sit under PREFIX; entries accumulate into ACC.  VOID? is non-nil for
symbols with no function definition, VOID-SUFFIX? for transient prefixes
with undefined suffix commands."
  (map-keymap
   (lambda (event def)
     ;; skip char ranges and default bindings
     (unless (or (consp event) (eq event t))
       (let ((def (dump-bindings--unwrap def))
             (key (vconcat prefix (vector event))))
         (cond
          ((keymapp def)
           (setq acc (dump-bindings--walk group def key acc)))
          ((dump-bindings--name def)
           (push (list group (key-description key) (dump-bindings--name def)
                       (and def (symbolp def) (not (fboundp def)))
                       (dump-bindings--void-suffix-p def))
                 acc))))))
   keymap)
  acc)

(defun dump-bindings-collect ()
  "Collect entries from the leader map and evil g/z/[/] prefix trees.
Sorted and deduplicated; shadowed duplicates keep the effective binding."
  (unless (boundp 'doom-leader-map)
    (error "dump-bindings: `doom-leader-map' is unbound - config not loaded?"))
  (let ((acc (dump-bindings--walk
              "leader" doom-leader-map
              (kbd (if (boundp 'doom-leader-key) doom-leader-key "SPC"))
              nil)))
    (pcase-dolist (`(,group . ,mapsym) '(("normal" . evil-normal-state-map)
                                         ("motion" . evil-motion-state-map)))
      (when (boundp mapsym)
        (dolist (pk '("g" "z" "[" "]"))
          (let ((km (dump-bindings--unwrap
                     (lookup-key (symbol-value mapsym) (kbd pk)))))
            (when (keymapp km)
              (setq acc (dump-bindings--walk group km (kbd pk) acc)))))))
    (sort (cl-remove-duplicates
           acc :test #'equal
           :key (lambda (e) (list (nth 0 e) (nth 1 e))))
          (lambda (a b)
            (string< (format "%s %s" (nth 0 a) (nth 1 a))
                     (format "%s %s" (nth 0 b) (nth 1 b)))))))

(defun dump-bindings (out-file)
  "Write the binding dump to OUT-FILE, one line per entry."
  (interactive "FDump bindings to: ")
  (let ((entries (dump-bindings-collect)))
    (make-directory (file-name-directory (expand-file-name out-file)) t)
    (with-temp-file out-file
      (dolist (e entries)
        (insert (format "(%S %S %S%s%s)\n" (nth 0 e) (nth 1 e) (nth 2 e)
                        (if (nth 3 e) " :void" "")
                        (if (nth 4 e) " :void-suffix" "")))))
    (message "dump-bindings: %d entries -> %s" (length entries) out-file)
    (length entries)))

;;; Boot mode

(defun dump-bindings--boot ()
  "Require KEYDUMP_REQUIRE features, dump to KEYDUMP_OUT, kill Emacs."
  (condition-case err
      (progn
        (setq dump-bindings-load-autoloads t)
        (dolist (feat (split-string (or (getenv "KEYDUMP_REQUIRE") "")
                                    "," t "[ \t]+"))
          (require (intern feat)))
        (dump-bindings (getenv "KEYDUMP_OUT"))
        (kill-emacs 0))
    (error
     (message "KEYDUMP-FAILED: %S" err)
     (kill-emacs 1))))

(when (getenv "KEYDUMP_OUT")
  ;; -l files load after `after-init-hook'; on a warm cache elpaca may
  ;; already be done - check, don't just hook (same as smoke-check.el).
  (if (bound-and-true-p elpaca-after-init-time)
      (dump-bindings--boot)
    (add-hook 'elpaca-after-init-hook #'dump-bindings--boot 99))
  ;; Watchdog: never hang the caller.
  (run-at-time
   (string-to-number (or (getenv "KEYDUMP_WATCHDOG") "600")) nil
   (lambda ()
     (message "KEYDUMP-TIMEOUT")
     (kill-emacs 124))))
