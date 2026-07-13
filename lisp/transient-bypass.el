;;; transient-bypass.el --- pass-through keys for transients -*- lexical-binding: t; -*-
;;; Commentary:
;; Shared helper for transient prefixes (expreg-transient, sexp-transient)
;; that want some keys to behave exactly as they do outside the transient,
;; honoring major/minor mode maps.
;;
;; Ported from doom.d lisp/sexp-transient.el, where both transients pulled it
;; in.  The layout walker is a dialect-aware rewrite: the original's
;; cl-typep/oref walk never matched spec-list layouts, so its key-conflict
;; guard was dead in Doom all along.
;;; Code:

(require 'cl-lib)
(require 'transient)

(defun transient-layout-keys (prefix)
  "Return all keys in PREFIX's static layout.
Walks both layout dialects: suffix plists live in the cdr (transient >= 0.8)
or nested one level down (0.7.x)."
  (let (keys)
    (cl-labels
        ((walk (node)
           (cond
            ((vectorp node) (mapc #'walk (append node nil)))
            ((not (proper-list-p node)) nil)
            ((keywordp (car node))
             (when-let* ((key (plist-get node :key)))
               (push key keys)))
            (t (if-let* ((key (plist-get (cdr node) :key)))
                   (push key keys)
                 (mapc #'walk node))))))
      (mapc #'walk (get prefix 'transient--layout)))
    (nreverse keys)))

(defun transient-bypass-keys (prefix key-specs)
  "Create transient suffixes for PREFIX that act like plain keys.
Sometimes a transient should let a key do exactly what it does outside
of it, honoring major/minor mode maps.  Every spec in KEY-SPECS is
either a string - invoke the command the key normally binds, exiting
the transient - or (KEY TRANSIENT? &optional CMD) to stay in the
transient and optionally call an explicit CMD."
  (let ((existing-keys (transient-layout-keys prefix)))
    (dolist (spec key-specs)
      (let ((key (if (stringp spec) spec (car spec))))
        (when (member key existing-keys)
          (error "transient-bypass-keys: key %S conflicts with an explicit binding in `%S'"
                 key prefix)))))
  (transient-parse-suffixes
   prefix
   (mapcar
    (lambda (key-map)
      (let* ((key (if (stringp key-map) key-map (car key-map)))
             (explicit-cmd (ignore-errors (nth 2 key-map)))
             (transient? (and (listp key-map) (cadr key-map)))
             (cmd (or explicit-cmd
                      (lambda ()
                        (interactive)
                        (if transient?
                            (call-interactively
                             (or
                              (lookup-key evil-normal-state-map (kbd key))
                              (lookup-key evil-motion-state-map (kbd key))
                              (lookup-key evil-visual-state-map (kbd key))
                              (lookup-key (current-local-map) (kbd key))
                              (lookup-key global-map (kbd key))))
                          (general--simulate-keys nil key)))))
             (desc (format "%s" key)))
        (list key desc cmd :transient transient?)))
    key-specs)))

(provide 'transient-bypass)
;;; transient-bypass.el ends here
