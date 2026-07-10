;;; custom/general/autoload/expreg.el -*- lexical-binding: t; -*-

;;;###autoload
(defun expreg--line ()
  "Return a list of regions containing surrounding sentences."
  (ignore-errors
    (let (beg end)
      (end-of-visual-line)
      (setq end (point))
      (beginning-of-visual-line)
      (setq beg (point))
      `((line . ,(cons beg end))))))

;;;###autoload
(defun expreg--markdown-subtree ()
  "Return regions for each enclosing Markdown subtree, innermost outward.
Producer for `expreg-functions': emits a region per ancestor heading
so that `expreg-expand' can grow the selection up to the whole section.
Uses a regex-based heading predicate so it works even before jit-lock
has fontified the current window."
  (when (derived-mode-p 'markdown-mode)
    (save-excursion
      (let ((at-heading-p
             (lambda ()
               (save-excursion
                 (beginning-of-line)
                 (and (looking-at-p markdown-regex-header)
                      (not (markdown-code-block-at-point-p))))))
            regions (continue t))
        (unless (funcall at-heading-p)
          (ignore-errors (markdown-previous-visible-heading 1)))
        (while (and continue (funcall at-heading-p))
          (beginning-of-line)
          (let ((beg (point)))
            (save-excursion
              (markdown-end-of-subtree)
              (push (cons 'markdown-subtree (cons beg (point))) regions)))
          (let ((p (point)))
            (setq continue
                  (and (ignore-errors (markdown-up-heading 1) t)
                       (/= (point) p)))))
        regions))))

;; Ported ahead of lisp/sexp-transient.el (its original home in doom.d) -
;; move, don't duplicate, when that lib restores.
(defun transient-layout-keys (prefix)
  "Return all keys in PREFIX's static layout.
Walks both layout dialects, like the tests' walker: suffix plists live
in the cdr (transient >= 0.8) or nested one level down (0.7.x)."
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

;;;###autoload
(transient-define-prefix expreg-transient ()
  "expand/contract"
  [[("v" "expand" expreg-expand :transient t)]
   [("V" "contract" expreg-contract :transient t)]]
  ["bypass keys"
   :class transient-column
   :hide always
   :setup-children
   (lambda (_)
     (transient-bypass-keys
      'expreg-transient
      '("d" "p" "P" "r" "c" "R" "t" "T" "f" "F" "n" "C-;"
        "SPC" "," ":" "M-x" "M-:" "`" "C-h" "C-x TAB"
        "s-k" "s-]" "s-j" "s-]"
        ">" "<" "=" "~"  "[" "]" "J" "s" "z"
        ("*" nil evil-ex-search-word-forward)
        ("#" nil evil-ex-search-word-backward)
        ("j" t evil-next-visual-line)
        ("k" t evil-previous-visual-line)
        ("h" t evil-backward-char)
        ("l" t evil-forward-char)
        ("%" t evil-jump-item)
        ("0" t evil-beginning-of-line)
        ("y" t evil-yank)
        ("o" t exchange-point-and-mark)
        ("C-l" t) ("C-e" t)  ("C-y" t)
        ("w" t) ("W" t) ("b" t) ("B" t)  ("$" t)
        ("/" t) ("{" t) ("}" t)
        ("g" t evil-goto-first-line) ("G" t evil-goto-line)
        ("x" nil (lambda () (interactive) (general--simulate-keys nil "SPC x"))))))]
  ["Misc"
   :hide (lambda () (not transient-show-common-commands))
   [("u" (lambda () (interactive) (undo) (evil-visual-restore)) :transient t)]
   [("C-r" (lambda () (interactive) (undo-redo) (evil-visual-restore)) :transient t)]]
  ["Org Mode"
   :if (lambda () (derived-mode-p 'org-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("; *" "bold" (lambda () (interactive) (org-emphasize ?*)))
    ("; b" "bold" (lambda () (interactive) (org-emphasize ?*)))
    ("; /" "italic" (lambda () (interactive) (org-emphasize ?\/)))
    ("; i" "italic" (lambda () (interactive) (org-emphasize ?\/)))
    ("; _" "underline" (lambda () (interactive) (org-emphasize ?_)))
    ("; =" "verbatim" (lambda () (interactive) (org-emphasize ?=)))
    ("; `" "code" (lambda () (interactive) (org-emphasize ?~)))
    ("; +" "strikethrough" (lambda () (interactive) (org-emphasize ?+)))]
   [("C-c l" "insert link" org-insert-link)
    ("; l" "insert link" org-insert-link)
    ("; q" "wrap in quote block"
     (lambda () (interactive) (org-wrap-in-block 'quote)))
    ("; c" "wrap in source block"
     (lambda () (interactive) (org-wrap-in-block 'src)))]]
  ;; Dropped until their modules port (doom.d stays the reference, see
  ;; MIGRATION Decisions log): Markdown and Clojure sections,
  ;; vulpea-insert, the browser-url inserters, and the Magit section -
  ;; that one also collides with bypass "s"/"x", which Doom's dead
  ;; conflict guard never caught.
  )
