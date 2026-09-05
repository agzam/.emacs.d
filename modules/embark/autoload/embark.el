;;; modules/embark/autoload/embark.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro embark-ace-action (fn)
  "Define an embark action running FN in an ace-window-selected window."
  `(defun ,(intern (concat "embark-ace-" (symbol-name fn))) ()
     (interactive)
     (with-demoted-errors "%s"
       (require 'ace-window)
       (let ((aw-dispatch-always t))
         (aw-switch-to-window (aw-select nil))
         (call-interactively (symbol-function ',fn))))))

;;;###autoload
(defmacro embark-split-action (fn split-type)
  "Define an embark action running FN after a SPLIT-TYPE window split."
  `(defun ,(intern (concat "embark-"
                           (symbol-name fn)
                           "-"
                           (symbol-name split-type))) ()
     (interactive)
     (funcall #',split-type)
     (call-interactively #',fn)))

;;;###autoload
(defun avy-action-embark (pt)
  "Embark-act on the avy target at PT, staying in the original window.
Borrowed from
https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere"
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun edebug-instrument-symbol (symbol)
  "Edebug-instrument the function named SYMBOL."
  (interactive "sSymbol: ")
  (edebug-instrument-function (intern symbol)))

;;;###autoload
(defun embark-collect-outline-cycle (&optional arg)
  "Cycle outline visibility in an embark-collect buffer; with ARG the whole buffer."
  (interactive "P")
  (if arg (outline-cycle-buffer)
    (outline-cycle))
  (evil-beginning-of-line))

(defun rfc-number-at-point ()
  "Number of the RFC reference at point, nil when there is none.
Matches the shapes `embark-target-RFC-number-at-point' targets: RFC 123,
rfc-123, RFC123."
  (require 'org)
  (when-let* ((bounds (org-in-regexp "\\b[rR][fF][cC][- ]?[0-9]+\\b" 1)))
    (string-to-number
     (replace-regexp-in-string
      "[^0-9]" ""
      (buffer-substring-no-properties (car bounds) (cdr bounds))))))

;;;###autoload
(defun search-rfc-number-online (&optional rfc-num)
  "Open the RFC editor page for RFC-NUM in the external browser.
Reads the number at point, so the embark action lands on the reference
it was invoked from."
  (interactive (list (rfc-number-at-point)))
  (browse-url-externally
   (format
    "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s"
    (or rfc-num ""))))

;;;###autoload
(defun browse-rfc-number-at-point ()
  "Read RFC number at point in rfc-mode when available, online otherwise."
  (interactive)
  (if-let* ((rfc-num (rfc-number-at-point)))
      (if (featurep 'rfc-mode)
          (switch-to-buffer-other-window
           (rfc-mode--document-buffer rfc-num))
        (search-rfc-number-online rfc-num))
    (if (featurep 'rfc-mode)
        (rfc-mode-browse)
      (search-rfc-number-online))))

;;;###autoload
(defun embark-project-search (target)
  "Ripgrep the project for TARGET.
doom.d reaches for +vertico-file-search here; consult-ripgrep with an
initial query is the lab stand-in until the search module ports."
  (consult-ripgrep nil target))

;;;###autoload
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;;;###autoload
(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

;;;###autoload
(defun embark-preview ()
  "Preview the target: forge for GitHub topics/files, eww for urls, else dwim."
  (interactive)
  ;; reachable from vertico's C-SPC before embark ever loads
  (require 'embark)
  (when-let* ((target (car (embark--targets)))
              (type (plist-get target :type))
              (str (or (plist-get target :target) ""))
              ;; async searches have no candidate yet: acting on the empty
              ;; string crashes actions like consult-gh--pr-view-action
              ((not (string-empty-p str))))
    (cond
     ;; these preview via `consult-preview-key' already; dwim-ing them
     ;; would additionally run the default action (browser / thread capture,
     ;; and for a HN result a full page fetch - Hacker News answers the
     ;; resulting concurrent requests with HTTP 429)
     ((memq type '(github-topics-pr slacko-message consult-hn-result)) nil)
     ((and (member type '(url consult-omni))
           (string-match-p
            ;; only match PRs/Issues or individual files
            "https://github\\.com/\\([^/]+/[^/]+/\\)\\(pull\\|issues\\|blob\\)[^#\n]+"
            str))
      (cl-labels ((forge-visit-topic-url*
                    (url &rest _)
                    (forge-visit-topic-via-url url)))
        (embark--act #'forge-visit-topic-url* target nil)))

     ((member type '(url consult-omni))
      (cl-labels ((eww-browse-url*
                    (url &rest _)
                    (eww-browse-url url)))
        (embark--act #'eww-browse-url* target nil)))

     ((fboundp 'embark-dwim)
      (save-selected-window
        (let (embark-quit-after-action)
          (embark-dwim)))))))

(defvar embark-url-patterns nil
  "(TYPE PATTERN) pairs `embark-target-url-at-point' dispatches on.
Rebuilt from `embark-url-config' by `embark-setup-url-types'.")

(defconst embark-url-scheme-regexp "[a-z][a-z0-9+.-]*://"
  "Match a url scheme followed by its separator.")

(defun embark-url-at-point ()
  "Return the url at point with any wrapping link syntax stripped.
`bounds-of-thing-at-point' spans the whole [[target][description]]
construct in org buffers, and `thing-at-point' hands back the org
target verbatim, so neither yields something an action can fetch:
`org-mode' targets like elisp:(hnreader-comment \"URL\") and eww:URL
both carry the real url inside them."
  (when-let* ((raw (thing-at-point 'url t)))
    (cond
     ;; already a url; leave it whole, it may embed another one
     ((string-match-p (concat "\\`" embark-url-scheme-regexp) raw) raw)
     ((string-match (concat embark-url-scheme-regexp "[^][ \t\n\"'<>]+") raw)
      (match-string 0 raw))
     (t raw))))

(defun embark-target-url-at-point ()
  "Universal embark url resolver."
  (let ((url (embark-url-at-point))
        (bounds (bounds-of-thing-at-point 'url)))
    (when (and url bounds)
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (or
         ;; Try each pattern in order
         (cl-loop for (type pattern) in embark-url-patterns
                  when (if (functionp pattern)
                           (funcall pattern url)
                         (string-match-p pattern url))
                  return `(,type ,url . ,(cons beg end)))
         ;; Fallback to generic URL
         `(url ,url . ,(cons beg end)))))))

;;;###autoload
(defun embark-setup-url-types ()
  "Setup all URL types from `embark-url-config'."
  ;; Clear existing patterns & remove our only finder if already added
  ;; to avoid duplicates
  (setq
   embark-url-patterns nil
   embark-target-finders
   (remove 'embark-target-url-at-point embark-target-finders))
  ;; Get shared actions from nil entry
  (let ((shared-actions (plist-get (cdr (assq nil embark-url-config)) :actions)))
    (dolist (config embark-url-config)
      (let* ((type (car config))
             (plist (cdr config))
             (pattern (plist-get plist :pattern))
             (actions (plist-get plist :actions))
             (keymap-name (intern (format "%s-map" type))))

        (when type
          ;; Add pattern to our list (used by the ONE target finder)
          (add-to-list 'embark-url-patterns (list type pattern))
          ;; Create keymap for this URL type
          (set keymap-name (make-sparse-keymap))
          (set-keymap-parent (symbol-value keymap-name) embark-url-map)
          ;; Add shared actions
          (dolist (action shared-actions)
            (define-key (symbol-value keymap-name) (kbd (car action)) (cdr action)))
          ;; Add type-specific actions
          (dolist (action actions)
            (define-key (symbol-value keymap-name) (kbd (car action)) (cdr action)))
          ;; Register the keymap for this target type
          (add-to-list 'embark-keymap-alist (cons type keymap-name))))))
  ;; Register our ONE universal target finder.  `thing-at-point' reports a URL
  ;; even on bug-reference / shr / goto-address buttons (via their `*-url' text
  ;; property), so this catch-all must run AFTER the specialized finders that
  ;; prepend to the front (bug-reference-link, markdown-link, org-block, RFC,
  ;; jira, remoto) yet BEFORE embark's generic file/identifier finders that
  ;; would mis-grab a URL.  Splicing it right ahead of the file finder keeps the
  ;; slot stable no matter the module load order or how often this reruns; a
  ;; plain front `add-to-list' jumps ahead of the specialized finders on rerun.
  (cl-callf2 cons 'embark-target-url-at-point
    (nthcdr (or (cl-position 'embark-target-file-at-point embark-target-finders)
                (length embark-target-finders))
            embark-target-finders)))

;;;###autoload
(defun embark-target-org-block ()
  "Target any org block at point."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((case-fold-search t)
            (pos (point)))
        (beginning-of-line)
        ;; Search forward for ANY #+end_ from current line
        (when (re-search-forward "^[ \t]*#\\+end_\\(\\S-+\\)" nil t)
          (let ((end (line-end-position))
                (block-type (match-string 1)))
            ;; Now search backward for the matching #+begin_
            (when (re-search-backward (format "^[ \t]*#\\+begin_%s" (regexp-quote block-type)) nil t)
              (let ((begin (match-beginning 0)))
                ;; Verify our original position was inside this block
                (when (and (<= begin pos) (<= pos end))
                  `(org-block ,block-type ,begin . ,end))))))))))

;;;###autoload
(defun embark-org-block-convert (target-type)
  "Convert org block at point to TARGET-TYPE."
  (when-let* ((targets (embark--targets))
              (target (car targets)))
    (let* ((bounds (plist-get target :bounds))
           (begin (car bounds))
           (end (cdr bounds))
           (current-type (string-trim (plist-get target :target))))
      (save-excursion
        ;; First find and replace the #+end_ line (do this first!)
        (goto-char begin)
        (when (re-search-forward (format "^[ \t]*#\\+end_%s"
                                         (regexp-quote current-type))
                                 end t)
          (replace-match (format "#+end_%s" target-type) t t))
        ;; Now go back and replace the #+begin_ line
        (goto-char begin)
        (when (re-search-forward (format "^[ \t]*#\\+begin_%s\\(.*\\)$"
                                         (regexp-quote current-type))
                                 end t)
          (let ((params (match-string 1)))
            (replace-match (format "#+begin_%s%s"
                                   target-type
                                   (if (string= target-type "src")
                                       params
                                     ""))
                           t t)))))))

;;;###autoload
(defun embark-org-block-convert-to-src ()
  "Convert current block to src block."
  (interactive)
  (embark-org-block-convert "src"))

;;;###autoload
(defun embark-org-block-convert-to-example ()
  "Convert current block to example block."
  (interactive)
  (embark-org-block-convert "example"))

;;;###autoload
(defun embark-org-block-convert-to-quote ()
  "Convert current block to quote block."
  (interactive)
  (embark-org-block-convert "quote"))

(defun embark--ephemeral-cleanup (&rest _)
  "One-shot post-action hook: unhook itself, then exit the minibuffer.
The nested timers keep the action's target window (eww etc.) selected
through the minibuffer teardown."
  (setq embark-post-action-hooks
        (remove (list t 'embark--ephemeral-cleanup)
                embark-post-action-hooks))
  (run-with-timer
   0.1 nil
   (lambda ()
     (let ((w (selected-window)))
       (run-with-timer
        0.1 nil
        (lambda (w) (select-window w)) w)
       (exit-minibuffer)))))

;;;###autoload
(defun embark-ephemeral-act (text)
  "Act on TEXT using Embark via minibuffer interaction."
  (interactive)
  ;; reachable via browser-tab-act before embark ever loads
  (require 'embark)
  ;; I have to override default-action, otherwise it's hijacking
  ;; whatever calls this function, making "RET" not to work in embark-act
  (cl-letf (((symbol-function 'embark--default-action)
             (lambda (x)
               (lookup-key (embark--raw-action-keymap x) "\r")))
            (embark-quit-after-action nil)
            ;; Prevent minibuffer from restoring window config on exit
            (read-minibuffer-restore-windows nil)
            (minibuffer-exit-hook nil))
    (push (list t 'embark--ephemeral-cleanup) embark-post-action-hooks)
    (run-with-timer 0.1 nil #'embark-act)
    (read-string "Act on: " text)))
