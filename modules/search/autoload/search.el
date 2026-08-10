;;; modules/search/autoload/search.el -*- lexical-binding: t; -*-

(defvar search-github-mode->lang
  '(((clojurescript clojure cider-clojure-interaction) . "Clojure")
    ((emacs-lisp Info lisp-data helpful) . "Emacs Lisp")
    ((tsx-ts) . "TypeScript")
    ((js jtsx-jsx) . "JavaScript")
    ((fennel) . "Fennel"))
  "Major-mode families -> GitHub code-search language terms.")

;;;###autoload
(defun search-github-with-lang ()
  "Search GitHub code, guessing the language from the current mode."
  (interactive)
  (let* ((lang (cl-some (lambda (entry)
                          (when (cl-intersection
                                 (seq-map #'symbol-name
                                          (derived-mode-all-parents major-mode))
                                 (seq-map (lambda (x) (concat (symbol-name x) "-mode"))
                                          (car entry))
                                 :test #'equal)
                            (cdr entry)))
                        search-github-mode->lang))
         (lang-term (if lang (concat "language:\"" lang "\" ") ""))
         (word-at-point (if (region-active-p)
                            (buffer-substring (region-beginning) (region-end))
                          (thing-at-point 'symbol)))
         (search-term (read-string "Search Github: " (concat lang-term word-at-point)))
         (query (format "https://github.com/search?q=%s&type=code"
                        (url-hexify-string search-term))))
    (browse-url query)))

;;;###autoload
(defun zoxide-find (&optional query)
  "Use zoxide to open a directory with dired."
  (interactive "P")
  (if (not (executable-find "zoxide"))
      (error "zoxide executable cannot be found")
    (require 'consult)
    (let* ((items (thread-last
                    (or query "")
                    (format "zoxide query --list '%s'")
                    shell-command-to-string
                    ((lambda (s) (split-string s "\n")))
                    (seq-remove #'string-blank-p)))
           (path (or (and (length= items 1) (car-safe items))
                     (consult--read
                      items
                      :prompt "Choose: "
                      :sort nil
                      :initial query))))
      (if (eq major-mode 'eshell-mode)
          path
        (find-file path)))))

;;;###autoload
(defun add-to-zoxide-cache ()
  "Teach zoxide the current buffer's directory."
  (when-let* ((dir (if (eq major-mode 'dired-mode)
                       dired-directory
                     (and buffer-file-name
                          (file-name-directory buffer-file-name))))
              ((stringp dir))
              ((file-readable-p dir)))
    (call-process-shell-command
     (format "zoxide add \"%s\"" dir))))

(defun consult-line-collect-urls--candidates (&optional ignore-regexp)
  "Collect \"LINE: URL\" candidates, skipping lines matching IGNORE-REGEXP.
Only the first URL of a line is taken.  Each candidate carries the bare
URL in its `multi-category' property - embark refines those to `url'
targets - and the URL's buffer position in `consult--candidate'."
  (let ((line 1)
        candidates)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (pos-bol))
              (eol (pos-eol)))
          ;; ] [ " excluded so org [[url][desc]] markup yields a clean url
          (when (and (re-search-forward "https?://[^][\"[:space:]()<>]+" eol t)
                     (not (and ignore-regexp
                               (string-match-p
                                ignore-regexp
                                (buffer-substring-no-properties bol eol)))))
            (let ((url (replace-regexp-in-string
                        "[.,;:!?]+\\'" "" (match-string-no-properties 0))))
              (push (propertize (format "%d: %s" line url)
                                'multi-category (cons 'url url)
                                'consult--candidate (match-beginning 0))
                    candidates))))
        (forward-line 1)
        (setq line (1+ line))))
    (nreverse candidates)))

;;;###autoload
(defun consult-line-collect-urls (&optional ignore-regexp)
  "Like `consult-line', but over the buffer's URL-bearing lines.
Lines matching IGNORE-REGEXP are skipped.  Selecting jumps to the URL;
preview only on `consult-preview-key'.  Embark sees the bare URL - the
`multi-category' candidates refine to `url' targets - so all url
actions apply."
  (interactive)
  ;; consult--read is only bound once consult loads
  (require 'consult)
  (let ((candidates (consult-line-collect-urls--candidates ignore-regexp)))
    (unless candidates
      (user-error "No URLs in buffer"))
    (consult--read
     candidates
     :prompt "URL: "
     :category 'multi-category
     :sort nil
     :require-match t
     :lookup #'consult--lookup-candidate
     :state (consult--jump-state))))

;;;###autoload
(defun search-in-project ()
  "Ripgrep the project, seeded with the region or symbol at point."
  (interactive)
  (consult-ripgrep
   (project-root (project-current))
   (if (use-region-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     ;; (symbol-name (symbol-at-point)) here seeded literal "nil"
     ;; between symbols - doom.d rot
     (thing-at-point 'symbol t))))
