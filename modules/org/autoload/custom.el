;;; modules/org/autoload/custom.el -*- lexical-binding: t; -*-
;; Slimmed from doom.d org/autoload/custom.el + org-link.el.  Dropped as dead
;; even in doom.d: org-roam--link-to, org--insert-selection-dwim,
;; org-link-parse, vulpea-insert+.  Long-tail: edit-indirect-guess-mode-fn+
;; (org-edit-indirect), the gif/image-data link helpers.  org-wrap-in-block
;; lives in general/autoload/misc.el (expreg-transient depends on it).

;;;###autoload
(defun org-roam-toggle-ui-xwidget ()
  (interactive)
  (let* ((url (concat "http://localhost:" (number-to-string org-roam-ui-port)))
         (buf (or (xwidget-webkit-get-url-buffer
                   (concat "localhost:" (number-to-string org-roam-ui-port)))
                  (xwidget-webkit-url-get-create url "*org-roam-ui*"))))
    (if-let* ((win (get-buffer-window buf)))
        (delete-window win)
      (switch-to-buffer-other-window buf))))

;;;###autoload
(defun get-gh-item-title (uri)
  "Based on given GitHub URI for pull-request or issue,
  return the title of that pull-request or issue."
  (cond
   (;; either PR or issue
    (string-match "\\(github.com\\).*\\(issues\\|pull\\)" uri)
    (pcase-let*
        ((`(_ _ ,owner ,repo ,type ,number) (remove "" (split-string uri "/")))
         (gh-resource (format "/repos/%s/%s/%s/%s"
                              owner
                              repo
                              (if (string= type "pull") "pulls" type)
                              number))
         (resp (ghub-get gh-resource nil :auth 'forge)))
      (when resp
        (let-alist resp
          (format
           "%s/%s#%s — %s" owner repo number .title)))))

   ;; just a link to a repo.  doom.d matched [[:graph:]]+ here, which eats
   ;; "/" - every file/commit link fell into this branch and the
   ;; file-in-branch arm below was dead; fixed on port
   ((string-match "\\(github.com\\)/[^/]+/[^/]+$" uri)
    (pcase-let* ((`(_ _ ,owner ,repo) (remove "" (split-string uri "/\\|\\?")))
                 (gh-resource (format "/repos/%s/%s" owner repo))
                 (resp (ghub-get gh-resource nil :auth 'forge)))
      (when resp
        (let-alist resp
          (format
           "%s/%s — %s" owner repo .description)))))

   (;; link to a file in a branch
    (string-match "\\(github.com\\).*" uri)
    (pcase-let* ((`(_ _ ,owner ,repo ,type ,branch ,dir ,file)
                  (remove "" (split-string uri "/\\|\\?")))
                 (branch (if (or (string= type "commit") (string= type "tree"))
                             (substring branch 0 7)  ; trim to shorten sha
                           branch)))
      (mapconcat
       #'identity (delq nil (list owner repo type branch dir file))
       "/")))
   (t uri)))

;;;###autoload
(defun org-link-make-description-fn (link desc)
  "For `org-link-make-description-function': GitHub links get real titles."
  (cond ((and desc (not (string-empty-p desc))) desc)
        ((string-match "\\(github.com\\).*" link)
         (get-gh-item-title link))
        (t desc)))

;;;###autoload
(defun org-store-link-id-optional (&optional arg)
  "Stores a link, reversing the value of `org-id-link-to-org-use-id'.
If it's globally set to create the ID property, then it wouldn't,
and if it is set to nil, then it would forcefully create the ID."
  (interactive "P")
  (let ((org-id-link-to-org-use-id (not org-id-link-to-org-use-id)))
    (org-store-link arg :interactive)))

;;;###autoload
(defun org-remove-link-at-point ()
  "Remove link at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (delete-region (match-beginning 0) (match-end 0))))

;;;###autoload
(defun org-goto-bottommost-heading (&optional maxlevel)
  "Go to the last heading in the current subtree."
  (interactive "P")
  (if (listp maxlevel)
      (setq maxlevel 4)
    (unless maxlevel (setq maxlevel 3)))
  ;; doom.d leaked `currlevel' as a global - fixed on port
  (let ((currlevel 1))
    (while (<= currlevel maxlevel)
      (org-next-visible-heading 1)
      (unless (org-at-heading-p)
        (org-previous-visible-heading 1)
        (org-cycle)
        (setq currlevel (1+ currlevel))))))

;;;###autoload
(defun org-goto-datetree-date (&optional _date)
  "Jump to selected date heading in the datetree."
  (interactive)
  ;; not autoloaded by org itself - the doom.d version errored when the
  ;; datetree lib hadn't been pulled in by a capture template yet
  (require 'org-datetree)
  (save-restriction
    (let* ((datetree-date (org-read-date))
           (dt (org-date-to-gregorian datetree-date)))
      (org-datetree-find-date-create dt t)
      (org-fold-show-hidden-entry)
      (outline-show-subtree))))

;;;###autoload
(defun person-w-name-based-id ()
  "Returns a person record with name-based id. To be used in capture template."
  (interactive)
  (let* ((name-parts (thread-last
                       (or (org-capture-get ':initial)
                           (gui-get-selection 'CLIPBOARD))
                       (read-from-minibuffer "Name: ")
                       (split-string)
                       (seq-map #'string-trim)
                       (seq-map #'capitalize)))
         (id (mapconcat #'downcase (reverse name-parts) "-"))
         (name (format "%s %s" (car name-parts)
                       (substring (cadr name-parts) 0 1)))
         (full-name (mapconcat #'identity name-parts " ")))
    (format "%s\n:PROPERTIES:\n:ID: %s\n:roam_aliases: \"%s\"\n:END:" name id full-name)))
