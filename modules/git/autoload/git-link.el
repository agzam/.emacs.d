;;; modules/git/autoload/git-link.el -*- lexical-binding: t; -*-

;;;###autoload
(defun git-link-main-branch (&optional _browse?)
  "Copy a git-link URL pinned to the repo's main branch."
  (interactive "P")
  (require 'git-link)
  (let* ((git-link-default-branch (magit-main-branch)))
    (call-interactively #'git-link-kill)))

;;;###autoload
(defun git-link-blame ()
  "Copy a git-link URL pointing at the blame view."
  (interactive)
  (cl-flet ((git-link--new* (x) (replace-regexp-in-string "/blob/" "/blame/" x)))
    (advice-add 'git-link--new :override #'git-link--new*)
    (let ((link (call-interactively 'git-link)))
      (advice-remove 'git-link--new #'git-link--new*)
      (git-link--new link))))

;;;###autoload
(defun git-link-kill (&optional browse?)
  "Copy URL to current file/revision/forge-topic; BROWSE? opens it too."
  (interactive "P")
  (require 'git-link)
  (cl-letf (((symbol-function #'git-link--new) (lambda (link) link)))
    (let ((link (pcase major-mode
                  ((pred (lambda (x) (string-match-p "forge-topic" (symbol-name x))))
                   (git-link-forge-topic))

                  ((pred (lambda (x) (string-match-p "magit" (symbol-name x))))
                   (message (browse-at-remote-kill)))

                  ;; #'git-link fn detestably has been made to be exclusively called
                  ;; interactively, so I had to temporarily redefine #'git-link--new
                  ;; (above), ignore prefix arg and other parameters, in order to
                  ;; retrieve the link
                  (_ (let* ((current-prefix-arg nil)
                            (git-link-open-in-browser browse?)
                            (lnk (call-interactively #'git-link)))
                       (kill-new lnk)
                       (prin1 lnk))))))
      (if browse?
          (browse-url link)
        link))))

;;;###autoload
(defun git-link-forge-topic ()
  "Copy the URL of the forge topic at point."
  (interactive)
  (let ((url (forge-get-url (forge-current-topic))))
    (message url)
    (kill-new url)))

;;;###autoload
(defun git-https-url->ssh (url)
  "Convert git https URL to an ssh url."
  (if (string-match "^https://\\([^/]+\\)/\\([^/]+\\)/\\([^/#?]+\\)" url)
      (let* ((domain (match-string 1 url))
             (user (match-string 2 url))
             (repo (string-trim-right (match-string 3 url) "/"))
             (repo (if (string-suffix-p ".git" repo)
                       (substring repo 0 -4)
                     repo))
             (ssh-url (format "git@%s:%s/%s.git" domain user repo)))
        (kill-new ssh-url)
        (message ssh-url))
    (error "Invalid HTTPS URL format")))

;;; browse-at-remote wrappers (bindings-tree names; Doom's +vc/* lineage)

(defun vc-git-link--homepage-url ()
  "Forge homepage URL for the current repository."
  (require 'browse-at-remote)
  (if-let* ((ref (browse-at-remote--remote-ref))
            (url (plist-get (browse-at-remote--get-url-from-remote (car ref))
                            :url)))
      url
    (user-error "Can't determine the remote homepage")))

;;;###autoload
(defun vc-git-link (&optional arg)
  "Open the current file/region at remote in the browser.
With prefix ARG, open the repo homepage instead."
  (interactive "P")
  (require 'browse-at-remote)
  (if arg (vc-git-link-homepage) (browse-at-remote)))

;;;###autoload
(defun vc-git-link-homepage ()
  "Open the remote homepage of the current repo in the browser."
  (interactive)
  (browse-url (vc-git-link--homepage-url)))

;;;###autoload
(defun vc-git-link-kill ()
  "Copy the remote URL of the current file/region."
  (interactive)
  (require 'browse-at-remote)
  (browse-at-remote-kill))

;;;###autoload
(defun vc-git-link-kill-homepage ()
  "Copy the remote homepage URL of the current repo."
  (interactive)
  (let ((url (vc-git-link--homepage-url)))
    (kill-new url)
    (message "Copied: %s" url)))
