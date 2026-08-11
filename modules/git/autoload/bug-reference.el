;;; modules/git/autoload/bug-reference.el -*- lexical-binding: t; -*-

;;;###autoload
(defun init-bug-reference-mode-settings ()
  "Recognize org/repo#123 references and route them through GitHub."
  (setq
   bug-reference-bug-regexp
   (concat
    "\\b"
    "\\("
    "\\([A-Za-z0-9_.-]+\\)"  ; org
    "/"                      ; slash
    "\\([A-Za-z0-9_.-]+\\)"  ; repo
    "#\\([0-9]+\\)"          ; hash prefixed ticket number
    "\\)"))
  (setq bug-reference-url-format #'bug-reference-url-format-fn)
  ;; `bug-reference' overlays are buttons with no `action', so `push-button'
  ;; callers (evil-ret, org RET) would `funcall' nil.  Supply one.
  (put 'bug-reference 'action #'bug-reference-button-action))

;;;###autoload
(defun bug-reference-button-action (button)
  "Visit the bug reference of BUTTON.
Adapts `bug-reference-push-button' (which takes a position) to the
button `action' convention so `push-button' works on references."
  (bug-reference-push-button (overlay-start button)))

;;;###autoload
(defun bug-reference-github-issue-url (org project ticket)
  "GitHub issue URL for ORG/PROJECT#TICKET.
A blank ORG falls back to `bug-reference-default-org'.  GitHub redirects
the /issues/ form to /pull/ so it resolves pull requests too."
  (format "https://github.com/%s/%s/issues/%s"
          (if (or (null org) (string-blank-p org))
              bug-reference-default-org
            org)
          project ticket))

;;;###autoload
(defun bug-reference-github-resolve-url (url)
  "Ask the GitHub API for the accurate html_url behind issue URL.
Issues and pull requests share one number sequence, so a constructed
/issues/N url is only a guess; the issues API accepts both kinds and
its html_url says /pull/N for pull requests.  On any failure (no ghub,
no token, offline, 404) returns URL as-is - github.com still redirects."
  (or (and (or (fboundp 'ghub-get) (require 'ghub nil t))
           (string-match
            "github\\.com/\\([^/]+\\)/\\([^/]+\\)/issues/\\([0-9]+\\)\\'" url)
           (condition-case nil
               (alist-get 'html_url
                          (ghub-get (format "/repos/%s/%s/issues/%s"
                                            (match-string 1 url)
                                            (match-string 2 url)
                                            (match-string 3 url))
                                    nil :auth 'forge))
             (error nil)))
      url))

;;;###autoload
(defun bug-reference-url-format-fn ()
  "GitHub issue URL for the last `bug-reference-bug-regexp' match."
  (bug-reference-github-issue-url (match-string-no-properties 2)
                                  (match-string-no-properties 3)
                                  (match-string-no-properties 4)))

;;;###autoload
(defun bug-reference->github-url (ref)
  "GitHub URL for a bug REF string like \"org/repo#123\".
Returns nil when REF is not a bug reference.  Re-matches against REF
rather than the buffer, so it is safe to call away from point.  Unlike
the in-buffer button path this url gets pasted as text, so it consults
the API to tell pull requests from issues instead of leaning on the
github.com redirect; offline it falls back to the /issues/ form."
  (when (string-match bug-reference-bug-regexp ref)
    (bug-reference-github-resolve-url
     (bug-reference-github-issue-url (match-string-no-properties 2 ref)
                                     (match-string-no-properties 3 ref)
                                     (match-string-no-properties 4 ref)))))
