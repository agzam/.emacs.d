;;; modules/jira/autoload/jira.el -*- lexical-binding: t; -*-

;; User glue that isn't part of the go-jira package itself: the browse helper
;; reaches into the web-browsing module (browser-get-tabs/-activate-tab, macOS
;; JXA) and the PR search hands off to the git module (github-topics-find-prs).
;; Both lean on go-jira internals (go-jira--find-exe, -ticket-arg-or-ticket-at-
;; point) that carry no autoload cookie, so each `(require 'go-jira)' first -
;; doom.d omitted it and broke on a cold `M-x'.

;;;###autoload
(defun go-jira-browse-ticket-url (ticket)
  "Open TICKET in browser."
  (interactive "sJira ticket number: ")
  (require 'go-jira)
  (let ((j (go-jira--find-exe))
        (ticket (or ticket
                    (buffer-local-value 'go-jira--ticket-number (current-buffer)))))

    ;; let's not open a new tab if got one in browser already
    (if-let* ((_ (eq system-type 'darwin))
              (ticket-url (go-jira-ticket->url ticket))
              (btab
               (thread-last
                 (browser-get-tabs)
                 (seq-filter
                  (lambda (x)
                    (string= ticket-url (plist-get x :url))))
                 (seq-first)))
              (win-idx (plist-get btab :windowIndex))
              (tab-idx (plist-get btab :tabIndex)))
        (browser-activate-tab win-idx tab-idx)
      (shell-command-to-string (format "%s browse %s" j ticket)))))


;;;###autoload
(defun go-jira-find-pull-requests-on-github (&optional jira-ticket)
  "Search for mentioning of JIRA-TICKET on GitHub.
If JIRA-TICKET is not provided, uses ticket at point or prompts."
  (interactive)
  (require 'go-jira)
  (let* ((ticket (or (go-jira--ticket-arg-or-ticket-at-point jira-ticket)
                     (read-string "Gimme the JIRA ticket to search: "))))
    (github-topics-find-prs ticket)))
