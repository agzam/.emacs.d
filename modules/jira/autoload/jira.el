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

;;;###autoload
(defun go-jira-search-slack-threads (&optional jira-ticket)
  "Search Slack threads for JIRA-TICKET.
If JIRA-TICKET is not provided, uses ticket at point or prompts.
Doubles as an Embark action on a Jira ticket at point."
  (interactive)
  (require 'go-jira)
  (let ((ticket (or (go-jira--ticket-arg-or-ticket-at-point jira-ticket)
                    (read-string "Gimme the JIRA ticket to search: "))))
    (slacko-search ticket)))

(defvar go-jira-autopilot-script
  "~/.config/eca/death-contraptions/scripts/autopilot/autopilot.bb"
  "The autopilot script that queues a ticket for background analysis.")

(defvar go-jira-autopilot-repo-roots
  '("~/GitHub/qlik-trial" "~/GitHub/stitchdata" "~/GitHub/singer-io")
  "Org directories whose subdirectories are the repos a ticket can be about.")

(defconst go-jira--autopilot-guess "guess from the summary"
  "The repo choice that leaves the repo to the autopilot.")

(defun go-jira--autopilot-repos ()
  "List the local repos as \"org/name\"."
  (seq-mapcat
   (lambda (root)
     (when (file-directory-p root)
       (thread-last
         (directory-files root nil directory-files-no-dot-files-regexp)
         (seq-filter (lambda (name) (file-directory-p (expand-file-name name root))))
         (mapcar (lambda (name)
                   (concat (file-name-nondirectory (directory-file-name root)) "/" name))))))
   go-jira-autopilot-repo-roots))

(defun go-jira--autopilot-command (ticket repo force)
  "Build the argv that queues TICKET, for REPO when non-nil, with FORCE."
  (append (list "bb" (expand-file-name go-jira-autopilot-script)
                "submit" ticket "--by" "emacs")
          (when repo (list "--repo" repo))
          (when force (list "--force"))))

;;;###autoload
(defun go-jira-autopilot-submit (&optional jira-ticket force)
  "Queue JIRA-TICKET for background analysis by the autopilot.
With prefix arg FORCE, queue it even when an analysis exists.
A detached worker does the work, so it survives an Emacs restart.
Doubles as an Embark action on a Jira ticket at point."
  (interactive (list nil current-prefix-arg))
  (require 'go-jira)
  (let* ((ticket (or (go-jira--ticket-arg-or-ticket-at-point jira-ticket)
                     (read-string "Gimme the JIRA ticket to analyze: ")))
         (choice (completing-read
                  (format "Repo for %s: " ticket)
                  (cons go-jira--autopilot-guess (go-jira--autopilot-repos))
                  nil t nil nil go-jira--autopilot-guess))
         (repo (unless (equal choice go-jira--autopilot-guess) choice))
         (buffer (generate-new-buffer " *autopilot-submit*")))
    (make-process
     :name (concat "autopilot-submit-" ticket)
     :buffer buffer
     :noquery t
     :command (go-jira--autopilot-command ticket repo force)
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (message "autopilot %s: %s" ticket
                            (with-current-buffer buffer (string-trim (buffer-string))))
                   (kill-buffer buffer))))))
