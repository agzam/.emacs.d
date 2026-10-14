;;; tests/org/config-tests.el --- org module wiring specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun org-config-tests--use-package (name)
  "Return the `use-package' form for NAME in the org module config.
The specs read source: config.el cannot load in the batch tier, where
neither `use-package' nor general (behind `map!') exists."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/org/config.el" test-config-root))
    (let (found)
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) name))
                (setq found form))))
        (end-of-file nil))
      found)))

(defun org-config-tests--forms (file)
  "Return the top-level forms of FILE, relative to the config root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file test-config-root))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun org-config-tests--roam-keys (form)
  "Collect the key pairs of every roam prefix group inside FORM."
  (when (proper-list-p form)
    (apply #'append
           (and (eq (car form) :prefix)
                (equal (cadr form) '("r" . "roam"))
                (map-form-key-pairs (cddr form)))
           (mapcar #'org-config-tests--roam-keys form))))

;; Stand-ins for what the vulpea-graph :config forms touch: neither the
;; package nor websocket exists in the batch tier.
(defvar vulpea-graph-port 35903)
(defvar vulpea-db-updated-functions nil)
(defvar org-config-tests--graph-clients nil
  "Clients the stubbed `vulpea-graph--ws-on-open' admitted.")

(defun vulpea-graph--ws-on-open (client)
  "Record CLIENT as admitted."
  (push client org-config-tests--graph-clients))

(dolist (form (use-package-body-forms
               (cdr (org-config-tests--use-package 'vulpea-graph)) :config))
  (eval form t))

(describe "org module config"
  (it "leaves the note graph to vulpea"
    (expect (org-config-tests--use-package 'vulpea) :to-be-truthy)
    (expect (org-config-tests--use-package 'org-roam) :to-be nil)
    (expect (org-config-tests--use-package 'org-roam-ui) :to-be nil))

  (it "keeps the org settings that notes rely on"
    (let ((config (flatten-tree
                   (use-package-body-forms
                    (cdr (org-config-tests--use-package 'org)) :config))))
      (expect (memq 'org-export-with-broken-links config) :to-be-truthy)
      (expect (memq 'org-id-link-to-org-use-id config) :to-be-truthy)
      (expect (member "roam_aliases" config) :to-be-truthy)
      (expect (member "roam_refs" config) :to-be-truthy)
      (expect (memq 'backlink-count-mode config) :to-be-truthy))))

(describe "vulpea-graph"
  (it "installs Arenile's browser graph with its frontend assets"
    (let ((recipe (cadr (memq :ensure (org-config-tests--use-package 'vulpea-graph)))))
      (expect (plist-get (cdr recipe) :repo) :to-equal "Arenile/vulpea-graph-ui")
      ;; without assets/ the package refuses to start its server
      (expect (member "assets" (plist-get (cdr recipe) :files)) :to-be-truthy)))

  (it "defers loading to its commands"
    ;; loading it loads vulpea, whose :config starts the db autosync
    (expect (memq :commands (org-config-tests--use-package 'vulpea-graph))
            :to-be-truthy))

  (it "admits only the page it serves to the websocket"
    (let (closed)
      (setq org-config-tests--graph-clients nil)
      (cl-letf (((symbol-function 'websocket-origin) #'identity)
                ((symbol-function 'websocket-close)
                 (lambda (client) (push client closed))))
        (dolist (origin '("http://localhost:35903" "http://127.0.0.1:35903"
                          "https://evil.example" "http://localhost:8080" nil))
          (vulpea-graph--ws-on-open origin)))
      (expect org-config-tests--graph-clients :to-have-same-items-as
              '("http://localhost:35903" "http://127.0.0.1:35903"))
      (expect closed :to-have-same-items-as
              '("https://evil.example" "http://localhost:8080" nil))))

  (it "follows a changed port"
    (setq org-config-tests--graph-clients nil)
    (let ((vulpea-graph-port 4000))
      (cl-letf (((symbol-function 'websocket-origin) #'identity)
                ((symbol-function 'websocket-close) #'ignore))
        (vulpea-graph--ws-on-open "http://localhost:35903")
        (vulpea-graph--ws-on-open "http://localhost:4000")))
    (expect org-config-tests--graph-clients :to-equal '("http://localhost:4000")))

  (it "marks the graph stale on every vulpea db write"
    (expect (memq 'vulpea-graph--mark-dirty vulpea-db-updated-functions)
            :to-be-truthy))

  (it "opens from the roam keys org-roam-ui used"
    (expect (cdr (assoc "W" (org-config-tests--roam-keys
                             (org-config-tests--use-package 'org))))
            :to-equal '(function vulpea-graph))
    (expect (cdr (assoc "W" (org-config-tests--roam-keys
                             (org-config-tests--forms "config.el"))))
            :to-equal '(function vulpea-graph))))

(describe "journal keys"
  (it "open today's work journal and ask for the personal journal date"
    (let ((keys (org-config-tests--roam-keys (org-config-tests--forms "config.el"))))
      (expect (cdr (assoc "t" keys)) :to-equal '(cmd! (open-journal 'work)))
      (expect (cdr (assoc "T" keys)) :to-equal '(cmd! (open-journal-date 'personal)))))

  (it "ask for the date under n and N on the leader and the localleader"
    (dolist (form (list (org-config-tests--forms "config.el")
                        (org-config-tests--use-package 'org)))
      (let ((keys (org-config-tests--roam-keys form)))
        (expect (cdr (assoc "n" keys)) :to-equal '(cmd! (open-journal-date 'work)))
        (expect (cdr (assoc "N" keys))
                :to-equal '(cmd! (open-journal-date 'personal)))))))

;;; config-tests.el ends here
