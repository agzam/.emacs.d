;;; tests/tab-bar/tab-bar-tests.el --- tab-bar/autoload/tab-bar.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; tab-bar and transient are built-in; the module file requires them for
;; real.  window-undo/window-redo (general module) are tab-bar-transient
;; suffixes - load their file so the layout walk can assert fboundp.
(load-module-file "modules/general/autoload/windows.el")
(load-module-file "modules/tab-bar/autoload/tab-bar.el")

(defun tab-bar-tests--tabs (&rest names)
  "Fixture tab list in `tab-bar-tabs' shape, one tab per name in NAMES.
Real tabs always carry the explicit-name key (`tab-bar--tab' emits it);
rename-dups' in-place setf relies on the cons being there."
  (mapcar (lambda (n) (list 'tab (cons 'name n) (cons 'explicit-name nil)))
          names))

(defun tab-bar-tests--names (tabs)
  (mapcar (lambda (tab) (alist-get 'name tab)) tabs))

(defun tab-bar-tests--layout-symbols (prefix)
  "Symbol suffix commands of PREFIX's layout.
Drops closures and the transient:PREFIX::N symbols modern transient
interns for lambda suffixes."
  (seq-remove (lambda (cmd) (string-prefix-p "transient:" (symbol-name cmd)))
              (seq-filter #'symbolp
                          (transient-layout-commands
                           (get prefix 'transient--layout)))))

(describe "tab-bar-rename-dups"
  (it "suffixes duplicate names, first occurrence stays bare"
    (let ((tabs (tab-bar-tests--tabs "foo" "foo" "foo")))
      (tab-bar-rename-dups tabs)
      (expect (tab-bar-tests--names tabs) :to-equal '("foo" "foo 2" "foo 3"))))

  (it "processes every duplicate group (doom.d only renamed the first)"
    (let ((tabs (tab-bar-tests--tabs "foo" "bar" "foo" "bar")))
      (tab-bar-rename-dups tabs)
      (expect (tab-bar-tests--names tabs) :to-equal '("foo" "bar" "foo 2" "bar 2"))))

  (it "ignores existing numeric suffixes when grouping"
    (let ((tabs (tab-bar-tests--tabs "foo 2" "foo")))
      (tab-bar-rename-dups tabs)
      (expect (tab-bar-tests--names tabs) :to-equal '("foo" "foo 2"))))

  (it "marks renamed tabs explicit, leaves singles untouched"
    (let ((tabs (tab-bar-tests--tabs "dup" "alone" "dup")))
      (tab-bar-rename-dups tabs)
      (expect (alist-get 'explicit-name (nth 0 tabs)) :to-be t)
      (expect (alist-get 'explicit-name (nth 2 tabs)) :to-be t)
      (expect (alist-get 'explicit-name (nth 1 tabs)) :to-be nil)
      (expect (alist-get 'name (nth 1 tabs)) :to-equal "alone")))

  (it "tolerates tabs without a name"
    (let ((tabs (list (list 'tab) (list 'tab (cons 'name "x")))))
      (expect (tab-bar-rename-dups tabs) :not :to-throw)
      (expect (alist-get 'name (car tabs)) :to-be nil))))

(describe "tab movers"
  (it "move left/right translate to tab-bar-move-tab -1/+1"
    (let (moves)
      (cl-letf (((symbol-function 'tab-bar-move-tab)
                 (lambda (n) (push n moves))))
        (tab-move-left)
        (tab-move-right)
        (expect (nreverse moves) :to-equal '(-1 1))))))

(describe "tab-bar-fmt-show-index-fn"
  (it "prefixes a styled index when hints are on"
    (let ((tab-bar-tab-hints t))
      (let ((res (tab-bar-fmt-show-index-fn "name" nil 3)))
        (expect res :to-equal "3name")
        (expect (get-text-property 0 'display res) :to-equal '(raise -0.5))
        (expect (plist-get (get-text-property 0 'face res) :foreground)
                :to-equal "orange"))))

  (it "passes the name through when hints are off"
    (let ((tab-bar-tab-hints nil))
      (expect (tab-bar-fmt-show-index-fn "name" nil 3) :to-equal "name"))))

(describe "tab-bar-kill-tab"
  (it "closes the tab and schedules a dedup pass"
    (let (closed timer)
      (cl-letf (((symbol-function 'tab-bar-close-tab)
                 (lambda (&rest _) (setq closed t)))
                ((symbol-function 'run-with-timer)
                 (lambda (secs _repeat fn) (setq timer (list secs fn)))))
        (tab-bar-kill-tab)
        (expect closed :to-be t)
        (expect timer :to-equal (list 0.3 #'tab-bar-rename-dups))))))

(describe "tab-bar-duplicate / tab-bar-rename"
  (it "duplicate wraps the built-in then dedups"
    (let (calls)
      (cl-letf (((symbol-function 'tab-bar-duplicate-tab)
                 (lambda (&rest _) (push 'duplicate calls)))
                ((symbol-function 'tab-bar-rename-dups)
                 (lambda (&rest _) (push 'dedup calls))))
        (tab-bar-duplicate)
        (expect (nreverse calls) :to-equal '(duplicate dedup)))))

  (it "rename wraps the built-in interactively then dedups"
    (let (calls)
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (fn &rest _) (push fn calls)))
                ((symbol-function 'tab-bar-rename-dups)
                 (lambda (&rest _) (push 'dedup calls))))
        (tab-bar-rename)
        (expect (nreverse calls)
                :to-equal (list #'tab-bar-rename-tab 'dedup))))))

(describe "tab-bar-move-buffer-to-tab"
  ;; the doom.d original tested (length (window-list)) - always truthy -
  ;; so delete-window was dead code; the port fixes the branch
  (it "buries instead of deleting when the window is the only one"
    (let (buried deleted)
      (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1)))
                ((symbol-function 'bury-buffer) (lambda (&rest _) (setq buried t)))
                ((symbol-function 'delete-window) (lambda (&rest _) (setq deleted t)))
                ((symbol-function 'call-interactively) #'ignore)
                ((symbol-function 'split-window-sensibly) #'ignore)
                ((symbol-function 'switch-to-buffer) #'ignore)
                ((symbol-function 'goto-char) #'ignore))
        (tab-bar-move-buffer-to-tab)
        (expect buried :to-be t)
        (expect deleted :to-be nil))))

  (it "deletes the window when others remain"
    (let (buried deleted)
      (cl-letf (((symbol-function 'window-list) (lambda (&rest _) '(w1 w2)))
                ((symbol-function 'bury-buffer) (lambda (&rest _) (setq buried t)))
                ((symbol-function 'delete-window) (lambda (&rest _) (setq deleted t)))
                ((symbol-function 'call-interactively) #'ignore)
                ((symbol-function 'split-window-sensibly) #'ignore)
                ((symbol-function 'switch-to-buffer) #'ignore)
                ((symbol-function 'goto-char) #'ignore))
        (tab-bar-move-buffer-to-tab)
        (expect deleted :to-be t)
        (expect buried :to-be nil)))))

(describe "tab-bar-kill-project-buffers"
  (it "kills project buffers, then the tab"
    (let (calls)
      (cl-letf (((symbol-function 'project-kill-buffers)
                 (lambda (&rest _) (push 'project-kill calls)))
                ((symbol-function 'tab-bar-kill-tab)
                 (lambda () (push 'tab-kill calls))))
        (tab-bar-kill-project-buffers)
        (expect (nreverse calls) :to-equal '(project-kill tab-kill))))))

(describe "tab-bar-find-buffer-in-tabs"
  ;; consult isn't installed in the batch tier; the fake feature keeps the
  ;; fn's (require 'consult) inert while cl-letf provides the pieces
  (it "switches to the owning tab by name"
    (with-fake-feature 'consult
      (let ((buf (generate-new-buffer " *fixture*"))
            switched)
        (unwind-protect
            (cl-letf (((symbol-function 'consult-buffer)
                       (lambda () (funcall (symbol-function 'consult--buffer-action) buf)))
                      ((symbol-function 'tab-bar-get-buffer-tab)
                       (lambda (_) '(tab (name . "T2"))))
                      ((symbol-function 'tab-bar-switch-to-tab)
                       (lambda (name) (setq switched name))))
              (tab-bar-find-buffer-in-tabs)
              (expect switched :to-equal "T2"))
          (kill-buffer buf)))))

  (it "selects the buffer's window when the current tab owns it"
    (with-fake-feature 'consult
      (let ((buf (generate-new-buffer " *fixture*"))
            selected switched)
        (unwind-protect
            (cl-letf (((symbol-function 'consult-buffer)
                       (lambda () (funcall (symbol-function 'consult--buffer-action) buf)))
                      ((symbol-function 'tab-bar-get-buffer-tab)
                       (lambda (_) '(current-tab (name . "T1"))))
                      ((symbol-function 'get-buffer-window) (lambda (_) 'win))
                      ((symbol-function 'select-window)
                       (lambda (w) (setq selected w)))
                      ((symbol-function 'tab-bar-switch-to-tab)
                       (lambda (name) (setq switched name))))
              (tab-bar-find-buffer-in-tabs)
              (expect selected :to-be 'win)
              (expect switched :to-be nil))
          (kill-buffer buf)))))

  (it "does nothing when no tab owns the selection"
    (with-fake-feature 'consult
      (let ((buf (generate-new-buffer " *fixture*"))
            acted)
        (unwind-protect
            (cl-letf (((symbol-function 'consult-buffer)
                       (lambda () (funcall (symbol-function 'consult--buffer-action) buf)))
                      ((symbol-function 'tab-bar-get-buffer-tab) #'ignore)
                      ((symbol-function 'select-window)
                       (lambda (&rest _) (setq acted t)))
                      ((symbol-function 'tab-bar-switch-to-tab)
                       (lambda (&rest _) (setq acted t))))
              (tab-bar-find-buffer-in-tabs)
              (expect acted :to-be nil))
          (kill-buffer buf))))))

(describe "tab-bar-name-fn"
  ;; magit isn't installed in the batch tier
  (it "names gh-notify buffers after the buffer"
    (with-fake-feature 'magit
      (with-temp-buffer
        (setq major-mode 'gh-notify-mode)
        (cl-letf (((symbol-function 'magit-get-current-branch) #'ignore)
                  ((symbol-function 'project-current) #'ignore))
          (expect (tab-bar-name-fn) :to-equal (buffer-name))))))

  (it "names project file buffers root ▸ branch"
    (with-fake-feature 'magit
      (with-temp-buffer
        (cl-letf (((symbol-function 'buffer-file-name)
                   (lambda (&optional _) "/repo/src/file.el"))
                  ((symbol-function 'magit-get-current-branch) (lambda () "main"))
                  ((symbol-function 'magit-rev-parse) (lambda (_) "."))
                  ((symbol-function 'project-current) (lambda (&rest _) 'proj))
                  ((symbol-function 'project-root) (lambda (_) "/repo/")))
          (expect (tab-bar-name-fn)
                  :to-equal (concat "repo" (string #xE0020) " ▸ main"))))))

  (it "names worktree checkouts after the container directory"
    (with-fake-feature 'magit
      (with-temp-buffer
        (cl-letf (((symbol-function 'buffer-file-name)
                   (lambda (&optional _) "/wt/feature-co/src/file.el"))
                  ((symbol-function 'magit-get-current-branch) (lambda () "feature"))
                  ;; rev-parse pointing outside .git = linked worktree
                  ((symbol-function 'magit-rev-parse)
                   (lambda (_) "/repo/.git/worktrees/feature-co"))
                  ((symbol-function 'project-current) (lambda (&rest _) 'proj))
                  ((symbol-function 'project-root) (lambda (_) "/wt/feature-co/")))
          (expect (tab-bar-name-fn)
                  :to-equal (concat "wt" (string #xE0020) " ▸ feature"))))))

  (it "falls back to the file directory outside projects"
    (with-fake-feature 'magit
      (with-temp-buffer
        (cl-letf (((symbol-function 'buffer-file-name)
                   (lambda (&optional _) "/tmp/notes.txt"))
                  ((symbol-function 'magit-get-current-branch) #'ignore)
                  ((symbol-function 'project-current) #'ignore))
          (expect (tab-bar-name-fn) :to-equal "/tmp/")))))

  (it "renders minibuffers as empty"
    (with-fake-feature 'magit
      (with-temp-buffer
        (cl-letf (((symbol-function 'buffer-name)
                   (lambda (&optional _) " *Minibuf-1*"))
                  ((symbol-function 'magit-get-current-branch) #'ignore)
                  ((symbol-function 'project-current) #'ignore))
          (expect (tab-bar-name-fn) :to-equal ""))))))

(describe "tab-bar-hints-off-h"
  (it "turns hints off and unhooks itself"
    (let ((tab-bar-tab-hints t)
          (transient-exit-hook nil))
      (add-hook 'transient-exit-hook #'tab-bar-hints-off-h)
      (tab-bar-hints-off-h)
      (expect tab-bar-tab-hints :to-be nil)
      (expect (memq #'tab-bar-hints-off-h transient-exit-hook) :to-be nil))))

(describe "tab-bar-transient layout"
  (it "references only defined commands (lambda suffixes aside)"
    (let ((cmds (seq-filter #'symbolp
                            (transient-layout-commands
                             (get 'tab-bar-transient 'transient--layout)))))
      (expect cmds :not :to-be nil)
      (expect (seq-remove #'fboundp cmds) :to-equal nil)))

  (it "covers exactly the expected command set"
    (expect (seq-uniq (tab-bar-tests--layout-symbols 'tab-bar-transient))
            :to-have-same-items-as
            '(tab-bar-switch-to-recent-tab
              tab-bar-switch-to-prev-tab tab-bar-switch-to-next-tab
              tab-move-left tab-move-right tab-bar-move-window-to-tab
              tab-bar-add-new-tab tab-bar-duplicate tab-bar-rename
              tab-bar-select-tab-by-name tab-bar-move-buffer-to-tab
              tab-bar-find-buffer-in-tabs tab-bar-kill-project-buffers
              window-undo window-redo tab-bar-kill-tab tab-undo
              tab-bar-new-tab-transient restore-desktop-and-tabs
              quicksave-session))))

(describe "tab-bar-new-tab-transient layout"
  ;; consult/vulpea/gptel suffixes aren't loadable in batch; the exact
  ;; key/command set is the rot net, fboundp coverage is probe territory
  (it "covers exactly the expected command set"
    ;; go-jira-browse-default-board rides the darwin-only [:if ...] column;
    ;; the layout walk ignores :if predicates, so it's always in the set.
    (expect (seq-uniq (tab-bar-tests--layout-symbols 'tab-bar-new-tab-transient))
            :to-have-same-items-as
            '(vulpea-find vulpea-backlinks open-gptel gh-notify
              find-in-config-dir go-jira-browse-default-board zoxide-find
              consult-buffer consult-recent-file tab-bar-kill-tab))))

(describe "desktop quarantine"
  (it "desktop-path points into the (sandboxed) state dir"
    (expect desktop-path :to-equal (list doom-state-dir))))
