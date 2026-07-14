;;; tests/embark/embark-tests.el --- embark/autoload/embark.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)

(load-module-file "modules/embark/autoload/embark.el")

;; embark isn't installed in the test env; the defvars below stand in for
;; its own (and the module config.el's embark-url-config).  They carry
;; values on purpose: a value-less defvar marks the symbol special only
;; file-locally, so functions from the module file would LET-bind these
;; lexically (embark-hide-which-key-indicator does) - invisible to the
;; specs.  Valued defvars mark them special globally, matching real embark.
(defvar embark-url-config nil)
(defvar embark-url-map nil)
(defvar embark-target-finders nil)
(defvar embark-keymap-alist nil)
(defvar embark-indicators nil)
(defvar embark-post-action-hooks nil)

;; with-fake-feature lives in tests/helper.el (shared with web-browsing).

(defvar test-url-config
  '((nil :actions (("RET" . shared-open)
                   ("b o" . shared-browse)))
    (test-gh-repo
     :pattern "github\\.com/[^/]+/[^/]+/?$"
     :actions (("g c" . repo-clone)
               ("RET" . repo-open)))
    (test-video
     :pattern "youtu"
     :actions ()))
  "Miniature `embark-url-config' fixture with a shared (nil) entry.")

(defmacro with-url-type-fixture (&rest body)
  "Run BODY with sandboxed embark vars and `test-url-config' installed."
  `(let ((embark-url-config test-url-config)
         (embark-url-map (make-sparse-keymap))
         (embark-target-finders '(preexisting-finder))
         (embark-keymap-alist nil)
         (embark-url-patterns nil))
     ,@body))

(describe "embark-setup-url-types"
  (it "builds a keymap per type, merging shared and specific actions"
    (with-url-type-fixture
     (embark-setup-url-types)
     (let ((km (symbol-value 'test-gh-repo-map)))
       (expect (lookup-key km (kbd "g c")) :to-be 'repo-clone)
       (expect (lookup-key km (kbd "b o")) :to-be 'shared-browse)
       ;; the type-specific RET wins over the shared one
       (expect (lookup-key km (kbd "RET")) :to-be 'repo-open)
       (expect (keymap-parent km) :to-be embark-url-map))))

  (it "registers a pattern and a keymap-alist entry per typed entry"
    (with-url-type-fixture
     (embark-setup-url-types)
     (expect (length embark-url-patterns) :to-equal 2)
     (expect (car (assq 'test-video embark-url-patterns)) :to-be 'test-video)
     (expect (cdr (assq 'test-gh-repo embark-keymap-alist))
             :to-be 'test-gh-repo-map)
     (expect (cdr (assq 'test-video embark-keymap-alist))
             :to-be 'test-video-map)))

  (it "registers the universal finder exactly once across reruns"
    (with-url-type-fixture
     (embark-setup-url-types)
     (embark-setup-url-types)
     (expect (cl-count 'embark-target-url-at-point embark-target-finders)
             :to-equal 1)
     ;; foreign finders survive the rerun cleanup
     (expect (memq 'preexisting-finder embark-target-finders)
             :to-be-truthy)))

  (it "keeps the universal finder after specialized finders, before the file finder"
    ;; The greedy url catch-all (`thing-at-point' matches bug-reference / shr
    ;; buttons too) must lose to the specialized finders that prepend to the
    ;; front, yet beat embark's generic file finder - and stay put on rerun.
    (let ((embark-url-config test-url-config)
          (embark-url-map (make-sparse-keymap))
          (embark-keymap-alist nil)
          (embark-url-patterns nil)
          (embark-target-finders
           '(specialized-finder embark-target-file-at-point)))
      (embark-setup-url-types)
      (embark-setup-url-types)          ; rerun must not reshuffle the slot
      (let ((url-idx (cl-position 'embark-target-url-at-point embark-target-finders))
            (spec-idx (cl-position 'specialized-finder embark-target-finders))
            (file-idx (cl-position 'embark-target-file-at-point embark-target-finders)))
        (expect (cl-count 'embark-target-url-at-point embark-target-finders)
                :to-equal 1)
        (expect (< spec-idx url-idx) :to-be-truthy)
        (expect (< url-idx file-idx) :to-be-truthy)))))

(describe "embark-target-url-at-point"
  (it "classifies a url via the pattern table"
    (let ((embark-url-patterns '((test-gh-repo "github\\.com/[^/]+/[^/]+/?$"))))
      (with-temp-buffer
        (insert "see https://github.com/agzam/foo pls")
        (search-backward "github")
        (pcase-let ((`(,type ,url . ,bounds) (embark-target-url-at-point)))
          (expect type :to-be 'test-gh-repo)
          (expect url :to-equal "https://github.com/agzam/foo")
          (expect (buffer-substring-no-properties (car bounds) (cdr bounds))
                  :to-equal url)))))

  (it "supports predicate-function patterns"
    (let ((embark-url-patterns
           (list (list 'test-video (lambda (u) (string-match-p "youtu" u))))))
      (with-temp-buffer
        (insert "https://youtu.be/xyz")
        (goto-char (+ (point-min) 5))
        (expect (car (embark-target-url-at-point)) :to-be 'test-video))))

  (it "falls back to the generic url type"
    (let ((embark-url-patterns '((test-gh-repo "github\\.com/[^/]+/[^/]+/?$"))))
      (with-temp-buffer
        (insert "https://example.com/page")
        (goto-char (+ (point-min) 5))
        (pcase-let ((`(,type ,url . ,_) (embark-target-url-at-point)))
          (expect type :to-be 'url)
          (expect url :to-equal "https://example.com/page")))))

  (it "returns nil off-url"
    (let ((embark-url-patterns nil))
      (with-temp-buffer
        (insert "no links here")
        (goto-char (point-min))
        (expect (embark-target-url-at-point) :to-be nil)))))

(describe "embark-target-org-block"
  (it "targets the enclosing block with its type"
    (with-temp-buffer
      (org-mode)
      (insert "before\n#+begin_src elisp\n(foo)\n#+end_src\nafter\n")
      (search-backward "(foo)")
      (pcase-let ((`(org-block ,type ,beg . ,end) (embark-target-org-block)))
        (expect type :to-equal "src")
        (goto-char beg)
        (expect (looking-at "#\\+begin_src") :to-be-truthy)
        (expect (buffer-substring-no-properties beg end)
                :to-match "#\\+end_src$"))))

  (it "returns nil when point is above the block"
    (with-temp-buffer
      (org-mode)
      (insert "before\n#+begin_quote\nwords\n#+end_quote\n")
      (goto-char (point-min))
      (expect (embark-target-org-block) :to-be nil)))

  (it "returns nil outside org-mode"
    (with-temp-buffer
      (insert "#+begin_src elisp\n(foo)\n#+end_src\n")
      (goto-char (+ (point-min) 20))
      (expect (embark-target-org-block) :to-be nil))))

(describe "embark-org-block-convert"
  (cl-flet ((convert-fixture (text to)
              (with-temp-buffer
                (org-mode)
                (insert text)
                (goto-char (point-min))
                (search-forward "body")
                (pcase-let ((`(org-block ,type ,beg . ,end)
                             (embark-target-org-block)))
                  (cl-letf (((symbol-function 'embark--targets)
                             (lambda ()
                               `((:type org-block
                                  :target ,type
                                  :bounds ,(cons beg end))))))
                    (embark-org-block-convert to)))
                (buffer-string))))

    (it "converts src to example, dropping the src params"
      (expect (convert-fixture
               "#+begin_src elisp :results none\nbody\n#+end_src\n" "example")
              :to-equal "#+begin_example\nbody\n#+end_example\n"))

    (it "keeps header params when converting to src"
      (expect (convert-fixture
               "#+begin_example elisp\nbody\n#+end_example\n" "src")
              :to-equal "#+begin_src elisp\nbody\n#+end_src\n"))

    (it "converts to quote"
      (expect (convert-fixture
               "#+begin_example\nbody\n#+end_example\n" "quote")
              :to-equal "#+begin_quote\nbody\n#+end_quote\n"))))

(describe "embark-org-block-convert-to-* wrappers"
  (it "dispatch to their target types"
    (let (seen)
      (cl-letf (((symbol-function 'embark-org-block-convert)
                 (lambda (type) (push type seen))))
        (embark-org-block-convert-to-src)
        (embark-org-block-convert-to-example)
        (embark-org-block-convert-to-quote))
      (expect (nreverse seen) :to-equal '("src" "example" "quote")))))

(describe "browse-rfc-number-at-point"
  (it "opens the rfc-mode document buffer when rfc-mode is loaded"
    (let (doc-arg switched)
      (with-fake-feature 'rfc-mode
        (cl-letf (((symbol-function 'rfc-mode--document-buffer)
                   (lambda (n) (setq doc-arg n) 'fake-buffer))
                  ((symbol-function 'switch-to-buffer-other-window)
                   (lambda (b) (setq switched b))))
          (with-temp-buffer
            (insert "see RFC 1234 here")
            (search-backward "1234")
            (browse-rfc-number-at-point))))
      (expect doc-arg :to-equal 1234)
      (expect switched :to-be 'fake-buffer)))

  (it "searches online when rfc-mode is absent"
    (expect (featurep 'rfc-mode) :to-be nil)
    (let (searched)
      (cl-letf (((symbol-function 'search-rfc-number-online)
                 (lambda (&optional n) (setq searched n))))
        (with-temp-buffer
          (insert "rfc-822 style")
          (search-backward "822")
          (browse-rfc-number-at-point)))
      (expect searched :to-equal 822)))

  (it "falls back to rfc-mode-browse off-number when rfc-mode is loaded"
    (let (browsed)
      (with-fake-feature 'rfc-mode
        (cl-letf (((symbol-function 'rfc-mode-browse)
                   (lambda () (setq browsed t))))
          (with-temp-buffer
            (insert "nothing here")
            (goto-char (point-min))
            (browse-rfc-number-at-point))))
      (expect browsed :to-be t)))

  (it "searches online without a number off-number sans rfc-mode"
    (let ((searched 'untouched))
      (cl-letf (((symbol-function 'search-rfc-number-online)
                 (lambda (&optional n) (setq searched n))))
        (with-temp-buffer
          (insert "nothing here")
          (goto-char (point-min))
          (browse-rfc-number-at-point)))
      (expect searched :to-be nil))))

(describe "embark-project-search"
  (it "hands the target to consult-ripgrep as the initial query"
    (let (args)
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (&optional dir initial) (setq args (list dir initial)))))
        (embark-project-search "needle"))
      (expect args :to-equal '(nil "needle")))))

(describe "embark-hide-which-key-indicator"
  (it "strips the which-key indicator while prompting"
    (let ((embark-indicators (list #'embark-which-key-indicator 'other-indicator))
          seen)
      (cl-letf (((symbol-function 'which-key--hide-popup-ignore-command) #'ignore))
        (embark-hide-which-key-indicator
         (lambda (&rest _) (setq seen embark-indicators))))
      (expect seen :to-equal '(other-indicator)))))

;; with-fake-feature keeps embark-preview's (require 'embark) inert; every
;; internal it touches is stubbed per spec
(describe "embark-preview"
  (it "routes github topic urls through forge"
    (let (acted visited)
      (with-fake-feature 'embark
        (cl-letf (((symbol-function 'embark--targets)
                   (lambda () '((:type url :target "https://github.com/agzam/foo/pull/12"))))
                  ((symbol-function 'embark--act)
                   (lambda (fn target &optional quit) (setq acted (list fn target quit))))
                  ((symbol-function 'forge-visit-topic-via-url)
                   (lambda (u &rest _) (setq visited u))))
          (embark-preview)
          (expect acted :to-be-truthy)
          (funcall (nth 0 acted) (plist-get (nth 1 acted) :target))))
      (expect visited :to-equal "https://github.com/agzam/foo/pull/12")
      (expect (nth 2 acted) :to-be nil)))

  (it "previews non-github urls in eww"
    (let (acted browsed)
      (with-fake-feature 'embark
        (cl-letf (((symbol-function 'embark--targets)
                   (lambda () '((:type url :target "https://example.com"))))
                  ((symbol-function 'embark--act)
                   (lambda (fn target &optional quit) (setq acted (list fn target quit))))
                  ((symbol-function 'eww-browse-url)
                   (lambda (u &rest _) (setq browsed u))))
          (embark-preview)
          (funcall (nth 0 acted) (plist-get (nth 1 acted) :target))))
      (expect browsed :to-equal "https://example.com")))

  (it "falls back to embark-dwim for non-url targets"
    (let (dwim)
      (with-fake-feature 'embark
        (cl-letf (((symbol-function 'embark--targets)
                   (lambda () '((:type buffer :target "some-buf"))))
                  ((symbol-function 'embark--act)
                   (lambda (&rest _) (error "should not act directly")))
                  ((symbol-function 'embark-dwim)
                   (lambda () (setq dwim t))))
          (embark-preview)))
      (expect dwim :to-be t)))

  (it "no-ops without targets"
    (let (acted)
      (with-fake-feature 'embark
        (cl-letf (((symbol-function 'embark--targets) #'ignore)
                  ((symbol-function 'embark--act)
                   (lambda (&rest _) (setq acted t))))
          (embark-preview)))
      (expect acted :to-be nil))))

(describe "embark--ephemeral-cleanup"
  (it "unhooks itself and schedules a single minibuffer exit"
    (let ((embark-post-action-hooks
           (list (list t 'embark--ephemeral-cleanup) '(t other-fn)))
          timers)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest args) (push args timers))))
        (embark--ephemeral-cleanup))
      (expect embark-post-action-hooks :to-equal '((t other-fn)))
      (expect (length timers) :to-equal 1))))
