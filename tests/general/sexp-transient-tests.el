;;; tests/general/sexp-transient-tests.el --- general/autoload/sexp-transient.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; sexp-transient.el defines a transient prefix at top level.  It requires
;; only transient + the shared bypass lib (smartparens/avy/edit-indirect stay
;; lazy), so it loads in the bare -Q batch env; the sp-*/cider-*/lsp-clojure-*
;; commands it references are just symbols in the layout, never resolved here.
(require 'transient)
(require 'transient-bypass)
(load-module-file "modules/general/autoload/sexp-transient.el")

(describe "sexp-transient"
  (it "is defined as a transient prefix"
    (expect (fboundp 'sexp-transient) :to-be-truthy)
    (expect (get 'sexp-transient 'transient--prefix) :to-be-truthy))
  ;; rot net for the layout; bypass suffixes are built at setup time, not
  ;; part of the static layout, so they are absent here by design
  (it "ships exactly the expected static commands"
    (expect (seq-uniq
             (seq-remove
              (lambda (s) (string-prefix-p "transient:" (symbol-name s)))
              (seq-filter #'symbolp
                          (transient-layout-commands
                           (get 'sexp-transient 'transient--layout)))))
            :to-have-same-items-as
            '(sp-evil-sexp-go-back sp-evil-sexp-go-forward
              sp-backward-parallel-sexp sp-forward-parallel-sexp
              evil-next-visual-line evil-previous-visual-line
              evil-backward-char evil-forward-char
              avy-goto-beg-sexp avy-goto-end-sexp
              sp-wrap-sexp sp-unwrap-sexp sp-reindent
              raise-sexp sp-convolute-sexp sp-transpose-sexp
              sp-split-sexp sp-join-sexp
              sp-narrow-to-current-sexp widen sp-edit-indirect-current-sexp
              sp-forward-slurp-sexp sp-forward-barf-sexp
              sp-backward-slurp-sexp sp-backward-barf-sexp
              sp-kill-sexp sp-copy-sexp evil-undo
              sp-eval-current-in-mode sp-pp-eval-current-in-mode
              cider-pprint-eval-last-sexp-to-comment clojure-toggle-ignore
              lsp-clojure-thread-first lsp-clojure-thread-last
              lsp-clojure-unwind-thread clojure-wrap-rich-comment))))

;; avy is not installed in the bare -Q test env: `with-fake-feature' turns the
;; commands' (require 'avy) into a no-op and `avy-jump' is stubbed to capture
;; the dynamic bindings the command establishes.  A plain function, not a
;; describe-local macro (which would be undefined when the `it' bodies expand).
(defun sexp-transient-test--capture-avy-jump (thunk)
  "Run THUNK with avy faked-loaded and `avy-jump' stubbed to capture bindings.
Return a plist of the regex, avy-jump keywords and the dynamic avy vars seen."
  (with-fake-feature 'avy
    (let (cap)
      (cl-letf (((symbol-function 'avy-jump)
                 (lambda (regex &rest kw)
                   (setq cap (list :regex regex
                                   :window-flip (plist-get kw :window-flip)
                                   :action (plist-get kw :action)
                                   :avy-all-windows avy-all-windows
                                   :avy-action avy-action)))))
        (funcall thunk))
      cap)))

(describe "avy sexp jumps"
  ;; These specs pin the three fixes: current-window scope (avy-all-windows
  ;; nil), no leaked end-action bleeding into a beginning-jump (avy-action
  ;; nil), and - because avy stays unloaded here - that the forward `defvar's
  ;; keep those let bindings dynamic rather than silently lexical (a lexical
  ;; bind would leave avy-all-windows/avy-action unbound and error the stub).
  (it "avy-goto-beg-sexp scopes to the current window, ignoring a leaked action"
    ;; a stale end-jump action left in the global must not turn the
    ;; beginning-jump into an end-jump
    (let* ((avy-all-windows t)               ; global default is all-windows
           (avy-action (lambda (_pt) 'leaked-end-action))
           (cap (sexp-transient-test--capture-avy-jump
                 (lambda ()
                   (let ((this-command 'avy-goto-beg-sexp))
                     (avy-goto-beg-sexp))))))
      (expect (plist-get cap :regex) :to-equal "(+\\|\\[+\\|{+")
      (expect (plist-get cap :window-flip) :to-be nil)
      (expect (plist-get cap :action) :to-be nil)
      (expect (plist-get cap :avy-all-windows) :to-be nil)
      (expect (plist-get cap :avy-action) :to-be nil)))

  (it "avy-goto-end-sexp scopes to the current window and passes a goto-end action"
    (let* ((avy-all-windows t)
           (avy-action nil)
           (cap (sexp-transient-test--capture-avy-jump
                 (lambda ()
                   (let ((this-command 'avy-goto-end-sexp))
                     (avy-goto-end-sexp))))))
      (expect (plist-get cap :regex) :to-equal "\\([^])}>]+\\)[])}]+")
      (expect (plist-get cap :window-flip) :to-be nil)
      (expect (plist-get cap :avy-all-windows) :to-be nil)
      (expect (functionp (plist-get cap :action)) :to-be-truthy)))

  (it "labels the two avy suffixes with their delimiter"
    (let ((alist (transient-layout-suffix-alist
                  (get 'sexp-transient 'transient--layout))))
      (expect (cdr (assq 'avy-goto-beg-sexp alist)) :to-equal "avy (")
      (expect (cdr (assq 'avy-goto-end-sexp alist)) :to-equal "avy )"))))

(describe "sp-eval-current-in-mode"
  ;; guards the doom.d rename: sp-eval-current-sexp -> eval-current-sexp.
  ;; If the reference reverted, call-interactively would hit the void old
  ;; name and error instead of reaching this stub.
  (it "routes non-clojure eval to eval-current-sexp"
    (let (called)
      (cl-letf (((symbol-function 'eval-current-sexp)
                 (lambda () (interactive) (setq called 'eval-current-sexp))))
        (with-temp-buffer
          (fundamental-mode)
          (call-interactively #'sp-eval-current-in-mode)))
      (expect called :to-be 'eval-current-sexp))))
