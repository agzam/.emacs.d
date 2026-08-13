;;; tests/lisp/doom-keybinds-tests.el --- map! which-key label specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'which-key nil t)

;; `lisp/doom-keybinds.el' can't load in the batch tier (top-level `general',
;; and its `define-prefix-command' would hand back a blank leader map).  Read
;; out the which-key helper and the whole `map!' machinery instead: they only
;; lean on `doom-unquote' and `after!' (doom-compat), and the `general--concat'
;; calls they emit are never run here.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "lisp/doom-keybinds.el" test-config-root))
  (goto-char (point-min))
  (search-forward "(defun which-key-label-prefix ")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t)
  (search-forward ";;; ** `map!' macro")
  (ignore-error end-of-file
    (while t (eval (read (current-buffer)) t))))

(defun map-def-form (key def states desc)
  "Return the single def form `doom--map-def' emits for KEY/DEF/STATES/DESC."
  (let ((doom--map-batch-forms nil))
    (doom--map-def key def states desc)
    (nth 1 (cadar doom--map-batch-forms))))

(defun label-prefix-calls (form)
  "Argument lists of every `which-key-label-prefix' call inside FORM."
  (cond ((not (consp form)) nil)
        ((eq (car form) 'which-key-label-prefix) (list (cdr form)))
        (t (append (label-prefix-calls (car form))
                   (label-prefix-calls (cdr form))))))

(defun shown-bindings (keymap)
  "KEYMAP as which-key renders it: an alist of (KEY . DESCRIPTION)."
  (with-temp-buffer
    (fundamental-mode)
    (mapcar (lambda (b)
              (cons (substring-no-properties (nth 0 b))
                    (substring-no-properties (nth 2 b))))
            (which-key--get-bindings nil keymap nil nil))))

(describe "doom--map-def :desc handling"
  (it "embeds a terminal key's label in the keymap as a (DESC . DEF) cons"
    ;; This is what survives embark showing a sub-keymap with the prefix
    ;; stripped: built-in which-key reads the car of the menu item directly,
    ;; where a global key-sequence replacement (keyed on the whole sequence)
    ;; would never match.
    (let ((form (map-def-form "s" '(function go-jira-search-slack-threads)
                              '(nil) "Slack Threads")))
      (expect (car form) :to-be 'cons)
      (expect form :to-equal
              '(cons "Slack Threads" (function go-jira-search-slack-threads)))))

  (it "keeps a leader prefix label as an :ignore replacement, not a fresh keymap"
    ;; Only the leader tree reaches this branch (see `map!' :prefix), and it
    ;; labels the whole <leader>-anchored sequence.  Rebinding the prefix to
    ;; (cons DESC (make-sparse-keymap)) instead would drop the keys another
    ;; module already bound under the same prefix.
    (let ((form (map-def-form "" nil '(nil) "find")))
      (expect (car form) :to-be 'list)
      (expect form :to-equal '(list :ignore t :which-key "find"))))

  (it "still routes an extended (keyword-car) def through :which-key"
    (let ((form (map-def-form "k" '(:def bar) '(nil) "Edit")))
      (expect (car form) :to-be 'quote)
      (expect (plist-get (cadr form) :def) :to-be 'bar)
      (expect (plist-get (cadr form) :which-key) :to-equal "Edit")))

  (it "leaves a label-less key untouched"
    (expect (map-def-form "k" '(function foo) '(nil) nil)
            :to-equal '(function foo))))

(describe "map! :prefix labels"
  (it "labels a keymap prefix through which-key-label-prefix"
    (let ((form (doom--map-process
                 '((:map embark-url-map
                    (:prefix ("b" . "browse") "e" #'eww))))))
      (expect (label-prefix-calls form) :to-equal '(("b" "browse")))
      ;; nothing is bound on the prefix key itself
      (expect (format "%S" form) :not :to-match ":which-key")))

  (it "carries the outer prefix into a nested label"
    (let ((form (doom--map-process
                 '((:map embark-region-map
                    (:prefix ("x" . "text")
                     (:prefix ("l" . "language") "d" #'define-it-at-point)))))))
      (expect (label-prefix-calls form)
              :to-equal '(("x" "text")
                          ((general--concat t "x" "l") "language")))))

  (it "leaves the leader tree on its <leader>-anchored replacement"
    ;; A leader label is keyed on the full "SPC b" sequence, which no
    ;; prefix-stripped sub-keymap can match, so it stays with general.
    (let ((form (doom--map-process '(:leader (:prefix ("b" . "buffer")
                                              "k" #'kill-buffer)))))
      (expect (label-prefix-calls form) :to-be nil)
      (expect form :to-contain '(list :ignore t :which-key "buffer")))))

(describe "which-key-label-prefix"
  (before-each (assume (fboundp 'which-key--maybe-replace)
                       "which-key internals changed"))

  (it "renames a prefix binding, bare or already labelled"
    (let ((which-key-replacement-alist nil))
      (which-key-label-prefix "b" "browse")
      (expect (which-key--maybe-replace '("b" . "prefix"))
              :to-equal '("b" . "browse"))
      ;; a (DESC . KEYMAP) menu item renders as "group:DESC" - replaced whole,
      ;; not spliced into the old label
      (expect (which-key--maybe-replace '("b" . "group:stale"))
              :to-equal '("b" . "browse"))))

  (it "leaves a command sharing the key alone"
    ;; The regression: embark hands which-key the sub-keymap of a pressed
    ;; prefix with the prefix stripped, so "c b" arrives as a bare "b" and a
    ;; key-only replacement would call the command "browse".
    (let ((which-key-replacement-alist nil))
      (which-key-label-prefix "b" "browse")
      (expect (which-key--maybe-replace '("b" . "link->link-bug-reference"))
              :to-equal '("b" . "link->link-bug-reference"))))

  (it "matches the exact key sequence only"
    (let ((which-key-replacement-alist nil))
      (which-key-label-prefix "x l" "language")
      (expect (which-key--maybe-replace '("x l" . "prefix"))
              :to-equal '("x l" . "language"))
      (expect (which-key--maybe-replace '("l" . "prefix"))
              :to-equal '("l" . "prefix"))))

  (it "registers a label once, however often the module reloads"
    (let ((which-key-replacement-alist nil))
      (which-key-label-prefix "b" "browse")
      (which-key-label-prefix "b" "browse")
      (expect (length which-key-replacement-alist) :to-equal 1))))

(describe "the embedded label under embark's prefix strip"
  ;; The embark indicator resolves a pressed prefix to its bare sub-keymap
  ;; (`lookup-key' drops the menu-item label) and hands that to which-key with
  ;; no prefix context, so the label has to ride on the binding itself.
  (it "resolves the sub-keymap and keeps the command runnable"
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f s") (cons "Slack Threads" #'ignore))
      (let ((submap (lookup-key map (kbd "f"))))
        (expect (keymapp submap) :to-be-truthy)
        (expect (lookup-key map (kbd "f s")) :to-be #'ignore)
        (expect (car (cdr (assq ?s (cdr submap)))) :to-equal "Slack Threads"))))

  (it "renders the label through which-key on the standalone sub-keymap"
    (assume (fboundp 'which-key--get-bindings) "which-key internals changed")
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f g") (cons "GH PRs" #'ignore))
      (define-key map (kbd "f s") (cons "Slack Threads" #'ignore))
      (let* ((submap (lookup-key map (kbd "f")))
             (which-key-replacement-alist nil)
             (shown (shown-bindings submap)))
        (expect (assoc "s" shown) :to-equal '("s" . "Slack Threads"))
        (expect (assoc "g" shown) :to-equal '("g" . "GH PRs")))))

  (it "keeps a prefix label off a command one level down"
    ;; The embark url-type maps: a per-type keymap parented on `embark-url-map',
    ;; with its own raw binding overriding the parent's labelled one.  Pressing
    ;; "c" shows that sub-keymap with the prefix stripped, where the top-level
    ;; "b" -> "browse" label used to take over the command's name.
    (assume (fboundp 'which-key--get-bindings) "which-key internals changed")
    (let ((url-map (make-sparse-keymap))
          (type-map (make-sparse-keymap))
          (which-key-replacement-alist nil))
      (define-key url-map (kbd "b o") (cons "browser" #'browse-url))
      (define-key url-map (kbd "c m") (cons "markdown link" #'ignore))
      (set-keymap-parent type-map url-map)
      (define-key type-map (kbd "b b") 'forge-visit-topic-via-url)
      (define-key type-map (kbd "c b") 'link->link-bug-reference)
      (which-key-label-prefix "b" "browse")
      (which-key-label-prefix "c" "convert")
      (let ((top (shown-bindings type-map))
            (sub (shown-bindings (lookup-key type-map (kbd "c")))))
        ;; the prefixes keep their labels
        (expect (cdr (assoc "b" top)) :to-match "browse")
        (expect (cdr (assoc "c" top)) :to-match "convert")
        ;; the command under the prefix keeps its own name
        (expect (assoc "b" sub) :to-equal '("b" . "link->link-bug-reference"))
        (expect (assoc "m" sub) :to-equal '("m" . "markdown link"))))))

(provide 'doom-keybinds-tests)
;;; tests/lisp/doom-keybinds-tests.el ends here
