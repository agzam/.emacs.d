;;; tests/general/windows-tests.el --- window-layout engine specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "modules/general/autoload/windows.el")

(defun reset-layout-history ()
  "Start each spec with clean rings and a single window."
  (delete-other-windows)
  (clrhash tab-bar-history-back)
  (clrhash tab-bar-history-forward)
  (setq window-layout--pending nil
        window-layout--last-cmd nil))

(describe "window-layout--transient-buffer-p"
  (it "spots transient popup buffers by name"
    (with-current-buffer (get-buffer-create " *transient*-probe")
      (expect (window-layout--transient-buffer-p (current-buffer))
              :to-be-truthy)
      (kill-buffer))
    (with-temp-buffer
      (expect (window-layout--transient-buffer-p (current-buffer))
              :to-be nil))))

(describe "window-layout--norm"
  (before-each (reset-layout-history))
  (after-each (delete-other-windows))
  (it "gives equal fingerprints for unchanged layouts"
    (expect (window-layout--norm) :to-equal (window-layout--norm)))
  (it "changes the fingerprint when the layout changes"
    (let ((before (window-layout--norm)))
      (split-window)
      (expect (window-layout--norm) :not :to-equal before))))

(describe "window-layout--entry"
  (it "snapshots a stock-compatible entry with a fingerprint"
    (let ((entry (window-layout--entry)))
      (expect (window-configuration-p (alist-get 'wc entry)) :to-be-truthy)
      (expect (alist-get 'norm entry) :to-equal (window-layout--norm)))))

(describe "window-undo / window-redo"
  (before-each (reset-layout-history))
  (after-each (delete-other-windows))
  (it "round-trips one recorded layout step"
    ;; bracket a layout-changing command manually, as the hooks would
    (window-layout--pre-change)
    (split-window)
    (let ((this-command 'probe-split))
      (window-layout--record))
    (expect (length (gethash (selected-frame) tab-bar-history-back))
            :to-equal 1)
    (window-undo)
    (expect (length (window-list nil 'nomini)) :to-equal 1)
    (window-redo)
    (expect (length (window-list nil 'nomini)) :to-equal 2))
  (it "collapses consecutive changes by the same command into one step"
    (window-layout--pre-change)
    (split-window)
    (let ((this-command 'probe-split))
      (window-layout--record))
    (window-layout--pre-change)
    (split-window)
    (let ((this-command 'probe-split))
      (window-layout--record))
    (expect (length (gethash (selected-frame) tab-bar-history-back))
            :to-equal 1))
  (it "does nothing at the ring boundary"
    (window-undo)
    (expect (length (window-list nil 'nomini)) :to-equal 1)
    (expect (gethash (selected-frame) tab-bar-history-back) :to-be nil)))

(describe "window-cleanup"
  (after-each (delete-other-windows))
  (it "leaves one window per buffer"
    (delete-other-windows)
    (let ((b1 (generate-new-buffer "cleanup-probe-1"))
          (b2 (generate-new-buffer "cleanup-probe-2")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) b1)
            (set-window-buffer (split-window) b1)
            (set-window-buffer (split-window) b2)
            (window-cleanup)
            (let ((showing-b1
                   (seq-count (lambda (w) (eq (window-buffer w) b1))
                              (window-list nil 'nomini))))
              (expect showing-b1 :to-equal 1)
              (expect (length (window-list nil 'nomini)) :to-equal 2)))
        (delete-other-windows)
        (kill-buffer b1)
        (kill-buffer b2)))))

(describe "keymap-selects-windows-p"
  (it "accepts a keymap binding a winum selector directly"
    (let ((map (make-sparse-keymap)))
      (define-key map "1" 'winum-select-window-1)
      (expect (keymap-selects-windows-p map) :to-be-truthy)))
  (it "accepts a selector bound with a which-key label"
    (let ((map (make-sparse-keymap)))
      (define-key map "2" '("two" . winum-select-window-2))
      (expect (keymap-selects-windows-p map) :to-be-truthy)))
  (it "accepts the composed keymap a leader key resolves to"
    ;; `key-binding' of SPC returns (keymap doom/leader doom/leader)
    (let ((map (make-sparse-keymap)))
      (define-key map "1" 'winum-select-window-1)
      (fset 'window-numbers-probe-map map)
      (unwind-protect
          (expect (keymap-selects-windows-p
                   (make-composed-keymap '(window-numbers-probe-map
                                           window-numbers-probe-map)))
                  :to-be-truthy)
        (fmakunbound 'window-numbers-probe-map))))
  (it "rejects a selector that sits under a nested prefix only"
    (let ((map (make-sparse-keymap))
          (sub (make-sparse-keymap)))
      (define-key sub "1" 'winum-select-window-1)
      (define-key map "w" sub)
      (define-key map "f" 'find-file)
      (expect (keymap-selects-windows-p map) :to-be nil)))
  (it "rejects anything that is not a keymap"
    (expect (keymap-selects-windows-p nil) :to-be nil)
    (expect (keymap-selects-windows-p 'find-file) :to-be nil)))

(defvar window-numbers-tests--shown nil
  "Calls to the stubbed `posframe-show', newest first.")

(defvar window-numbers-tests--hidden nil
  "Buffers passed to the stubbed `posframe-hide', newest first.")

(defun window-numbers-tests--run (numbers workable fn)
  "Call FN with winum reporting NUMBERS and posframe stubbed.
NUMBERS is an alist of (WINDOW . N); WORKABLE is what
`posframe-workable-p' answers."
  (setq window-numbers-tests--shown nil
        window-numbers-tests--hidden nil)
  (with-fake-feature 'ace-window
    (cl-letf (((symbol-function 'winum-get-number)
               (lambda (&optional w) (alist-get (or w (selected-window)) numbers)))
              ((symbol-function 'posframe-workable-p) (lambda () workable))
              ((symbol-function 'posframe-show)
               (lambda (buf &rest args)
                 (push (list buf (selected-window) (plist-get args :string)
                             (plist-get args :poshandler))
                       window-numbers-tests--shown)))
              ((symbol-function 'posframe-hide)
               (lambda (buf) (push buf window-numbers-tests--hidden))))
      (funcall fn))))

(defun reset-window-numbers ()
  "Start and end each spec with one window and no labels."
  (delete-other-windows)
  (setq window-number-labels nil)
  (remove-hook 'pre-command-hook #'hide-window-numbers))

(describe "show-window-numbers"
  (before-all (make-face 'aw-leading-char-face))
  (before-each (reset-window-numbers))
  (after-each (reset-window-numbers))

  (it "puts each window's number over that window, top left"
    (let* ((w1 (selected-window))
           (w2 (split-window-right)))
      (window-numbers-tests--run `((,w1 . 1) (,w2 . 2)) t #'show-window-numbers)
      (expect (reverse window-numbers-tests--shown)
              :to-equal
              `((" *window-number-1*" ,w1 "1" posframe-poshandler-window-top-left-corner)
                (" *window-number-2*" ,w2 "2" posframe-poshandler-window-top-left-corner)))
      ;; global: the next command may run in any buffer
      (expect (memq #'hide-window-numbers (default-value 'pre-command-hook))
              :to-be-truthy)))

  (it "leaves the windows winum does not number alone"
    (let* ((w1 (selected-window))
           (w2 (split-window-right)))
      (split-window w2 nil 'below)
      (window-numbers-tests--run `((,w1 . 1) (,w2 . 2)) t #'show-window-numbers)
      (expect (mapcar #'caddr window-numbers-tests--shown)
              :to-have-same-items-as '("1" "2"))))

  (it "draws nothing for a lone window"
    (window-numbers-tests--run `((,(selected-window) . 1)) t #'show-window-numbers)
    (expect window-numbers-tests--shown :to-be nil)
    (expect (memq #'hide-window-numbers (default-value 'pre-command-hook))
            :to-be nil))

  (it "draws nothing where posframe cannot"
    (let* ((w1 (selected-window))
           (w2 (split-window-right)))
      (window-numbers-tests--run `((,w1 . 1) (,w2 . 2)) nil #'show-window-numbers)
      (expect window-numbers-tests--shown :to-be nil)))

  (it "leaves labels already on display alone"
    (let* ((w1 (selected-window))
           (w2 (split-window-right)))
      (window-numbers-tests--run `((,w1 . 1) (,w2 . 2)) t
                                 (lambda () (show-window-numbers) (show-window-numbers)))
      (expect (length window-numbers-tests--shown) :to-equal 2))))

(describe "hide-window-numbers"
  (before-all (make-face 'aw-leading-char-face))
  (before-each (reset-window-numbers))
  (after-each (reset-window-numbers))

  (it "takes the labels down at the next command, once"
    ;; a buffer without a local `pre-command-hook', so the let binds the global
    (with-temp-buffer
      (let* ((w1 (selected-window))
             (w2 (split-window-right))
             (pre-command-hook nil))
        (window-numbers-tests--run `((,w1 . 1) (,w2 . 2)) t
                                   (lambda ()
                                     (show-window-numbers)
                                     (run-hooks 'pre-command-hook)))
        (expect window-numbers-tests--hidden
                :to-have-same-items-as '(" *window-number-1*" " *window-number-2*"))
        (expect window-number-labels :to-be nil)
        (expect pre-command-hook :to-be nil)))))

(describe "which-key-window-numbers-a"
  :var (calls)
  (before-each
    (setq calls nil)
    (spy-on 'show-window-numbers :and-call-fake (lambda () (push 'show calls)))
    (spy-on 'hide-window-numbers :and-call-fake (lambda () (push 'hide calls))))

  (it "numbers the windows only under a prefix that binds winum selectors"
    (with-temp-buffer
      (let ((map (make-sparse-keymap))
            (windows (make-sparse-keymap))
            (files (make-sparse-keymap)))
        (define-key windows "1" 'winum-select-window-1)
        (define-key files "f" 'find-file)
        (define-key map (kbd "C-c w") windows)
        (define-key map (kbd "C-c f") files)
        (use-local-map map)
        (which-key-window-numbers-a (kbd "C-c w"))
        (which-key-window-numbers-a (kbd "C-c f"))
        ;; which-key's own keymap views pass no prefix
        (which-key-window-numbers-a nil windows)
        (expect (reverse calls) :to-equal '(show hide hide))))))
