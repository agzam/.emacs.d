;;; tests/general/windows-tests.el --- window-layout engine specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

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

(describe "window-cleanup+"
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
            (window-cleanup+)
            (let ((showing-b1
                   (seq-count (lambda (w) (eq (window-buffer w) b1))
                              (window-list nil 'nomini))))
              (expect showing-b1 :to-equal 1)
              (expect (length (window-list nil 'nomini)) :to-equal 2)))
        (delete-other-windows)
        (kill-buffer b1)
        (kill-buffer b2)))))
