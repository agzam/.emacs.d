;;; tests/e2e/rewrite-pending-keys.el --- rewrite verdict keys -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; A batch buffer has no major mode and no evil, so which command the
;; verdict keys reach can only be proven here.

(require 'cl-lib)

(defun rewrite-pending-keys-e2e ()
  "Press the verdict keys on a real rewrite overlay, from outside it."
  (require 'gptel)
  (require 'gptel-rewrite)
  (unless (fboundp 'gptel-rewrite-pending-mode)
    (load (expand-file-name "modules/ai/autoload/gptel.el" e2e-root)
          nil 'nomessage))
  (let* ((file (expand-file-name "rewrite-keys.org" e2e-work-dir))
         (buf (find-file-noselect file))
         (body "PREFIX line.\nthe original sentence.\nSUFFIX line.\n")
         (results '())
         accept-key reject-key accept-was reject-was)
    (cl-flet ((record (label ok got want)
                (push (list :label (format "rewrite verdict keys: %s" label)
                            :ok ok :got got :want want)
                      results)
                ok)
              (arm ()
                ;; the overlay gptel's rewrite callback leaves behind
                (goto-char (point-min))
                (forward-line 1)
                (let ((ov (make-overlay (point) (line-beginning-position 2))))
                  (overlay-put ov 'gptel-rewrite "the improved sentence.\n")
                  (overlay-put ov 'status (list " Ready" "" "" ""))
                  (overlay-put ov 'keymap gptel-rewrite-actions-map)
                  (setq-local gptel--rewrite-overlays (list ov))
                  (gptel-rewrite-ready-banner ov)
                  ov)))
      (unwind-protect
          (catch 'stop
            (with-current-buffer buf
              (switch-to-buffer buf)
              (delete-other-windows)
              (erase-buffer)
              (insert body)
              (font-lock-ensure)
              (evil-normal-state)
              (goto-char (point-min))
              ;; every key below comes from the keymap, so rebinding one in
              ;; the config moves the scenario with it
              (setq accept-key (gptel-rewrite-pending-key 'gptel--rewrite-accept)
                    reject-key (gptel-rewrite-pending-key 'gptel--rewrite-reject)
                    accept-was (key-binding accept-key)
                    reject-was (key-binding reject-key))
              (unless (record "the verdict keys are bound and busy elsewhere"
                              (and accept-key reject-key
                                   (not (eq accept-was 'gptel--rewrite-accept))
                                   (not (eq reject-was 'gptel--rewrite-reject)))
                              (format "accept=%s->%S reject=%s->%S"
                                      (and accept-key (key-description accept-key))
                                      accept-was
                                      (and reject-key (key-description reject-key))
                                      reject-was)
                              "both bound, both meaning something else")
                (throw 'stop nil))

              (arm)
              (goto-char (point-min)) ;outside the overlay on purpose
              (unless (record "the banner arms the pending mode"
                              (and gptel-rewrite-pending-mode
                                   (eq (key-binding accept-key)
                                       'gptel--rewrite-accept)
                                   (eq (key-binding reject-key)
                                       'gptel--rewrite-reject))
                              (format "mode=%s accept=%S reject=%S"
                                      gptel-rewrite-pending-mode
                                      (key-binding accept-key)
                                      (key-binding reject-key))
                              "mode=t accept=gptel--rewrite-accept reject=gptel--rewrite-reject")
                (throw 'stop nil))

              ;; insert state must come out of arming untouched, whatever
              ;; it binds the verdict keys to
              (goto-char (point-max))
              (evil-insert-state)
              (let ((armed (list (key-binding accept-key)
                                 (key-binding reject-key))))
                (gptel-rewrite-pending-mode -1)
                (let ((bare (list (key-binding accept-key)
                                  (key-binding reject-key))))
                  (gptel-rewrite-pending-mode 1)
                  (evil-normal-state)
                  (unless (record "arming leaves insert state alone"
                                  (equal armed bare)
                                  (format "armed=%S bare=%S" armed bare)
                                  "armed=bare")
                    (throw 'stop nil))))

              ;; the keypress that must beat the major mode's localleader
              (goto-char (point-min))
              (execute-kbd-macro accept-key)
              (let ((got (buffer-substring-no-properties
                          (point-min) (point-max)))
                    (want "PREFIX line.\nthe improved sentence.\nSUFFIX line.\n"))
                (unless (record "the accept key accepts from outside the overlay"
                                (equal got want) got want)
                  (throw 'stop nil)))

              (unless (record "the verdict hands the keys back"
                              (and (not gptel-rewrite-pending-mode)
                                   (eq (key-binding accept-key) accept-was)
                                   (eq (key-binding reject-key) reject-was))
                              (format "mode=%s accept=%S reject=%S"
                                      gptel-rewrite-pending-mode
                                      (key-binding accept-key)
                                      (key-binding reject-key))
                              (format "mode=nil accept=%S reject=%S"
                                      accept-was reject-was))
                (throw 'stop nil))

              ;; rejecting leaves the text as it was
              (erase-buffer)
              (insert body)
              (arm)
              (goto-char (point-min))
              (execute-kbd-macro reject-key)
              (let ((got (buffer-substring-no-properties
                          (point-min) (point-max))))
                (record "the reject key restores the original"
                        (and (equal got body)
                             (not gptel-rewrite-pending-mode))
                        (format "text=%S mode=%s" got gptel-rewrite-pending-mode)
                        (format "text=%S mode=nil" body)))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (bound-and-true-p gptel-rewrite-pending-mode)
              (gptel-rewrite-pending-mode -1))
            (set-buffer-modified-p nil))
          (kill-buffer buf))))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'rewrite-pending-keys-e2e)
