;;; tests/e2e/clipboard-image-menu.el --- the clipimg menu off the leader -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it,
;; and of clipimg-menu.el so nothing shadows the package file).
;;
;; The package's own suite covers the clip, the engines and the menu's
;; readers with the transient never drawn.  What only shows up here: the
;; leader reaching a command elpaca autoloads from a local checkout, the
;; prefix actually setting up, RET inside it landing on the suffix rather
;; than on evil's normal-state map, the columns actually rendering, and an
;; infix set by real keys reaching the action that reads it.  The
;; clipboard, the engine and the network are stubbed - a headless run has
;; none of them, and no suite should upload anything.

(require 'cl-lib)

(defconst clipboard-image-menu-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  "A one pixel PNG, enough for `image-type-from-data' to call it an image.")

(defun clipboard-image-menu--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "clipimg menu: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun clipboard-image-menu-e2e ()
  "Open the menu with the leader, then read the clip with RET."
  (require 'clipimg-menu)
  (let* ((file (expand-file-name "clipimg-case.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (clip (clipimg-clip-create :data clipboard-image-menu-png :type 'png))
         (clipimg-ocr-backend 'e2e-fake)
         (clipimg-ocr-backends
          (list (list 'e2e-fake
                      :label "fake"
                      :available-p #'always
                      :languages #'ignore
                      :recognize (lambda (&rest _) "read text"))))
         results opened)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (switch-to-buffer buf)
            (delete-other-windows)
            (erase-buffer)
            (insert "case\n")
            (evil-force-normal-state)
            (goto-char (point-min)))
          (discard-input)
          (cl-letf (((symbol-function 'clipimg-clipboard-clip) (lambda () clip)))
            (execute-kbd-macro (kbd "SPC i i"))
            (setq opened (and (bound-and-true-p transient--prefix)
                              (oref transient--prefix command)))
            (push (clipboard-image-menu--result
                   "the leader opens the prefix" opened 'clipimg)
                  results)
            (when (eq opened 'clipimg)
              (execute-kbd-macro (kbd "RET"))))
          (let ((buffer (get-buffer clipimg-ocr-buffer-name)))
            (push (clipboard-image-menu--result
                   "RET reads the clip into the buffer"
                   (when buffer
                     (with-current-buffer buffer
                       (list (buffer-substring-no-properties
                              (point-min) (line-end-position))
                             (clipimg-ocr-text)
                             major-mode)))
                   (list "PNG, 70" "read text" 'clipimg-ocr-mode))
                  results)))
      (discard-input)
      (when (bound-and-true-p transient--prefix)
        (ignore-errors (transient--emergency-exit)))
      (when-let* ((buffer (get-buffer clipimg-ocr-buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-other-windows))
    (nreverse results)))

(defun clipboard-image-upload-e2e ()
  "Upload from the menu with real keys, with the network stubbed out."
  (require 'clipimg-menu)
  (let* ((file (expand-file-name "clipimg-upload-case.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (clip (clipimg-clip-create :data clipboard-image-menu-png :type 'png))
         ;; The upload action reaches the system clipboard on purpose, and
         ;; a suite has no business replacing what the user copied.
         (select-enable-clipboard nil)
         (kill-ring nil)
         posted results opened drawn)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (switch-to-buffer buf)
            (delete-other-windows)
            (erase-buffer)
            (insert "case\n")
            (evil-force-normal-state)
            (goto-char (point-min)))
          (discard-input)
          (cl-letf (((symbol-function 'clipimg-clipboard-clip) (lambda () clip))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'clipimg-upload--post)
                     (lambda (url &rest _)
                       (setq posted url)
                       "https://example.test/e2e.png\n")))
            (execute-kbd-macro (kbd "SPC i i"))
            (setq opened (and (bound-and-true-p transient--prefix)
                              (oref transient--prefix command)))
            (setq drawn (when-let* ((buffer (get-buffer transient--buffer-name)))
                          (with-current-buffer buffer
                            (substring-no-properties (buffer-string)))))
            (push (clipboard-image-menu--result
                   "the Upload column is drawn"
                   (mapcar (lambda (want) (and drawn (string-search want drawn) t))
                           '("Upload" "service: " "insert URL at point"))
                   '(t t t))
                  results)
            (when (eq opened 'clipimg)
              (execute-kbd-macro (kbd "-s 0x0 RET"))
              (execute-kbd-macro (kbd "u"))))
          (push (clipboard-image-menu--result
                 "u posts to the host -s named, and the URL lands on the kill ring"
                 (list posted (car kill-ring))
                 (list "https://0x0.st" "https://example.test/e2e.png"))
                results))
      (discard-input)
      (when (bound-and-true-p transient--prefix)
        (ignore-errors (transient--emergency-exit)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-other-windows))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'clipboard-image-menu-e2e)
(add-to-list 'e2e-scenarios #'clipboard-image-upload-e2e)
