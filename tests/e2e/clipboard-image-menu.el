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

(defconst clipboard-image-menu-tiff
  (base64-decode-string
   (concat "SUkqAAoAAAD//w8AAAEDAAEAAAABAAAAAQEDAAEAAAABAAAAAgEDAAEAAAAQAAAAAwEDAAEAAAAB"
           "AAAABgEDAAEAAAABAAAACgEDAAEAAAABAAAAEQEEAAEAAAAIAAAAEgEDAAEAAAABAAAAFQEDAAEA"
           "AAABAAAAFgEDAAEAAAABAAAAFwEEAAEAAAACAAAAHAEDAAEAAAABAAAAKQEDAAIAAAAAAAEAPgEF"
           "AAIAAAD0AAAAPwEFAAYAAADEAAAAAAAAAIXrUQAAAIAAw/WoAAAAAALNzEwAAAAAAc3MTAAAAIAA"
           "zcxMAAAAAAKPwvUAAAAAEDcaoAAAAAACK4cKAAAAIAA="))
  "A one pixel TIFF, the shape a macOS clipboard hands over.")

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
              (execute-kbd-macro (kbd "-s catbox RET"))
              (execute-kbd-macro (kbd "u"))))
          (push (clipboard-image-menu--result
                 "u posts to the host -s named, and the URL lands on the kill ring"
                 (list posted (car kill-ring))
                 (list "https://catbox.moe/user/api.php" "https://example.test/e2e.png"))
                results))
      (discard-input)
      (when (bound-and-true-p transient--prefix)
        (ignore-errors (transient--emergency-exit)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-other-windows))
    (nreverse results)))

(defun clipboard-image-save-e2e ()
  "Save from the menu with real keys, into a real file on disk.
Only the prompt is stubbed - a harness has no one to type a file name.
What the key does after it is the whole point, so the file is written for
real and read back as an image."
  (require 'clipimg-menu)
  (let* ((file (expand-file-name "clipimg-save-case.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (clip (clipimg-clip-create :data clipboard-image-menu-png :type 'png))
         (target (expand-file-name "saved-from-menu.png" e2e-work-dir))
         results opened drawn)
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
                    ((symbol-function 'read-file-name) (lambda (&rest _) target)))
            (execute-kbd-macro (kbd "SPC i i"))
            (setq opened (and (bound-and-true-p transient--prefix)
                              (oref transient--prefix command)))
            (setq drawn (when-let* ((buffer (get-buffer transient--buffer-name)))
                          (with-current-buffer buffer
                            (substring-no-properties (buffer-string)))))
            (push (clipboard-image-menu--result
                   "the Save column is drawn"
                   (mapcar (lambda (want) (and drawn (string-search want drawn) t))
                           '("Save" "to file"))
                   '(t t))
                  results)
            (when (eq opened 'clipimg)
              (execute-kbd-macro (kbd "s"))))
          (push (clipboard-image-menu--result
                 "s writes a file that opens as an image"
                 (list (file-exists-p target)
                       (and (file-exists-p target)
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally target)
                              (image-type-from-data (buffer-string)))))
                 (list t 'png))
                results))
      (discard-input)
      (when (bound-and-true-p transient--prefix)
        (ignore-errors (transient--emergency-exit)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-other-windows))
    (nreverse results)))

(defun clipboard-image-tiff-e2e ()
  "Drive the menu on a TIFF clip, the shape a macOS clipboard hands over.
The other cases carry a PNG, which is the format both settings ask for
anyway, so no conversion ever runs in them.  Here it does, in the save
and in the upload both.  What this machine can convert decides what to
expect: with nothing on PATH the format of the clip has to win
everywhere, which is the rule that keeps a prompt from offering a name
it cannot write."
  (require 'clipimg-menu)
  (let* ((file (expand-file-name "clipimg-tiff-case.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (clip (clipimg-clip-create :data clipboard-image-menu-tiff :type 'tiff))
         (format (if (clipimg-converter) 'png 'tiff))
         (select-enable-clipboard nil)
         (kill-ring nil)
         offered target posted results)
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
                    ((symbol-function 'read-file-name)
                     (lambda (_prompt _dir _default _mustmatch initial &rest _)
                       (setq offered initial
                             target (expand-file-name initial e2e-work-dir))))
                    ((symbol-function 'clipimg-upload--post)
                     (lambda (_url _headers body)
                       (setq posted body)
                       "https://example.test/e2e.png\n")))
            ;; A suffix exits the prefix, so the menu is opened once per key.
            (execute-kbd-macro (kbd "SPC i i"))
            (execute-kbd-macro (kbd "s"))
            (execute-kbd-macro (kbd "SPC i i"))
            (execute-kbd-macro (kbd "u")))
          (push (clipboard-image-menu--result
                 "s offers, and writes, a format this machine can make"
                 (list (and offered (file-name-extension offered))
                       (and target (file-exists-p target)
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally target)
                              (image-type-from-data (buffer-string)))))
                 (list (symbol-name format) format))
                results)
          (push (clipboard-image-menu--result
                 "u sends the clip in that format too"
                 (and posted
                      (string-search (format "Content-Type: image/%s" format) posted)
                      t)
                 t)
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
(add-to-list 'e2e-scenarios #'clipboard-image-save-e2e)
(add-to-list 'e2e-scenarios #'clipboard-image-tiff-e2e)
