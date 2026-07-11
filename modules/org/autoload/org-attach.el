;;; modules/org/autoload/org-attach.el -*- lexical-binding: t; -*-
;; Core-slice survivors of doom.d org/autoload/org-attach.el; the gallery/
;; insert-link helpers wait on org-download and Doom browse infra.

;;;###autoload
(defun yank-media--tiff-as-png-a (orig-fun mimetype data)
  "There's never a situation when I want the clipboard image content to be
attached as .tiff. Flameshot.app stores it as tiff."
  (if (string= mimetype "image/tiff")
      (let ((png-data (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert data)
                        (let ((coding-system-for-read 'binary)
                              (coding-system-for-write 'binary))
                          (shell-command-on-region (point-min) (point-max)
                                                   "magick tiff:- png:-"
                                                   (current-buffer)
                                                   'no-mark)
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max))))))
        (funcall orig-fun "image/png" png-data))
    (funcall orig-fun mimetype data)))

;;;###autoload
(defun yank-from-clipboard ()
  (interactive)
  (unless (executable-find "magick")
    (error "Imagemagick is not found"))
  (condition-case err
      (let ((yank-media-preferred-types '(image/tiff)))
        (call-interactively #'yank-media))
    (error
     (when-let* ((_ (string-match "No handler in the current buffer for anything on the clipboard"
                                  (error-message-string err)))
                 (fshot-path (if (featurep :system 'macos)
                                 "/Applications/flameshot.app/Contents/MacOS/flameshot"
                               "flameshot"))
                 (_ (file-exists-p fshot-path))
                 (cmd (format "%s gui" fshot-path)))
       (shell-command-to-string cmd)))))
