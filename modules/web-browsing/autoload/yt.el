;;; modules/web-browsing/autoload/yt.el -*- lexical-binding: t; -*-
(defvar yt-extracted-vids ()
  "The list of extracted videos,
where each item is a k/v pair of the url and filepath.")

(defun yt-dlp-command (url)
  "Build the yt-dlp download command for URL.
Two Linux-only twists, both verified against a live Reddit post:
yt-dlp picks the cookie-decryption keyring by desktop environment, and
outside GNOME/KDE (Hyprland here) it falls back to plaintext decrypting
nothing, so the keyring is named explicitly; and some CDNs (Reddit's)
reject the system python's TLS fingerprint outright, so curl_cffi browser
impersonation carries the requests.  macOS needs neither: the key comes
from the Keychain, and its yt-dlp lacks the impersonation extra."
  (format "yt-dlp --verbose --restrict-filenames %s '%s'"
          (if (eq system-type 'gnu/linux)
              "--cookies-from-browser brave+gnomekeyring --impersonate chrome"
            "--cookies-from-browser brave")
          url))

;;;###autoload
(defun yt-extract-video-y-entonces (url &optional callback)
  "Extracts video from URL with yt-dlp and runs CALLBACK fn.

Passes the filepath as the param to CALLBACK."
  (interactive "sVideo URL: ")
  (let* ((default-directory "~/Downloads/")
         (pbuf "*yt-dlp*")
         (process (async-shell-command (yt-dlp-command url) pbuf)))
    (set-process-sentinel
     (get-buffer-process pbuf)
     (lambda (_process event)
       (cond ((string= event "finished\n")
              (when-let* ((fpath (with-current-buffer pbuf
                                   (goto-char (point-max))
                                   (when (re-search-backward
                                          "\\(Deleting original file\\|\\[download\\]\\) \\([^.]+\\)" nil t)
                                     (let ((base-name (match-string-no-properties 2)))
                                       (car-safe
                                        (directory-files
                                         (expand-file-name default-directory)
                                         'full
                                         (concat
                                          "^"
                                          (regexp-quote base-name) "\\."))))))))
                (setf (plist-get yt-extracted-vids url) fpath)
                (when callback (funcall callback fpath))))
             (t (message "downloading %s" url)))))))
