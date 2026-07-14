;;; modules/chat/autoload/telega.el -*- lexical-binding: t; -*-

;;;###autoload
(defun telega-extract-and-attach-video (url)
  "Extract the video at URL and attach it to the current telega chat buffer."
  (interactive (list (read-string
                      "Video URL: "
                      (when-let* ((k (car-safe kill-ring))
                                  ((string-match-p "^https?://" k)))
                        (string-trim k)))))
  (yt-extract-video-y-entonces
   url
   (lambda (fpath)
     (funcall-interactively #'telega-chatbuf-attach-video fpath))))
