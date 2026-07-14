;;; tests/chat/telega-tests.el --- chat/autoload/telega.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/chat/autoload/telega.el")

(describe "telega-extract-and-attach-video"
  :var (extracted attached)
  (before-each
    (setq extracted nil attached nil))

  (it "extracts the url, then attaches the produced file to the chat"
    (cl-letf (((symbol-function 'yt-extract-video-y-entonces)
               (lambda (url callback)
                 (setq extracted url)
                 (funcall callback "/tmp/vid.mp4")))
              ((symbol-function 'telega-chatbuf-attach-video)
               (lambda (fpath) (interactive) (setq attached fpath))))
      (telega-extract-and-attach-video "https://youtu.be/xyz")
      (expect extracted :to-equal "https://youtu.be/xyz")
      (expect attached :to-equal "/tmp/vid.mp4")))

  (it "defaults the prompt to a trimmed http url from the kill-ring head"
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional default &rest _) default))
              ((symbol-function 'yt-extract-video-y-entonces)
               (lambda (url _cb) (setq extracted url)))
              ;; must begin with the url (^-anchored); trailing whitespace is
              ;; trimmed off
              (kill-ring '("https://youtu.be/abc\n")))
      (call-interactively #'telega-extract-and-attach-video)
      (expect extracted :to-equal "https://youtu.be/abc")))

  (it "offers no default when the kill-ring head is not a url"
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional default &rest _) (or default "typed")))
              ((symbol-function 'yt-extract-video-y-entonces)
               (lambda (url _cb) (setq extracted url)))
              (kill-ring '("not a url")))
      (call-interactively #'telega-extract-and-attach-video)
      (expect extracted :to-equal "typed"))))
