;;; tests/web-browsing/subed-tests.el --- web-browsing/autoload/subed.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/subed.el")

(defvar subed-tests--srt
  "1\n00:00:01,000 --> 00:00:02,000\nHello world\n\n2\n00:00:03,000 --> 00:00:04,000\nSecond line\n\n")

(defvar subed-tests--vtt
  "WEBVTT\nKind: captions\nLanguage: en\n\n00:00:01.000 --> 00:00:02.000\nHola mundo\n\n00:00:03.000 --> 00:00:04.000\nSegunda linea\n\n")

(defun subed-tests--metadata-overlays ()
  (seq-filter (lambda (ov) (overlay-get ov 'subtitle-metadata))
              (overlays-in (point-min) (point-max))))

(describe "subed-toggle-srt-metadata"
  (it "hides SRT metadata behind invisible overlays and toggles back"
    (with-temp-buffer
      (insert subed-tests--srt)
      (subed-toggle-srt-metadata)
      (let ((ovs (subed-tests--metadata-overlays)))
        ;; 2 per entry: the id+timestamp block and the trailing newline
        (expect (length ovs) :to-equal 4)
        (expect (seq-every-p (lambda (ov) (overlay-get ov 'invisible)) ovs)
                :to-be-truthy))
      (expect subed--subtitle-metadata-hidden :to-be-truthy)
      (subed-toggle-srt-metadata)
      (expect (subed-tests--metadata-overlays) :to-equal nil)
      (expect subed--subtitle-metadata-hidden :to-be nil)))

  (it "skips the WEBVTT header and covers id-less VTT entries"
    (with-temp-buffer
      (insert subed-tests--vtt)
      (subed-toggle-srt-metadata)
      (let ((ovs (subed-tests--metadata-overlays)))
        (expect (length ovs) :to-equal 4)
        ;; header must not be hidden
        (expect (seq-every-p (lambda (ov) (> (overlay-start ov) (length "WEBVTT")))
                             ovs)
                :to-be-truthy)))))

(describe "subed-view-plain-text"
  :var (shown)

  (before-each (setq shown nil))
  (after-each (when (get-buffer "*Subtitle Text Only*")
                (kill-buffer "*Subtitle Text Only*")))

  (it "extracts only the subtitle text from SRT"
    (cl-letf (((symbol-function 'switch-to-buffer-other-window)
               (lambda (buf) (setq shown buf))))
      (with-temp-buffer
        (insert subed-tests--srt)
        (subed-view-plain-text)))
    (expect (buffer-name shown) :to-equal "*Subtitle Text Only*")
    (expect (with-current-buffer shown (buffer-string))
            :to-equal "Hello world\nSecond line"))

  (it "extracts only the subtitle text from VTT"
    (cl-letf (((symbol-function 'switch-to-buffer-other-window)
               (lambda (buf) (setq shown buf))))
      (with-temp-buffer
        (insert subed-tests--vtt)
        (subed-view-plain-text)))
    (expect (with-current-buffer shown (buffer-string))
            :to-equal "Hola mundo\nSegunda linea")))

(describe "subed-mpv-play-media"
  :var (calls)

  (before-each (setq calls nil))

  (it "prefers the sibling mp3 for srt files"
    (cl-letf (((symbol-function 'subed-mpv-play-from-file)
               (lambda (f) (push (list 'play f) calls)))
              ((symbol-function 'subed-mpv-unpause)
               (lambda () (push '(unpause) calls)))
              ((symbol-function 'buffer-file-name)
               (lambda (&optional _) "/tmp/talk.srt"))
              ((symbol-function 'file-exists-p)
               (lambda (f) (equal f "/tmp/talk.mp3"))))
      (subed-mpv-play-media "/tmp/other.mp4"))
    (expect (reverse calls)
            :to-equal '((play "/tmp/talk.mp3") (unpause))))

  (it "falls back to the given file otherwise"
    (cl-letf (((symbol-function 'subed-mpv-play-from-file)
               (lambda (f) (push (list 'play f) calls)))
              ((symbol-function 'subed-mpv-unpause)
               (lambda () (push '(unpause) calls)))
              ((symbol-function 'buffer-file-name)
               (lambda (&optional _) "/tmp/talk.vtt"))
              ((symbol-function 'file-exists-p) #'ignore))
      (subed-mpv-play-media "/tmp/other.mp4"))
    (expect (reverse calls)
            :to-equal '((play "/tmp/other.mp4") (unpause)))))
