;;; modules/web-browsing/autoload/subed.el -*- lexical-binding: t; -*-
(defvar subed-mpv--server-proc)
(defvar subed-mpv--retry-delays)
(defvar subed-mpv-media-file)
(defvar subed-mpv-is-playing)
(defvar mpv--process)

(defvar-local subed--subtitle-metadata-hidden nil
  "Whether subtitle metadata is currently hidden.")

;;;###autoload
(defun subed-mpv-attach (file)
  "Sync this subtitle buffer with the player mpv.el already runs.
No second mpv: that player loads FILE unless it plays it already, and
subed's own client joins its socket.  subed derives its socket path
from the buffer file name, so a link at that path lets subed's connect
and cleanup code run unchanged; the cleanup removes only the link."
  (subed-mpv-kill)
  (let ((file (expand-file-name file)))
    (unless (equal (file-truename file)
                   (ignore-errors (file-truename (mpv-get-property "path"))))
      (mpv-play file))
    (setq subed-mpv-media-file file)
    (subed-clear-file-duration-ms-cache)
    (make-symbolic-link (mpv-ipc-socket mpv--process) (subed-mpv--socket) t)
    (subed-mpv--client-connect subed-mpv--retry-delays)
    (subed-mpv-add-subtitles (buffer-file-name))
    (subed-mpv--client-send '(observe_property 1 time-pos))
    (setq subed-mpv-is-playing (eq (mpv-get-property "pause") :json-false))))

;; subed runs `subed-mpv-play-from-file-hook' by value, so the hook signals
;; as soon as a function sits on it; advice is the seam that works
(defadvice! subed-mpv-share-player-a (orig file)
  "One player for subed and mpv.el.
While mpv.el runs a player that is not this buffer's own, subed joins
it; otherwise subed starts one and mpv.el adopts it.  Either way
`media-transient' drives the video from any buffer and subed keeps its
point/player sync."
  :around #'subed-mpv-play-from-file
  (if (and (featurep 'mpv) (mpv-live-p)
           (not (eq mpv--process subed-mpv--server-proc)))
      (subed-mpv-attach file)
    (funcall orig file)
    (mpv-connect subed-mpv--server-proc)))

;;;###autoload
(defun subed-toggle-srt-metadata ()
  "Hide timestamps behind overlays in subtitle files (.srt or .vtt)."
  (interactive)
  (if subed--subtitle-metadata-hidden
      (remove-overlays nil nil 'subtitle-metadata t)
    (save-excursion
      (goto-char (point-min))
      ;; Skip VTT header if present
      (when (looking-at "WEBVTT")
        (forward-line 1)
        (while (looking-at "^\\(NOTE\\|Kind:\\|Language:\\)")
          (forward-line 1))
        (when (looking-at "^$")
          (forward-line 1)))
      ;; Match both SRT and VTT subtitle entries
      ;; SRT: number\ntimestamp --> timestamp\ntext\n\n
      ;; VTT with ID: id\ntimestamp --> timestamp\ntext\n\n
      ;; VTT without ID: timestamp --> timestamp\ntext\n\n
      (while (re-search-forward "^\\(\\(?:[0-9]+\n\\)?\\([0-9:.,-]+ --> [0-9:.,-]+\\(?: .*\\)?\\)\\)\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
        (let ((ov-metadata (make-overlay (match-beginning 1) (match-beginning 3)))
              (ov-extra-newline (make-overlay (1+ (match-end 3)) (match-end 0))))
          (overlay-put ov-metadata 'invisible t)
          (overlay-put ov-metadata 'subtitle-metadata t)
          (overlay-put ov-extra-newline 'invisible t)
          (overlay-put ov-extra-newline 'subtitle-metadata t)))))
  (setq subed--subtitle-metadata-hidden (not subed--subtitle-metadata-hidden))
  (redraw-display))

;;;###autoload
(defun subed-view-plain-text ()
  "Show only subtitle text (without timestamp metadata) in a separate buffer.
Works with both .srt and .vtt files."
  (interactive)
  (let ((text-content "")
        (buf (get-buffer-create "*Subtitle Text Only*")))
    (save-excursion
      (goto-char (point-min))
      ;; Skip VTT header if present
      (when (looking-at "WEBVTT")
        (forward-line 1)
        (while (looking-at "^\\(NOTE\\|Kind:\\|Language:\\)")
          (forward-line 1))
        (when (looking-at "^$")
          (forward-line 1)))
      ;; Match both SRT and VTT subtitle entries
      ;; SRT: number\ntimestamp --> timestamp\ntext\n\n
      ;; VTT with ID: id\ntimestamp --> timestamp\ntext\n\n
      ;; VTT without ID: timestamp --> timestamp\ntext\n\n
      (while (re-search-forward "^\\(?:[0-9]+\n\\)?[0-9:.,-]+ --> [0-9:.,-]+\\(?: .*\\)?\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
        (let ((subtitle-text (match-string 1)))
          ;; Add spacing between subtitles but avoid consecutive empty lines
          (when (> (length text-content) 0)
            (setq text-content (concat text-content "\n")))
          (setq text-content (concat text-content subtitle-text)))))
    (with-current-buffer buf
      (erase-buffer)
      (insert text-content)
      ;; Clean up any remaining consecutive empty lines
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n"))
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

(defun subed--sibling-mp3 ()
  "Return the mp3 next to this srt file, or nil.
An mp3 extracted from the video is the lighter thing to play along."
  (when-let* ((current-file (buffer-file-name))
              ((equal (file-name-extension current-file) "srt"))
              (mp3 (concat (file-name-sans-extension current-file) ".mp3"))
              ((file-exists-p mp3)))
    mp3))

;;;###autoload
(defun subed-mpv-play-media (&optional file)
  "Play FILE in subed's mpv, or the media that belongs to this subtitle file.
Without FILE: the sibling mp3 of an srt, then the media subed guesses
from the base name, then a prompt."
  (interactive)
  (if-let* ((file (or file (subed--sibling-mp3) (subed-guess-media-file))))
      (subed-mpv-play-from-file file)
    (call-interactively #'subed-mpv-play-from-file))
  (subed-mpv-unpause))
