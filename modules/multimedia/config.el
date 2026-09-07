;;; modules/multimedia/config.el -*- lexical-binding: t; -*-

;; Playback and transcripts: the mpv client, subed, and the backend-
;; switching `media-transient' (autoload/media.el) that drives either mpv
;; or the browser through navegosa - a web-browsing package, called
;; across the module boundary the way embark calls `media-open'.
;; autoload/yt.el is the yt-dlp download the chat module attaches with.

(use-package mpv
  :defer t
  :config
  (setopt mpv-volume-step 1.1))

;; The package's own autoload cookie binds T on embark's file and url maps
;; as soon as embark loads; the embark module routes the same command on
;; "b t" for youtube links.
(use-package transcripto
  :ensure (transcripto :host github :repo "agzam/transcripto.el")
  :defer t)

(use-package subed
  :ensure (subed :host github :repo "sachac/subed" :files ("subed/*.el"))
  :defer t
  :init
  ;; a transcript is thousands of short lines: the line-count rule of
  ;; `doom-so-long-p' would trim the buffer's minor modes for nothing
  (add-to-list 'doom-file-lines-threshold-alist '("\\.\\(?:srt\\|vtt\\|ass\\)\\'"))
  :config
  (add-hook! 'subed-mode-hook
             #'subed-enable-pause-while-typing
             #'subed-enable-sync-player-to-point
             #'subed-enable-sync-point-to-player)
  (map! :map subed-mode-map
        ;; the text-landing motions, not the id ones: their target stays
        ;; visible while `subed-toggle-srt-metadata' hides the timestamps
        :n "]]" #'subed-forward-subtitle-text
        :n "[[" #'subed-backward-subtitle-text
        :localleader
        (:prefix ("t" . "toggle")
                 :desc "metadata" "t" #'subed-toggle-srt-metadata
                 :desc "seek player to point" "s" #'subed-toggle-sync-player-to-point
                 :desc "point follows player" "f" #'subed-toggle-sync-point-to-player)
        "v" #'subed-view-plain-text
        "p" #'subed-mpv-play-media))
