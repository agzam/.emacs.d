;;; modules/web-browsing/config.el -*- lexical-binding: t; -*-

;; Ported from doom.d modules/custom/web-browsing.  Dropped: elfeed (SohumB
;; fork) + elfeed-org/elfeed-tube/elfeed-tube-mpv, yeetube (unused;
;; git-resurrectable).  mpv promoted to an explicit package - it was a
;; transitive elfeed-tube-mpv dep.
;; NOTE Own packages (browser-hist, consult-hn, reddigg, navegosa) declare
;; GitHub recipes; `local-checkout-recipe' (init.el) redirects them to
;; ~/GitHub/agzam checkouts on machines that have them.

(use-package eww
  :ensure nil  ; built-in
  :commands (eww eww-open-in-other-window)
  :config
  (setopt shr-use-fonts nil
          shr-inhibit-images t
          shr-max-image-proportion 0.5
          shr-max-width nil
          shr-fill-text nil
          eww-browse-url-new-window-is-tab nil
          shr-put-image-function #'shr-put-sliced-image
          eww-readable-urls '("."))

  (add-hook! 'eww-after-render-hook #'eww--rename-buffer)
  (defadvice! eww-rename-buffer-a ()
    :after #'eww-back-url
    :after #'eww-forward-url
    (eww--rename-buffer))

  (add-hook! 'eww-mode-hook
    #'visual-line-mode
    (defun eww-set-local-keys-h ()
      (map! :map shr-map
            "z" nil
            "v" nil)
      (map! :map eww-mode-map
            "C-c C-o" #'eww-browse-with-external-browser
            :n "C-j" (cmd! () (pixel-scroll-precision-scroll-down 50))
            :n "C-k" (cmd! () (pixel-scroll-precision-scroll-up 50))
            :n "j" #'evil-next-visual-line
            :n "k" #'evil-previous-visual-line
            :ni "C-<return>" #'eww-open-in-other-window
            :n "yy" #'eww-copy-current-url
            :n "zk" #'eww-increase-font-size
            :n "zj" #'eww-decrease-font-size
            :n "q" #'kill-buffer-and-window
            [remap imenu] #'eww-jump-to-url-on-page
            :n "[[" #'backward-paragraph
            :n "]]" #'forward-paragraph

            (:localleader
             :desc "prev-url" "[[" #'eww-previous-url
             :desc "next-url" "]]" #'eww-next-url
             :desc "zoom" "z" #'eww-zoom-transient
             :desc "external browser" "e" #'eww-browse-with-external-browser
             :desc "buffers" "b" #'eww-switch-to-buffer
             :desc "reload" "r" #'eww-reload
             (:prefix ("t" . "toggle")
              :desc "readable" "r" #'eww-readable
              :desc "colors" "c" #'eww-toggle-colors
              :desc "fonts" "f" #'eww-toggle-fonts
              :desc "images" "i" #'eww-toggle-images)

             (:prefix ("y" . "copy")
              :desc "copy url" "y" #'eww-copy-current-url
              :desc "copy for Org" "o" #'org-eww-copy-for-org-mode)))))

  ;; (advice-add #'eww-display-html :around #'eww-make-readable-a)
  )

(after! xwidget
  (map!
   :map xwidget-webkit-mode-map
   :n "zk" #'xwidget-webkit-zoom-in
   :n "zj" #'xwidget-webkit-zoom-out
   :localleader
   "x" #'kill-current-buffer))

(use-package browser-hist
  :ensure (browser-hist :host github :repo "agzam/browser-hist.el")
  :init
  ;; embark integration registers itself inside `browser-hist-search' (guarded
  ;; by `boundp'), so there is no need to force-load embark at startup here.
  (setq browser-hist-default-browser 'brave)
  :commands (browser-hist-search))

(use-package mpv
  :defer t
  :config
  (setopt mpv-volume-step 1.1))

(use-package rfc-mode
  :after org)

(use-package subed
  :ensure (subed :host github :repo "sachac/subed" :files ("subed/*.el"))
  :defer t
  :config
  (add-hook! 'subed-mode-hook
             #'subed-enable-pause-while-typing
             #'subed-enable-sync-player-to-point
             #'subed-enable-sync-point-to-player)
  (map! :map subed-mode-map
        :localleader
        (:prefix ("t" . "toggle")
                 "t" #'subed-toggle-srt-metadata)
        "v" #'subed-view-plain-text
        "p" #'subed-mpv-play-media))

(use-package consult-hn
  :ensure (consult-hn :host github :repo "agzam/consult-hn")
  :commands (consult-hn consult-hn-transient)
  :defer t
  :config
  (require 'consult-hn-transient)
  (cl-defun consult-hn-reader (&key hn-object-url &allow-other-keys)
    (hnreader-comment hn-object-url))
  (setopt consult-hn-browse-fn #'consult-hn-reader)
  (transient-remap-suffix-key 'consult-hn-transient "RET" "s-<return>"))

;; hnreader builds in place from ~/GitHub/agzam/emacs-hnreader
;; (local-dev-packages redirect).  Another hard fork, tracked on master:
;; the major-mode branch stays frozen at the head of the upstream PR, so
;; fixes that upstream has no interest in only ever reach master.
(use-package hnreader
  :ensure (hnreader :host github :repo "agzam/emacs-hnreader")
  :defer t
  :hook (hnreader-mode . reddigg-hnreader-show-all-h)
  :config
  ;; HN hands an anonymous client a small request budget and answers 429
  ;; once it is spent, which is a handful of items; fetching through the
  ;; logged-in browser carries the session, same trick as reddigg below
  (when (eq system-type 'darwin)
    (setopt hnreader-fetch-function #'hnreader--fetch-via-browser))

  (map! :map hnreader-mode-map
        "C-c C-o" #'hnreader-browse-nh-story-url
        :n "yy" #'hnreader-copy-hn-story-url
        :n "q" #'kill-buffer-and-window
        :n "^" #'hnreader-goto-parent
        (:localleader
         "[[" #'hnreader-back
         "]]" #'hnreader-more
         (:prefix ("u" . "urls")
          :desc "urls" "s" (cmd! (consult-line-collect-urls "ycombinator\\.com\\|view story in eww")))))

  ;; no ranking numbers on front page
  (advice-add 'hnreader--print-frontpage-item
              :around #'hnreader-frontpage-item-no-rank-a))

;; reddigg builds in place from ~/GitHub/agzam/emacs-reddigg
;; (local-dev-packages redirect).  A hard fork tracked on master: upstream
;; carries none of the fetch layer reddit now demands, and is dormant.
(use-package reddigg
  :ensure (reddigg :host github :repo "agzam/emacs-reddigg")
  :defer t
  :hook (reddigg-mode . reddigg-hnreader-show-all-h)
  :config
  ;; reddit 403-blocks Emacs's TLS fingerprint; fetch through the live
  ;; browser on macOS, headless chromium elsewhere (needs one-time
  ;; M-x reddigg-chromium-warmup)
  (setopt reddigg-fetch-function (if (eq system-type 'darwin)
                                     #'reddigg--fetch-via-browser
                                   #'reddigg--fetch-via-chromium)
          reddigg-subs '(emacs clojure programming))
  (map! :map reddigg-mode-map
        "C-c C-o" #'reddigg-browse-current-sub-url
        :n "yy" #'reddigg-copy-current-sub-url
        :n "q" #'kill-buffer-and-window
        (:localleader
         (:prefix ("u" . "urls")
          :desc "urls" "s" #'consult-line-collect-urls))))

(after! (ol-eww hnreader)
  (defadvice! org-eww-open-other-window-a (orig-fun &rest args)
    "Always open eww links in other window."
    :around #'org-eww-open
    :around #'hnreader-comment
    (let ((display-buffer-alist
           '((".*"
              (display-buffer-reuse-window
               display-buffer-reuse-mode-window
               display-buffer-in-quadrant)
              (direction . right)
              (init-width . 0.5)
              (window . root)))))
      (apply orig-fun args))))

(use-package navegosa
  :ensure (navegosa :host github :repo "agzam/navegosa.el")
  :commands (navegosa-insert-link)
  :config
  (defadvice! navegosa-insert-link-skip-parked-space-a (&rest _)
    "Step over the space Evil parks point on when leaving insert state.
Without this the inserted link glues onto the preceding word."
    :before #'navegosa-insert-link
    (when (and (evil-normal-state-p)
               (not (use-region-p))
               (memq (char-after) '(?\s ?\t)))
      (forward-char 1))))
