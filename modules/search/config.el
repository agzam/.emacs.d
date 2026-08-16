;;; modules/search/config.el -*- lexical-binding: t; -*-

;; Ported from doom.d modules/custom/search.  Unpinned from Feb-2025
;; 2398ddb: develop since gained consult 3.x compatibility and
;; if-let*/when-let* fixes.  The straight-era load-path hack for the
;; sources dir dissolves under the :files recipe.
(use-package consult-omni
  :ensure (consult-omni :host github :repo "armindarvish/consult-omni"
                        :branch "develop"
                        :files (:defaults "sources/*.el"))
  :defer t
  :config
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)

  ;; doom.d hand-rolled this require loop (consult-omni-load-sources+);
  ;; the package API does it from a list.  elfeed dropped with its package
  ;; (web-browsing port); notmuch parks until its module;
  ;; duckduckgo/invidious/line-multi were loaded but surfaced nowhere.
  (setopt consult-omni-sources-modules-to-load
          '(consult-omni-apps
            consult-omni-brave
            consult-omni-browser-history
            consult-omni-gh
            consult-omni-google
            consult-omni-gptel
            consult-omni-wikipedia
            consult-omni-youtube))
  (consult-omni-sources-load-modules)

  (setopt consult-omni-multi-sources '("Google"
                                       "Brave"
                                       "Wikipedia"
                                       "Browser History"
                                       "gptel"
                                       "GitHub"
                                       "YouTube")
          consult-omni-default-count 30
          consult-omni-dynamic-input-debounce 0.7
          consult-omni-dynamic-refresh-delay 0.5
          consult-omni-default-browse-function #'browse-url)

  ;; keys resolve lazily from authinfo at search time - function values
  ;; are package-sanctioned (consult-omni-expand-variable-function);
  ;; with-temp-message keeps epa's "Decrypting..." out of the minibuffer
  (setopt consult-omni-brave-api-key
          (lambda () (with-temp-message ""
                       (auth-host->pass "api.search.brave.com")))
          consult-omni-youtube-search-key
          (lambda () (with-temp-message ""
                       (auth-host->pass "youtube-api")))
          ;; the googleapis.com record is "CX:KEY", colon-separated
          consult-omni-google-customsearch-cx
          (lambda () (with-temp-message ""
                       (car (split-string (auth-host->pass "www.googleapis.com") ":"))))
          consult-omni-google-customsearch-key
          (lambda () (with-temp-message ""
                       (cadr (split-string (auth-host->pass "www.googleapis.com") ":")))))

  (defadvice! consult-omni-use-thing-at-point-a
    (fn &optional initial no-cb &rest args)
    "Seed omni searches with the region or symbol at point."
    :around #'consult-omni-multi
    :around #'consult-omni-google
    :around #'consult-omni-wikipedia
    :around #'consult-omni-youtube
    :around #'consult-omni-github
    :around #'consult-omni-gptel
    :around #'consult-omni-browser-history
    (let ((init (or initial
                    (if (use-region-p)
                        (buffer-substring (region-beginning) (region-end))
                      (thing-at-point 'symbol :no-props)))))
      (apply fn init no-cb args)))

  (advice-add 'consult-omni--multi-dynamic
              :filter-args #'consult-omni-no-require-match-a)

  (defun consult-omni-embark-video-process (cand)
    "Route the video CAND's url through `process-external-url'."
    (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
        (process-external-url url)))

  (map! :map consult-omni-embark-video-actions-map
        "e" #'consult-omni-embark-video-process))

(use-package tldr
  :defer t
  :config
  (setopt tldr-use-word-at-point t))

(use-package slacko
  :ensure (slacko :host github :repo "agzam/slacko.el")
  :defer t
  :config
  (setopt slacko-default-host "qlikdev.slack.com"))

;; extracted from this module's autoload/search.el (was zoxide-find +
;; add-to-zoxide-cache); the dired module turns tracking on, eshell/z reads
;; through consult-zoxide-read
(use-package consult-zoxide
  :ensure (consult-zoxide :host github :repo "agzam/consult-zoxide.el")
  :defer t)

;; relocated home from the root layer (was pulled ahead for SPC h h)
(use-package consult-symbol
  :ensure (consult-symbol :host github :repo "danielfleischer/consult-symbol")
  :defer t)
