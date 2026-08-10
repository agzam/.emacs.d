;;; modules/search/autoload/consult-omni.el -*- lexical-binding: t; -*-

;; Loading this file (first use of the transient) pulls the whole omni
;; stack: use-package :config loads the source modules and sets API keys.
;; doom.d needed a :before advice for that - source files carry no
;; autoload cookies, so nothing short of loading them defines commands.
(require 'transient)
(require 'consult-omni)

;;;###autoload
(defun consult-omni-no-require-match-a (args)
  "Force :require-match nil so free input always reaches :on-new.
Some sources declare :require-match t (gptel, youtube, apps), which
would reject RET on anything but a fetched candidate.  Options plist
starts after the 3 positionals (develop signature: SOURCES MIN-INPUT
ARGS &rest OPTIONS)."
  (append (seq-take args 3)
          (plist-put (copy-sequence (seq-drop args 3))
                     :require-match nil)))

;;;###autoload
(transient-define-prefix consult-omni-transient ()
  ["consult-omni"
   [("/" "multi" consult-omni-multi)
    ("go" "google" consult-omni-google)
    ("w" "wiki" consult-omni-wikipedia)
    ("y" "youtube" consult-omni-youtube)]
   [("a" "apps" consult-omni-apps)
    ("bh" "browser-hist" consult-omni-browser-history)
    ("gp" "gptel" consult-omni-gptel :if (lambda () (featurep 'gptel)))
    ("gf" "gptel log find" gptel-log-find :if (lambda () (featurep 'gptel)))]
   [("gh" "code search" search-github-with-lang)
    ("gH" "github" consult-omni-github :if (lambda () (featurep 'consult-gh)))
    ("hn" "HN" consult-hn-transient :if (lambda () (featurep 'consult-hn)))]
   [("ss" "slack visible msg" slack-visible-capture)
    ("sy" "slack yank link" slack-visible-yank)
    ("sS" "slack search" slacko-search)]])
