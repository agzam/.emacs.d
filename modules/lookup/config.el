;;; modules/lookup/config.el -*- lexical-binding: t; -*-

;; Vendored core of Doom's :tools lookup (autoload/lookup.el machinery,
;; commands, xref/evil fallbacks), plus-free renames (+lookup/definition ->
;; lookup-definition, +lookup-*-functions -> lookup-*-functions).  The SPC c
;; and SPC s rows in bindings/config.el and both set-lookup-handlers!
;; consumers (lsp module, clojure module) were already written against these
;; names.  2026-07 lookup verdict - see MIGRATION.org Decisions log.
;;
;; K (`lookup-documentation') is in-Emacs only: every target mode registers a
;; handler that renders inside Emacs (helpful, lsp hover -> *lsp-help*, cider,
;; fennel, sdcv, or a Dash docset via eww), the online backend is off the
;; default chain, and `lookup-open-url-fn' is eww - so K never spawns a
;; system browser.
;;
;; Not vendored from Doom: the +docsets / +dictionary machinery.  The three
;; SPC s commands that referenced it (lookup-in-all-docsets,
;; lookup-dictionary-definition, lookup-synonyms) are lab-native thin wrappers
;; over consult-dash / sdcv / mw-thesaurus, in this module's autoload/commands.el.
;; Also dropped: the ivy/helm online provider frontends (the alist keeps plain
;; URL strings) and better-jumper (evil-set-jump records the origin jump).

(defvar lookup-provider-url-alist
  '(("Google"            "https://google.com/search?q=%s")
    ("Google images"     "https://www.google.com/images?q=%s")
    ("Google maps"       "https://maps.google.com/maps?q=%s")
    ("Kagi"              "https://kagi.com/search?q=%s")
    ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("DuckDuckGo"        "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"        "https://devdocs.io/#q=%s")
    ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
    ("StackExchange"     "https://stackexchange.com/search?q=%s")
    ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
    ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
    ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
    ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
    ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
    ("Internet archive"  "https://web.archive.org/web/*/%s")
    ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal"))
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `lookup-online'.")

(defvar lookup-open-url-fn #'eww-browse-url
  "Function to use to open search urls.
Defaults to eww so no lookup path (K's fallback, the SPC s online
providers) ever hands off to a system browser - results stay in Emacs.")

(defvar lookup-definition-functions
  '(lookup-xref-definitions-backend-fn
    lookup-dumb-jump-backend-fn
    lookup-project-search-backend-fn
    lookup-evil-goto-definition-backend-fn)
  "Functions for `lookup-definition' to try.
Stops at the first function to return non-nil or change the current
window/point.  See `set-lookup-handlers!' about adding to this list.")

(defvar lookup-implementations-functions ()
  "Functions for `lookup-implementations' to try.
Stops at the first function to return non-nil or change the current
window/point.  See `set-lookup-handlers!' about adding to this list.")

(defvar lookup-type-definition-functions ()
  "Functions for `lookup-type-definition' to try.
Stops at the first function to return non-nil or change the current
window/point.  See `set-lookup-handlers!' about adding to this list.")

(defvar lookup-references-functions
  '(lookup-xref-references-backend-fn
    lookup-project-search-backend-fn)
  "Functions for `lookup-references' to try.
Stops at the first function to return non-nil or change the current
window/point.  See `set-lookup-handlers!' about adding to this list.")

(defvar lookup-documentation-functions
  ()
  "Functions for `lookup-documentation' to try.
Stops at the first function to return non-nil or change the current
window/point.  See `set-lookup-handlers!' about adding to this list.

Intentionally has no global fallback: every mode we care about registers
an in-Emacs :documentation handler (helpful, lsp hover, cider, sdcv, dash
via eww), so K never falls through to an external browser.  Unhandled
modes get a \"no handler\" message rather than a web search.")

(defvar lookup-file-functions
  '(lookup-bug-reference-backend-fn
    lookup-ffap-backend-fn)
  "Functions for `lookup-file' to try.
Stops at the first function to return non-nil or change the current
window/point.  See `set-lookup-handlers!' about adding to this list.")

;;; Packages

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setopt dumb-jump-default-project doom-emacs-dir
          dumb-jump-prefer-searcher 'rg
          dumb-jump-aggressive nil
          dumb-jump-selector 'popup))

;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'lookup-definition)
(global-set-key [remap xref-find-references]  #'lookup-references)

(after! xref
  ;; Use consult to display xref candidate lists (Doom's vertico glue).
        (setopt xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref))

;;; Keybindings

;; Doom binds these in :editor evil's config behind (modulep! :tools lookup);
;; the module itself is the guard here.
;; gI belongs to ibuffer-sidebar-jump (gi is the gptel-inline dwim);
;; lookup-implementations stays reachable at SPC c i.
(map! :nv "K"  #'lookup-documentation
      :nv "gd" #'lookup-definition
      :nv "gD" #'lookup-references
      :nv "gf" #'lookup-file)
