;;; modules/search/autoload/slack-visible.el -*- lexical-binding: t; -*-

;; Minibuffer picker over the Slack messages currently visible on screen.
;; The AX-tree scraping lives in spacehammer's my-slack module; Emacs
;; reaches it through the hammerspoon module's monroe bridge.  Kept out of
;; slacko deliberately: the package boundary is "here is a message URL" -
;; everything upstream of the URL is this machine's glue.

(require 'consult)

(declare-function slacko-thread-capture "slacko-thread")
(declare-function hammerspoon-monroe-eval-sync "monroe")
(declare-function hammerspoon-monroe-eval-async "monroe")

(defun slack-visible--messages ()
  "Slack messages currently visible on screen, newest first.
Each is a plist (:sender :text :time :url :frame), straight from
my-slack's visible-messages-json.  The payload travels base64 in the
eval's value: jeejah swallows print output, and its hs.inspect value
serialization would bury raw JSON under another escaping layer."
  (let* ((res (hammerspoon-monroe-eval-sync
               "(let [ms (require :my-slack)] (hs.base64.encode (ms.visible-messages-json)))"))
         (value (string-trim (or (plist-get res :value) "") "[\"\n ]+" "[\"\n ]+")))
    (when (string-empty-p value)
      (user-error "Hammerspoon returned no payload for visible Slack messages"))
    (let* ((json (decode-coding-string (base64-decode-string value) 'utf-8))
           (messages (json-parse-string json :object-type 'plist :array-type 'list)))
      (unless messages
        (user-error "No visible Slack messages found"))
      (nreverse messages))))

(defvar slack-visible-max-text-length 300
  "Character cap for the displayed message text.
Display-only: the full text stays inside the candidate's invisible
tail, so filtering still matches words past this cut (stacktraces,
log dumps and other walls of text).")

(defun slack-visible--fill (text width)
  "Fill TEXT to WIDTH columns, returning the wrapped string."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun slack-visible--candidates (messages)
  "Format MESSAGES as minibuffer candidates, content first.
A candidate is one logical line for the completion UI, so wrapped
content is split: the first wrapped line IS the candidate, the
remaining lines render below it via `slack-visible--annotate',
followed by a dim author/time line.  Since annotations do not
participate in filtering, the full text rides inside the candidate
under an invisible property - typing any word of the message (or
its author) matches, wherever it fell in the wrap.  The invisible
permalink also keeps otherwise identical one-liners distinct for
`consult--lookup-member' and the preview highlight."
  (mapcar
   (lambda (msg)
     (let* ((flat (replace-regexp-in-string
                   "\n+" " " (string-trim (or (plist-get msg :text) ""))))
            (shown (truncate-string-to-width
                    flat slack-visible-max-text-length nil nil "…"))
            (lines (split-string (slack-visible--fill shown 100) "\n" t))
            (head (or (car lines) ""))
            (rest (when (cdr lines)
                    (mapconcat (lambda (l) (concat "  " l)) (cdr lines) "\n")))
            (hidden (propertize
                     (concat flat " "
                             (or (plist-get msg :sender) "") " "
                             (or (plist-get msg :url) ""))
                     'invisible t)))
       (propertize (format "%s %s" head hidden)
                   'slack-visible-message msg
                   'slack-visible-rest rest)))
   messages))

(defun slack-visible--hl-input (str)
  "Highlight the current minibuffer input's matches inside STR.
Annotations never participate in completion-style highlighting, so
matches landing on the wrapped continuation or the author line have
to be lit up manually, consult-line style."
  (let ((input (and (minibufferp) (minibuffer-contents-no-properties))))
    (if (and input (not (string-blank-p input))
             (fboundp 'orderless-highlight-matches))
        (car (orderless-highlight-matches input (list str)))
      str)))

(defun slack-visible--annotate (cand)
  "Render CAND's wrapped continuation lines, then dim author and time.
Both get the current input's matches highlighted via
`slack-visible--hl-input'."
  (if-let* ((msg (get-text-property 0 'slack-visible-message cand)))
      (let ((rest (get-text-property 0 'slack-visible-rest cand))
            (meta (propertize
                   (string-trim-right
                    (format "  %s  %s"
                            (or (plist-get msg :sender) "")
                            (or (plist-get msg :time) "")))
                   'face 'completions-annotations)))
        (slack-visible--hl-input
         (concat (when rest (concat "\n" rest)) "\n" meta)))
    ""))

(defun slack-visible--highlight (frame)
  "Draw the my-slack overlay over FRAME, a plist (:x :y :w :h)."
  (when frame
    (hammerspoon-monroe-eval-async
     (format "(let [ms (require :my-slack)] (ms.show-indicator {:x %s :y %s :w %s :h %s}))"
             (plist-get frame :x) (plist-get frame :y)
             (plist-get frame :w) (plist-get frame :h)))))

(defun slack-visible--unhighlight ()
  "Remove the my-slack overlay."
  (hammerspoon-monroe-eval-async
   "(let [ms (require :my-slack)] (ms.hide-indicator))"))

(defun slack-visible--pick (prompt)
  "Choose among visible Slack messages with PROMPT; return the URL.
Selection preview drives the same highlight overlay in Slack that
the Hammerspoon chooser uses."
  (let* ((cands (slack-visible--candidates (slack-visible--messages)))
         (selected (consult--read
                    cands
                    :prompt prompt
                    :require-match t
                    :sort nil
                    :category 'slack-visible-message
                    :annotate #'slack-visible--annotate
                    ;; lookup-member hands :state and the return value the
                    ;; ORIGINAL propertized candidate; the default identity
                    ;; lookup yields a props-stripped string, which starved
                    ;; the preview of the message data
                    :lookup #'consult--lookup-member
                    ;; the global consult-preview-key is C-SPC; the whole
                    ;; point here is the highlight tracking the selection
                    :preview-key 'any
                    :state (lambda (action cand)
                             (pcase action
                               ('preview
                                (if-let* ((msg (and cand
                                                    (get-text-property
                                                     0 'slack-visible-message cand))))
                                    (slack-visible--highlight (plist-get msg :frame))
                                  (slack-visible--unhighlight)))
                               ('exit (slack-visible--unhighlight))))))
         (msg (and selected (get-text-property 0 'slack-visible-message selected))))
    (or (plist-get msg :url)
        (user-error "Selected candidate carries no URL"))))

;;;###autoload
(defun slack-visible-capture ()
  "Capture a visible Slack message into a slacko thread buffer."
  (interactive)
  (slacko-thread-capture (slack-visible--pick "Slack message: ")))

;;;###autoload
(defun slack-visible-yank ()
  "Kill-ring the permalink of a visible Slack message."
  (interactive)
  (let ((url (slack-visible--pick "Yank Slack link: ")))
    (kill-new url)
    (message "Copied %s" url)))
