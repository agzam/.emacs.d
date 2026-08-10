;;; tests/search/slack-visible-tests.el --- search/autoload/slack-visible.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; the file's top-level (require 'consult) must no-op in the batch tier -
;; everything under test is package-independent
(provide 'consult)

(load-module-file "modules/search/autoload/slack-visible.el")

(defconst slack-visible-tests--json
  (concat
   "[{\"sender\":\"ag\",\"text\":\"older message\",\"time\":\"9:54\","
   "\"url\":\"https://x.slack.com/archives/C1/p1\","
   "\"frame\":{\"x\":680.0,\"y\":-1231.0,\"w\":1611.0,\"h\":52.0}},"
   "{\"sender\":\"ericdallo\",\"text\":\"newer message\",\"time\":\"10:12\","
   "\"url\":\"https://x.slack.com/archives/C1/p2\","
   "\"frame\":{\"x\":680.0,\"y\":-404.0,\"w\":1611.0,\"h\":79.0}}]"))

(defun slack-visible-tests--b64 (json)
  "Wrap JSON the way jeejah serializes a base64 string value."
  (format "\"%s\"\n" (base64-encode-string (encode-coding-string json 'utf-8) t)))

(describe "slack-visible--messages"
  (it "decodes the base64 JSON payload, newest message first"
    (cl-letf (((symbol-function 'hammerspoon-monroe-eval-sync)
               (lambda (&rest _)
                 (list :value (slack-visible-tests--b64 slack-visible-tests--json)))))
      (let ((msgs (slack-visible--messages)))
        (expect (length msgs) :to-equal 2)
        (expect (plist-get (car msgs) :sender) :to-equal "ericdallo")
        (expect (plist-get (plist-get (car msgs) :frame) :h) :to-equal 79.0))))

  (it "user-errors on an empty message list"
    (cl-letf (((symbol-function 'hammerspoon-monroe-eval-sync)
               (lambda (&rest _) (list :value (slack-visible-tests--b64 "[]")))))
      (expect (slack-visible--messages) :to-throw 'user-error)))

  (it "user-errors when no payload arrives at all"
    (cl-letf (((symbol-function 'hammerspoon-monroe-eval-sync)
               (lambda (&rest _) (list :value nil :out nil))))
      (expect (slack-visible--messages) :to-throw 'user-error))))

(describe "slack-visible--candidates"
  (it "shows the first wrapped line; full text and permalink ride invisibly"
    (let* ((slack-visible-max-text-length 20)
           (msg '(:sender "ag" :time "10:04" :url "https://u1" :frame nil
                  :text "one two  three\nfour five six seven eight nine ten"))
           (cand (car (slack-visible--candidates (list msg))))
           (inv-start (text-property-any 0 (length cand) 'invisible t cand))
           (url-pos (string-match "https://u1" cand)))
      ;; visible head: display-trimmed with an ellipsis, single line
      (expect (substring-no-properties cand 0 inv-start)
              :to-match "\\`one two three four…")
      (expect (substring-no-properties cand 0 inv-start) :not :to-match "seven")
      ;; the hidden tail carries the full flattened text for filtering
      (expect (substring-no-properties cand) :to-match "seven eight nine ten")
      (expect (substring-no-properties cand) :not :to-match "10:04")
      (expect (get-text-property url-pos 'invisible cand) :to-be t)
      (expect (get-text-property 0 'slack-visible-message cand) :to-be msg)))

  (it "hands longer messages a wrapped continuation for the annotation"
    (let* ((long (mapconcat #'identity (make-list 40 "word") " "))
           (msg (list :sender "ag" :time "1:00 PM" :url "https://u1"
                      :frame nil :text long))
           (cand (car (slack-visible--candidates (list msg))))
           (rest (get-text-property 0 'slack-visible-rest cand)))
      ;; 40 x "word " wraps past one 100-column line
      (expect rest :to-be-truthy)
      (expect rest :to-match "\\`  word")
      (expect (slack-visible--annotate cand)
              :to-match "\n  ag  1:00 PM\\'")))

  (it "keeps identical one-liners distinct via the hidden permalink"
    (let* ((mk (lambda (url) (list :sender "ag" :text "yes" :url url :frame nil)))
           (cands (slack-visible--candidates
                   (list (funcall mk "https://u1") (funcall mk "https://u2")))))
      (expect (length (delete-dups (mapcar #'substring-no-properties cands)))
              :to-equal 2)))

  (it "makes the author filterable through the invisible tail"
    (let* ((msg '(:sender "ericdallo" :time "10:04" :url "https://u1" :frame nil
                  :text "plain words only"))
           (cand (car (slack-visible--candidates (list msg))))
           (author-pos (string-match "ericdallo" cand)))
      (expect author-pos :to-be-truthy)
      (expect (get-text-property author-pos 'invisible cand) :to-be t))))

(describe "slack-visible--hl-input"
  (it "passes through unchanged outside the minibuffer"
    (expect (slack-visible--hl-input "plain") :to-equal "plain"))

  (it "delegates highlighting to orderless against the live input"
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t))
              ((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "there"))
              ((symbol-function 'orderless-highlight-matches)
               (lambda (input strs)
                 (list (propertize (car strs) 'hl input)))))
      (let ((res (slack-visible--hl-input "hello there")))
        (expect (get-text-property 0 'hl res) :to-equal "there")))))

(describe "slack-visible--annotate"
  (it "renders dim author and time on the line below"
    (let* ((msg '(:sender "ag" :time "10:04" :url "https://u1" :frame nil
                  :text "hi"))
           (cand (car (slack-visible--candidates (list msg))))
           (ann (slack-visible--annotate cand)))
      (expect ann :to-equal "\n  ag  10:04")
      (expect (get-text-property 1 'face ann) :to-be 'completions-annotations)))

  (it "drops a trailing blank when the message carries no time"
    (let* ((msg '(:sender "ag" :time "" :url "https://u1" :frame nil
                  :text "hi"))
           (cand (car (slack-visible--candidates (list msg))))
           (ann (slack-visible--annotate cand)))
      (expect ann :to-equal "\n  ag"))))

(describe "slack-visible--highlight"
  (it "ships the frame as a fennel table to show-indicator"
    (let (sent)
      (cl-letf (((symbol-function 'hammerspoon-monroe-eval-async)
                 (lambda (form) (setq sent form))))
        (slack-visible--highlight '(:x 680.0 :y -1231.0 :w 1611.0 :h 52.0))
        (expect sent :to-match "ms.show-indicator")
        (expect sent :to-match ":x 680.0")
        (expect sent :to-match ":y -1231.0"))))

  (it "does nothing without a frame"
    (let (sent)
      (cl-letf (((symbol-function 'hammerspoon-monroe-eval-async)
                 (lambda (form) (setq sent form))))
        (slack-visible--highlight nil)
        (expect sent :to-be nil)))))

(describe "slack-visible-capture"
  (it "hands the picked URL to slacko"
    (let (captured)
      (cl-letf (((symbol-function 'slack-visible--pick)
                 (lambda (_) "https://x.slack.com/archives/C1/p2"))
                ((symbol-function 'slacko-thread-capture)
                 (lambda (url) (setq captured url))))
        (slack-visible-capture)
        (expect captured :to-equal "https://x.slack.com/archives/C1/p2")))))

(describe "slack-visible-yank"
  (it "puts the picked URL on the kill ring"
    (let (killed)
      (cl-letf (((symbol-function 'slack-visible--pick)
                 (lambda (_) "https://x.slack.com/archives/C1/p1"))
                ((symbol-function 'kill-new)
                 (lambda (s) (setq killed s))))
        (slack-visible-yank)
        (expect killed :to-equal "https://x.slack.com/archives/C1/p1")))))
