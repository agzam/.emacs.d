;;; tests/email/quotes-tests.el --- quote depth face specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/email/autoload/quotes.el")

(defun quotes-tests-faces (text)
  "Each line of TEXT with the faces left on its first and last character."
  (with-temp-buffer
    (insert text)
    (highlight-mail-quotes)
    (goto-char (point-min))
    (let (lines)
      (while (not (eobp))
        (let ((end (max (point) (1- (line-end-position)))))
          (push (list (buffer-substring-no-properties (point) (line-end-position))
                      (get-text-property (point) 'face)
                      (get-text-property end 'face))
                lines))
        (forward-line 1))
      (nreverse lines))))

(defun quotes-tests-face (text line)
  "Face on the last character of LINE after faces are applied to TEXT."
  (nth 2 (assoc line (quotes-tests-faces text))))

(defun quotes-tests-message-mode-face (text line)
  "Face message-mode's own font-lock puts on LINE of a reply body TEXT."
  (with-temp-buffer
    (let ((message-mode-hook nil)
          (text-mode-hook nil))
      (message-mode))
    (insert "To: a@example.com\nSubject: s\n" mail-header-separator "\n" text)
    (font-lock-ensure)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote line) "$"))
    (prog1 (get-text-property (1- (point)) 'face)
      ;; message-mode offers to save a modified buffer when it is killed
      (set-buffer-modified-p nil))))

(defvar quotes-tests-thread
  (concat "> one\n"
          "> > two\n"
          ">>> three\n"
          "> > > > four\n"
          "> > > > > five\n")
  "Quotes one to five deep.")

(describe "highlight-mail-quotes"
  (it "faces each quoted line by its number of markers"
    (expect (mapcar (lambda (line) (nth 2 line)) (quotes-tests-faces quotes-tests-thread))
            :to-equal '(message-cited-text-1 message-cited-text-2 message-cited-text-3
                        message-cited-text-4 message-cited-text-1)))
  (it "faces the markers along with the text"
    (expect (nth 1 (assoc "> > two" (quotes-tests-faces quotes-tests-thread)))
            :to-be 'message-cited-text-2))
  (it "paints a line the way message-mode paints it in a reply"
    (dolist (line (split-string quotes-tests-thread "\n" t))
      (expect (quotes-tests-face quotes-tests-thread line)
              :to-be (quotes-tests-message-mode-face quotes-tests-thread line))))
  (it "faces a single quoted line"
    ;; gnus-cite ignores a prefix seen on fewer than two lines
    (expect (quotes-tests-faces "Hi,\n> lone\nright.\n")
            :to-equal '(("Hi," nil nil)
                        ("> lone" message-cited-text-1 message-cited-text-1)
                        ("right." nil nil))))
  (it "takes a quote indented by blanks"
    (expect (quotes-tests-face "  > indented\n" "  > indented") :to-be 'message-cited-text-1))
  (it "leaves a line alone when its > comes after text"
    (expect (quotes-tests-faces "a > b\n=> c\n")
            :to-equal '(("a > b" nil nil) ("=> c" nil nil))))
  (it "faces quotes past the size gnus-cite gives up at"
    (let* ((filler (concat "> " (make-string 70 ?x) "\n"))
           (text (concat (apply #'concat (make-list 400 filler)) "> > last\n")))
      (expect (< 25000 (length text)) :to-be t)
      (expect (quotes-tests-face text "> > last") :to-be 'message-cited-text-2)))
  (it "keeps a face already on the text in front"
    (with-temp-buffer
      (insert "> see " (propertize "link" 'face 'link) "\n")
      (highlight-mail-quotes)
      (expect (get-text-property 1 'face) :to-be 'message-cited-text-1)
      (expect (get-text-property 8 'face) :to-equal '(link message-cited-text-1)))))
