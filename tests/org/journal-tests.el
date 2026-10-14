;;; tests/org/journal-tests.el --- org/autoload/journal.el specs -*- lexical-binding: t; -*-
;; open-journal and vulpea-journal--type-from-note need live vulpea
;; structs/db - smoke-only.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/org/autoload/journal.el")

;; vulpea-journal is not installed in the batch tier
(defvar vulpea-journal-tag "journal")

(describe "journal-template"
  (it "dispatches on the global type"
    (let ((vulpea-journal--type 'work))
      (let ((tpl (journal-template nil)))
        (expect (plist-get tpl :tags) :to-equal '("work-notes"))
        (expect (plist-get tpl :file-name) :to-match "work-notes")))
    (let ((vulpea-journal--type 'personal))
      (expect (plist-get (journal-template nil) :tags)
              :to-equal '("personal-notes"))))
  (it "prefers the buffer-local type over the global"
    (let ((vulpea-journal--type 'work))
      (with-temp-buffer
        (setq-local vulpea-journal--buffer-type 'personal)
        (expect (plist-get (journal-template nil) :tags)
                :to-equal '("personal-notes"))))))

(describe "vulpea-journal--detect-buffer-type"
  (it "detects work journals from filetags under daily/"
    (with-temp-buffer
      (insert "#+filetags: :work-notes:\n")
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda (&optional _) "/home/u/org/daily/2026-07-work-notes.org")))
        (expect (vulpea-journal--detect-buffer-type) :to-be 'work))))
  (it "detects personal journals"
    (with-temp-buffer
      (insert "#+filetags: :personal-notes:\n")
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda (&optional _) "/home/u/org/daily/2026-07-journal.org")))
        (expect (vulpea-journal--detect-buffer-type) :to-be 'personal))))
  (it "ignores org files outside daily/"
    (with-temp-buffer
      (insert "#+filetags: :work-notes:\n")
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda (&optional _) "/home/u/org/notes.org")))
        (expect (vulpea-journal--detect-buffer-type) :to-be nil))))
  (it "ignores buffers without a file"
    (with-temp-buffer
      (expect (vulpea-journal--detect-buffer-type) :to-be nil))))

(describe "open-journal-date"
  (it "reads the date while the TYPE journal tag is in effect"
    (let (tags opened)
      (with-fake-feature 'vulpea
        (cl-letf (((symbol-function 'vulpea-journal--read-date)
                   (lambda (_prompt) (push vulpea-journal-tag tags) 'picked))
                  ((symbol-function 'open-journal)
                   (lambda (&rest args) (push args opened))))
          (with-temp-buffer
            ;; a work journal buffer must not lend its type to the prompt
            (setq-local vulpea-journal--buffer-type 'work)
            (open-journal-date 'personal)
            (open-journal-date 'work))))
      (expect (nreverse tags) :to-equal '("personal-notes" "work-notes"))
      (expect (nreverse opened) :to-equal '((personal picked) (work picked)))
      (expect vulpea-journal-tag :to-equal "journal"))))
