;;; tests/bb-edn-tests.el --- batch entry point specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun bb-edn-tests--payload ()
  "Read the string literal(s) of one --eval argument at point.
Steps through the (format ...) and (str ...) wrappers bb.edn uses and
concatenates consecutive literals, so a payload split across lines still
reads as the one form Emacs will receive."
  (let (parts)
    (while (progn
             (skip-chars-forward " \t\n")
             (cond ((looking-at "(format\\|(str") (goto-char (match-end 0)) t)
                   ((eq (char-after) ?\") (push (read (current-buffer)) parts) t)
                   (t nil))))
    (apply #'concat (nreverse parts))))

(defun bb-edn-tests--payloads (file)
  "Every --eval payload FILE hands to a batch Emacs."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (payloads)
      (while (search-forward "\"--eval\"" nil t)
        (push (bb-edn-tests--payload) payloads))
      (nreverse payloads))))

(defun bb-edn-tests--unguarded (file)
  "Payloads in FILE that reach for native-comp without testing for it first."
  (seq-filter (lambda (payload)
                (and (string-match-p "startup-redirect-eln-cache" payload)
                     (not (string-match-p "(featurep 'native-compile)" payload))))
              (bb-edn-tests--payloads file)))

(defun bb-edn-tests--fixture (payload)
  "Write a throwaway bb.edn whose batch vector evals PAYLOAD."
  (let ((file (make-temp-file "bb-edn" nil ".edn")))
    (write-region (format "{:tasks {:init (def emacs-batch [\"emacs\" \"--eval\" %S])}}" payload)
                  nil file nil 'silent)
    file))

(defvar bb-edn-tests-file (expand-file-name "bb.edn" test-config-root))

(describe "the bb.edn batch Emacs invocation"
  (it "keeps the eln cache out of the config root"
    (expect (seq-filter (lambda (payload)
                          (string-match-p "startup-redirect-eln-cache" payload))
                        (bb-edn-tests--payloads bb-edn-tests-file))
            :not :to-be nil))

  (it "guards the redirect for an Emacs built without native-comp"
    ;; CI's Emacs has none: unguarded, the redirect dies on a void
    ;; native-comp-eln-load-path before a single task runs.
    (expect (bb-edn-tests--unguarded bb-edn-tests-file) :to-equal nil))

  (it "flags a redirect that lost its guard"
    (let ((file (bb-edn-tests--fixture "(startup-redirect-eln-cache \"/tmp/eln\")")))
      (unwind-protect
          (expect (bb-edn-tests--unguarded file) :to-equal
                  '("(startup-redirect-eln-cache \"/tmp/eln\")"))
        (delete-file file))))

  (it "hands Emacs well-formed forms" ; a truncated payload evals to nothing good
    (dolist (payload (bb-edn-tests--payloads bb-edn-tests-file))
      (expect (read payload) :not :to-throw))))
