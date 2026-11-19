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

(describe "the bb.edn pty probes"
  (it "boot Emacs in an 80x24 pty, CI's size, whatever terminal runs bb"
    ;; the e2e measures wrapping against the window width
    (with-temp-buffer
      (insert-file-contents bb-edn-tests-file)
      (goto-char (point-min))
      (search-forward "(defn run-pty-probe")
      (let ((probe (buffer-substring (point) (save-excursion (up-list) (point)))))
        (expect probe :to-match "\"stty cols 80 rows 24; exec \"")
        (expect probe :to-match "\\[\"script\" \"-q\" \"/dev/null\" \"sh\" \"-c\" sized\\]")
        (expect probe :to-match "\\[\"script\" \"-qec\" sized \"/dev/null\"\\]")))))

(defun bb-edn-tests--loc-repo (files)
  "Make a temp git repo with a copy of bb.edn and FILES, an alist of (PATH . TEXT)."
  (let* ((dir (file-name-as-directory (make-temp-file "bb-loc" t)))
         (default-directory dir))
    (copy-file bb-edn-tests-file "bb.edn")
    ;; the copy is an .edn file too, and its lines belong to no fixture
    (write-region "bb.edn\n" nil ".gitignore" nil 'silent)
    (pcase-dolist (`(,path . ,text) files)
      (thread-first path expand-file-name file-name-directory (make-directory t))
      (write-region text nil path nil 'silent))
    (call-process "git" nil nil nil "init" "-q")
    dir))

(defun bb-edn-tests--loc (dir)
  "Standard output of `bb loc' run from the copy of bb.edn in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      ;; bb's error report quotes the task's format string, and buttercup
      ;; formats a failure message with the value it got, % signs included
      (call-process "bb" nil '(t nil) nil "--config" "bb.edn" "loc")
      (buffer-string))))

(describe "the bb.edn loc task"
  (it "counts source and tests/ apart, split into blank, comment and code lines"
    (let ((dir (bb-edn-tests--loc-repo
                '(("init.el" . ";;; init.el\n\n(setq a 1) ; trailing\n  ;; indented\n(setq b 2)\n")
                  ("scripts/tool.bb" . "(println 1)\n")
                  ("tests/init-tests.el" . "(it \"works\")\n\n")
                  ("README.md" . "# not code\n")))))
      (unwind-protect
          (let ((out (bb-edn-tests--loc dir)))
            (expect out :to-match "^ +files +lines +blank +comment +code$")
            (expect out :to-match "^source +2 +6 +1 +2 +3$")
            (expect out :to-match "^tests +1 +2 +1 +0 +1$")
            (expect out :to-match "^total +3 +8 +2 +2 +4$"))
        (delete-directory dir t))))

  (it "abbreviates a count of a thousand and more to one decimal and a k"
    (let ((dir (bb-edn-tests--loc-repo
                `(("big.el" . ,(apply #'concat (make-list 1000 "(a)\n")))
                  ("tests/big-tests.el" . ,(apply #'concat (make-list 999 "(a)\n")))))))
      (unwind-protect
          (let ((out (bb-edn-tests--loc dir)))
            (expect out :to-match "^source +1 +1\\.0k +0 +0 +1\\.0k$")
            (expect out :to-match "^tests +1 +999 +0 +0 +999$")
            (expect out :to-match "^total +2 +2\\.0k +0 +0 +2\\.0k$"))
        (delete-directory dir t))))

  (it "skips a tracked file deleted from the work tree"
    (let ((dir (bb-edn-tests--loc-repo '(("kept.el" . "(a)\n") ("gone.el" . "(b)\n")))))
      (unwind-protect
          (let ((default-directory dir))
            (call-process "git" nil nil nil "add" "-A")
            (delete-file "gone.el")
            (expect (bb-edn-tests--loc dir) :to-match "^source +1 +1 +0 +0 +1$"))
        (delete-directory dir t)))))
