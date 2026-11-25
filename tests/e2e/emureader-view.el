;;; tests/e2e/emureader-view.el --- emureader as the PDF viewer -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; The wiring leg runs anywhere: which mode a PDF and an EPUB get once
;; emureader has loaded (its load adds the claims the config strips), and
;; the keys over evil-collection's.  The view leg needs a GUI frame and a
;; built module - an emureader buffer without one asks to build it - so it
;; reports a loud skip elsewhere.

(require 'cl-lib)

(defun emureader-view--case (label ok got want &optional err)
  (list :label label :ok (and ok (null err)) :got got :want want :err err))

(defun emureader-view-wiring-cases ()
  "Mode routing and key bindings, checked without opening a document."
  (require 'emureader)
  (let* ((epub (assoc-default "book.epub" auto-mode-alist #'string-match))
         (pdf (major-mode-remap
               (or (assoc-default "%PDF" magic-mode-alist)
                   (assoc-default "book.pdf" auto-mode-alist #'string-match))))
         (normal (evil-get-auxiliary-keymap emureader-mode-map 'normal))
         (leader (lambda (k) (concat doom-localleader-key " " k)))
         (want `(("q" . kill-current-buffer)
                 ("J" . emureader-next-page)
                 ("K" . emureader-previous-page)
                 ("C-d" . emureader-scroll-down-screen-or-next-page)
                 ("C-u" . emureader-scroll-up-screen-or-prev-page)
                 ("zk" . emureader-enlarge-size)
                 ("zj" . emureader-shrink-size)
                 ("z0" . emureader-reset-size)
                 ("P" . fit-emureader-page)
                 (,(funcall leader "e") . toggle-emureader)
                 (,(funcall leader ",") . show-emureader-progress)
                 (,(funcall leader "f p") . fit-emureader-page)
                 (,(funcall leader "z k") . emureader-enlarge-size)))
         (keys (mapcar (lambda (w) (cons (car w) (lookup-key normal (kbd (car w)))))
                       want)))
    (list
     (emureader-view--case
      "a PDF opens in emureader, an EPUB in nov"
      (and (eq pdf 'emureader-mode) (eq epub 'nov-mode))
      (format "pdf=%s epub=%s" pdf epub) "pdf=emureader-mode epub=nov-mode")
     (emureader-view--case
      "pdf-view's keys win over evil-collection's in emureader"
      (equal keys want)
      (format "%S" (seq-difference keys want))
      "every key bound as in pdf-view"))))

(defun emureader-view-document-cases ()
  "A generated PDF through emureader with real keys; GUI and module only."
  (cond
   ((not (display-graphic-p))
    (list (emureader-view--case
           "emureader view SKIPPED: tty frame" t "skip" "skip")))
   ((not (emureader--module-built-p))
    (list (emureader-view--case
           "emureader view SKIPPED: module not built (M-x emureader-build)"
           t "skip" "skip")))
   (t
    (let* ((dir (file-name-as-directory (make-temp-file "emureader-e2e" t)))
           (file (expand-file-name "fixture.pdf" dir))
           results)
      ;; the sibling pdf-text scenario owns the fixture writer
      (pdf-text-view--make-pdf file '(("one") ("two") ("three")))
      (unwind-protect
          (progn
            (find-file file)
            (delete-other-windows)
            (redisplay t)
            (push (emureader-view--case
                   "find-file opens the PDF in emureader on page 1"
                   (and (eq major-mode 'emureader-mode)
                        (eql 1 (emureader-current-doc-pagenumber)))
                   (format "mode=%s page=%s" major-mode
                           (ignore-errors (emureader-current-doc-pagenumber)))
                   "mode=emureader-mode page=1")
                  results)
            (discard-input)
            (let (err)
              (condition-case e (execute-kbd-macro (kbd "J"))
                (error (setq err e)))
              (redisplay t)
              (push (emureader-view--case
                     "J turns the page"
                     (eql 2 (emureader-current-doc-pagenumber))
                     (format "page=%s" (emureader-current-doc-pagenumber))
                     "page=2" err)
                    results))
            (let (err fitted zoomed)
              ;; P first: the opening fit ran before the new frame settled
              (condition-case e
                  (progn
                    (execute-kbd-macro (kbd "P"))
                    (setq fitted (emureader-current-doc-scale-value))
                    (execute-kbd-macro (kbd "zk"))
                    (setq zoomed (emureader-current-doc-scale-value))
                    (execute-kbd-macro (kbd "P")))
                (error (setq err e)))
              (push (emureader-view--case
                     "zk zooms in and P fits the page back"
                     (and fitted zoomed (< fitted zoomed)
                          (= fitted (emureader-current-doc-scale-value)))
                     (format "scale %s -> %s -> %s" fitted zoomed
                             (emureader-current-doc-scale-value))
                     "larger after zk, the same fit after P" err)
                    results))
            (let (err)
              (condition-case e
                  (execute-kbd-macro (kbd (concat doom-localleader-key " e")))
                (error (setq err e)))
              (push (emureader-view--case
                     "localleader e reopens the PDF out of emureader"
                     (not (eq major-mode 'emureader-mode))
                     (format "mode=%s" major-mode) "mode other than emureader-mode"
                     err)
                    results))
            (let (err)
              (condition-case e
                  (execute-kbd-macro (kbd (concat doom-localleader-key " e")))
                (error (setq err e)))
              (redisplay t)
              (push (emureader-view--case
                     "localleader e brings it back into emureader"
                     (eq major-mode 'emureader-mode)
                     (format "mode=%s" major-mode) "mode=emureader-mode" err)
                    results))
            (let ((buf (current-buffer)) err)
              (condition-case e (execute-kbd-macro (kbd "q"))
                (error (setq err e)))
              (push (emureader-view--case
                     "q kills the emureader buffer"
                     (not (buffer-live-p buf))
                     (format "live=%s" (buffer-live-p buf)) "live=nil" err)
                    results)))
        (when-let* ((buf (get-file-buffer file)))
          (let ((kill-buffer-query-functions nil))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (setq save-place-alist
              (assoc-delete-all (file-truename file)
                                (assoc-delete-all file save-place-alist)))
        (delete-directory dir t))
      (nreverse results)))))

(defun emureader-view-e2e ()
  "Wiring everywhere; the document leg where a GUI frame and the module allow."
  (append (emureader-view-wiring-cases)
          (emureader-view-document-cases)))

(add-to-list 'e2e-scenarios #'emureader-view-e2e)
