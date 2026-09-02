;;; tests/general/fonts-tests.el --- font picker specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/fonts.el")

(defvar fonts-tests-lookups 0
  "How often the stubbed backend was asked for a font entity.")

(defmacro fonts-tests-with-fonts (spacings &rest body)
  "Run BODY with the font backend answering SPACINGS, (FAMILY . SPACING) pairs.
Batch Emacs opens no fonts at all, so the entity a family resolves to is
its own name here, and the real `font-get' still serves the font-spec
the code hands to `find-font'."
  (declare (indent 1))
  `(let ((real-font-get (symbol-function 'font-get)))
     (clrhash font-family-spacing-cache)
     (setq fonts-tests-lookups 0)
     (cl-letf (((symbol-function 'font-family-list)
                (lambda () (mapcar #'car ,spacings)))
               ((symbol-function 'find-font)
                (lambda (spec)
                  (cl-incf fonts-tests-lookups)
                  (car (assoc (symbol-name (funcall real-font-get spec :family))
                              ,spacings))))
               ((symbol-function 'font-get)
                (lambda (entity prop)
                  (if (stringp entity)
                      (alist-get entity ,spacings nil nil #'equal)
                    (funcall real-font-get entity prop)))))
       ,@body)))

(describe "font-family-monospaced-p"
  (it "reads a fixed advance off the spacing property"
    (fonts-tests-with-fonts '(("Mono" . 100) ("Charcell" . 110) ("Dual" . 90))
      (expect (font-family-monospaced-p "Mono") :to-be-truthy)
      (expect (font-family-monospaced-p "Charcell") :to-be-truthy)
      (expect (font-family-monospaced-p "Dual") :to-be-truthy)))

  (it "calls a proportional font proportional"
    (fonts-tests-with-fonts '(("Sans" . 0))
      (expect (font-family-monospaced-p "Sans") :to-be nil)))

  (it "treats a font that names no spacing as proportional"
    (fonts-tests-with-fonts '(("Quiet" . nil))
      (expect (font-family-monospaced-p "Quiet") :to-be nil)))

  (it "asks the backend once per family"
    (fonts-tests-with-fonts '(("Sans" . 0))
      (font-family-monospaced-p "Sans")
      (font-family-monospaced-p "Sans")
      (expect fonts-tests-lookups :to-equal 1))))

(describe "font-families-by-spacing"
  (it "lists proportional families first, each run alphabetical"
    (fonts-tests-with-fonts '(("Zeta" . 0) ("Mono B" . 100)
                              ("Alpha" . 0) ("Mono A" . 100))
      (expect (font-families-by-spacing)
              :to-equal '("Alpha" "Zeta" "Mono A" "Mono B"))))

  (it "tags each family with the class consult narrows and groups on"
    (fonts-tests-with-fonts '(("Alpha" . 0) ("Mono A" . 100))
      (expect (mapcar (lambda (cand) (get-text-property 0 'consult--type cand))
                      (font-families-by-spacing))
              :to-equal '(?p ?m)))))

(describe "font-family-face"
  (it "drops the tag the completion hung on the family"
    (expect (font-family-face (propertize "Alpha" 'consult--type ?p))
            :to-equal '(:family "Alpha"))
    (expect (get-text-property 0 'consult--type
                               (plist-get (font-family-face
                                           (propertize "Alpha" 'consult--type ?p))
                                          :family))
            :to-be nil)))

(describe "buffer-font-family"
  (it "reads the family out of a buffer face"
    (with-temp-buffer
      (buffer-face-set '(:family "Alpha"))
      (expect (buffer-font-family (current-buffer)) :to-equal "Alpha")))

  (it "ignores a buffer face that is switched off"
    (with-temp-buffer
      (setq-local buffer-face-mode-face '(:family "Alpha"))
      (expect (buffer-font-family (current-buffer)) :not :to-equal "Alpha"))))

(defun fonts-tests--config-forms ()
  "Every top-level form in the root config.el."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "config.el" test-config-root))
    (let (forms form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (push form forms))
      (nreverse forms))))

(describe "the toggle leader"
  (it "opens the font picker on f"
    (expect (cl-search '("f" function set-buffer-font)
                       (flatten-tree (fonts-tests--config-forms))
                       :test #'equal)
            :to-be-truthy)))

(defvar fonts-tests-read-options nil
  "Options the stubbed `consult--read' last received.")

(defun fonts-tests--read (choice &optional during)
  "A `consult--read' stub answering CHOICE, running DURING on the state fn."
  (lambda (_candidates &rest options)
    (setq fonts-tests-read-options options)
    (when during
      (funcall during (plist-get options :state)))
    choice))

(defmacro fonts-tests-with-consult (read &rest body)
  "Run BODY with consult faked out and `consult--read' answering through READ.
The type helpers hand their alist straight back, so a spec can see which
one the command handed over."
  (declare (indent 1))
  `(with-fake-feature 'consult
     (cl-letf (((symbol-function 'consult--read) ,read)
               ((symbol-function 'consult--type-group)
                (lambda (types) (cons 'group types)))
               ((symbol-function 'consult--type-narrow)
                (lambda (types) (cons 'narrow types))))
       ,@body)))

(describe "set-buffer-font"
  (it "sets the family the read returns"
    (with-temp-buffer
      (fonts-tests-with-fonts '(("Alpha" . 0))
        (fonts-tests-with-consult (fonts-tests--read "Alpha")
          (set-buffer-font (current-buffer))))
      (expect buffer-face-mode :to-be-truthy)
      (expect buffer-face-mode-face :to-equal '(:family "Alpha"))))

  (it "gives the read one header and one narrowing key per class"
    (with-temp-buffer
      (fonts-tests-with-fonts '(("Alpha" . 0))
        (fonts-tests-with-consult (fonts-tests--read "Alpha")
          (set-buffer-font (current-buffer))))
      (expect (plist-get fonts-tests-read-options :group)
              :to-equal (cons 'group font-family-types))
      (expect (plist-get fonts-tests-read-options :narrow)
              :to-equal (cons 'narrow font-family-types))))

  (it "previews a candidate in the buffer being read for"
    (with-temp-buffer
      (let (previewed)
        (fonts-tests-with-fonts '(("Alpha" . 0))
          (fonts-tests-with-consult
              (fonts-tests--read
               nil
               (lambda (state)
                 (funcall state 'preview "Alpha")
                 (setq previewed (list buffer-face-mode buffer-face-mode-face))))
            (set-buffer-font (current-buffer))))
        (expect previewed :to-equal '(t (:family "Alpha"))))))

  (it "leaves an unfonted buffer unfonted when the read is abandoned"
    (with-temp-buffer
      (fonts-tests-with-fonts '(("Alpha" . 0))
        (fonts-tests-with-consult
            (fonts-tests--read
             nil
             (lambda (state)
               (funcall state 'preview "Alpha")
               (funcall state 'preview nil)))
          (set-buffer-font (current-buffer))))
      (expect buffer-face-mode :to-be nil)
      ;; the global default is `variable-pitch'; a buffer that never had a
      ;; face of its own must not be left holding a copy of it
      (expect (local-variable-p 'buffer-face-mode-face) :to-be nil)))

  (it "puts back the face the buffer had before the preview"
    (with-temp-buffer
      (buffer-face-set 'variable-pitch)
      (fonts-tests-with-fonts '(("Alpha" . 0))
        (fonts-tests-with-consult
            (fonts-tests--read
             nil
             (lambda (state)
               (funcall state 'preview "Alpha")
               (funcall state 'preview nil)))
          (set-buffer-font (current-buffer))))
      (expect buffer-face-mode :to-be-truthy)
      (expect buffer-face-mode-face :to-equal 'variable-pitch))))
