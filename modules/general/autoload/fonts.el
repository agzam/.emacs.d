;;; modules/general/autoload/fonts.el -*- lexical-binding: t; -*-

(require 'face-remap)

(declare-function consult--read "consult")
(declare-function consult--type-group "consult")
(declare-function consult--type-narrow "consult")

(defconst font-family-types '((?p . "Proportional") (?m . "Monospaced"))
  "Narrowing key and header for each font class.")

(defvar font-family-spacing-cache (make-hash-table :test #'equal)
  "Monospaced-p per family, so grouping survives fast typing.
Metrics cannot change while Emacs runs; a newly installed font needs
`clrhash' here to show up in the right group.")

(defun font-family-monospaced-p (family)
  "Whether FAMILY advances every glyph by the same width.
Reads the fontconfig spacing property, which the Core Text backend
reports too.  A font that names no spacing is proportional - the value
fontconfig itself assumes when a font omits it."
  (let ((cached (gethash family font-family-spacing-cache 'unknown)))
    (if (eq cached 'unknown)
        (puthash family
                 (when-let* ((entity (find-font (font-spec :family family)))
                             (spacing (font-get entity :spacing)))
                   ;; 0 proportional, 90 dual, 100 mono, 110 charcell
                   (<= 90 spacing))
                 font-family-spacing-cache)
      cached)))

(defun font-families-by-spacing ()
  "Available families tagged by class, the proportional ones first.
The tag is what consult groups and narrows on.  Both runs are
alphabetical, and their order decides which header comes first."
  (let (proportional monospaced)
    (dolist (family (font-family-list))
      (if (font-family-monospaced-p family)
          (push (propertize family 'consult--type ?m) monospaced)
        (push (propertize family 'consult--type ?p) proportional)))
    (nconc (sort proportional #'string-lessp)
           (sort monospaced #'string-lessp))))

(defun font-family-face (family)
  "Face spec for FAMILY, without the tag the completion hung on it."
  (list :family (substring-no-properties family)))

(defun font-family-sample (family)
  "Specimen line for FAMILY, set in FAMILY."
  (concat "  " (propertize "Handgloves 0Oo1Il"
                           'face (font-family-face family))))

(defun buffer-font-family (buffer)
  "Family BUFFER renders in, from its buffer face or from the frame."
  (let ((face (and (buffer-local-value 'buffer-face-mode buffer)
                   (buffer-local-value 'buffer-face-mode-face buffer))))
    (or (cond ((consp face) (plist-get face :family))
              ((facep face)
               (let ((family (face-attribute face :family nil t)))
                 (unless (eq family 'unspecified) family))))
        (face-attribute 'default :family))))

;;;###autoload
(defun set-buffer-font (&optional buffer)
  "Set the font family of BUFFER, previewing each candidate in place.
Monospaced and proportional families get a header and a narrowing key
each, because the class is what a font is usually picked or rejected on."
  (interactive)
  (require 'consult)
  (let* ((buffer (or buffer (current-buffer)))
         (original-face (buffer-local-value 'buffer-face-mode-face buffer))
         (original-mode (buffer-local-value 'buffer-face-mode buffer))
         ;; `buffer-face-mode-face' is a defcustom with a global value, so a
         ;; buffer that never set one must be left without a local binding
         (original-local (local-variable-p 'buffer-face-mode-face buffer))
         (family
          (consult--read
           (font-families-by-spacing)
           :prompt (format "Font for %s: " (buffer-name buffer))
           :require-match t
           :sort nil
           :group (consult--type-group font-family-types)
           :narrow (consult--type-narrow font-family-types)
           :annotate #'font-family-sample
           :preview-key 'any
           :default (buffer-font-family buffer)
           :state (lambda (action candidate)
                    ;; consult resets the preview with a nil candidate before
                    ;; it leaves, so an abort lands back on the old font
                    (when (and (eq action 'preview) (buffer-live-p buffer))
                      (with-current-buffer buffer
                        (if candidate
                            (buffer-face-set (font-family-face candidate))
                          (if original-local
                              (setq-local buffer-face-mode-face original-face)
                            (kill-local-variable 'buffer-face-mode-face))
                          (buffer-face-mode (if original-mode 1 -1)))))))))
    (when (and family (buffer-live-p buffer))
      (with-current-buffer buffer
        (buffer-face-set (font-family-face family))))))
