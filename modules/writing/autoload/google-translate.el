;;; modules/writing/autoload/google-translate.el -*- lexical-binding: t; -*-

;; doom.d demand-loaded google-translate at startup (:init require) so these
;; helpers and the transient's variable-backed suffixes always had the
;; package's defcustoms around; here the file requires it instead - the
;; loaddefs make this file lazy, so nothing loads before first use.
(require 'google-translate)  ; pulls in google-translate-default-ui
(require 'transient)
(require 'cape nil t)

;;;###autoload
(defun set-google-translate-languages (&optional override-p)
  "Set source language for google translate.
For instance pass En as source for English."
  (interactive "P")
  (let* ((langs (google-translate-read-args override-p nil))
         (source-language (car langs))
         (target-language (cadr langs)))
    (setq google-translate-default-source-language source-language)
    (setq google-translate-default-target-language target-language)
    (message
     (format "Set google translate source language to %s and target to %s"
             source-language target-language))))

;;;###autoload
(defun set-google-translate-target-language ()
  "Set the target language for google translate."
  (interactive)
  (set-google-translate-languages nil))

;;;###autoload
(defun google-translate-es->en ()
  (interactive)
  (let ((google-translate-default-source-language "es")
        (google-translate-default-target-language "en"))
    (google-translate-query-translate)))

;;;###autoload
(defun google-translate-en->es ()
  (interactive)
  (let ((google-translate-default-source-language "en")
        (google-translate-default-target-language "es"))
    (google-translate-query-translate)))

(defvar number-to-words-workdir
  (expand-file-name "number-to-words/" doom-cache-dir)
  "npm workdir for the number-to-words package.
node resolves node_modules from cwd, so installing here keeps the
config dir (and whatever else happens to be cwd) clean.")

;;;###autoload
(defun number-to-words (number)
  "Convert NUMBER into English words using Node.js."
  (make-directory number-to-words-workdir t)
  (let ((default-directory number-to-words-workdir))
    (unless (zerop (call-process "node" nil nil nil
                                 "-e" "require('number-to-words')"))
      (unless (zerop (call-process "npm" nil nil nil
                                   "install" "number-to-words"))
        (user-error "npm install number-to-words failed")))
    (string-trim
     (shell-command-to-string
      (format
       "node -e \"var toWords = require('number-to-words'); console.log(toWords.toWords(%d));\""
       number)))))

(defadvice! google-translate-years-to-words-a
  (orig-fn src-lang tgt-lang text &optional output-dest)
  "Google Translate doesn't spell out numbers and I often need to
see/hear them in their written form. For example, I don't know
how to say \"2023\" in Spanish. This function advises g-translate,
so text that contains something that looks like a year (four
digits), would be converted to a written representation, so a
text like: \"2023 was a better year than 2021\" would translate to:
\"dos mil veintitrés fue un año mejor que dos mil veintiuno\""
  :around #'google-translate-translate
  (if (string= src-lang "en")
      (let ((txt (replace-regexp-in-string
                  "\\b\\([0-9]\\{3,4\\}\\)\\b"
                  (lambda (match)
                    (replace-regexp-in-string
                     ", " " and "
                     (number-to-words (string-to-number match))))
                  text)))
        (funcall orig-fn src-lang tgt-lang txt output-dest))
    (funcall orig-fn src-lang tgt-lang text output-dest)))

(defvar translate--last-context nil)
(defvar translate--field-text nil)
(defvar translate--langs-src '("auto" "en" "es" "ru"))
(defvar translate--langs-target '("en" "es" "ru"))

(defvar translate-history nil
  "Texts sent to translation from the translate field.")

(defvar translate-field--text nil
  "Text the open field hands back when it closes; nil when cancelled.")

(defvar translate-field--buffer nil
  "Buffer of the open translate field.")

(defvar translate-field--origin nil
  "Window the field opened from; the menu redraws with it selected.")

(defvar translate-field--depth nil
  "Recursion depth of the open field's recursive edit.")

(defvar-local translate-field--method nil
  "Input method the field saw last, to notice a switch.")

(defun translate--next (lang langs)
  "Return the language after LANG in LANGS, or the first one if none follows."
  (or (cadr (member lang langs)) (car langs)))

(defun translate--select-source (lang &optional target)
  "Make LANG the source language and pick a different target language for it.
Prefer TARGET, then LANG's habitual pair, then the current target."
  (let ((targets (remove lang translate--langs-target)))
    (setq google-translate-default-source-language lang
          google-translate-default-target-language
          (or (car (member target targets))
              (pcase lang ("en" "es") ("ru" "en") ("es" "en"))
              (car (member google-translate-default-target-language targets))
              (car targets)))))

(defun translate--input-method-language (method)
  "Return the language typed with input METHOD; nil METHOD is no input method.
The pairs come from `google-translate-preferable-input-methods-alist'."
  (cadr (assq (and method (intern (format "%s" method)))
              google-translate-preferable-input-methods-alist)))

(defun translate--language-input-method (lang)
  "Return the input method for typing LANG.
The pairs come from `google-translate-preferable-input-methods-alist'."
  (car (seq-find (lambda (entry) (member lang (cdr entry)))
                 google-translate-preferable-input-methods-alist)))

(transient-define-suffix translate--set-source ()
  :description "source"
  :key "s"
  :variable 'google-translate-default-source-language
  :class 'transient-lisp-variable
  (interactive)
  (translate--select-source
   (translate--next google-translate-default-source-language
                    translate--langs-src))
  (transient-reset))

(transient-define-suffix translate--set-target ()
  :description "target"
  :key "t"
  :variable 'google-translate-default-target-language
  :class 'transient-lisp-variable
  (interactive)
  (thread-last
    translate--langs-target
    (remove google-translate-default-source-language)
    (translate--next google-translate-default-target-language)
    (transient-infix-set (transient-suffix-object))))

(defun translate--field-prompt ()
  "Return the field prompt, naming the translation direction."
  (format "%s → %s: "
          google-translate-default-source-language
          google-translate-default-target-language))

(defun translate--refresh-menu ()
  "Redraw the open menu with the current languages."
  (when (and transient--prefix (window-live-p transient--window))
    (mapc #'transient-init-value transient--suffixes)
    (if (window-live-p translate-field--origin)
        (with-selected-window translate-field--origin (transient--show))
      (transient--show))))

(define-derived-mode translate-field-mode text-mode "Translate"
  "Major mode of the small buffer that holds the text to translate.")

(defun translate-field-history-capf ()
  "Complete the field text from earlier translations."
  (when translate-history
    (list (point-min) (point) translate-history :exclusive 'no)))

(defun translate-field--show-direction ()
  "Show the translation direction in front of the field text."
  (setq-local line-prefix (propertize (translate--field-prompt)
                                      'face 'minibuffer-prompt)))

(defun translate-field-follow-h ()
  "Point the translation source at the language of a newly chosen input method."
  (let ((method (input-method-in-effect)))
    (unless (equal method translate-field--method)
      (setq translate-field--method method)
      (when-let* ((lang (translate--input-method-language method))
                  ((not (equal lang google-translate-default-source-language))))
        (translate--select-source lang)
        (translate-field--show-direction)
        (translate--refresh-menu)))))

(defun translate-field-fit-h ()
  "Fit the field window to its text, from one to six lines."
  (when-let* ((win (get-buffer-window)))
    (when (window-live-p transient--window)
      (window-preserve-size transient--window nil t))
    (let ((window-min-height 1))
      (fit-window-to-buffer win 6 1))))

(defun translate-field--abandon (depth)
  "Cancel the field if its recursive edit at DEPTH is the innermost one."
  (when (eql (recursion-depth) depth)
    (setq translate-field--text nil)
    (exit-recursive-edit)))

(defun translate-field-leave-h (_frame)
  "Cancel the field once a window other than it or the minibuffer is selected."
  (let ((buf (window-buffer (selected-window))))
    (unless (or (eq buf translate-field--buffer) (minibufferp buf))
      (run-at-time 0 nil #'translate-field--abandon translate-field--depth))))

(defun translate-field-submit ()
  "Close the field and translate its text."
  (interactive)
  (setq translate-field--text
        (string-trim (buffer-substring-no-properties (point-min) (point-max))))
  (exit-recursive-edit))

(defun translate-field-cancel ()
  "Close the field and go back to the menu."
  (interactive)
  (setq translate-field--text nil)
  (exit-recursive-edit))

(defun translate-field--prepare ()
  "Set up the current buffer as the field, typing in the source language."
  (translate-field-mode)
  (setq-local mode-line-format nil
              completion-at-point-functions
              (list #'translate-field-history-capf #'cape-dabbrev #'cape-dict)
              cape-dabbrev-buffer-function #'cape-text-buffers)
  (when-let* ((method (translate--language-input-method
                       google-translate-default-source-language)))
    (activate-input-method method))
  (setq translate-field--method (input-method-in-effect))
  (translate-field--show-direction)
  (add-hook! 'post-command-hook :local
             #'translate-field-follow-h #'translate-field-fit-h))

(defun translate-field--open (origin buffer)
  "Show BUFFER in a one-line window between ORIGIN and the menu below it."
  (let* ((window-min-height 1)
         (win (split-window origin -2 'below)))
    (set-window-buffer win buffer)
    (set-window-dedicated-p win t)
    (when (window-live-p transient--window)
      (window-preserve-size transient--window nil t))
    (fit-window-to-buffer win 6 1)
    win))

(defun translate-field-edit ()
  "Edit the text to translate in a small buffer above the menu.
Return the text, or nil when the field is cancelled or left empty.
Transient suspends the menu's keys during the recursive edit."
  (let* ((translate-field--origin (selected-window))
         (translate-field--depth (1+ (recursion-depth)))
         (translate-field--buffer (generate-new-buffer "*translate*"))
         (translate-field--text nil)
         win)
    (unwind-protect
        (progn
          (with-current-buffer translate-field--buffer
            (translate-field--prepare))
          (setq win (translate-field--open translate-field--origin
                                           translate-field--buffer))
          (select-window win)
          (when (fboundp 'evil-insert-state)
            (evil-insert-state))
          (add-hook! 'window-selection-change-functions #'translate-field-leave-h)
          (recursive-edit))
      (remove-hook! 'window-selection-change-functions #'translate-field-leave-h)
      (when (window-live-p win)
        (delete-window win))
      (kill-buffer translate-field--buffer)
      (when (window-live-p translate-field--origin)
        (select-window translate-field--origin)))
    (and translate-field--text
         (not (string-empty-p translate-field--text))
         translate-field--text)))

(transient-define-suffix translate--field ()
  "Type the text to translate in a small buffer above the menu."
  :description (lambda ()
                 (if translate--field-text
                     (format "Text: %s" translate--field-text)
                   "Type some text"))
  :key "i"
  :transient t
  (interactive)
  (when-let* ((text (translate-field-edit)))
    (setq translate--field-text text)
    (add-to-history 'translate-history text)
    (setq unread-command-events
          (append unread-command-events (listify-key-sequence (kbd "RET"))))))

(defun translate--translate ()
  "Translate the typed text, else the region, else the word at point."
  (interactive)
  (let ((content (cond (translate--field-text
                        translate--field-text)
                       ((use-region-p)
                        (buffer-substring
                         (region-beginning)
                         (region-end)))
                       (t (word-at-point)))))
    (with-temp-buffer
      (insert content)
      (setq translate--last-context
            (buffer-substring-no-properties (point-min) (point-max)))
      (google-translate-buffer))))

;;;###autoload
(transient-define-prefix translate-transient ()
  "Translate"
  [["Langs"
    (translate--set-source)
    (translate--set-target)]
   [""
    ("r" "reverse" (lambda ()
                     (interactive)
                     (translate--select-source
                      google-translate-default-target-language
                      google-translate-default-source-language)
                     (transient-reset))
     :transient t)
    (translate--field)]
   [""
    ("p" (lambda ()
           (concat "translate popup: "
                   (if (buffer-local-value 'google-translate-posframe-mode (current-buffer))
                       "on" "off")))
     google-translate-posframe-mode)
    ("RET" "Go!" translate--translate)]]
  [:hide always
   [("<return>" "Go!" translate--translate)]]
  (interactive)
  (setq translate--field-text nil)
  (transient-setup 'translate-transient))

;;;###autoload
(defun google-translate-buffer-listen-source ()
  "Listen to the source text in the translation buffer."
  (interactive)
  (push-button (next-button (point-min))))

;;;###autoload
(defun google-translate-buffer-listen-translation ()
  "Listen to the translation text in the translation buffer."
  (interactive)
  (goto-char (point-min))
  (push-button (forward-button 2)))

;;;###autoload
(defun translate-at-point-smart ()
  "Translate the region or the word/sentence at point via the posframe UI.
Un-parked from doom.d with the pdf module (nov's localleader T), its only
consumer.  The posframe file is deferred here (doom.d loaded google-translate
eagerly), so pull it in before reaching for its text-grab helper."
  (interactive)
  (require 'google-translate-posframe)
  (when-let* ((text (google-translate-posframe--get-text-to-translate)))
    (google-translate-translate
     google-translate-default-source-language
     google-translate-default-target-language
     text)))