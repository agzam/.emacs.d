;;; tests/e2e/tutor.el --- the tutor menu over a real selection -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The package's own suite sets its menu up in batch.  Only a real boot has
;; evil's visual selection reaching the menu's suffixes through M-x and
;; expreg-transient, and q in the lesson buffer's normal state.

(require 'cl-lib)

(defvar gptel--known-backends)
(defvar tutor-backend)
(defvar tutor-model)
(defvar tutor-language)

(defun tutor-e2e ()
  "Select text with real keys, open the tutor menu and drive it."
  (require 'gptel)
  ;; load before stubbing: an autoload firing under a stub redefines the
  ;; stubbed function, and the stub's exit restores the autoload
  (require 'tutor)
  (let* ((file (expand-file-name "tutor.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (menu (list tutor-backend tutor-model tutor-language))
         (chat (list (default-value 'gptel-backend) (default-value 'gptel-model)))
         ;; expreg-transient replays the keys it lets through, except inside
         ;; a keyboard macro, which general assumes recorded them
         (general--simulate-as-is t)
         (results '())
         sent seen system)
    (cl-flet* ((record (label ok got want)
                 (push (list :label (format "tutor: %s" label)
                             :ok ok :got got :want want)
                       results)
                 ok)
               (reset ()
                 (switch-to-buffer buf)
                 (delete-other-windows)
                 (with-current-buffer buf
                   (evil-force-normal-state)
                   (goto-char (point-min)))
                 (setq sent nil seen nil system nil)
                 (discard-input))
               (press (keys)
                 ;; a read that never ends would hang the whole run
                 (let ((guard (run-with-timer
                               10 nil (lambda ()
                                        (when (active-minibuffer-window)
                                          (abort-recursive-edit))))))
                   (unwind-protect
                       (condition-case err
                           (progn (execute-kbd-macro keys) nil)
                         ((error quit) err))
                     (cancel-timer guard))))
               (open-menu ()
                 (press (kbd "M-x tutor RET"))
                 (and (bound-and-true-p transient--prefix)
                      (oref transient--prefix command)))
               (drawn ()
                 (when-let* ((b (get-buffer transient--buffer-name)))
                   (with-current-buffer b
                     (substring-no-properties (buffer-string)))))
               (lesson-stub (prompt &rest args)
                 ;; answers at once, streamed, the way gptel calls back
                 (setq sent prompt
                       seen (list gptel-backend gptel-model)
                       system (plist-get args :system))
                 (let ((info (list :buffer (current-buffer)
                                   :context (plist-get args :context)
                                   :stream t))
                       (callback (plist-get args :callback)))
                   (funcall callback "Vamos " info)
                   (funcall callback "a leer." info)
                   (funcall callback t info))))
      (unwind-protect
          (catch 'stop
            (with-current-buffer buf
              (erase-buffer)
              (insert "Las fuertes lluvias continuarán.\nOtra frase.\n")
              (font-lock-ensure))

            ;; a visual-line selection, the menu through M-x, then RET
            (reset)
            (press (kbd "V"))
            (let ((opened (open-menu))
                  (text (drawn)))
              (unless (record "M-x on a visual-line selection opens the menu"
                              (eq opened 'tutor)
                              (format "%S" opened) "tutor")
                (throw 'stop nil))
              (record "the menu draws the language, gptel's model and both actions"
                      (and text
                           (string-match-p "l +Language +\"Spanish\"" text)
                           (string-match-p "m +Model" text)
                           (string-search (concat (gptel-backend-name (car chat)) ":"
                                                  (gptel--model-name (cadr chat)))
                                          text)
                           (string-match-p "RET +Explain" text)
                           (string-match-p "t +Translate, then explain" text)
                           (not (string-search "<return>" text)))
                      text
                      "l Language, m Model BACKEND:MODEL, RET Explain, t Translate, no <return>"))
            (cl-letf (((symbol-function 'gptel-request) #'lesson-stub))
              (press (kbd "RET")))
            (let* ((shown (window-buffer (selected-window)))
                   (got (list sent
                              (buffer-name shown)
                              (buffer-local-value 'major-mode shown)
                              (with-current-buffer shown
                                (buffer-substring-no-properties (point-min) (point-max)))
                              (buffer-local-value 'header-line-format shown)
                              seen
                              (and system
                                   (string-match-p "The user message is a Spanish text"
                                                   system)
                                   t)
                              (with-current-buffer buf (evil-visual-state-p))))
                   (want (list "Las fuertes lluvias continuarán.\n"
                               (format "*Spanish lesson: %s*" (cadr chat))
                               'tutor-mode
                               "Vamos a leer."
                               (format " %s  done" (cadr chat))
                               chat
                               t
                               nil)))
              (unless (record "RET sends the selected line and streams the lesson into its buffer"
                              (equal got want)
                              (format "%S" got) (format "%S" want))
                (throw 'stop nil))
              (with-current-buffer shown (evil-force-normal-state)))

            (press (kbd "q"))
            (record "q in the lesson hands the window back"
                    (eq (window-buffer (selected-window)) buf)
                    (buffer-name (window-buffer (selected-window)))
                    (buffer-name buf))

            ;; a charwise selection goes out as evil shows it, its last
            ;; character included; the motions run through expreg-transient
            (reset)
            (press (kbd "v w w"))
            (let ((selected (with-current-buffer buf
                              (let ((range (evil-visual-range)))
                                (buffer-substring-no-properties
                                 (evil-range-beginning range)
                                 (evil-range-end range))))))
              (when (eq (open-menu) 'tutor)
                (cl-letf (((symbol-function 'gptel-request) #'lesson-stub))
                  (press [return])))
              (record "<return> on a charwise selection sends exactly what evil selected"
                      (and (equal sent selected)
                           (string-prefix-p "Las fuertes" selected)
                           (not (string-search "\n" selected)))
                      (format "%S" sent) (format "%S" selected)))

            ;; t asks for a translation ahead of the lesson
            (reset)
            (press (kbd "V"))
            (when (eq (open-menu) 'tutor)
              (cl-letf (((symbol-function 'gptel-request) #'lesson-stub))
                (press (kbd "t"))))
            (record "t sends the selection with the translation task"
                    (and (equal sent "Las fuertes lluvias continuarán.\n")
                         system
                         (string-match-p "First translate it into natural Spanish"
                                         system)
                         t)
                    (format "%S %S" sent (and system (substring system 0 (min 160 (length system)))))
                    "the selected line, and a system prompt that opens with the translation")

            ;; l changes the language of the next lesson
            (reset)
            (press (kbd "V"))
            (when (eq (open-menu) 'tutor)
              ;; stubbed only now: M-x reads its command the same way
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "French")))
                (press (kbd "l")))
              (cl-letf (((symbol-function 'gptel-request) #'lesson-stub))
                (press (kbd "RET"))))
            (record "l picks the language the next lesson is taught in"
                    (and (equal tutor-language "French")
                         (equal (buffer-name (window-buffer (selected-window)))
                                (format "*French lesson: %s*" (cadr chat)))
                         system
                         (string-match-p "\\`You are a French teacher" system)
                         t)
                    (format "%S %S" tutor-language
                            (buffer-name (window-buffer (selected-window))))
                    "French, and a lesson buffer named after it")
            (set-default 'tutor-language (nth 2 menu))

            ;; m keeps its pick to this menu: gptel's model stays put, and
            ;; the next lesson goes out with the pick
            (reset)
            (let ((pick (cl-loop for (name . backend) in gptel--known-backends
                                 thereis (cl-loop for model in (gptel-backend-models backend)
                                                  unless (eq model (cadr chat))
                                                  return (list (concat name ":" (gptel--model-name model))
                                                               backend model))))
                  redrawn)
              (press (kbd "V"))
              (when (eq (open-menu) 'tutor)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (&rest _) (car pick))))
                  (press (kbd "m")))
                (setq redrawn (drawn))
                (cl-letf (((symbol-function 'gptel-request) #'lesson-stub))
                  (press (kbd "RET"))))
              (let ((got (list (list tutor-backend tutor-model)
                               (equal (list (default-value 'gptel-backend)
                                            (default-value 'gptel-model))
                                      chat)
                               (local-variable-p 'gptel-model buf)
                               seen
                               (and redrawn (string-search (car pick) redrawn) t)
                               (buffer-name (window-buffer (selected-window)))))
                    (want (list (cdr pick) t nil (cdr pick) t
                                (format "*Spanish lesson: %s*" (nth 2 pick)))))
                (record "m picks for this menu only, and the next lesson goes out with the pick"
                        (equal got want)
                        (format "%S" got) (format "%S" want)))))
        (discard-input)
        (set-default 'tutor-backend (car menu))
        (set-default 'tutor-model (cadr menu))
        (set-default 'tutor-language (nth 2 menu))
        ;; a failed step may strand the transient; never leak it into the
        ;; scenarios that run after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (dolist (b (buffer-list))
          (when (eq (buffer-local-value 'major-mode b) 'tutor-mode)
            (kill-buffer b)))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-other-windows)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'tutor-e2e)
