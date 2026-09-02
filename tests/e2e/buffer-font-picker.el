;;; tests/e2e/buffer-font-picker.el --- set-buffer-font over a real consult read -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The batch suite hands `set-buffer-font' a stubbed `consult--read' and
;; calls its state function by hand, so it says nothing about the read
;; itself: whether the group function reaches vertico's headers in the
;; order the candidates were handed over, whether consult previews on any
;; key, whether abandoning the read runs the reset that puts the buffer's
;; font back.  The font backend stays stubbed even here - a terminal
;; Emacs opens no fonts at all, so `font-family-list' is empty.

(require 'cl-lib)

(defvar buffer-font-e2e-families
  '(("Alpha Sans" . 0) ("Zeta Sans" . 0) ("Mono Uno" . 100) ("Mono Duo" . 100))
  "Families the stubbed backend serves, as (FAMILY . SPACING).")

(defmacro buffer-font-e2e--with-fonts (&rest body)
  "Run BODY with the font backend answering `buffer-font-e2e-families'."
  (declare (indent 0))
  `(let ((real-font-get (symbol-function 'font-get)))
     (clrhash font-family-spacing-cache)
     (cl-letf (((symbol-function 'font-family-list)
                (lambda () (mapcar #'car buffer-font-e2e-families)))
               ((symbol-function 'find-font)
                (lambda (spec)
                  (car (assoc (symbol-name (funcall real-font-get spec :family))
                              buffer-font-e2e-families))))
               ((symbol-function 'font-get)
                (lambda (entity prop)
                  (if (stringp entity)
                      (alist-get entity buffer-font-e2e-families nil nil #'equal)
                    (funcall real-font-get entity prop)))))
       ,@body)))

(defun buffer-font-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "buffer font picker: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun buffer-font-e2e--drive (keys &optional setup)
  "Font the buffer ends up with after KEYS go into `set-buffer-font'.
SETUP runs in the buffer first.  A quit is what C-g leaves behind, so it
counts as an answer rather than an error.  Only a buffer-local face is
reported: the global `buffer-face-mode-face' default is `variable-pitch'
and says nothing about this buffer."
  (with-temp-buffer
    (when setup (funcall setup))
    (let ((guard (run-with-timer 10 nil
                                 (lambda ()
                                   (when (active-minibuffer-window)
                                     (abort-recursive-edit)))))
          signal)
      (unwind-protect
          (condition-case err
              (buffer-font-e2e--with-fonts
                (let ((unread-command-events (listify-key-sequence (kbd keys))))
                  (set-buffer-font (current-buffer))))
            (quit nil)
            (error (setq signal (list 'signalled err))))
        (cancel-timer guard))
      (or signal
          (list buffer-face-mode
                (and (local-variable-p 'buffer-face-mode-face)
                     buffer-face-mode-face))))))

(defun buffer-font-e2e--typed (input)
  "A DONE-P for `buffer-font-e2e--snapshot': the minibuffer reads INPUT."
  (lambda () (equal (minibuffer-contents-no-properties) input)))

(defun buffer-font-e2e--narrowed-typed (key input)
  "A DONE-P for `buffer-font-e2e--snapshot': narrowed by KEY, reading INPUT.
Input on top of the narrowing key on purpose: with an empty prompt the
read sits on the default rather than on a candidate, and previews that."
  (lambda () (and (eq consult--narrow key)
                  (equal (minibuffer-contents-no-properties) input))))

(defun buffer-font-e2e--snapshot (keys done-p)
  "What the live read shows once KEYS are in and DONE-P holds.
Captured from `post-command-hook', which runs after both vertico's own
arranging and consult's preview hook, so the headers, the candidates and
the preview all belong to the same command."
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (seen 'never-captured)
           (capture (lambda ()
                      (when (and (minibufferp) (funcall done-p))
                        (setq seen
                              (list :groups (mapcar #'substring-no-properties
                                                    vertico--groups)
                                    :candidates (mapcar #'substring-no-properties
                                                        vertico--candidates)
                                    :previewed (buffer-local-value
                                                'buffer-face-mode-face buffer)))
                        (abort-minibuffers))))
           (guard (run-with-timer 10 nil
                                  (lambda ()
                                    (when (active-minibuffer-window)
                                      (abort-recursive-edit))))))
      (add-hook 'post-command-hook capture)
      (unwind-protect
          (condition-case err
              (buffer-font-e2e--with-fonts
                (let ((unread-command-events (listify-key-sequence (kbd keys))))
                  (set-buffer-font buffer))
                seen)
            (quit seen)
            (error (list 'signalled err)))
        (remove-hook 'post-command-hook capture)
        (cancel-timer guard)))))

(defun buffer-font-e2e ()
  "Check `set-buffer-font' against a real consult read."
  ;; the command is autoloaded, and the stubs below touch its cache
  (let ((fn (symbol-function 'set-buffer-font)))
    (when (autoloadp fn)
      (autoload-do-load fn 'set-buffer-font)))
  (list
   (buffer-font-e2e--result
    "one header per class, proportional families first, previewing on any key"
    (buffer-font-e2e--snapshot "n" (buffer-font-e2e--typed "n"))
    '(:groups ("Proportional" "Monospaced")
      :candidates ("Alpha Sans" "Zeta Sans" "Mono Duo" "Mono Uno")
      :previewed (:family "Alpha Sans")))
   (buffer-font-e2e--result
    "< m leaves the monospaced families alone"
    (buffer-font-e2e--snapshot "< m n" (buffer-font-e2e--narrowed-typed ?m "n"))
    '(:groups ("Monospaced")
      :candidates ("Mono Duo" "Mono Uno")
      :previewed (:family "Mono Duo")))
   (buffer-font-e2e--result
    "< p leaves the proportional ones"
    (buffer-font-e2e--snapshot "< p n" (buffer-font-e2e--narrowed-typed ?p "n"))
    '(:groups ("Proportional")
      :candidates ("Alpha Sans" "Zeta Sans")
      :previewed (:family "Alpha Sans")))
   (buffer-font-e2e--result
    "RET sets the buffer's font to the family it lands on"
    (buffer-font-e2e--drive "U n o RET")
    '(t (:family "Mono Uno")))
   (buffer-font-e2e--result
    "C-g leaves a buffer that had no font of its own without one"
    (buffer-font-e2e--drive "U n o C-g")
    '(nil nil))
   (buffer-font-e2e--result
    "C-g puts back the font the buffer already had"
    (buffer-font-e2e--drive "U n o C-g"
                            (lambda () (buffer-face-set 'variable-pitch)))
    '(t variable-pitch))))

(add-to-list 'e2e-scenarios #'buffer-font-e2e)
