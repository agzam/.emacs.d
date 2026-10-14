;;; tests/e2e/journal-keys.el --- the keys that open the journals -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.  The batch
;; suite reads the map! forms; only a booted config shows the keys reaching
;; the commands.  Those are stubbed: the real ones prompt for a date and
;; visit, or create, notes in the real journal.

(defun journal-keys-e2e--call (keys)
  "The journal call made by the command that KEYS reach."
  (let (call)
    (cl-letf (((symbol-function 'open-journal)
               (lambda (&rest args) (setq call (cons 'open-journal args))))
              ((symbol-function 'open-journal-date)
               (lambda (&rest args) (setq call (cons 'open-journal-date args)))))
      (when-let* ((cmd (key-binding (kbd keys))))
        (funcall cmd)))
    call))

(defun journal-keys-e2e--case (label got want)
  "A harness result for LABEL comparing GOT with WANT."
  (list :label label :ok (equal got want)
        :got (format "%S" got) :want (format "%S" want)))

(defun journal-keys-e2e ()
  "The roam keys open today's work journal and ask for the other dates."
  (let ((buf (find-file-noselect (expand-file-name "journal-keys.org" e2e-work-dir)))
        (local (concat doom-localleader-key " r ")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (evil-normal-state)
          (list
           (journal-keys-e2e--case "the fixture is an org buffer in normal state"
                                   (list major-mode evil-state) '(org-mode normal))
           (journal-keys-e2e--case "open-journal-date is autoloaded"
                                   (fboundp 'open-journal-date) t)
           (journal-keys-e2e--case "SPC o r t opens today's work journal"
                                   (journal-keys-e2e--call "SPC o r t")
                                   '(open-journal work))
           (journal-keys-e2e--case "SPC o r T asks for the personal journal date"
                                   (journal-keys-e2e--call "SPC o r T")
                                   '(open-journal-date personal))
           (journal-keys-e2e--case "SPC o r n asks for the work journal date"
                                   (journal-keys-e2e--call "SPC o r n")
                                   '(open-journal-date work))
           (journal-keys-e2e--case "SPC o r N asks for the personal journal date"
                                   (journal-keys-e2e--call "SPC o r N")
                                   '(open-journal-date personal))
           (journal-keys-e2e--case "localleader r n asks for the work journal date"
                                   (journal-keys-e2e--call (concat local "n"))
                                   '(open-journal-date work))
           (journal-keys-e2e--case "localleader r N asks for the personal journal date"
                                   (journal-keys-e2e--call (concat local "N"))
                                   '(open-journal-date personal))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(add-to-list 'e2e-scenarios #'journal-keys-e2e)
