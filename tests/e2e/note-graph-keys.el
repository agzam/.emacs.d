;;; tests/e2e/note-graph-keys.el --- the keys that open the note graph -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.  The batch
;; suite reads the map! forms; only a booted config shows a keypress on the
;; leader, or the localleader of an org buffer, reaching the command.  Nothing
;; presses the keys: the command starts a server and opens a browser.

(defun note-graph-e2e--case (label got want)
  "A harness result for LABEL comparing GOT with WANT."
  (list :label label :ok (equal got want)
        :got (format "%S" got) :want (format "%S" want)))

(defun note-graph-e2e ()
  "The leader and the org localleader both reach `vulpea-graph'."
  (let ((buf (find-file-noselect (expand-file-name "note-graph.org" e2e-work-dir))))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (delete-other-windows)
          (evil-normal-state)
          (list
           (note-graph-e2e--case "the fixture is an org buffer in normal state"
                                 (list major-mode evil-state) '(org-mode normal))
           (note-graph-e2e--case "SPC o r W reaches vulpea-graph"
                                 (key-binding (kbd "SPC o r W")) 'vulpea-graph)
           (note-graph-e2e--case "localleader r W reaches vulpea-graph"
                                 (key-binding (kbd (concat doom-localleader-key " r W")))
                                 'vulpea-graph)
           (note-graph-e2e--case "vulpea-graph is a command that loads on first use"
                                 (list (commandp 'vulpea-graph) (featurep 'vulpea-graph))
                                 '(t nil))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(add-to-list 'e2e-scenarios #'note-graph-e2e)
