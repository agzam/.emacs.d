;;; modules/multiple-cursors/config.el -*- lexical-binding: t; -*-

;; evil-define-command can't ride the generated loaddefs (make-autoload
;; doesn't know the macro), so these are eager - evil loads before any module.
(evil-define-command mc-toggle-cursor-here ()
  "Create a cursor at point.
In visual block/line, put a cursor on each selected line at the cursor's
column; otherwise pause cursors."
  :repeat nil :keep-visual nil :evil-mc t
  (interactive)
  (cond ((and (evil-mc-has-cursors-p)
              (evil-normal-state-p)
              (let* ((pos (point))
                     (cursor (cl-find-if (lambda (c)
                                           (eq pos (evil-mc-get-cursor-start c)))
                                         evil-mc-cursor-list)))
                (when cursor
                  (evil-mc-delete-cursor cursor)
                  (setq evil-mc-cursor-list (delq cursor evil-mc-cursor-list))
                  t))))
        ((memq evil-this-type '(block line))
         (let ((col (evil-column))
               (line-at-pt (line-number-at-pos)))
           (when (= evil-visual-direction 1)
             (cl-decf col)
             (backward-char))
           (save-excursion
             (evil-apply-on-block
              (lambda (ibeg _)
                (unless (or (= line-at-pt (line-number-at-pos ibeg))
                            (invisible-p ibeg))
                  (goto-char ibeg)
                  (move-to-column col)
                  (when (= (current-column) col)
                    (evil-mc-make-cursor-here))))
              evil-visual-beginning
              (if (eq evil-this-type 'line) (1- evil-visual-end) evil-visual-end)
              nil)
             (evil-exit-visual-state))))
        (t
         (evil-mc-pause-cursors)
         (evil-mc-make-cursor-here))))

(evil-define-command mc-undo-cursor ()
  "Undo the last cursor, or every cursor within the visual region."
  :repeat nil :evil-mc t
  (interactive)
  (if (evil-visual-state-p)
      (or (mapc (lambda (c)
                  (evil-mc-delete-cursor c)
                  (setq evil-mc-cursor-list (delq c evil-mc-cursor-list)))
                (cl-remove-if-not
                 (lambda (pos)
                   (and (>= pos evil-visual-beginning)
                        (< pos evil-visual-end)))
                 evil-mc-cursor-list
                 :key #'evil-mc-get-cursor-start))
          (message "No cursors to undo in region"))
    (evil-mc-undo-last-added-cursor)))

(use-package evil-multiedit
  :defer t)

(use-package iedit
  :defer t
  :init
  ;; free C-; for embark-act (evil-multiedit pulls iedit in)
  (setq iedit-toggle-key-default nil))

(use-package evil-mc
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :init
  ;; evil-mc's own bindings are too aggressive; we set our own map instead.
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  ;; evil-mc lazy-loads its vars/hooks and keeps its minor mode perpetually
  ;; active (upstream #6021); Doom's HACK to undo all of that.
  (evil-mc-define-vars)
  (evil-mc-initialize-vars)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-incompatible-modes)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-initialize-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-teardown-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-resume-incompatible-modes)
  (advice-add #'evil-mc-initialize-hooks :override #'ignore)
  (advice-add #'evil-mc-teardown-hooks :override #'evil-mc-initialize-vars)
  (advice-add #'evil-mc-initialize-active-state :before #'turn-on-evil-mc-mode)
  (advice-add #'evil-mc-teardown-active-state :after #'turn-off-evil-mc-mode)
  (defadvice! mc-dont-reinit-vars-a (fn &rest args)
    :around #'evil-mc-mode
    (letf! ((#'evil-mc-initialize-vars #'ignore))
      (apply fn args)))

  (setq evil-mc-enable-bar-cursor (eq system-type 'gnu/linux))

  (after! smartparens
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; how evil-mc replays each command across cursors; trimmed to commands the
  ;; lab has - undo-fu/evil-numbers/ess/company/eval entries return with them.
  (dolist (fn '((backward-kill-word)
                (backward-to-bol-or-indent . evil-mc-execute-default-call)
                (forward-to-last-non-comment-or-eol . evil-mc-execute-default-call)
                (evil-delete-back-to-indentation . evil-mc-execute-default-call)
                (evil-escape . evil-mc-execute-default-evil-normal-state)
                (evil-digit-argument-or-evil-beginning-of-visual-line
                 (:default . evil-mc-execute-default-call)
                 (visual . evil-mc-execute-visual-call))
                (evil-org-delete . evil-mc-execute-default-evil-delete)))
    (setf (alist-get (car fn) evil-mc-custom-known-commands)
          (if (and (cdr fn) (listp (cdr fn)))
              (cdr fn)
            (list (cons :default
                        (or (cdr fn)
                            #'evil-mc-execute-default-call-with-count))))))

  (defadvice! mc-make-repeatable-a (fn)
    :around '(evil-mc-make-and-goto-first-cursor
              evil-mc-make-and-goto-last-cursor
              evil-mc-make-and-goto-prev-cursor
              evil-mc-make-and-goto-next-cursor
              evil-mc-skip-and-goto-prev-cursor
              evil-mc-skip-and-goto-next-cursor
              evil-mc-make-and-goto-prev-match
              evil-mc-make-and-goto-next-match
              evil-mc-skip-and-goto-prev-match
              evil-mc-skip-and-goto-next-match)
    (dotimes (_ (if (integerp current-prefix-arg) current-prefix-arg 1))
      (funcall fn)))

  ;; entering insert usually means we want the cursors working
  (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors)
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-escape-mode)

  (add-hook! 'doom-escape-hook
    (defun mc-clear-cursors-h ()
      "Clear evil-mc cursors and restore state on ESC."
      (when (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
        (evil-mc-resume-cursors)
        t)))

  (map! :map evil-mc-key-map
        :nv "g." nil
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-S-n" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-S-p" #'evil-mc-make-and-goto-first-cursor))
