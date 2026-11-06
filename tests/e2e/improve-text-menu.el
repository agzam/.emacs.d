;;; tests/e2e/improve-text-menu.el --- improve-text menu keys and summary -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; Return arrives as <return>, and evil's org maps bind that, so a RET-only
;; suffix is dead in an org buffer.  The summary's q sits in an evil state
;; map that general binds once autoload/gptel.el loads.  Only real keys
;; over a real org buffer show either.

(require 'cl-lib)

(defun improve-text-menu-e2e ()
  "Open the improve-text menu off the leader in an org file and drive it."
  (require 'gptel)
  ;; load before stubbing: an autoload firing under a stub redefines the
  ;; stubbed function, and the stub's exit restores the autoload
  (unless (boundp 'gptel-summary-prompt)
    (load (expand-file-name "modules/ai/autoload/gptel.el" e2e-root)
          nil 'nomessage))
  (let* ((file (expand-file-name "improve-text-menu.org" e2e-work-dir))
         (buf (find-file-noselect file))
         (body (concat "* Parent\nparent text\n"
                       "** Child :tag:\nchild text\n"
                       "* Sibling\nsibling text\n"))
         (answer "**Gist:** a tiny summary.")
         (want-text "#+title: Child\n\n*Gist:* a tiny summary.")
         (results '())
         drawn sent summary)
    (cl-flet ((record (label ok got want)
                (push (list :label (format "improve-text menu: %s" label)
                            :ok ok :got got :want want)
                      results)
                ok)
              (reset ()
                (switch-to-buffer buf)
                (delete-other-windows)
                (with-current-buffer buf
                  (evil-force-normal-state)
                  (goto-char (point-min))
                  (search-forward "child text")
                  (beginning-of-line))
                (discard-input))
              (open-menu ()
                (execute-kbd-macro (kbd "SPC g e"))
                (and (bound-and-true-p transient--prefix)
                     (oref transient--prefix command)))
              (summarize-stub (prompt &rest args)
                ;; answers at once, the way a finished request calls back
                (setq sent prompt)
                (funcall (plist-get args :callback) answer
                         (list :buffer (current-buffer)))))
      (unwind-protect
          (catch 'stop
            (with-current-buffer buf
              (erase-buffer)
              (insert body)
              (font-lock-ensure))
            (reset)
            (let ((cmd (with-current-buffer buf (key-binding [return]))))
              (unless (record "<return> is taken by the org buffer's own maps"
                              (and cmd (not (eq cmd 'gptel-improve-text)))
                              (format "%S" cmd)
                              "a non-nil binding other than gptel-improve-text")
                (throw 'stop nil)))

            (dolist (key (list [return] (kbd "RET")))
              (reset)
              (let (ran opened)
                (cl-letf (((symbol-function 'gptel-improve-text)
                           (lambda (&optional _) (interactive) (setq ran t))))
                  (setq opened (open-menu))
                  (unless drawn
                    (setq drawn (when-let* ((b (get-buffer transient--buffer-name)))
                                  (with-current-buffer b
                                    (substring-no-properties (buffer-string))))))
                  (when (eq opened 'gptel-improve-text-transient)
                    (execute-kbd-macro key)))
                (unless (record (format "%s runs gptel-improve-text"
                                        (key-description key))
                                (and (eq opened 'gptel-improve-text-transient) ran)
                                (format "opened=%s ran=%s" opened ran)
                                "opened=gptel-improve-text-transient ran=t")
                  (throw 'stop nil))))

            (record "the menu draws RET and e, and hides <return>"
                    (and drawn
                         (string-match-p "RET +Let's go" drawn)
                         (string-match-p "e +ELI5 summary" drawn)
                         (not (string-search "<return>" drawn)))
                    drawn
                    "RET Let's go, e ELI5 summary, no <return>")

            ;; e on the subtree: the real converter turns the answer to Org
            (reset)
            (cl-letf (((symbol-function 'gptel-request) #'summarize-stub))
              (when (eq (open-menu) 'gptel-improve-text-transient)
                (execute-kbd-macro (kbd "e"))))
            (setq summary (window-buffer (selected-window)))
            (let ((got (list sent
                             (buffer-local-value 'major-mode summary)
                             (with-current-buffer summary
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
                  (want (list "** Child :tag:\nchild text\n"
                              'gptel-summary-mode want-text)))
              (unless (record "e sends the subtree and shows the Org answer"
                              (equal got want)
                              (format "%S" got) (format "%S" want))
                (throw 'stop nil)))

            (with-current-buffer summary (evil-force-normal-state))
            (execute-kbd-macro (kbd "q"))
            (unless (record "q in the summary hands the window back"
                            (eq (window-buffer (selected-window)) buf)
                            (buffer-name (window-buffer (selected-window)))
                            (buffer-name buf))
              (throw 'stop nil))

            ;; a visual-line selection wins over the subtree around it.  V
            ;; opens expreg-transient, which replays the SPC it lets through,
            ;; except inside a keyboard macro, which general assumes recorded it
            (reset)
            (setq sent nil)
            (let ((msgs (with-current-buffer (messages-buffer) (point-max)))
                  (general--simulate-as-is t)
                  after-v opened after-menu)
              (cl-letf (((symbol-function 'gptel-request) #'summarize-stub))
                (execute-kbd-macro (kbd "V"))
                (setq after-v (with-current-buffer buf evil-state))
                (setq opened (open-menu))
                (setq after-menu (with-current-buffer buf evil-state))
                (when (eq opened 'gptel-improve-text-transient)
                  (execute-kbd-macro (kbd "e"))))
              (let ((got (list sent (with-current-buffer buf (evil-visual-state-p))))
                    (want (list "child text\n" nil)))
                (record "e on a visual-line selection sends that line and drops it"
                        (equal got want)
                        (format "%S after-v=%s opened=%s after-menu=%s text=%S messages=%S"
                                got after-v opened after-menu
                                (with-current-buffer buf
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))
                                (with-current-buffer (messages-buffer)
                                  (buffer-substring-no-properties
                                   (min msgs (point-max)) (point-max))))
                        (format "%S" want))))

            ;; m keeps its pick to this menu: gptel's model stays put, and
            ;; the menu's next request goes out with the pick
            (reset)
            (let* ((chat (list (default-value 'gptel-backend)
                               (default-value 'gptel-model)))
                   (menu (list gptel-improve-text-backend gptel-improve-text-model))
                   ;; any model gptel knows besides the chat's, so a pick shows
                   (pick (cl-loop for (name . backend) in gptel--known-backends
                                  thereis (cl-loop for model in (gptel-backend-models backend)
                                                   unless (eq model gptel-model)
                                                   return (list (concat name ":" (gptel--model-name model))
                                                                backend model))))
                   seen redrawn)
              (unwind-protect
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (&rest _) (car pick)))
                            ((symbol-function 'gptel-request)
                             (lambda (prompt &rest args)
                               (setq seen (list gptel-backend gptel-model))
                               (apply #'summarize-stub prompt args))))
                    (when (eq (open-menu) 'gptel-improve-text-transient)
                      (execute-kbd-macro (kbd "m"))
                      (setq redrawn (when-let* ((b (get-buffer transient--buffer-name)))
                                      (with-current-buffer b
                                        (substring-no-properties (buffer-string)))))
                      (execute-kbd-macro (kbd "e")))
                    ;; read gptel's own values: the summary buffer, current
                    ;; now, holds the pick locally on purpose
                    (let ((got (list (list gptel-improve-text-backend gptel-improve-text-model)
                                     (equal (list (default-value 'gptel-backend)
                                                  (default-value 'gptel-model))
                                            chat)
                                     (local-variable-p 'gptel-model buf)
                                     seen
                                     (and redrawn (string-search (car pick) redrawn) t)))
                          (want (list (cdr pick) t nil (cdr pick) t)))
                      (record "m picks for this menu only, and its next request uses the pick"
                              (equal got want)
                              (format "%S" got) (format "%S" want))))
                (set-default 'gptel-improve-text-backend (car menu))
                (set-default 'gptel-improve-text-model (cadr menu)))))
        (discard-input)
        ;; a failed step may strand the transient; never leak it into the
        ;; scenarios that run after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (dolist (b (buffer-list))
          (when (string-prefix-p "*summary:" (buffer-name b))
            (kill-buffer b)))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-other-windows)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'improve-text-menu-e2e)
