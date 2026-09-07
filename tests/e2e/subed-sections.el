;;; tests/e2e/subed-sections.el --- subtitle motion and player-sync toggles -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The buttercup suite reads the bindings out of the config form; only a
;; booted Emacs shows the dispatch: ]] and [[ through the evil
;; normal-state binding on subed-mode-map, and the localleader keys
;; reaching subed's own sync toggles.  The motions also have to land on
;; visible text while `subed-toggle-srt-metadata' holds the timestamps
;; behind invisible overlays - the state this file exists to cover,
;; since an overlay is invisible to a `with-temp-buffer' spec.

(defvar subed-sections--srt
  (concat "1\n00:00:01,000 --> 00:00:02,000\nFirst subtitle\nwrapped onto two lines\n\n"
          "2\n00:00:03,000 --> 00:00:04,000\nSecond subtitle\n\n"
          "3\n00:00:05,000 --> 00:00:06,000\nThird subtitle\n\n")
  "Three subtitles, the first one wrapped, so a motion cannot pass as a line move.")

(defun subed-sections--case (label ok got want &optional err)
  (list :label (format "subed sections: %s" label)
        :ok (and ok (null err)) :got (format "%S" got) :want want :err err))

(defun subed-sections--here ()
  "Point's line and whether point sits on hidden text."
  (list (buffer-substring-no-properties
         (line-beginning-position) (line-end-position))
        (and (invisible-p (point)) t)))

(defun subed-sections--press (keys)
  "Press KEYS for real; return the error they signal, or nil."
  (condition-case e (progn (execute-kbd-macro (kbd keys)) nil) (error e)))

(defun subed-sections--step (label keys want)
  "Press KEYS and expect point on line WANT, visible."
  (let ((err (subed-sections--press keys)))
    (subed-sections--case label
                          (equal (subed-sections--here) (list want nil))
                          (subed-sections--here)
                          (format "(%S nil)" want)
                          err)))

(defun subed-sections--walk (label)
  "]] and [[ across the fixture, LABEL naming the metadata state."
  (goto-char (point-min))
  (subed-jump-to-subtitle-text)
  (list
   (subed-sections--step (format "%s: ]] reaches the next subtitle" label)
                         "]]" "Second subtitle")
   (subed-sections--step (format "%s: ]] again reaches the third" label)
                         "]]" "Third subtitle")
   (subed-sections--step (format "%s: ]] on the last one stays put" label)
                         "]]" "Third subtitle")
   (subed-sections--step (format "%s: [[ reaches the previous subtitle" label)
                         "[[" "Second subtitle")
   (subed-sections--step (format "%s: [[ crosses the wrapped one to its text" label)
                         "[[" "First subtitle")
   (subed-sections--step (format "%s: [[ on the first one stays put" label)
                         "[[" "First subtitle")))

(defun subed-sections--toggle-case (label keys probe)
  "Press KEYS twice; PROBE must flip and come back.
Read relative to the state found: subed turns point-to-player sync off
for a moment after each motion, so no absolute state is deterministic."
  (let* ((before (and (funcall probe) t))
         (first-err (subed-sections--press keys))
         (flipped (and (funcall probe) t))
         (second-err (subed-sections--press keys))
         (back (and (funcall probe) t)))
    (subed-sections--case label
                          (and (eq flipped (not before)) (eq back before))
                          (list before flipped back)
                          (format "(%S %S %S)" before (not before) before)
                          (or first-err second-err))))

(defun subed-sections-e2e ()
  "Subtitle motions and the localleader sync toggles over a real .srt file."
  (let* ((file (expand-file-name "sections.srt" e2e-work-dir))
         (subed-auto-play-media nil)
         (local doom-localleader-key)
         buf results)
    (with-temp-file file (insert subed-sections--srt))
    (unwind-protect
        (progn
          (setq buf (find-file-noselect file))
          (switch-to-buffer buf)
          (delete-other-windows)
          (evil-normal-state)
          (push (subed-sections--case
                 "the fixture opens in subed-srt-mode, evil normal state"
                 (and (eq major-mode 'subed-srt-mode) (evil-normal-state-p))
                 (list major-mode evil-state) "(subed-srt-mode normal)")
                results)
          (push (subed-sections--case
                 "]] and [[ dispatch subed's motions in normal state"
                 (and (eq (key-binding (kbd "]]")) 'subed-forward-subtitle-text)
                      (eq (key-binding (kbd "[[")) 'subed-backward-subtitle-text))
                 (list (key-binding (kbd "]]")) (key-binding (kbd "[[")))
                 "(subed-forward-subtitle-text subed-backward-subtitle-text)")
                results)
          ;; ahead of the motions: those leave a pending re-enable timer
          ;; behind, and its state is nobody's to predict
          (dolist (case (list
                         (list "localleader t s flips seeking the player to point"
                               (concat local " t s") #'subed-sync-player-to-point-p)
                         (list "localleader t f flips point following the player"
                               (concat local " t f") #'subed-sync-point-to-player-p)))
            (push (apply #'subed-sections--toggle-case case) results))
          (setq results (append (nreverse (subed-sections--walk "metadata shown"))
                                results))
          (let ((err (subed-sections--press (concat local " t t"))))
            (goto-char (point-min))
            (push (subed-sections--case
                   "localleader t t hides the timestamps"
                   (and subed--subtitle-metadata-hidden (invisible-p (point)))
                   (list subed--subtitle-metadata-hidden
                         (and (invisible-p (point)) t))
                   "(t t)" err)
                  results))
          (setq results (append (nreverse (subed-sections--walk "metadata hidden"))
                                results))
          (nreverse results))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file file))))

(add-to-list 'e2e-scenarios #'subed-sections-e2e)
