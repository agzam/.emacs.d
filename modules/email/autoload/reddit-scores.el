;;; modules/email/autoload/reddit-scores.el -*- lexical-binding: t; -*-
;;; Commentary:
;; The score of each Reddit post and comment, on its line in the thread view,
;; and the votes that change it.  scripts/reddit-fetch.el saves every score
;; it reads, and the login's vote, in a folder's .nnmaildir/reddit-scores; a
;; vote posts through reddit-api.el, and its line shows it at once.
;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'subr-x)

(defvar reddit-maildir)
;; the thread view's, whose lines show the scores
(defvar mail-thread-buffer-name)
(defvar mail-thread-group)

(defconst reddit-scores-file "reddit-scores"
  "File in a folder's .nnmaildir/ the Reddit fetch saves the scores in.")

(defvar reddit-scores-cache nil
  "(GROUP MTIME . TABLE) of each Reddit group whose scores were read.")

(defvar reddit-votes (make-hash-table :test #'equal)
  "The login's votes the scores file may lack, by Message-ID, as (DIR ROW).
ROW is the message's row in the file at the vote.  A row that differs
came with a fetch since, and the vote goes.")

(defface reddit-score '((t :inherit shadow :height 0.8))
  "Score of a Reddit message on its line in the thread view."
  :group 'gnus-summary)

(defface reddit-upvoted '((t :inherit reddit-score :foreground "#d19a66"))
  "Score of a Reddit message the user voted up."
  :group 'gnus-summary)

(defface reddit-downvoted '((t :inherit reddit-score :foreground "#7193ff"))
  "Score of a Reddit message the user voted down."
  :group 'gnus-summary)

(defface reddit-negative-score '((t :inherit reddit-score :foreground "#bf616a"))
  "Score below zero of a Reddit message the user did not vote on."
  :group 'gnus-summary)

(defun reddit-scores (group)
  "Scores of Reddit GROUP, a table of Message-ID to (SCORE LIKES), or nil.
The fetch saves the file anew each run, and only a new one is read."
  (let* ((file (expand-file-name (concat (gnus-group-real-name group) "/.nnmaildir/"
                                         reddit-scores-file)
                                 reddit-maildir))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (cached (assoc group reddit-scores-cache)))
    (cond ((not mtime) nil)
          ((and cached (time-equal-p mtime (cadr cached))) (cddr cached))
          (t (let ((table (make-hash-table :test #'equal)))
               (with-temp-buffer
                 (let ((coding-system-for-read 'utf-8))
                   (insert-file-contents file))
                 (pcase-dolist (`(,id . ,score) (read (current-buffer)))
                   (puthash id score table)))
               (setf (alist-get group reddit-scores-cache nil nil #'equal) (cons mtime table))
               table)))))

(defun reddit-score-row (group id)
  "(SCORE LIKES) of the message with Message-ID ID in Reddit GROUP, or nil.
A vote the scores file lacks counts in."
  (let ((row (when-let* ((scores (reddit-scores group)))
               (gethash id scores))))
    (pcase (gethash id reddit-votes)
      ('nil row)
      (`(,dir ,before)
       (if (not (equal row before))
           (progn (remhash id reddit-votes) row)
         (pcase-let ((`(,score ,likes) before))
           (list (and score (+ score dir (- (or likes 0))))
                 (unless (zerop dir) dir))))))))

;;;###autoload
(defun reddit-score-line (header)
  "The score on the thread view's line of HEADER's Reddit message, or nil.
The arrow points down below zero, and the color shows the login's vote.
While Reddit hides the score, only a vote shows, as its arrow alone."
  (when-let* (((reddit-group-p mail-thread-group))
              (row (reddit-score-row mail-thread-group (mail-header-id header))))
    (pcase-let ((`(,score ,likes) row))
      (when (or score likes)
        (propertize (concat (if (if score (< score 0) (eql likes -1)) "↓" "↑")
                            (and score (number-to-string (abs score))))
                    'face (pcase likes
                            (1 'reddit-upvoted)
                            (-1 'reddit-downvoted)
                            (_ (if (< score 0) 'reddit-negative-score 'reddit-score))))))))

;;;###autoload
(defun redraw-reddit-scores ()
  "Draw the scores in the thread view again, while it shows a Reddit thread."
  (when-let* ((view (get-buffer mail-thread-buffer-name))
              ((reddit-group-p (buffer-local-value 'mail-thread-group view))))
    (with-current-buffer view
      (mail-thread-draw-lines))))

;;; Votes

(defun reddit-vote-target ()
  "(GROUP . HEADER) of the Reddit message on screen, or nil."
  (pcase-let ((`(,summary . ,article) (mail-on-screen)))
    (when (and summary article (buffer-live-p (get-buffer summary)))
      (with-current-buffer summary
        (when-let* (((reddit-group-p gnus-newsgroup-name))
                    (header (gnus-summary-article-header article)))
          (cons gnus-newsgroup-name header))))))

(defun reddit-vote (wanted)
  "Vote the Reddit message on screen WANTED, 1 up or -1 down.
The same vote again takes it back, as Reddit's arrows do.  The line shows
the vote at once, and the vote before it again if Reddit refuses."
  (pcase-let* ((`(,group . ,header) (or (reddit-vote-target)
                                        (user-error "No Reddit message here")))
               (id (mail-header-id header))
               (dir (if (eql (cadr (reddit-score-row group id)) wanted) 0 wanted))
               (before (gethash id reddit-votes))
               (vote (list dir (when-let* ((scores (reddit-scores group)))
                                 (gethash id scores)))))
    (puthash id vote reddit-votes)
    (redraw-reddit-scores)
    (reddit-api-post "/api/vote" `(("id" . ,(reddit-thing-id id))
                                   ("dir" . ,(number-to-string dir)))
                     (lambda (_answer error)
                       (if (not error)
                           (message "%s" (pcase dir
                                           (1 "Upvoted")
                                           (-1 "Downvoted")
                                           (_ "Vote taken back")))
                         ;; a vote made since stands
                         (when (eq (gethash id reddit-votes) vote)
                           (if before
                               (puthash id before reddit-votes)
                             (remhash id reddit-votes))
                           (redraw-reddit-scores))
                         (message "Reddit refused the vote: %s" error))))))

;;;###autoload
(defun reddit-upvote ()
  "Vote the Reddit message on screen up, or take the upvote back."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (reddit-vote 1))

;;;###autoload
(defun reddit-downvote ()
  "Vote the Reddit message on screen down, or take the downvote back."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (reddit-vote -1))

;;; reddit-scores.el ends here
