;;; modules/git/autoload/code-review.el -*- lexical-binding: t; -*-

(defvar forge-browse-topics-using-forge)

;;;###autoload
(defun code-review-browse-pr ()
  "Open the reviewed PR on GitHub."
  (interactive)
  (let ((pr (code-review-db-get-pullreq))
        ;; forge would open a PR of a repo it tracks in a forge buffer
        (forge-browse-topics-using-forge nil))
    (browse-url (format "https://github.com/%s/%s/pull/%s"
                        (oref pr owner) (oref pr repo) (oref pr number)))))
