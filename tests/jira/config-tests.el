;;; tests/jira/config-tests.el --- modules/jira/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; go-jira and embark are absent from the batch tier, so the config's forms
;; are read and walked as data.
(defvar jira-config-tests--forms
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/jira/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (push form forms))
      (nreverse forms)))
  "Every top-level form of the jira module config.")

(describe "embark keys on a jira ticket"
  :var* ((form (seq-find (lambda (f)
                           (and (eq (car-safe f) 'use-package)
                                (eq (cadr f) 'go-jira)))
                         jira-config-tests--forms))
         (jira (map-form-prefix-pairs form 'go-jira-embark-jira-ticket-map "j"))
         (browse (map-form-prefix-keys form 'go-jira-embark-jira-ticket-map "b")))

  (it "opens the ticket's spec on j o and its summary on j t"
    (expect (cdr (assoc "o" jira)) :to-equal '#'ticket-spec-open)
    (expect (cdr (assoc "t" jira)) :to-equal '#'ticket-tldr))

  (it "keeps the other jira actions on their keys"
    (expect (cdr (assoc "s" jira)) :to-equal '#'go-jira-change-status)
    (expect (cdr (assoc "j" jira)) :to-equal '#'go-jira-popup-show))

  (it "leaves b to the ticket view and the browser"
    (expect (car browse) :to-be nil)
    (expect (cdr browse) :to-have-same-items-as '("b" "o"))))

(provide 'jira-config-tests)
;;; tests/jira/config-tests.el ends here
