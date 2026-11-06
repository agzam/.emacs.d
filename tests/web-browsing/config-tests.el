;;; tests/web-browsing/config-tests.el --- modules/web-browsing/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun web-browsing-config-tests--top-level-forms ()
  "Read every top-level form out of the web-browsing module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/web-browsing/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(describe "shr link keys"
  ;; a link's shr-map outranks evil's maps, and Gnus renders HTML with shr
  ;; in a session that never opened an eww buffer
  (it "frees v and z on shr-map once shr loads, whichever mode renders"
    (let* ((form (seq-find (lambda (form)
                             (and (eq (car-safe form) 'after!) (eq (cadr form) 'shr)))
                           (web-browsing-config-tests--top-level-forms)))
           (pairs (mapcan #'map-form-key-pairs (map-form-groups form 'shr-map))))
      (expect form :not :to-be nil)
      (expect (assoc "v" pairs) :to-equal '("v"))
      (expect (assoc "z" pairs) :to-equal '("z")))))

;;; config-tests.el ends here
