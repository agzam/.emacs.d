;;; tests/general/buffers-tests.el --- general/autoload/buffers.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/buffers.el")

(describe "rudekill-matching-buffers"
  (it "kills matching buffers without asking and returns the count"
    (let ((a (generate-new-buffer "rudekill-probe-a"))
          (b (generate-new-buffer "rudekill-probe-b"))
          (keep (generate-new-buffer "rudekill-spare-me")))
      (unwind-protect
          (progn
            (expect (rudekill-matching-buffers "^rudekill-probe-") :to-equal 2)
            (expect (buffer-live-p a) :to-be nil)
            (expect (buffer-live-p b) :to-be nil)
            (expect (buffer-live-p keep) :to-be-truthy))
        (dolist (buf (list a b keep))
          (when (buffer-live-p buf) (kill-buffer buf))))))
  (it "spares internal (space-prefixed) buffers by default"
    (let ((internal (generate-new-buffer " rudekill-internal-probe")))
      (unwind-protect
          (expect (rudekill-matching-buffers "rudekill-internal-probe")
                  :to-equal 0)
        (when (buffer-live-p internal) (kill-buffer internal))))))

(describe "yank-buffer-name"
  (it "copies the bare buffer name, usable by get-buffer"
    (let ((kill-ring nil)
          (kill-ring-yank-pointer nil))
      (with-temp-buffer
        (rename-buffer "yank-name-spec" t)
        (yank-buffer-name)
        (expect (car kill-ring) :to-equal (buffer-name))
        (expect (get-buffer (car kill-ring)) :to-be (current-buffer))))))
