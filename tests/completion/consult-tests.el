;;; tests/completion/consult-tests.el --- completion/autoload/consult.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "modules/completion/autoload/consult.el")

;; consult owns these and is not installed here; the narrowing predicate
;; reads the first as the key in force, the group titles the second as the
;; key to spell out, so the specs bind them dynamically.
(defvar consult--narrow nil)
(defvar consult-narrow-key nil)

(describe "candidate-prefix"
  (it "takes the text before the first separator"
    (expect (candidate-prefix "Copilot:gpt-4o" ":") :to-equal "Copilot"))

  ;; ollama names carry a second colon (llama3:latest); only the first counts
  (it "stops at the first separator"
    (expect (candidate-prefix "Ollama:llama3:latest" ":") :to-equal "Ollama"))

  (it "reads an alist entry by its key"
    (expect (candidate-prefix '("Claude-OAuth:claude-opus-5" backend model) ":")
            :to-equal "Claude-OAuth"))

  (it "is nil without the separator"
    (expect (candidate-prefix "solar" ":") :to-be nil)))

(describe "narrowing-keys"
  (it "keys each group by its initial"
    (expect (narrowing-keys '("anthropic" "github-copilot" "ollama"))
            :to-equal '((?a . "anthropic") (?g . "github-copilot") (?o . "ollama"))))

  (it "moves on to the next free letter of a group whose initial is taken"
    (expect (narrowing-keys '("Claude-OAuth" "Copilot"))
            :to-equal '((?c . "Claude-OAuth") (?o . "Copilot"))))

  (it "lets a pin fix a group's key and reserves it before the others pick"
    (expect (narrowing-keys '("Claude-OAuth" "Copilot" "Ollama")
                            '((?a . "Claude-OAuth") (?g . "Copilot")))
            :to-equal '((?a . "Claude-OAuth") (?g . "Copilot") (?o . "Ollama"))))

  ;; the pinned key is off the table even for a group that comes first
  (it "never hands a pinned key to an earlier group"
    (expect (narrowing-keys '("Copilot" "Claude-OAuth") '((?c . "Claude-OAuth")))
            :to-equal '((?o . "Copilot") (?c . "Claude-OAuth"))))

  (it "drops a group with no letter left"
    (expect (narrowing-keys '("a" "a" "ab")) :to-equal '((?a . "a") (?b . "ab"))))

  (it "ignores a pin for a group that is not there"
    (expect (narrowing-keys '("ollama") '((?x . "openai")))
            :to-equal '((?o . "ollama")))))

(describe "prefix-narrowing"
  :var (config)
  (before-each
    (setq config (prefix-narrowing '("anthropic/opus" "github-copilot/gpt" "anthropic/sonnet" "ollama/solar")
                                   "/")))

  (it "keys each distinct prefix once, in order of first appearance"
    (expect (plist-get config :keys)
            :to-equal '((?a . "anthropic") (?g . "github-copilot") (?o . "ollama"))))

  (it "matches the candidates of the key in force"
    (let ((consult--narrow ?a)
          (pred (plist-get config :predicate)))
      (expect (funcall pred "anthropic/opus") :to-be-truthy)
      (expect (funcall pred "anthropic/sonnet") :to-be-truthy)
      (expect (funcall pred "ollama/solar") :to-be nil)))

  ;; completion hands the predicate an alist's conses, not their keys
  (it "matches an alist entry by its key"
    (let* ((config (prefix-narrowing '(("Copilot:gpt-4o" b m) ("Ollama:solar" b m)) ":"))
           (consult--narrow ?o))
      (expect (funcall (plist-get config :predicate) '("Ollama:solar" b m)) :to-be-truthy)
      (expect (funcall (plist-get config :predicate) '("Copilot:gpt-4o" b m)) :to-be nil)))

  (it "never matches a candidate without a prefix"
    (let* ((config (prefix-narrowing '("ollama/solar" "solar") "/"))
           (consult--narrow ?o))
      (expect (funcall (plist-get config :predicate) "solar") :to-be nil))))

(describe "prefix-group"
  :var (title)
  (before-each
    (setq title (lambda (candidate &optional narrow-key)
                  ;; consult parses the key itself; the built-in parser is
                  ;; equivalent for a plain key description
                  (cl-letf (((symbol-function 'consult--key-parse) #'key-parse))
                    (let ((consult-narrow-key narrow-key))
                      (funcall (prefix-group ":" '((?c . "Claude-OAuth") (?o . "Ollama")))
                               candidate nil))))))

  (it "titles a candidate by its prefix and the key that narrows to it"
    (expect (funcall title "Claude-OAuth:claude-opus-5" "<")
            :to-equal "Claude-OAuth  (< c)"))

  (it "leaves the key off a group without one"
    (expect (funcall title "Copilot:gpt-4o" "<") :to-equal "Copilot"))

  (it "leaves the key off while narrowing is disabled"
    (expect (funcall title "Ollama:solar" nil) :to-equal "Ollama"))

  (it "puts a candidate without a prefix in no group"
    (expect (funcall title "solar" "<") :to-be nil))

  ;; vertico asks for the candidate itself with TRANSFORM set
  (it "hands the candidate back untouched on transform"
    (expect (funcall (prefix-group ":" nil) "Ollama:solar" t) :to-equal "Ollama:solar")))

(describe "default-first"
  (it "moves the default to the front"
    (expect (default-first '("a/x" "b/y" "c/z") "b/y") :to-equal '("b/y" "a/x" "c/z")))

  (it "moves an alist entry by its key"
    (expect (default-first '(("k1" 1) ("k2" 2)) "k2") :to-equal '(("k2" 2) ("k1" 1))))

  (it "is nil for a default that is not in the list"
    (expect (default-first '("a/x") "zzz") :to-be nil))

  (it "is nil without a string default or a list"
    (expect (default-first '("a/x") nil) :to-be nil)
    (expect (default-first (lambda (&rest _) nil) "a/x") :to-be nil)))

;; consult is not installed here: the read is stubbed to record what it was
;; handed, and the require is faked away.
(defmacro consult-tests--with-read-stub (return &rest body)
  "Run BODY with `consult--read' recording its arguments and returning RETURN.
The table and options land in `captured', the `completing-read-function'
in force during the read in `reader-inside'; both are lexical in the
spec that expands this."
  (declare (indent 1))
  `(with-fake-feature 'consult
     (cl-letf (((symbol-function 'consult--read)
                (lambda (table &rest options)
                  (setq captured (cons table options)
                        reader-inside completing-read-function)
                  ,return)))
       ,@body)))

(describe "call-with-prefix-narrowing"
  :var (captured reader-inside)
  (before-each
    (setq captured nil reader-inside 'unset))

  (it "routes the function's completing-read through consult--read"
    (consult-tests--with-read-stub "chosen"
      (expect (call-with-prefix-narrowing
               "/" nil
               (lambda () (completing-read "Model: " '("anthropic/x" "ollama/y") nil t)))
              :to-equal "chosen"))
    (expect (car captured) :to-equal '("anthropic/x" "ollama/y"))
    (expect (plist-get (cdr captured) :prompt) :to-equal "Model: ")
    (expect (plist-get (cdr captured) :require-match) :to-be t))

  (it "derives the narrowing from the candidate prefixes"
    (consult-tests--with-read-stub "chosen"
      (call-with-prefix-narrowing
       ":" '((?g . "Copilot"))
       (lambda () (completing-read "Model: " '("Copilot:gpt-4o" "Ollama:solar") nil t))))
    (let ((narrow (plist-get (cdr captured) :narrow)))
      (expect (plist-get narrow :keys) :to-equal '((?g . "Copilot") (?o . "Ollama")))
      (expect (functionp (plist-get narrow :predicate)) :to-be t)))

  (it "groups the list under headers that spell out those keys"
    (cl-letf (((symbol-function 'consult--key-parse) #'key-parse))
      (let ((consult-narrow-key "<"))
        (consult-tests--with-read-stub "chosen"
          (call-with-prefix-narrowing
           ":" '((?g . "Copilot"))
           (lambda () (completing-read "Model: " '("Copilot:gpt-4o" "Ollama:solar") nil t))))
        (let ((group (plist-get (cdr captured) :group)))
          (expect (funcall group "Copilot:gpt-4o" nil) :to-equal "Copilot  (< g)")
          (expect (funcall group "Ollama:solar" nil) :to-equal "Ollama  (< o)")))))

  ;; consult--read reads through completing-read itself; a shim that answered
  ;; that call too would recurse forever
  (it "restores the outer reader for the read consult performs"
    (let ((outer completing-read-function))
      (consult-tests--with-read-stub "chosen"
        (call-with-prefix-narrowing
         "/" nil (lambda () (completing-read "P: " '("a/x") nil t))))
      (expect reader-inside :to-be outer)
      (expect completing-read-function :to-be outer)))

  (it "leads with the default instead of handing it to completing-read"
    (consult-tests--with-read-stub "chosen"
      (call-with-prefix-narrowing
       ":" nil
       (lambda () (completing-read "P: " '(("Copilot:a" 1) ("Ollama:b" 2)) nil t nil nil "Ollama:b"))))
    (expect (car captured) :to-equal '(("Ollama:b" 2) ("Copilot:a" 1)))
    (expect (plist-get (cdr captured) :default) :to-be nil)
    (expect (plist-member (cdr captured) :sort) :to-be-truthy)
    (expect (plist-get (cdr captured) :sort) :to-be nil))

  (it "passes a default the list lacks straight through"
    (consult-tests--with-read-stub "chosen"
      (call-with-prefix-narrowing
       ":" nil (lambda () (completing-read "P: " '("Copilot:a") nil t nil nil "gone"))))
    (expect (car captured) :to-equal '("Copilot:a"))
    (expect (plist-get (cdr captured) :default) :to-equal "gone"))

  ;; completing-read takes (SYMBOL . POSITION); consult--read only a symbol
  (it "strips the position off a history cons"
    (consult-tests--with-read-stub "chosen"
      (call-with-prefix-narrowing
       "/" nil (lambda () (completing-read "P: " '("a/x") nil nil nil '(some-history . 1)))))
    (expect (plist-get (cdr captured) :history) :to-be 'some-history))

  (it "hands the function's arguments and result back unchanged"
    (consult-tests--with-read-stub "a/x"
      (expect (call-with-prefix-narrowing
               "/" nil
               (lambda (prefix)
                 (concat prefix (completing-read "P: " '("a/x") nil t)))
               "picked:")
              :to-equal "picked:a/x"))))
