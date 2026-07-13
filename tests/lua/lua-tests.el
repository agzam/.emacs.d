;;; tests/lua/lua-tests.el --- lua/autoload/lua.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/lua/autoload/lua.el")

(defvar lua-tests--last-cmd nil
  "Shell command captured from the last stubbed installer run.")

(defun lua-tests--run (fn present arg)
  "Call installer FN with executable-find -> PRESENT and prefix ARG.
Records the shelled command string in `lua-tests--last-cmd'.  A top-level
defun (not a describe-body cl-flet) so it is in scope when specs run."
  (setq lua-tests--last-cmd nil)
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) present))
            ((symbol-function 'start-process-shell-command)
             (lambda (_name _buf cmd) (setq lua-tests--last-cmd cmd) nil))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (funcall fn arg)))

(describe "fennel toolchain installers"
  (it "errors when antifennel present and no prefix"
    (expect (lua-tests--run #'antifennel-install t nil) :to-throw 'user-error))

  (it "installs antifennel when absent"
    (lua-tests--run #'antifennel-install nil nil)
    (expect lua-tests--last-cmd :to-match "git clone")
    (expect lua-tests--last-cmd :to-match "make install PREFIX=\\$HOME/\\.local"))

  ;; Regression: doom.d's antifennel command ended in a dangling "&&" (its
  ;; cleanup rm was commented out, leaving a broken shell invocation).
  (it "does not leave a trailing && in the antifennel command"
    (lua-tests--run #'antifennel-install nil nil)
    (expect lua-tests--last-cmd :not :to-match "&&[[:space:]]*$"))

  (it "reinstalls antifennel with a prefix even when present"
    (lua-tests--run #'antifennel-install t t)
    (expect lua-tests--last-cmd :to-match "git clone"))

  (it "errors when fnlfmt present and no prefix"
    (expect (lua-tests--run #'fennel-fnlfmt-install t nil) :to-throw 'user-error))

  (it "installs fnlfmt when absent, cleaning up its clone"
    (lua-tests--run #'fennel-fnlfmt-install nil nil)
    (expect lua-tests--last-cmd :to-match "rm -rf /tmp/fnlfmt$"))

  (it "errors when fennel-ls present and no prefix"
    (expect (lua-tests--run #'fennel-lsp-server-install t nil) :to-throw 'user-error))

  (it "installs fennel-ls when absent"
    (lua-tests--run #'fennel-lsp-server-install nil nil)
    (expect lua-tests--last-cmd :to-match "git clone .*fennel-ls")))

(describe "friar drop"
  ;; friar was an AwesomeWM (Linux) fennel REPL; nothing should resurrect its
  ;; awesomewm-repl alias here.
  (it "leaves no awesomewm-repl alias behind"
    (expect (fboundp 'awesomewm-repl) :to-be nil)))
