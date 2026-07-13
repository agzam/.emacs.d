;;; modules/lua/autoload/lua.el -*- lexical-binding: t; -*-

;; Installers for the Fennel toolchain (antifennel/fnlfmt/fennel-ls), cloned
;; and built from technomancy/xerool sr.ht repos into ~/.local.  M-x only.

;;;###autoload
(defun antifennel-install (arg)
  "Clone & install antifennel executable.
With a prefix, re-installs it."
  (interactive "P")
  (let ((repo-url "https://git.sr.ht/~technomancy/antifennel")
        (dir "/tmp/antifennel")
        (buf-name "install antifennel"))
    (when (and (executable-find "antifennel")
               (not arg))
      (user-error "antifennel is already installed"))
    (start-process-shell-command
     "Clone and install antifennel"
     buf-name
     ;; doom.d left a trailing "&&" here (its cleanup rm was commented out),
     ;; which broke the shell invocation; drop it and keep the clone around.
     (format (concat "rm -rf %s &&"
                     "git clone %s %s &&"
                     "cd %s && make && make install PREFIX=$HOME/.local")
             dir repo-url dir dir))
    (pop-to-buffer buf-name)))

;;;###autoload
(defun fennel-fnlfmt-install (arg)
  "Clone & install fnlfmt executable.
With a prefix, re-installs it."
  (interactive "P")
  (let ((repo-url "https://git.sr.ht/~technomancy/fnlfmt")
        (dir "/tmp/fnlfmt")
        (buf-name "install fnlfmt"))
    (when (and (executable-find "fnlfmt")
               (not arg))
      (user-error "fnlfmt is already installed"))
    (start-process-shell-command
     "Clone and install fnlfmt"
     buf-name
     (format (concat "rm -rf %s &&"
                     "git clone %s %s &&"
                     "cd %s && make && make install PREFIX=$HOME/.local &&"
                     "rm -rf %s")
             dir repo-url dir dir dir))
    (pop-to-buffer buf-name)))

;;;###autoload
(defun fennel-lsp-server-install (arg)
  "Clone & install fennel-ls executable.
With a prefix, re-installs it."
  (interactive "P")
  (let ((repo-url "https://git.sr.ht/~xerool/fennel-ls")
        (dir "/tmp/fennel-ls")
        (buf-name "install fennel-ls"))
    (when (and (executable-find "fennel-ls")
               (not arg))
      (user-error "fennel-ls is already installed"))
    (start-process-shell-command
     "Clone and install fennel-ls"
     buf-name
     (format (concat "rm -rf %s &&"
                     "git clone %s %s &&"
                     "cd %s && make && make install PREFIX=$HOME/.local &&"
                     "rm -rf %s")
             dir repo-url dir dir dir))
    (pop-to-buffer buf-name)))
