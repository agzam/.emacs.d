;;; modules/java/config.el -*- lexical-binding: t; -*-

(use-package lsp-java
  :defer t
  :init
  (add-hook! (java-ts-mode java-mode) #'lsp!))
