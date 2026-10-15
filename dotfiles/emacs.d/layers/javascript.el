;;; -*- lexical-binding: t; -*-

;; .js/.ts/.tsx use the built-in tree-sitter modes (see `treesit-enabled-modes')

(use-package js
  :defer t
  :custom
  (js-indent-level 2))

(use-package jq-format
  :ensure t
  :defer t)
