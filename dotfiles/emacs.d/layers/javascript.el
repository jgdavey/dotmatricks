;;; -*- lexical-binding: t; -*-

;; .js/.ts/.tsx use the built-in tree-sitter modes (see `treesit-enabled-modes')

(use-package js
  :custom
  (js-indent-level 2))

(use-package jq-format
  :ensure t)
