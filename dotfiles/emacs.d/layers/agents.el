;;; -*- lexical-binding: t; -*-

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :init
  (setq claude-code-ide-terminal-backend 'ghostel)
  ;; (setq claude-code-ide-no-flicker nil)
  :config
  (claude-code-ide-emacs-tools-setup))
