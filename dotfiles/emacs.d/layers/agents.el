(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :init
  (setq claude-code-ide-no-flicker t)
  :config
  (claude-code-ide-emacs-tools-setup))
