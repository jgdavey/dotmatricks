(use-package evil
  :ensure t
  :init
  (setq evil-toggle-key "C-c C-z"
        evil-cross-lines t
        evil-default-state 'emacs))

(use-package evil-surround
  :ensure t)

