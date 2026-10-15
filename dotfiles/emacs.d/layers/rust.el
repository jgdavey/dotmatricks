;;; -*- lexical-binding: t; -*-

;; Provides the .rs entry in `auto-mode-alist', which remaps to
;; rust-ts-mode (see `treesit-enabled-modes')
(use-package rust-mode
  :ensure t
  :defer t)

(use-package cargo
  :ensure t
  :defer t
  :config
  (setq cargo-process--enable-rust-backtrace t)
  :init
  (add-hook 'rust-ts-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :pin melpa
  :defer t
  :init
  (add-hook 'rust-ts-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
