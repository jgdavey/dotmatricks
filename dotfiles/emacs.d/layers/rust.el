(use-package rust-mode
  :ensure t
  :config
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-proc-macro-enable t))

(use-package cargo
  :ensure t
  :config
  (setq cargo-process--enable-rust-backtrace t)
  :init
  (add-hook 'rust-ts-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :pin melpa
  :init
  (add-hook 'rust-ts-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
