(use-package rust-mode
  :ensure t
  :config
  (setq lsp-rust-server 'rust-analyzer))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :disabled
  :ensure t
  :pin melpa
  :config
  (setq racer-rust-src-path
        (or
         (getenv "RUST_SRC_PATH")
         (when (executable-find "rustc")
           (let* ((sysroot (s-trim-right
                            (shell-command-to-string
                             (format "%s --print sysroot" (executable-find "rustc")))))
                  (src-path (f-join sysroot "lib/rustlib/src/rust/src")))
             (when (file-exists-p src-path)
               src-path)
             src-path))))
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

