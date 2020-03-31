(use-package powerline
  ;; disabled by default. Enable with (powerline-default-theme)
  :ensure t
  :init (setq powerline-default-separator 'utf-8))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

(use-package default-text-scale
  :ensure t
  :bind (:map default-text-scale-mode-map
              ("s-0" . default-text-scale-reset)
              ("s-=" . default-text-scale-increase)
              ("s--" . default-text-scale-decrease))
  :init
  (default-text-scale-mode))

