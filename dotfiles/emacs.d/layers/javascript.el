(use-package js2-mode
  :pin melpa
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package jq-format
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package counsel-jq
  :ensure t
  :after counsel)

(use-package typescript-mode
  :ensure t
  :after tree-sitter
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  )
