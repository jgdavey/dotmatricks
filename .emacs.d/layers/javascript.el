(use-package js2-mode
  :pin melpa
  :ensure t)

(use-package json
  :ensure json-mode
  :config
  (setq js-indent-level 2))

(use-package rjsx-mode
  :ensure t)
