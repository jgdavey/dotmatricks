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
