(use-package forge
  :after magit
  :pin melpa
  :ensure t
  :disabled
  :config
  ;; These two setting make the list look more like GitHub's default PR list
  (setq forge-topic-list-order '(created . string>))
  (setq forge-topic-list-limit '(50 . 0)))
