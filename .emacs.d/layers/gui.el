(use-package default-text-scale
  :ensure t
  :bind (:map default-text-scale-mode-map
              ("s-0" . default-text-scale-reset)
              ("s-=" . default-text-scale-increase)
              ("s--" . default-text-scale-decrease))
  :init
  (default-text-scale-mode))

(setq gc-cons-threshold (* 1024 1024 8))     ;; 8mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb
