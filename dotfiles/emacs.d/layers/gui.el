;;; -*- lexical-binding: t; -*-

(use-package default-text-scale
  :ensure t
  :bind (:map default-text-scale-mode-map
              ("s-0" . default-text-scale-reset)
              ("s-=" . default-text-scale-increase)
              ("s--" . default-text-scale-decrease))
  :init
  (default-text-scale-mode))

;; gc-cons-threshold is set in early-init.el
(setq read-process-output-max (* 1024 1024)) ;; 1mb
