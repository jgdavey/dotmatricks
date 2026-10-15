;;; -*- lexical-binding: t; -*-

;; Loaded before the package system and the first frame. Settings here
;; take effect before anything is drawn, avoiding visual thrashing.

;; Don't collect garbage during startup; restore a sane threshold after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 8)))) ;; 8mb

;; Turn off useless UI elements
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(width . 1.0))
(add-to-list 'default-frame-alist '(height . 1.0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))

;; Don't resize the frame when the font is set during init
(setq frame-inhibit-implied-resize t)
