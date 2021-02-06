(use-package go-mode
  :ensure t
  :init
  (defun jd/go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (whitespace-mode -1)
    (setq indent-tabs-mode +1)
    (setq-local whitespace-style '(face trailing))
    (whitespace-mode +1))
  (add-hook 'go-mode-hook 'jd/go-mode-hook))

(use-package yaml-mode
  :ensure t)

(use-package es-mode
  :pin melpa
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  ;; (add-hook 'web-mode-hook 'display-line-numbers-mode)
  :config
  (use-package company-web
    :ensure t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        css-indent-offset 2))

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :pin melpa
  :ensure t
  :hook ((rust-mode . lsp)
         ;;(clojure-mode . lsp)
         ;;(clojurescript-mode . lsp)
         ;;(clojurec-mode . lsp)
         ;;(enh-ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-response-timeout 2
        lsp-completion-enable t
        lsp-enable-snippet t))

(use-package lsp-ui
  :pin melpa
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets
    :ensure t)
  :init
  (yas-global-mode 1))
