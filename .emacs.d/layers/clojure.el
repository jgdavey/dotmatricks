(use-package flycheck-clj-kondo
  :pin melpa
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (require 'cider)
  (require 'ob-babashka)
  (define-clojure-indent
    (defroutes 'defun)
    (defui '(1 nil (1)))))

(use-package inf-clojure
  :pin melpa
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :hook (cider-mode . jd/prefer-cider-to-lsp)
  :commands cider-connect-clj
  :init
  (defun jd/prefer-cider-to-lsp ()
    (setq-local
     lsp-eldoc-enable-hover nil
     lsp-signature-auto-activate nil
     lsp-enable-indentation nil
     lsp-enable-completion-at-point nil))
  :config
  (setq org-babel-clojure-backend 'cider)
  (setq cider-eval-result-duration 'command) ;; 'change
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-use-tooltips nil))
