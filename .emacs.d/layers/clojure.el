(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (define-clojure-indent
    (defroutes 'defun)
    (defui '(1 nil (1)))))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package inf-clojure
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (defvar cider-clojure-cli-global-options-history '("-A:dev"))
  (defun set-cider-clojure-cli-global-options ()
    (interactive)
    (setq cider-clojure-cli-global-options
          (read-string "Additional CLI options: "
                       (car cider-clojure-cli-global-options-history)
                       'cider-clojure-cli-global-options-history)))
  (setq org-babel-clojure-backend 'cider)
  (setq cider-prompt-for-symbol nil)
  (setq cider-mode-line-show-connection nil)
  (setq cider-repl-display-help-banner nil))

(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :after cider
  :config
  (setq cljr-favor-prefix-notation nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-auto-sort-ns nil
        cljr-favor-private-functions nil)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))
