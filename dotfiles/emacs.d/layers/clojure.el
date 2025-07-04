(use-package flycheck-clj-kondo
  :pin melpa
  :ensure t)

(defun jd/lsp-indent-defun (&optional arg)
  "Reindent the current defun.

If point is inside a string or comment, fill the current
paragraph instead, and with ARG, justify as well (with
`fill-paragraph')

Otherwise, reindent the current defun, and adjust the position
of the point."
  (interactive "P")
  (if (sp-point-in-string-or-comment)
      (fill-paragraph arg)
    (save-excursion
      (mark-defun)
      (deactivate-mark)
      (lsp-format-region (point) (mark)))))

(use-package clojure-mode
  :ensure t
  :bind (:map clojure-mode-map
              ("M-q" . jd/lsp-indent-defun))
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
  :bind (:map cider-repl-mode-map
              ("s-k" . comint-clear-buffer))
  :init
  (defun jd/prefer-cider-to-lsp ()
    (setq-local
     lsp-eldoc-enable-hover nil
     lsp-signature-auto-activate nil
     lsp-enable-completion-at-point nil))
  :config
  (setq org-babel-clojure-backend 'cider)
  (setq cider-eval-result-duration 'command) ;; 'change
  (setq cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-use-tooltips nil)
  (setq cider-test-fail-fast nil))

(use-package jet
  :pin melpa
  :ensure t)
