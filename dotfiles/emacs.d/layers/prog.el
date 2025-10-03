(use-package go-mode
  :ensure t
  :hook ((go-mode . jd/go-mode-hook)
         (go-ts-mode . jd/go-mode-hook))
  :init
  (defun jd/go-mode-hook ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (whitespace-mode -1)
    (setq tab-width 4)
    (setq indent-tabs-mode +1)
    (setq-local electric-indent-inhibit t)
    (setq-local
     whitespace-style
     '(face ; viz via faces
       trailing ; trailing blanks visualized
       space-before-tab
       space-after-tab
       newline ; lines with only blanks
       indentation ; spaces used for indent
       empty ; empty lines at beginning or end
       ))
    (whitespace-mode +1)))

(use-package applescript-mode
  :pin melpa
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package es-mode
  :pin melpa
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.mustache\\'"
         "\\.erb\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        css-indent-offset 2
        web-mode-content-types-alist '(("sql" . "\\.sql\\.erb\\'"))
        ))

(use-package dumb-jump
  :ensure t
  :pin melpa
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flycheck
  :ensure t
  :pin melpa)

(use-package consult-flycheck
  :ensure t)

(use-package lsp-mode
  :pin melpa-stable
  :ensure t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :hook ((rust-mode . lsp)
         (rust-ts-mode .lsp)
         (clojure-mode . lsp)
         (clojure-ts-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (go-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         ;;(enh-ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . jd/lsp-mode-setup-completion))
  :commands lsp
  :autoload lsp-deferred
  :init
  (defun jd/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :config
  (setq ;; lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable nil
        lsp-response-timeout 2
        lsp-completion-enable t
        lsp-signature-auto-activate nil
        lsp-enable-snippet t
        lsp-idle-delay 0.5)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]resources/public\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].datomic-local\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].clj-kondo\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]data/batches\\'")
  (setq lsp-enable-xref t))

(use-package consult-lsp
  :ensure t)

(use-package lsp-ui
  :pin melpa-stable
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil
        ;; lsp-ui-doc-header nil
        ;; lsp-ui-sideline-show-diagnostics t
        ;; lsp-ui-doc-position 'top
        ;; lsp-ui-doc-alignment 'frame
        lsp-ui-sideline-delay 0.5
        lsp-ui-doc-delay 0.5
        ))

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :pin melpa-stable
  :commands lsp-treemacs-errors-list
  :init
  (setq treemacs-space-between-root-nodes nil))


(use-package lsp-docker
  :ensure t
  :after lsp-mode)

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :hook ((java-mode . lsp)
         (java-ts-mode . lsp))
  :init
  (setq lsp-java-jdt-ls-prefer-native-command t))

(use-package lsp-tailwindcss
  :ensure t
  :after lsp-mode
  :pin melpa
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  ;; doesn't seem to work yet
  ;(add-to-list 'lsp-tailwindcss-major-modes 'clojurescript-mode)
  )

(use-package dockerfile-mode
  :ensure t)

(use-package protobuf-ts-mode
  :ensure t
  :mode ("\\.proto\\'")
  )

(use-package mise
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-mise-mode))

;; Taken from:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (c++ "https://github.com/tree-sitter/tree-sitter-cpp")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
        (php "https://github.com/tree-sitter/tree-sitter-php")
        (proto "https://github.com/mitchellh/tree-sitter-proto")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Treesit wrappers for Emacs 29+
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (css-mode . css-ts-mode)
        (java-mode . java-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (rust-mode . rust-ts-mode)
        (toml-mode . toml-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (typescriptreact-mode . tsx-ts-mode)
        (python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)))
