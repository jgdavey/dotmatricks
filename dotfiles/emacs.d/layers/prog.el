;;; -*- lexical-binding: t; -*-

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
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package es-mode
  :pin melpa
  :ensure t
  :defer t
  :init
  ;; es-mode's autoloads call this once yasnippet loads, which would load
  ;; all of es-mode (and js) at startup. Register its snippets without it.
  (defun es-mode-snippets-initialize ()
    (add-to-list 'yas-snippet-dirs
                 (expand-file-name "snippets"
                                   (file-name-directory (locate-library "es-mode")))
                 t)))

(use-package nginx-mode
  :ensure t
  :defer t)

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
  :defer t
  :pin melpa
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flycheck
  :ensure t
  :pin melpa)

(use-package consult-flycheck
  :ensure t
  :defer t)

(use-package lsp-mode
  :pin melpa
  :ensure t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-disabled-clients '(tfls))
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-response-timeout 2)
  (lsp-signature-auto-activate nil)
  (lsp-idle-delay 0.5)
  (lsp-terraform-ls-enable-show-reference t)
  ;; clojurescript-mode and clojurec-mode derive from clojure-mode, so
  ;; its hook covers them
  :hook ((clojure-mode . lsp-deferred)
         (clojure-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . jd/lsp-mode-setup-completion))
  :init
  (defun jd/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :config
  (dolist (dir '("[/\\\\]resources/public\\'"
                 "[/\\\\].datomic-local\\'"
                 "[/\\\\].clj-kondo\\'"
                 "[/\\\\]data/batches\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))

(use-package consult-lsp
  :ensure t
  :defer t)

(use-package lsp-ui
  :pin melpa
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
  :pin melpa
  :commands lsp-treemacs-errors-list
  :init
  (setq treemacs-space-between-root-nodes nil))


;; Hooked from lsp-mode above: hooks here would wait on `:after'
(use-package lsp-java
  :ensure t
  :pin melpa
  :after lsp-mode
  :init
  (setq lsp-java-jdt-ls-prefer-native-command t))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package protobuf-ts-mode
  :ensure t
  :mode ("\\.proto\\'")
  )

(use-package mise
  :ensure t
  :init
  (add-to-list 'magic-mode-alist
               '("#!.*/env\\(?:[ \t]+-[A-Za-z]+\\)*[ \t]+usage[ \t]+\\(ba\\|z\\|k\\)sh\\b" . sh-mode))
  (add-hook 'after-init-hook #'global-mise-mode))

;; Emacs 31's tree-sitter modes register their own grammar recipes (pinned
;; to the versions they were written against) and remaps from the
;; non-treesit modes; `treesit-enabled-modes' picks which remaps apply.
;; Its setter only runs once treesit is loaded.
(require 'treesit)
(setopt treesit-enabled-modes
        '(c-ts-mode
          css-ts-mode
          java-ts-mode
          js-ts-mode
          json-ts-mode
          python-ts-mode
          ruby-ts-mode
          rust-ts-mode
          toml-ts-mode
          tsx-ts-mode
          typescript-ts-mode
          yaml-ts-mode))

;; If the json-mode package is installed, it claims .json files
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

;; Grammars for modes that don't come with a recipe
(add-to-list 'treesit-language-source-alist
             '(proto "https://github.com/mitchellh/tree-sitter-proto"))

(defun jd/treesit-install-grammars ()
  "Install or update the grammars for `treesit-enabled-modes'.
Loading each mode registers the grammar versions it expects."
  (interactive)
  (dolist (mode treesit-enabled-modes)
    (when (autoloadp (symbol-function mode))
      (autoload-do-load (symbol-function mode) mode)))
  (dolist (source treesit-language-source-alist)
    (treesit-install-language-grammar (car source))))

(add-to-list 'auto-mode-alist '("\\.env\\.tmpl\\'" . bash-ts-mode))
