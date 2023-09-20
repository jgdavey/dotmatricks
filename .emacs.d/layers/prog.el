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

(use-package yaml-mode
  :ensure t)

(use-package es-mode
  :pin melpa
  :ensure t)

;; (use-package restclient
;;   :pin melpa
;;   :ensure t)

;; (use-package ob-restclient
;;   :pin melpa
;;   :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package company-nginx
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.mustache\\'"
         "\\.erb\\'")
  :init
  ;; (add-hook 'web-mode-hook 'display-line-numbers-mode)
  :config
  (use-package company-web
    :ensure t)
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

(use-package imenu-anywhere
  :ensure t
  :bind (("C-." . jd/imenu-anywhere))
  :init
  (defvar-local jd/show-imenu-filename t)
  (defun jd/hide-imenu-filename ()
    (setq jd/show-imenu-filename nil))
  :config
  (add-hook 'clojure-mode-hook #'jd/hide-imenu-filename)
  (defun jd/imenu-anywhere-preprocess (entry entry-name)
    (when entry
      (let* ((bufname (when (and jd/show-imenu-filename
                                 (markerp (cdr entry)))
                        (buffer-name (marker-buffer (cdr entry)))))
             (bname (if bufname
                        (concat
                         (propertize bufname 'face 'ivy-grep-info)
                         (propertize ": " 'face 'ivy-separator))
                      "")))
        (setcar entry (concat bname
                              (when entry-name
                                (concat
                                 (propertize entry-name 'face 'ivy-grep-info)
                                 (propertize imenu-anywhere-delimiter 'face 'ivy-separator)))
                              (car entry))))
      entry))

  (defun jd/imenu-anywhere ()
    "Use ivy for imenu-anywhere"
    (interactive)
    (unless (require 'ivy nil t)
      (error "[imenu-anywhere]: This command requires 'ivy' package"))
    (let ((ivy-sort-functions-alist)
          (imenu-anywhere-preprocess-entry-function #'jd/imenu-anywhere-preprocess)
          (completing-read-function 'ivy-completing-read))
      (imenu-anywhere))))


(use-package flycheck
  :ensure t
  :pin melpa)

(use-package lsp-mode
  :pin melpa
  :ensure t
  :hook ((rust-mode . lsp)
         (clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (go-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         ;;(enh-ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :autoload lsp-deferred
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

(use-package lsp-ivy
  :ensure t
  :pin melpa
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :pin melpa
  :commands lsp-treemacs-errors-list
  :init
  (setq treemacs-space-between-root-nodes nil))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :pin melpa
  :config
  (setq lsp-java-java-path "/usr/local/opt/java/bin/java")
  )

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

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

;; Taken from:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Treesit wrappers for Emacs 29+
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (typescriptreact-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (rust-mode . rust-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (go-mode . go-ts-mode)
        (java-mode . java-ts-mode)
        (python-mode . python-ts-mode)))
