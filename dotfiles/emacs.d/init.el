;; Turn off useless UI elements -- do this as early as possible to
;; avoid visual thrashing.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")

(add-to-list 'default-frame-alist '(width . 1.0))
(add-to-list 'default-frame-alist '(height . 1.0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))

(defun jd/setup-frame (frame)
  (if (display-graphic-p frame)
      ;; GUI window mode
      (with-selected-frame frame
        (setq doom-modeline-icon t)
        (if (fboundp 'menu-bar-mode) (menu-bar-mode +1)))
    ;; terminal mode
    (with-selected-frame frame
      (setq doom-modeline-icon nil)
      (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
      (require 'mouse)
      (xterm-mouse-mode t)
      (setq mouse-sel-mode t)
      (defun track-mouse (e))

      (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
      (global-set-key (kbd "<mouse-5>") 'scroll-up-line))))

(add-hook 'after-make-frame-functions #'jd/setup-frame)

(jd/setup-frame (selected-frame))

(defun debug-on-load-obsolete (filename)
  (when (equal (car (last (split-string filename "[/\\]") 2))
               "obsolete")
    (debug)))
(add-to-list 'after-load-functions #'debug-on-load-obsolete)

;; Editor

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance
(fset 'yes-or-no-p 'y-or-n-p)

;; Newline at end of file
(setq require-final-newline t
      apropos-do-all t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; better buffer filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-trailing-separator-p 't)

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)

;; Don't copy everything to system clipboard by default
(setq select-enable-clipboard nil)

(defun jd/toggle-clipboard ()
  (interactive)
  (setq select-enable-clipboard (not select-enable-clipboard)))

;; ...but allow it to be enabled
(global-set-key (kbd "C-c C-o C-p") 'jd/toggle-clipboard)

;; Use ibuffer instead of buff-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq whitespace-style '(face trailing tabs))

(global-set-key (kbd "C-c C-o C-w") 'whitespace-mode)

(add-hook 'prog-mode-hook 'whitespace-mode)

(delete-selection-mode t)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;; Packages
(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu-mirror" . "https://mirror.endianness.com/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu-mirror" . 9)
        ("gnu" . 5)
        ("melpa" . 1)))

(defvar base-packages
  '((color-theme-sanityinc-tomorrow . "melpa-stable")
    (zenburn-theme . "melpa-stable")
    (use-package . "melpa-stable"))
  "A list of packages to ensure are installed at launch.")

(dolist (p base-packages)
  (add-to-list 'package-pinned-packages p))

(dolist (p base-packages)
  (let* ((pkg (car p)))
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-set-key (kbd "C-c C-o C-l") 'display-line-numbers-mode))

;; Package setup
(require 'use-package)

;; Required until PATH injection in Info.plist works
(use-package exec-path-from-shell
  :ensure t
  :custom
  ;; Don't use interactive shell
  (exec-path-from-shell-arguments (list "-l"))
  :if (display-graphic-p)
  :config
  (exec-path-from-shell-initialize))

(use-package recentf
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package nerd-icons
  :pin melpa
  :ensure t
  :demand t
  :custom
  (nerd-icons-font-family  "Hack Nerd Font Mono")
  )

(use-package doom-themes
  :ensure t
  :pin melpa
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  ;;(load-theme 'doom-gruvbox t)
  (load-theme 'doom-opera t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )

;; Has to be first to use :diminish option
(use-package diminish
  :pin melpa
  :ensure t)

(use-package ws-butler
  :pin melpa
  :ensure t)

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(use-package browse-kill-ring
  :pin melpa
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package smartparens
  :ensure t
  :pin melpa
  :bind (:map smartparens-strict-mode-map
         ("M-)" . sp-forward-slurp-sexp)
         ;;("M-(" . sp-wrap-round)
         ("M-(" . sp-wrap)
         ("M-J" . sp-join-sexp)
         ("C-M-t" . sp-transpose-sexp)
         :map smartparens-mode-map
         ("M-)" . sp-forward-slurp-sexp)
         :map emacs-lisp-mode-map
         ("M-q" . sp-indent-defun)
         :map lisp-mode-map
         ("M-q" . sp-indent-defun))
  :config
  (require 'smartparens-config)
  :init
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always-end)
  (setq blink-matching-paren nil)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (define-key smartparens-mode-map (read-kbd-macro "M-?") nil)
  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook
                  clojure-ts-mode-hook
                  clojurescript-mode-hook
                  clojurec-mode-hook
                  clojure-mode-hook
                  cider-repl-mode-hook
                  babashka-mode-hook))
    (add-hook hook #'turn-on-smartparens-strict-mode)))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line-function
        (lambda () (format " [%s]" (projectile-project-name))))
  ;; (setq projectile-enable-caching t)
  ;; (setq projectile-indexing-method 'hybrid)
  (add-to-list 'projectile-project-search-path "~/src")
  (projectile-mode +1)
  (if (not projectile-known-projects)
      (projectile-reset-known-projects)))

(use-package projectile-ripgrep
  :ensure t)

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (vertico-sort-function 'vertico-sort-history-alpha)
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (vertico-count 16)  ;; limit to a fixed size
  :init
  (vertico-mode))

;; Convenient path selection
(use-package vertico-directory
  :after vertico
  :ensure nil  ;; no need to install, it comes with vertico
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal
                               orderless-initialism
                               orderless-regexp))
  ;; Activate orderless completion
  (completion-styles '(orderless basic))
  ;; Enable partial completion for file wildcard support
  (completion-category-overrides '((file (styles partial-completion)))))

(defun jd/get-project-root (&optional _)
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

(use-package consult
  :ensure t
  :custom
  ;; Disable preview
  (consult-preview-key "M-.")
  (consult-project-function 'jd/get-project-root)
  (consult-async-input-throttle 0.3)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-ripgrep
   :add-history (seq-some #'thing-at-point '(region symbol))
   )
  :bind
  (("C-c i" . 'consult-imenu)     ;; original: imenu
   ("C-x b" . 'consult-buffer)    ;; Switch buffer, including recentf and bookmarks
   ;; ("M-l"   . 'consult-git-grep)  ;; Search inside a project
   ("C-s" . 'consult-line)
   :map projectile-command-map
   ("s r" . 'consult-ripgrep)
   ("s g" . 'consult-git-grep)
   ("s s" . 'consult-grep)
   ))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  (marginalia-separator "    ")
  (marginalia-max-relative-age (* 1 24 60 60)) ;; 1 day ago
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package embark
  :ensure t
  :bind
  (("C-."    . embark-act)         ;; Begin the embark process
   ("C-c ."  . embark-export)      ;; Use completion candidates in another context
   ("C-;"    . embark-dwim)
   ("C-h B"  . embark-bindings))   ;; alternative for `describe-bindings'
  :config
  (use-package embark-consult))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m l" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m b" . mc/edit-beginnings-of-lines)
         ("C-c m a" . mc/mark-all-like-this-dwim)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m u" . mc/unmark-next-like-this)))

(use-package expand-region
  :pin melpa
  :ensure t
;;  :bind ;;(("C-c e e" . er/))
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1)
  :init
  (global-company-mode 1))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode))

(use-package wgrep
  :ensure t
  :init
  (use-package wgrep-ag
    :ensure t)
  (autoload 'wgrep-ag-setup "wgrep-ag"))

(use-package ag
  :ensure t
  :config
  (require 'wgrep-ag)
  :init
  (defun jd/ag-project-root-fn (file-path)
    (or (ag/longest-string
         (vc-find-root file-path "build.boot")
         (vc-git-root file-path)
         (vc-svn-root file-path)
         (vc-hg-root file-path))
        file-path))
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-project-root-function 'jd/ag-project-root-fn)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package rg
  :ensure t)

(use-package ssh-config-mode
  :ensure t)

(use-package visual-fill-column
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :diminish which-key-mode)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package consult-yasnippet
  :ensure t)

(use-package doom-modeline
  :pin melpa
  :ensure t
  :init
  (doom-modeline-mode 1))

;; (use-package treemacs
;;   :ensure t
;;   :pin melpa)

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :pin melpa
  :demand t
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :pin melpa
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package nerd-icons-ibuffer
  :pin melpa
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package vterm
  :ensure t
  :pin melpa)

(setq browse-url-default-browser
      (if (eq system-type 'darwin)
          'browse-url-default-macosx-browser
        'browse-url-chrome))

;; use browser depending on url
(setq browse-url-handlers
      '(("github\\.com" . browse-url-default-browser)
        ("postgres\\.org" . eww-browse-url)
        ("docs\\.oracle\\.com" . eww-browse-url)))

(when (version<= emacs-version "28.0.0")
  (add-to-list 'browse-url-handlers '("." . browse-url-default-browser) t)
  (setq browse-url-browser-function browse-url-handlers))

(setq sh-basic-offset 2)

;; load all (or configured) files in ~/.emacs.d/layers
(require 'layers)

;; additional (local) config
(dolist (extra-file '("~/.emacs.d/default.el" "~/.emacs.d/local.el"))
  (jd/load-file-if-exists extra-file))

;; set custom file
(let ((custom "~/.emacs.d/custom.el"))
  (setq custom-file custom)
  (jd/load-file-if-exists custom))
