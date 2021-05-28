;; Turn off useless UI elements -- do this as early as possible to
;; avoid visual thrashing.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")

(defun jd/setup-frame (frame)
  (if (display-graphic-p frame)
      ;; GUI window mode
      (with-selected-frame frame
        (setq doom-modeline-icon t)
        (exec-path-from-shell-initialize)
        (if (fboundp 'menu-bar-mode) (menu-bar-mode +1))
        (set-frame-size frame 187 56))
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

(setq whitespace-style '(face trailing tabs))

(global-set-key (kbd "C-c C-o C-w") 'whitespace-mode)

(add-hook 'prog-mode-hook 'whitespace-mode)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;(setq gnutls-algorithm-priority nil)

;; Packages
(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("org" . 20)
        ("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

(defvar base-packages
  '((color-theme-sanityinc-tomorrow . "melpa-stable")
    (monokai-pro-theme . "melpa-stable")
    (all-the-icons . "melpa")
    (zenburn-theme . "melpa-stable")
    (use-package . "melpa-stable"))
  "A list of packages to ensure are installed at launch.")

(dolist (p base-packages)
  (add-to-list 'package-pinned-packages p))

(package-initialize)

(dolist (p base-packages)
  (let* ((pkg (car p)))
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-set-key (kbd "C-c C-o C-l") 'display-line-numbers-mode))

;; Package setup
(require 'use-package)

(use-package doom-themes
  :ensure t
  :pin melpa
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (load-theme 'doom-opera t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Has to be first to use :diminish option
(use-package diminish
  :pin melpa
  :ensure t)

(use-package exec-path-from-shell
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
              ("M-q" . sp-indent-defun)
              :map smartparens-mode-map
              ("M-)" . sp-forward-slurp-sexp))
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
                  clojure-mode-hook
                  cider-repl-mode-hook))
    (add-hook hook #'turn-on-smartparens-strict-mode)))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line-function
        (lambda () (format " [%s]" (projectile-project-name))))
  (setq projectile-completion-system 'ivy)
  ;;(projectile-mode +1)
  (counsel-projectile-mode +1))

(use-package projectile-ripgrep
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view))
  :config
  (progn
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d "
          ivy-height 12
          ivy-display-style 'fancy)
    (defun jd/ivy-kill-buffer ()
      (interactive)
      (ivy-set-action 'kill-buffer)
      (ivy-done))
    (bind-keys
     :map ivy-switch-buffer-map
     ("C-k" . jd/ivy-kill-buffer))))

(use-package smex
  :ensure t)

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :ensure t
  :pin melpa)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c C-s" . isearch-forward)
         :map isearch-mode-map
         ("C-c C-s" . swiper-isearch-toggle)
         :map swiper-map
         ("C-c C-s" . swiper-isearch-toggle)))

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

(use-package doom-modeline
  :pin melpa
  :ensure t
  :init
  (doom-modeline-mode 1))

(setq browse-url-default-browser
      (if (eq system-type 'darwin)
          'browse-url-default-macosx-browser
        'browse-url-chrome))

;; use browser depending on url
(setq browse-url-browser-function
      '(("github\\.com" . browse-url-default-browser)
        ("postgres\\.org" . eww-browse-url)
        ("." . browse-url-default-browser)))

;; load all (or configured) files in ~/.emacs.d/layers
(require 'layers)

;; additional (local) config
(dolist (extra-file '("~/.emacs.d/default.el" "~/.emacs.d/local.el"))
  (jd/load-file-if-exists extra-file))

;; set custom file
(let ((custom "~/.emacs.d/custom.el"))
  (setq custom-file custom)
  (jd/load-file-if-exists custom))
