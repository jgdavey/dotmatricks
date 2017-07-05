;; Turn off useless UI elements -- do this as early as possible to
;; avoid visual thrashing.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")

(when (and window-system (eq system-type 'darwin))
  (set-face-attribute 'default nil :family "Menlo")
  (set-face-attribute 'default nil :height 140))

(when (not window-system)
  (require 'mouse)
  (xterm-mouse-mode t)
  (setq mouse-sel-mode t)
  (defun track-mouse (e))

  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Packages
(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/"))
      package-archive-priorities
      '(("org" . 20)
        ("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar base-packages '(monokai-theme
                        use-package
                        zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p base-packages)
  (when (not (package-installed-p p))
    (require 'cl)
    (package-install p)))

;; Editor

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance
(fset 'yes-or-no-p 'y-or-n-p)

;; Newline at end of file
(setq require-final-newline t)

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
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator ":")

(setq whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80)

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))

(defun cleanup-buffer ()
 "Perform a bunch of operations on the whitespace content of a buffer."
 (interactive)
 (indent-region (point-min) (point-max))
 (untabify (point-min) (point-max))
 (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

(load-theme 'zenburn t)

;; Package setup
(require 'use-package)

(use-package browse-kill-ring
  :ensure t)

(use-package exec-path-from-shell
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package gist
  :ensure t)

(use-package git-link
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package json
  :ensure json-mode)

(use-package markdown-mode
  :ensure t)

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("M-)" . paredit-forward-slurp-sexp))
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook       #'enable-paredit-mode))

(use-package jsx
  :ensure jsx-mode
  :mode ("\\.jsx\\'" . jsx-mode))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :config
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges"))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'whitespace-mode)
  (add-hook 'clojure-mode-hook 'show-paren-mode)
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (defui '(1 nil (1)))))

(use-package cider
  :ensure t
  :config
  (setq org-babel-clojure-backend 'cider)
  (setq cider-prompt-for-symbol nil)
  (setq cider-mode-line-show-connection nil)
  (setq cider-repl-display-help-banner nil))

(use-package clj-refactor
  :ensure t
  :init
  (setq cljr-favor-prefix-notation nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-auto-sort-ns nil
        cljr-favor-private-functions nil)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package ido
  :init
  (setq ido-everywhere t
        ido-enable-flex-matching t)
  (ido-mode t)
  ;; Recent files support
  (require 'recentf)
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
      (message "Aborting")))
  (global-set-key (kbd "C-x M-f") 'ido-recentf-open)
  (global-set-key (kbd "C-x C-M-f") 'ido-recentf-open))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-l" . mc/edit-lines)))

(use-package company
  :ensure t
  :init
  (global-company-mode 1))

(use-package deft
  :ensure t
  :bind ("C-c d" . deft)
  :init
  (setq deft-extensions '("org" "md")
        deft-default-extension "org"
        deft-directory "~/notes"
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 0))

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq org-directory (expand-file-name "~/org"))
  (setq org-babel-clojure-backend 'cider)
  (setq org-export-backends '(ascii html icalendar latex md odt))
  ;; Let's have pretty source code blocks
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-support-shift-select 'always)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 4)))
  (setq org-log-done t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
           "* TODO %?\n  %i")))
  (setq org-agenda-span 14)
  (add-to-list 'org-agenda-files org-directory 'append)
  :config
  ;; Here I specify the languages I want to be able to use with Org-babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (shell . t)
     (http . t)
     (ruby . t)
     (emacs-lisp . t))))

(use-package ob-http
  :ensure t)

(use-package orgit
  :ensure t)

(use-package diminish
  :ensure t
  :init
  (diminish 'company-mode "comp")
  (diminish 'paredit-mode))

(use-package wgrep
  :ensure t
  :init
  (autoload 'wgrep-ag-setup "wgrep-ag"))

(use-package ag
  :ensure t
  :bind (("C-c s" . ag-project)
         ("C-c C-s" . ag-project-regexp))
  :init
  (setq ag-highlight-search t)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

;; Make SQL mode usable

;; Silence compiler warnings
(defvar sql-product)
(defvar sql-prompt-regexp)
(defvar sql-prompt-cont-regexp)

(defun my-sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (when (eq sql-product 'postgres)
    ;; Allow symbol chars in database names in prompt.
    ;; Default postgres pattern was: "^\\w*=[#>] " (see `sql-product-alist').
    (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\)*=[#>] *")
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(][#>] *")
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n"))))

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)

; (setq sql-interactive-mode-hook nil)

;; additional (local) config
(dolist (extra-file '("~/.emacs.d/default.el" "~/.emacs.d/local.el"))
  (if (file-exists-p extra-file)
    (load-file extra-file)
    nil))

;; set custom file
(let ((custom "~/.emacs.d/custom.el"))
  (setq custom-file custom)
  (if (file-exists-p custom)
      (load-file custom)
    nil))
