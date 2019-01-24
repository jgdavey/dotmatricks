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
  (set-face-attribute 'default nil :font "Hack-14.0")
  (set-frame-font "Hack-14.0" nil t))

(when (not window-system)
  (require 'mouse)
  (xterm-mouse-mode t)
  (setq mouse-sel-mode t)
  (defun track-mouse (e))

  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

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

(defvar base-packages '(ample-theme
                        color-theme-sanityinc-tomorrow
                        monokai-theme
                        railscasts-reloaded-theme
                        use-package
                        zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p base-packages)
  (when (not (package-installed-p p))
    (require 'cl)
    (package-install p)))

(when (version<= "26.0.50" emacs-version)
  ;; (global-display-line-numbers-mode)
  (set-face-attribute 'line-number-current-line nil :background "color-237")
  (global-set-key (kbd "C-c C-o C-l") 'display-line-numbers-mode))

;;(load-theme 'railscasts-reloaded t)
(load-theme 'sanityinc-tomorrow-eighties t)

;; Package setup
(require 'use-package)

;; Has to be first to use :diminish option
(use-package diminish
  :ensure t)

(use-package powerline
  ;; disabled by default. Enable with (powerline-default-theme)
  :ensure t
  :init (setq powerline-default-separator 'utf-8))

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t)

(use-package gist
  :ensure t)

(use-package git-link
  :ensure t)

(use-package ruby-mode
  :init
  ;; (add-hook 'ruby-mode-hook 'display-line-numbers-mode)
  (add-hook 'ruby-mode-hook 'whitespace-mode))

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'whitespace-mode))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package inf-ruby
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'whitespace-mode))

(use-package json
  :ensure json-mode)

(use-package markdown-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :bind (:map smartparens-strict-mode-map
              ("M-)" . sp-forward-slurp-sexp)
              ("M-(" . sp-wrap-from-point)
              ("M-J" . sp-join-sexp)
              ("M-q" . sp-indent-defun))
  :config
  (require 'smartparens-config)
  :init
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always)
  (setq blink-matching-paren nil)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  cider-repl-mode-hook))
    (add-hook hook #'turn-on-smartparens-strict-mode)))

(use-package jsx
  :ensure jsx-mode
  :mode ("\\.jsx\\'" . jsx-mode))

(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-c g" . magit-status)
         :map git-commit-mode-map
         ("C-c C-a" . git-commit-co-authored-by))
  :config
  (add-to-list 'git-commit-known-pseudo-headers "Co-authored-by")
  (defun git-commit-co-authored-by (name mail)
    "Insert a header mentioning a co-author"
    (interactive (git-commit-read-ident))
    (git-commit-insert-header "Co-authored-by" name mail))
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-completing-read-function 'ivy-completing-read)
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges"))

(use-package forge
  :after magit
  :pin melpa
  :ensure t
  :config
  ;; These two setting make the list look more like GitHub's default PR list
  (setq forge-topic-list-order '(created . string>))
  (setq forge-topic-list-limit '(50 . 0)))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'whitespace-mode)
  ;; (add-hook 'clojure-mode-hook 'display-line-numbers-mode)
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (defui '(1 nil (1)))))

(use-package inf-clojure
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (setq org-babel-clojure-backend 'cider)
  (setq cider-prompt-for-symbol nil)
  (setq cider-mode-line-show-connection nil)
  (setq cider-repl-display-help-banner nil))

(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :after cider
  :init
  (setq cljr-favor-prefix-notation nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-auto-sort-ns nil
        cljr-favor-private-functions nil)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line-function
        (lambda () (format " [%s]" (projectile-project-name))))
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :pin melpa
  :config
  (counsel-projectile-mode))

(use-package ivy
  :ensure t
  :pin melpa
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
    (defun my/ivy-kill-buffer ()
      (interactive)
      (ivy-set-action 'kill-buffer)
      (ivy-done))
    (bind-keys
     :map ivy-switch-buffer-map
     ("C-k" . my/ivy-kill-buffer))))

(use-package smex
  :ensure t)

(use-package counsel
  :ensure t
  :pin melpa
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-l" . mc/edit-lines)))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t)
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
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c C-o" . my/org-open-at-point))
  :init
  (use-package org-bullets
    :ensure t)
  (use-package ob-http
    :ensure t)
  (use-package orgit
    :ensure t)
  (use-package ox-pandoc
    :defer t
    :ensure t)
  (use-package ob-sql-mode
    :ensure t)
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
        `(("t" "Todo" entry (file+headline ,(concat org-directory "/todo.org") "Tasks")
           "* TODO %?\n  %i")
          ("l" "Linked Todo" entry (file+headline ,(concat org-directory "/todo.org") "Tasks")
           "* TODO %?\n  %i\n  %A")
          ("s" "Scheduled Todo" entry (file+headline ,(concat org-directory "/todo.org") "Tasks")
           "* TODO %?\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n  %i\n  %a\n")))
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "DELEGATED")))

  (add-to-list 'org-agenda-files org-directory 'append)
  :config
  (defun my/org-open-at-point (&optional arg)
    "Wrapper for mu4e-view-go-to-url to use eww instead of default browser"
    (interactive "P")
    (if arg
        (let ((browse-url-browser-function 'eww-browse-url))
          (org-open-at-point))
      (org-open-at-point)))
  (require 'ob-sql)
  (require 'ob-sql-mode)
  (require 'ob-clojure)
  ;; Here I specify the languages I want to be able to use with Org-babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (shell . t)
     (http . t)
     (ruby . t)
     (emacs-lisp . t))))

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
  (defun my-project-root-fn (file-path)
    (or (ag/longest-string
         (vc-find-root file-path "build.boot")
         (vc-git-root file-path)
         (vc-svn-root file-path)
         (vc-hg-root file-path))
        file-path))
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-project-root-function 'my-project-root-fn)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package ssh-config-mode
  :ensure t)

(use-package visual-fill-column
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  ;; (add-hook 'web-mode-hook 'display-line-numbers-mode)
  :config
  (use-package company-web
    :ensure t)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        css-indent-offset 2))

(use-package winner
  :ensure t
  :config
  (winner-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :diminish which-key-mode)

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

(defun my-sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (toggle-truncate-lines t)
  (when (or (eq sql-product 'postgres)
            (eq sql-product 'heroku))
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n")
      (comint-send-string proc "\\pset pager off\n"))))

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
;; (add-hook 'sql-mode-hook 'display-line-numbers-mode)

;; Use postgres as default .sql file type
(sql-set-product "postgres")

(sql-set-product-feature 'postgres :prompt-regexp "^[_[:alnum:]\-:]*=[#>] *")
(sql-set-product-feature 'postgres :prompt-cont-regexp  "^[_[:alnum:]\-:]*[-(][#>] *")

;; (setq sql-interactive-mode-hook nil)

(defun sql-postgres-connect-url (url)
  (interactive "sDB URL: ")
  (let ((parsed (url-generic-parse-url url)))
    (let ((sql-product   (url-type parsed)) ;; postgres
          (sql-user      "")
          (sql-password  "")
          (sql-server    "")
          (sql-database  url)
          (sql-port      0)
          (sql-postgres-login-params '()))
      (sql-product-interactive 'postgres (car (url-path-and-query parsed))))))

(defun sql-heroku-connect (app-name)
  (interactive "sHeroku App: ")
  (let ((sql-product 'postgres)
        (sql-user      "")
        (sql-password  "")
        (sql-server    "")
        (sql-database  "")
        (sql-port      0)
        (sql-postgres-login-params '())
        (sql-postgres-program "heroku")
        (sql-postgres-options (list "pg:psql" "-a" app-name)))
    (sql-product-interactive 'postgres app-name)))

;; use browser depending on url
(setq browse-url-browser-function
      '(("github\\.com" . browse-url-chromium)
        ("postgres\\.org" . eww-browse-url)
        ("." . browse-url-default-browser)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

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
