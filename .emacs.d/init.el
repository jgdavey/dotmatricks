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

;; Packages
(require 'package)
(require 'cl)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/"))
      package-archive-priorities
      '(("org" . 20)
        ("melpa-stable" . 10)
        ("gnu" . 5)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ag
                      browse-kill-ring
                      cider
                      clj-refactor
                      clojure-mode
                      company
                      deft
                      diminish
                      exec-path-from-shell
                      expand-region
                      gist
                      git-link
                      inf-ruby
                      json-mode
                      magit
                      markdown-mode
                      monokai-theme
                      multiple-cursors
                      org-plus-contrib
                      paredit
                      wgrep-ag
                      zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)

     (error (message (format "I guess we don't have %s available." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)

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

(setq whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80)
(add-hook 'clojure-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)

(load-theme 'zenburn t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

(with-library magit
  (global-set-key (kbd "C-c g") 'magit-status)
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges"))

(with-library cider
  (setq org-babel-clojure-backend 'cider)
  (setq cider-prompt-for-symbol nil)
  (setq cider-mode-line-show-connection nil)
  (setq cider-repl-display-help-banner nil))

(with-library clj-refactor
  (defun my-clj-refactor-mode-hook ()
      (clj-refactor-mode 1)
      (cljr-add-keybindings-with-prefix "C-c C-m"))
  (setq cljr-favor-prefix-notation nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-auto-sort-ns nil
        cljr-favor-private-functions nil)
  (add-hook 'clojure-mode-hook #'my-clj-refactor-mode-hook))

(with-library ido
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

(with-library paredit
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(with-library uniquify
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator ":"))

(with-library multiple-cursors
  (global-set-key (kbd "C-c C-l") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

(with-library company
  (global-company-mode 1))

(with-library deft
  (global-set-key (kbd "C-c d") 'deft)
  (setq deft-extensions '("org" "md")
        deft-default-extension "org"
        deft-directory "~/notes"
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 0))

(with-library org
  (require 'ob-clojure)
  (require 'ob-ruby)
  (setq org-babel-clojure-backend 'cider)
  ;; Here I specify the languages I want to be able to use with Org-babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (sh . t)
     (emacs-lisp . t)))

  ;; Let's have pretty source code blocks
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-support-shift-select 'always))

(with-library diminish
  (diminish 'company-mode "comp")
  (diminish 'paredit-mode))

(with-library wgrep
  (autoload 'wgrep-ag-setup "wgrep-ag"))

(with-library ag
  (setq ag-highlight-search t)
  (global-set-key (kbd "C-c a") 'ag-project)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

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

(let ((local "~/.emacs.d/default.el"))
  (if (file-exists-p local)
    (load-file local)
    nil))

;; set custom file
(let ((custom "~/.emacs.d/custom.el"))
  (setq custom-file custom)
  (if (file-exists-p custom)
      (load-file custom)
    nil))
