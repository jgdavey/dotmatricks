;; Turn off useless UI elements -- do this as early as possible to
;; avoid visual thrashing.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")

(defun jd/setup-frame (frame)
  (if (display-graphic-p frame)
      ;; GUI window mode
      (with-selected-frame frame
        (if (fboundp 'menu-bar-mode) (menu-bar-mode +1))
        (set-frame-size frame 187 56))
    ;; terminal mode
    (with-selected-frame frame
      (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
      (require 'mouse)
      (xterm-mouse-mode t)
      (setq mouse-sel-mode t)
      (defun track-mouse (e))

      (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
      (global-set-key (kbd "<mouse-5>") 'scroll-up-line))))

(add-hook 'after-make-frame-functions #'jd/setup-frame)

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

(defun load-file-if-exists (file)
  (if (file-exists-p file)
      (load-file file)
    nil))

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

(defvar base-packages '(color-theme-sanityinc-tomorrow
                        use-package)
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

(add-to-list 'load-path "~/.emacs.d/lib")

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package expand-region
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

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line-function
        (lambda () (format " [%s]" (projectile-project-name))))
  (setq projectile-completion-system 'ivy))

(use-package projectile-ripgrep
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

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

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         :map isearch-mode-map
         ("C-c s" . swiper-isearch-toggle)
         :map swiper-map
         ("C-c s" . swiper-isearch-toggle)))

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

(use-package winner
  :ensure t
  :config
  (winner-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :diminish which-key-mode)

(setq browse-url-default-browser
      (if (eq system-type 'darwin)
          'browse-url-default-macosx-browser
        'browse-url-chrome))

;; use browser depending on url
(setq browse-url-browser-function
      '(("github\\.com" . browse-url-default-browser)
        ("postgres\\.org" . eww-browse-url)
        ("." . browse-url-default-browser)))

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

(defun delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (progn
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))

;; load all (or configured) files in ~/.emacs.d/layers
(require 'layers)

;; additional (local) config
(dolist (extra-file '("~/.emacs.d/default.el" "~/.emacs.d/local.el"))
  (load-file-if-exists extra-file))

;; set custom file
(let ((custom "~/.emacs.d/custom.el"))
  (setq custom-file custom)
  (load-file-if-exists custom))
