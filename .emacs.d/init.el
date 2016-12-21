;; Packages
(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cider
                      clj-refactor
                      company
                      exec-path-from-shell
                      gist
                      magit
                      monokai-theme
                      org
                      paredit
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
(menu-bar-mode -1)

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
  (global-set-key (kbd "C-x g") 'magit-status))

(with-library cider
  (setq cider-prompt-for-symbol nil))

(with-library clj-refactor
  (defun my-clj-refactor-mode-hook ()
      (clj-refactor-mode 1)
      (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clj-refactor-mode-hook))

(with-library ido
  (setq ido-everywhere t
        ido-enable-flex-matching t)
  (ido-mode t))

(with-library paredit
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(let ((local "~/.emacs.d/default.el"))
  (if (file-exists-p local)
    (load-file local)
    nil))
