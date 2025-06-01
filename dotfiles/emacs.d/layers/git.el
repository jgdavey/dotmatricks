(use-package transient
  :ensure t
  :pin melpa)

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

(use-package magit-section
  :ensure t
  :pin melpa)

(use-package treemacs-magit
  :pin melpa
  :after (treemacs magit)
  :ensure t)

;; (use-package magit-popup
;;   :ensure t)

(use-package git-link
  :ensure t)
