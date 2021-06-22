(use-package org
  :ensure t
  :bind (("C-c c" . counsel-projectile-org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c C-o" . jd/org-open-at-point))
  :pin org
  :init
  (use-package ob-http
    :ensure t)
  (use-package ox-gfm
    :ensure t
    :init
    (eval-after-load "org"
      '(require 'ox-gfm nil t)))
  (use-package ob-sql-mode
    :ensure t
    :config
    ;; Override so that session can link to existing SQL sessions
    (defun org-babel-sql-mode--buffer-name (params)
      (format "%s" (cdr (assoc :session params))))
    ;; This plugin adds an invalid entry
    (custom-reevaluate-setting 'org-structure-template-alist))

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
  (setq org-log-done t)
  (setq org-refile-use-outline-path 'file)
  ;;(setq org-agenda-todo-list-sublevels nil)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq counsel-projectile-org-capture-templates
        `(("p" "[${name}] Task" entry
           (file+headline "${root}/notes.org" "Tasks")
           "* TODO %?\n  %u\n  %a\n  %i")))
  (setq org-capture-templates
        `(("t" "Todo" entry "todo.org"
           "* TODO %?\n  %i")
          ("n" "Pending Todo (waiting on something else)" entry "todo.org"
           "* PENDING %?\n  %i")
          ("l" "Linked Todo" entry "todo.org"
           "* TODO %?\n  %i\n  %A")
          ("s" "Scheduled Todo" entry "todo.org"
           "* TODO %?\n  SCHEDULED: %^t\n  %i\n  %a\n")))
  (setq org-todo-keywords
        '((sequence "PENDING" "TODO" "|" "DONE" "DELEGATED")))
  :config
  (when (version<= "9.2.0" (org-version))
    (add-to-list 'org-modules 'org-tempo))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (shell . t)
                                 (http . t)
                                 (ruby . t)
                                 (emacs-lisp . t)
                                 (elasticsearch . t)))
  (add-to-list 'org-agenda-files org-directory 'append)

  (require 'ob-sql)
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass{letter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (defun jd/toggle-org-pdf-export-on-save ()
    "Enable or disable export PDF when saving current buffer."
    (interactive)
    (when (not (eq major-mode 'org-mode))
      (error "Not an org-mode file!"))
    (if (memq 'org-html-export-to-html after-save-hook)
        (progn (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
               (message "Disabled org html export on save"))
      (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
      (set-buffer-modified-p t)
      (message "Enabled org PDF export on save")))

  (defun jd/org-open-at-point (&optional arg)
    "Wrapper for to use eww instead of default browser. Use prefix to get default behavior"
    (interactive "P")
    (if arg
        (let ((browse-url-browser-function 'eww-browse-url))
          (org-open-at-point))
      (org-open-at-point))))
