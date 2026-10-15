;;; -*- lexical-binding: t; -*-

(use-package inf-ruby
  :ensure t
  :config
  (defun inf-ruby-console-pry-rails (dir)
    "Run Rails console in DIR."
    (interactive (list (inf-ruby-console-read-directory 'rails)))
    (let* ((default-directory (file-name-as-directory dir))
           (env (inf-ruby-console-rails-env))
           (process-environment (cons (format "RAILS_ENV=%s" env)
                                    process-environment))
           (with-bundler (file-exists-p "Gemfile")))
      (inf-ruby-console-run
       (concat (when with-bundler "bundle exec ")
               "pry -r ./config/environment.rb")
       "pry-rails")))
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

;; ruby-mode files open in ruby-ts-mode (see `treesit-enabled-modes')
(use-package ruby-mode)

