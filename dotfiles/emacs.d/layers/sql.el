;; Silence compiler warnings
(require 'sql)

(defun jd/sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (toggle-truncate-lines t)
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      (comint-send-string proc "\\x auto\n"))))

(add-hook 'sql-interactive-mode-hook 'jd/sql-interactive-mode-hook)
;; (add-hook 'sql-mode-hook 'display-line-numbers-mode)

;; Use postgres as default .sql file type
(sql-set-product "postgres")

(sql-set-product-feature 'postgres :prompt-regexp "^[_[:alnum:]\\-:]*=\\*?[#>] *")
(sql-set-product-feature 'postgres :prompt-cont-regexp  "^[_[:alnum:]\\-:]*[-(]\\*?[#>] *")


;; psql -X --pset=null='ø' --set=ON_ERROR_ROLLBACK=interactive
(setq sql-postgres-options
      (append sql-postgres-options '("-X"
                                     "-e"
                                     "--pset" "null=ø"
                                     "--set" "ON_ERROR_ROLLBACK=interactive")))

(defun sql-postgres-connect-url (url)
  (interactive "sDB URL: ")
  (let ((parsed (url-generic-parse-url url)))
    (let ((sql-product   (url-type parsed)) ;; postgres
          (sql-user      "")
          (sql-password  "")
          (sql-server    "")
          (sql-database  url)
          (sql-port      0) ;; (or (url-portspec parsed) 0)
          (sql-postgres-login-params '()))
      (sql-product-interactive 'postgres (car (url-path-and-query parsed))))))

(defun sql-postgres-connect-sshx (host dbname)
  (interactive "sSSH: \nsDatabase: ")
  (let ((default-directory (format "/sshx:%s:" host)))
    (let ((sql-product  'postgres) ;; postgres
          (sql-user     "")
          (sql-password "")
          (sql-server   "")
          (sql-database dbname)
          (sql-port     0)
          (sql-postgres-login-params '()))
      (sql-product-interactive 'postgres (format "%s:%s" host dbname)))))

(defun sql-heroku-connect (app-name)
  (interactive "sHeroku App: ")
  (let* ((s (split-string app-name ":+"))
         (app (car s))
         (db (cadr s))
         (progopts (seq-filter 'stringp (list "pg:psql" db "-a" app))))
    (let ((sql-product 'postgres)
          (sql-user      "")
          (sql-password  "")
          (sql-server    "")
          (sql-database  "")
          (sql-port      0)
          (sql-postgres-login-params '())
          (sql-postgres-program "heroku")
          (sql-postgres-options progopts))
      (sql-product-interactive 'postgres app-name))))

(when (executable-find "pg_format")
  (defun jd/reformat-sql (begin end)
    "Use `pg_format' executable to reformat sql between BEGIN and END."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (let* ((width-str (number-to-string fill-column))
           (err-buf "*pg_format Error*")
           (existed (if (get-buffer err-buf)
                        (kill-buffer err-buf)))
           (pg-format-cmd (concat "pg_format"
                                  " -u 0 -U 0"
                                  " -W1"
                                  " -s2"
                                  " -C "))
           (before-text (buffer-substring-no-properties begin end))
           pg-format-ret
           (after-text (with-temp-buffer
                         (insert before-text)
                         (setq pg-format-ret (shell-command-on-region
                                          (point-min) (point-max)
                                          pg-format-cmd nil :replace
                                          err-buf :display-error-buffer))
                         (buffer-substring-no-properties (point-min) (point-max)))))
      (if (get-buffer err-buf)
          (save-excursion
            (switch-to-buffer-other-window err-buf)
            (special-mode))) ; Set this mode so that you can quit it quickly using C-u q
      ;; If 1 is returned, error occurred in the cmd execution; 0 - no error
      (if (= 1 pg-format-ret)
          nil
        ;; If no error occurred, do below in the original buffer
        (delete-region begin end)
        (insert after-text))
      (message "Executed `%s' on the region" pg-format-cmd))))
