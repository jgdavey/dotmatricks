;; Silence compiler warnings
(require 'sql)

(defun jd/sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (toggle-truncate-lines t)
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n")
      (comint-send-string proc "\\pset pager off\n"))))

(add-hook 'sql-interactive-mode-hook 'jd/sql-interactive-mode-hook)
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
