(use-package markdown-mode
  :ensure t)

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

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(when (executable-find "prose")
  (defun jd/prose-fill-region (begin end)
    "Use `prose' executable to fill region between BEGIN and END."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (let* ((width-str (number-to-string fill-column))
           (err-buf "*prose Error*")
           (existed (if (get-buffer err-buf)
                        (kill-buffer err-buf)))
           (prose-cmd (concat "prose -w" width-str " "))
           (before-text (buffer-substring-no-properties begin end))
           prose-ret
           ;; Do the formatting in a temp buffer so that the text in the original
           ;; buffer doesn't get corrupted in case `prose' fails due to some error.
           (after-text (with-temp-buffer
                         (insert before-text)
                         (setq prose-ret (shell-command-on-region
                                          (point-min) (point-max)
                                          prose-cmd nil :replace
                                          err-buf :display-error-buffer))
                         (buffer-substring-no-properties (point-min) (point-max)))))
      (if (get-buffer err-buf)
          (save-excursion
            (switch-to-buffer-other-window err-buf)
            (special-mode))) ; Set this mode so that you can quit it quickly using C-u q
      ;; If 1 is returned, error occurred in the cmd execution; 0 - no error
      (if (= 1 prose-ret)
          nil
        ;; If no error occurred, do below in the original buffer
        (delete-region begin end)
        (insert after-text))
      (message "Executed `%s' on the region" prose-cmd)))
  ;; Rationale for the below binding is that it flows very well when selecting a
  ;; paragraph and then filling that region: "M-h M-H"
  (global-set-key (kbd "M-H") #'jd/prose-fill-region))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))
