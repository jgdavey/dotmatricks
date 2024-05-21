(defun jd/default-layers (base-dir)
  (delq nil
        (delete-dups
         (mapcar 'file-name-base
                 (directory-files base-dir nil "^[^\\.].*\\.elc?$")))))

(defconst jd/layer-path
  (expand-file-name "layers" user-emacs-directory))

(defcustom jd/layers (jd/default-layers jd/layer-path)
  "List of layers to load")

;; (setq jd/layers (jd/default-layers "~/.emacs.d/layers"))

;; (setq jd/layers '("custom" "clojure" "editing"))

(defun jd/load-file-if-exists (file)
  (if (file-exists-p file)
      (load-file file)
    nil))

(jd/load-file-if-exists
 (expand-file-name "local-layers.el" user-emacs-directory))

(mapc (lambda (layer)
        (load
         (expand-file-name layer jd/layer-path)))
      jd/layers)

(provide 'layers)
