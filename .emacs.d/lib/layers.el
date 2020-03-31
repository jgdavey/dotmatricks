(defun jd/default-layers (base-dir)
  (delq nil
        (delete-dups
         (mapcar 'file-name-base
                 (directory-files base-dir nil "^[^\\.].*\\.elc?$")))))


(defconst jd/layer-path "~/.emacs.d/layers")

(defcustom jd/layers (jd/default-layers jd/layer-path)
  "List of layers to load")

;; (setq jd/layers (jd/default-layers "~/.emacs.d/layers"))

;; (setq jd/layers '("custom" "clojure" "editing"))

(load-file-if-exists "~/.emacs.d/local-layers.el")

(mapc (lambda (layer)
        (load
         (expand-file-name layer jd/layer-path))) jd/layers)

(provide 'layers)
