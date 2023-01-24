(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-ui)
(require 'init-evil)
(require 'init-company)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(provide 'init)
