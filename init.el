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
;(require 'init-frame-hooks)
;(require 'init-theme)
;(require 'init-gui-frames)

(require 'init-evil)
(require 'init-company)

(require-package 'cuda-mode)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-dark))
 '(package-selected-packages
   '(cuda-mode company saveplace flycheck powerline-evil general counsel avy evil-escape evil color-theme-approximate disable-mouse default-text-scale spacemacs-theme command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq powerline ivy evil-snipe color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
