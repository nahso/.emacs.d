(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-theme)
(require 'init-gui-frames)

(require 'init-evil)

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
   '(general counsel avy evil-escape evil disable-mouse default-text-scale spacemacs-theme command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq ivy evil-snipe color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
