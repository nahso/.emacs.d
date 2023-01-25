(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; `lsp-mode' benefits from that.
(setq read-process-output-max (* 4 1024 1024))

(require 'package)
(setq package-archives
      '(("melpa"  . "http://1.15.88.122/melpa/")
        ("gnu"    . "http://1.15.88.122/gnu/")
        ("nongnu" . "http://1.15.88.122/nongnu/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-base)
(require 'init-ui)
(require 'init-evil)
(require 'init-company)
(require 'init-org)
(require 'init-git)

(load custom-file 'no-error 'no-message)

(provide 'init)
