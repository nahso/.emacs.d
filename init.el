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

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(provide 'init)
