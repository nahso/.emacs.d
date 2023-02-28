(use-package color-theme-approximate
  :ensure t
  :config
  (color-theme-approximate-on))

(use-package modus-themes
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-nil)
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-icon nil)
  (doom-modeline-unicode-fallback nil)
  (doom-modeline-enable-word-count nil))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-enable-variable-value-editing t))

(use-package highlight-indent-guides
  :ensure t
  :hook (python-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(use-package tree-sitter
  :ensure t
  :init
  (setq tsc-dyn-get-from '(:compilation))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)
(defun load-treesitter()
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2")
  (setq site-lisp-path (expand-file-name "site-lisp/" user-emacs-directory))
  (add-to-list 'load-path (concat site-lisp-path "elisp-tree-sitter/core"))
  (add-to-list 'load-path (concat site-lisp-path "elisp-tree-sitter/lisp"))
  (add-to-list 'load-path (concat site-lisp-path "elisp-tree-sitter/langs"))
  (require 'tree-sitter)
  (require 'tree-sitter-hl)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query))
(add-hook 'after-init-hook 'load-treesitter)

(provide 'init-ui)
