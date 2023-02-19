(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package general
  :config
  (my-space-leader-def
    "xg" 'magit-status))

(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

(provide 'init-git)
