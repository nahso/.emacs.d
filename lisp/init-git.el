(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

(provide 'init-git)
