(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-w" . evil-delete-backward-word)
              ("C-s" . counsel-company)
              )
  :config
  (defvar my-company-select-by-number-p t
    "User can press number key to select company candidate.")
  (defun my-company-number ()
    "Forward to `company-complete-number'.
               Unless the number is potentially part of the candidate.
               In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k))
           (n (if (equal k "0") 10 (string-to-number k))))
      (cond
       ((or (cl-find-if (lambda (s) (string-match re s)) company-candidates)
            (> n (length company-candidates))
            (looking-back "[0-9]" (line-beginning-position)))
        (self-insert-command 1))

       ((and (eq n 10) my-company-zero-key-for-filter)
        (company-filter-candidates))

       (t
        (company-complete-number n)))))
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'my-company-number))
     (number-sequence 0 9)))
  (mapc #'evil-declare-change-repeat
        '(my-company-number))
  ;; remove redundant candidates
  (add-to-list 'company-transformers #'delete-dups)
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0)
  (company-clang-insert-arguments nil)
  (company-show-numbers t)
  (company-minimum-prefix-length 1)
  (completion-ignore-case t))

;(use-package flx-ido
;  :ensure t
;  :hook (after-init . flx-ido-mode))

;(use-package company-fuzzy
;  :ensure t
;  :hook (company-mode . company-fuzzy-mode)
;  :init
;  (setq company-fuzzy-sorting-backend 'flx
;        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

(use-package company-ctags
  :ensure t
  :hook (company-mode . company-ctags-auto-setup)
  :custom
  (company-ctags-ignore-case t)
  (company-ctags-fuzzy-match-p nil))

;(use-package flycheck
;  :ensure t
;  :hook (after-init . global-flycheck-mode)
;  :config
;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;  (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
;  (setq flycheck-standard-error-navigation nil))

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package cuda-mode
             :ensure t)

(use-package eglot
  :ensure t
  ;:hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (advice-add 'jsonrpc--log-event :around
              (lambda (_orig-func &rest _)))
  (setq eglot-connect-timeout 120)
  (setq eldoc-echo-area-use-multiline-p 3
        eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode cuda-mode)
                                        . ("clangd"
                                           "-j=16"
                                           "--log=error"
                                           "--header-insertion=never"
                                           "--function-arg-placeholders=0")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'cuda-mode-hook 'eglot-ensure))

(use-package general
  :config
  (my-space-leader-def
    "ed" 'xref-find-definitions
    "er" 'xref-find-references
    "en" 'eglot-rename
    "ea" 'eglot-code-actions
    "ef" 'eglot-format
    "ecn" 'flymake-goto-next-error
    "ecp" 'flymake-goto-prev-error
    "eh" 'eldoc))

(use-package evil
  :ensure nil
  :config
  (evil-add-command-properties #'flymake-goto-next-error :jump t)
  (evil-add-command-properties #'flymake-goto-prev-error :jump t))

(use-package dtrt-indent
  :ensure t
  :hook (after-init . dtrt-indent-global-mode))

(provide 'init-company)
