(use-package avy
  :ensure t)

(use-package counsel-etags
  :ensure t
;  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(defvar my-initial-evil-state-setup
  '((minibuffer-inactive-mode . emacs)
    (grep-mode . emacs)
    (Info-mode . emacs)
    (term-mode . emacs)
    (anaconda-nav-mode . emacs)
    (log-edit-mode . emacs)
    (vc-log-edit-mode . emacs)
    (diff-mode . emacs)
    (neotree-mode . emacs)
    (w3m-mode . emacs)
    (gud-mode . emacs)
    (help-mode . emacs)
    (eshell-mode . emacs)
    (shell-mode . emacs)
    (vterm-mode . emacs)
    (xref--xref-buffer-mode . emacs)
    (epa-key-list-mode . emacs)
    (sr-mode . emacs)
    (profiler-report-mode . emacs)
    (dired-mode . emacs)
    (compilation-mode . emacs)
    (speedbar-mode . emacs)
    (ivy-occur-mode . emacs)
    (ivy-occur-grep-mode . normal)
    (messages-buffer-mode . normal)
    (js2-error-buffer-mode . emacs))
  "Default evil state per major mode.")
;; }}

(use-package evil
  :ensure t
  :after (counsel-etags)
  :init
  :hook (after-init . evil-mode)
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-visual-state-map
         ("C-u" . evil-scroll-up)
         :map evil-normal-state-map
         ("C-u" . evil-scroll-up)
         ("C-]" . counsel-etags-find-tag-at-point)
         ("s" . avy-goto-char-2))
  :config
  (evil-add-command-properties #'find-file-at-point :jump t)
  (evil-add-command-properties #'counsel-etags-list-tag :jump t)

  ;; {{ define my own text objects, works on evil v1.0.9 using older method
  ;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
  (defmacro my-evil-define-and-bind-text-object (key start-regex end-regex)
    (let* ((inner-name (make-symbol "inner-name"))
           (outer-name (make-symbol "outer-name")))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
  ;; between equal signs
  (my-evil-define-and-bind-text-object "=" "=" "=")
  ;; between pipe characters:
  (my-evil-define-and-bind-text-object "|" "|" "|")
  ;; regular expression
  (my-evil-define-and-bind-text-object "/" "/" "/")
  ;; trimmed line
  (my-evil-define-and-bind-text-object "l" "^ *" " *$")
  ;; angular template
  (my-evil-define-and-bind-text-object "r" "\{\{" "\}\}")
  ;; }}

  (dolist (p my-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))
  :custom
  ;; undo will never freeze my Emacs
  (evil-undo-system 'undo-redo)
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-fine-undo t))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (setq evil-escape-inhibit-functions '(evil-visual-state-p)))

(use-package evil-terminal-cursor-changer
  :ensure t
  :config
  (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (add-hook 'org-mode-hook (lambda ()
                             (push '(?\* . (" *" . "* ")) evil-surround-pairs-alist)
                             (push '(?\+ . (" +" . "+ ")) evil-surround-pairs-alist)
                             (push '(?\/ . (" /" . "/ ")) evil-surround-pairs-alist)
                             (push '(?\~ . (" ~" . "~ ")) evil-surround-pairs-alist)
                             (push '(?\= . (" =" . "= ")) evil-surround-pairs-alist)
                             (push '(?\$ . (" $" . "$ ")) evil-surround-pairs-alist)
                             (push '(?\_ . (" _" . "_ ")) evil-surround-pairs-alist))))

(use-package counsel
  :ensure t
  :hook (after-init . ivy-mode)
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x g" . counsel-git)
         ("C-x j" . counsel-git-grep)
         ("C-x k" . counsel-rg)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

(use-package general
  :ensure t
  :config
  (general-create-definer my-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (my-space-leader-def
    "fs" 'save-buffer
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wo" 'delete-other-windows
    "wd" 'delete-window
    "wc" 'delete-window
    "bb" 'ivy-switch-buffer
    "t" 'counsel-etags-list-tag
    "fg" 'counsel-git
    "s" 'swiper-thing-at-point
    "g" 'counsel-rg
    "rr" 'revert-buffer-quick))

(provide 'init-evil)
