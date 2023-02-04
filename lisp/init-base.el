;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-underline-at-descent-line t)

;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
;; for the key passphrase.
(setq epg-pinentry-mode 'loopback)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; auto pair
(electric-pair-mode 1)

;; Line number
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(global-hl-line-mode 1)

;; Disable blink
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Font size
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (set-face-attribute 'default nil :height 135))

;; Sane defaults
(setq use-short-answers t)

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

;; Use TeX as default IM
(setq default-input-method "TeX")

;; Keep clean but enable `menu-bar' in MacOS
(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Show line/column number and more
(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  ;; No visual feedback on copy/delete.
  (copy-region-blink-delay 0)
  (delete-pair-blink-delay 0)
  ;; confusing if no fringes (GUI only).
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; don't save current clipboard text before replacing it
  (save-interprogram-paste-before-kill nil)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n' when point starts at the beginning-of-line
  (kill-whole-line t)
  ;; show cwd when `shell-command' and `async-shell-command'
  (shell-command-prompt-show-cwd t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t)
  ;; List only applicable commands.
  ;;
  ;; ``` elisp
  ;; (defun foo ()
  ;;   (interactive nil org-mode)
  ;;   (message "foo"))
  ;; ```
  ;;
  ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end-double-space nil))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Highlight current line in GUI
(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init . global-hl-line-mode))

;; Enable `repeat-mode' to reduce key sequence length
;;
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
(use-package repeat
  :ensure nil
  :custom
  (repeat-mode t)
  (repeat-exit-timeout 3)
  (repeat-exit-key (kbd "RET")))

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; transparent remote access
(use-package tramp
  :ensure nil
  :defer t
  :custom
  ;; Always use file cache when using tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-default-method "ssh"))

;; Command line interpreter
(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
              ([remap kill-region]   . backward-kill-word))
  :custom
  ;; Make the prompt of "*Python*" buffer readonly
  (comint-prompt-read-only t)
  (comint-history-isearch 'dwim)
  ;; Colorize
  (comint-terminfo-terminal "dumb-emacs-ansi"))

;; Better abbrev expansion
(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (defun try-expand-tempo (_old)
    (require 'tempo)
    (tempo-expand-if-complete))
  :custom
  (hippie-expand-try-functions-list '(try-expand-tempo
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

;; Buffer manager
;;
;; `sR': switch to saved filter groups
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("Browser" (or (mode . eww-mode)
                     (mode . xwidget-webkit-mode)))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Repl" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Term" (or (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Mail" (or (mode . mail-mode)
                  (mode . message-mode)
                  (derived-mode . gnus-mode)))
      ("Conf" (or (mode . yaml-mode)
                  (mode . conf-mode)))
      ("Dict" (or (mode . fanyi-mode)
                  (mode . dictionary-mode)))
      ("Text" (and (derived-mode . text-mode)
                   (not (starred-name))))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)
                   (mode . vc-annotate-mode)))
      ("VC" (or (mode . diff-mode)
                (derived-mode . log-view-mode)))
      ("Prog" (and (derived-mode . prog-mode)
                   (not (starred-name))))
      ("Dired" (mode . dired-mode))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))))))

;; Recently opened files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(;; Folders on MacOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on MacOS end
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

;; GC optimization
(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000)) ;; 100 MB

;; Writable grep buffer
(use-package wgrep
  :ensure t
  :hook (grep-setup . wgrep-setup)
  :custom
  (wgrep-change-readonly-file t))

(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save)
  :hook (after-init . global-clipetty-mode))

;(defun begin-of-current-block()
;  (interactive)
;  (line-move -1 nil nil t)
;  (while (or (eq 32 (following-char))
;             (eq 10 (following-char)))
;    (line-move -1 nil nil t)))

(provide 'init-base)
