(require-package 'evil)
(evil-mode 1)

(require-package 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
(setq evil-escape-inhibit-functions '(evil-visual-state-p))

(require-package 'avy)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

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

(require-package 'counsel)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(require-package 'general)
(general-create-definer my-space-leader-def
  :prefix "SPC"
  :states '(normal visual))
(my-space-leader-def
 "fs" 'save-buffer)

(provide 'init-evil)
