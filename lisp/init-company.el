(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(defvar my-company-select-by-number-p t
  "User can press number key to select company candidate.")

(with-eval-after-load 'company
  (setq company-idle-delay 0.1)
  (setq company-clang-insert-arguments nil)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 1)
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
  )

(require-package 'yasnippet)
(yas-global-mode 1)

(provide 'init-company)
