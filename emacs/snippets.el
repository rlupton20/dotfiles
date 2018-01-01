;;; MODE CONFIGURATIONS

;; R :: editing modes and configuration for R
(use-package ess
  :defer
  :ensure t)


;;; JavaScript :: editing mode and extensions for JavaScript
(use-package js3-mode
  :ensure t
  :defer
  :config
  (setq-default js3-indent-level 4)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js3-mode)
  (add-hook 'js3-mode 'smart-parens-mode))

;; use tern to provide backend function
(use-package tern
  :ensure t
  :defer
  :config
  (add-hook 'js3-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure t
  :defer
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js3-mode-hook 'company-mode))


;;; TypeScript :: editing mode for TypeScript
(use-package tide
  :ensure t
  :defer
  :config
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'tide-mode)
  (add-hook 'typescript-mode-hook 'tide-setup))


;;; IO :: mode for editing Io
(use-package io-mode
  :defer
  :ensure t)


;;; ELM :: editing modes and configuration for elm
(use-package elm-mode
  :ensure t
  :after company
  :defer
  :config
  (add-to-list 'company-backends 'company-elm)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(use-package flycheck-elm
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))



;;; EXPERIMENTS

;;; VIM-LATEX like key chords

;; JUMP TO <++> :: Handy for snippets of various kinds
(defun insert/jump-to-marker (&optional backwards?)
  "Jump to nearest <++> marker and replace it.  If BACKWARDS? then jump backwards."
  (interactive)
  (if backwards?
      (progn
	(search-backward "<++>")
	(delete-char 4))
    (progn
      (search-forward "<++>")
      (delete-char (- 4)))))

(define-key evil-insert-state-map (kbd "C-j")
  'insert/jump-to-marker)

(defun shitty-latex-macro ()
  (interactive)
  (let ((p (point)))
    (insert "$<++>$ <++>")
    (goto-char p)
    (insert/jump-to-marker)))

(key-chord-define evil-insert-state-map "$$"
		  'shitty-latex-macro)
