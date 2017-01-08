;;; Custom emacs configuration.

;;; First we set up the emacs package repository, and require
;;; emacs to use it.

;; We would like to be able to install packages from MELPA
;; so lets include the repository
(require 'package)
(package-initialize)


;;; Now we make some adjustments to the basic appearance of
;;; emacs. For example, we load themes, and remove unwanted
;;; toolbars.

;; BASIC :: Alter appearances
(tool-bar-mode -1)    ; Turn off tool bar in X mode
(menu-bar-mode -1)    ; Turn off the menu bar
(scroll-bar-mode -1)  ; Remove the scrollbar
(global-visual-line-mode 1)  ; Use visual line mode to wrap lines nicely
(setq show-trailing-whitespace t)


;;; FIXES :: For things which don't behave quite right

;; projectile tries to use the local shel in TRAMP mode
;; which is a pain on systems which have a different path for
;; the shell can cause an error (shell can't be found).
;; Since most systems symlink sh to /bin/bash or the default
;; shell anyway, we may as well just use sh
(setq shell-file-name "sh")

;; EMACS SPECIFIC :: more specific emacs customization
(put 'narrow-to-region 'disabled nil)

;; THEME :: Load the monokai theme
(use-package monokai-theme
  :ensure t
  :config
  (setq monokai-background "#101010")
  (setq monokai-highlight-line "#000000")
  (load-theme 'monokai t))


;; LINUM RELATIVE MODE :: use relative line numbering
(use-package linum-relative
  :ensure t
  :config (global-linum-mode))


;; HELM :: Use helm in places where it is useful
(use-package helm
  :ensure t
  :bind ( ;; First using helm for M-x so we get a live filter
          ;; of options, and don't need to keep tab completing.
          ("M-x" . helm-M-x)
          ;; Also use helm for buffers. I can never remember the
          ;; buffers I have open.
          ("C-x C-b" . helm-buffers-list)
          ;; Finding files can also be a pain, so use helm
          ;; to locate and open files
          ("C-x C-f" . helm-find-files)))


;;; Odd tweaks for general behaviour

;; BUFFER BEHAVIOUR :: handy function from wikipedia, that
;; makes new windows automatically load the next buffer
;; rather than the same one as is already open.

(defun split-vertical-to-next-buffer ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun split-horizontal-to-next-buffer ()
  (interactive)
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key (kbd "C-x 2") 'split-vertical-to-next-buffer)
(global-set-key (kbd "C-x 3") 'split-horizontal-to-next-buffer)

;; Now some function I wrote to switch buffers around
;; between different windows.
(defun switch-buffer-next ()
  (interactive)
  (let ((current (window-buffer)))
  (set-window-buffer nil
		     (window-buffer (next-window)))
  (set-window-buffer (next-window) current)))

(defun switch-buffer-previous ()
  (interactive)
  (let ((current (window-buffer)))
  (set-window-buffer nil
		     (window-buffer (previous-window)))
  (set-window-buffer (previous-window) current)))

(global-set-key (kbd "s->") 'switch-buffer-next)
(global-set-key (kbd "s-<") 'switch-buffer-previous)

;; Add a keybinding to F5 to refresh the current buffer (from the file
;; on the disk
(global-set-key (kbd "<f5>")
		(lambda ()
		  (interactive)
		  (revert-buffer t t)
		  (message (concat "Refreshed buffer from " (buffer-file-name)))))

;; Sometimes its useful to revert all open buffers to that on the disk
(defun revert-all-buffers ()
  "Refreshes all buffers from the files on disk"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files"))

(global-set-key (kbd "<S-f5>") 'revert-all-buffers)



;;; There are some packages which are useful across a range of
;;; modes. We configure them here.

;; AVY :: Create some keybindings for avy
(use-package avy
  :ensure t
  :bind (("H-f" . avy-goto-word-1)))

;; COMPANY-MODE :: autocompletion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; FLYCHECK :: On the fly syntax checking
(use-package flycheck
  :ensure t
  :config (setq flycheck-command-wrapper-function
		  (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
		flycheck-executable-find
		  (lambda (command) (nix-executable-find (nix-current-sandbox) command)))
          (global-flycheck-mode))

;; YASNIPPET :: Use Yasnippet everywhere
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; MAGIT :: Magit is a wrapper for git
(use-package magit
  :ensure t
  :bind (("H-g" . magit-status)))

;; PROJECTILE :: Projectile helps with project management
(use-package projectile
  :ensure t
  :config (projectile-global-mode)
          (use-package helm-projectile
            :ensure t)
          (helm-projectile-on))

;; EMAMUX :: control tmux sessions from emacs
(use-package emamux
  :ensure t)

;; NIX :: Stuff for working with nix
(use-package nixos-options
  :ensure t)

(use-package nix-sandbox
  :ensure t)

;; NIX MODE :: for editing nix files and nix expressions
(use-package nix-mode
  :ensure t)

;; Configure some packages to use with particular kinds of files

;; YAML :: Add YAML mode and configure
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml$" . yaml-mode))

;; HASKELL :: Modes for working with Haskell
(use-package haskell-mode
  :ensure t
  :config 
  (custom-set-variables '(haskell-tags-on-save t)))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(add-hook 'haskell-mode-hook 'subword-mode)


;; RUST :: emacs configuration for editing rust

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package cargo
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :after rust-mode
  :config
  (setq racer-cmd "racer")
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Some nice things inspired by vim

;;; SPACELINE :: Use spaceline for an emacs powerline
(use-package spaceline
  :ensure t
  :config (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-height 25)
  (setq powerline-default-separator 'arrow))


(use-package evil-leader
  :ensure t
  :config (global-evil-leader-mode)
   (evil-leader/set-leader ",")
   (evil-leader/set-key
   "x" 'helm-M-x
   "f" 'helm-find-files
   "p" 'helm-projectile-find-file
   "g" 'magit-status))


;; Allow evil mode to be used if preferred
(use-package evil
  :ensure t
  :config (evil-mode t))

;; Fix keymaps
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)


;;; OTHER NICE THINGS :: Other things that are nice to have

;; Structure and Interpretation of Computer Programs
(use-package sicp
  :ensure t)
