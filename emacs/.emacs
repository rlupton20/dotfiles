;;; Custom emacs configuration.

;;; First we set up the emacs package repository, and require
;;; emacs to use it.

;; We would like to be able to install packages from MELPA
;; so lets include the repository
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;;; USEPACKAGE :: Check usepackage is installed, and install it
;;; otherwise. Usepackage is then used to check other package
;;; files are installed, and to configure them.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; Now we make some adjustments to the basic appearance of
;;; emacs. For example, we load themes, and remove unwanted
;;; toolbars.

;; BASIC :: Alter appearances
(tool-bar-mode -1)    ; Turn off tool bar in X mode
(menu-bar-mode -1)    ; Turn off the menu bar
(scroll-bar-mode -1)  ; Remove the scrollbar
(visual-line-mode 1)  ; Use visual line mode to wrap lines nicely
(setq-default show-trailing-whitespace t) ; Show trailing whitespace by default

;;; FIXES :: For things which don't behave quite right

;; projectile tries to use the local shel in TRAMP mode
;; which is a pain on systems which have a different path for
;; the shell can cause an error (shell can't be found).
;; Since most systems symlink sh to /bin/bash or the default
;; shell anyway, we may as well just use sh
(setq shell-file-name "sh")

;; EMACS SPECIFIC :: more specific emacs customization
(put 'narrow-to-region 'disabled nil)


;; LINUM RELATIVE MODE :: use relative line numbering
(use-package linum-relative
  :ensure t
  :config (linum-relative-global-mode))


;; THEME :: Load the monokai theme
(use-package monokai-theme
  :ensure t
  :config
  (setq monokai-bg "#101010")
  (setq monokai-highlight-line "#000000")
  (load-theme 'monokai t))


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

;; FUZZY :: Fuzzy lets us use fuzzy matching, which is useful
;; with packages like auto-complete.
(use-package fuzzy
  :ensure t)

;; AUTO-COMPLETE
;;(use-package auto-complete
;;  :ensure t
;;  :bind (("H-p" . auto-complete))
;;  :config (require 'auto-complete-config)
;;          (ac-config-default)
;;          (setq ac-use-fuzzy t))

;; COMPANY-MODE :: autocompletion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1))

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

;; FLYCHECK :: On the fly syntax checking
(use-package flycheck
  :ensure t
  :config (setq flycheck-command-wrapper-function
		  (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
		flycheck-executable-find
		  (lambda (command) (nix-executable-find (nix-current-sandbox) command)))
          (global-flycheck-mode))

(use-package multiple-cursors
  :ensure t)

;;; Now we configure packages for individual editing modes.


;; NIX :: Specific stuff for working in nix
(use-package nixos-options
  :ensure t)

(use-package nix-sandbox
  :ensure t)

;; NIX MODE :: for editing nix files and nix expressions
(use-package nix-mode
  :ensure t)

;; YAML :: Add YAML mode and configure
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml$" . yaml-mode))


;; Do some Haskell specific configuration.

;; HASKELL :: haskell-mode

;; Haskell modes use various packages installed by cabal,
;; so we need to add cabal's bin directory to our path
(defun add-cabal-path ()
  (let ((cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path cabal-path)))


;; Make it possible to launch ghci instances from emacs
(use-package haskell-mode
  :ensure t
  :config (add-cabal-path)
          (require 'haskell-interactive-mode)
          (require 'haskell-process)
	  ;; First lets make haskell-mode able to use the nix environment
	  (setq haskell-process-wrapper-function
		(lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	  ;; Add hindent and make it load automaticall with haskell-mode
	  (use-package hindent
	    :ensure t
	    :config (add-hook 'haskell-mode-hook #'hindent-mode))
	  ;; Add package ghc to work with ghc-mod
	  ;; (use-package ghc
	  ;;  :ensure t
	  ;;  :config (autoload 'ghc-init "ghc" nil t)
	   ;;         (autoload 'ghc-debug "ghc" nil t)
	;;    (add-hook 'haskell-mode-hook (lambda () (ghc-init))))
	  (use-package flycheck-haskell
	    :ensure t
	    :config (eval-after-load 'flycheck
		      '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))
	  ;; Add jump to imports shortcut
	  (eval-after-load 'haskell-mode
	    '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
	  ;; Now we set up haskell-mode in the way which we like
	  (custom-set-variables '(haskell-tags-on-save t)  ; hasktags
				'(haskell-process-suggest-remove-import-lines t)
				'(haskell-process-auto-import-loaded-modules t)
				'(haskell-process-log t)
				'(haskell-process-type 'stack-ghci)))

;; PYTHON :: elpy for editing Python
(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; RUST :: emacs configuration for editing rust
(use-package rust-mode
  :ensure t
  :config
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode))
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (use-package racer
    :ensure t
    :config
    (setq racer-cmd "racer")
    (setq racer-rust-src-path "~/.rust/src")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
  
;;; R :: editing modes and configuration for R
(use-package ess
  :ensure t)

;;; JavaScript :: editing mode and extensions for JavaScript
(use-package js3-mode
  :ensure t)

;;; TypeScript :: editing mode for TypeScript
(use-package tide
  :ensure t)


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

;;(use-package evil-tabs
;;  :ensure t
;;  :config (global-evil-tabs-mode t))


;; Fix keymaps
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)


;;; OTHER NICE THINGS :: Other things that are nice to have

;; Structure and Interpretation of Computer Programs
(use-package sicp
  :ensure t)
