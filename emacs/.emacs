;;; Custom emacs configuration.


;;; First we set up the emacs package repository, and require
;;; emacs to use it.

;; We would like to be able to install packages from MELPA
;; so lets include the repository
(require 'package)
(package-initialize)
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
(set-frame-font "LiberationMono" nil t)

;; EMACS SPECIFIC :: more specific emacs customization
(put 'narrow-to-region 'disabled nil)

;; RELATIVE-LINE-NUMBERS :: Enable relative line numbers
;; everywhere.
(use-package relative-line-numbers
  :ensure t
  :config (global-relative-line-numbers-mode))

;; THEME :: Load the monokai theme
(use-package monokai-theme
  :ensure t
  :config (load-theme 'monokai t))


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

(global-set-key "\C-x2" 'split-vertical-to-next-buffer)
(global-set-key "\C-x3" 'split-horizontal-to-next-buffer)

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

(global-set-key (kbd "C-x >") 'switch-buffer-next)
(global-set-key (kbd "C-x <") 'switch-buffer-previous)


;;; There are some packages which are useful across a range of
;;; modes. We configure them here.

;; AVY :: Create some keybindings for avy
(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-word-1)))

;; FUZZY :: Fuzzy lets us use fuzzy matching, which is useful
;; with packages like auto-complete.
(use-package fuzzy
  :ensure t)

;; AUTO-COMPLETE
(use-package auto-complete
  :ensure t
  :bind (("M-n" . auto-complete))
  :config (require 'auto-complete-config)
          (ac-config-default)
          (setq ac-use-fuzzy t)
          (setq ac-auto-start nil))

;; YASNIPPET :: Use Yasnippet everywhere
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; MAGIT :: Magit is a wrapper for git
(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)))

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
	  (use-package shm
	    :ensure t)
	  (use-package flycheck-haskell
	    :ensure t
	    :config (eval-after-load 'flycheck
		      '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))
	  ;; Add jump to imports shortcut
	  (eval-after-load 'haskell-mode
	    '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
	  ;; Now we set up haskell-mode in the way which we like
	  ;; Use structured haskell mode to handle indentation etc.
	  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
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

;;; R :: editing modes and configuration for R
(use-package ess
  :ensure t)

;;; JavaScript :: editing mode and extensions for JavaScript
(use-package js3-mode
  :ensure t)

;;; TypeScript :: editing mode for TypeScript
(use-package tide
  :ensure t)


;;; EVIL AND POWERLINE :: Start evil mode and powerline

;; Use powerline
(use-package powerline
  :ensure t
  :config
  (use-package powerline-evil
    :ensure t
    :config (powerline-evil-vim-color-theme)))

;; Allow evil mode to be used if preferred
(use-package evil
  :ensure t
  :config
  (evil-mode t))


;;; OTHER NICE THINGS :: Other things that are nice to have

;; Structure and Interpretation of Computer Programs
(use-package sicp
  :ensure t)

