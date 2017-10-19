;;; .emacs --- Custom emacs configuration.
;;; Commentary:
;;; Custom Emacs configuration
(require 'package)
;;; Code:

;;; First we set up the emacs package repository, and require
;;; emacs to use it.

;; We would like to be able to install packages from MELPA
;; so lets include the repository
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
(global-visual-line-mode 1)  ; Use visual line mode to wrap lines nicely
(setq show-trailing-whitespace t)
(global-hl-line-mode)

;;; FIXES :: For things which don't behave quite right

;; projectile tries to use the local shell in TRAMP mode
;; which is a pain on systems which have a different path for
;; the shell can cause an error (shell can't be found).
;; Since most systems symlink sh to /bin/bash or the default
;; shell anyway, we may as well just use sh
(setq shell-file-name "sh")

;; EMACS SPECIFIC :: more specific emacs customization
(put 'narrow-to-region 'disabled nil)


;; LINE NUMBERING:: use nlinum for line numbering
(use-package nlinum
  :ensure t
  :diminish nlinum-mode)

(use-package nlinum-relative
  :ensure t
  :pin melpa
  :diminish nlinum-relative-mode
  :after evil
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol "0")
  (global-nlinum-relative-mode))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-enable-bold t
	doom-enable-italic t))

(use-package color-theme-sanityinc-solarized
  :ensure t)

;; ICONS :: Nicer icons
(use-package all-the-icons
  :ensure t)


;; WHITESPACE MODE :: Configure whitespace mode to show potential
;; untidiness in code. Add it automatically to programming modes.
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face tabs lines))
  (set-face-attribute 'whitespace-line nil
		      :background (doom-color (quote base3))
		      :foreground 'unspecified)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (set-face-attribute 'sp-pair-overlay-face nil
		      :foreground (doom-color (quote bg))
		      :background (doom-color (quote base5))))

;; ELISP :: Setup for editing emacs lisp
(use-package hl-sexp
  :ensure t
  :diminish hl-sexp-mod
  :config
  (set-face-attribute 'hl-sexp-face nil
		      :background (doom-color (quote bg-alt)))
  (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode))

;; Enable smartparens in elisp mode
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)


;; TRAMP :: Fix tramps remote paths - this is needed in order that
;; we find programs in user profiles on remote nix boxes
(use-package tramp
  :ensure t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


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

(use-package helm-swoop
  :ensure t
  :after helm)


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
  "Switch current and next window's buffers."
  (interactive)
  (let ((current (window-buffer)))
  (set-window-buffer nil
		     (window-buffer (next-window)))
  (set-window-buffer (next-window) current)))

(defun switch-buffer-previous ()
  "Switch current and previous window's buffers."
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
		  (message
		   (concat "Refreshed buffer from " (buffer-file-name)))))

;; Sometimes its useful to revert all open buffers to that on the disk
(defun revert-all-buffers ()
  "Refreshes all buffers from the files on disk."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
		 (file-exists-p (buffer-file-name))
		 (not (buffer-modified-p)))
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
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;; YASNIPPET :: Use Yasnippet everywhere
(use-package yasnippet
  :ensure t
  :diminish
  :config (yas-global-mode 1))

;; MAGIT :: Magit is a wrapper for git
(use-package magit
  :ensure t
  :config
  (magit-define-popup-action 'magit-push-popup
    ?a
    "push current branch to all remotes"
    'magit-custom-push-current-all-remotes ; Defined below
    ?e))

;; Define a utility function to push a branch to all remotes
(defun magit-custom-push-current-all-remotes (args)
  "Push the current branch to all remotes for the git repository."
  (interactive
   (list (magit-push-arguments)))
  (let* ((branch (magit-get-current-branch))
	(remotes (magit-list-remotes)))
    (dolist (remote remotes nil)
      (let ((target (mapconcat 'identity (list remote branch) "/")))
	(magit-push-current target args)))))


;; PROJECTILE :: Projectile helps with project management
(use-package projectile
  :ensure t
  :config (projectile-mode)
          (use-package helm-projectile
            :ensure t)
          (helm-projectile-on))

;; FLYCHECK :: On the fly syntax checking

;; First we write some utility functions to look inside a nix sandbox, but only
;; if there is one
(defun nix-maybe-shell-command (command)
  (if (nix-current-sandbox)
      (apply 'nix-shell-command (nix-current-sandbox) command)
    command))

(defun nix-maybe-executable-find (command)
  (if (nix-current-sandbox)
      (apply 'nix-executable-find (nix-current-sandbox) command)
    command))

(use-package flycheck
  :ensure t
  :config (setq flycheck-command-wrapper-function
		(lambda (command) command)
		flycheck-executable-find
		(lambda (command) command))
          (global-flycheck-mode))

(use-package multiple-cursors
  :ensure t)

;; HYDRA :: library for creating magit-like keyboard driven menus
(use-package hydra
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

;; MARKDOWN :: markdown mode
(use-package markdown-mode
  :ensure t)

;; EMAMUX :: control tmux sessions from emacs
(use-package emamux
  :ensure t)


;; HASKELL :: modes for working with Haskell code
;; -- External dependencies --
;; happy, hindent, hasktags, stylish-haskell, ghc-mod, hlint, hoogle, hare

;; Haskell modes use various packages installed by cabal,
;; so we need to add cabal's bin directory to our path
(defun add-cabal-path ()
  (let ((cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path cabal-path)))

(defun add-stack-path ()
  (let ((stack-path (expand-file-name "~/.local/bin/")))
    (setenv "PATH" (concat stack-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path stack-path)))

(use-package haskell-mode
  :ensure t
  :defer
  :config (add-cabal-path)
  (add-stack-path)
  (custom-set-variables '(haskell-tags-on-save t))
  (add-hook 'haskell-mode-hook 'smartparens-mode)
  (add-hook 'haskell-mode-hook 'subword-mode))

(use-package intero
  :ensure t
  :after haskell-mode
  :defer
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook (lambda () (setq show-trailing-whitespace t))))

;; PYTHON :: elpy for editing Python
(use-package elpy
  :ensure t
  :defer
  :config
  (elpy-enable))


;; RUST :: emacs configuration for editing rust
(use-package rust-mode
  :ensure t
  :defer
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :after rust-mode
  :defer
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :after rust-mode
  :defer
  :config
  (setq racer-cmd "racer")
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))


(use-package flycheck-rust
    :ensure t
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


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


;;; SCALA :: mode for editing scala, with ENSIME integration
(use-package sbt-mode
  :ensure t
  :defer)

(use-package ensime
  :ensure t
  :defer
  :pin melpa-stable)


;;; Clojure :: mode for editing clojure
(use-package clojure-mode
  :ensure t
  :defer
  :config
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'hl-sexp-mode))

(use-package cider
  :defer
  :ensure t)


;;; IO :: mode for editing Io
(use-package io-mode
  :defer
  :ensure t)

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


;;; POWERLINE :: vim-like powerline mode line
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'slant)
  (setq powerline-height 29))

(use-package airline-themes
  :ensure t
  :config
  (load-theme 'airline-powerlineish t)
  (setq powerline-utf-8-separator-left      #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1))

;; Custom theming :: there is too much cruft in the modeline,
;; and all-the-icons makes it simpler to represent


;; EVIL :: (emulated) vim text control

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

;; EVIL SURROUND :: surround text objects
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; EVIL SMARTPARENS :: do what you mean in smartparens mode
(use-package evil-smartparens
  :ensure t
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; EVIL EASYMOTION :: ace jump mode with evil
(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

;; EVIL COMMENTARY :: comment out lines in evil
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; EVIL NUMBERS :: Increment/decrement numbers at point
(use-package evil-numbers
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "H-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "H-x") 'evil-numbers/dec-at-pt))


;; EVIL LEADER :: leader key for commands
(use-package evil-leader
  :ensure t
  :diminish evil-leader-mode
  :config (global-evil-leader-mode)
   (evil-leader/set-leader ",")
   (evil-leader/set-key
   "V" 'global-hl-line-mode
   "x" 'helm-M-x
   "f" 'helm-find-files
   "pf" 'helm-projectile-find-file
   "ps" 'helm-projectile-switch-project
   "pb" 'helm-projectile-switch-to-buffer ; For uniformity
   "b"  'helm-projectile-switch-to-buffer
   "pg" 'helm-projectile-grep
   "s"  'helm-swoop
   ","  'avy-goto-char
   "k" 'helm-show-kill-ring
   "g" 'magit-status))

;; EVIL :: Enable evil mode
(use-package evil
  :ensure t
  :diminish evil-mode
  :config (evil-mode t))

;; Fix keymaps
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-j") 'sp-up-sexp)
;; M-. is often used for jump to definition, which is useful in normal mode
(define-key evil-normal-state-map (kbd "M-.") nil)

;; Keybindings for move-text
(define-key evil-visual-state-map (kbd "C-j") 'move-text-down)
(define-key evil-visual-state-map (kbd "C-k") 'move-text-up)



;;; HYDRAS :: Hydras for controlling emacs

;; Window management hydra
(defhydra hydra-window-manager (:hint nil)
  "Window management"
  ("-" split-vertical-to-next-buffer "vertical split")
  ("\\" split-horizontal-to-next-buffer "horizontal split")
  ("f" delete-other-windows "focus")
  ("d" delete-window "delete")
  ("h" evil-window-left "left")
  ("j" evil-window-down "down")
  ("k" evil-window-up "up")
  ("l" evil-window-right "right")
  ("b" helm-buffers-list "switch buffer"))


;; Theme control hydra
(defun theme/choose-doom-molokai ()
  "Set theme to doom-molokai."
  (interactive)
  (load-theme 'doom-molokai t)
  (set-face-attribute 'hl-sexp-face nil
		      :background (doom-color (quote bg-alt)))
  (load-theme 'airline-doom-molokai t))

(defun theme/choose-molokai-powerline ()
  "Set theme to doom-molokai with powerlineish powerline theme."
  (interactive)
  (load-theme 'doom-molokai t)
  (set-face-attribute 'hl-sexp-face nil
		      :background (doom-color (quote bg-alt)))
  (load-theme 'airline-powerlineish t))

(defun theme/choose-solarized-dark ()
  "Set theme to solarized dark."
  (interactive)
  (setq powerline-default-separator 'bar)
  (color-theme-sanityinc-solarized-dark)
  (set-face-attribute 'hl-sexp-face nil
		      :background "#073642")
  (load-theme 'airline-powerlineish t))

(defun theme/choose-doom-one ()
  "Set theme to doom-one."
  (interactive)
  (load-theme 'doom-one t)
  (load-theme 'airline-doom-one t))

(defhydra hydra-theme-menu (:hint nil)
  "Theme selection"
  ("m" theme/choose-doom-molokai "Doom molokai")
  ("M" theme/choose-molokai-powerline "Molokai with pure Powerline")
  ("s" theme/choose-solarized-dark "Solarized dark")
  ("o" theme/choose-doom-one "Doom one"))

;; Control hydras :: hydras that collect other control hydras together
(defhydra hydra-emacs-control (:hint nil)
  "Control panel"
  ("w" hydra-window-manager/body "Window management" :exit t))

(evil-leader/set-key
  "c" 'hydra-emacs-control/body)

(defhydra hydra-emacs-meta-control (:hint nil)
  "Configuration panel"
  ("t" hydra-theme-menu/body "Theming" :exit t))

(evil-leader/set-key
  "C" 'hydra-emacs-meta-control/body)


;;; OTHER NICE THINGS :: Other things that are nice to have

;; Structure and Interpretation of Computer Programs
(use-package sicp
  :ensure t)

;; THEME SETTING :: Override default themes
;; Different machine need different setups to look nice.
;; We provide a default, but allow a different theme to be
;; specified by setting an environment variable
(let ((theme (getenv "CUSTOM_EMACS_THEME")))
  (cond
   ((string-equal theme "SOLARIZED") (theme/choose-solarized-dark))
   (t (theme/choose-molokai-powerline))))

;; LOCAL CUSTOMIZATIONS :: Custom local configuration (if it exists)
(let ((local-file "~/.emacs-local"))
  (if (file-exists-p local-file)
      (load-file local-file)))

(provide '.emacs)
;;; .emacs ends here

