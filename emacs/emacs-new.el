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


;;; BROAD AND BASIC CONFIGURATION OF EMACS

;; BASIC :: Basic configuration of emacs

;; DIMINISH :: hide minor modes on the modeline
(use-package diminish
  :ensure t)

;; GENERAL :: Allow more sophisticated keybindings
(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-create-definer evil-leader-def
    :prefix ","))

(use-package emacs
  :diminish visual-line-mode
  :config
    (setq custom-file "~/.emacs.d/custom.el")  ; Stop polluting .emacs
    (load custom-file 'noerror)                ; with custom variables
    (tool-bar-mode -1)    ; Turn off tool bar in X mode
    (menu-bar-mode -1)    ; Turn off the menu bar
    (scroll-bar-mode -1)  ; Remove the scrollbar
    (setq-default tab-width 4 indent-tabs-mode nil)  ; Use spaces not tabs
    (global-visual-line-mode 1)  ; Use visual line mode to wrap lines nicely
    (global-hl-line-mode)
    (setq-default show-trailing-whitespace t)
    ;; Special emacs features
    (put 'narrow-to-region 'disabled nil)
    ;; projectile tries to use the local shell in TRAMP mode
    ;; which is a pain on systems which have a different path for
    ;; the shell can cause an error (shell can't be found).
    ;; Since most systems symlink sh to /bin/bash or the default
    ;; shell anyway, we may as well just use sh
    (setq shell-file-name "sh"))

;; Fix TRAMP to properly locate remote helpers
(use-package tramp
  :config
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package emacs
  :bind ("C-x 2" . split-vertical-to-next-buffer)
        ("C-x 3" . split-horizontal-to-next-buffer)
  :config
    (defun split-vertical-to-next-buffer ()
    (interactive)
    (split-window-vertically)
    (set-window-buffer (next-window) (other-buffer)))

    (defun split-horizontal-to-next-buffer ()
    (interactive)
    (split-window-horizontally)
    (set-window-buffer (next-window) (other-buffer))))

(use-package emacs
  :bind ("s->" . switch-buffer-next)
        ("s-<" . switch-buffer-previous)
  :config
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
    (set-window-buffer (previous-window) current))))

(use-package emacs
  :bind ("<f5>" . revert-current-buffer)
        ("<S-f5>" . revert-all-buffers)
  :config
    (defun revert-current-buffer ()
      "Refreshes the current buffer from the file on disk"
      (interactive)
      (revert-buffer t t)
      (message
       (concat "Refreshed buffer from " (buffer-file-name))))

    (defun revert-all-buffers ()
      "Refreshes all buffers from the files on disk."
      (interactive)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (buffer-file-name)
                     (file-exists-p (buffer-file-name))
                     (not (buffer-modified-p)))
            (revert-buffer t t t) )))
      (message "Refreshed open files")))

(use-package eldoc
  :diminish eldoc-mode
  :hook (racer-mode . eldoc-mode))

;; THEMES :: Make emacs look nice
(use-package color-theme-sanityinc-solarized
  :ensure t
  :config
  (color-theme-sanityinc-solarized-dark))

;; Configure the modeline
(use-package emacs
  :config
  (column-number-mode)
  (defvar mode-line/start-of-line nil "Marker for start of modeline")
  (defvar mode-line/position nil "Position in the file for use in modeline")
  (setq mode-line/position
        '(line-number-mode ("%l" (column-number-mode ":%c"))))
  (setq-default mode-line-format
        '("%e"
         mode-line/start-of-line
         mode-line-buffer-identification
         " "
         mode-line-modes
         " "
         mode-line/position)))

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
  (setq nlinum-relative-redisplay-delay 0.1)
  (setq nlinum-relative-current-symbol "0")
  (global-nlinum-relative-mode))


;;; HELPERS

;; WHICH-KEY :: pops up a buffer which helps with keybindings
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; TEXT EDITING UTILITIES

;; HIGHLIGHT :: Allow regions to be (temporarily) highlighted
(use-package highlight
  :ensure t)

;; BRACKETS :: better tracking of parentheses
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook (emacs-lisp-mode . smartparens-strict-mode))

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))


;;; EDITOR BEHAVIOUR

;; HELM :: Use helm in places where it is useful
(use-package helm
  :ensure t
  :bind (;; First using helm for M-x so we get a live filter
         ;; of options, and don't need to keep tab completing.
         ("M-x" . helm-M-x)
         ;; Also use helm for buffers. I can never remember the
         ;; buffers I have open.
         ("C-x C-b" . helm-buffers-list)
         ;; Finding files can also be a pain, so use helm
         ;; to locate and open files
         ("C-x C-f" . helm-find-files)))

;; MAGIT :: Magit is a wrapper for git
(use-package magit
  :ensure t
  :commands magit-status
  :diminish auto-revert-mode
  :config
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

    (magit-define-popup-action 'magit-push-popup
      ?a
      "push current branch to all remotes"
      'magit-custom-push-current-all-remotes ; Defined above
      ?e))

;; PROJECTILE :: Projectile helps with project management
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
    (projectile-mode))

;; HELM-PROJECTILE :: Makes projectile easier to use
(use-package helm-projectile
  :ensure t
  :after projectile
  :config
    (helm-projectile-on))

;; HELM-SWOOP :: Cross buffer fuzzy searching
(use-package helm-swoop
  :ensure t
  :commands helm-swoop
  :after helm)

;; HELM-GTAGS
(use-package helm-gtags
  :ensure t
  :pin melpa-stable
  :diminish helm-gtags-mode
  :hook ((c-mode . helm-gtags-mode)
         (c++-mode . helm-gtags-mode))
  :bind
    (:map helm-gtags-mode-map
          ("M-." . helm-gtags-dwim)
          ("M-," . helm-gtags-pop-stack)))

;; FLYCHECK :: On the fly syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook ((emacs-lisp-mode . flycheck-mode)
         (rust-mode . flycheck-mode)))

;; COMPANY-MODE :: auto-completion
(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :hook ((emacs-lisp-mode . company-mode)
         (racer-mode . company-mode))
  :config
    (setq company-idle-delay 0.5
          company-minimum-prefix-length 1
          company-tooltip-align-annotations t))


;; COMPANY-QUICKHELP :: Provides documentation for company completions
(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))


;;; EMACS EDITING MODES :: Tools and modes for working with
;;; specific kinds of files

;; ORG-MODE :: Emacs powertool
(use-package org
  :commands org-mode
  :config
    (defun split-window-with-overview ()
        "Split the current window to provide a small pane for document overview"
        (interactive)
        (let* ((win-edges (window-edges))
            (far-right-edge (nth 3 win-edges))
            (proportion 0.2)
            (overview-width (floor (* proportion far-right-edge)))
            (delta (- overview-width far-right-edge)))
        (split-window-horizontally)
        (enlarge-window delta t)
        (org-content)))

    (defun focus-section ()
      "Work on the currently selected section"
      (interactive)
      (org-tree-to-indirect-buffer)
      (org-content)
      (evil-window-right 1))

    (org-defkey org-mode-map (kbd "C-x <tab>") #'split-window-with-overview)
    (evil-define-key 'normal org-mode-map "zf" #'focus-section))


;;; EDITING MODES :: Modes for working with different laguages and formats

;;; RUST
(use-package rust-mode
  :ensure t
  :defer
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
 :ensure t
 :after rust-mode
 :diminish racer-mode
 :hook (rust-mode . racer-mode)
 :config
   (setq racer-cmd "racer")
   (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package flycheck-rust
   :ensure t
   :after flycheck
   :hook (flycheck-mode . flycheck-rust-setup))


;;; KITCHEN SINKS :: Modes that somehow find there way into emacs
(use-package w3m
  :ensure t
  :commands w3m
  :bind (:map w3m-mode-map
          ("f" . w3m/jump-follow-link))
  :config
    (require 'cl)
    (defun w3m/jump-follow-link ()
        "Jump to a link and follow it"
        (interactive)
        ;; ace-link-eww tried to follow links using
        ;; eww-follow-link - we redirect eww-follow-link
        ;; to w3m-view-this-url
        (flet ((eww-follow-link () (w3m-view-this-url)))
          (ace-link-eww))))

;; We can patch ace-link-eww in emacs-w3m to jump to links
(use-package ace-link
  :ensure t
  :commands ace-link-eww)

;; EVIL :: (emulated) vim text control
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

;; EVIL SURROUND :: surround text objects
(use-package evil-surround
  :ensure t
  :config
    (global-evil-surround-mode 1))

;; EVIL EASYMOTION :: ace jump mode with evil
(use-package evil-easymotion
  :ensure t
  :config
    (evilem-default-keybindings "SPC"))

;; EVIL COMMENTARY :: comment out lines in evil
(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :config
    (evil-commentary-mode))

;; EVIL SMARTPARENS :: do what you mean in smartparens mode
(use-package evil-smartparens
  :ensure t
  :diminish evil-smartparens-mode
  :hook (smartparens-enabled . evil-smartparens-mode))

;; EVIL LEADER :: leader key for commands, implemented with general
(evil-leader-def
  :states '(normal motion)
  :keymaps 'override
  "x" 'helm-M-x
  "f" 'helm-find-files
  "p" '(:ignore t :which-key "project")
  "pf" 'helm-projectile-find-file
  "ps" 'helm-projectile-switch-project
  "pb" 'helm-projectile-switch-to-buffer ; For uniformity
  "b"  'helm-buffers-list
  "pg" 'helm-projectile-grep
  "s"  'helm-swoop
  "S"  'helm-swoop-back-to-last-point
  "t"  'helm-gtags-select
  "T"  'helm-gtags-show-stack
  "j"  'helm-imenu
  ","  'avy-goto-char
  "k" 'helm-show-kill-ring
  "g" 'magit-status)

;; EVIL :: Enable evil mode
(use-package evil
  :ensure t
  :diminish evil-mode
  :bind (:map evil-normal-state-map
        ("C-u" . evil-scroll-up)
        ("M-." . nil)  ; Allows jump to definition
        :map evil-visual-state-map
        ("C-u" . evil-scroll-up))
  :config
    (evil-mode t)
    (setq evil-normal-state-tag " NORMAL "
            evil-emacs-state-tag " EMACS "
            evil-insert-state-tag " INSERT "
            evil-motion-state-tag " MOTION "
            evil-visual-state-tag " VISUAL "
            evil-operator-state-tag " OPERATOR ")
    (setq evil-mode-line-format '(before . mode-line/start-of-line)))

;;; OTHER NICE THINGS :: Other things that are nice to have

;; Structure and Interpretation of Computer Programs
(use-package sicp
  :ensure t)

(provide '.emacs)
;;; .emacs ends here
