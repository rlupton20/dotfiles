;;; Custom emacs configuration.


;;; First we set up the emacs package repository, and require
;;; emacs to use it.

;; We would like to be able to install packages from MELPA
;; so lets include the repository
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)



;;; Now we make some adjustments to the basic appearance of
;;; emacs. For example, we load themes, and remove unwanted
;;; toolbars.

;; BASIC :: Alter appearances
(tool-bar-mode -1)    ; Turn off tool bar in X mode
(menu-bar-mode -1)    ; Turn off the menu bar
(scroll-bar-mode -1)  ; Remove the scrollbar

;; THEME :: Load the monokai theme
(load-theme 'monokai t)

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



;;; There are some packages which are useful across a range of
;;; modes. We configure them here.

;; AVY :: Create some keybindings for avy
(global-set-key (kbd "C-.") 'avy-goto-word-1)

;; FUZZY :: Fuzzy lets us use fuzzy matching, which is useful
;; with packages like auto-complete.
(require 'fuzzy)

;; AUTO-COMPLETE
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-fuzzy t)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-n") 'auto-complete)

;; YASNIPPET :: Use Yasnippet everywhere
(require 'yasnippet)
(yas-global-mode 1)



;;; Now we configure packages for individual editing modes.

;; YAML :: Add YAML mode and configure
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;; Do some Haskell specific configuration.

;; HASKELL :: haskell-mode

;; Make is possible to launch ghci instances from emacs
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
