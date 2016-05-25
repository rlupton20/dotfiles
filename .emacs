;;; Custom emacs configuration.


;;; First we set up the emacs package repository, and require
;;; emacs to use it.

;; We would like to be able to install packages from MELPA
;; so lets include the repository
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)


;;; Now we make some adjustments to the basic appearance of
;;; emacs. For example, we load themes, and remove unwanted
;;; toolbars.

;; BASIC :: Alter appearances
(tool-bar-mode -1)    ; Turn off tool bar in X mode
(menu-bar-mode -1)    ; Turn off the menu bar
(scroll-bar-mode -1)  ; Remove the scrollbar


;; THEME :: Load the monokai theme
(load-theme 'monokai t)


;; HELM :: Use helm in places where it is useful

;; First using helm for M-x so we get a live filter
;; of options, and don't need to keep tab completing.
(global-set-key (kbd "M-x") 'helm-M-x)
;; Also use helm for buffers. I can never remember the
;; buffers I have open.
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; Finding files can also be a pain, so use helm
;; to locate and open files
(global-set-key (kbd "C-x C-f") 'helm-find-files)


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


;;; EVIL AND POWERLINE :: Start evil mode and powerline

;; Use powerline
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-vim-color-theme)

;; Allow evil mode to be used if preferred
(require 'evil)
(evil-mode t)

;;; EXPERIMENTAL

;; An ssh connection manager (integrated with helm)
(require 'term)
(setq host-list '())

(setq ssh-sessions '())

;; filter is a utility function - it returns the elements of list lst
;; which fulfil a predicate pred
(defun filter (pred lst)
  (cond
   ((null lst) '())
   ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
   (t (filter pred (cdr lst)))))

;; (ssh-term host) opens a new ssh session to host
(defun ssh-term (host)
  (setq ssh-buffer-name (concat "*" host "*")) 
  (set-buffer (apply 'term-ansi-make-term ssh-buffer-name "ssh" nil (list host)))
  (term-mode)
  (term-char-mode)
  (add-to-list 'ssh-sessions ssh-buffer-name)
  (switch-to-buffer ssh-buffer-name))

;; helm-new-ssh-session uses a helm menu to create a new ssh session
;; with a host on the list host-list
(defun helm-new-ssh-session ()
  (interactive)
  (setq helm-outline-hosts
	'((name . "Hosts")
	  (candidates . host-list)
	  (action . ssh-term)))
  (helm :sources '(helm-outline-hosts)))

;; helm-switch-to-ssh-session uses a helm menu to select a live ssh
;; session, and then switch to it.
(defun helm-switch-to-ssh-session ()
  (interactive)
  ; First we filter the ssh-sessions so we only list
  ; those which are still live (have a buffer)
  (setq ssh-sessions (filter 'get-buffer ssh-sessions))
  (setq helm-outline-current
	'((name . "Current SSH sessions")
	  (candidates . ssh-sessions)
	  (action . choice)))
  (helm :sources '(helm-outline-current)))

;; Some keybindings to test with
(global-set-key (kbd "s-s") 'helm-switch-to-ssh-session)
(global-set-key (kbd "s-c") 'helm-new-ssh-session)
