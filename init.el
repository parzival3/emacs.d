;; use streight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Packages
(straight-use-package 'use-package)

;; Magit
(use-package magit
  :straight t)

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :straight t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package corfu
  :straight t
  :init
  (corfu-global-mode))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :straight t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; Configuration
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta   t
	mac-command-modifier      'super
	ns-command-modifier       'meta
	mac-option-modifier       'meta
	ns-option-modifier        'meta
	mac-right-option-modifier 'none
	ns-right-option-modifier  'none))

;; Remove white spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Camel-case words are separated words in program mode :-)
(add-hook 'prog-mode-hook 'subword-mode)

;; Single space after period
(setq sentence-end-double-space nil)

;; Delete selected text if we start typing
(delete-selection-mode t)

;; Y-N as default
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

;; Silent startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Dired file size as human redeable by default
(setq-default dired-listing-switches "-alh")

;; Show matching parethesis
(show-paren-mode t)

;; Don't make sound by flash screen
(setq visible-bell t)

;; Try to not use tabs
(setq-default indent-tabs-mode nil)

;; Display all the functions by default
(setq apropos-do-all t)

;; Setup the backup directory
(unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

;; Uniquify is a library to make the filename of similar buffer different
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Remember the last postion visited
(save-place-mode 1)

;; UI-UX
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(load-theme 'modus-vivendi)

;; Prefer to load the more recent version of a file
(setq load-prefer-newer t)

;; Keybindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
