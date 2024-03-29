;;; init.el --- Personal Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; My personal init file

;;; Code:
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
(straight-use-package 'org)

;; Load enviroment file for this computer based on the hostname
(load-file (concat user-emacs-directory "env/" (system-name) ".el"))
(load-file secrets-file)

;; Load the keybidings configuration
(load-file (concat user-emacs-directory "kbd.el"))

;; Load the language packages
(load-file (concat user-emacs-directory "lang.el"))

;; Load the org customization
(load-file (concat user-emacs-directory "org.el"))

(when (or (eq system-type `gnu/linux)
          (eq system-type 'darwin))

(use-package vterm
  :defer t
  :straight t)
:config
  (setq vterm-copy-mode-remove-fake-newlines t))

(use-package wgrep
  :straight t)

;; Magit
(use-package magit
  :commands (magit-status)
  :bind
  (("C-x g" . magit-status))
  :straight t
  :bind (:map magit-mode-map
              ("D" . #'magit-discard))
  :config
  (defun winnt-get-git-tools-path ()
     (let ((git-exe-path (shell-command-to-string "where git.exe")))
         (if (string-match "git" git-exe-path)
             (let ((git-path  (butlast (file-name-split git-exe-path) 2)))
               (concat (mapconcat #'identity git-path "/") "/usr/bin/")))))

  (when (eq system-type `windows-nt)
    (setq exec-path (cons (winnt-get-git-tools-path) exec-path))
    (setenv "PATH" (concat (getenv "PATH") ";" (winnt-get-git-tools-path))))

  (defun magit-commit-fast (commit-message)
    (interactive "commit message:")
    (let ((magit-commit-ask-to-stage t)
          (magit-commit-show-diff nil))
      (magit-git "commit" "--all" "-m" commit-message)))

  (when (eq system-type `windows-nt)
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)))

(use-package debugger
  :defer t
  :config
  :bind
  (:map debugger-mode-map
        ("h" . meow-left)
        ("l" . meow-right)
        ("j" . meow-up)
        ("k" . meow-down)
        ("x" . meow-line)
        ("y" . meow-clipboard-save)
        ("q" . debugger-quit)))

(use-package project
  :config
  ;;; add element to project-switch-commands alist
  (defun project-magit-status ()
    (interactive)
    (magit-status (project-root (project-current t))))
  (add-to-list 'project-switch-commands '(project-magit-status "Magit Status" ?m)))

(use-package vertico
  :defer t
  :straight t
  :config
  (setq vertico-cycle t)
  :init
  (vertico-mode))

(use-package consult
  :defer t
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark
  :defer t
  :straight t
  :bind
  (("<f9>" . embark-export)
   ("C-." . embark-act)
   ("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-\\" . embark-act)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun dired-find-file-directory ()
    "In Dired, visit the file or directory named on this line."
    (interactive)
    (let ((filename (read-from-minibuffer "Input: ")))
      (dired--find-possibly-alternative-file (file-name-directory filename))))

  (define-key embark-file-map (kbd "d") #'dired-find-file-directory)

  ;; fix problem in marginaglia/embark
  (setf (alist-get 'xref-location embark-default-action-overrides)
      #'embark-consult-goto-grep)
  (setf (alist-get 'xref-location embark-exporters-alist)
      #'embark-consult-export-grep)
  (setf (alist-get 'consult-xref embark-default-action-overrides)
      #'embark-consult-goto-grep)
  (setf (alist-get 'consult-xref embark-exporters-alist)
      #'embark-consult-export-grep))


(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package which-key
  :defer t
  :straight t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  :init
  (which-key-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately while 'completing-read' prompter.
FN: the function to apply.
ARGS: the arguments to the function."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(use-package orderless
  :defer t
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles . (orderless flex))))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :defer t
  :straight t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :commands (dabbrev-completion
             dabbrev-expand)
  :straight t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package spacemacs-theme
  :straight t)


(use-package diminish
  :straight t
  :config
  (diminish 'copilot-mode " CoPy")
  (diminish 'eldoc-mode))

(use-package window
  :config
  (defvar original-display-buffer-alist display-buffer-alist)

  ;; Define common parameters
  (setq display-buffer-base-params
        '((side . bottom)
          (slot . -1)
          (window-parameters
           (no-delete-other-windows . nil))))

  ;; Add entries using add-to-list
  (add-to-list 'display-buffer-alist
               '("\\*\\(Embark Export\\|cider-error\\|Flutter-Runner\\|repl\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 ,@display-buffer-base-params))

  (add-to-list 'display-buffer-alist
               '("\\*\\(e?shell\\|vterm\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.33)
                 ,@display-buffer-base-params))

  (add-to-list 'display-buffer-alist
               '("\\*no-display\\*"
                 (display-buffer-no-window)))

  (add-to-list 'display-buffer-alist
               '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\|Compilation\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0)
                 (window-parameters
                  (no-delete-other-windows . nil)))))


(use-package elfeed-protocol
  :straight t
  :after elfeed)

(use-package elfeed
  :straight t
  :init
  (setq elfeed-use-curl t)

  (setq elfeed-protocol-ttrss-maxsize 200) ; bigger than 200 is invalid
  (setq elfeed-protocol-ttrss-fetch-category-as-tag t)
  (setq elfeed-protocol-enabled-protocols '(ttrss))
  (setq elfeed-protocol-feeds `((,(concat "ttrss+https://" user@ttrss-url "/tt-rss")
                                 :password ,ttrss-password)))
  :config
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable)

  (defun et-elfeed-eww-visit (&optional use-generic-p)
    "Visit the current entry in eww."
    (interactive "P")
    (let ((browse-url-browser-function 'eww-browse-url))
      (elfeed-show-visit use-generic-p)))

  :bind
        (:map elfeed-search-mode-map
                ("b" . et-elfeed-eww-visit)
                ("B" . elfeed-show-visit))
        (:map elfeed-show-mode-map
                ("b" . et-elfeed-eww-visit)
                ("B" . elfeed-show-visit)))
(use-package eww
  :bind
  (:map eww-mode-map
        ("L" . eww-forward-url)
        ("H" . eww-back-url)
        ("l" . meow-right)
        ("h" . meow-left)
        ("j" . meow-up)
        ("k" . meow-down)
        ("x" . meow-line)
        ("y" . meow-clipboard-save)))

(use-package fd-dired
  :straight t
  :config
  (defun fd-dired-simple ()
    (interactive (list (read-string "Run fd (with args and search): " fd-dired-input-fd-args
                                    '(fd-dired-args-history . 1))))
    ;; if current buffer is a dired buffer, use its directory
    (let ((dir (if (eq major-mode 'dired-mode)
                   ;; if the element under the cursor is a directory use it
                   (if (file-directory-p (dired-get-file-for-visit))
                       (dired-get-file-for-visit))
                 default-directory)))
      (fd-dired dir fd-dired-input-fd-args)))
  :bind
  (:map dired-mode-map
        ("C-x C-d" . fd-dired-simple)))

(use-package emacs
  :init
  ;; Configuration
  (when (eq system-type 'darwin)
    (setq mac-command-key-is-meta   t
	  mac-command-modifier      'super
	  ns-command-modifier       'meta
	  mac-option-modifier       'meta
	  ns-option-modifier        'meta
	  mac-right-option-modifier 'none
	  ns-right-option-modifier  'none))

  (defvar wsl (string-match "-[Mm]icrosoft" operating-system-release))

  (setq whitespace-line-column 200)

  (add-hook 'find-file-hook 'set-file-coding-if-crlf)

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
  (setq inhibit-startup-message t
        initial-scratch-message nil
        inhibit-startup-screen t
        inhibit-default-init t
        inhibit-startup-echo-area-message user-login-name)

  ;; Dired file size as human redeable by default
  (setq-default dired-listing-switches "-alh")

  ;; Show matching parethesis
  (show-paren-mode t)

  ;; Don't make sound by flash screen
  (setq visible-bell nil)

  ;; Try to not use tabs
  (setq-default indent-tabs-mode nil)

  ;; Display all the functions by default
  (setq apropos-do-all t)

  ;; Setup the backup directory
  (unless backup-directory-alist
    (setq backup-directory-alist `((".*" . ,(concat user-emacs-directory
                                                   "backups")))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
          backup-by-copying t      ; don't clobber symlinks
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t))

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


  (add-to-list 'default-frame-alist `(font . ,et-font))
  (set-face-attribute 'default t :font et-font )
  (set-face-attribute 'default nil :font et-font)
  (set-frame-font et-font nil t)
  (set-face-font 'fixed-pitch-serif et-font)
  (set-face-font 'variable-pitch et-font)

  ;; Prefer to load the more recent version of a file
  (setq load-prefer-newer t)

  ;; Disabled functions
  (put 'narrow-to-region 'disabled nil)

  ;; Keybindings
  (global-set-key (kbd "C-s") 'consult-line)

  ;; Set line numbers in the buffer
  (global-display-line-numbers-mode t)

  ;; Use ibuffer instead of the usual buffer menu
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq minibuffer-complete-and-exit 'after-completion)

  ;; Open a new file when switching tab
  (advice-add #'tab-new :after (lambda (&rest r) (call-interactively #'find-file)) '((name . "new-tab-find-file")))

  ;; Open a new file when switching tab
  (advice-add #'tab-duplicate :after (lambda (&rest r) (call-interactively #'find-file)) '((name . "new-tab-find-file")))

  (global-set-key (kbd "C-x t k") #'tab-close)

  ;; recompile with f5
  (global-set-key (kbd "<f5>") #'recompile)

  ;; Set tab width to 4
  (setq tab-width 4)
  ;; Set file encoding to linux
  (prefer-coding-system 'utf-8-unix)

  ;; enable column mode
  (setq column-number-mode 1)

  ;; don't hide the line feed type
  (setq inhibit-eol-conversion t)

  ;; set default find program from git
  (when (and (eq system-type 'windows-nt)
             (executable-find "git.exe"))
    (setq find-program (prin1-to-string
                        (concat
                         (file-name-directory (executable-find "git.exe"))
                         "../usr/bin/find.exe"))))

  ;; DCI configuration for windows
  (when (file-directory-p (concat git-directory "dci-emacs"))
    (let ((dci (concat git-directory  "/dci-emacs/dci.el"))
          (gaming (concat git-directory "dci-emacs/gaming.el")))
      (load-file dci)))

  (add-hook 'prog-mode-hook #'hs-minor-mode)

  ;; custom variables
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror)

  ;; Use window move
  (windmove-default-keybindings)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Set current theme to my p-theme variable
  ;;(load-theme et-theme t)

  ;; Start server for org-roam-protocol-capture
  (server-start))


(straight-use-package
 '(nano :type git :host github :repo "rougier/nano-emacs"))

(require 'nano)


(use-package string-inflection
  :straight t
  :config

  (defun to-pascal-case ()
         (interactive)
         (let ((word (symbol-name (symbol-at-point)))
               (bounds (bounds-of-thing-at-point 'symbol)))
           (kill-region (car bounds) (cdr bounds))
           (insert (string-inflection-pascal-case-function word))))

  (defun to-lower-case ()
         (interactive)
         (let ((word (symbol-name (symbol-at-point)))
               (bounds (bounds-of-thing-at-point 'symbol)))
           (kill-region (car bounds) (cdr bounds))
           (insert (downcase word))))

  (defun search-and-relace-to-lowecase ()
         (interactive)
         (let ((word (symbol-name (symbol-at-point))))
           ;; save excusion
           (save-excursion
             (beginning-of-buffer)
             (query-replace-regexp word (downcase word))))))

(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file "~/.emacs_bookmarks"))

(use-package dired
  :defer t
  :bind
  (:map dired-mode-map
   ("-" . dired-up-directory))
  :config
  ;; prevent for creating new buffers for each folder.
  (setf dired-kill-when-opening-new-dired-buffer t)
  ;; easilly copy to other windows
  (setq dired-dwim-target t)
  (when (eq system-type 'windows-nt)
    (defun et-dired-look ()
      (when (derived-mode-p 'dired-mode)
        (setq-local line-spacing 0.1)
        (setq-local left-margin-width 1)
        (setq-local right-margin-width 1)
        (setq-local word-wrap t)
        (setq-local truncate-lines nil)
        (setq-local truncate-partial-width-windows nil)
        (setq-local wrap-prefix "  ")
        (setq-local dired-listing-switches "-alh --group-directories-first")
        (dired-hide-details-mode 1)))
    (add-hook 'dired-mode-hook #'et-dired-look)))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output t)
  (setq compilation-auto-jump-to-first-error t))

(use-package xref
  :defer t
  :config
  (setq xref-search-program 'ripgrep))

(use-package man
  :defer r
  :config
  (when (eq system-type 'windows-nt)
    (setq manual-program "wsl -- man")))

(use-package tramp
  :defer t
  :config
  (when (eq system-type 'windows-nt)
    (setq tramp-use-ssh-controlmaster-options nil)
    (add-to-list 'tramp-connection-properties
	         (list (regexp-quote "/ssh:")
		       "login-args"
		       '(("-tt") ("-l" "%u") ("-p" "%p") ("%c")
		         ("-e" "none") ("%h"))))))

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        (remove 'try-expand-line (remove 'try-expand-list hippie-expand-try-functions-list))))

(use-package cc-vars
  :defer t
  :config
  (setq c-old-style-variable-behavior t))

(use-package eglot
  :defer t
  :straight t
  :config
  (global-set-key (kbd "C-.") 'eglot-code-actions)
  (when (eq system-type `windows-nt)
    (add-to-list 'eglot-server-programs
                 `(c++-mode . ("c:/Tools/LLVM/bin/clangd.exe")))))

;; Custom functions

;; Hooks for vc-next-action
(defun et-commit-filename ()
"File name to add to the header of a git commit."
  (require 'project)
  (let* ((root (project-root (project-current)))
         (file-name (abbreviate-file-name (file-name-sans-extension buffer-file-name)))
         (extension (file-name-extension buffer-file-name))
         (final-file-name (mapconcat #'identity
                                     (cl-remove-duplicates (split-string (file-relative-name file-name root))
                                                           :test #'string-equal) ":")))
         (when (or (string-equal extension "c")
                 (string-equal extension "h")
                 (string-equal extension "cpp")
                 (string-equal extension "hpp"))
                (setq final-file-name (format "%s:%s" final-file-name extension)))
         (concat final-file-name ": ")))

(defun et-insert-preamble (preamble)
"Insert the PREAMBLE (aka filepath:filename) in the git commit."
  (when (equal (buffer-name) "*vc-log*")
                   (insert preamble)))

(defun et-vc-log-advice (orig-fun &rest args)
  "Advice the 'vc-next-action' function with inser-preamble.
The arguments are ORIG-FUN (vc-next-action) and ARGS the argument
of 'vc-next-action'."
  (let ((preamble (et-commit-filename)))
    (apply orig-fun args)
    (et-insert-preamble preamble)))

;; Advicing vc-next-action
(advice-add 'vc-next-action :around #'et-vc-log-advice)

(defun et-open-config ()
  "Open this configuration."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun et-dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (format-replace-strings '(("" . "")))
    (goto-char (point-min))
    (format-replace-strings '((" " . ""))))
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun et-unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun et-trim-whitespace-based-on-encoding ()
  "Trim trailing whitespace based on the buffer's encoding."
  (interactive)
  (let ((coding-system (symbol-name buffer-file-coding-system)))
    (if (string-match "dos" coding-system)
        (progn
          ;; For Windows (CRLF) encoding
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil t)
            (replace-match "")))
      (if (string-match "unix" coding-system)
        (delete-trailing-whitespace)))))

(add-hook 'before-save-hook 'et-trim-whitespace-based-on-encoding)

(defun set-file-coding-if-crlf ()
  "Check for ^M characters (Windows line endings) in the current buffer.
   If found, set the file's encoding to utf-8-dos."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\r\n" nil t)
      (set-buffer-file-coding-system 'utf-8-dos)
      (message "File encoding set to utf-8-dos due to Windows line endings (^M)."))))

(defun et-make-unix-dir (dir)
  "Find all non hidden files in DIR and convert their line ending into unix."
  (interactive)
  (let ((files (directory-files dir t)))
    (while files
      (let ((current-file (pop files)))
        (if (not (file-directory-p current-file))
            (with-temp-file current-file
              (insert-file-contents current-file)
              (et-dos2unix))
          (unless (string-match "^." (file-name-base current-file)) ;; remove all the hidden files
            (et-make-unix-dir current-file)))))))

(defvar-local project-test-cmd nil
  "Function for testing the current project, ovveride it in the dir locals var.")

(defvar-local project-run-cmd nil
  "Function for running the current project, ovveride it in the dir locals var.")

(put 'project-run-cmd 'safe-local-variable 'string-or-null-p)
(put 'project-test-cmd 'safe-local-variable 'string-or-null-p)

(defun et-project-run-tests ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compile-command (or project-test-cmd
                            compile-command)))
    (call-interactively #'compile)))

(defun et-project-run ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (run-command project-run-value))
    (cl-flet ((prompt-text (lambda () (insert run-command))))
      (minibuffer-with-setup-hook #'prompt-text
          (call-interactively #'project-async-shell-command)))))

(defun et-search-for-word-in-directory (dir-to-search)
  "Search for current word inside the DIR-TO-SEARCH.
If there is no selected word, simply start an empty search."
  (interactive "DChoose the directory...")
  (let* ((string-to-search (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end)) "")))
    (consult-grep dir-to-search string-to-search)))

(defun et-find-file ()
  (interactive)
  ;; Project current check if we are inside a project otherwise uses the normal find
  (if (project-current)
    (project-find-file)
    (call-interactively 'find-file)))

(defun file-metadata ()
  (interactive)
  (let* ((fname (buffer-file-name))
         (data (file-attributes fname))
         (access (current-time-string (nth 4 data)))
         (mod (current-time-string (nth 5 data)))
         (change (current-time-string (nth 6 data)))
         (size (nth 7 data))
         (mode (nth 8 data)))
    (message
     "%s:
  Accessed: %s
  Modified: %s
  Changed: %s
  Size: %s bytes
  Mode: %s"
     fname access mod change size mode)))

(defun et-other-window ()
  "Switch to the next window in a cyclic manner, including side windows."
  (interactive)
  (let ((windows (window-list)))
    (cond
     ((null windows)
      (message "No windows to switch to."))
     ((= 1 (length windows))
      (message "Only one window is available."))
     (t
      (select-window (if (eq (selected-window) (car (last windows)))
                         (car windows)
                       (next-window)))))))

(defun et-set-msdos-file-type ()
  "Set the file type as MSDOS (CRLF line endings)."
  (interactive)
  (setq buffer-file-coding-system 'dos)
  (message "File type set to MSDOS (CRLF line endings)."))

(defun et-system-type ()
  "Detect if we are using a WSL system or not."
  (if (eq wsl nil)
      system-type
    'wsl))

(provide 'init)
;;; init.el ends here
