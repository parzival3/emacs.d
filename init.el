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

(use-package evil
  :straight t
  :config
  (evil-set-leader '(normal motion visual replace) (kbd "SPC"))
  (evil-define-key 'visual 'global          (kbd "v") 'evil-delete-char)
  (evil-define-key '(normal motion) 'global (kbd "<leader>SPC") 'project-find-file)
  (evil-define-key '(normal motion) 'global (kbd "<leader>RET") 'consult-bookmark)
  (evil-define-key '(normal motion) 'global (kbd "<leader>pp")  'tabspaces-open-existing-project-and-workspace)
  (evil-define-key '(normal motion) 'global (kbd "<leader>gg")  'magit-status)
  (evil-define-key '(normal motion) 'global (kbd "<leader>,")   'consult-project-buffer)
  (evil-define-key '(normal motion) 'global (kbd "<leader>TAB") 'tabspaces-switch-workspace)
  (evil-define-key '(normal motion) 'global (kbd "<leader>fp")  'p-open-config)
  (evil-define-key '(normal motion) 'global (kbd "<leader>pr")  'p-project-run)
  (evil-define-key '(normal motion) 'global (kbd "<f1>")        'vc-next-action)
  (evil-define-key '(normal motion) 'global (kbd "<leader>pc")  'project-compile)
  (evil-define-key 'insert          'global  (kbd "C-x C-f")    'dabbrev-completion)
  (evil-define-key '(normal motion) 'global (kbd "<leader>pe")  'project-eshell)
  (evil-define-key '(normal motion) 'global (kbd "<leader>br")  'revert-buffer)
  (evil-define-key '(normal motion) 'global (kbd "<leader>po")  'ff-find-other-file)
  (evil-define-key '(normal motion) 'global (kbd "<leader>pt")  'p-project-run-tests)
  (evil-define-key '(normal motion) 'global (kbd "<leader>bd")  'kill-current-buffer)
  (evil-define-key '(normal motion) 'global (kbd "<leader>/")   'p-search-for-word-at-point)
  (evil-define-key '(normal motion) 'global (kbd "<leader>wV")  'evil-window-vsplit)
  (evil-define-key '(normal motion) 'global (kbd "<leader>ws")  'evil-window-split)
  :init
  (evil-mode 1))

;; Magit
(use-package magit
  :defer t
  :straight t)

(use-package vertico
  :defer t
  :straight t
  :init
  (vertico-mode))

(use-package cider
  :defer t
  :straight t
  :after evil
  :config
  (with-eval-after-load 'evil
    (defun evil-collection-cider-last-sexp (command &rest args)
      "In normal-state or motion-state, last sexp ends at point."
      (if (and (not evil-move-beyond-eol)
               (or (evil-normal-state-p) (evil-motion-state-p)))
          (save-excursion
            (unless (or (eobp) (eolp)) (forward-char))
            (apply command args))
        (apply command args)))

    (unless evil-move-beyond-eol
      (advice-add 'cider-eval-last-sexp :around 'evil-collection-cider-last-sexp)
      (advice-add 'cider-eval-last-sexp-and-replace :around 'evil-collection-cider-last-sexp)
      (advice-add 'cider-eval-last-sexp-to-repl :around 'evil-collection-cider-last-sexp)
      (with-eval-after-load 'cider-eval-sexp-fu
        (advice-add 'cider-esf--bounds-of-last-sexp :around 'evil-collection-cider-last-sexp)))))

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
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<"))

(use-package embark
  :defer t
  :straight t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
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
  :defer t
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :defer t
  :straight t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  :init
  (which-key-mode))

(use-package tabspaces
  :defer t
  ;; use this next line only if you also use straight, otherwise ignore it.
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-create-workspace
             tabspaces-create-new-project-and-workspace
             tabspaces-open-existing-project-and-workspace
             tabspaces-switch-workspace)
  :custom (tabspaces-use-filtered-buffers-as-default t))

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b")
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda ()
                      (tabspaces--tab-bar-buffer-name-filter ((lambda () (consult--buffer-query :sort 'visibility
                                                                                                  :as #'buffer-name))))))

    "Set workspace buffer list for consult-buffer.")
  (push consult--source-workspace consult-buffer-sources))

(defun embark-which-key-indicator ()
  "An embark indicator tha
t displays keymaps using which-key.
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
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :defer t
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

(use-package clang-format+
  :defer t
  :straight t
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :commands (dabbrev-completion
             dabbrev-expand)
  :straight t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package window
  :config
  (setq display-bffer-alist
        (append display-buffer-alist '(("\\*\\(cider-error\\|Backtrace\\)\\*"
                                        (display-buffer-in-side-window)
                                        (window-height . 0.25)
                                        (side . bottom)
                                        (slot . 1))
                                       ("\\*e?shell\\*"
                                        (display-buffer-in-side-window)
                                        (window-height . 0.25)
                                        (side . bottom)
                                        (slot . 0))
                                       ("\\*repl\\*"
                                        (display-buffer-in-side-window)
                                        (window-height . 0.25)
                                        (side . bottom)
                                        (slot . -1))))))


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

  (add-to-list 'default-frame-alist '(font . "Hack-12" ))
  (set-face-attribute 'default t :font "Hack" )
  (set-face-attribute 'default nil :font "Hack-12")
  (set-frame-font "Hack-12" nil t)
  (set-face-font 'fixed-pitch-serif "Hack")
  (set-face-font 'variable-pitch "Hack")

  ;; Prefer to load the more recent version of a file
  (setq load-prefer-newer t)

  ;; Keybindings
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)

  ;; Set line numbers in the buffer
  (global-display-line-numbers-mode t)

  ;; Use ibuffer instead of the usual buffer menu
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq minibuffer-complete-and-exit 'after-completion)

  ;; Set tab width to 4
  (setq tab-width 4)
  ;; Set file encoding to linux
  (prefer-coding-system 'utf-8-unix)

  ;; set default find program from git
  (when (and (eq system-type 'windows-nt)
             (executable-find "git.exe"))
    (setq find-program (prin1-to-string
                        (concat
                         (file-name-directory (executable-find "git.exe"))
                         "../usr/bin/find.exe"))))

  ;; DCI configuration for windows
  (when (and (eq system-type 'windows-nt)
             (file-directory-p "C:/Git/dci-emacs"))
    (let ((dci "C:/Git/dci-emacs/dci.el")
          (msvc "C:/Git/dci-emacs/msvc.el")
          (gaming "C:/Git/dci-emacs/gaming.el"))
      (load-file dci)
      (load-file msvc)
      (load-file gaming)))

  (add-hook 'prog-mode-hook #'hs-minor-mode)

  ;; custom variables
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror))

(use-package profiler
  :defer t
  :commands (profiler-report-find-entry
             quit-window
             profiler-report-toggle-entry)
  :after evil
  :config
  (evil-set-initial-state 'profiler-report-mode 'normal)
  (evil-define-key '(normal motion) 'profiler-report-mode-map (kbd "RET")   'profiler-report-find-entry)
  (evil-define-key '(normal motion) 'profiler-report-mode-map (kbd "q")     'quit-window)
  (evil-define-key '(normal motion) 'profiler-report-mode-map (kbd "<tab>") 'profiler-report-toggle-entry))

(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file "~/.emacs_bookmarks"))

(use-package dired
  :defer t
  :commands (dired-find-file
             dired-up-directory)
  :after evil
  :config
  ;; prevent for creating new buffers for each folder.
  (setf dired-kill-when-opening-new-dired-buffer t)
  (evil-define-key '(normal motion) 'dired-mode-map (kbd "RET") 'dired-find-file)
  (evil-define-key '(normal motion) 'dired-mode-map (kbd "-")   'dired-up-directory))

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

;; Custom functions

;; Hooks for vc-next-action
(defun p-commit-filename ()
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

(defun p-insert-preamble (preamble)
"Insert the PREAMBLE (aka filepath:filename) in the git commit."
  (when (equal (buffer-name) "*vc-log*")
                   (insert preamble)))

(defun p-vc-log-advice (orig-fun &rest args)
  "Advice the 'vc-next-action' function with inser-preamble.
The arguments are ORIG-FUN (vc-next-action) and ARGS the argument
of 'vc-next-action'."
  (let ((preamble (p-commit-filename)))
    (apply orig-fun args)
    (p-insert-preamble preamble)))

;; Advicing vc-next-action
(advice-add 'vc-next-action :around #'p-vc-log-advice)

(defun p-open-config ()
  "Open this configuration."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun p-dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun p-unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defvar-local project-test-cmd nil
  "Function for testing the current project, ovveride it in the dir locals var.")

(defvar-local project-run-cmd nil
  "Function for running the current project, ovveride it in the dir locals var.")

(put 'project-run-cmd 'safe-local-variable 'string-or-null-p)
(put 'project-test-cmd 'safe-local-variable 'string-or-null-p)

(defun p-project-run-tests ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compile-command (or project-test-value
                            compile-command)))
    (call-interactively #'compile)))

(defun p-project-run ()
  "Run test in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (run-command project-run-value))
    (cl-flet ((prompt-text (lambda () (insert run-command))))
      (minibuffer-with-setup-hook #'prompt-text
          (call-interactively #'project-async-shell-command)))))

(defun p-search-for-word-at-point (start end)
  "Search for the current selected word.
If there is no selected word, simply start an empty search.
START: beggining of the selected region.
END: end of the selected region."
  (interactive "r")
  (let* ((region (if (use-region-p)
                     (buffer-substring start end) "")))
    (consult-ripgrep (project-root (project-current t)) region)))


(defun p-update-cpp-etags ()
  "Update the project etgas for a cpp project."

  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (shell-command-to-string "fd --path-separator \"/\" \"\\.(h|cpp|hpp|cxx|c)$\" -X etags -a")))

(defun p-consult-grep-folder ()
  (interactive)
  (consult-ripgrep default-directory))

;;; Outdated or only for reference
;; Set shell file name using WSL
(setq shell-file-name "C:/Windows/system32/bash.exe")
;; (setenv "ESHELL" "bash")

(provide 'init)
;;; init.el ends here
