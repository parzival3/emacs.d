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


(remove-hook 'find-file-hooks 'vc-find-file-hook)
(remove-hook 'find-file-hooks 'vc-refresh-state)

(setq vc-handled-backends '(Git))
(when (file-directory-p "C:\\Tools\\Git\\bin")
    (setq vc-git-program "C:\\Tools\\Git\\bin\\git.exe"))

;; I use straight here because transient used in Emacs is too old
;; to support the master version of magit
(use-package transient
  :straight t
  :demand t)

;; Load org as early as possible to avoid any incompatibilities
(use-package org
  :straight t
  :demand t)

(use-package emacs
  :init
  (defvar wsl (string-match "-[Mm]icrosoft" operating-system-release))
  (defvar et-system-type (if (eq wsl nil)
                             system-type
                           'wsl)
    "The system type of the current machine.")
  (defvar et-emacs-files-dir  "~/.emacs_files/"
    "The directory where all the Emacs packages files are stored.")
  :config

  ;; install the nano emacs configuration
  (straight-use-package
   '(nano :type git :host github :repo "rougier/nano-emacs"))
  (load-theme 'modus-vivendi t)
  ;;(require 'nano-layout)
  (require 'nano-defaults)

  (setq default-frame-alist
        (append (list
                 '(font . "Roboto Mono:style=Light:size=18")
	             '(min-height . 1) '(height    . 45)
	             '(min-width  . 1) '(width      . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 1)
                 '(left-fringe    . 24)
                 '(right-fringe   . 24)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))

  ;; nano disable popup windows, but I want the
  (setq pop-up-windows t)

  ;; Set file encoding to linux
  (prefer-coding-system 'utf-8-unix)

  ;; don't hide the line feed type
  (setq inhibit-eol-conversion t)

  ;; Hide-show minnor mode for code folding
  (add-hook 'prog-mode-hook #'hs-minor-mode)

  ;; Use window move
  (windmove-default-keybindings)

  ;; custom variables
  (setq custom-file (concat et-emacs-files-dir "custom.el"))
  (load custom-file 'noerror)

  ;; backups
  (setq backup-directory-alist `(("." . ,(concat et-emacs-files-dir "backups"))))

  ;; autosave
  (setq auto-save-list-file-prefix (concat et-emacs-files-dir "autosave/.saves-"))

  ;; session
  (setq session-save-file (concat et-emacs-files-dir "session/.session"))

  ;; eln files
  (setq eln-cache-dir (concat et-emacs-files-dir "eln-cache"))

  ;; save windows configuration by default
  (winner-mode 1)

  ;; miximum compilation speed for elisp
  (setq native-comp-speed 3)

  ;; print message for garbage collection
  (setq garbage-collection-messages t))


(use-package eshell
  :defer t
  :config
  (setq eshell-directory-name (concat et-emacs-files-dir "eshell/")))


(use-package grep
  :ensure t
  :config
  (setq grep-highlight-matches t
        grep-scroll-output t)

  ;; use rg instead of grep
  (grep-apply-setting
     'grep-use-null-device nil)
  (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
  (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
  (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
  (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))


(use-package xref
  :defer t
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read)

  (setq xref-ripgrep-args '("--type-add" "source=*.{c,cpp,py}" "--type" "source")))


(use-package artist
  :defer t
  :bind
  (:map artist-mode-map ("C-c C-a C-o" . 'et-select-artist-operation)
                        ("C-c C-a C-c" . 'et-select-artist-settings))
  :config
     (defun et-select-artist-operation (type)
     "Use ido to select a drawing operation in artist-mode"
     (interactive (list (completing-read "Drawing operation: "
                                             (list "Pen" "Pen Line" "line" "straight line" "rectangle"
                                                   "square" "poly-line" "straight poly-line" "ellipse"
                                                   "circle" "text see-thru" "text-overwrite" "spray-can"
                                                   "erase char" "erase rectangle" "vaporize line" "vaporize lines"
                                                   "cut rectangle" "cut square" "copy rectangle" "copy square"
                                                   "paste" "flood-fill"))))
     (artist-select-operation type))
     (defun et-select-artist-settings (type)
     "Use ido to select a setting to change in artist-mode"
     (interactive (list (completing-read "Setting: "
                                             (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
                                                   "Rubber-banding" "Trimming" "Borders"))))
     (if (equal type "Spray-size")
       (artist-select-operation "spray set size")
       (call-interactively (artist-fc-get-fn-from-symbol
			    (cdr (assoc type '(("Set Fill" . set-fill)
					       ("Set Line" . set-line)
					       ("Set Erase" . set-erase)
					       ("Rubber-banding" . rubber-band)
					       ("Trimming" . trimming)
					       ("Borders" . borders)
					       ("Spray-chars" . spray-chars)))))))))


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


(use-package eglot
  :defer t
  :straight t
  :config
  (global-set-key (kbd "C-x C-.") 'eglot-code-actions)
  (setq eglot-events-buffer-size 0))


(use-package project
  :defer t
  :config
  (setq project-list-file (concat et-emacs-files-dir "projects.el"))
  ;;; add element to project-switch-commands alist

  (defun project-magit-status ()
    (interactive)
    (magit-status (project-root (project-current t))))
  (add-to-list 'project-switch-commands '(project-magit-status "Magit Status" ?m))
  (add-to-list 'project-switch-commands '(project-compile "Compile Project" ?c))

  (defun project-keep-dir-open (dir)
    (dired-other-window dir))

  (advice-add 'project-switch-project :after 'project-keep-dir-open))


(use-package transient
  :config
  (setq transient-levels-file (concat et-emacs-files-dir "transient/levels.el"))
  (setq transient-values-file (concat et-emacs-files-dir "transient/values.el"))
  (setq transient-history-file (concat et-emacs-files-dir "transient/history.el")))


(use-package tramp
  :defer t
  :config
  (setq tramp-compat-temporary-file-directory (concat et-emacs-files-dir "tramp/temp"))
  (setq tramp-persistency-file-name (concat et-emacs-files-dir "tramp/tramp")))


(use-package saveplace
  :defer t
  :config
  (setq save-place-file (concat et-emacs-files-dir "places")))


(use-package window
  :config

  (defvar et-no-display-buffer "no-display")

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
               '("\\*\\(e?shell\\|vterm\\|eat\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.33)
                 ,@display-buffer-base-params))

  (add-to-list 'display-buffer-alist
               '("\\*\\(no-display\\)\\*"
                 (display-buffer-no-window)))

  (add-to-list 'display-buffer-alist
               '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\|Compilation\\|Spray Temp\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0)
                 (window-parameters
                  (no-delete-other-windows . nil))))

  (add-to-list 'display-buffer-alist
               '("\\*\\(Warnings\\)\\*"
                 (display-buffer-in-side-window)
                 (windowpbr_wan_4_dst_ip_user-height . 0.05)
                 (side . bottom)
                 (slot . 0)
                 (window-parameters
                  (no-delete-other-windows . nil))))


  ;; convenience functions for splitting windows
    (defun et-split-window-right-and-move-there-dammit ()
      "Split window right and move to the new window"
      (interactive)
      (split-window-right)
      (windmove-right))

    (defun et-split-window-below-and-move-there-dammit ()
      "Split window below and move to the new window"
      (interactive)
      (split-window-below)
      (windmove-down)))


(use-package hexl
  :defer t
  :config
  (setq hexl-bits 8))


(use-package eww
  :defer t
  :bind
  (:map eww-mode-map
        ("L" . eww-forward-url)
        ("H" . eww-back-url)
        ("l" . meow-right)
        ("h" . meow-left)
        ("j" . meow-up)
        ("k" . meow-down)
        ("x" . meow-line)
        ("y" . meow-clipboard-save)
        ("," . meow-inner-of-thing)
        ("Q" . meow-goto-line))
  :config
  (setq eww-bookmarks-directory (concat et-emacs-files-dir "eww/"))

  (defun eww--rename-buffer-hook-function (name)
    "Rename the eww buffer to the title of the page"
    (let ((function-name (make-symbol (concat "eww--rename-buffer-hook-function-" name))))
    `(defun ,function-name ()
        (rename-buffer ,name)
        (remove-hook 'eww-after-render-hook ',function-name)))))


(use-package url-cookie
  :defer t
  :config
  (setq url-cookie-file (concat et-emacs-files-dir "url/cookies")))


(use-package url-cache
  :defer t
  :config
  (setq url-cache-directory (concat et-emacs-files-dir "url/cache")))


(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat et-emacs-files-dir "emacs_bookmarks")))


(use-package server
  :config
  (setq server-auth-dir (concat et-emacs-files-dir "server/")))


(use-package dired
  :defer t
  :bind
  (:map dired-mode-map
   ("-" . dired-up-directory))
  :config
  ;; prevent for creating new buffers for each folder.
  (setf dired-kill-when-opening-new-dired-buffer t)
  ;; easilly copy to other windows
  (setq dired-dwim-target t))


(use-package replace
  :defer t
  :config
  (defun get-buffers-matching-mode (mode)
    "Returns a list of buffers where their major-mode is equal to MODE"
    (let ((buffer-mode-matches '()))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq mode major-mode)
            (push buf buffer-mode-matches))))
      buffer-mode-matches))


  (defun multi-occur-in-this-mode ()
    "Show all lines matching REGEXP in buffers with this major mode."
    (interactive)
    (multi-occur
     (get-buffers-matching-mode major-mode)
     (car (occur-read-primary-args)))))


(use-package compile
  :ensure t
  :bind (:map compilation-mode-map
              ("w" . meow-mark-word)
              ("e" . meow-next-word)
              ("b".  meow-back-word)
              ("l" . meow-right)
              ("h" . meow-left)
              ("y" . platform-copy)
              ("s" . platform-cut)
              ("x" . meow-line))
  :config
  (setq compilation-scroll-output t)
  (setq compilation-auto-jump-to-first-error t)
  ;; How to debug compilation regex alist
  ;; (setq compilation-debug 't)
  ;; And then eval this line in the matching error
  ;; (car (aref  (car (get-text-property (point) 'compilation-debug)) 1))
  ;; Add them to the dir-locals, for example flutter
  ;; ((nil . ((eval . (setq compilation-error-regexp-alist
  ;;                     (thread-last compilation-error-regexp-alist
  ;;                                  (remove 'guile-line)
  ;;                                  (remove 'ada)))))))
  )


(use-package xref
  :defer t
  :config
  (setq xref-search-program 'ripgrep))


(use-package hippie-exp
  :defer t
  :config
  (setq hippie-expand-try-functions-list
        (remove 'try-expand-line (remove 'try-expand-list hippie-expand-try-functions-list))))


(use-package recentf
  :defer t
  :config
  (setq recentf-save-file (concat et-emacs-files-dir "recentf")))


(use-package savehist
  :defer t
  :config
  (setq savehist-file (concat et-emacs-files-dir "savehist")))


(use-package nxml-mode
  :defer t
  :requires (sgml-mode hideshow)
  :bind (:map nxml-mode-map
              ("C-c h" . hs-toggle-hiding))

  :hook ((nxml-mode . hs-minor-mode))
  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))
  (setq nxml-slash-auto-complete-flag t))

(defvar et-elisp-dir (concat user-emacs-directory "elisp/"))
(defvar secrets-file (concat et-elisp-dir "env/secrets.el"))

;; Load enviroment file for this computer based on the hostname
(load-file (concat et-elisp-dir "env/" (system-name) ".el"))
(load-file secrets-file)

;; Load the keybidings configuration
(load-file (concat et-elisp-dir "kbd.el"))

(load-file (concat et-elisp-dir "packages.el"))

;; Load the org customization
(load-file (concat et-elisp-dir "org-config.el"))

;; Load the language packages
(load-file (concat et-elisp-dir "lang.el"))

(load-file (concat et-elisp-dir "utils.el"))

;; Load the operating system specific configuration at the end
;; so we can override any previous configuration
(when (or (eq system-type `gnu/linux)
          (eq system-type 'darwin))
  (load-file (concat et-elisp-dir "unix.el")))

(when (eq system-type 'windows-nt)
    (load-file (concat et-elisp-dir "dos.el")))


(use-package emacs
  :config
  (server-start))

;; (load-file (concat et-elisp-dir "appearance.el"))

;; (set-face 'fringe  'nano-face-faded)
;; (set-face-attribute 'fringe nil
;;                     :foreground (face-background 'nano-face-subtle)
;;                     :background (face-background 'default))

(provide 'init)
;;; init.el ends here
