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
  (setq nano-font-size 12)
  (require 'nano)
  (nano-theme-set-dark)
  (call-interactively 'nano-refresh-theme)

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
  )

(use-package eshell
  :config
  (setq eshell-directory-name (concat et-emacs-files-dir "eshell/")))

(use-package grep
  :defer t
  :config
  (setq grep-highlight-matches t)
  (setq grep-scroll-output t)
  ;; use rg instead of grep
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(use-package artist
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


(use-package project
  :config
  (setq project-list-file (concat et-emacs-files-dir "projects.el"))
  ;;; add element to project-switch-commands alist
  (defun project-magit-status ()
    (interactive)
    (magit-status (project-root (project-current t))))
  (add-to-list 'project-switch-commands '(project-magit-status "Magit Status" ?m)))


(use-package transient
  :config
  (setq transient-levels-file (concat et-emacs-files-dir "transient/levels.el"))
  (setq transient-values-file (concat et-emacs-files-dir "transient/values.el"))
  (setq transient-history-file (concat et-emacs-files-dir "transient/history.el")))

(use-package tramp
  :config
  (setq tramp-compat-temporary-file-directory (concat et-emacs-files-dir "tramp/temp"))
  (setq tramp-persistency-file-name (concat et-emacs-files-dir "tramp/tramp")))

(use-package saveplace
  :config
  (setq save-place-file (concat et-emacs-files-dir "places")))

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
               '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\|Compilation\\|Spray Temp\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0)
                 (window-parameters
                  (no-delete-other-windows . nil)))))


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
        ("y" . meow-clipboard-save)
        ("," . meow-inner-of-thing)
        ("Q" . meow-goto-line))
  :config
  (set-face 'shr-text 'nano-face-default)
  (setq eww-bookmarks-directory (concat et-emacs-files-dir "eww/"))

  (defun eww--rename-buffer-hook-function (name)
    "Rename the eww buffer to the title of the page"
    (let ((function-name (make-symbol (concat "eww--rename-buffer-hook-function-" name))))
    `(defun ,function-name ()
        (rename-buffer ,name)
        (remove-hook 'eww-after-render-hook ',function-name)))))


(use-package url-cookie
  :config
  (setq url-cookie-file (concat et-emacs-files-dir "url/cookies")))


(use-package url-cache
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
  (setq dired-dwim-target t)
  (set-face dired-directory-face 'nano-face-popout))


(use-package replace
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
  :defer t
  :bind (:map compilation-mode-map
              ("l" . meow-right)
              ("h" . meow-left)
              ("y" . platform-copy)
              ("s" . platform-cut)
              ("x" . meow-line))
  :config
  (setq compilation-scroll-output t)
  (set-face 'compilation-error 'nano-face-header-critical)
  (setq compilation-auto-jump-to-first-error t))


(use-package xref
  :defer t
  :config
  (setq xref-search-program 'ripgrep))


(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        (remove 'try-expand-line (remove 'try-expand-list hippie-expand-try-functions-list))))


(use-package recentf
  :config
  (setq recentf-save-file (concat et-emacs-files-dir "recentf")))


(use-package savehist
  :config
  (setq savehist-file (concat et-emacs-files-dir "savehist")))


(defvar et-elisp-dir (concat user-emacs-directory "elisp/"))
(defvar secrets-file (concat et-elisp-dir "env/secrets.el"))

;; Load enviroment file for this computer based on the hostname
(load-file (concat et-elisp-dir "env/" (system-name) ".el"))
(load-file secrets-file)

;; Load the keybidings configuration
(load-file (concat et-elisp-dir "kbd.el"))

;; Load the language packages
(load-file (concat et-elisp-dir "lang.el"))

;; Load the org customization
(load-file (concat et-elisp-dir "org-config.el"))

(load-file (concat et-elisp-dir "packages.el"))

(load-file (concat et-elisp-dir "utils.el"))

;; Load the operating system specific configuration at the end
;; so we can override any previous configuration
(when (or (eq system-type `gnu/linux)
          (eq system-type 'darwin))
  (load-file (concat et-elisp-dir "unix.el")))

(when (eq system-type 'windows-nt)
    (load-file (concat et-elisp-dir "dos.el")))


(provide 'init)
;;; init.el ends here
