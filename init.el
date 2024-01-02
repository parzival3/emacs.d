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
  :config
  (straight-use-package
    '(nano :type git :host github :repo "rougier/nano-emacs"))
  (setq nano-font-size 12)
  (require 'nano)
  (nano-theme-set-dark)
  (call-interactively 'nano-refresh-theme)

 (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

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
  (set-face dired-directory-face 'nano-face-popout))


(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output t)
  (setq compilation-auto-jump-to-first-error t))


(use-package xref
  :defer t
  :config
  (setq xref-search-program 'ripgrep))


(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        (remove 'try-expand-line (remove 'try-expand-list hippie-expand-try-functions-list))))


;; Load enviroment file for this computer based on the hostname
(load-file (concat user-emacs-directory "env/" (system-name) ".el"))
(load-file secrets-file)

(defvar et-elisp-dir (concat user-emacs-directory "elisp/"))

;; Load the keybidings configuration
(load-file (concat et-elisp-dir "kbd.el"))

;; Load the language packages
(load-file (concat et-elisp-dir "lang.el"))

;; Load the org customization
(load-file (concat et-elisp-dir "org.el"))

(load-file (concat et-elisp-dir "packages.el"))

(load-file (concat et-elisp-dir "utils.el"))

(when (or (eq system-type `gnu/linux)
          (eq system-type 'darwin))
  (load-file (concat et-elisp-dir "unix.el")))

(when (eq system-type 'windows-nt)
    (load-file (concat et-elisp-dir "dos.el")))


(provide 'init)
;;; init.el ends here
