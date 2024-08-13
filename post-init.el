;;; post-init.el --- Post Init file -*- no-byte-compile: t; lexical-binding: t; -*-

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)


;; I use straight here because transient used in Emacs is too old
;; to support the master version of magit
(straight-use-package 'use-package)
(straight-use-package 'org)
(straight-use-package 'transient)
(straight-use-package
   '(nano :type git :host github :repo "rougier/nano-emacs"))

(use-package transient
  :straight t
  :demand t)

;; Load org as early as possible to avoid any incompatibilities
(use-package org
  :straight t
  :demand t)

;; The Garbage Collector Magic Hack (gcmh-mode) optimizes
;; Emacs'garbage-collection-messages process, reducing the frequency
;; of garbage collection during normal operations and only performing
;; it during idle times. This results in smoother performance and fewer
;; interruptions, especially during intensive tasks or when working
;; with large files.
(use-package gcmh
  :straight t
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold))

;; The auto-compile package automates the byte-compilation of Emacs Lisp files,
;; ensuring that your code runs more efficiently by converting it to byte-code.
;; This process reduces the load time and execution time of your Emacs
;; configuration and other Lisp files, leading to faster performance.
;; Additionally, auto-compile helps maintain an up-to-date and optimized
;; configuration by recompiling files automatically when they are saved,
;; eliminating the need for manual compilation and minimizing potential
;; issues caused by outdated byte-code.
(use-package auto-compile
  :demand t
  :custom
  (auto-compile-check-parens nil)
  (auto-compile-display-buffer nil)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


(defvar wsl (string-match "-[Mm]icrosoft" operating-system-release)
  "The only way to easilly distinguish if we are running on WSL or Linux")

(defvar et-system-type (or wsl system-type)
  "The system type of the current machine.")

(defvar et-emacs-files-dir  "~/.emacs_files/"
  "The directory where all the Emacs packages files are stored.")

;; Set default theme
(load-theme 'modus-vivendi t)

;; Need to check theese parameters
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


;; don't hide the line feed type
(setq inhibit-eol-conversion t)
;; Hide-show minnor mode for code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; I want pop windows
(setq pop-up-windows t)
;; Use window move
(windmove-default-keybindings)
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
(setq garbage-collection-messages t)


(defvar et-elisp-dir (concat minimal-emacs-user-directory "elisp/"))
(defvar secrets-file (concat et-elisp-dir "env/secrets.el"))

;; Load enviroment file for this computer based on the hostname
(load-file (concat et-elisp-dir "env/" (system-name) ".el"))
(load-file secrets-file)

;; Configure Emacs packages
(minimal-emacs-load-user-init "internal-package-config.el")

(server-start)
(provide 'post-init)
