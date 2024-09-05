;;; post-init.el --- Post Init file -*- no-byte-compile: t; lexical-binding: t; -*-

;; I use straight here because transient used in Emacs is too old
;; to support the master version of magit
(use-package transient
  :demand t)

;; Load org as early as possible to avoid any incompatibilities
(use-package org
  :demand t)

;; The Garbage Collector Magic Hack (gcmh-mode) optimizes
;; Emacs'garbage-collection-messages process, reducing the frequency
;; of garbage collection during normal operations and only performing
;; it during idle times. This results in smoother performance and fewer
;; interruptions, especially during intensive tasks or when working
;; with large files.
(use-package gcmh
  :defer 10
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold)
  :config
  ;; manually activating it instead of the :hook keyworkd, because I want to defer the loading to a 30 seconds delay
  (gcmh-mode 1))

;; The auto-compile package automates the byte-compilation of Emacs Lisp files,
;; ensuring that your code runs more efficiently by converting it to byte-code.
;; This process reduces the load time and execution time of your Emacs
;; configuration and other Lisp files, leading to faster performance.
;; Additionally, auto-compile helps maintain an up-to-date and optimized
;; configuration by recompiling files automatically when they are saved,
;; eliminating the need for manual compilation and minimizing potential
;; issues caused by outdated byte-code.
(use-package auto-compile
  :defer 10
  :custom
  (auto-compile-check-parens nil)
  (auto-compile-display-buffer nil)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


(use-package emacs
  :ensure nil
  :init
  (defvar wsl (string-match "-[Mm]icrosoft" operating-system-release)
    "The only way to easilly distinguish if we are running on WSL or Linux")

  (defvar et-system-type (or (when wsl 'wsl) system-type)
    "The system type of the current machine.")

  (defvar et-emacs-files-dir  "~/.emacs_files/"
    "The directory where all the Emacs packages files are stored.")

  (defvar et-elisp-dir (concat minimal-emacs-user-directory "elisp/"))
  (defvar secrets-file (concat et-elisp-dir "env/secrets.el"))

  ;; Load enviroment file for this computer based on the hostname
  (defvar et-machine-config (or
                             ;; Normally I store the computer specific config based on the hostname
                             (when-let* ((hostname-config (concat et-elisp-dir "env/" (system-name) ".el"))
                                        (file-exists? (file-exists-p hostname-config)))
                               hostname-config)
                             ;; Check for computer name on macos since at work hostname changes based on the last IP on
                             ;; on the network
                             (when-let* ((macos? (eq system-type 'darwin))
                                         (computer-name-config
                                          (concat et-elisp-dir
                                                  "env/"
                                                  (string-trim-right (shell-command-to-string "scutil --get ComputerName"))
                                                  ".el"))
                                         (file-exists? (file-exists-p computer-name-config)))
                               computer-name-config)
                             (error "Couldn't determin the machine configuration")))

  :bind
  (("M-<up>" . enlarge-window)
   ("M-<down>" . shrink-window)
   ("M-<left>" . shrink-window-horizontally)
   ("M-<right>" . enlarge-window-horizontally)
   ("<xterm-paste>" . scroll-up-command)
   ("C-x C-b" . ibuffer))
  :hook
  (
   ;; Auto-revert in Emacs is a feature that automatically updates the
   ;; contents of a buffer to reflect changes made to the underlying file
   ;; on disk.
   (after-init . global-auto-revert-mode)
   ;; recentf is an Emacs package that maintains a list of recently
   ;; accessed files, making it easier to reopen files you have worked on
   ;; recently.
   (after-init . recentf-mode)
   ;; savehist is an Emacs feature that preserves the minibuffer history between
   ;; sessions. It saves the history of inputs in the minibuffer, such as commands,
   ;; search strings, and other prompts, to a file. This allows users to retain
   ;; their minibuffer history across Emacs restarts.
   (after-init . savehist-mode)
   ;; save-place-mode enables Emacs to remember the last location within a file
   ;; upon reopening. This feature is particularly beneficial for resuming work at
   ;; the precise point where you previously left off.
   ('after-init . save-place-mode)
   ;; proper trimming of white spaces when the encoding of the file is dos
   ('before-save . et-trim-whitespace-based-on-encoding)
   ;; Hide-show minnor mode for code folding
   ('prog-mode . hs-minor-mode))
  :config
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

  ;; I want pop windows
  (setq pop-up-windows t)
  ;; Use window move
  (windmove-default-keybindings)
  ;; save windows configuration by default
  (winner-mode 1)
  ;; miximum compilation speed for elisp
  (setq native-comp-speed 3)

  ;; Set lisp indentation
  (setq lisp-indent-offset 2))

;; Load enviroment file for this computer based on the hostname
(load-file et-machine-config)
(load-file secrets-file)

;; HACK: Fix problems of signatures
;; (setq package-check-signature nil)

;;Configure Emacs packages
(load-file (concat et-elisp-dir "internal-p-config.el"))
;; Configure External packages
(load-file (concat et-elisp-dir "minimal-p-config.el"))

;; Load the keybidings configuration
(load-file (concat et-elisp-dir "kbd-p-config.el"))

(load-file (concat et-elisp-dir "casual-p-config.el"))

;; Load the operating system specific configuration at the end
;; so we can override any previous configuration
(when (or (eq system-type `gnu/linux)
          (eq system-type 'darwin))
  (load-file (concat et-elisp-dir "os/unix.el")))

(when (eq system-type 'darwin)
  (load-file (concat et-elisp-dir "os/macos.el")))

(when (eq system-type 'windows-nt)
    (load-file (concat et-elisp-dir "os/dos.el")))

(load-file (concat et-elisp-dir "other-p-config.el"))
(load-file (concat et-elisp-dir "lang-p-config.el"))

(eval-and-compile
  (defvar utils-package-dir (concat et-elisp-dir "utils")))

(use-package utils
  :preface
  (unless (seq-contains-p
            (directory-files utils-package-dir t "el")
            ".*autoloads.el$"
            (lambda (elem regex) (string-match regex elem nil t)))
    (package-generate-autoloads "utils" utils-package-dir))
  :defer t
  :load-path utils-package-dir
  :commands (et-other-window
             et-find-file
             et-split-compile
             et-open-config)
  :bind
  (("C-x o" . et-other-window)
   ("<f13>" . et-split-compile)))
