
(use-package emacs
  :config
  (require 'eshell)
  ;; configuring some environment variables
  (setenv "PATH" (concat "C:\\Tools\\LLVM\\bin;" (getenv "PATH")))
  (setenv "PATH" (concat "C:\\Tools\\Git\\bin;" (getenv "PATH")))
  (add-to-list 'exec-path "C:\\Tools\\LLVM\\bin")

  ;; (setenv "PYTHONPATH" "C:/Git/dci_windows/lib/
  (setenv "PLATFORM" "x64")
  (setenv "CONFIGURATION" "Debug")
  (setenv "EposPythonRoot" "c:/Tools/Python3.9/")

  (setenv "PATH" (concat (getenv "EposPythonRoot") ";" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "EposPythonRoot") "Scripts;" (getenv "PATH")))
  (add-to-list 'exec-path (getenv "EposPythonRoot"))
  (add-to-list 'exec-path (concat (getenv "EposPythonRoot") "Scripts"))
  (eshell/addpath (concat (getenv "EposPythonRoot") "Scripts"))
  (eshell/addpath (getenv "EposPythonRoot"))

  ;; set zig folder
  (setenv "ZIG_ROOT" "C:\\Tools\\zig\\")
  (add-to-list 'exec-path (getenv "ZIG_ROOT"))
  (setenv "PATH" (concat  (getenv "ZIG_ROOT") ";" (getenv "PATH")))
  (eshell/addpath (concat  (getenv "ZIG_ROOT")))

  ;; set putty path
  (setenv "PUTTY" "C:\\Tools\\putty\\")
  (add-to-list 'exec-path (getenv "PUTTY"))
  (setenv "PATH" (concat  (getenv "PUTTY") ";" (getenv "PATH")))
  (eshell/addpath (concat  (getenv "PUTTY")))

  ;; add 7zip
  (setenv "7ZIP" "C:\\Program Files\\7-Zip\\")
  (add-to-list 'exec-path (getenv "7ZIP"))
  (setenv "PATH" (concat  (getenv "7ZIP") ";" (getenv "PATH")))
  (eshell/addpath (getenv "7ZIP"))

  ;; Debug tools
  (setenv "DEBUGGERS" "C:\\Program Files (x86)\\Windows Kits\\10\\Debuggers\\x64\\")
  (add-to-list 'exec-path (getenv "DEBUGGERS"))
  (setenv "PATH" (concat  (getenv "DEBUGGERS") ";" (getenv "PATH")))
  (eshell/addpath (getenv "DEBUGGERS"))

  ;; Android
  (setenv "ANDROID_CMDLINE_TOOLS" "c:/Tools/android_sdk/cmdline-tools/latest/bin/")
  (setenv "ANDROID_HOME" "c:/Tools/android_sdk/")
  (setenv "PATH" (concat (getenv "ANDROID_CMDLINE_TOOLS") ";" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "ANDROID_HOME") "/tools/bin;" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "ANDROID_HOME") "/platform-tools;" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "ANDROID_HOME") "/emulator;" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "ANDROID_HOME") "/tools;" (getenv "PATH")))

  (add-to-list 'exec-path (getenv "ANDROID_CMDLINE_TOOLS"))
  (add-to-list 'exec-path (concat (getenv "ANDROID_HOME") "/tools/bin"))
  (add-to-list 'exec-path (concat (getenv "ANDROID_HOME") "/platform-tools"))
  (add-to-list 'exec-path (concat (getenv "ANDROID_HOME") "/emulator"))
  (add-to-list 'exec-path (concat (getenv "ANDROID_HOME") "/tools"))

  (eshell/addpath (getenv "ANDROID_CMDLINE_TOOLS"))
  (eshell/addpath (concat (getenv "ANDROID_HOME") "/tools/bin"))
  (eshell/addpath (concat (getenv "ANDROID_HOME") "/platform-tools"))
  (eshell/addpath (concat (getenv "ANDROID_HOME") "/emulator"))
  (eshell/addpath (concat (getenv "ANDROID_HOME") "/tools"))

  ;; DCI
  (setenv "DCI_REPOSITORY" "c:/Git/dci_windows/")

  ;; on windows we need to clean the clipboard before pasting
  (advice-add 'clipboard-yank :around #'et-clean-clipboard-yank)
  (advice-add 'yank :around #'et-clean-clipboard-yank)

  ;; TODO copy this command somewhere else msiexec /a "core_d (1).msi" /qb TARGETDIR=C:\Tools\Python3.11

  (when (file-directory-p "C:\\msys64\\user\\bin")
    (setq find-program "C:\\msys64\\user\\bin\\find.exe"))

  ;; allow mingw shell
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))


(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               `(c++-mode . ("c:/Tools/LLVM/bin/clangd.exe")))
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ("c:/Tools/Python3.11/Scripts/pylsp.exe"))))


(use-package python
  :config
  (setq python-shell-interpreter (concat (getenv "EposPythonRoot") "/python.exe")))


(use-package tramp
  :defer t
  :config
  (setq tramp-use-ssh-controlmaster-options nil)
  (add-to-list 'tramp-connection-properties
	         (list (regexp-quote "/ssh:")
		       "login-args"
		       '(("-tt") ("-l" "%u") ("-p" "%p") ("%c")
		         ("-e" "none") ("%h")))))


(use-package dired
  :config
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
  :config
  (setq compilation-scroll-output 'first-error)
  (setq compile-command "msbuild.exe"))


(use-package magit
  :defer t
  :config
  ;; simplify magit status headers
  (setq magit-status-headers-hook '(magit-insert-head-branch-header))
  (when (file-directory-p "C:\\Tools\\Git\\bin")
    (setq magit-git-executable "C:\\Tools\\Git\\bin\\git.exe"))

  ;; remove some magit status sections
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(load-file (concat et-elisp-dir "work/windows/cbd-gud.el"))

;; cdb commands http://www.windbg.info/doc/1-common-cmds.html
(use-package gud
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t)
  :bind
  (:map gud-minor-mode-map
        ("<f5>"  .  'gud-cont)
        ("<f7>"  .  'gud-tbreak)
        ("<f8>"  .  'gud-step)
        ("<f9>"  .  'gud-break)
        ("<f10>" .  'gud-next)
        ("<f11>" .  'gud-finish)))


(defun search-msdn-term ()
  "Search MSDN for the selected term or the term under the cursor."
  (interactive)
  (let* ((term (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t)))
         (query (if term
                    (url-encode-url term)
                  (user-error "No term found at point or selected"))))
    (browse-url (concat "https://learn.microsoft.com/en-us/search/?category=Documentation&terms=" query))))

(provide 'dos)
