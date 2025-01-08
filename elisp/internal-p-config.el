;;; internal-package-config.el --- Internal Package Config -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package info
  :ensure nil
  :after (meow)
  :defer t
  :bind
  (:map Info-mode-map
    ("-" . Info-up)
    ("n" . Info-next)
    ("p" . Info-prev))
  :config
  (eval `(bind-keys :map Info-mode-map ,@meow-normal-movement)))

(use-package debug
  :ensure nil
  :defer t
  :config
  (eval `(bind-keys :map debugger-mode-map ,@meow-normal-movement)))

(use-package simple
  :ensure nil
  :after (meow)
  :defer t
  :config
  (eval `(bind-keys :map messages-buffer-mode-map ,@meow-normal-movement)))

(use-package grep
  :ensure nil
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
  :ensure nil
  :defer t
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :config
  (defvar xref-rg-command "rg <C> --null -nH --no-heading --no-messages -g '!*/' -g 'TAGS' -e <R>")
  (defvar xargs-max-chars (if (memq system-type '(windows-nt ms-dos)) "-s 10000 " ""))

  ;; Use faster search tool but don't searg in the TAGS file
  (when-let ((has-rg? (executable-find "rg"))
             (et-xref-rg-command (concat "xargs -0 "xargs-max-chars xref-rg-command)))
    (add-to-list 'xref-search-program-alist `(ripgrep .  ,et-xref-rg-command))
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))


(use-package artist
  :ensure nil
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
  :ensure nil
  :defer t
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
  :ensure nil
  :defer t
  :config
  ;;; add element to project-switch-commands alist
  (defun project-magit-status ()
    (interactive)
    (magit-status (project-root (project-current t))))
  (add-to-list 'project-switch-commands '(project-magit-status "Magit Status" ?m))
  (add-to-list 'project-switch-commands '(project-compile "Compile Project" ?c))

  (defun project-keep-dir-open (dir)
    (dired-other-window dir))

  (advice-add 'project-switch-project :after 'project-keep-dir-open))


(use-package window
  :ensure nil
  :config
  (defvar et-no-display-buffer "no-display"
    "Hidden buffer name")

  (defvar original-display-buffer-alist display-buffer-alist
    "Save the original value for debugging")

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
    (defun et-split-window-right ()
      "Split window right and move to the new window"
      (interactive)
      (split-window-right)
      (windmove-right))

    (defun et-split-window-below ()
      "Split window below and move to the new window"
      (interactive)
      (split-window-below)
      (windmove-down)))


(use-package hexl
  :ensure nil
  :defer t
  :config
  (setq hexl-bits 8))


(use-package eww
  :ensure nil
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
  (defun eww--rename-buffer-hook-function (name)
    "Rename the eww buffer to the title of the page"
    (let ((function-name (make-symbol (concat "eww--rename-buffer-hook-function-" name))))
    `(defun ,function-name ()
        (rename-buffer ,name)
        (remove-hook 'eww-after-render-hook ',function-name)))))

(use-package dired
  :ensure nil
  :defer t
  :bind
  (:map dired-mode-map
   ("-" . dired-up-directory))
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  ;; prevent for creating new buffers for each folder.
  (setf dired-kill-when-opening-new-dired-buffer t)
  ;; easilly copy to other windows
  (setq dired-dwim-target t))


(use-package replace
  :ensure nil
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
  :after (meow)
  :defer t
  :ensure nil
  :bind
  (:map compilation-mode-map
    ("g" . recompile))
  :config
  (eval `(bind-keys :map compilation-mode-map ,@meow-normal-movement))

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


(use-package hippie-exp
  :ensure nil
  :defer t
  :config
  (setq hippie-expand-try-functions-list
        (remove 'try-expand-line (remove 'try-expand-list hippie-expand-try-functions-list))))


(use-package nxml-mode
  :ensure nil
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


(use-package edebug
  :defer t
  :bind
  (:map edebug-mode-map
        ("<f10>" . edebug-step-mode)
        ("<f11>" .  edebug-step-in)))


(use-package c-ts-mode
  :defer t
  :config

  (dir-locals-set-class-variables 'et-dci-project
                                  '((c++-ts-mode . ((fill-column . 50)))))

  (dir-locals-set-directory-class
   (concat et-git-directory "dci_windows") 'et-dci-project)

  (setq c-ts-mode-indent-offset 4)
  (setq-local indent-tabs-mode nil)

  (setq treesit--indent-verbose t) ;; uncomment to debug indentation

  ;; limit xref search to only soruce files
  (setq xref-ripgrep-args '("--type-add" "source=*.{c,cpp,py,js}" "--type" "source"))

  (setq c-ts-mode-indent-style #'et-indent-style)

  (defun et-update-tags (directory)
    (interactive
     (list (read-string "Enter directory name: " (if (project-current)
                                                     (project-root (project-current))
                                                     default-directory))))
    (let ((default-directory directory)
          (tag-file "TAGS"))
      (shell-command (concat "rm -rf TAGS; fd \".(cpp|h|c|cxx|hxx|mm)$\" -X ctags -e -a -f"  (expand-file-name tag-file))))))


(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :custom
  (eglot-report-progress nil)  ; Prevent minibuffer spam

  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))


(use-package ibuffer
  :ensure nil
  :defer t
  :custom
  (ibuffer-saved-filter-groups '(("work"
                                   ("TINMAN"
                                     (filename . "Git/thetinman"))
                                   ("DCI"
                                     (filename . "Git/dci"))
                                   ("MOTOMOTO"
                                     (filename . "motomoto"))
                                   ("ORG-MODE"
                                     (mode . org-mode))))))


(use-package emacs
  :bind
  ("<f1>" . read-only-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(py\\|pyi\\)\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(json\\|jsonnet\\)\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ino\\)\\'" . c++-ts-mode)))

(use-package gud
  :ensure nil
  :defer t
  :config
  (when (not (executable-find "gdb"))
    (defun gud-setup-lldb ()
      (gud-def gud-break "b %d%f:%l" "\C-b" "Set breakpoint at current line."))
    (add-hook 'gdb-mode-hook 'gud-setup-lldb))
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(use-package cc-mode
  :defer t
  :config
  (setq delete-trailing-lines nil))


(use-package vc
  :ensure nil
  :defer t
  :config
  (advice-add 'vc-next-action :around #'et-vc-log-advice))

(use-package eshell
  :ensure nil
  :defer t
  :config

  (defun fancy-shell ()
    "A pretty shell with git status"
    (let* ((cwd (abbreviate-file-name (eshell/pwd)))
            (ref (magit-get-shortname "HEAD"))
            (stat (magit-file-status))
            (x-stat eshell-last-command-status))
      (propertize
        (format "%s %s $ "
          (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "red")) x-stat)
            (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "red" "green"))))
          (propertize cwd 'font-lock-face '(:foreground "#45babf")))
        ; 'read-only t
        'front-sticky   '(font-lock-face read-only)
        'rear-nonsticky '(font-lock-face read-only))))

  (setopt eshell-prompt-function 'fancy-shell)
  (setopt eshell-prompt-regexp "^[^#$\n]* [#$] ")
  (setopt eshell-highlight-prompt nil))

(provide 'internal-package-config)
