;;; internal-package-config.el --- Internal Package Config -*- no-byte-compile: t; lexical-binding: t; -*-

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
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read)

  (setq xref-ripgrep-args '("--type-add" "source=*.{c,cpp,py}" "--type" "source")))


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
  :ensure nil
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
