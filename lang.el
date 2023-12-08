(use-package flyspell
  :straight t
  :config
  (setq ispell-program-name "aspell")
  :hook
  ((org-mode) . flyspell-mode))

(use-package clojure-mode
  :defer t
  :straight t)

(use-package carp
  :straight (el-patch :type git :host github :repo "carp-lang/carp-emacs")
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode)))

(use-package dart-mode
  :straight t)

(use-package flutter
  :straight t
  :config
  (add-hook #'dart-mode-hook #'eglot-ensure)
  (setq flutter-buffer-name "*Flutter-Runner*"))

(use-package rust-mode
  :straight t)

(use-package zig-mode
  :straight t)

(use-package clang-format+
  :defer t
  :straight t
  :config
  (defun dired-clang-format-thing ()
    (interactive)
    (let ((list-of-files (dired-get-marked-files)))
      (while list-of-files
        (let ((current-file (pop list-of-files)))
          (if (file-name-directory current-file)
              (dired-run-shell-command (format "find %s -iname *.cpp -o -iname *.h | xargs clang-format -i" (file-name-as-directory current-file)))
            (dired-run-shell-command (format "clang-format -i %s" current-file))))))))

(use-package cc-mode
  :config
  (setq delete-trailing-lines nil))

(use-package devdocs
  :straight t)

(use-package treesit
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(py\\|pyi\\)\\'" . python-ts-mode)))

(use-package c-ts-mode
  :config
  (setq c-ts-mode-indent-style 'k&r)
  (setq c-ts-mode-indent-offset 4)
  (setq-local indent-tabs-mode nil))

(defun c-ts-microsoft-style ()
  (let ((microsoft-style
         '(((node-is "}") parent-bol 0)
           ((node-is "labeled_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "labeled_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "compound_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "if_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "else_clause") parent-bol c-ts-mode-indent-offset)
           ((parent-is "for_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "while_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "switch_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "case_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "do_statement") parent-bol c-ts-mode-indent-offset))))
    ;; Merge with the existing 'linux' style
    (setq microsoft-style (cons 'microsoft-style (alist-get 'linux (c-ts-mode--indent-styles 'c++))))
    (print microsoft-style)
    microsoft-style))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  :hook
  (prog-mode . copilot-mode)
  :config
  (defun toggle-copilot-mode ()
  "Toggle Copilot mode for programming modes."
  (interactive)
  (if (bound-and-true-p copilot-mode)
      (progn
        (remove-hook 'prog-mode-hook 'copilot-mode)
        (setq copilot-mode nil)
        (message "Copilot mode disabled for programming mode"))
    (setq copilot-mode t)
    (add-hook 'prog-mode-hook 'copilot-mode)
    (message "Copilot mode enabled for programming mode")))
  (global-set-key (kbd "<f10>") #'toggle-copilot-mode))

(use-package powershell
  :straight t
  :config
  (define-key powershell-mode-map (kbd "M-'") #'powershell-quote-selection 'remove))

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((PowerShell . ("https://github.com/parzival3/tree-sitter-PowerShell.git"))
     ))
  :config
  (defun et-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package groovy-mode
  :straight t
  :config
  (defun groovy-remove-indentation ()
    ;; remove the electric indentation
    (add-hook 'hack-local-variable-hook
              (lambda () (setq indent-line-function #'identity))
              nil t))
  :hook
  (groovy-mode . #'groovy-remove-indentation))

(use-package yaml-mode
  :straight t)


(defvar et-format-functions-alist
  '((python-mode blacken-buffer blacken-region)
    (js-mode prettier-js prettier-js-region)
    (c++-mode clang-format-buffer clang-format-region)
    (c-mode clang-format-buffer clang-format-region)
    (c++-ts-mode clang-format-buffer clang-format-region)
    (c-ts-mode clang-format-buffer clang-format-region)
    ;; Add more modes and their associated formatting functions here
    )
  "Alist of major modes and their corresponding buffer and region formatting functions.")

(defun et-format-code-or-region (start end)
  "Formats the current buffer or a region based on the major mode.
If START and END are provided, format that region."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((buffer-formatter (nth 1 (assoc major-mode et-format-functions-alist)))
        (region-formatter (nth 2 (assoc major-mode et-format-functions-alist))))
    (if start
        (if region-formatter
            (funcall region-formatter start end)
          (indent-region start end))
      (if buffer-formatter
          (funcall buffer-formatter)
        (indent-region (point-min) (point-max))))))
