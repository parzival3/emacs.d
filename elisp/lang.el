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
    (add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-ts-mode))
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
  (setq-local indent-tabs-mode nil)

  ;; (setq treesit--indent-verbose t) ;; uncomment to debug indentation

  (defvar ms-cpp-style '(((parent-is "compound_statement")
                          standalone-parent 0)
                         ((or (match nil "compound_statement" nil 1 1)
                              (match null "compound_statement"))
                          standalone-parent 0)
                         ((node-is "compound_statement") standalone-parent 0)
                         ))

  (defun add-my-indentation-style (orig-fun &rest args)
    (let* ((style (apply orig-fun args))
           (bsd-style (alist-get 'bsd style))
           (ms-style (append bsd-style ms-cpp-style)))
      (add-to-list 'style `(ms-style . ,ms-style))))

  (advice-add 'c-ts-mode--indent-styles :around #'add-my-indentation-style)
  (advice-remove 'c-ts-mode--indent-styles #'add-my-indentation-style)

  (defun et-ms-cpp-style ()
    `(
      ((node-is "}")
       parent-bol 0)
      ((parent-is "compound_statement")
       parent-bol c-ts-mode-indent-offset)
      ((or
        (match nil "compound_statement" nil 1 1)
        (match null "compound_statement"))
       standalone-parent 0)
      ((parent-is "compound_statement")
       standalone-parent 0)
      ((node-is "labeled_statement")
       parent-bol c-ts-mode-indent-offset)
      ((parent-is "labeled_statement")
       parent-bol c-ts-mode-indent-offset)
      (c-ts-mode--for-each-tail-body-matcher prev-line c-ts-mode-indent-offset)
      ((parent-is "translation_unit")
       column-0 c-ts-mode-indent-offset)
      ((query "(ERROR (ERROR)) @indent")
       column-0 0)
      ((node-is ")")
       parent 1)
      ((node-is "]")
       parent-bol 0)
      ((node-is "else")
       parent-bol 0)
      ((node-is "case")
       parent-bol 0)
      ((node-is "preproc_arg")
       no-indent)
      ((and
        (parent-is "comment")
        c-ts-common-looking-at-star)
       c-ts-common-comment-start-after-first-star -1)
      (c-ts-common-comment-2nd-line-matcher c-ts-common-comment-2nd-line-anchor 1)
      ((parent-is "comment")
       prev-adaptive-prefix 0)
      ((node-is "labeled_statement")
       standalone-parent 0)
      ((parent-is "labeled_statement")
       c-ts-mode--standalone-grandparent c-ts-mode-indent-offset)
      ((node-is "preproc")
       column-0 0)
      ((node-is "#endif")
       column-0 0)
      ((match "preproc_call" "compound_statement")
       column-0 0)
      ((n-p-gp nil "preproc" "translation_unit")
       column-0 0)
      ((and no-node
            (parent-is "\\(?:
\\|preproc\\)"))
       c-ts-mode--standalone-parent-skip-preproc c-ts-mode--preproc-offset)
      ((match nil "preproc_\\(?:\\(?:el\\)?if\\)" nil 3 3)
       c-ts-mode--standalone-parent-skip-preproc c-ts-mode-indent-offset)
      ((match nil "preproc_ifdef" nil 2 2)
       c-ts-mode--standalone-parent-skip-preproc c-ts-mode-indent-offset)
      ((match nil "preproc_else" nil 1 1)
       c-ts-mode--standalone-parent-skip-preproc c-ts-mode-indent-offset)
      ((parent-is "preproc")
       c-ts-mode--anchor-prev-sibling 0)
      ((parent-is "function_definition")
       parent-bol 0)
      ((parent-is "conditional_expression")
       first-sibling 0)
      ((parent-is "assignment_expression")
       parent-bol c-ts-mode-indent-offset)
      ((parent-is "concatenated_string")
       first-sibling 0)
      ((parent-is "comma_expression")
       first-sibling 0)
      ((parent-is "init_declarator")
       parent-bol c-ts-mode-indent-offset)
      ((parent-is "parenthesized_expression")
       first-sibling 1)
      ((parent-is "argument_list")
       first-sibling 1)
      ((parent-is "parameter_list")
       first-sibling 1)
      ((parent-is "binary_expression")
       parent 0)
      ((query "(for_statement initializer: (_) @indent)")
       parent-bol 5)
      ((query "(for_statement condition: (_) @indent)")
       parent-bol 5)
      ((query "(for_statement update: (_) @indent)")
       parent-bol 5)
      ((query "(call_expression arguments: (_) @indent)")
       parent c-ts-mode-indent-offset)
      ((parent-is "call_expression")
       parent 0)
      ((node-is "}")
       standalone-parent 0)
      ((node-is "access_specifier")
       parent-bol 0)
      ((parent-is "declaration_list")
       parent-bol c-ts-mode-indent-offset)
      ((match nil "initializer_list" nil 1 1)
       parent-bol c-ts-mode-indent-offset)
      ((parent-is "initializer_list")
       c-ts-mode--anchor-prev-sibling 0)
      ((match nil "enumerator_list" nil 1 1)
       standalone-parent c-ts-mode-indent-offset)
      ((parent-is "enumerator_list")
       c-ts-mode--anchor-prev-sibling 0)
      ((match nil "field_declaration_list" nil 1 1)
       standalone-parent c-ts-mode-indent-offset)
      ((parent-is "field_declaration_list")
       c-ts-mode--anchor-prev-sibling 0)

      ((parent-is "compound_statement")
       c-ts-mode--anchor-prev-sibling 0)
      ((match "expression_statement" nil "body")
       standalone-parent c-ts-mode-indent-offset)
      ((node-is "compound_statement")
       standalone-parent 0)
      ((parent-is "if_statement")
       standalone-parent c-ts-mode-indent-offset)
      ((parent-is "for_statement")
       standalone-parent c-ts-mode-indent-offset)
      ((parent-is "while_statement")
       standalone-parent c-ts-mode-indent-offset)
      ((parent-is "do_statement")
       standalone-parent c-ts-mode-indent-offset)
      ((parent-is "case_statement")
       standalone-parent c-ts-mode-indent-offset)
      ((node-is "field_initializer_list")
       parent-bol 8)

      ))
  (setq c-ts-mode-indent-style #'et-ms-cpp-style)
  )


(use-package combobulate
  :straight t)


(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
    (add-hook 'prog-mode-hook 'copilot-mode)
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

;; TODO: move this function
(defun et-download-and-extract-tar-gz (url target-directory)
  "Download a tar.gz file from URL and extract it into TARGET-DIRECTORY."
  (require 'url)
  (require 'tar-mode)
  (let ((download-file (concat temporary-file-directory "downloaded.tar.gz")))
    (url-copy-file url download-file t)
    (when (file-exists-p target-directory)
      (delete-directory target-directory t))
    (make-directory target-directory t)
    (let ((default-directory target-directory))
      (shell-command (format "tar -xf %s" download-file)))))

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((PowerShell . ("https://github.com/parzival3/tree-sitter-PowerShell.git"))))

  :config
  (defvar language-pack-version "0.12.86")
  (defvar languages-pack-url
    (format "https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/%s/tree-sitter-grammars-windows-%s.tar.gz" language-pack-version language-pack-version)
    "Url for downloading the treesiter language package (mainly for windows)")

  (defun download-new-languages ()
    (interactive)
    (et-download-and-extract-tar-gz languages-pack-url (concat user-emacs-directory "tree-sitter")))
  (defun et-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package yaml-mode
  :straight t)

(use-package edebug
  :bind
  (:map edebug-mode-map
        ("<f10>" . #'edebug-step-mode)
        ("<f11>" . #'edebug-step-in)
        ))



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
