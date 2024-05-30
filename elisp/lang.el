(use-package flyspell
  :straight t
  :config
  (setq ispell-program-name "aspell")
  :hook
  ((org-mode) . flyspell-mode))

(use-package clojure-mode
  :defer t
  :straight t)

(use-package cider
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
  :straight t
  :defer t
  :hook ((zig-mode . eglot-ensure)
         (gptel-mode . zig-add-gptel-directive))
  :config

  (defun zig-add-gptel-directive ()
    (setq gptel-directives (add-to-list 'gptel-directives '(zig . "You are a large language model and a careful zig programmer. Provide code and explanations about my zig code and suggests enhanced error handling and zig idioms. Use the zig 0.11 standard."))))

  (defvar zig-projects nil "List of zig projects to search in")

  (defun zig-ripgrep-search ()
    (interactive)
    (consult-ripgrep zig-projects))

  (defun find-in-zig-src ()
    (interactive)
    (fd-dired (getenv "ZIG_SRC") (read-string "Find in zig src: ")))


  (defun et-open-zig-docs ()
    (interactive)
    (let ((zig-docs-buffer-name "*zig docs*"))
      (if (get-buffer zig-docs-buffer-name)
          (switch-to-buffer zig-docs-buffer-name)
          (letrec ((hookfun (lambda ()
                              (rename-buffer "*zig docs*")
                              `(remove-hook 'eww-after-render-hook ,hookfun))))
            (add-hook 'eww-after-render-hook hookfun)
            (eww "https://ziglang.org/documentation/master/" t))))))

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
  :straight t
  :config
  (setq devdocs-data-dir (concat et-emacs-files-dir "devdocs")))

(use-package treesit
  :config
  ;; remove .h from the auto-mode-alist
  (setq auto-mode-alist (delete '("\\.h\\'" . c-or-c++-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("\\.h\\'" . c-or-c++-mode) auto-mode-alist))
    (add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(py\\|pyi\\)\\'" . python-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(json\\|jsonnet\\)\\'" . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(ino\\)\\'" . c++-ts-mode)))

(use-package c-ts-mode
  :config

  (dir-locals-set-class-variables 'et-dci-project
                                  '((c++-ts-mode . ((fill-column . 50)))))

  (dir-locals-set-directory-class
   (concat et-git-directory "dci_windows") 'et-dci-project)

  (setq c-ts-mode-indent-style 'k&r)
  (setq c-ts-mode-indent-offset 4)
  (setq-local indent-tabs-mode nil)

  (setq treesit--indent-verbose t) ;; uncomment to debug indentation

  ;; limit xref search to only soruce files
  (setq xref-ripgrep-args '("--type-add" "source=*.{c,cpp,py,js}" "--type" "source"))




  (defun et-update-tags ()
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (tag-file "TAGS"))
      (shell-command (concat "rm -rf TAGS; fd \".(cpp|h|c|cxx|hxx)$\" -X ctags -e -a -f"  (expand-file-name tag-file))))))


(defun et-indent-style()
  "Override the built-in BSD indentation style with some additional rules"
  `(
    ;; align function arguments to the start of the first one, offset if standalone
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") (nth-sibling 1) 0)
    ;; same for parameters
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") (nth-sibling 1) 0)
    ;; indent inside case blocks
    ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
    ;; do not indent preprocessor statements
    ((node-is "preproc") column-0 0)
    ;; namespace
    ((and (parent-is "translation_unit") (n-p-gp nil nil "namespace_definition")) grand-parent 0)
    ;; append to bsd style
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))


(use-package c-ts-mode
  :config
  (setq c-ts-mode-indent-style #'et-indent-style))


(use-package combobulate
  :straight t)


(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind
  ("TAB" . et-copilot-tab)
  ("S-TAB" . copilot-accept-completion)
  ("<backtab>" . copilot-accept-completion) ;; S-TAB is recognized as backtab
  ("<f10>" . toggle-copilot-mode)
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

  (defun et-copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion-by-word)
      (indent-for-tab-command))))

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


(use-package python
  :bind
  ;; remove the default binding for backtab
  (:map python-ts-mode-map
        ("<backtab>" . nil))
  :hook
  (python-ts-mode . eglot-ensure))


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
