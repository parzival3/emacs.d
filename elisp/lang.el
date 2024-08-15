;;; lang.el --- Programming modes -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package flyspell
  :defer t
  :config
  (setq ispell-program-name "aspell")
  :hook
  ((org-mode) . flyspell-mode))

(use-package clojure-mode
  :defer t)

(use-package cider
  :defer t)

(eval-and-compile (setq dart-ts-mode-path "~/.emacs.d/straight/repos/dart-ts-mode"))

;; TODO: fix this
(use-package dart-ts-mode
  :defer t
  :load-path dart-ts-mode-path
  :init
  (with-eval-after-load 'eglot
    ;; (progn (add-to-list 'eglot-server-programs
    ;;                     '(dart-ts-mode . ("dart" "language-server" "--client-id" "emacs.eglot-dart" :initializationOptions (:onlyAnalyzeProjectsWithOpenFiles t)))))
    (progn (add-to-list 'eglot-server-programs
                            '(dart-ts-mode . ("dart" "language-server" "--client-id" "emacs.eglot-dart"))))
    ;; make sure eglot doesn't talk to fast to the dart server
    (setq eglot-sync-connect 2)
    (setq eglot-events-buffer-config '(:size 0 :format nil))
    (setq eldoc-echo-area-prefer-doc-buffer t)))

(use-package flutter
  :defer t
  :config
  (add-hook #'dart-mode-hook #'eglot-ensure)
  (setq flutter-buffer-name "*Flutter-Runner*"))

(use-package rust-mode
  :defer t)

(use-package zig-mode
  :defer t
  :hook ((zig-mode . eglot-ensure))
  :config

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
  :defer t)


(use-package devdocs
  :defer t
  :straight t
  :config
  (setq devdocs-data-dir (concat et-emacs-files-dir "devdocs")))


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
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
    ((n-p-gp "compound_statement" "for_statement" nil) standalone-parent c-ts-mode-indent-offset)
    ;; append to bsd style
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))


(use-package copilot
  :defer t
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind
  ("TAB" . et-copilot-tab)
  ("S-TAB" . copilot-accept-completion)
  ("<backtab>" . copilot-accept-completion) ;; S-TAB is recognized as backtab
  ("<f10>" . toggle-copilot-mode)
  ;; :hook
  ;; (prog-mode . copilot-mode)
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
  (if (bound-and-true-p copilot-mode)
      (copilot-accept-completion-by-word)
      (indent-for-tab-command))))


(use-package python
  :defer t
  :bind
  ;; remove the default binding for backtab
  (:map python-ts-mode-map
        ("<backtab>" . nil))
  :hook
  (python-ts-mode . et-eglot-python)
  :config

  (defun et-eglot-pyhon ()
    (unless
        (string-equal
         (file-name-extension (buffer-file-name (current-buffer)))
         "pyi")
      (eglot-ensure)))

  (setq et--p-venv-exec-path nil)
  (setq et--p-venv-eshell-path nil)

  (defun et-python-venv (directory)
    "Activate the python virtual environment in DIRECTORY."
    (interactive "D")
    (when (or et--p-venv-exec-path
              et--p-venv-eshell-path)
     (error "Previous environment still active"))

    (setq et--p-venv-exec-path exec-path)
    (setq et--p-venv-eshell-path (eshell-get-path))

    (et-add-directory-to-env directory (format "%s not a directory" directory))
    (et-add-directory-to-env (concat directory "/Scripts"))
    (et-add-directory-to-env (concat directory "/bin")))

  (defun et-python-venv-deactivate ()
    (interactive)
    (setq exec-path et--p-venv-exec-path)
    (eshell-set-path et--p-venv-eshell-path)
    (setq et--p-venv-exec-path nil)
    (setq et--p-venv-eshell-path nil)))


(use-package yaml-mode
  :defer t
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

(provide 'lang)
