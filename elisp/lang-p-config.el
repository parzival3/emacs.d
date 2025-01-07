;; lang.el --- Programming modes -*- no-byte-compile: t; lexical-binding: t; -*-
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
  :bind
  (:map devdocs-mode-map
    ("d" . #'devdocs-peruse)
    ("i" . #'devdocs-lookup)
    ("p" . #'devdocs-previous-entry)
    ("n" . #'devdocs-next-entry)
    ("g" . #'devdocs-goto-page)
    ("[" . #'devdocs-previous-page)
    ("]" . #'devdocs-next-page)
    ("<" . #'devdocs-first-page)
    (">" . #'devdocs-last-page)
    ("L" . #'devdocs-go-back)
    ("r" . #'devdocs-go-forward)
    ("w" . #'devdocs-copy-url)
    ("." . #'devdocs-goto-target))
  :config
  (eval `(bind-keys :map devdocs-mode-map ,@meow-normal-movement)))


(use-package dash-docs
  :defer t)

(defun et-indent-style()
  "Override the built-in BSD indentation style with some additional rules"
  `(
    ;; align function arguments to the start of the first one, offset if standalone
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") (nth-sibling 1) 0)
    ;; same for parameters
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") (nth-sibling 1) 0)
    ((parent-is "try_statement") (nth-sibling 1) 0)

     ;; indent inside case blocks
    ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
    ;; do not indent preprocessor statements
    ((node-is "preproc") column-0 0)
    ;; namespace
    ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
    ((n-p-gp "compound_statement" "catch_clause" nil) standalone-parent 0)
    ((n-p-gp "compound_statement" "try_statement" nil) standalone-parent 0)
     ;; append to bsd style
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))


(unless (file-directory-p (concat package-user-dir "/copilot"))
  (package-vc-install '(copilot
                        :url "https://github.com/copilot-emacs/copilot.el.git")))

(use-package copilot
  :defer t
  :bind
  ("TAB" . et-copilot-tab)
  ("S-TAB" . copilot-accept-completion)
  ("<backtab>" . copilot-accept-completion) ;; S-TAB is recognized as backtab
  ("<f10>" . toggle-copilot-mode)
  ;; :hook
  ;; (prog-mode . copilot-mode)
  :config
  (defun et-copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (if (bound-and-true-p copilot-mode)
      (copilot-accept-completion-by-word)
      (indent-for-tab-command))))


(unless (file-directory-p (concat package-user-dir "/dart-ts-mode"))
  (package-vc-install '(dart-ts-mode
                         :url "https://github.com/50ways2sayhard/dart-ts-mode.git")))

(use-package dart-ts-mode
  :defer t
  :hook
  (eglot-mode . dart-ts-eglot-server)
  :config
  (defun dart-ts-eglot-server ()
    (unless (alist-get 'dart-ts-mode eglot-server-programs)
      (add-to-list 'eglot-server-programs
               '(dart-ts-mode . ("dart" "language-server" "--client-id" "emacs.eglot-dart")))))
  ;; make sure eglot doesn't talk to fast to the dart server
  (setq eglot-sync-connect 2)
  (setq eglot-events-buffer-config '(:size 0 :format nil))
  (setq eldoc-echo-area-prefer-doc-buffer t))


(use-package groovy-mode
  :defer t
  :ensure nil)


(use-package python
  :defer t
  :bind
  ;; remove the default binding for backtab
  (:map python-ts-mode-map
        ("<backtab>" . nil))
  :hook
  (python-ts-mode . eglot-ensure)
  :init
  ;; (unless (boundp 'eglot-workspace-configuration)
  ;;   (setq-default eglot-workspace-configuration nil))
  ;; (add-to-list 'eglot-workspace-configuration
  ;;   `(:pylsp (:plugins
  ;;              (;; Fix imports and syntax using `eglot-format-buffer`
  ;;                :isort (:enabled t)
  ;;                :autopep8 (:enabled t)
  ;;
  ;;                ;; Syntax checkers (works with Flymake)
  ;;                :pylint (:enabled t)
  ;;                :pycodestyle (:enabled t)
  ;;                :flake8 (:enabled t)
  ;;                :pyflakes (:enabled t)
  ;;                :pydocstyle (:enabled t)
  ;;                :mccabe (:enabled t)
  ;;
  ;;                :yapf (:enabled :json-false)
  ;;                :rope_autoimport (:enabled :json-false)))))

  :commands (et-python-venv et-python-venv-deactivate)
  :config
  (setq-default et--p-venv-exec-path nil)
  (setq-default et--p-venv-dir nil)

  (defun et-python-venv (directory)
    "Activate the python virtual environment in DIRECTORY."
    (interactive "D")
    (when (and (not (string= directory et--p-venv-dir))
               et--p-venv-exec-path)
      (error "Previous environment still active"))

    (setq-default et--p-venv-exec-path exec-path)
    (setq-default et--p-venv-dir directory)

    (et-add-directory-to-env directory (format "%s not a directory" directory))
    (et-add-directory-to-env (concat directory "/Scripts"))
    (et-add-directory-to-env (concat directory "/bin")))

  (defun et-python-venv-deactivate ()
    (interactive)
    (setq exec-path et--p-venv-exec-path)
    (eshell-set-path exec-path)
    (setq-default et--p-venv-exec-path nil)
    (setq-default et--p-venv-dir nil)))

(use-package yaml-mode
  :defer t)

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
