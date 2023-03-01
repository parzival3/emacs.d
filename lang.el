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
  (setq flutter-buffer-name "*Flutter-Runner*"))

(use-package rust-mode
  :straight t
  :config
  (let ((cargo-dir (concat (file-name-as-directory (getenv "HOME"))
                           ".cargo/bin")))
    (when (and (file-exists-p cargo-dir)
               (not (cl-find-if (lambda (path) (not (null (string-match "cargo" path)))) exec-path)))
      (setq exec-path (cons cargo-dir exec-path))
      (setenv "PATH" (concat (getenv "PATH") ":" cargo-dir)))))

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
