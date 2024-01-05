;; -*- lexical-binding: t; -*-
;; Unix specific configuration

(use-package vterm
  :straight t)

;; TODO: This should probably live in the wls config and not here
(use-package project
  :bind (:map project-prefix-map
              ("c" . et-project-compile))

  :config
  (defun et-project-compile (arg)
  "Run `compile' in the project root."
  (interactive "P")
  (if arg
      (progn
        (message "Running on Windows directory")
        (let* ((project-dir (string-replace "~/Git/"  "/mnt/c/Git/" (project-root (project-current t))))
               (default-directory project-dir)
               (compilation-buffer-name-function
                       (or project-compilation-buffer-name-function
                           compilation-buffer-name-function)))
          (call-interactively #'compile)))
      (call-interactively #'project-compile))))

(provide 'unix)
