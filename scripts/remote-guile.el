(setq-local windows-guile-buffer-name "Windows Guile RPL")
(setq-local geiser-repl-buffer-name-function (lambda (_) windows-guile-buffer-name))

(setq-local windows-guile-buffer (or (get-buffer windows-guile-buffer-name)
                                  (geiser-connect 'guile "localhost" 9919)))


(defvar change-directory-to-dci
  (prin1-to-string '(chdir "/c/Git/dci")))

(defun run-system-command (command)
  (prin1-to-string `(system ,command)))

(defun set-current-branch ()
  `(system ,(format "git fetch --all; git reset --hard origin/%s" (magit-get-current-branch))))

(defun set-master-branch ()
  (run-system-command "git reset --hard origin/master"))

;; (with-current-buffer windows-guile-buffer
;;   (geiser-repl--send change-directory-to-dci)
;;   (geiser-repl--send (set-current-branch))
;;   (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release kowalski/src"))
;;   (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release skipperlite/src")))

(defun run (command)
    (with-current-buffer windows-guile-buffer
      (geiser-repl--send (prin1-to-string command))))

(with-current-buffer windows-guile-buffer
  (geiser-repl--send change-directory-to-dci)
  (geiser-repl--send (set-master-branch))
  (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release skipperlite/src")))

;; TODO: fixme :-)
(setq counter 0)

(let ((default-directory "~/Git/dci"))
  (when (magit-unstaged-files)
    (setq-local counter (+ 1 counter))
    (magit-stage-modified)
    (magit-commit-fast (format "guile-repl %d" counter))
    (run (set-current-branch))))
