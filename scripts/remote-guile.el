(setq-local windows-guile-buffer-name "Windows Guile RPL")
(setq-local geiser-repl-buffer-name-function (lambda (_) windows-guile-buffer-name))

(setq-local windows-guile-buffer (or (get-buffer windows-guile-buffer-name)
                                  (geiser-connect 'guile "localhost" 9919)))


;; use system to run bat commands

(defun run (command)
    (with-current-buffer windows-guile-buffer
      (geiser-repl--send (prin1-to-string command))))

(defvar guile-dci-repo 'guile-dci-repo)

(defun guile-git-repo-init ()
  (run `(use-modules (git)))
  (run `(begin
          (libgit2-init!)
          (chdir "/c/Git/dci")
          (define ,guile-dci-repo
            (repository-open ".")))))

(guile-git-repo-init)

(defun create-virtual-env ()
  (let ((python-path "/c/Tools/Python3.9")
        (dci-python-env "EposPythonRoot"))
  (run  `(system (string-concat

(defun check-commit-value ()
  (let* ((default-directory "~/Git/dci")
         (wsl-commit (magit-rev-hash "HEAD"))
         (wsl-patch (shell-command-to-string "git diff HEAD"))
         (libgit-command `(let
                            ((windows-oid (reference-target (repository-head ,guile-dci-repo))))
                            (unless (oid=? windows-oid (string->oid ,wsl-commit))
                              (system "git fetch --all"))
                            (reset ,guile-dci-repo (object-lookup ,guile-dci-repo (string->oid ,wsl-commit)) RESET_HARD)
                            (apply-diff ,guile-dci-repo (string->diff ,wsl-patch) APPLY-LOCATION-INDEX)
                            )))
    (run libgit-command)))

(check-commit-value)

(run '(setenv "EposPythonRoot" "/c/Tools/Python3.9"))
(run `(system "msbuild.exe -p:Configuration=Release py_device_service/src"))
(run `(system "py_device_service/quickdfu/build_quickdfu.bat"))
(run `(system "start cmd.exe @cmd -k \"./bin/windows-x64-release-static/quickdfu.exe\""))
(run `(system "wt -w 0 nt --title \"QuickDFU\" --tabColor \"#6B8E35\" -p ps -Command \"/c/Git/dci/bin/windows-x64-release-static/quickdfu.exe\""))

;; (with-current-buffer windows-guile-buffer
;;   (geiser-repl--send change-directory-to-dci)
;;   (geiser-repl--send (set-master-branch))
;;   (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release skipperlite/src")))

;; TODO: fixme :-)
;; (setq counter 0)
;;
;; (let ((default-directory "~/Git/dci"))
;;   (when (magit-unstaged-files)
;;     (setq-local counter (+ 1 counter))
;;     (magit-stage-modified)
;;     (magit-commit-fast (format "guile-repl %d" counter))
;;     (run (set-current-branch))))
