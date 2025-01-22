(setq-default windows-guile-buffer-name "Windows Guile RPL")

(setq-default windows-guile-buffer nil)

(setq-default dci-socket-process nil)
(setq-default dci-socket-buffer-name "dci-socket-buffer")
(setq-default dci-socket-port 9912)
(setq-default dci-socket-buffer nil)
(setq-default guile-dci-socket 'dci-socket)
(defvar guile-dci-repo 'guile-dci-repo)

;; use system to run bat commands

(defun run (command)
  (with-current-buffer windows-guile-buffer
    (when (eq (car command) 'bat)
      (setq command `(system ,(concat (cadr command) " | nc -c localhost " (int-to-string dci-socket-port)))))
    (geiser-repl--send (prin1-to-string command))))


(defun guile-git-repo-init ()
  (setq-default windows-guile-buffer
    (let ((geiser-repl-buffer-name-function (lambda (_) windows-guile-buffer-name)))
                                (or (get-buffer windows-guile-buffer-name)
                                   (geiser-connect 'guile "localhost" 9919))))

  (setq-default dci-socket-process (make-network-process :server t :host 'local :service dci-socket-port :name "dci-socket"
                                     :family 'ipv4 :buffer (get-buffer-create dci-socket-buffer-name)))

  (run `(define ,guile-dci-socket (socket PF_INET SOCK_STREAM 0)))
  (run `(connect dci-socket AF_INET INADDR_LOOPBACK ,dci-socket-port))
  (run `(use-modules (git)))
  (run `(begin
          (libgit2-init!)
          (chdir "/c/Git/dci")
          (define ,guile-dci-repo
            (repository-open ".")))))

(guile-git-repo-init)

(defun check-commit-value ()
  (let* ((default-directory "~/Git/dci")
         (wsl-commit (magit-rev-hash "HEAD"))
         (wsl-patch (shell-command-to-string "git diff HEAD"))
         (libgit-command `(let
                            ((windows-oid (reference-target (repository-head ,guile-dci-repo))))

                            (unless (oid=? windows-oid (string->oid ,wsl-commit))
                              (system "git fetch --all")
                              (reset ,guile-dci-repo (object-lookup ,guile-dci-repo (string->oid ,wsl-commit)) RESET_HARD))

                            (let*
                              ((new-index (apply-diff-to-tree ,guile-dci-repo (commit-tree (commit-lookup ,guile-dci-repo windows-oid)) (string->diff ,wsl-patch)))
                               (next-diff (diff-index-to-index ,guile-dci-repo (repository-index ,guile-dci-repo) new-index)))
                              (apply-diff ,guile-dci-repo next-diff APPLY-LOCATION-BOTH)
                              (display "Patch is :\n")
                              (display "\n---------------------------\n")
                              (display (diff->string next-diff))
                              (display "\n---------------------------\n")
                              )
                            )))
    (run libgit-command)))

(check-commit-value)

(defun windows:create-python-venv ()
  (interactive)
  (let ((create-venv-command `(let ((dci-path-file
"/Git/dci/lib/windows-x64-release-static/
/Git/dci/cloud_client/scripts
/Git/dci/py_device_service/src
/Git/dci/py_device_service/lib
/Git/dci/py_device_service/demo
/Git/dci/py_device_service/quickdfu"))
                                (unless (file-exists? "_python_venv")
                                  (system "/c/Tools/Python3.9/python3 -m venv _python_venv"))
                                (with-output-to-file "/c/Git/dci/_python_venv/Lib/site-packages/dci.pth"
                                  (lambda ()
                                    (display dci-path-file)))
                                (setenv "PATH" (string-append "/c/Git/dci/_python_venv/Scripts:" (getenv "PATH"))))))
    (run create-venv-command)))

(defun windows:build-quick-dfu ()
  (interactive)
  (check-commit-value)
  (run '(setenv "EposPythonRoot" "/c/Tools/Python3.9"))
  (run `(system "msbuild.exe -p:Configuration=Release py_device_service/src"))
  (run `(system "py_device_service/quickdfu/build_quickdfu.bat"))
  (run `(system "wt -w 0 nt --title \"QuickDFU\" --tabColor \"#6B8E35\" -p ps -Command \"/c/Git/dci/bin/windows-x64-release-static/quickdfu.exe\"")))


(defun windows:run-quick-dfu ()
  (interactive)
  (check-commit-value)
  (windows:create-python-venv)
  (run '(setenv "EposPythonRoot" "/c/Tools/Python3.9"))
  (run `(system "msbuild.exe -p:Configuration=Release py_device_service/src"))
  (run `(system "wt -w 0 nt --title \"QuickDFU\" --tabColor \"#AE8E35\" -d /c/Git/dci/ -p ps /c/Git/dci/_python_venv/Scripts/python.exe /c/Git/dci/py_device_service/quickdfu/quickdfu.py --epos-manager-config=staging_kowalski")))



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
