;;; remote-guile.el --- Collections of elisp/guile function to for WSL development -*- lexical-binding: t; -*-
(require 'geiser)
(require 'geiser-repl)

(defvar elgu:repl-buffer-name "*Geiser Windows Guile RPL*")
(defvar elgu:repl-buffer nil)
(defvar elgu:elisp-socket-process nil)
(defvar elgu:socket-buffer-name "dci-socket-buffer")
(defvar elgu:elisp-port 9912)
(defvar elgu:guile-cmd-input-port 9900)
(defvar elgu:guile-windows-cmd-socket 'windows-cmd-socket)
(defvar elgu:guile-elisp-socket 'dci-socket)
(defvar elgu:guile-dci-gitrepo 'elgu:guile-dci-gitrepo)
(defvar elgu:windows-dci-directory "/c/Git/dci")

;; use system to run bat commands

(defun elgu:run (command)
  (with-current-buffer elgu:repl-buffer
    (when (eq (car command) 'bat)
      (setq command `(begin
                       (system ,(concat (cadr command) " | nc -c localhost " (int-to-string elgu:guile-cmd-input-port)))
                       (let ((input-port (accept ,elgu:guile-windows-cmd-socket)))
                         (format ,elgu:guile-elisp-socket (get-string-all (car input-port)))))))
    (geiser-repl--send (string-replace "\\#" "#" (prin1-to-string command)))))

(defun elgu:run-raw (command)
  (with-current-buffer elgu:repl-buffer
    (geiser-repl--send command)))


(defun elgu:init-repl-buffer ()
  (unless elgu:repl-buffer
    (let ((geiser-repl-buffer-name-function (lambda (_) elgu:repl-buffer-name))
           (repl-buffer (get-buffer elgu:repl-buffer-name)))
      (if repl-buffer
        (setq elgu:repl-buffer repl-buffer)
        (save-excursion
               (geiser-connect 'guile "localhost" 9919))
        (setq elgu:repl-buffer (get-buffer elgu:repl-buffer-name))))))

(defun elgu:init ()
  (interactive)
  (elgu:init-repl-buffer)
  (setq elgu:guile-elisp-socket-process
    (make-network-process :server t :host 'local
                          :service elgu:elisp-port :name "window-guile->wsl-elisp"
                          :family 'ipv4 :buffer (get-buffer-create elgu:socket-buffer-name)))

  (elgu:run '(use-modules (rnrs io ports)))
  (elgu:run `(define ,elgu:guile-windows-cmd-socket (socket PF_INET SOCK_STREAM 0)))
  (elgu:run `(bind ,elgu:guile-windows-cmd-socket AF_INET INADDR_ANY ,elgu:guile-cmd-input-port))
  (elgu:run `(listen ,elgu:guile-windows-cmd-socket 1))

  (elgu:run `(define ,elgu:guile-elisp-socket (socket PF_INET SOCK_STREAM 0)))
  (elgu:run `(connect dci-socket AF_INET INADDR_LOOPBACK ,elgu:elisp-port))
  (elgu:run `(use-modules (git)))
  (elgu:run '(libgit2-init!))
  (elgu:run `(chdir ,elgu:windows-dci-directory))
  (elgu:run `(define ,elgu:guile-dci-gitrepo (repository-open ".")))
  (elgu:run '(define elgu:guile-dci-remote (remote-lookup elgu:guile-dci-gitrepo "origin")))
  (elgu:run '(remote-connect elgu:guile-dci-remote))
  (elgu:run '(define remote-fetch-options (make-fetch-options)))
  (elgu:run '(set-fetch-options-download-tags! remote-fetch-options 'all))
  (elgu:run '(remote-fetch elgu:guile-dci-remote \#:fetch-options remote-fetch-options)))

(defun elgu:sync-repo ()
  (let* ((default-directory "~/Git/dci")
          (wsl-commit (magit-rev-hash "HEAD"))
          (wsl-patch (shell-command-to-string "git diff HEAD"))
          (libgit-command `(let
                            ((windows-oid (reference-target (repository-head ,elgu:guile-dci-gitrepo))))
                            (unless (oid=? windows-oid (string->oid ,wsl-commit))
                                (remote-fetch elgu:guile-dci-remote \#:fetch-options remote-fetch-options)
                                (reset ,elgu:guile-dci-gitrepo (object-lookup ,elgu:guile-dci-gitrepo (string->oid ,wsl-commit)) RESET_HARD))
                             (when (not (string= ,wsl-patch ""))
                               (display "Applying patch")
                               (let* ((new-index (apply-diff-to-tree ,elgu:guile-dci-gitrepo (commit-tree (commit-lookup ,elgu:guile-dci-gitrepo windows-oid)) (string->diff ,wsl-patch)))
                                       (next-diff (diff-index-to-index ,elgu:guile-dci-gitrepo (repository-index ,elgu:guile-dci-gitrepo) new-index)))
                                 (apply-diff ,elgu:guile-dci-gitrepo next-diff APPLY-LOCATION-BOTH)))
                               )))
    (elgu:run libgit-command)))

(defun elgu:create-python-venv ()
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
    (elgu:run create-venv-command)))

(defun elgu:build-quick-dfu ()
  (interactive)
  (elgu:sync-repo)
  (elgu:run '(setenv "EposPythonRoot" "/c/Tools/Python3.9"))
  (elgu:run `(bat "msbuild.exe -p:Configuration=Release py_device_service/src"))
  (elgu:run `(bat "py_device_service/quickdfu/build_quickdfu.bat"))
  (elgu:run `(system "wt -w 0 nt --title \"QuickDFU\" --tabColor \"#6B8E35\" -p ps -Command \"/c/Git/dci/bin/windows-x64-release-static/quickdfu.exe\"")))

(defun elgu:run-quick-dfu ()
  (interactive)
  (elgu:sync-repo)
  (elgu:create-python-venv)
  (elgu:run '(setenv "EposPythonRoot" "/c/Tools/Python3.9"))
  (elgu:run `(bat "msbuild.exe -p:Configuration=Release py_device_service/src"))
  (elgu:run `(system "wt -w 0 nt --title \"QuickDFU\" --tabColor \"#AE8E35\" -d /c/Git/dci/ -p ps /c/Git/dci/_python_venv/Scripts/python.exe /c/Git/dci/py_device_service/quickdfu/quickdfu.py --epos-manager-config=staging_kowalski")))
