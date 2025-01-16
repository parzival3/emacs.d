(setq-local windows-guile-buffer-name "Windows Guile RPL")
(setq-local geiser-repl-buffer-name-function (lambda (_) windows-guile-buffer-name))

(setq-local windows-guile-buffer (or (get-buffer windows-guile-buffer-name)
                                  (geiser-connect 'guile "localhost" 9919)))


(defvar change-directory-to-dci
  (prin1-to-string '(chdir "/c/Git/dci")))

(defun run-system-command (command)
  (prin1-to-string `(system ,command)))

(defun set-current-branch ()
  (run-system-command
    (format "git fetch --all; git reset --hard origin/%s" (magit-get-current-branch))))

(defun set-master-branch ()
  (run-system-command "git reset --hard origin/master"))

;; (defun activate-venv-python ()
;;   (


;; (with-current-buffer windows-guile-buffer
;;   (geiser-repl--send change-directory-to-dci)
;;   (geiser-repl--send (set-current-branch))
;;   (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release kowalski/src"))
;;   (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release skipperlite/src")))

(defun repl (command)
  (with-current-buffer windows-guile-buffer
    (geiser-repl--send (prin1-to-string command))))

(repl '(setenv "EposPythonRoot" "/c/Tools/Python3.9"))
(repl '(system "msbuild.exe -p:Configuration=Release py_device_service/src/"))
(repl '(system "/c/Tools/Python3.9/python -m venv _python_venv"))
(repl '(system "./_python_venv/Scripts/activate"))
(repl '(let* ((win-path (getenv "PATH"))
               (env-path (string-append "./_python_venv/Scripts" ";" "./_python_venv/bin" ";" win-path)))
         (setenv "PATH" env-path)
         (getenv "PATH")))
(repl '(system "where python"))

(with-current-buffer windows-guile-buffer
  (geiser-repl--send (prin1-to-string (setenv "EposPythonRoot" "/c/Tools/Python3.9")))
  (geiser-repl--send change-directory-to-dci)
  (geiser-repl--send (set-current-branch))
 ;(geiser-repl--send (set-master-branch))
  (geiser-repl--send (run-system-command "msbuild.exe -p:Configuration=Release py_device_service/src/")))
