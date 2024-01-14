(defvar git-directory "~/Git/")
(defvar secrets-file (concat user-emacs-directory "env/secrets.el"))

(defvar et-font-size (let ((geometry (alist-get 'geometry (car (display-monitor-attributes-list)))))
         (if (eq 1440 (caddr geometry))
             "12"
           "14")))

(defvar et-font (concat "FiraCode Nerd Font Mono-" et-font-size))

(add-to-list 'exec-path "/opt/homebrew/bin/")
(add-to-list 'exec-path "/opt/homebrew/sbin/")
(setq eshell-path-env (mapconcat #'identity exec-path ":"))
(setenv "PATH" eshell-path-env)

(defvar et-theme 'doom-badger)

(if (not (display-graphic-p))
    (setq et-theme 'modus-vivendi)
  (setq et-theme 'doom-laserwave))
