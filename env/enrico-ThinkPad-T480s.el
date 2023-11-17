(defvar git-directory "~/Git/")
(defvar secrets-file (concat user-emacs-directory "env/secrets.el"))
(defvar et-font-size "12")
(defvar et-font (concat "FiraCode Nerd Font Mono-" et-font-size))

(defvar et-theme 'doom-badger)

(if (not (display-graphic-p))
    (setq et-theme 'modus-vivendi)
  (setq et-theme 'doom-laserwave))
