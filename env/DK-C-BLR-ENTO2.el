(defvar git-directory "~/Git/")
(defvar secrets-file (concat user-emacs-directory "env/secrets.el"))
(defvar p-font-size "12")
(defvar p-font (concat "FiraCode-" p-font-size))

(defvar p-theme 'doom-badger)

(if (not (display-graphic-p))
    (setq p-theme 'modus-vivendi)
  (setq p-theme 'doom-laserwave))
