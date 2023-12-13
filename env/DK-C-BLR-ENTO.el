(defvar git-directory "~/Git/")
(defvar secrets-file (concat user-emacs-directory "env/secrets.el"))
(defvar et-font-size "12")
(cond
      ((eq system-type 'gnu/linux)
       (defvar et-font (concat "Fira Code-" et-font-size)))
      ((eq system-type 'windows-nt)
       (defvar et-font (concat "FiraCode NFM-" et-font-size)))
      (t (error "Wrong system type")))

(defvar et-theme 'doom-badger)

(if (not (display-graphic-p))
    (setq et-theme 'spacemacs-dark)
  (setq et-theme 'doom-laserwave))
