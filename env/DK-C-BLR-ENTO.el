(defvar git-directory "~/Git/")
(defvar secrets-file (concat user-emacs-directory "env/secrets.el"))
(defvar p-font-size "12")
(cond
      ((eq system-type 'gnu/linux)
       (defvar p-font (concat "Fira Code-" p-font-size)))
      ((eq system-type 'windows-nt)
       (defvar p-font (concat "FiraCode NFM-" p-font-size)))
      (t (error "Wrong system type")))

(defvar p-theme 'doom-badger)

(if (not (display-graphic-p))
    (setq p-theme 'modus-vivendi)
  (setq p-theme 'doom-laserwave))
