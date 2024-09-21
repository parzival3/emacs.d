(defvar et-git-directory "~/Git/")
(defvar et-font-size "12")
(defvar et-font (concat "FiraCode Nerd Font Mono-" et-font-size))

(if (not (display-graphic-p))
    (setq et-theme 'modus-vivendi)
  (setq et-theme 'doom-laserwave))

