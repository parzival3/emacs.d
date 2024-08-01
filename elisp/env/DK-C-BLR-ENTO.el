(defvar et-font-size "12")
(cond
      ((eq system-type 'gnu/linux)
       (defvar et-font (concat "Fira Code-" et-font-size))
       (defvar et-git-directory "~/Git/"))
      ((eq system-type 'windows-nt)
       (defvar et-font (concat "FiraCode NFM-" et-font-size))
       (defvar et-git-directory "C:/Git/"))
      (t (error "Wrong system type")))

