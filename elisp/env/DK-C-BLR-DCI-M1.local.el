(defvar et-git-directory "~/Git/")

(defvar et-font-size (let ((geometry (alist-get 'geometry (car (display-monitor-attributes-list)))))
         (if (eq 1440 (caddr geometry))
             "12"
           "14")))

(defvar et-font (concat "FiraCode Nerd Font Mono-" et-font-size))

(add-to-list 'exec-path "/opt/homebrew/bin/")
(add-to-list 'exec-path "/opt/homebrew/sbin/")
(add-to-list 'exec-path "/Users/ento/development/flutter/bin")


(eshell-set-path (mapconcat #'identity exec-path path-separator))
(setenv "PATH" (mapconcat #'identity exec-path path-separator))

(defvar et-theme 'doom-badger)

(if (not (display-graphic-p))
    (setq et-theme 'modus-vivendi)
  (setq et-theme 'doom-laserwave))
