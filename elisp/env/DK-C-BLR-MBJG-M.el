(defvar et-git-directory "~/Git/")

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

(setq exec-path (append exec-path '("/Users/ento/tools")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/ento/tools"))

;; Rust with brew
(setenv "RUSTUP_HOME" (concat (getenv "HOME") "/development/rust/rustp"))
(setenv "CARGO_HOME"  (concat (getenv "HOME") "/development/rust/cargo"))

;; Guile with brew
(setenv "GUILE_LOAD_PATH" "/opt/homebrew/share/guile/site/3.0")
(setenv "GUILE_LOAD_COMPILED_PATH" "/opt/homebrew/lib/guile/3.0/site-ccache")
(setenv "GUILE_SYSTEM_EXTENSIONS_PATH" "/opt/homebrew/lib/guile/3.0/extensions")
