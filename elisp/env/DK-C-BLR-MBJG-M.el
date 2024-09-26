(defvar et-git-directory "~/Git/")


;; Flutter
(add-to-list 'exec-path (concat (getenv "HOME") "/development/flutter/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin/")
(add-to-list 'exec-path "/opt/homebrew/sbin/")
(setq eshell-path-env (mapconcat #'identity exec-path ":"))

(setenv "PATH" eshell-path-env)

(setq exec-path (append exec-path '("/Users/ento/tools")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/ento/tools"))

;; Rust with brew
(setenv "RUSTUP_HOME" (concat (getenv "HOME") "/development/rust/rustp"))
(setenv "CARGO_HOME"  (concat (getenv "HOME") "/development/rust/cargo"))

;; Guile with brew
(setenv "GUILE_LOAD_PATH" "/opt/homebrew/share/guile/site/3.0")
(setenv "GUILE_LOAD_COMPILED_PATH" "/opt/homebrew/lib/guile/3.0/site-ccache")
(setenv "GUILE_SYSTEM_EXTENSIONS_PATH" "/opt/homebrew/lib/guile/3.0/extensions")


;; Downloader
(setenv "FIRMWARES_DIRECTORY" (concat (getenv "HOME") "/firmwares"))
