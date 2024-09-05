(defvar et-git-directory "~/Git/")

(defvar et-font-size (let ((geometry (alist-get 'geometry (car (display-monitor-attributes-list)))))
         (if (eq 1440 (caddr geometry))
             "12"
           "14")))

(defvar et-font (concat "FiraCode Nerd Font Mono-" et-font-size))

(add-to-list 'exec-path "/opt/homebrew/bin/")
(add-to-list 'exec-path "/opt/homebrew/sbin/")
(add-to-list 'exec-path "/Users/ento/Library/Python/3.9/bin")
(setq eshell-path-env (mapconcat #'identity exec-path ":"))
(setenv "PATH" eshell-path-env)

(use-package emacs
  :config
  (setenv "ANDROID_HOME" "/Users/enrico/.android/sdk")
  (setenv "NO_PROXY" "127.0.0.1,localhost,::1")
  (setenv "PATH" (concat (getenv "PATH") ":"
                         (getenv "ANDROID_HOME") "/" "cmdline-tools" ":"
                         (getenv "ANDROID_HOME") "/" "cmdline-tools/bin"  ":"
                         (getenv "ANDROID_HOME") "/" "platform-tools" ":"
                         (getenv "ANDROID_HOME") "/" "emulator"))
  (setenv "ANDROID_EMULATOR" "Andoird_35")
  (setenv "NO_PROXY" "127.0.0.1,localhost,::1")

  (defun et-run-emulator ()
    (interactive)
    (async-shell-command "emulator  -avd $ANDROID_EMULATOR -no-snapshot -no-boot-anim -wipe-data"))
  ;; For flutter you might also need "flutter config --android-sdk $ANDROID_HOME"
  )


;; shell commands for fixing the keyboard
;; defaults write -g ApplePressAndHoldEnabled -bool false
;; InitialKeyRepeat is the delay before the key repeat, 15 is the default
;; defaults write -g InitialKeyRepeat -int 10
;; KeyRepeat is the speed of the key repeat, 2 is the default
;; defaults write -g KeyRepeat -int 1
