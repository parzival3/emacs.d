;;; macos.el --- Optimizations for macOs -*- no-byte-compile: t; lexical-binding: t; -*-

;; Mac specific
(setq ns-use-native-fullscreen t
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil
        mac-use-title-bar nil)

;; Make sure clipboard works properly in tty mode on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (not (display-graphic-p))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

;; Show time in the modeline since on macos when full scree one cannot see the time
(display-time-mode)

;; On macos ls doesn't support --dired
(setq-default dired-use-ls-dired nil)

(defun macos-get-resolution ()
  "Get the resolution of the main display in order to adapt the font based on laptop / laptop docked"
  (let ((command "system_profiler SPDisplaysDataType"))
    (with-temp-buffer
      (save-match-data
        (shell-command command (current-buffer))
        (goto-char (point-min))
        (re-search-forward "Main Display:[[:space:]]+Yes")
        (re-search-backward "Resolution:[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+x[[:space:]]+\\([[:digit:]]+\\)")
        (cons (string-to-number (match-string 1)) (string-to-number (match-string 2)))))))

;; If I'm using just the laptop set the font to 14 instead of 18
(when (> (car (macos-get-resolution)) 1080)
  (add-to-list 'default-frame-alist '(font . "Roboto Mono:style=Light:size=14")))

(use-package modus-themes
  :ensure t)

(custom-set-variables
  '(custom-safe-themes
       ("8d146df8bd640320d5ca94d2913392bc6f763d5bc2bb47bed8e14975017eea91" "e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032" default)))
(provide 'macos)
