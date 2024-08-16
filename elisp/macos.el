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
