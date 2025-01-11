;;; linux.el --- Linux config -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Key repeat
;;; on linux in order to increase the key repeat one can use
;;; xset r rate 200 130

(use-package guix-emacs
  :defer t
  :ensure nil)

;; Setup wl-copytools for wls clipboard integration
;; https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                          :buffer nil
                          :command '("wl-copy" "-f" "-n")
                          :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
    nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(provide 'linux)
