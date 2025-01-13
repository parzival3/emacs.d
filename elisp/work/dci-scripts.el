;;; dci-scripts.el --- Useful work scripts  -*- lexical-binding: t; -*-

(defun emscripten-flutter-guix-env ()
  "Function for quickly start the development of Motomoto on web"
  (interactive)
  (require 'eshell)
  (let ((default-directory "~/Git/dci")
         (emscripten-eshell "emscripten-eshell")
         (flutter-eshell "flutter-eshell"))

    (unless (get-buffer emscripten-eshell)
      (let ((eshell-buffer-name emscripten-eshell))
        (eshell)
        (with-current-buffer emscripten-eshell
          (insert "~/.emacs.d/scripts/emsdk_and_flutter.sh")
          (eshell-send-input))))

    (unless (get-buffer flutter-eshell)
      (let ((eshell-buffer-name flutter-eshell))
        (eshell)
        (with-current-buffer flutter-eshell
          (insert "~/.emacs.d/scripts/emsdk_and_flutter.sh")
          (eshell-send-input)
          (insert "cd motomoto/build/web")
          (eshell-send-input)
          )))))
