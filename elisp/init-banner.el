;;; init-banner.el --- ASCII Banner LETTERS -*- lexical-binding: t; -*-

(defvar +banner--width 80)
(defun +banner--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? ) s))

(defvar +banner--top-pos 3
  "2 - Perfect center; 3 - A bit higher from the buffer center.")

(defun random-color ()
  "Generate a random hex color code that is not too dark."
  (let ((r (+ (random 256) 64))
        (g (+ (random 256) 64))
        (b (+ (random 256) 64)))
    (format "#%02X%02X%02X" r g b)))

(defun draw-ascii-banner-fn ()
  (let* ((banner
          '("     ___           ___           ___           ___           ___ "
            "    /\\  \\         /\\  \\         /\\  \\         /\\  \\         /\\  \\ "
            "   /::\\  \\       /||\\  \\       /::\\  \\       /::\\  \\       /||\\  \\ "
            "  /:/\\:\\  \\     /|/\\|\\  \\     /:/\\:\\  \\     /:/\\:\\  \\     /|/\\|\\  \\ "
            " /::\\~\\:\\  \\   /|/ /||\\  \\   /::\\~\\:\\  \\   /:/  \\:\\  \\   _\\|\\~\\|\\  \\ "
            "/:/\\:\\ \\:\\__\\ /|/ /|/\\|\\__\\ /:/\\:\\ \\:\\__\\ /:/__/ \\:\\__\\ /\\ \\|\\ \\|\\__\\ "
            "\\:\\~\\:\\ \\/__/ \\/_/|/ /|/  / \\/__\\:\\/:/  / \\:\\  \\  \\/__/ \\|\\ \\|\\ \\/__/ "
            " \\:\\ \\:\\__\\     /|/ /|/  /       \\::/  /   \\:\\  \\        \\|\\ \\|\\__\\  "
            "  \\:\\ \\/__/     \\/_/|/  /        /:/  /     \\:\\  \\        \\|\\/|/  / "
            "   \\:\\__\\         /|/  /        /:/  /       \\:\\__\\        \\||/  / "
            "    \\/__/         \\/__/         \\/__/         \\/__/         \\/__/ "))
         (longest-line (apply #'max (mapcar #'length banner)))
         (current-width (window-width))
         (banner-height (length banner))

         ;; Adjust padding to be more gentle
         (padding-top (max 0 (floor (/ (- (window-height) banner-height) +banner--top-pos))))
         (padding-string (make-string longest-line ?\s)))
    (let (
          (inhibit-read-only t)
          (color (random-color)))
      (erase-buffer)
      ;; Add top padding
      (dotimes (_ padding-top)
        (insert padding-string "\n"))
      (dolist (line banner)
        (let ((colored-line (propertize line 'face `(:foreground ,color))))
          (insert (+banner--center current-width
                  (concat colored-line (make-string (max 0 (- longest-line (length line))) 32))) "\n"))))

      ;; Reduced number of newlines between sections
    (read-only-mode 1)))

(defun setup-ascii-banner ()
  "Setup the ASCII banner and hook it to window resizing."
  (lambda ()
    (let ((buffer (get-buffer-create "*Letters*")))
      (with-current-buffer buffer
        (draw-ascii-banner-fn))
      buffer)
    (add-hook 'window-size-change-functions #'+banner--resize-handler)
    (switch-to-buffer "*Letters*")))


(defun +banner--resize-handler (_)
  "Redraw the ASCII banner when the window is resized."
  (let ((buffer (get-buffer "*Letters*")))
    (when (and buffer (window-live-p (get-buffer-window buffer)))
      (with-current-buffer buffer
        (draw-ascii-banner-fn)))))

(setq initial-buffer-choice (setup-ascii-banner))

;;; init-banner.el ends here
