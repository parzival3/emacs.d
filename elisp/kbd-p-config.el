;;; kbd.el --- Meow configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package meow
  :ensure t
  :init
  (defvar meow-normal-movement
    '(("0" . meow-expand-0)
       ("9" . meow-expand-9)
       ("8" . meow-expand-8)
       ("7" . meow-expand-7)
       ("6" . meow-expand-6)
       ("5" . meow-expand-5)
       ("4" . meow-expand-4)
       ("3" . meow-expand-3)
       ("2" . meow-expand-2)
       ("1" . meow-expand-1)
       ("-" . negative-argument)
       (";" . meow-reverse)
       ("," . meow-inner-of-thing)
       ("." . meow-bounds-of-thing)
       ("[" . meow-beginning-of-thing)
       ("]" . meow-end-of-thing)
       ("b" . meow-back-word)
       ("B" . meow-back-symbol)
       ("e" . meow-next-word)
       ("E" . meow-next-symbol)
       ("f" . meow-find)
       ("g" . meow-cancel-selection)
       ("G" . meow-grab)
       ("h" . meow-left)
       ("H" . meow-left-expand)
       ("I" . meow-open-above)
       ("j" . meow-next)
       ("J" . meow-next-expand)
       ("k" . meow-prev)
       ("K" . meow-prev-expand)
       ("l" . meow-right)
       ("L" . meow-right-expand)
       ("m" . meow-join)
       ("n" . meow-search)
       ("o" . meow-block)
       ("O" . meow-to-block)
       ("q" . meow-quit)
       ("Q" . meow-goto-line)
       ("t" . meow-till)
       ("u" . meow-undo)
       ("U" . meow-undo-in-selection)
       ("v" . meow-visit)
       ("w" . meow-mark-word)
       ("W" . meow-mark-symbol)
       ("x" . meow-line)
       ("X" . meow-goto-line)
       ("y" . meow-save)
       ("Y" . meow-sync-grab)
       ("z" . meow-pop-selection)
       ("'" . repeat)
       ("`" . consult-imenu)
       ("<escape>" . ignore)))

  (defvar meow-normal-actions
    '(("a" . meow-append)
       ("A" . meow-open-below)
       ("c" . meow-change)
       ("d" . meow-delete)
       ("D" . meow-backward-delete)
       ("p" . meow-yank)
       ("q" . meow-quit)
       ("r" . meow-replace)
       ("R" . meow-swap-grab)
       ("s" . meow-kill)))

  :config
  ;; In terminal mode esc is interpreted as a keycode command
  (when (not (display-graphic-p))
    (setq meow-esc-delay 0.01))

  (defun et-is-current-coding-system (coding-system)
    (let ((eol-type-memonic (coding-system-eol-type-mnemonic buffer-file-coding-system)))
      (cond
        ((eq coding-system 'dos) (string-equal eol-type-memonic eol-mnemonic-dos))
        ((eq coding-system 'unix) (string-equal eol-type-memonic eol-mnemonic-unix)))))

  ; wsl-copy

  (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
  (meow-thing-register 'ticks '(pair ("'") ("'")) '(pair ("'") ("'")))
  (add-to-list 'meow-char-thing-table '(?a . arrow))
  (add-to-list 'meow-char-thing-table '(?t . ticks)) ;; Not working?

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    (meow-motion-overwrite-define-key
      '("j" . meow-next)
      '("k" . meow-prev)
      '("<escape>" . ignore))

    (meow-leader-define-key
      '("."  . embark-act)
      '(";"  . avy-goto-char-timer)
      '("ff" . et-find-file)
      '("pp" . project-switch-project)
      '("pe" . project-eshell)
      '("po" . ff-find-other-file)
      '("gg" . magit-status)
      '("fp" . et-open-config)
      '("pc" . project-compile)
      '("bb" . consult-buffer)
      '("bd" . kill-current-buffer)
      '("br" . revert-buffer)
      '("bm" . consult-bookmark)

      ;; Windows movements
      '("ws" . et-split-window-below)
      '("wv" . et-split-window-right)
      '("wk" . windmove-up)
      '("wj" . windmove-down)
      '("wh" . windmove-left)
      '("wl" . windmove-right)

      ;; Use SPC (0-9) for digit arguments.
      '("1" . meow-digit-argument)
      '("2" . meow-digit-argument)
      '("3" . meow-digit-argument)
      '("4" . meow-digit-argument)
      '("5" . meow-digit-argument)
      '("6" . meow-digit-argument)
      '("7" . meow-digit-argument)
      '("8" . meow-digit-argument)
      '("9" . meow-digit-argument)
      '("0" . meow-digit-argument)
      '("/" . meow-keypad-describe-key)
      '("?" . meow-cheatsheet))

    (apply 'meow-normal-define-key `(,@meow-normal-movement ,@meow-normal-actions)))
    (meow-setup)
    (meow-global-mode 1))


(when wsl
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
  (setq interprogram-paste-function 'wl-paste))

(provide 'kbd)
