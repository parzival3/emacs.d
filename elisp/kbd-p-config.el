;;; kbd.el --- Meow configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package meow
  :straight t
  :ensure t
  :config

  (defun et-is-current-coding-system (coding-system)
    (let ((eol-type-memonic (coding-system-eol-type-mnemonic buffer-file-coding-system)))
      (cond
       ((eq coding-system 'dos) (string-equal eol-type-memonic eol-mnemonic-dos))
       ((eq coding-system 'unix) (string-equal eol-type-memonic eol-mnemonic-unix)))))

  (defun et-clean-clipboard-yank (original-yank &rest args)
    "Remove extra carriage returns from the clipboard before yanking only if the buffer is unix
     or we don't have a file (which means we are trying to debug something or playing with the scratch
     buffer."
    (if (or (not buffer-file-name) (et-is-current-coding-system 'unix))
        ;; sanitize
        (progn
          (let ((cleaned-clip (replace-regexp-in-string "\r" "" (current-kill 0))))
               (kill-new cleaned-clip)
               (apply original-yank args)))
        ;; else
       (apply original-yank args)))


  ; wsl-copy
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "/mnt/c/Windows/System32/clip.exe")
    (kill-ring-save start end)
    (deactivate-mark))

  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -command 'Get-Clipboard'")))
      (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
      (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
      (insert clipboard)))

  (defun wsl-cut (start end)
    (interactive "r")
    (wsl-copy start end)
    (delete-region start end))

  (defun platform-copy ()
    (interactive)
    (cond
     ((eq et-system-type 'wsl) (call-interactively #'wsl-copy))
     ((eq et-system-type 'darwin) (call-interactively #'meow-clipboard-save))
     ((eq et-system-type 'gnu/linux) (call-interactively #'meow-clipboard-save))
     ((eq et-system-type 'windows-nt) (call-interactively #'meow-clipboard-save))))

  (defun platform-paste ()
    (interactive)
    (cond
     ((eq et-system-type 'wsl) (call-interactively #'wsl-paste))
     ((eq et-system-type 'darwin) (call-interactively #'meow-clipboard-yank))
     ((eq et-system-type 'gnu/linux) (call-interactively #'meow-clipboard-yank))
     ((eq et-system-type 'windows-nt) (call-interactively #'meow-clipboard-yank))))

  (defun platform-cut ()
    (interactive)
    (cond
     ((eq et-system-type 'wsl) (call-interactively #'wsl-cut))
     ((eq et-system-type 'darwin) (call-interactively #'meow-clipboard-kill))
     ((eq et-system-type 'gnu/linux) (call-interactively #'meow-clipboard-kill))
     ((eq et-system-type 'windows-nt) (call-interactively #'meow-clipboard-kill))))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    (meow-leader-define-key
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
     '("ws" . et-split-window-below-and-move-there-dammit)
     '("wv" . et-split-window-right-and-move-there-dammit)
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

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . platform-paste)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . platform-cut)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . platform-copy)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("`" . consult-imenu)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))

(provide 'kbd)
