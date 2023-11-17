(defun et-copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(use-package emacs
  :config
  ;; Define a new prefix for only git/vc/magit commands
  (define-prefix-command 'vc-actions-map nil "prefix for all the vc/magit actions")
  (global-set-key (kbd "<f1>") 'vc-actions-map)
  (define-key 'vc-actions-map (kbd "<f1>") #'vc-next-action)
  (define-key 'vc-actions-map (kbd "<f2>") #'magit-push)

  ;; Define a new prefix for the languages actions
  (define-prefix-command 'language-actions-map nil "prefix for all the languages actions")
  (global-set-key (kbd "<f2>") 'language-actions-map)
  (define-key 'language-actions-map (kbd "<f2>") 'whitespace-mode)
  (define-key 'language-actions-map (kbd "h") 'hs-hide-all)
  (define-key 'language-actions-map (kbd "r") 'eglot-rename)
  (define-key 'language-actions-map (kbd "f") 'eglot-format)

  (define-prefix-command 'org-actions-map nil "prefix for all the org actions")
  (global-set-key (kbd "<f3>") 'org-actions-map)
  (define-key 'org-actions-map (kbd "j") #'org-roam-insert-jurnal)
  (define-key 'org-actions-map (kbd "f") #'org-roam-insert-feeling)
  (define-key 'org-actions-map (kbd "t") #'org-todo-capture)
  (define-key 'org-actions-map (kbd "l") #'org-list-of-notes)

  (global-set-key (kbd "M-/") #'copilot-previous-completion)
  (global-set-key (kbd "M-\\") #'copilot-next-completion)

  (global-set-key (kbd "M-<up>") #'enlarge-window)
  (global-set-key (kbd "M-<down>") #'shrink-window)
  (global-set-key (kbd "M-<left>") #'shrink-window-horizontally)
  (global-set-key (kbd "M-<right>") #'enlarge-window-horizontally)
  (global-set-key (kbd "TAB") #'et-copilot-tab)
  (global-set-key (kbd "<xterm-paste>") #'scroll-up-command)
  (global-set-key (kbd "<f12>")  #'et-format-code-or-region)
  (global-set-key (kbd "C-x o")  #'et-other-window)

(use-package meow
  :straight t
  :ensure t
  :config
  ; wsl-copy
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "/mnt/c/Windows/System32/clip.exe")
    (kill-ring-save start end)
    (deactivate-mark))

  (defun sys-clipoard-copy ()
    ;; copy system clipboard to kill ring
    (interactive)
    (let ((powershell-program "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
          (clip-program "Get-Clipboard"))
      (kill-new
       (replace-regexp-in-string
        "\r" ""
        (replace-regexp-in-string
         "\n$" ""
         (with-output-to-string
           (with-current-buffer standard-output
             (call-process powershell-program nil t nil "-Command" clip-program))))))))

                                        ; wsl-paste
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

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     ;; '("j" . "H-j")
     ;; '("k" . "H-k")
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
     '("ws" . split-window-below)
     '("wv" . split-window-right)
     '("wk" . windmove-up)
     '("wj" . windmove-down)
     '("wh" . windmove-left)
     '("wl" . windmove-right)

     ;; Lsp
     '("a"  . eglot-code-actions)

     ;; Copilot
     '("."  . copilot-accept-completion)
     '("["  . copilot-previous-completion)
     '("]"  . copilot-next-completion)

     ;; Search
     '("sd" . et-search-for-word-in-directory)
     '("sf" . find-dired)

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
     `("p" . ,(if (not (display-graphic-p))
                  #'wsl-paste
                #'meow-clipboard-yank))
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     `("s" . ,(if (not (display-graphic-p))
                  #'wsl-cut
                #'meow-clipboard-kill))
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     `("y" . ,(if (not (display-graphic-p))
                 #'wsl-copy
               #'meow-clipboard-save))
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("`" . consult-imenu)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))
