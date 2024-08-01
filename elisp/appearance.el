

(use-package compile
  :config
  (set-face 'compilation-error 'nano-face-header-critical))

(use-package eww
  :config
  (set-face 'shr-text 'nano-face-default))

(use-package dired
  :config
  (set-face dired-directory-face 'nano-face-popout))

(use-package meow
  :config
  (set-face 'meow-normal-cursor 'nano-face-popout)
  (set-face 'meow-beacon-cursor 'nano-face-popout)
  (set-face 'meow-insert-cursor 'nano-face-popout)
  (set-face 'meow-keypad-cursor 'nano-face-popout)
  (set-face 'meow-motion-cursor 'nano-face-popout)
  (set-face 'meow-kmacro-cursor 'nano-face-popout)
  (set-face 'meow-unknown-cursor 'nano-face-popout))
