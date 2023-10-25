#!/bin/bash

# Navigate to your Emacs configuration directory
cd ~/.emacs.d

# Update straight.el itself
# Update straight.el itself
emacs -Q --batch --eval "(progn
  (message \"Updating straight.el\")
  (package-initialize) (add-to-list 'load-path \"~/.emacs.d/straight/build/straight\")
  (require 'straight)
  (straight-pull-all))"
