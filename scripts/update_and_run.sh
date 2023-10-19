#!/bin/bash

# Navigate to your Emacs configuration directory
cd ~/.emacs.d

# Update straight.el itself
emacs -Q --batch --eval "(progn (package-initialize) (add-to-list 'load-path \"~/.emacs.d/straight/build/straight\"))" --eval "(require 'straight)" --eval "(progn (setq straight-check-for-modifications '(check-on-save find-when-checking)) (straight-pull-all))"
# Run Emacs
emacs -nw
