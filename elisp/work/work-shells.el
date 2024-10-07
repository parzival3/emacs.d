;; What follows is a "manifest" equivalent to the command line you gave.
;; You can store it in a file that you may then pass to any 'guix' command
;; that accepts a '--manifest' (or '-m') option.

;; How to use it:
;; guix shell --pure -m emscripten.scm -- /bin/bash --init-file ./emsdk/emsdk_env.sh -i ./compile.sh

(specifications->manifest
  (list "bash" "binutils" "coreutils" "python" "make"))
