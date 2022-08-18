(defvar emsdk-mac-paths '("/Users/enrico/Git/emsdk" "/Users/enrico/Git/emsdk/node/14.18.2_64bit/bin" "/Users/enrico/Git/emsdk/upstream/emscripten")
  "List of directories to add to the PATH for emsdk")

(defun emsdk-add-directories ()
  "Add the emsdk directories to the Emacs exec-path and PATH environment variable"
  (interactive)
  (setq exec-path (append exec-path emsdk-mac-paths))
  (setenv "PATH" (mapconcat #'identity exec-path ":")))
