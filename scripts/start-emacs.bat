start /B x410.exe /desktop
wsl.exe run "if [ -z \"$(pidof emacs)\" ]; then export DISPLAY=127.0.0.1:0.0; /git/emacs/src/emacs& ; taskkill.exe /IM x410.exe; fi;"
