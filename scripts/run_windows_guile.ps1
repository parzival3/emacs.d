# Create a shortcut to this file and place it on the desktop
# C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -ExecutionPolicy Bypass -File "C:\Users\ento\.emacs.d\scripts\run_msys2_emacs.ps1"

$vsinst = Get-VSSetupInstance
$instpath = $vsinst[0].InstallationPath + "\\Common7\\Tools\Microsoft.VisualStudio.DevShell.dll"

Import-Module $instpath
Enter-VsDevShell -DevCmdArguments " -no_logo" $vsinst[0].InstanceId
$env:MSYSTEM="MINGW64"
$env:MSYS="winsymlinks:nativestrict"
$env:MSYS2_PATH_TYPE="inherit"
$env:PLATFORM="x64"
C:/msys64/usr/bin/bash.exe --login -c "guile --listen=9919"
