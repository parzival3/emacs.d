# This script to work needs
# Install-Module VSSetup -Scope CurrentUser

# Windows Terminal configuration
# {
#     "guid": "{17da3cac-b318-431e-8a3e-7fcdefe6d114}",
#     "name": "MSYS2",
#     "commandline": "C:\\Windows\\SysWOW64\\WindowsPowerShell\\v1.0\\powershell.exe  -noprofile -executionpolicy bypass -file C:/Users/ento/.emacs.d/scripts/msys2_powershell.ps1",
#     "startingDirectory": "%HOME%",
#     "icon": "C:/msys64/mingw64.ico"
# },

$vsinst = Get-VSSetupInstance
$instpath = $vsinst[0].InstallationPath + "\\Common7\\Tools\Microsoft.VisualStudio.DevShell.dll"

Import-Module $instpath
Enter-VsDevShell $vsinst[0].InstanceId

$env:MSYSTEM="MINGW64"
$env:MSYS="winsymlinks:nativestrict"
$env:MSYS2_PATH_TYPE="inherit"
$env:PLATFORM="x64"

C:/msys64/usr/bin/bash.exe --login
