@echo off
if not exist xd1.dsk goto download
if not exist xd2.dsk goto download
if not exist bin\Kronos3vm.exe goto build
bin\Kronos3vm.exe \\.\A: xd1.dsk xd2.dsk
goto done

:download
echo please open http://groups.google.com/group/kronos-project/files 
echo download and unpack xd1.dsk and xd2.dsk on this directory
pause
rundll32 url.dll,FileProtocolHandler http://groups.google.com/group/kronos-project/files
goto done

:build
Use Dev Studio 2008 Express or later to build Kronos3VM from int/ folder
call "C:\Program Files\Microsoft Visual Studio 9.0\Common7\Tools\vsvars32.bat"
pushd int
"C:\Program Files\Microsoft Visual Studio 9.0\VC\vcpackages\vcbuild" Kronos3vm.sln 
popd
:done
