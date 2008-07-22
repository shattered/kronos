# Microsoft Developer Studio Project File - Name="Kronos3vm" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Kronos3vm - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Kronos3vm.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Kronos3vm.mak" CFG="Kronos3vm - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Kronos3vm - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Kronos3vm - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "Kronos3vm - Win32 Hybrid" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Wonko/History/KronosV3/Emulator", EAAAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Kronos3vm - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /G5 /MT /W4 /O1 /Ob2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX"preCompiled.h" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i ".\SourceCode" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib ws2_32.lib gdi32.lib /nologo /subsystem:console /debug /machine:I386 /nodefaultlib

!ELSEIF  "$(CFG)" == "Kronos3vm - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /G5 /MTd /W4 /Gm /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX"preCompiled.h" /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /i ".\SourceCode" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib ws2_32.lib gdi32.lib /nologo /subsystem:console /debug /machine:I386 /nodefaultlib /pdbtype:sept

!ELSEIF  "$(CFG)" == "Kronos3vm - Win32 Hybrid"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Kronos3v"
# PROP BASE Intermediate_Dir "Kronos3v"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Hybrid"
# PROP Intermediate_Dir "Hybrid"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W4 /O2 /Ob2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX"preCompiled.h" /FD /c
# ADD CPP /nologo /G5 /MT /W4 /Zi /O1 /Ob2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "_HYBRID" /FR /YX"preCompiled.h" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i ".\SourceCode" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /nodefaultlib
# ADD LINK32 kernel32.lib user32.lib ws2_32.lib gdi32.lib /nologo /subsystem:console /debug /machine:I386 /nodefaultlib

!ENDIF 

# Begin Target

# Name "Kronos3vm - Win32 Release"
# Name "Kronos3vm - Win32 Debug"
# Name "Kronos3vm - Win32 Hybrid"
# Begin Group "Source Files"

# PROP Default_Filter "*.cpp"
# Begin Source File

SOURCE=.\SourceCode\cO_tcp.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\cO_win32.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\cO_win32_display.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\crtStartup.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\Disks.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\IGD480.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\Kronos3vm.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\Memory.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\preCompiled.cpp
# ADD CPP /Yc"preCompiled.h"
# End Source File
# Begin Source File

SOURCE=.\SourceCode\SIO.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\SIO_TCP.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\VM.cpp
# End Source File
# Begin Source File

SOURCE=.\SourceCode\vmConsole.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "*.h"
# Begin Source File

SOURCE=.\SourceCode\cO_tcp.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\cO_win32.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\cO_win32_display.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\Disks.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\IGD480.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\Memory.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\preCompiled.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\resource.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\SIO.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\SIO_TCP.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\VM.h
# End Source File
# Begin Source File

SOURCE=.\SourceCode\vmConsole.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "*.rc;*.ico"
# Begin Source File

SOURCE=.\Resources\kronos.ico
# End Source File
# Begin Source File

SOURCE=.\Resources\kronos3vm.rc

!IF  "$(CFG)" == "Kronos3vm - Win32 Release"

!ELSEIF  "$(CFG)" == "Kronos3vm - Win32 Debug"

!ELSEIF  "$(CFG)" == "Kronos3vm - Win32 Hybrid"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# End Group
# End Target
# End Project
