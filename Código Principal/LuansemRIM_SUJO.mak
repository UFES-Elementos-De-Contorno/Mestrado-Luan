# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=LuansemRIM_SUJO - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to LuansemRIM_SUJO - Win32\
 Debug.
!ENDIF 

!IF "$(CFG)" != "LuansemRIM_SUJO - Win32 Release" && "$(CFG)" !=\
 "LuansemRIM_SUJO - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "LuansemRIM_SUJO.mak" CFG="LuansemRIM_SUJO - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "LuansemRIM_SUJO - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "LuansemRIM_SUJO - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "LuansemRIM_SUJO - Win32 Debug"
RSC=rc.exe
F90=fl32.exe

!IF  "$(CFG)" == "LuansemRIM_SUJO - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\LuansemRIM_SUJO.exe"

CLEAN : 
	-@erase ".\Release\LuansemRIM_SUJO.exe"
	-@erase ".\Release\LuansemRIM_SUJA.obj"
	-@erase ".\Release\Resources_Gauss40_SUJO.obj"
	-@erase ".\Release\ABulcao _SUJA.obj"
	-@erase ".\Release\LeituraeEscrita _SUJA.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /I "Release/" /c /nologo
F90_PROJ=/Ox /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/LuansemRIM_SUJO.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/LuansemRIM_SUJO.pdb" /machine:I386\
 /out:"$(OUTDIR)/LuansemRIM_SUJO.exe" 
LINK32_OBJS= \
	"$(INTDIR)/LuansemRIM_SUJA.obj" \
	"$(INTDIR)/Resources_Gauss40_SUJO.obj" \
	"$(INTDIR)/ABulcao _SUJA.obj" \
	"$(INTDIR)/LeituraeEscrita _SUJA.obj"

"$(OUTDIR)\LuansemRIM_SUJO.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "LuansemRIM_SUJO - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\LuansemRIM_SUJO.exe"

CLEAN : 
	-@erase ".\Debug\LuansemRIM_SUJO.exe"
	-@erase ".\Debug\LuansemRIM_SUJA.obj"
	-@erase ".\Debug\Resources_Gauss40_SUJO.obj"
	-@erase ".\Debug\ABulcao _SUJA.obj"
	-@erase ".\Debug\LeituraeEscrita _SUJA.obj"
	-@erase ".\Debug\LuansemRIM_SUJO.ilk"
	-@erase ".\Debug\LuansemRIM_SUJO.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Zi /I "Debug/" /c /nologo
F90_PROJ=/Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/LuansemRIM_SUJO.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/LuansemRIM_SUJO.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/LuansemRIM_SUJO.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/LuansemRIM_SUJO.exe" 
LINK32_OBJS= \
	"$(INTDIR)/LuansemRIM_SUJA.obj" \
	"$(INTDIR)/Resources_Gauss40_SUJO.obj" \
	"$(INTDIR)/ABulcao _SUJA.obj" \
	"$(INTDIR)/LeituraeEscrita _SUJA.obj"

"$(OUTDIR)\LuansemRIM_SUJO.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "LuansemRIM_SUJO - Win32 Release"
# Name "LuansemRIM_SUJO - Win32 Debug"

!IF  "$(CFG)" == "LuansemRIM_SUJO - Win32 Release"

!ELSEIF  "$(CFG)" == "LuansemRIM_SUJO - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\LuansemRIM_SUJA.f

"$(INTDIR)\LuansemRIM_SUJA.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Resources_Gauss40_SUJO.f

"$(INTDIR)\Resources_Gauss40_SUJO.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\ABulcao _SUJA.f"

"$(INTDIR)\ABulcao _SUJA.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\LeituraeEscrita _SUJA.f"

"$(INTDIR)\LeituraeEscrita _SUJA.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
