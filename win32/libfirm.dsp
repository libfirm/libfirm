# Microsoft Developer Studio Project File - Name="libfirm" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libfirm - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "libfirm.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "libfirm.mak" CFG="libfirm - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "libfirm - Win32 Release" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "libfirm - Win32 Debug" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libfirm - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "../../obstack" /I "../win32" /I "../ir/adt" /I "../ir/ana" /I "../ir/common" /I "../ir/debug" /I "../ir/ident" /I "../ir/ir" /I "../ir/st" /I "../ir/tr" /I "../ir/tv" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /YX /FD /c
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libfirm - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "../../obstack" /I "../win32" /I "../ir/adt" /I "../ir/ana" /I "../ir/common" /I "../ir/debug" /I "../ir/ident" /I "../ir/ir" /I "../ir/st" /I "../ir/tr" /I "../ir/tv" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /YX /FD /GZ /c
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "libfirm - Win32 Release"
# Name "libfirm - Win32 Debug"
# Begin Group "Header-Dateien"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\config.h
# End Source File
# End Group
# Begin Group "FIRM"

# PROP Default_Filter ""
# Begin Group "adt"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\adt\array.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\array.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\cookies.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\debug.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\debug.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\eset.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\eset.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\host.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\misc.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\obst.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pdeq.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pdeq.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pmap.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pmap.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pset.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\set.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\set.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\xmalloc.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\xmalloc.h
# End Source File
# End Group
# Begin Group "ana"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\ana\cgana.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\cgana.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irbackedge.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irbackedge_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irdom.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irdom.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irdom_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irloop.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irloop_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irouts.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irouts.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irscc.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irsimpletype.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irsimpletype.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irtypeinfo.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irtypeinfo.h
# End Source File
# End Group
# Begin Group "common"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\common\firm.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\firm.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\firm_common.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\firm_common.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\firm_common_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\firmwalk.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\firmwalk.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\panic.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\panic.h
# End Source File
# End Group
# Begin Group "debug"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\debug\dbginfo.c
# End Source File
# Begin Source File

SOURCE=..\ir\debug\dbginfo.h
# End Source File
# Begin Source File

SOURCE=..\ir\debug\dbginfo_t.h
# End Source File
# End Group
# Begin Group "ident"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\ident\ident.c
# End Source File
# Begin Source File

SOURCE=..\ir\ident\ident.h
# End Source File
# Begin Source File

SOURCE=..\ir\ident\ident_t.h
# End Source File
# End Group
# Begin Group "ir"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\ir\ircgcons.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\ircgcons.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\ircgopt.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\ircgopt.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\ircons.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\ircons.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irdump.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irdump.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irflag.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irflag.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgmod.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgmod.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgopt.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgopt.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgraph.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgraph.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgraph_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgwalk.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irgwalk.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irmode.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irmode.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irmode_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irnode.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irnode.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irnode_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irop.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irop.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irop_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iropt.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iropt.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iropt_dbg.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iropt_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irprog.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irprog.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irprog_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irvrfy.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irvrfy.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\old_fctnames.h
# End Source File
# End Group
# Begin Group "st"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\st\bs.h
# End Source File
# Begin Source File

SOURCE=..\ir\st\exc.c
# End Source File
# Begin Source File

SOURCE=..\ir\st\exc.h
# End Source File
# Begin Source File

SOURCE=..\ir\st\st.c
# End Source File
# Begin Source File

SOURCE=..\ir\st\st.h
# End Source File
# End Group
# Begin Group "tr"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\tr\entity.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\entity.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\entity_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\mangle.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\mangle.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\tpop.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\tpop.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\tpop_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\trvrfy.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\trvrfy.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\type.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\type.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\type_or_entity.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\type_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\typegmod.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\typegmod.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\typewalk.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\typewalk.h
# End Source File
# End Group
# Begin Group "tv"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\tv\fltcalc.c
# End Source File
# Begin Source File

SOURCE=..\ir\tv\fltcalc.h
# End Source File
# Begin Source File

SOURCE=..\ir\tv\strcalc.c
# End Source File
# Begin Source File

SOURCE=..\ir\tv\strcalc.h
# End Source File
# Begin Source File

SOURCE=..\ir\tv\tv.c
# End Source File
# Begin Source File

SOURCE=..\ir\tv\tv.h
# End Source File
# Begin Source File

SOURCE=..\ir\tv\tv_t.h
# End Source File
# End Group
# End Group
# End Target
# End Project
