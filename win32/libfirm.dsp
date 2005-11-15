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
# PROP Output_Dir "D:\work\libfirm\Release"
# PROP Intermediate_Dir "D:\work\libfirm\Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "../../obstack" /I "../win32" /I "../ir/adt" /I "../ir/ana" /I "../ir/common" /I "../ir/debug" /I "../ir/ident" /I "../ir/ir" /I "../ir/opt" /I "../ir/st" /I "../ir/stat" /I "../ir/tr" /I "../ir/tv" /I "../ir/arch" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"S:\local\ipd\lib\libfirm.lib"

!ELSEIF  "$(CFG)" == "libfirm - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "D:\work\libfirm\Debug"
# PROP Intermediate_Dir "D:\work\libfirm\Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "../../obstack" /I "../win32" /I "../ir/adt" /I "../ir/ana" /I "../ir/common" /I "../ir/debug" /I "../ir/ident" /I "../ir/ir" /I "../ir/opt" /I "../ir/st" /I "../ir/stat" /I "../ir/tr" /I "../ir/tv" /I "../ir/arch" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /FD /D /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"S:\local\ipd\lib\libfirm_g.lib"

!ENDIF 

# Begin Target

# Name "libfirm - Win32 Release"
# Name "libfirm - Win32 Debug"
# Begin Group "Win32-specific"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\config.h
# End Source File
# Begin Source File

SOURCE=.\firm_config.h
# End Source File
# Begin Source File

SOURCE=.\ieee754.h
# End Source File
# Begin Source File

SOURCE=.\math.c
# End Source File
# End Group
# Begin Group "FIRM"

# PROP Default_Filter ""
# Begin Group "adt"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\adt\align.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\array.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\array.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\bitfiddle.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\bitset.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\bitset_std.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\eset.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\eset.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\fourcc.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\hashptr.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\impl.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\iterator.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\iterator.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\list.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\Makefile.in
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

SOURCE=..\ir\adt\plist.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\plist.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pmap.c
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pmap.h
# End Source File
# Begin Source File

SOURCE=..\ir\adt\pset.c
# ADD CPP /D "PSET"
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

SOURCE=..\ir\ana\analyze_irg_args.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\analyze_irg_args.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\callgraph.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\callgraph.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\cgana.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\cgana.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\execution_frequency.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\execution_frequency.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\field_temperature.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\field_temperature.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\interval_analysis.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\interval_analysis.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irbackedge.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irbackedge_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\ircfscc.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irconsconfirm.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irconsconfirm.h
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

SOURCE=..\ir\ana\irextbb.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irextbb.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\irextbb_t.h
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
# Begin Source File

SOURCE=..\ir\ana\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\ana\phiclass.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\phiclass.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\phiclass_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\rta.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\rta.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana\trouts.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana\trouts.h
# End Source File
# End Group
# Begin Group "ana2"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\ana2\ecg.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\ecg.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\gnu_ext.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\irmemwalk.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\irmemwalk.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\lset.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\lset.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_comp.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_comp.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_ctx.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_ctx.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_debug.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_debug.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_init.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_init.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_mod.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_mod.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_name.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_name.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_util.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\pto_util.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\qset.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\qset.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\timing.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\timing.h
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\typalise.c
# End Source File
# Begin Source File

SOURCE=..\ir\ana2\typalise.h
# End Source File
# End Group
# Begin Group "arch"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\arch\archop.c
# End Source File
# Begin Source File

SOURCE=..\ir\arch\archop.h
# End Source File
# Begin Source File

SOURCE=..\ir\arch\modeconv.c
# End Source File
# Begin Source File

SOURCE=..\ir\arch\modeconv.h
# End Source File
# End Group
# Begin Group "be"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\be\be.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\be_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bearch.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bearch.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bearch_firm.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bearch_firm.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beasm_asm_gnu.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beasm_asm_gnu.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beasm_dump_globals.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beasm_dump_globals.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beasm_types.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bechordal.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bechordal.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bechordal_draw.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bechordal_draw.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bechordal_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bechordalspill.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopyheur.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopyilp.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopyopt.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopyopt.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopyoptmain.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopyoptmain.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopystat.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\becopystat.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bedupl.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beirgmod.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beirgmod.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\belistsched.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\belistsched.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\belive.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\belive.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\belive_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bemain.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\benode.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\benode_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\benumb.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\benumb.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\benumb_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bera.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bera.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bera_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\besched.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\besched.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\besched_t.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bessadestr.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\bessadestr.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beutil.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\beutil.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\lpp.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\lpp.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\lpp_local.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\lpp_local.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\lpp_remote.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\lpp_remote.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\mps.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\mps.h
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\sp_matrix.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\be\sp_matrix.h
# PROP Exclude_From_Build 1
# End Source File
# End Group
# Begin Group "common"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\common\debug.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\debug.h
# End Source File
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

SOURCE=..\ir\common\firm_types.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\firmwalk.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\firmwalk.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\irtools.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\irtools.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\common\panic.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\panic.h
# End Source File
# Begin Source File

SOURCE=..\ir\common\statistics.c
# End Source File
# Begin Source File

SOURCE=..\ir\common\statistics.h
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
# Begin Source File

SOURCE=..\ir\debug\debugger.c
# End Source File
# Begin Source File

SOURCE=..\ir\debug\debugger.h
# End Source File
# Begin Source File

SOURCE=..\ir\debug\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\debug\seqnumbers.c
# End Source File
# Begin Source File

SOURCE=..\ir\debug\seqnumbers.h
# End Source File
# End Group
# Begin Group "external"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\external\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\external\read.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\external\read.h
# End Source File
# Begin Source File

SOURCE=..\ir\external\read_t.h
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
# Begin Source File

SOURCE=..\ir\ident\Makefile.in
# End Source File
# End Group
# Begin Group "ir"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\ir\irarch.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irarch.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irargs.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irargs_t.h
# End Source File
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

SOURCE=..\ir\ir\ircons_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irdump.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irdump.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irdump_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irdumptxt.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iredges.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iredges.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\iredges_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irflag.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irflag.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irflag_t.h
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

SOURCE=..\ir\ir\irgopt_t.h
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

SOURCE=..\ir\ir\irgwalk_blk.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irhooks.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irhooks.h
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

SOURCE=..\ir\ir\irprintf.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irprintf.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irprintf_t.h
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

SOURCE=..\ir\ir\irreflect.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irreflect.def
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irreflect.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irreflect_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irvrfy.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irvrfy.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\irvrfy_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\ir\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\ir\pseudo_irg.c
# End Source File
# Begin Source File

SOURCE=..\ir\ir\pseudo_irg.h
# End Source File
# End Group
# Begin Group "opt"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\opt\cfopt.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\cfopt.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\escape_ana.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\escape_ana.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\funccall.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\funccall.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\ifconv.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\ifconv.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\ldstopt.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\ldstopt.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\loop_unrolling.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\loop_unrolling.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\opt\opt_branches.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\opt_branches.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\opt_confirms.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\opt_confirms.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\opt_polymorphy.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\opt_polymorphy.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\proc_cloning.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\proc_cloning.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\reassoc.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\reassoc.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\reassoc_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\return.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\return.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\scalar_replace.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\scalar_replace.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\strength_red.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\strength_red.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\tailrec.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\tailrec.h
# End Source File
# Begin Source File

SOURCE=..\ir\opt\tropt.c
# End Source File
# Begin Source File

SOURCE=..\ir\opt\tropt.h
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

SOURCE=..\ir\st\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\st\st.c
# End Source File
# Begin Source File

SOURCE=..\ir\st\st.h
# End Source File
# End Group
# Begin Group "stat"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\stat\const_stat.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\counter.h
# End Source File
# Begin Source File

SOURCE=..\ir\stat\dags.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\dags.h
# End Source File
# Begin Source File

SOURCE=..\ir\stat\distrib.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\firmstat.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\firmstat.h
# End Source File
# Begin Source File

SOURCE=..\ir\stat\firmstat_t.h
# End Source File
# Begin Source File

SOURCE=..\ir\stat\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\ir\stat\pattern.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\pattern.h
# End Source File
# Begin Source File

SOURCE=..\ir\stat\pattern_dmp.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\pattern_dmp.h
# End Source File
# Begin Source File

SOURCE=..\ir\stat\stat_dmp.c
# End Source File
# Begin Source File

SOURCE=..\ir\stat\stat_dmp.h
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

SOURCE=..\ir\tr\Makefile.in
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

SOURCE=..\ir\tr\tr_inheritance.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\tr_inheritance.h
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

SOURCE=..\ir\tr\type_identify.c
# End Source File
# Begin Source File

SOURCE=..\ir\tr\type_identify.h
# End Source File
# Begin Source File

SOURCE=..\ir\tr\type_identify_t.h
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

SOURCE=..\ir\tv\Makefile.in
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
# Begin Group "lower"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ir\lower\lower_calls.c
# End Source File
# Begin Source File

SOURCE=..\ir\lower\lower_calls.h
# End Source File
# Begin Source File

SOURCE=..\ir\lower\lower_intrinsics.c
# End Source File
# Begin Source File

SOURCE=..\ir\lower\lower_intrinsics.h
# End Source File
# End Group
# End Group
# End Target
# End Project
