dnl
dnl Project:     libFIRM
dnl File name:   configure.in
dnl Purpose:
dnl Author:      Matthias Braun
dnl CVS-ID:      $Id$
dnl Copyright:   (c) 2002-2007 University of Karlsruhe
dnl Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
dnl
AC_PREREQ([2.54])
AC_REVISION([$Id$])
m4_define([firm_major_version], [1])
m4_define([firm_minor_version], [12])
m4_define([firm_micro_version], [1])
m4_define([firm_version],
          [firm_major_version.firm_minor_version.firm_micro_version])
AC_INIT([libfirm], [firm_version], [firm@ipd.info.uni-karlsruhe.de])
AC_CONFIG_SRCDIR([ir/common/firm.c])
AM_INIT_AUTOMAKE([foreign dist-bzip2 1.9])

# see libtool manual on what version numbers mean!
LT_VERSION=0:0:0
AC_SUBST([LT_VERSION])

AC_DEFINE([libfirm_VERSION_MAJOR], [firm_major_version], [Firms major version number])
AC_DEFINE([libfirm_VERSION_MINOR], [firm_minor_version], [Firms minor version number])
AC_DEFINE([libfirm_VERSION_MICRO], [firm_micro_version], [Firms micro version number])

dnl generate the config header file
AC_CONFIG_HEADER([config.h])

dnl keep track of the environment set by the user
libfirm_conf_env="${CC+CC='$CC'} ${CFLAGS+CFLAGS='$CFLAGS'} ${CPP+CPP='$CPP'} ${CPPFLAGS+CPPFLAGS='$CPPFLAGS'} ${LDFLAGS+LDFLAGS='$LDFLAGS'} ${LIBS+LIBS='$LIBS'}"
AC_SUBST(libfirm_conf_env)

dnl Package options
dnl ===============

dnl check for additional include dirs
AC_ARG_WITH(includedir, [  --with-includedir=add   colon seperated list of directories to include search path],
    CPPFLAGS=$CPPFLAGS" -I"`echo $withval|sed 's;\:; -I;g'`)

dnl check for additional library dirs
AC_ARG_WITH(libdir, [  --with-libdir=add       colon seperated list of directories to linker search path],
    LDFLAGS=$LDFLAGS" -L"`echo $withval|sed 's;\:; -L;g'`)

dnl set debugging
dnl -------------
AC_ARG_ENABLE([debug],
[AS_HELP_STRING([--disable-debug], [disable verifiers and additional debugging routines])],
[enable_debug_libfirm="$enableval"], [enable_debug_libfirm="yes"])
if test "$enable_debug_libfirm" = yes; then
	FIRMCONFIG_DEBUG_libfirm="#define DEBUG_libfirm"
fi
AC_SUBST([FIRMCONFIG_DEBUG_libfirm])
AC_SUBST([enable_debug_libfirm])

AC_ARG_ENABLE([assert],
[AS_HELP_STRING([--disable-assert], [disable assertions])],
[enable_assert="$enableval"], [enable_assert="yes"])
if test "$enable_assert" = no; then
  AC_DEFINE([NDEBUG], [], [define to disable asserts])
fi
AC_SUBST([enable_assert])

dnl disable inlining
dnl ----------------
AC_ARG_ENABLE([inlining],
[AS_HELP_STRING([--disable-inlining], [disable inline C-extension])],
[enable_inlining="$enableval"], [enable_inlining="yes"])

if test "$enable_inlining" = yes; then
  FIRMCONFIG_USE_INLINING="#define USE_INLINING"
fi
AC_SUBST([FIRMCONFIG_USE_INLINING])
AC_SUBST([enable_inlining])

dnl set profiling
dnl -------------
AC_ARG_ENABLE([profile],
[AS_HELP_STRING([--enable-profile], [enable profiling])],
[enable_profile_libfirm="$enableval"], [enable_profile_libfirm="no"])

AC_SUBST([enable_profile_libfirm])

dnl set heap analyses support
dnl -------------------------
AC_ARG_ENABLE([heapanalysis],
[AS_HELP_STRING([--enable-heapanalysis], [Compile with heap analysis.])],
[enable_heapanalysis=$enableval], [enable_heapanalysis="no"])

if test "$enable_heapanalysis" = "yes"; then
	FIRMCONFIG_DO_HEAPANALYSIS="#define DO_HEAPANALYSIS"
fi
AC_SUBST([FIRMCONFIG_DO_HEAPANALYSIS])
AC_SUBST([enable_heapanalysis])

dnl enable Firm hooks
dnl -------------
AC_ARG_ENABLE([hooks],
[AS_HELP_STRING([--disable-hooks], [disable Firm hooks])],
[enable_hooks="$enableval"], [enable_hooks="yes"])

if test "$enable_hooks" = yes; then
	FIRMCONFIG_FIRM_ENABLE_HOOKS="#define FIRM_ENABLE_HOOKS"
fi
AC_SUBST([FIRMCONFIG_FIRM_ENABLE_HOOKS])

dnl enable Firm statistics
dnl -------------
AC_ARG_ENABLE([statistics],
[AS_HELP_STRING([--enable-statistics], [enable Firm statistics])],
[enable_statistics="$enableval"], [enable_statistics="yes"])

if test "$enable_statistics" = yes; then
	FIRMCONFIG_FIRM_STATISTICS="#define FIRMCONFIG_FIRM_STATISTICS"
fi
AC_SUBST([FIRMCONFIG_FIRM_STATISTICS])
AC_SUBST([enable_statistics])

dnl enable ILP solver support
dnl --------------------------------
AC_ARG_ENABLE([ilp],
[AS_HELP_STRING([--enable-ilp], [enable ilp solver])],
[enable_ilp="$enableval"], [enable_ilp="no"])

if test "$enable_ilp" = yes; then
  FIRMCONFIG_WITH_ILP="#define FIRMCONFIG_WITH_ILP"
fi
AC_SUBST([FIRMCONFIG_WITH_ILP])
AC_SUBST(enable_ilp)

dnl enable JVM calling from Firm
dnl --------------------------------
AC_ARG_ENABLE([jvm],
[AS_HELP_STRING([--enable-jvm], [enable to call the jvm])],
[enable_jvm="$enableval"], [enable_jvm="no"])

if test "$enable_jvm" = yes; then
  FIRMCONFIG_WITH_JVM="#define FIRMCONFIG_WITH_JVM"
fi
AC_SUBST([FIRMCONFIG_WITH_JVM])
AC_SUBST(enable_jvm)

dnl enable wchar_t support for identifiers
dnl --------------------------------
AC_ARG_ENABLE([wchar_support],
[AS_HELP_STRING([--enable-wchar-support], [enable wchar_t support for identifiers (needed for java)])],
[enable_wchar_support="$enableval"], [enable_wchar_support="no"])

if test "$enable_wchar_support" = yes; then
  FIRMCONFIG_FIRM_ENABLE_WCHAR="#define FIRM_ENABLE_WCHAR"
fi
AC_SUBST([FIRMCONFIG_FIRM_ENABLE_WCHAR])
AC_SUBST([enable_wchar_support])

dnl disable backend
dnl ---------------
AC_ARG_ENABLE([backend],
[AS_HELP_STRING([--disable-backend], [disable backends])],
[enable_backend="$enableval"], [enable_backend="yes"])

AC_SUBST(enable_backend)

dnl Checks for programs.
dnl ====================

dnl check for a C compiler
dnl ----------------------
AC_PROG_CC
AC_PROG_LIBTOOL

AC_PATH_PROGS(PERL, perl perl5)

dnl check for awk
dnl -------------
AC_PROG_AWK

dnl Checks for header files.
dnl ========================

AC_HEADER_STDC
AC_CHECK_HEADERS([math.h], , AC_MSG_ERROR("math header file not found"))

dnl Checks for libraries.
dnl =====================

AC_CHECK_LIB(m, main, ac_m_library="yes", ac_m_library="no")
if test "$ac_m_library" != yes; then
  dnl std math library not found.
  AC_MSG_ERROR("standard math library not found")
fi

dnl Checks for typedefs, structures, and compiler characteristics.
dnl ==============================================================
AC_C_CONST
AC_C_VOLATILE
AC_C_INLINE
AC_C_BIGENDIAN(,,,)
AC_C_LONG_DOUBLE

dnl Checks for library functions.
dnl =============================

dnl check for strerror

AC_CHECK_FUNC(strerror,,
  AC_MSG_ERROR("need strerror function")
)

dnl we use the gnu extension obstack_printf
AC_DEFINE([_GNU_SOURCE], [], [we use gnu extensions])
AC_CHECK_FUNC(strerror,,
  AC_MSG_ERROR("need obstack_printf function")
)

dnl Error messaging
dnl ===============


dnl Output results
AC_CONFIG_FILES([
	firm_config.h
	Makefile
	ir/Makefile
	include/libfirm/Makefile
	libfirm.pc
])

BACKENDS=""
if test "$enable_backend" == yes; then
	BACKENDS="arm ia32 mips ppc32"
fi
AC_SUBST([BACKENDS])

AC_OUTPUT

dnl output summary of firm configuration
echo ""
echo "firm configuration summary:"
echo "     backends              $BACKENDS"
echo "     profiling             $enable_profile_libfirm"
echo "     debug/verifiers       $enable_debug_libfirm"
echo "     assertions            $enable_assert"
echo "     hooks                 $enable_hooks"
echo "     statistics            $enable_statistics"
echo "     ilp                   $enable_ilp"
echo "     jvm                   $enable_jvm"
echo "     wide char support     $enable_wchar_support"
echo ""
