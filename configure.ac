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
m4_define([firm_minor_version], [10])
m4_define([firm_micro_version], [0])
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

dnl set auto documentation
dnl ----------------------
AC_ARG_ENABLE([autodoc],
[AS_HELP_STRING([--enable-autodoc], [enable auto documentation])],
[enable_auto_documentation="$enableval"], [enable_auto_documentation="no"])

AC_SUBST([enable_auto_documentation])

dnl set firm jni
dnl ------------
AC_ARG_ENABLE([firmjni],
[AS_HELP_STRING([--enable-firmjni], [check for tools necesarry to construct a java native interface for Firm])],
[enable_firm_jni="$enableval"], [enable_firm_jni="no"])

AC_SUBST(enable_firm_jni)

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
[enable_statistics="$enableval"], [enable_statistics="no"])

if test "$enable_statistics" = yes; then
	FIRMCONFIG_FIRM_STATISTICS="#define FIRMCONFIG_FIRM_STATISTICS"
fi
AC_SUBST([FIRMCONFIG_FIRM_STATISTICS])
AC_SUBST([enable_statistics])

dnl enable libcore support
dnl --------------------------------
AC_ARG_ENABLE([libcore],
[AS_HELP_STRING([--disable-libcore], [disable libcore support])],
[enable_libcore="$enableval"],[enable_libcore="yes"])

AC_SUBST([enable_libcore])

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

dnl disable external effects in XML
dnl -------------------------------
dnl AC_ARG_ENABLE([external-effects],
dnl [AS_HELP_STRING([--enable-external-effects], [enable reading of descriptions of external effects in XML])],
dnl [enable_external_effects="$enableval"], [enable_external_effects="no"])

dnl AC_SUBST(enable_external_effects)

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

dnl check for doxygen if enabled
dnl ----------------------------
if test "$enable_auto_documentation" = yes; then
  AC_CHECK_PROG(DOXYGEN, doxygen, "doxygen", "")
  if test "$DOXYGEN" != "doxygen"; then
    AC_MSG_ERROR(need doxygen for auto documentation)
  fi
  AC_CHECK_PROG(DOT, dot, "dot", "")
  if test "$DOT" != "dot"; then
    AC_MSG_ERROR(need dot for doxygen documentation (for class graphs) (IPD: module add Graphviz))
  fi
fi

dnl check for availability of a jdk
dnl -------------------------------
if test "$enable_firm_jni" = yes; then
  AC_CHECK_PROG(JAVAC, javac, "javac", "")
  if test "$JAVAC" != "javac"; then
    AC_MSG_ERROR(need java compiler javac to generate jni (IPD: module add jdk-1.3.1-sun))
  fi
  AC_CHECK_PROG(JAVAH, javah, "javah", "")
  if test "$JAVAH" != "javah"; then
    AC_MSG_ERROR(need javah to generate jni headers (IPD: module add jdk-1.3.1-sun))
  fi
fi

dnl check for availability of pkg-config
PKG_PROG_PKG_CONFIG


dnl Checks for header files.
dnl ========================

AC_HEADER_STDC

dnl check for the math header file

AC_CHECK_HEADERS(math.h, ac_math_headers="yes", ac_math_headers="no")
if test "$ac_math_headers" != yes; then
  dnl math header not found.
  AC_MSG_ERROR("math header file not found")
fi


dnl check for the obstack header file
dnl does not work with:
dnl  - cygwin
dnl  - MacOSX
#AC_FUNC_OBSTACK

AC_CHECK_HEADERS(obstack.h, ac_obstack_headers="yes", ac_obstack_headers="no")
if test "$ac_obstack_headers" != yes; then
  dnl obstack header not found.
  AC_MSG_ERROR("obstack.h not found")
fi


AC_CHECK_HEADERS(alloca.h, ac_alloca_headers="yes", ac_alloca_headers="no")
if test "$ac_alloca_headers" = "yes"; then
    AC_DEFINE(HAVE_ALLOCA_H)
else
  if test "$ac_cv_header_stdc" = "no"; then
    AC_MSG_ERROR("alloca.h and stdlib.h not found")
  fi
fi


dnl check for jni header files

if test "$enable_firm_jni" = yes; then
  AC_CHECK_HEADERS(jni.h, ac_jni_headers="yes", ac_jni_headers="no")
  if test "$ac_jni_headers" != yes; then
    AC_MSG_ERROR(jni header file not found. (IPD: add --with-includedir=/usr/public2/java/jdk1.3.1-sun/include/:/usr/public2/java/jdk1.3.1-sun/include/linux/ to configure flags))
  fi
fi


if test "$enable_external_effects" == "yes"; then
	dnl check for libxml2 library installation dir
	AC_CHECK_PROG(XML2CONFIG, xml2-config, "xml2-config", "echo")
	if test "$XML2CONFIG" != "xml2-config"; then
	  AC_MSG_ERROR(xml2-config for detecting libxml2 not found)
	fi
	AC_SUBST(XML2CONFIG)
	LDFLAGS=$LDFLAGS" -L/usr/lib `$XML2CONFIG --libs`";
	CPPFLAGS=$CPPFLAGS" `$XML2CONFIG --cflags`";

	AC_CHECK_HEADERS(libxml/parser.h, ac_xml_headers="yes", ac_xml_headers="no")
	if test "$ac_xml_headers" != yes; then
		AC_MSG_ERROR(xml header files not found. (IPD: add --with-includedir=/usr/public/libxml2/include/libxml2/ to configure flags))
	fi
fi

dnl Checks for libraries.
dnl =====================

AC_CHECK_LIB(m, main, ac_m_library="yes", ac_m_library="no")
if test "$ac_m_library" != yes; then
  dnl std math library not found.
  AC_MSG_ERROR("standard math library not found")
fi


if test "$enable_external_effects" == "yes"; then
  AC_CHECK_LIB(xml2, xmlParseFile, ac_xml_library="yes", ac_xml_library="no")
  if test "$ac_m_library" != yes; then
    AC_MSG_ERROR("xml2 library not found (IPD: add --with-libdir=/usr/public/libxml2/lib/ to configure flags)")
  else
    LIBS=$LIBS" -lxml2"
  fi
fi

if test "$enable_libcore" == "yes"; then
	PKG_CHECK_MODULES([LIBCORE], [libcore])
	FIRMCONFIG_WITH_LIBCORE="#define WITH_LIBCORE"
fi
AC_SUBST([FIRMCONFIG_WITH_LIBCORE])

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
AC_CONFIG_COMMANDS([stamp-h], [touch stamp-h])

BACKENDS=""
if test "$enable_backend" == yes; then
	BACKENDS="arm ia32 mips ppc32"
fi
AC_SUBST([BACKENDS])

#if test "$BACKENDS"; then
#	for backend in $BACKENDS; do
#		AC_CONFIG_FILES([ir/be/$backend/Makefile])
#	done
#fi

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
echo "     libcore               $enable_libcore"
echo "     ilp                   $enable_ilp"
echo "     jvm                   $enable_jvm"
echo "     wide char support     $enable_wchar_support"
dnl echo "     external effects xml  $enable_external_effects"
echo ""
