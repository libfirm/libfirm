dnl configure.ac file for libfirm, author Matthias Braun <matze@braunis.de>
dnl Process this file with autoconf to produce a configure script.
AC_PREREQ([2.60])

dnl Firm Versions
dnl * Increment major/minor/micro version as you see fit. These 3 numbers
dnl   are intended for humans and are independent from the libtool versions
m4_define([firm_major_version], [1])
m4_define([firm_minor_version], [21])
m4_define([firm_micro_version], [0])
m4_define([firm_version],
          [firm_major_version.firm_minor_version.firm_micro_version])

dnl Libtool versions
dnl
dnl * If any code has changed at all (i.e. bugfixes) increment revision
dnl * If any interface has been added, removed or changed increment
dnl   current, set revision to 0
dnl * If any interface has been added increment age
dnl * If any interfaces have been remove set age to 0
dnl * use lt_release if substantial things have been changed. The release can be
dnl   thought of as part of the library name. So changing the release creates
dnl   a new library.
m4_define([lt_current],  [1])
m4_define([lt_revision], [0])
m4_define([lt_age],      [0])
dnl we use firm major version as release
m4_define([lt_release],  [firm_major_version])

AC_INIT([libfirm], [firm_version], [firm@ipd.info.uni-karlsruhe.de])
AC_CONFIG_SRCDIR([ir/common/firm.c])
AM_INIT_AUTOMAKE([foreign dist-bzip2 silent-rules 1.11])

AM_SILENT_RULES([yes])

AC_DEFINE([libfirm_VERSION_MAJOR], [firm_major_version], [Firms major version number])
AC_DEFINE([libfirm_VERSION_MINOR], [firm_minor_version], [Firms minor version number])
AC_DEFINE([libfirm_VERSION_MICRO], [firm_micro_version], [Firms micro version number])

LT_VERSION="lt_current:lt_revision:lt_age"
AC_SUBST([LT_VERSION])
#LT_RELEASE="lt_release"
#AC_SUBST([LT_RELEASE])

dnl enable libtool
LT_INIT([win32-dll])
AC_SUBST([LIBTOOL_DEPS])

dnl generate the config header file
AC_CONFIG_HEADER([config.h])
CPPFLAGS=$CPPFLAGS" -include config.h"

AC_DEFINE([FIRM_BUILD], [], [Defined when firm library is built])
AC_DEFINE([FIRM_DLL], [], [Define when a dynamically loadable shared library is built])

AC_CONFIG_MACRO_DIR([m4])

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
	AC_DEFINE([DEBUG_libfirm], [], [define to enable debug mode and checks])
fi

AC_ARG_ENABLE([assert],
[AS_HELP_STRING([--disable-assert], [disable assertions])],
[enable_assert="$enableval"], [enable_assert="yes"])
if test "$enable_assert" = no; then
  AC_DEFINE([NDEBUG], [], [define to disable asserts])
fi
AC_SUBST([enable_assert])

dnl Checks for programs.
dnl ====================

dnl check for a C compiler
dnl ----------------------
AC_PROG_CC

dnl enable visibility hidden (if supported)
AX_CFLAGS_GCC_OPTION([-fvisibility=hidden])


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

dnl Error messaging
dnl ===============


dnl Output results
AC_CONFIG_FILES([
	Makefile
	ir/Makefile
	include/libfirm/Makefile
	libfirm.pc
])

AC_OUTPUT
