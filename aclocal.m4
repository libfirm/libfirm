
dnl libFIRM Project

dnl Macros for autoconf
dnl $Id$


dnl Set variable `LD' to the name of the linker program.
AC_DEFUN(LIBFIRM_PROG_LD,[
dnl AC_REQUIRE([AC_PROG_CC])
if test -z "$LD"; then
  AC_CHECK_TOOL(LD, ld)
fi
])


dnl Set variable `libfirm_cv_prog_ld_r' to yes if the linker accepts -r, else to no.
AC_DEFUN(LIBFIRM_PROG_LD_R,
[AC_CACHE_CHECK(whether $LD accepts -r, libfirm_cv_prog_ld_r,
[AC_REQUIRE([LIBFIRM_PROG_LD])
libfirm_cv_prog_ld_r=no
if test "$LD"; then
  cat > conftest.$ac_ext <<EOF
int foo() { return 0; }
EOF
  if eval $ac_compile && mv conftest.o conftest2.o; then
    cat > conftest.$ac_ext <<EOF
extern int foo();
int main() { return foo(); }
EOF
    if eval $ac_compile; then
      if $LD -r -o conftest conftest.o conftest2.o; then
	libfirm_cv_prog_ld_r=yes
      fi
    fi
  fi
fi
rm -f conftest*])
AC_SUBST(libfirm_cv_prog_ld_r)
])

dnl end of aclocal.m
