SUBDIRS = include/libfirm ir

EXTRA_DIST = README.md COPYING NEWS.md m4/pkg.m4 m4/ax_cflags_gcc_option.m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libfirm.pc

LIBTOOL_DEPS = @LIBTOOL_DEPS@
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

ACLOCAL_AMFLAGS = -I m4
