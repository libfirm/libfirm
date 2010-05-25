SUBDIRS = include/libfirm ir

EXTRA_DIST = README COPYING NEWS acinclude.m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libfirm.pc

LIBTOOL_DEPS = @LIBTOOL_DEPS@
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck
