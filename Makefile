# libfirm Makefile
#
# This is currently experimental and not fully supported, but we plan to replace
# the jambuild with this at some point in the future.
#
# Most variable names are similar to the names used by autoconf...
-include config.mak

# Some build configuration defaults
top_srcdir   ?=
top_builddir ?= build
host         ?= unknown-host
variant      ?= debug

srcdir       ?= $(top_srcdir)
builddir     ?= $(top_builddir)/$(variant)

# This hides the noisy commandline outputs. You can see them with "make Q="
Q ?= @

CC ?= cc
LINK ?= $(CC)
AR ?= ar ru
RANLIB ?= ranlib

# Variants
CFLAGS_debug      = -O0 -g3 -DDEBUG_libfirm
CFLAGS_profile    = -O3 -pg -DNDEBUG -fno-inline
LINKFLAGS_profile = -pg
CFLAGS_optimize   = -O3 -DNDEBUG

# General flags
CFLAGS    += $(CFLAGS_$(variant))
CFLAGS    += -Wall -W -Wextra -Wstrict-prototypes -Wmissing-prototypes -Wwrite-strings
LINKFLAGS += $(LINKFLAGS_$(variant))
VPATH = $(srcdir)

.PHONY: all
all: firm

# A very naive way to create a config.h if it is missing
$(srcdir)config.h:
	@echo MakeConfig $@
	$(Q)rm -f $@
	$(Q)echo "#define libfirm_VERSION_MAJOR 1" >> $@
	$(Q)echo "#define libfirm_VERSION_MICRO 0" >> $@
	$(Q)echo "#define libfirm_VERSION_MINOR 20" >> $@
	$(Q)echo "#define HAVE_LONG_DOUBLE 1" >> $@
	$(Q)echo "#define FIRM_STATISTICS" >> $@

# libFirm
libfirm_DIRS := \
	ir         \
	ir/adt     \
	ir/ana     \
	ir/arch    \
	ir/common  \
	ir/debug   \
	ir/obstack \
	ir/ident   \
	ir/net     \
	ir/ir      \
	ir/lower   \
	ir/libcore \
	ir/opt     \
	ir/st      \
	ir/stat    \
	ir/tr      \
	ir/tv      \
	ir/be
libfirm_SOURCES  = $(foreach dir,$(libfirm_DIRS),$(wildcard $(dir)/*.c))
libfirm_a        = $(builddir)/libfirm.a
libfirm_so       = $(builddir)/libfirm.so
libfirm_CPPFLAGS = -Iinclude/libfirm -Iinclude/libfirm/adt -I. $(foreach dir,$(libfirm_DIRS),-I$(dir))

.PHONY: firm
firm: $(libfirm_so)

# backends
backends = amd64 arm ia32 sparc TEMPLATE

EMITTER_GENERATOR = $(srcdir)ir/be/scripts/generate_emitter.pl
REGALLOC_IF_GENERATOR = $(srcdir)ir/be/scripts/generate_regalloc_if.pl
OPCODES_GENERATOR = $(srcdir)ir/be/scripts/generate_new_opcodes.pl
MACHINE_GENERATOR = $(srcdir)ir/be/scripts/generate_machine.pl

define backend_template
$(1)_SOURCES = $$(wildcard ir/be/$(1)/*.c)
$(1)_SOURCES := $$(filter-out ir/be/$(1)/gen_%.c, $$($(1)_SOURCES))
$(1)_GEN_HEADERS =

$(1)_SPEC = ir/be/$(1)/$(1)_spec.pl

$$(srcdir)ir/be/$(1)/gen_$(1)_emitter.h $$(srcdir)ir/be/$(1)/gen_$(1)_emitter.c: $$($(1)_SPEC) $$(EMITTER_GENERATOR)
	@echo GEN $$@
	$(Q)$$(EMITTER_GENERATOR) $$($(1)_SPEC) $$(srcdir)ir/be/$(1)
$(1)_SOURCES += ir/be/$(1)/gen_$(1)_emitter.c
$(1)_GEN_HEADERS += ir/be/$(1)/gen_$(1)_emitter.h

$$(srcdir)ir/be/$(1)/gen_$(1)_regalloc_if.h $$(srcdir)ir/be/$(1)/gen_$(1)_regalloc_if.c: $$($(1)_SPEC) $$(REGALLOC_IF_GENERATOR)
	@echo GEN $$@
	$(Q)$$(REGALLOC_IF_GENERATOR) $$($(1)_SPEC) $$(srcdir)ir/be/$(1)
$(1)_SOURCES += ir/be/$(1)/gen_$(1)_regalloc_if.c
$(1)_GEN_HEADERS += ir/be/$(1)/gen_$(1)_regalloc_if.h

$$(srcdir)ir/be/$(1)/gen_$(1)_machine.h $$(srcdir)ir/be/$(1)/gen_$(1)_machine.c: $$($(1)_SPEC) $$(MACHINE_GENERATOR)
	@echo GEN $$@
	$(Q)$$(MACHINE_GENERATOR) $$($(1)_SPEC) $$(srcdir)ir/be/$(1)
$(1)_SOURCES += ir/be/$(1)/gen_$(1)_machine.c
$(1)_GEN_HEADERS += ir/be/$(1)/gen_$(1)_machine.h

$$(srcdir)ir/be/$(1)/gen_$(1)_new_nodes.h $$(srcdir)ir/be/$(1)/gen_$(1)_new_nodes.c.inl: $$($(1)_SPEC) $$(OPCODES_GENERATOR)
	@echo GEN $$@
	$(Q)$$(OPCODES_GENERATOR) $$($(1)_SPEC) $$(srcdir)ir/be/$(1)
$(1)_GEN_HEADERS += ir/be/$(1)/gen_$(1)_new_nodes.h

ir/be/$(1)/$(1)_new_nodes.c: ir/be/$(1)/gen_$(1)_new_nodes.c.inl

# We need to inform make of the headers it doesn't know yet...
$(1)_OBJECTS = $$($(1)_SOURCES:%.c=$$(builddir)/%.o)
$$($(1)_OBJECTS): $$($(1)_GEN_HEADERS)

libfirm_SOURCES += $$($(1)_SOURCES)
libfirm_DIRS += ir/be/$(1)
endef

$(foreach backend,$(backends),$(eval $(call backend_template,$(backend))))

# generators
IR_SPEC_GENERATED_FILES := \
	include/libfirm/nodeops.h \
	include/libfirm/opcodes.h \
	ir/ir/gen_ir_cons.c.inl   \
	ir/ir/gen_irop.c.inl      \
	ir/ir/gen_irnode.c.inl    \
	ir/ir/gen_irnode.h
IR_SPEC_GENERATOR := scripts/gen_ir.py
IR_SPEC := scripts/ir_spec.py

$(IR_SPEC_GENERATED_FILES): $(IR_SPEC_GENERATOR) $(IR_SPEC) scripts/spec_util.py
	@echo GEN $@
	$(Q)$(IR_SPEC_GENERATOR) $(IR_SPEC) ir/ir

IR_IO_GENERATOR := scripts/gen_ir_io.py
IR_IO_GENERATED_FILES := \
	ir/ir/gen_irio_import.inl \
	ir/ir/gen_irio_export.inl \
	ir/ir/gen_irio_lex.inl

$(IR_IO_GENERATED_FILES): $(IR_IO_GENERATOR) $(IR_SPEC) scripts/spec_util.py
	@echo GEN $@
	$(Q)$(IR_IO_GENERATOR) $(IR_SPEC) ir/ir

ir/ir/irio.c: $(IR_IO_GENERATED_FILES)

libfirm_OBJECTS = $(libfirm_SOURCES:%.c=$(builddir)/%.o)
libfirm_DEPS    = $(libfirm_OBJECTS:%.o=%.d)
-include $(libfirm_DEPS)

$(libfirm_a): $(libfirm_OBJECTS)
	@echo AR $@
	$(Q)$(AR) ru $@ $^
	@echo RANLIB $@
	$(Q)$(RANLIB) $@

$(libfirm_so): $(libfirm_OBJECTS)
	@echo LINK $@
	$(Q)$(LINK) -shared -o $@ $^

# Generic rules
UNUSED := $(shell mkdir -p $(libfirm_DIRS:%=$(builddir)/%))
$(builddir)/%.o: %.c $(IR_SPEC_GENERATED_FILES) config.h
	@echo CC $@
	$(Q)$(CC) $(CFLAGS) $(CPPFLAGS) $(libfirm_CPPFLAGS) -MMD -c -o $@ $<

.PHONY: clean
clean:
	@echo CLEAN
	$(Q)rm -f $(libfirm_OBJECTS)
	$(Q)rm -f $(libfirm_TARGET)
