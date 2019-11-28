ifneq ($(V),1)
Q ?= @
endif

name ?= liblfmalloc

sizes_include ?=

includedir ?= include
srcdir ?= src
builddir ?= build
#gendir ?= $(builddir)/gen

sources ?= $(wildcard $(srcdir)/*.c)
headers ?= $(wildcard $(includedir)/*.h) $(sizes_include)
objects ?= $(sources:$(srcdir)/%.c=$(builddir)/%.o)

CFLAGS ?= -ggdb -pthread -Wall -fPIC

$(builddir)/$(name).a: $(objects)
	@echo 'AR $@'
	$(Q)ar rcs $(builddir)/$(name).a $(objects)

$(builddir)/$(name).so: CFLAGS += -DLF_DYNAMIC_LIB
$(builddir)/$(name).so: $(objects)
	@echo 'LINK $@'
	$(Q)gcc -shared -o $(builddir)/$(name).so $(objects)

$(builddir)/%.o: $(srcdir)/%.c $(headers)
	@echo 'CC $@'
	$(Q)gcc -c $< $(CFLAGS) -I $(includedir) -I $(sizes_include) -o $@

.PHONY: clean

clean:
	-$(Q)rm -rf $(builddir)

print-%  : ; @echo $* = $($*)
