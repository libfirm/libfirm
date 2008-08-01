-include config.mak

FIRM_CFLAGS ?= `pkg-config --cflags libfirm`
FIRM_LIBS   ?= `pkg-config --libs libfirm`

CC          ?= gcc

CPPFLAGS += $(FIRM_CFLAGS)
CFLAGS += -Wall -W -Wstrict-prototypes -Wmissing-prototypes -Werror -pedantic
CFLAGS += -O0 -g3
LFLAGS += $(FIRM_LIBS)

SOURCES   = $(wildcard *.c)
GOALS     = $(basename $(SOURCES))

.PHONY: all

all: $(GOALS)

%: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS)
	strip $@

clean:
	rm -f $(GOALS)
