GOAL=libfirmprof.a
LFLAGS=
CFLAGS=-Wall -W
OBJECTS=instrument.o
CC?=gcc
AR?=ar
RANLIB?=ranlib

.PHONY: clean

all: $(GOAL)

$(GOAL): $(OBJECTS)
	$(AR) rc $@ $(OBJECTS)
	$(RANLIB) $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(GOAL) $(OBJECTS)
