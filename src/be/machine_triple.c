/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#include "target.h"

#include <assert.h>
#include <stdbool.h>
#include "xmalloc.h"
#include "util.h"

struct ir_machine_triple_t {
	char *cpu_type;
	char *manufacturer;
	char *operating_system;
};

const char *ir_triple_get_cpu_type(const ir_machine_triple_t *machine)
{
	return machine->cpu_type;
}

const char *ir_triple_get_manufacturer(const ir_machine_triple_t *machine)
{
	return machine->manufacturer;
}

const char *ir_triple_get_operating_system(const ir_machine_triple_t *machine)
{
	return machine->operating_system;
}

void ir_triple_set_cpu_type(ir_machine_triple_t *machine, const char *cpu_type)
{
	free(machine->cpu_type);
	machine->cpu_type = xstrdup(cpu_type);
}

void ir_free_machine_triple(ir_machine_triple_t *machine)
{
	free(machine->cpu_type);
	free(machine->manufacturer);
	free(machine->operating_system);
	free(machine);
}

static char *extract_strdup(const char *src, size_t len)
{
	char *result = XMALLOCN(char, len+1);
	memcpy(result, src, len);
	result[len] = '\0';
	return result;
}

ir_machine_triple_t *ir_parse_machine_triple(const char *string)
{
	/* Split string into 'first-second-third' components */
	const char *first  = string;
	const char *second = strchr(string, '-');
	const char *third  = NULL;
	if (second != NULL) {
		second += 1;
		third = strchr(second, '-');
		if (third != NULL)
			third += 1;
	}

	/* Figure out what is what */
	char *cpu_type;
	char *manufacturer;
	char *os;
	if (second == NULL) {
		cpu_type     = xstrdup(string);
		manufacturer = xstrdup("unknown");
		os           = xstrdup("unknown");
	} else {
		size_t cpu_type_len = second - first - 1;
		cpu_type = extract_strdup(first, cpu_type_len);
		if (third == NULL || strstart(second, "linux")) {
			manufacturer = xstrdup("unknown");
			os           = xstrdup(second);
		} else {
			size_t manufacturer_len = third - second - 1;
			manufacturer = extract_strdup(second, manufacturer_len);
			os           = xstrdup(third);
			if (streq(manufacturer, "none") || manufacturer[0] == '\0') {
				free(manufacturer);
				manufacturer = xstrdup("unknown");
			}
		}
		if (streq(os, "none") || os[0] == '\0') {
			free(os);
			os = xstrdup("unknown");
		}
	}

	/* Create triple */
	ir_machine_triple_t *triple = XMALLOCZ(ir_machine_triple_t);
	triple->cpu_type         = cpu_type;
	triple->manufacturer     = manufacturer;
	triple->operating_system = os;
	return triple;
}

ir_machine_triple_t *ir_get_host_machine_triple(void)
{
#ifdef HOST_TRIPLE
	/* a triple for the host machine was defined in the Makefile
	 * or config.mak */
	ir_machine_triple_t *triple = parse_machine_triple(HOST_TRIPLE);
	if (triple == NULL)
		panic("Cannot parse configured HOST_TRIPLE '%s'", HOST_TRIPLE);
	return triple;
#else
	const char *cpu =
#if defined(__x86_64__) || defined(__amd64__)
		"x86_64";
#elif defined(__i686__)
		"i686";
#elif defined(__i386__)
		"i386";
#elif defined(__mips__)
		"mips";
#elif defined(__riscv)
		"riscv32";
#elif defined(__sparc__)
		"sparc";
#elif defined(__arm__)
		"arm";
#elif defined(__aarch64__)
		"aarch64";
#else
		"unknown";
#endif

	const char *manufacturer =
#if defined(__leon__)
		"leon";
#elif defined(__APPLE__)
		"apple";
#else
		"unknown";
#endif

	const char *operating_system =
#if defined(_WIN32) || defined(__CYGWIN__)
		"win32";
#elif defined(__APPLE__)
		"darwin";
#elif defined(__FreeBSD__)
		"freebsd";
#elif defined(__OpenBSD__)
		"openbsd";
#elif defined(__gnu_linux__)
		"linux-gnu";
#elif defined(__linux__)
		"linux";
#elif defined(__midipix__)
		"midipix";
#elif defined(__ELF__)
		"elf";
#else
		"unknown";
#endif

	ir_machine_triple_t *triple = XMALLOCZ(ir_machine_triple_t);
	triple->cpu_type = xstrdup(cpu);
	triple->manufacturer = xstrdup(manufacturer);
	triple->operating_system = xstrdup(operating_system);
	return triple;
#endif
}
