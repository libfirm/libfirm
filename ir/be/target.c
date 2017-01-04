/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#include "target_t.h"

#include "platform_t.h"
#include "be_t.h"
#include "bearchs.h"
#include "bediagnostic.h"
#include "util.h"

target_info_t ir_target;

int ir_init_target_triple(ir_machine_triple_t const *machine)
{
	if (ir_target.initialized)
		panic("Can only initialize target once");

	ir_init_no_target();

	memset(&ir_target, 0, sizeof(ir_target));
	ir_target.ia32_po2_stackalign = 2;

	const char *const cpu          = ir_triple_get_cpu_type(machine);
	const char *const manufacturer = ir_triple_get_manufacturer(machine);
	const char *const os           = ir_triple_get_operating_system(machine);
	char          const *arch      = NULL;
	arch_isa_if_t const *isa;
	if (ir_is_cpu_x86_32(cpu)) {
		isa = &ia32_isa_if;
		arch   = cpu;
	} else if (streq(cpu, "arm")) {
		isa = &arm_isa_if;
	} else if (streq(cpu, "sparc")) {
		isa = &sparc_isa_if;
		if (strstr(manufacturer, "leon") != NULL
		 || streq(manufacturer, "invasic"))
			arch = "leon";
	} else if (streq(cpu, "x86_64") || streq(cpu, "amd64")) {
		isa = &amd64_isa_if;
	} else if (streq(cpu, "mips")) {
		isa = &mips_isa_if;
	} else if (streq(cpu, "TEMPLATE")) {
		isa = &TEMPLATE_isa_if;
	} else {
		return false;
	}
	ir_target.isa = isa;
	ir_target.big_endian = isa->big_endian;

	unsigned const pointer_size = isa->pointer_size;
	object_format_t object_format;
	if (streq(os, "elf") || streq(os, "octopos") || streq(os, "irtss")
	 || strstr(os, "linux") != NULL || streq(os, "unknown")) {
		object_format = OBJECT_FORMAT_ELF;
	} else if (strstr(os, "bsd") != NULL) {
		object_format = OBJECT_FORMAT_ELF;
		ir_target.ia32_struct_in_regs = true;
	} else if (strstart(os, "darwin")) {
		object_format = OBJECT_FORMAT_MACH_O;
		ir_target.ia32_po2_stackalign = 4;
		ir_target.ia32_struct_in_regs = true;
	} else if (strstart(os, "mingw")) {
		object_format = OBJECT_FORMAT_PE_COFF;
		if (pointer_size == 8) {
			ir_target.amd64_x64abi = true;
		} else {
			assert(pointer_size == 4);
		}
	} else if (strstart(os, "midipix")) {
		object_format = OBJECT_FORMAT_PE_COFF;
		if (pointer_size == 8) {
			ir_target.amd64_x64abi = true;
		} else {
			assert(pointer_size == 4);
		}
	} else {
		be_warningf(NULL, "Unknown operating system '%s'", os);
		object_format = OBJECT_FORMAT_ELF;
	}
	ir_target.object_format = object_format;

	/* Construct mode_P */
	unsigned modulo_shift = isa->modulo_shift;
	unsigned ptr_modulo_shift = modulo_shift == 0
	                          ? 0 : MIN(modulo_shift, pointer_size*8);
	ir_mode *const ptr_mode = new_reference_mode("P", pointer_size*8,
	                                             ptr_modulo_shift);
	set_modeP(ptr_mode);

	if (arch != NULL) {
		bool res = be_set_arch(arch);
		if (!res)
			panic("Could not set backend arch");
	}
	ir_target.initialized = true;

	ir_init_platform(machine);

	return true;
}

int ir_init_target(const char *target_triple)
{
	ir_machine_triple_t *target = ir_parse_machine_triple(target_triple);
	if (target == NULL)
		return false;

	int res = ir_init_target_triple(target);
	ir_free_machine_triple(target);
	return res;
}

void ir_init(void)
{
	ir_machine_triple_t *target = ir_get_host_machine_triple();
	ir_init_target_triple(target);
	ir_free_machine_triple(target);
}

void finish_target(void)
{
	ir_finish_platform();
}

int (ir_target_big_endian)(void)
{
	return ir_target_big_endian_();
}

unsigned ir_target_biggest_alignment(void)
{
	assert(ir_target.initialized);
	return 1u << ir_target.isa->po2_biggest_alignment;
}

unsigned ir_target_pointer_size(void)
{
	assert(ir_target.initialized);
	return ir_target.isa->pointer_size;
}

unsigned ir_target_modulo_shift(void)
{
	assert(ir_target.initialized);
	return ir_target.isa->modulo_shift;
}
