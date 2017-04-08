/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#include "target_t.h"

#include "be_t.h"
#include "iropt_t.h"
#include "irtools.h"
#include "isas.h"
#include "lc_opts.h"
#include "platform_t.h"
#include "util.h"

target_info_t ir_target;

int ir_target_set_triple(ir_machine_triple_t const *machine)
{
	memset(&ir_target, 0, sizeof(ir_target));
	ir_target.allow_ifconv = ir_is_optimizable_mux;

	const char *const cpu          = ir_triple_get_cpu_type(machine);
	const char *const manufacturer = ir_triple_get_manufacturer(machine);
	char          const *arch      = NULL;
	arch_isa_if_t const *isa;
	if (ir_is_cpu_x86_32(cpu)) {
		isa  = &ia32_isa_if;
		arch = cpu;
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

	if (arch != NULL) {
		bool res = be_set_arch(arch);
		if (!res)
			panic("Could not set backend arch");
	}

	ir_platform_set(machine, isa->pointer_size);

	return true;
}

int ir_target_set(const char *target_triple)
{
	ir_machine_triple_t *target = ir_parse_machine_triple(target_triple);
	if (target == NULL)
		return false;

	int res = ir_target_set_triple(target);
	ir_free_machine_triple(target);
	return res;
}

void ir_target_init(void)
{
	/* Construct mode_P */
	arch_isa_if_t const *const isa  = ir_target.isa;
	unsigned const pointer_size     = isa->pointer_size;
	unsigned const modulo_shift     = isa->modulo_shift;
	unsigned const ptr_modulo_shift = modulo_shift == 0
	                                  ? 0 : MAX(modulo_shift, pointer_size*8);
	ir_mode *const ptr_mode = new_reference_mode("P", pointer_size*8,
	                                             ptr_modulo_shift);
	set_modeP(ptr_mode);

	be_initialize();
	ir_target.isa->init();
	ir_target.isa_initialized = true;

	ir_platform_init();
}

void ir_init(void)
{
	ir_init_library();
	ir_machine_triple_t *target = ir_get_host_machine_triple();
	ir_target_set_triple(target);
	ir_free_machine_triple(target);
	ir_target_init();
}

void finish_target(void)
{
	ir_platform_finish();
}

int ir_target_option(const char *arg)
{
	assert(ir_target.isa != NULL && "Did not call ir_target_set yet");

	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	if (streq(arg, "help") || streq(arg, "?")) {
		lc_opt_print_help_for_entry(be_grp, '-', stdout);
		return -1;
	}

	/* backend args may not have an effect anymore after the backend
	 * has been initialized */
	assert(!ir_target.isa_initialized && "Target already initiazed");
	int res = lc_opt_from_single_arg(be_grp, arg);
	if (res)
		return res;

	/* Try passing the option along to the target */
	lc_opt_entry_t *target_grp = lc_opt_get_grp(be_grp, ir_target.isa->name);
	return lc_opt_from_single_arg(target_grp, arg);
}

int (ir_target_big_endian)(void)
{
	return ir_target_big_endian_();
}

unsigned ir_target_biggest_alignment(void)
{
	return 1u << ir_target.isa->po2_biggest_alignment;
}

unsigned ir_target_pointer_size(void)
{
	return ir_target.isa->pointer_size;
}

int ir_target_fast_unaligned_memaccess(void)
{
	assert(ir_target.isa_initialized);
	return ir_target.fast_unaligned_memaccess;
}

int ir_target_supports_pic(void)
{
	return ir_target.isa->pic_supported;
}

char const *ir_target_experimental(void)
{
	assert(ir_target.isa_initialized);
	return ir_target.experimental;
}

ir_mode *ir_target_float_arithmetic_mode(void)
{
	assert(ir_target.isa_initialized);
	return ir_target.mode_float_arithmetic;
}

float_int_conversion_overflow_style_t ir_target_float_int_overflow_style(void)
{
	assert(ir_target.isa_initialized);
	return ir_target.float_int_overflow;
}
