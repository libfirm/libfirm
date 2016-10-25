/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "be_t.h"
#include "bemodule.h"
#include "bera.h"
#include "gen_mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"
#include "irarch_t.h"
#include "irprog_t.h"
#include "lower_dw.h"
#include "mips_bearch_t.h"
#include "mips_emitter.h"
#include "mips_transform.h"

static int mips_is_mux_allowed(ir_node *const sel, ir_node *const mux_false, ir_node *const mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return false;
}

static ir_settings_arch_dep_t const mips_arch_dep = {
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = MIPS_MACHINE_SIZE - 1,
	.evaluate             = NULL,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.max_bits_for_mulh    = MIPS_MACHINE_SIZE,
};

static backend_params mips_backend_params = {
	.experimental                  = "the MIPS backend is highly experimental and unfinished",
	.byte_order_big_endian         = false,
	.pic_supported                 = false,
	.unaligned_memaccess_supported = false,
	.modulo_shift                  = MIPS_MACHINE_SIZE,
	.dep_param                     = &mips_arch_dep,
	.allow_ifconv                  = &mips_is_mux_allowed,
	.machine_size                  = MIPS_MACHINE_SIZE,
	.mode_float_arithmetic         = NULL,  /* will be set later */ // TODO
	.type_long_double              = NULL,  /* will be set later */ // TODO
	.stack_param_align             = 4,
	.float_int_overflow            = ir_overflow_indefinite,
};

static void mips_init(void)
{
	ir_mode *const ptr_mode = new_reference_mode("p32", MIPS_MACHINE_SIZE, MIPS_MACHINE_SIZE);
	set_modeP(ptr_mode);

	mips_create_opcodes();
	mips_register_init();
}

static void mips_finish(void)
{
	mips_free_opcodes();
}

static const backend_params *mips_get_libfirm_params(void)
{
	return &mips_backend_params;
}

static void mips_select_instructions(ir_graph *const irg)
{
	be_timer_push(T_CODEGEN);
	mips_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");

	place_code(irg);
	be_dump(DUMP_BE, irg, "place");
}

static regalloc_if_t const mips_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = NULL, // TODO
	.new_reload  = NULL, // TODO
};

static void mips_generate_code(FILE *const output, char const *const cup_name)
{
	be_begin(output, cup_name);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;
		// TODO be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
		mips_select_instructions(irg);
		be_step_schedule(irg);
		be_step_regalloc(irg, &mips_regalloc_if);
		mips_emit_function(irg);
		be_step_last(irg);
	}

	be_finish();
}

static void mips_lower64(void)
{
	ir_mode *const word_unsigned = mips_reg_classes[CLASS_mips_gp].mode;
	ir_mode *const word_signed   = find_signed_mode(word_unsigned);
	lwrdw_param_t lower_dw_params = {
		.create_intrinsic = NULL, // TODO
		.word_unsigned    = word_unsigned,
		.word_signed      = word_signed,
		.doubleword_size  = 64,
		.big_endian       = be_is_big_endian(),
	};

	ir_prepare_dw_lowering(&lower_dw_params);
	ir_lower_dw_ops();
}

static void mips_lower_for_target(void)
{
	mips_lower64();
}

static unsigned mips_get_op_estimated_cost(ir_node const *const node)
{
	(void)node; // TODO
	return 1;
}

static arch_isa_if_t const mips_isa_if = {
	.n_registers           = N_MIPS_REGISTERS,
	.registers             = mips_registers,
	.n_register_classes    = N_MIPS_CLASSES,
	.register_classes      = mips_reg_classes,
	.init                  = mips_init,
	.finish                = mips_finish,
	.get_params            = mips_get_libfirm_params,
	.generate_code         = mips_generate_code,
	.lower_for_target      = mips_lower_for_target,
	.is_valid_clobber      = NULL, // TODO
	.get_op_estimated_cost = mips_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_mips)
void be_init_arch_mips(void)
{
	be_register_isa_if("mips", &mips_isa_if);
}
