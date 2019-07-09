/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_bearch_t.h"

#include "be_t.h"
#include "bemodule.h"
#include "gen_vhdl_new_nodes.h"
#include "gen_vhdl_regalloc_if.h"
#include "irarch.h"
#include "irprog_t.h"
#include "target_t.h"
#include "vhdl_bemain.h"
#include "vhdl_emitter.h"
#include "vhdl_lower.h"
#include "vhdl_transform.h"

/*
static ir_settings_arch_dep_t const vhdl_arch_dep = {
	.replace_muls         = true,
	.replace_divs         = true,
	.replace_mods         = true,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.max_bits_for_mulh    = VHDL_MACHINE_SIZE,
};
*/
static int vhdl_ifconv(ir_node const *const sel, ir_node const *const mux_false,
                       ir_node const *mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return true;
}

void vhdl_init(void)
{
	vhdl_create_opcodes();
	vhdl_register_init();

	ir_target.experimental
		= "the VHDL backend is highly experimental and unfinished";
	ir_target.allow_ifconv       = vhdl_ifconv;
	ir_target.float_int_overflow = ir_overflow_indefinite;
}

void vhdl_finish(void)
{
	vhdl_free_opcodes();
}

static void vhdl_select_instructions(ir_graph *const irg)
{
	be_timer_push(T_CODEGEN);
	vhdl_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");
}

static void vhdl_lower_for_target(ir_graph *irg)
{
	//ir_arch_lower(&vhdl_arch_dep);
	lower_for_vhdl(irg);
	be_after_irp_transform("lower-arch-dep");
}

void vhdl_generate_code(FILE *const output, char const *const cup_name)
{
	vhdl_be_begin(output, cup_name);


	foreach_irp_irg(i, irg) {
		if (!(mtp_special_instruction & get_entity_additional_properties(get_irg_entity(irg)))) {
			continue;
		}

		vhdl_lower_for_target(irg);

		vhdl_select_instructions(irg);

		vhdl_emit_function(irg);
		vhdl_be_step_last(irg);
	}

	vhdl_be_finish();
}

/*
arch_isa_if_t const vhdl_isa_if = {
	.name                  = "vhdl",
	.pointer_size          = 4,
	.modulo_shift          = 32,
	.big_endian            = true,
	.po2_biggest_alignment = 3,
	.pic_supported         = false,
	.n_registers           = N_VHDL_REGISTERS,
	.registers             = vhdl_registers,
	.n_register_classes    = N_VHDL_CLASSES,
	.register_classes      = vhdl_reg_classes,
	.init                  = vhdl_init,
	.finish                = vhdl_finish,
	.generate_code         = vhdl_generate_code,
	.lower_for_target      = vhdl_lower_for_target,
	.register_prefix       = '$',
	.get_op_estimated_cost = vhdl_get_op_estimated_cost,
};
*/
BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_vhdl)
void be_init_arch_vhdl(void)
{
}
