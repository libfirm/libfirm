/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main TEMPLATE backend driver file.
 */
#include "TEMPLATE_emitter.h"
#include "TEMPLATE_new_nodes.h"
#include "TEMPLATE_transform.h"
#include "be_t.h"
#include "beirg.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "bestack.h"
#include "debug.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "iredges_t.h"
#include "irprog_t.h"
#include "isas.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "panic.h"
#include "target_t.h"

/**
 * Transforms the standard firm graph into a TEMPLATE firm graph
 */
static void TEMPLATE_select_instructions(ir_graph *irg)
{
	/* transform nodes into assembler instructions */
	be_timer_push(T_CODEGEN);
	TEMPLATE_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");
}

static ir_node *TEMPLATE_new_spill(ir_node *value, ir_node *after)
{
	(void)value;
	(void)after;
	panic("spilling not implemented yet");
}

static ir_node *TEMPLATE_new_reload(ir_node *value, ir_node *spill,
                                    ir_node *before)
{
	(void)value;
	(void)spill;
	(void)before;
	panic("reload not implemented yet");
}

static const regalloc_if_t TEMPLATE_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = TEMPLATE_new_spill,
	.new_reload  = TEMPLATE_new_reload,
};

static void introduce_prologue(ir_graph *const irg)
{
	ir_node  *const start      = get_irg_start(irg);
	ir_node  *const block      = get_nodes_block(start);
	ir_node  *const initial_sp = be_get_Start_proj(irg, &TEMPLATE_registers[REG_SP]);
	ir_type  *const frame_type = get_irg_frame_type(irg);
	unsigned  const frame_size = get_type_size(frame_type);
	ir_node  *const incsp      = be_new_IncSP(block, initial_sp, frame_size, false);
	edges_reroute_except(initial_sp, incsp, incsp);
	sched_add_after(start, incsp);
}

static void TEMPLATE_generate_code(FILE *output, const char *cup_name)
{
	be_begin(output, cup_name);
	unsigned *const sp_is_non_ssa = rbitset_alloca(N_TEMPLATE_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_SP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
		TEMPLATE_select_instructions(irg);

		be_step_schedule(irg);

		be_step_regalloc(irg, &TEMPLATE_regalloc_if);

		introduce_prologue(irg);

		be_fix_stack_nodes(irg, &TEMPLATE_registers[REG_SP]);
		be_birg_from_irg(irg)->non_ssa_regs = NULL;

		TEMPLATE_emit_function(irg);

		be_step_last(irg);
	}

	be_finish();
}

static void TEMPLATE_init(void)
{
	TEMPLATE_register_init();
	TEMPLATE_create_opcodes();

	ir_target.experimental
		= "The TEMPLATE backend is just a demo for writing backends";
	ir_target.float_int_overflow = ir_overflow_min_max;
}

static void TEMPLATE_finish(void)
{
	TEMPLATE_free_opcodes();
}

static void TEMPLATE_lower_for_target(void)
{
	lower_builtins(0, NULL, NULL);
	be_after_irp_transform("lower-builtins");

	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN,
				   lower_aggregates_as_pointers, NULL,
				   lower_aggregates_as_pointers, NULL,
				   reset_stateless_abi);
	be_after_irp_transform("lower-calls");
}

static unsigned TEMPLATE_get_op_estimated_cost(const ir_node *node)
{
	if (is_TEMPLATE_Load(node))
		return 5;
	if (is_TEMPLATE_Store(node))
		return 7;
	return 1;
}

arch_isa_if_t const TEMPLATE_isa_if = {
	.name                  = "TEMPLATE",
	.pointer_size          = 4,
	.modulo_shift          = 32,
	.big_endian            = false,
	.po2_biggest_alignment = 3,
	.pic_supported         = false,
	.n_registers           = N_TEMPLATE_REGISTERS,
	.registers             = TEMPLATE_registers,
	.n_register_classes    = N_TEMPLATE_CLASSES,
	.register_classes      = TEMPLATE_reg_classes,
	.init                  = TEMPLATE_init,
	.finish                = TEMPLATE_finish,
	.generate_code         = TEMPLATE_generate_code,
	.lower_for_target      = TEMPLATE_lower_for_target,
	.get_op_estimated_cost = TEMPLATE_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_TEMPLATE)
void be_init_arch_TEMPLATE(void)
{
	TEMPLATE_init_transform();
}
