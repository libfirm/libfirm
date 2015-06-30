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
#include "bestack.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "irprog_t.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "panic.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Transforms the standard firm graph into a TEMLPATE firm graph
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

static void TEMPLATE_generate_code(FILE *output, const char *cup_name)
{
	be_begin(output, cup_name);
	unsigned *const sp_is_non_ssa = rbitset_malloc(N_TEMPLATE_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_SP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
		TEMPLATE_select_instructions(irg);

		be_step_schedule(irg);

		be_step_regalloc(irg, &TEMPLATE_regalloc_if);

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
}

static void TEMPLATE_finish(void)
{
	TEMPLATE_free_opcodes();
}

static void TEMPLATE_lower_for_target(void)
{
	lower_builtins(0, NULL);
	be_after_irp_transform("lower-builtins");

	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN);
	be_after_irp_transform("lower-calls");
}

static int TEMPLATE_is_mux_allowed(ir_node *sel, ir_node *mux_false,
                                   ir_node *mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return false;
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *TEMPLATE_get_backend_params(void)
{
	static backend_params p = {
		.byte_order_big_endian         = false,
		.pic_supported                 = false,
		.unaligned_memaccess_supported = false,
		.modulo_shift                  = 32,
		.dep_param                     = NULL,
		.allow_ifconv                  = TEMPLATE_is_mux_allowed,
		.machine_size                  = 32,
		.mode_float_arithmetic         = NULL,
		.type_long_long                = NULL,
		.type_unsigned_long_long       = NULL,
		.type_long_double              = NULL,
		.stack_param_align             = 4,
		.float_int_overflow            = ir_overflow_min_max,
	};
	return &p;
}

static int TEMPLATE_is_valid_clobber(const char *clobber)
{
	(void)clobber;
	return false;
}

static unsigned TEMPLATE_get_op_estimated_cost(const ir_node *node)
{
	if (is_TEMPLATE_Load(node))
		return 5;
	if (is_TEMPLATE_Store(node))
		return 7;
	return 1;
}

static arch_isa_if_t const TEMPLATE_isa_if = {
	.n_registers           = N_TEMPLATE_REGISTERS,
	.registers             = TEMPLATE_registers,
	.n_register_classes    = N_TEMPLATE_CLASSES,
	.register_classes      = TEMPLATE_reg_classes,
	.init                  = TEMPLATE_init,
	.finish                = TEMPLATE_finish,
	.get_params            = TEMPLATE_get_backend_params,
	.generate_code         = TEMPLATE_generate_code,
	.lower_for_target      = TEMPLATE_lower_for_target,
	.is_valid_clobber      = TEMPLATE_is_valid_clobber,
	.get_op_estimated_cost = TEMPLATE_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_TEMPLATE)
void be_init_arch_TEMPLATE(void)
{
	be_register_isa_if("TEMPLATE", &TEMPLATE_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.TEMPLATE.cg");
	TEMPLATE_init_transform();
}
