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
#include "bearch_TEMPLATE_t.h"
#include "bemodule.h"
#include "benode.h"
#include "bestack.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "panic.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Transforms the standard firm graph into
 * a TEMLPATE firm graph
 */
static void TEMPLATE_prepare_graph(ir_graph *irg)
{
	/* transform nodes into assembler instructions */
	be_timer_push(T_CODEGEN);
	TEMPLATE_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");
}



/**
 * Last touchups and emitting of the generated code of a function.
 */
static void TEMPLATE_emit(ir_graph *irg)
{
	/* fix stack entity offsets */
	be_fix_stack_nodes(irg, &TEMPLATE_registers[REG_SP]);

	/* emit code */
	TEMPLATE_emit_function(irg);
}


static void TEMPLATE_before_ra(ir_graph *irg)
{
	(void)irg;
}

static void TEMPLATE_init(void)
{
	TEMPLATE_register_init();
	TEMPLATE_create_opcodes(&be_null_ops);
}

static void TEMPLATE_finish(void)
{
	TEMPLATE_free_opcodes();
}

static arch_env_t *TEMPLATE_begin_codegeneration(void)
{
	TEMPLATE_isa_t *isa = XMALLOC(TEMPLATE_isa_t);
	return &isa->base;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void TEMPLATE_end_codegeneration(void *self)
{
	free(self);
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

static arch_isa_if_t const TEMPLATE_isa_if = {
	.n_registers          = N_TEMPLATE_REGISTERS,
	.registers            = TEMPLATE_registers,
	.n_register_classes   = N_TEMPLATE_CLASSES,
	.register_classes     = TEMPLATE_reg_classes,
	.spill_cost           = 7,
	.reload_cost          = 5,
	.init                 = TEMPLATE_init,
	.finish               = TEMPLATE_finish,
	.get_params           = TEMPLATE_get_backend_params,
	.lower_for_target     = TEMPLATE_lower_for_target,
	.is_valid_clobber     = TEMPLATE_is_valid_clobber,
	.begin_codegeneration = TEMPLATE_begin_codegeneration,
	.end_codegeneration   = TEMPLATE_end_codegeneration,
	.new_spill            = TEMPLATE_new_spill,
	.new_reload           = TEMPLATE_new_reload,
	.prepare_graph        = TEMPLATE_prepare_graph,
	.before_ra            = TEMPLATE_before_ra,
	.emit                 = TEMPLATE_emit,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_TEMPLATE)
void be_init_arch_TEMPLATE(void)
{
	be_register_isa_if("TEMPLATE", &TEMPLATE_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.TEMPLATE.cg");
	TEMPLATE_init_transform();
}
