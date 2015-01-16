/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   The main arm backend driver file.
 * @author  Matthias Braun, Oliver Richter, Tobias Gneist
 */
#include "arm_emitter.h"
#include "arm_new_nodes.h"
#include "arm_transform.h"
#include "be_t.h"
#include "bearch_arm_t.h"
#include "beflags.h"
#include "begnuas.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "betranshlp.h"
#include "gen_arm_regalloc_if.h"
#include "irarch_t.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irprog_t.h"
#include "irtools.h"
#include "lc_opts_enum.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_softfloat.h"
#include "lowering.h"
#include "util.h"

#define ARM_MODULO_SHIFT 256
#define ARM_MACHINE_SIZE 32

arm_codegen_config_t arm_cg_config;

ir_mode *arm_mode_gp;
ir_mode *arm_mode_flags;

/**
 * Transforms the standard Firm graph into an ARM firm graph.
 */
static void arm_prepare_graph(ir_graph *irg)
{
	/* transform nodes into assembler instructions */
	be_timer_push(T_CODEGEN);
	arm_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");

	/* do local optimizations (mainly CSE) */
	local_optimize_graph(irg);

	/* do code placement, to optimize the position of constants */
	place_code(irg);
}

static ir_node *arm_new_reload(ir_node *value, ir_node *spill, ir_node *before)
{
	ir_node  *block  = get_block(before);
	ir_graph *irg    = get_irn_irg(before);
	ir_node  *frame  = get_irg_frame(irg);
	ir_mode  *mode   = get_irn_mode(value);
	ir_node  *load   = new_bd_arm_Ldr(NULL, block, frame, spill, mode, NULL,
	                                  false, 0, true);
	ir_node  *proj   = new_r_Proj(load, mode, pn_arm_Ldr_res);
	arch_add_irn_flags(load, arch_irn_flag_reload);
	sched_add_before(before, load);
	return proj;
}

static ir_node *arm_new_spill(ir_node *value, ir_node *after)
{
	ir_node  *block  = get_block(after);
	ir_graph *irg    = get_irn_irg(after);
	ir_node  *frame  = get_irg_frame(irg);
	ir_node  *mem    = get_irg_no_mem(irg);
	ir_mode  *mode   = get_irn_mode(value);
	ir_node  *store  = new_bd_arm_Str(NULL, block, frame, value, mem, mode,
	                                  NULL, false, 0, true);
	arch_add_irn_flags(store, arch_irn_flag_spill);
	sched_add_after(after, store);
	return store;
}

static void arm_emit(ir_graph *irg)
{
	arm_finish_graph(irg);
	arm_emit_function(irg);
}

static void arm_before_ra(ir_graph *irg)
{
	be_sched_fix_flags(irg, &arm_reg_classes[CLASS_arm_flags], NULL, NULL, NULL);
}

static ir_entity *divsi3;
static ir_entity *udivsi3;
static ir_entity *modsi3;
static ir_entity *umodsi3;

static void handle_intrinsic(ir_node *node, void *data)
{
	(void)data;
	if (is_Div(node)) {
		ir_mode *mode = get_Div_resmode(node);
		if (get_mode_arithmetic(mode) == irma_twos_complement) {
			ir_entity *entity = mode_is_signed(mode) ? divsi3 : udivsi3;
			be_map_exc_node_to_runtime_call(node, mode, entity, pn_Div_M,
			                                pn_Div_X_regular, pn_Div_X_except,
			                                pn_Div_res);
		}
	} else if (is_Mod(node)) {
		ir_mode *mode = get_Mod_resmode(node);
		assert(get_mode_arithmetic(mode) == irma_twos_complement);
		ir_entity *entity = mode_is_signed(mode) ? modsi3 : umodsi3;
		be_map_exc_node_to_runtime_call(node, mode, entity, pn_Mod_M,
		                                pn_Mod_X_regular, pn_Mod_X_except,
		                                pn_Mod_res);
	}
}

static void arm_create_runtime_entities(void)
{
	if (divsi3 != NULL)
		return;

	ir_mode *mode_int = new_int_mode("arm_be_int", irma_twos_complement,
	                                 ARM_MACHINE_SIZE, true, ARM_MODULO_SHIFT);
	ir_mode *mode_uint = new_int_mode("arm_be_int", irma_twos_complement,
	                                  ARM_MACHINE_SIZE, false,
	                                  ARM_MODULO_SHIFT);

	ir_type *int_tp  = get_type_for_mode(mode_int);
	ir_type *uint_tp = get_type_for_mode(mode_uint);

	ir_type *tp_divsi3 = new_type_method(2, 1);
	set_method_param_type(tp_divsi3, 0, int_tp);
	set_method_param_type(tp_divsi3, 1, int_tp);
	set_method_res_type(tp_divsi3, 0, int_tp);
	divsi3 = create_compilerlib_entity(new_id_from_str("__divsi3"), tp_divsi3);

	ir_type *tp_udivsi3 = new_type_method(2, 1);
	set_method_param_type(tp_udivsi3, 0, uint_tp);
	set_method_param_type(tp_udivsi3, 1, uint_tp);
	set_method_res_type(tp_udivsi3, 0, uint_tp);
	udivsi3 = create_compilerlib_entity(new_id_from_str("__udivsi3"), tp_udivsi3);

	ir_type *tp_modsi3 = new_type_method(2, 1);
	set_method_param_type(tp_modsi3, 0, int_tp);
	set_method_param_type(tp_modsi3, 1, int_tp);
	set_method_res_type(tp_modsi3, 0, int_tp);
	modsi3 = create_compilerlib_entity(new_id_from_str("__modsi3"), tp_modsi3);

	ir_type *tp_umodsi3 = new_type_method(2, 1);
	set_method_param_type(tp_umodsi3, 0, uint_tp);
	set_method_param_type(tp_umodsi3, 1, uint_tp);
	set_method_res_type(tp_umodsi3, 0, uint_tp);
	umodsi3 = create_compilerlib_entity(new_id_from_str("__umodsi3"), tp_umodsi3);
}

/**
 * Maps all intrinsic calls that the backend support
 * and map all instructions the backend did not support
 * to runtime calls.
 */
static void arm_handle_intrinsics(ir_graph *irg)
{
	arm_create_runtime_entities();
	irg_walk_graph(irg, handle_intrinsic, NULL, NULL);
}

static arm_isa_t arm_isa_template = {
	.base = {
		.n_registers        = N_ARM_REGISTERS,
		.registers          = arm_registers,
		.n_register_classes = N_ARM_CLASSES,
		.register_classes   = arm_reg_classes,
		.spill_cost         = 7,
		.reload_cost        = 5,
	},
};

static arch_env_t *arm_begin_codegeneration(void)
{
	arm_isa_t *isa = XMALLOC(arm_isa_t);
	*isa = arm_isa_template;

	be_gas_emit_types = false;
	be_gas_elf_type_char = '%';

	arm_emit_file_prologue();

	return &isa->base;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void arm_end_codegeneration(void *self)
{
	free(self);
}

/**
 * Allows or disallows the creation of Mux nodes for the given Phi nodes.
 * @return 1 if allowed, 0 otherwise
 */
static int arm_is_mux_allowed(ir_node *sel, ir_node *mux_false,
                              ir_node *mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return false;
}

static int arm_is_valid_clobber(const char *clobber)
{
	(void)clobber;
	return false;
}

static void arm_lower_for_target(void)
{
	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN);
	be_after_irp_transform("lower-calls");

	foreach_irp_irg(i, irg) {
		/* Turn all small CopyBs into loads/stores and all bigger CopyBs into
		 * memcpy calls. */
		lower_CopyB(irg, 31, 32, false);
		be_after_transform(irg, "lower-copyb");
	}
	if (arm_cg_config.fpu == ARM_FPU_SOFTFLOAT) {
		lower_floating_point();
		be_after_irp_transform("lower-fp");
	}

	ir_builtin_kind supported[1];
	size_t s = 0;
	supported[s++] = ir_bk_clz;
	assert(s <= ARRAY_SIZE(supported));
	lower_builtins(s, supported);
	be_after_irp_transform("lower-builtins");

	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, arm_mode_gp);
		be_after_transform(irg, "lower-switch");
	}

	arm_lower_64bit();
	be_after_irp_transform("lower-64");
}

static const ir_settings_arch_dep_t arm_arch_dep = {
	.also_use_subs        = true,
	.maximum_shifts       = 1,
	.highest_shift_amount = 31,
	.evaluate             = NULL,
	.allow_mulhs          = false,
	.allow_mulhu          = false,
	.max_bits_for_mulh    = ARM_MACHINE_SIZE,
};
static backend_params arm_backend_params = {
	.byte_order_big_endian         = false,
	.pic_supported                 = false,
	.unaligned_memaccess_supported = false,
	.modulo_shift                  = ARM_MODULO_SHIFT,
	.dep_param                     = &arm_arch_dep,
	.allow_ifconv                  = arm_is_mux_allowed,
	.machine_size                  = ARM_MACHINE_SIZE,
	.mode_float_arithmetic         = NULL,
	.type_long_long                = NULL,
	.type_unsigned_long_long       = NULL,
	.type_long_double              = NULL,
	.stack_param_align             = 4,
	.float_int_overflow            = ir_overflow_min_max,
};

static void arm_init_backend_params(void)
{
	arm_backend_params.byte_order_big_endian = arm_cg_config.big_endian;
}

static const backend_params *arm_get_libfirm_params(void)
{
	return &arm_backend_params;
}

static void arm_finish(void)
{
	arm_free_opcodes();
}

static void arm_init(void)
{
	arm_mode_gp    = new_int_mode("arm_gp", irma_twos_complement,
	                              ARM_MACHINE_SIZE, 0, ARM_MODULO_SHIFT);
	arm_mode_flags = new_non_arithmetic_mode("arm_flags", 32);

	arm_register_init();
	arm_create_opcodes(&be_null_ops);
	arm_init_backend_params();
}

static arch_isa_if_t const arm_isa_if = {
	.init                 = arm_init,
	.finish               = arm_finish,
	.get_params           = arm_get_libfirm_params,
	.lower_for_target     = arm_lower_for_target,
	.is_valid_clobber     = arm_is_valid_clobber,
	.begin_codegeneration = arm_begin_codegeneration,
	.end_codegeneration   = arm_end_codegeneration,
	.new_spill            = arm_new_spill,
	.new_reload           = arm_new_reload,
	.handle_intrinsics    = arm_handle_intrinsics,
	.prepare_graph        = arm_prepare_graph,
	.before_ra            = arm_before_ra,
	.emit                 = arm_emit,
};

static const lc_opt_enum_int_items_t arm_fpu_items[] = {
	{ "softfloat", ARM_FPU_SOFTFLOAT },
	{ "fpa",       ARM_FPU_FPA       },
	{ NULL,        0                 },
};
static lc_opt_enum_int_var_t arch_fpu_var = {
	(int*)&arm_cg_config.fpu, arm_fpu_items
};

static const lc_opt_enum_int_items_t arm_arch_items[] = {
	{ "armv4",   ARM_VARIANT_4   },
	{ "armv5t",  ARM_VARIANT_5T  },
	{ "armv6",   ARM_VARIANT_6   },
	{ "armv6t2", ARM_VARIANT_6T2 },
	{ "armv7",   ARM_VARIANT_7   },
	{ NULL,      0               },
};
static lc_opt_enum_int_var_t arch_var = {
	(int*)&arm_cg_config.variant, arm_arch_items
};

static const lc_opt_table_entry_t arm_options[] = {
	LC_OPT_ENT_ENUM_INT("fpu", "select the floating point unit", &arch_fpu_var),
	LC_OPT_ENT_ENUM_INT("arch", "select architecture variant", &arch_var),
	LC_OPT_LAST
};

static void arm_init_architecture(void)
{
	memset(&arm_cg_config, 0, sizeof(arm_cg_config));
	arm_cg_config.variant    = ARM_VARIANT_6T2;
	arm_cg_config.fpu        = ARM_FPU_SOFTFLOAT;
	arm_cg_config.big_endian = false;

	lc_opt_entry_t *be_grp  = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *arm_grp = lc_opt_get_grp(be_grp, "arm");
	lc_opt_add_table(arm_grp, arm_options);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_arm)
void be_init_arch_arm(void)
{
	arm_init_transform();
	arm_init_emitter();
	arm_init_architecture();

	be_register_isa_if("arm", &arm_isa_if);
}
