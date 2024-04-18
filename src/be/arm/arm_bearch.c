/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   The main arm backend driver file.
 * @author  Matthias Braun, Oliver Richter, Tobias Gneist
 */
#include "arm_bearch_t.h"

#include "arm_emitter.h"
#include "arm_new_nodes.h"
#include "arm_transform.h"
#include "be_t.h"
#include "beflags.h"
#include "begnuas.h"
#include "beirg.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "betranshlp.h"
#include "gen_arm_regalloc_if.h"
#include "irarch.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irprog_t.h"
#include "irtools.h"
#include "isas.h"
#include "lc_opts_enum.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_softfloat.h"
#include "lowering.h"
#include "target_t.h"
#include "util.h"

#define ARM_MODULO_SHIFT 256
#define ARM_MACHINE_SIZE 32

arm_codegen_config_t arm_cg_config;

ir_mode *arm_mode_gp;
ir_mode *arm_mode_flags;

/**
 * Transforms the standard Firm graph into an ARM firm graph.
 */
static void arm_select_instructions(ir_graph *irg)
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
	ir_node  *proj   = be_new_Proj(load, pn_arm_Ldr_res);
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

static ir_type *make_divmod_type(ir_type *const tp)
{
	ir_type *const mtp = new_type_method(2, 1, false, cc_cdecl_set, mtp_no_property);
	set_method_param_type(mtp, 0, tp);
	set_method_param_type(mtp, 1, tp);
	set_method_res_type(mtp, 0, tp);
	return mtp;
}

static void arm_create_runtime_entities(void)
{
	if (divsi3 != NULL)
		return;

	ir_mode *mode_int = new_int_mode("arm_be_int", ARM_MACHINE_SIZE, true,
	                                 ARM_MODULO_SHIFT);
	ir_mode *mode_uint = new_int_mode("arm_be_int", ARM_MACHINE_SIZE, false,
	                                  ARM_MODULO_SHIFT);

	ir_type *int_tp  = get_type_for_mode(mode_int);
	ir_type *uint_tp = get_type_for_mode(mode_uint);

	ir_type *const mtps = make_divmod_type(int_tp);
	divsi3 = create_compilerlib_entity("__divsi3", mtps);
	modsi3 = create_compilerlib_entity("__modsi3", mtps);

	ir_type *const mtpu = make_divmod_type(uint_tp);
	udivsi3 = create_compilerlib_entity("__udivsi3", mtpu);
	umodsi3 = create_compilerlib_entity("__umodsi3", mtpu);
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

static const regalloc_if_t arm_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = arm_new_spill,
	.new_reload  = arm_new_reload,
};

static void arm_generate_code(FILE *output, const char *cup_name)
{
	be_gas_emit_types = false;
	be_gas_elf_type_char = '%';

	be_begin(output, cup_name);
	unsigned *const sp_is_non_ssa = rbitset_alloca(N_ARM_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_SP);

	arm_emit_file_prologue();

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		struct obstack *obst = be_get_be_obst(irg);
		be_birg_from_irg(irg)->isa_link = OALLOCZ(obst, arm_irg_data_t);

		be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
		arm_select_instructions(irg);

		be_step_schedule(irg);

		be_timer_push(T_RA_PREPARATION);
		be_sched_fix_flags(irg, &arm_reg_classes[CLASS_arm_flags], NULL, NULL, NULL);
		be_timer_pop(T_RA_PREPARATION);

		be_step_regalloc(irg, &arm_regalloc_if);

		be_timer_push(T_EMIT);
		arm_finish_graph(irg);
		arm_emit_function(irg);
		be_timer_pop(T_EMIT);

		be_step_last(irg);
	}

	be_finish();
}

static const ir_settings_arch_dep_t arm_arch_dep = {
	.replace_muls         = true,
	.replace_divs         = true,
	.replace_mods         = true,
	.allow_mulhs          = false,
	.allow_mulhu          = false,
	.also_use_subs        = true,
	.maximum_shifts       = 1,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.max_bits_for_mulh    = ARM_MACHINE_SIZE,
};

static void arm_lower_for_target(void)
{
	ir_arch_lower(&arm_arch_dep);
	be_after_irp_transform("lower-arch-dep");

	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN,
				   lower_aggregates_as_pointers, NULL,
				   lower_aggregates_as_pointers, NULL,
				   reset_stateless_abi);
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
	lower_builtins(s, supported, NULL);
	be_after_irp_transform("lower-builtins");

	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, arm_mode_gp);
		be_after_transform(irg, "lower-switch");
	}

	arm_lower_64bit();
	be_after_irp_transform("lower-64");
}

static void arm_init_asm_constraints(void)
{
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP,     "Qm");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER,  "lr");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_ANY,       "g");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE, "IJKLMin");
}

static int arm_ifconv(ir_node const *const sel, ir_node const *const mux_false,
                      ir_node const *mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return false;
}

static void arm_init(void)
{
	arm_mode_gp    = new_int_mode("arm_gp", ARM_MACHINE_SIZE, 0,
	                              ARM_MODULO_SHIFT);
	arm_mode_flags = new_non_arithmetic_mode("arm_flags", 32);

	arm_init_asm_constraints();
	arm_register_init();
	arm_create_opcodes();

	ir_target.experimental
		= "the arm backend is highly experimental and unfinished";
	ir_target.fast_unaligned_memaccess = false;
	ir_target.allow_ifconv             = arm_ifconv;
	ir_target.float_int_overflow       = ir_overflow_min_max;
}

static void arm_finish(void)
{
	arm_free_opcodes();
}

static unsigned arm_get_op_estimated_cost(const ir_node *node)
{
	(void)node; /* TODO */
	return 1;
}

arch_isa_if_t const arm_isa_if = {
	.name                  = "arm",
	.pointer_size          = 4,
	.big_endian            = false,
	.modulo_shift          = ARM_MODULO_SHIFT,
	.po2_biggest_alignment = 3,
	.pic_supported         = false,
	.n_registers           = N_ARM_REGISTERS,
	.registers             = arm_registers,
	.n_register_classes    = N_ARM_CLASSES,
	.register_classes      = arm_reg_classes,
	.init                  = arm_init,
	.finish                = arm_finish,
	.generate_code         = arm_generate_code,
	.lower_for_target      = arm_lower_for_target,
	.handle_intrinsics     = arm_handle_intrinsics,
	.get_op_estimated_cost = arm_get_op_estimated_cost,
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
}
