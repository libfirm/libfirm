/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main sparc backend driver file.
 * @author   Hannes Rapp, Matthias Braun
 */
#include "sparc_bearch_t.h"

#include "be_t.h"
#include "beflags.h"
#include "begnuas.h"
#include "beirg.h"
#include "bemodule.h"
#include "bera.h"
#include "besched.h"
#include "betranshlp.h"
#include "bevarargs.h"
#include "gen_sparc_regalloc_if.h"
#include "irarch.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irprog_t.h"
#include "irtools.h"
#include "isas.h"
#include "lc_opts_enum.h"
#include "lower_alloc.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_mode_b.h"
#include "lower_softfloat.h"
#include "lowering.h"
#include "platform_t.h"
#include "sparc_cconv.h"
#include "sparc_emitter.h"
#include "sparc_new_nodes.h"
#include "sparc_transform.h"
#include "target_t.h"
#include "util.h"

pmap *sparc_constants;

ir_mode *sparc_mode_Q;

typedef enum {
	cpu_generic,
	cpu_v8plus,
	cpu_leon,
	cpu_supersparc,
	cpu_hypersparc,
} sparc_cpu_t;
static const lc_opt_enum_int_items_t cpu_items[] = {
	{ "generic",    cpu_generic    },
	{ "v8",         cpu_generic    },
	{ "v8plus",     cpu_v8plus     },
	{ "leon",       cpu_leon       },
	{ "leon3",      cpu_leon       },
	{ "supersparc", cpu_supersparc },
	{ "hypersparc", cpu_hypersparc },
	{ NULL,         0               },
};

static int cpu;
static lc_opt_enum_int_var_t cpu_var = {
	&cpu, cpu_items
};

sparc_codegen_config_t sparc_cg_config;

typedef enum {
	use_fpu_auto,
	use_fpu_yes,
	use_fpu_no
} sparc_use_fpu_t;
/* fpu set architectures. */
static const lc_opt_enum_int_items_t fpu_items[] = {
	{ "auto",      use_fpu_auto },
	{ "fpu",       use_fpu_yes  },
	{ "softfloat", use_fpu_no   },
	{ NULL,        0 }
};

static int fpu;
static lc_opt_enum_int_var_t arch_fpu_var = {
	&fpu, fpu_items
};

static bool use_softfloat;

static const lc_opt_table_entry_t sparc_options[] = {
	LC_OPT_ENT_ENUM_INT("fpunit",     "select the floating point unit", &arch_fpu_var),
	LC_OPT_ENT_ENUM_INT("cpu",        "select architecture variant",    &cpu_var),
	LC_OPT_ENT_BOOL    ("soft-float", "equivalent to fpmath=softfloat", &use_softfloat),
	LC_OPT_LAST
};

/**
 * Transforms the standard firm graph into a SPARC firm graph
 */
static void sparc_select_instructions(ir_graph *irg)
{
	be_timer_push(T_CODEGEN);
	sparc_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");
}

static bool sparc_modifies_flags(const ir_node *node)
{
	be_foreach_out(node, o) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(node, o);
		if (req->cls == &sparc_reg_classes[CLASS_sparc_flags])
			return true;
	}
	return false;
}

static bool sparc_modifies_fp_flags(const ir_node *node)
{
	be_foreach_out(node, o) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(node, o);
		if (req->cls == &sparc_reg_classes[CLASS_sparc_fpflags])
			return true;
	}
	return false;
}

/**
 * rewrite unsigned->float conversion.
 * Sparc has no instruction for this so instead we do the following:
 *
 *   int    signed_x = unsigned_value_x;
 *   double res      = signed_x;
 *   if (signed_x < 0)
 *       res += 4294967296. ;
 *   return (float) res;
 */
static void rewrite_unsigned_float_Conv(ir_node *node)
{
	ir_graph *irg         = get_irn_irg(node);
	dbg_info *dbgi        = get_irn_dbg_info(node);
	ir_node  *lower_block = get_nodes_block(node);

	part_block(node);

	ir_node   *block       = get_nodes_block(node);
	ir_node   *unsigned_x  = get_Conv_op(node);
	ir_mode   *mode_u      = get_irn_mode(unsigned_x);
	ir_mode   *mode_s      = find_signed_mode(mode_u);
	ir_mode   *mode_d      = mode_D;
	ir_node   *signed_x    = new_rd_Conv(dbgi, block, unsigned_x, mode_s);
	ir_node   *res         = new_rd_Conv(dbgi, block, signed_x, mode_d);
	ir_node   *zero        = new_r_Const_null(irg, mode_s);
	collect_new_start_block_node(zero);
	ir_node   *cmp         = new_rd_Cmp(dbgi, block, signed_x, zero,
	                                    ir_relation_less);
	ir_node   *cond        = new_rd_Cond(dbgi, block, cmp);
	ir_node   *proj_true   = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node   *proj_false  = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node   *in_true[1]  = { proj_true };
	ir_node   *in_false[1] = { proj_false };
	ir_node   *true_block  = new_r_Block(irg, ARRAY_SIZE(in_true), in_true);
	ir_node   *false_block = new_r_Block(irg, ARRAY_SIZE(in_false),in_false);
	ir_node   *true_jmp    = new_r_Jmp(true_block);
	ir_node   *false_jmp   = new_r_Jmp(false_block);
	ir_tarval *correction  = new_tarval_from_double(4294967296., mode_d);
	ir_node   *c_const     = new_r_Const(irg, correction);
	collect_new_start_block_node(c_const);
	ir_node   *fadd        = new_rd_Add(dbgi, true_block, res, c_const);

	ir_node  *lower_in[2] = { true_jmp, false_jmp };
	ir_node  *phi_in[2]   = { fadd, res };
	ir_mode  *dest_mode   = get_irn_mode(node);

	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi = new_r_Phi(lower_block, ARRAY_SIZE(phi_in), phi_in, mode_d);
	collect_new_phi_node(phi);

	ir_node *res_conv = new_rd_Conv(dbgi, lower_block, phi, dest_mode);
	exchange(node, res_conv);
}

/**
 * rewrite float->unsigned conversions.
 * Sparc has no instruction for this so instead we do the following:
 *
 * if (x >= 2147483648.) {
 *   converted ^= (int)(x-2147483648.) ^ 0x80000000;
 * } else {
 *   converted = (int)x;
 * }
 * return (unsigned)converted;
 */
static void rewrite_float_unsigned_Conv(ir_node *node)
{
	ir_graph *irg         = get_irn_irg(node);
	dbg_info *dbgi        = get_irn_dbg_info(node);
	ir_node  *lower_block = get_nodes_block(node);

	part_block(node);

	ir_node   *block       = get_nodes_block(node);
	ir_node   *float_x     = get_Conv_op(node);
	ir_mode   *mode_u      = get_irn_mode(node);
	ir_mode   *mode_s      = find_signed_mode(mode_u);
	ir_mode   *mode_f      = get_irn_mode(float_x);
	ir_tarval *limit       = new_tarval_from_double(2147483648., mode_f);
	ir_node   *limitc      = new_r_Const(irg, limit);
	collect_new_start_block_node(limitc);
	ir_node   *cmp         = new_rd_Cmp(dbgi, block, float_x, limitc,
	                                    ir_relation_greater_equal);
	ir_node   *cond        = new_rd_Cond(dbgi, block, cmp);
	ir_node   *proj_true   = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node   *proj_false  = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node   *in_true[1]  = { proj_true };
	ir_node   *in_false[1] = { proj_false };
	ir_node   *true_block  = new_r_Block(irg, ARRAY_SIZE(in_true), in_true);
	ir_node   *false_block = new_r_Block(irg, ARRAY_SIZE(in_false),in_false);
	ir_node   *true_jmp    = new_r_Jmp(true_block);
	ir_node   *false_jmp   = new_r_Jmp(false_block);

	ir_node   *c_const     = new_r_Const_long(irg, mode_s, 0x80000000L);
	collect_new_start_block_node(c_const);
	ir_node   *sub         = new_rd_Sub(dbgi, true_block, float_x, limitc);
	ir_node   *sub_conv    = new_rd_Conv(dbgi, true_block, sub, mode_s);
	ir_node   *xorn        = new_rd_Eor(dbgi, true_block, sub_conv, c_const);

	ir_node   *converted   = new_rd_Conv(dbgi, false_block, float_x,mode_s);

	ir_node  *lower_in[2] = { true_jmp, false_jmp };
	ir_node  *phi_in[2]   = { xorn, converted };

	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi = new_r_Phi(lower_block, ARRAY_SIZE(phi_in), phi_in, mode_s);
	collect_new_phi_node(phi);

	ir_node *res_conv = new_rd_Conv(dbgi, lower_block, phi, mode_u);
	exchange(node, res_conv);
}

static bool sparc_rewrite_Conv(ir_node *node)
{
	ir_mode *to_mode   = get_irn_mode(node);
	ir_node *op        = get_Conv_op(node);
	ir_mode *from_mode = get_irn_mode(op);

	if (mode_is_float(to_mode) && mode_is_int(from_mode)
	    && get_mode_size_bits(from_mode) == 32
	    && !mode_is_signed(from_mode)) {
		rewrite_unsigned_float_Conv(node);
		return true;
	}
	if (mode_is_float(from_mode) && mode_is_int(to_mode)
	    && get_mode_size_bits(to_mode) <= 32
	    && !mode_is_signed(to_mode)) {
		rewrite_float_unsigned_Conv(node);
		return true;
	}

	return false;
}

static ir_entity *rem;
static ir_entity *urem;

static void handle_intrinsic(ir_node *node, void *data)
{
	bool *changed = (bool*)data;
	if (is_Mod(node)) {
		ir_mode *mode = get_Mod_resmode(node);
		if (get_mode_arithmetic(mode) == irma_twos_complement) {
			ir_entity *entity = mode_is_signed(mode) ? rem : urem;
			be_map_exc_node_to_runtime_call(node, mode, entity, pn_Mod_M,
			                                pn_Mod_X_regular, pn_Mod_X_except,
			                                pn_Mod_res);
			*changed = true;
		}
	} else if (is_Conv(node)) {
		if (sparc_rewrite_Conv(node))
			*changed = true;
	}
}

static ir_type *make_mod_type(ir_type *const tp)
{
	ir_type *const mtp = new_type_method(2, 1, false, cc_cdecl_set, mtp_no_property);
	set_method_param_type(mtp, 0, tp);
	set_method_param_type(mtp, 1, tp);
	set_method_res_type(mtp, 0, tp);
	return mtp;
}

static void sparc_create_runtime_entities(void)
{
	if (rem != NULL)
		return;

	ir_type *const int_tp = get_type_for_mode(mode_Is);
	ir_type *const mod_tp = make_mod_type(int_tp);
	rem = create_compilerlib_entity(".rem", mod_tp);

	ir_type *const uint_tp = get_type_for_mode(mode_Iu);
	ir_type *const umod_tp = make_mod_type(uint_tp);
	urem = create_compilerlib_entity(".urem", umod_tp);
}

static void sparc_handle_intrinsics(ir_graph *irg)
{
	sparc_create_runtime_entities();
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs_and_start_block_nodes(irg);
	bool changed = false;
	irg_walk_graph(irg, handle_intrinsic, NULL, &changed);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	if (changed) {
		confirm_irg_properties(irg,
			IR_GRAPH_PROPERTY_NO_BADS
			| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
			| IR_GRAPH_PROPERTY_MANY_RETURNS
			| IR_GRAPH_PROPERTY_ONE_RETURN);
	}
}

static void sparc_setup_cg_config(void)
{
	memset(&sparc_cg_config, 0, sizeof(sparc_cg_config));
	bool has_fpu = false;
	switch ((sparc_cpu_t)cpu) {
	case cpu_v8plus:
		sparc_cg_config.use_cas = true;
		sparc_cg_config.cas_asi = 0x80;
		has_fpu = false;
		break;
	case cpu_leon:
		sparc_cg_config.use_cas = true;
		/* According to Gaisler's GRIP documentation, section 77.2.7,
		 * the LEON requires the ASI 0x0A for CAS usage in user mode.
		 */
		sparc_cg_config.cas_asi = 0x0A;
		has_fpu = true;
		break;
	case cpu_hypersparc:
		has_fpu = true;
		break;
	case cpu_supersparc:
	case cpu_generic:
		has_fpu = false;
		break;
	}

	if (use_softfloat)
		fpu = use_fpu_no;

	if (fpu == use_fpu_auto) {
		sparc_cg_config.use_fpu = has_fpu;
	} else {
		sparc_cg_config.use_fpu = fpu == use_fpu_yes;
	}
}

static void sparc_init(void)
{
	sparc_init_asm_constraints();
	sparc_register_init();
	sparc_create_opcodes();
	sparc_cconv_init();
	sparc_setup_cg_config();

	sparc_mode_Q
		= new_float_mode("Q", irma_ieee754, 15, 112, ir_overflow_min_max);

	ir_target.float_int_overflow = ir_overflow_min_max;
	ir_platform_set_va_list_type_pointer();
}

static void sparc_finish(void)
{
	sparc_free_opcodes();
}

static ir_node *sparc_new_spill(ir_node *value, ir_node *after)
{
	ir_node  *block = get_block(after);
	ir_graph *irg   = get_irn_irg(value);
	ir_node  *frame = get_irg_frame(irg);
	ir_node  *mem   = get_irg_no_mem(irg);
	ir_mode  *mode  = get_irn_mode(value);

	ir_node  *store;
	if (mode_is_float(mode)) {
		store = create_stf(NULL, block, value, frame, mem, mode, NULL, 0, true);
	} else {
		store = new_bd_sparc_St_imm(NULL, block, mem, value, frame, mode, NULL, 0, true);
	}
	arch_add_irn_flags(store, arch_irn_flag_spill);
	sched_add_after(after, store);
	return store;
}

static ir_node *sparc_new_reload(ir_node *value, ir_node *spill,
                                 ir_node *before)
{
	ir_node  *block = get_block(before);
	ir_graph *irg   = get_irn_irg(value);
	ir_node  *frame = get_irg_frame(irg);
	ir_mode  *mode  = get_irn_mode(value);

	ir_node  *load;
	if (mode_is_float(mode)) {
		load = create_ldf(NULL, block, frame, spill, mode, NULL, 0, true);
	} else {
		load = new_bd_sparc_Ld_imm(NULL, block, spill, frame, mode, NULL, 0, true);
	}
	arch_add_irn_flags(load, arch_irn_flag_reload);
	sched_add_before(before, load);
	assert((long)pn_sparc_Ld_res == (long)pn_sparc_Ldf_res);
	return be_new_Proj(load, pn_sparc_Ld_res);
}

static const regalloc_if_t sparc_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = sparc_new_spill,
	.new_reload  = sparc_new_reload,
};

static void sparc_generate_code(FILE *output, const char *cup_name)
{
	be_gas_elf_type_char = '#';
	be_gas_elf_variant   = ELF_VARIANT_SPARC;
	sparc_constants = pmap_create();

	be_begin(output, cup_name);
	unsigned *const sp_is_non_ssa = rbitset_alloca(N_SPARC_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_SP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		struct obstack *obst = be_get_be_obst(irg);
		be_birg_from_irg(irg)->isa_link = OALLOCZ(obst, sparc_irg_data_t);

		be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
		sparc_select_instructions(irg);

		be_step_schedule(irg);

		be_timer_push(T_RA_PREPARATION);
		be_sched_fix_flags(irg, &sparc_reg_classes[CLASS_sparc_flags],
		                   NULL, sparc_modifies_flags, NULL);
		be_sched_fix_flags(irg, &sparc_reg_classes[CLASS_sparc_fpflags],
		                   NULL, sparc_modifies_fp_flags, NULL);
		be_timer_pop(T_RA_PREPARATION);

		be_step_regalloc(irg, &sparc_regalloc_if);

		sparc_finish_graph(irg);
		sparc_emit_function(irg);

		be_step_last(irg);
	}

	be_finish();
	pmap_destroy(sparc_constants);
}

static void sparc_lower_va_arg(ir_node *node)
{
	be_default_lower_va_arg(node, true, 4);
}

static const ir_settings_arch_dep_t sparc_arch_dep = {
	.replace_muls         = true,
	.replace_divs         = true,
	.replace_mods         = true,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.also_use_subs        = true,
	.maximum_shifts       = 1,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.max_bits_for_mulh    = 32,
};

static void sparc_lower_for_target(void)
{
	ir_arch_lower(&sparc_arch_dep);
	be_after_irp_transform("lower-arch-dep");

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

	if (!sparc_cg_config.use_fpu) {
		lower_floating_point();
		be_after_irp_transform("lower-fp");
	}

	ir_builtin_kind supported[8];
	size_t s = 0;
	supported[s++] = ir_bk_saturating_increment;
	if (sparc_cg_config.use_cas)
		supported[s++] = ir_bk_compare_swap;
	supported[s++] = ir_bk_va_start;
	assert(s < ARRAY_SIZE(supported));
	lower_builtins(s, supported, sparc_lower_va_arg);
	be_after_irp_transform("lower-builtins");

	ir_mode *mode_gp = sparc_reg_classes[CLASS_sparc_gp].mode;
	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, mode_gp);
		be_after_transform(irg, "lower-switch");
	}

	sparc_lower_64bit();
	be_after_irp_transform("lower-64");

	foreach_irp_irg(i, irg) {
		ir_lower_mode_b(irg, mode_Iu);
		be_after_transform(irg, "lower-modeb");
		lower_alloc(irg, SPARC_PO2_STACK_ALIGNMENT);
		be_after_transform(irg, "lower-alloc");
	}
}

static unsigned sparc_get_op_estimated_cost(const ir_node *node)
{
	/* TODO: refine */
	if (sparc_has_load_store_attr(node))
		return 5;
	return 1;
}

arch_isa_if_t const sparc_isa_if = {
	.name                  = "sparc",
	.pointer_size          = 4,
	.big_endian            = true,
	.modulo_shift          = 32,
	.po2_biggest_alignment = 3,
	.pic_supported         = false,
	.n_registers           = N_SPARC_REGISTERS,
	.registers             = sparc_registers,
	.n_register_classes    = N_SPARC_CLASSES,
	.register_classes      = sparc_reg_classes,
	.init                  = sparc_init,
	.finish                = sparc_finish,
	.generate_code         = sparc_generate_code,
	.lower_for_target      = sparc_lower_for_target,
	.handle_intrinsics     = sparc_handle_intrinsics,
	.get_op_estimated_cost = sparc_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_sparc)
void be_init_arch_sparc(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *sparc_grp = lc_opt_get_grp(be_grp, "sparc");

	lc_opt_add_table(sparc_grp, sparc_options);

	sparc_init_transform();
	sparc_init_emitter();
}
