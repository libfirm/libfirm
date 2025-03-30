/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the IR transformation from firm into
 *              ia32-Firm.
 * @author      Christian Wuerdig, Matthias Braun
 */
#include "ia32_transform.h"

#include "array.h"
#include "bediagnostic.h"
#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "debug.h"
#include "gen_ia32_regalloc_if.h"
#include "heights.h"
#include "ia32_architecture.h"
#include "ia32_bearch_t.h"
#include "ia32_new_nodes.h"
#include "ia32_nodes_attr.h"
#include "irargs_t.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "iropt.h"
#include "iropt_t.h"
#include "irouts.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "panic.h"
#include "platform_t.h"
#include "tv_t.h"
#include "util.h"
#include "x86_address_mode.h"
#include "x86_cconv.h"
#include "x86_x87.h"
#include <stdbool.h>

/* define this to construct SSE constants instead of load them */
#undef CONSTRUCT_SSE_CONST

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static x86_cconv_t          *current_cconv;
static be_stack_env_t        stack_env;
static ir_heights_t         *heights;
static x86_immediate_kind_t  lconst_imm_kind;
static x86_addr_variant_t    lconst_variant;
static ir_node              *initial_va_list;

#define GP &ia32_reg_classes[CLASS_ia32_gp]
#define FP &ia32_reg_classes[CLASS_ia32_fp]
const x86_asm_constraint_list_t ia32_asm_constraints = {
	['A'] = { MATCH_REG, GP, 1 << REG_GP_EAX | 1 << REG_GP_EDX },
	['D'] = { MATCH_REG, GP, 1 << REG_GP_EDI },
	['I'] = { MATCH_IMM, GP, 0 },
	['J'] = { MATCH_IMM, GP, 0 },
	['K'] = { MATCH_IMM, GP, 0 },
	['L'] = { MATCH_IMM, GP, 0 },
	['M'] = { MATCH_IMM, GP, 0 },
	['N'] = { MATCH_IMM, GP, 0 },
	['O'] = { MATCH_IMM, GP, 0 },
	['R'] = { MATCH_REG, GP, 0 },
	['S'] = { MATCH_REG, GP, 1 << REG_GP_ESI },
	['Q'] = { MATCH_REG, GP, 1 << REG_GP_EAX | 1 << REG_GP_EBX
		| 1 << REG_GP_ECX | 1 << REG_GP_EDX },
	['V'] = { MATCH_MEM, GP, 0 },
	['X'] = { MATCH_ANY, GP, 0 },
	['a'] = { MATCH_REG, GP, 1 << REG_GP_EAX },
	['b'] = { MATCH_REG, GP, 1 << REG_GP_EBX },
	['c'] = { MATCH_REG, GP, 1 << REG_GP_ECX },
	['d'] = { MATCH_REG, GP, 1 << REG_GP_EDX },
	['e'] = { MATCH_IMM, GP, 0 },
	['f'] = { MATCH_REG, FP, 0 },
	['g'] = { MATCH_ANY, GP, 0 },
	['i'] = { MATCH_IMM, GP, 0 },
	['l'] = { MATCH_REG, GP, 1 << REG_GP_EAX | 1 << REG_GP_EBX
		| 1 << REG_GP_ECX | 1 << REG_GP_EDX | 1 << REG_GP_ESI
		| 1 << REG_GP_EDI | 1 << REG_GP_EBP },
	['m'] = { MATCH_MEM, GP, 0 },
	['n'] = { MATCH_IMM, GP, 0 },
	['o'] = { MATCH_MEM, GP, 0 },
	['p'] = { MATCH_REG, GP, 0 },
	['q'] = { MATCH_REG, GP, 1 << REG_GP_EAX | 1 << REG_GP_EBX
		| 1 << REG_GP_ECX | 1 << REG_GP_EDX },
	['r'] = { MATCH_REG, GP, 0 },
	['t'] = { MATCH_REG, FP, 1 << REG_FP_ST0 },
	['u'] = { MATCH_REG, FP, 1 << REG_FP_ST1 },
	['x'] = { MATCH_REG, &ia32_reg_classes[CLASS_ia32_xmm], 0 },

	// There are more constraints and in general there is a lot of internal gcc
	// logic at play not everything is documented in the manual. In the gcc
	// source you can look at reload.c, stmt.c and constraints.md. I am not sure
	// how much we want/need to understand and reimplement here.
};
#undef GP
#undef FP

typedef ir_node *construct_binop_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op1,
        ir_node *op2, x86_insn_size_t size);

typedef ir_node *construct_binop_flags_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op1, ir_node *op2,
        ir_node *flags, x86_insn_size_t size);

typedef ir_node *construct_shift_func(dbg_info *db, ir_node *block,
        ir_node *op1, ir_node *op2, x86_insn_size_t size);

typedef ir_node *construct_binop_dest_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op,
        x86_insn_size_t size);

typedef ir_node *construct_unop_dest_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, x86_insn_size_t size);

typedef ir_node *construct_binop_float_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op1, ir_node *op2,
        ir_node *fpcw, x86_insn_size_t size);

typedef ir_node *construct_unop_func(dbg_info *db, ir_node *block, ir_node *op,
                                     x86_insn_size_t size);

static ir_node *create_immediate_or_transform(ir_node *node, char immediate_mode);

static ir_node *create_I2I_Conv(ir_mode *src_mode, dbg_info *dbgi, ir_node *block, ir_node *op);

/* its enough to have those once */
static ir_node *nomem;
static ir_node *noreg_GP;

/** Return non-zero is a node represents the -1 constant. */
static bool is_Const_Minus_1(ir_node *node)
{
	return is_Const(node) && is_Const_all_one(node);
}

/**
 * returns true if constant can be created with a simple float command
 */
static bool is_simple_x87_Const(ir_node *node)
{
	ir_tarval *tv = get_Const_tarval(node);
	if (tarval_is_null(tv) || tarval_is_one(tv))
		return true;

	/* TODO: match all the other float constants */
	return false;
}

/**
 * returns true if constant can be created with a simple float command
 */
static bool is_simple_sse_Const(ir_node *node)
{
	ir_tarval *tv   = get_Const_tarval(node);
	ir_mode   *mode = get_tarval_mode(tv);

	if (get_mode_size_bits(mode) == 32)
		return true;

	if (tarval_is_null(tv)
#ifdef CONSTRUCT_SSE_CONST
	    || tarval_is_one(tv)
#endif
	   )
		return true;
#ifdef CONSTRUCT_SSE_CONST
	if (mode == ia32_mode_float64) {
		unsigned const val = be_get_tv_bits32(tv, 0);
		if (val == 0)
			/* lower 32bit are zero, really a 32bit constant */
			return true;
	}
#endif /* CONSTRUCT_SSE_CONST */
	/* TODO: match all the other float constants */
	return false;
}

ir_node *ia32_get_pic_base(ir_graph *irg)
{
	ir_node **const get_eip = &ia32_get_irg_data(irg)->get_eip;
	if (!*get_eip) {
		ir_node *const block = get_irg_start_block(irg);
		*get_eip = new_bd_ia32_GetEIP(NULL, block);
	}
	return *get_eip;
}

/**
 * return NoREG or pic_base in case of PIC.
 * This is necessary as base address for newly created symbols
 */
static ir_node *get_global_base(ir_graph *const irg)
{
	if (ir_platform.pic_style != BE_PIC_NONE)
		return ia32_get_pic_base(irg);
	return noreg_GP;
}

static ir_node *get_initial_sp(ir_graph *irg)
{
	return be_get_Start_proj(irg, &ia32_registers[REG_ESP]);
}

static ir_node *get_initial_fp(ir_graph *irg)
{
	return be_get_Start_proj(irg, &ia32_registers[REG_EBP]);
}

static ir_node *get_initial_fpcw(ir_graph *irg)
{
	return be_get_Start_proj(irg, &ia32_registers[REG_FPCW]);
}

ir_node *ia32_create_Immediate_full(ir_graph *const irg,
                                    x86_imm32_t const *const imm)
{
	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const immediate
		= new_bd_ia32_Immediate(NULL, start_block, imm);
	arch_set_irn_register(immediate, &ia32_registers[REG_GP_NOREG]);
	return immediate;
}

static void adjust_relocation(x86_imm32_t *imm)
{
	if (imm->kind != X86_IMM_ADDR)
		return;
	ir_entity *entity = imm->entity;
	if (is_tls_entity(entity)) {
		imm->kind = entity_has_definition(entity) ? X86_IMM_TLS_LE
		                                          : X86_IMM_TLS_IE;
		return;
	}
}

static ir_node *try_create_Immediate(const ir_node *node, char const constraint)
{
	x86_imm32_t immediate;
	if (!x86_match_immediate(&immediate, node, constraint))
		return NULL;
	adjust_relocation(&immediate);

	ir_graph *const irg = get_irn_irg(node);
	return ia32_create_Immediate_full(irg, &immediate);
}

static ir_entity *create_float_const_entity(ir_tarval *tv, ident *name)
{
	ir_mode *mode = get_tarval_mode(tv);
	if (!ia32_cg_config.use_sse2) {
		/* try to reduce the mode to produce smaller sized entities */
		ir_mode *const modes[] = { mode_F, mode_D, NULL };
		for (ir_mode *const *i = modes;; ++i) {
			ir_mode *const to = *i;
			if (!to || to == mode)
				break;
			if (tarval_ieee754_can_conv_lossless(tv, to)) {
				tv   = tarval_convert_to(tv, to);
				mode = to;
				break;
			}
		}
	}

	ir_entity *res = pmap_get(ir_entity, ia32_tv_ent, tv);
	if (!res) {
		if (!name)
			name = id_unique("C");

		ir_type *const tp = get_type_for_mode(mode);
		res = new_global_entity(get_glob_type(), name, tp,
		                        ir_visibility_private,
		                        IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

		ir_initializer_t *const initializer = create_initializer_tarval(tv);
		set_entity_initializer(res, initializer);

		pmap_insert(ia32_tv_ent, tv, res);
	}
	return res;
}

static void set_am_const_entity(ir_node *node, ir_entity *entity)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->addr.immediate = (x86_imm32_t) {
		.kind   = lconst_imm_kind,
		.entity = entity,
	};
	attr->addr.variant = lconst_variant;
}

/**
 * Transforms a Const.
 */
static ir_node *gen_Const(ir_node *node)
{
	ir_node        *const block = be_transform_nodes_block(node);
	dbg_info       *const dbgi  = get_irn_dbg_info(node);
	ir_mode        *const mode  = get_irn_mode(node);
	ir_tarval      *const tv    = get_Const_tarval(node);
	x86_insn_size_t const size  = x86_size_from_mode(mode);

	if (mode_is_float(mode)) {
		ir_graph *const irg = get_irn_irg(node);
		ir_node        *res;
		if (ia32_cg_config.use_sse2) {
			if (tarval_is_null(tv)) {
				res = new_bd_ia32_xZero(dbgi, block, size);
#ifdef CONSTRUCT_SSE_CONST
			} else if (tarval_is_one(tv)) {
				int      cnst    = mode == ia32_mode_float32 ? 26 : 55;
				ir_node *imm1    = ia32_create_Immediate(irg, cnst);
				ir_node *imm2    = ia32_create_Immediate(irg, 2);
				ir_node *allones = new_bd_ia32_xAllOnes(dbgi, block, size);
				ir_node *pslld   = new_bd_ia32_xPslld(dbgi, block, allones,
				                                      imm1, size);
				res = new_bd_ia32_xPsrld(dbgi, block, pslld, imm2, size);
#endif /* CONSTRUCT_SSE_CONST */
			} else if (mode == ia32_mode_float32) {
				/* we can place any 32bit constant by using a movd gp, sse */
				x86_imm32_t imm = {
					.offset = be_get_tv_bits32(tv, 0),
				};
				ir_node *cnst = new_bd_ia32_Const(dbgi, block, &imm);
				res = new_bd_ia32_Movd(dbgi, block, cnst);
			} else {
#ifdef CONSTRUCT_SSE_CONST
				if (mode == ia32_mode_float64 && be_get_tv_bits32(tv, 0) == 0) {
					ir_node *imm32 = ia32_create_Immediate(irg, 32);

					/* fine, lower 32bit are zero, produce 32bit value */
					x86_imm32_t const imm = {
						.offset = be_get_tv_bits32(tv, 4),
					};
					ir_node *const cnst = new_bd_ia32_Const(dbgi, block, &imm);
					ir_node *const movd = new_bd_ia32_xMovd(dbgi, block, cnst);
					res = new_bd_ia32_xPsllq(dbgi, block, movd, imm32, size);
					goto end;
				}
#endif /* CONSTRUCT_SSE_CONST */
				ir_entity *const floatent = create_float_const_entity(tv, NULL);

				ir_node *base = get_global_base(irg);
				ir_node *load = new_bd_ia32_xLoad(dbgi, block, base, noreg_GP,
				                                  nomem, size);
				set_ia32_op_type(load, ia32_AddrModeS);
				set_am_const_entity(load, floatent);
				arch_add_irn_flags(load, arch_irn_flag_rematerializable);
				res = be_new_Proj(load, pn_ia32_xLoad_res);
			}
		} else {
			if (tarval_is_null(tv)) {
				res = new_bd_ia32_fldz(dbgi, block);
			} else if (tarval_is_one(tv)) {
				res = new_bd_ia32_fld1(dbgi, block);
			} else if (tarval_is_minus_null(tv)) {
				res = new_bd_ia32_fldz(dbgi, block);
				goto negate;
			} else if (tarval_is_minus_one(tv)) {
				res = new_bd_ia32_fld1(dbgi, block);
negate:
				res = new_bd_ia32_fchs(dbgi, block, res);
			} else {
				ir_entity *const floatent = create_float_const_entity(tv, NULL);
				/* create_float_const_ent is smart and sometimes creates
				   smaller entities */
				ir_mode *ent_mode = get_type_mode(get_entity_type(floatent));
				x86_insn_size_t const size = x86_size_from_mode(ent_mode);
				ir_node *base = get_global_base(irg);
				ir_node *load = new_bd_ia32_fld(dbgi, block, base, noreg_GP,
				                                nomem, size);
				set_am_const_entity(load, floatent);
				set_ia32_op_type(load, ia32_AddrModeS);
				arch_add_irn_flags(load, arch_irn_flag_rematerializable);
				res = be_new_Proj(load, pn_ia32_fld_res);
			}
		}
#ifdef CONSTRUCT_SSE_CONST
end:
#endif
		return res;
	} else { /* non-float mode */
		ir_tarval *conv_tv = tarval_convert_to(tv, ia32_mode_gp);
		if (conv_tv == get_tarval_bad())
			panic("couldn't convert constant tarval (%+F)", node);

		x86_imm32_t imm = { .offset = get_tarval_long(conv_tv) };
		return new_bd_ia32_Const(dbgi, block, &imm);
	}
}

/**
 * Transforms an Address.
 */
static ir_node *gen_Address(ir_node *node)
{
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);

	ir_entity *entity = get_Address_entity(node);
	x86_imm32_t imm = {
		.kind   = X86_IMM_ADDR,
		.entity = entity,
	};
	adjust_relocation(&imm);

	if (is_tls_entity(entity)) {
		ir_node     *const tls_base = new_bd_ia32_LdTls(NULL, block);
		ir_node     *const lea      = new_bd_ia32_Lea(dbgi, block, tls_base, noreg_GP);
		ia32_attr_t *const attr     = get_ia32_attr(lea);
		attr->addr.variant   = X86_ADDR_BASE;
		attr->addr.immediate = imm;
		return lea;
	} else {
		return new_bd_ia32_Const(dbgi, block, &imm);
	}
}

static ir_node *gen_be_Relocation(ir_node *node)
{
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	x86_imm32_t imm = {
		.kind   = (x86_immediate_kind_t)be_get_Relocation_kind(node),
		.entity = be_get_Relocation_entity(node),
	};
	return new_bd_ia32_Const(dbgi, block, &imm);
}

/**
 * Create a float[2] array type for the given atomic type.
 *
 * @param tp  the atomic type
 */
static ir_type *ia32_create_float_array(ir_type *tp)
{
	static ir_type *float_F;
	static ir_type *float_D;
	static ir_type *float_E;
	ir_mode  *const mode = get_type_mode(tp);
	ir_type **const arr  =
		mode == ia32_mode_float32 ? &float_F :
		mode == ia32_mode_float64 ? &float_D :
		/*                       */ &float_E;
	if (!*arr)
		*arr = new_type_array(tp, 2);
	return *arr;
}

/* Generates an entity for a known FP const (used for FP Neg + Abs) */
ir_entity *ia32_gen_fp_known_const(ia32_known_const_t const kct)
{
	static const struct {
		const char *name;
		const char *cnst_str;
		char        mode;
	} names [ia32_known_const_max] = {
		{ "C_sfp_sign", "0x80000000",          0 },
		{ "C_dfp_sign", "0x8000000000000000",  1 },
		{ "C_sfp_abs",  "0x7FFFFFFF",          0 },
		{ "C_dfp_abs",  "0x7FFFFFFFFFFFFFFF",  1 },
		{ "C_ull_bias", "0x10000000000000000", 2 }
	};
	static ir_entity *ent_cache[ia32_known_const_max];

	ir_entity *ent = ent_cache[kct];

	if (ent == NULL) {
		char const *const cnst_str = names[kct].cnst_str;
		ident      *const name     = new_id_from_str(names[kct].name);
		ir_mode          *mode;
		switch (names[kct].mode) {
		case 0:  mode = ia32_mode_gp; break;
		case 1:  mode = mode_Lu; break;
		case 2:  mode = ia32_mode_float32;  break;
		default: panic("internal compiler error");
		}
		ir_tarval *tv = new_tarval_from_str(cnst_str, strlen(cnst_str), mode);

		if (kct == ia32_ULLBIAS) {
			ir_type *type  = get_type_for_mode(ia32_mode_float32);
			ir_type *atype = ia32_create_float_array(type);

			ent = new_global_entity(get_glob_type(), name, atype,
			                        ir_visibility_private,
			                        IR_LINKAGE_CONSTANT|IR_LINKAGE_NO_IDENTITY);

			ir_initializer_t *initializer = create_initializer_compound(2);
			set_initializer_compound_value(initializer, 0,
				create_initializer_tarval(get_mode_null(mode)));
			set_initializer_compound_value(initializer, 1,
				create_initializer_tarval(tv));
			set_entity_initializer(ent, initializer);
		} else {
			ent = create_float_const_entity(tv, name);
		}
		/* cache the entry */
		ent_cache[kct] = ent;
	}

	return ent_cache[kct];
}

static ir_node *gen_Unknown(ir_node *node)
{
	ir_node *const block = be_transform_nodes_block(node);
	ir_mode *const mode  = get_irn_mode(node);
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			return be_new_Unknown(block, &ia32_class_reg_req_xmm);
		} else {
			/* Must occupy a slot on the x87 register stack. */
			return new_bd_ia32_fldz(NULL, block);
		}
	} else if (be_mode_needs_gp_reg(mode)) {
		return be_new_Unknown(block, &ia32_class_reg_req_gp);
	} else {
		panic("unsupported Unknown-Mode");
	}
}

static bool prevents_AM_one(ir_node *const other, ir_node *const am_candidate)
{
	/* Do not block ourselves from getting eaten */
	if (is_Proj(other) && get_Proj_pred(other) == am_candidate)
		return false;

	if (!heights_reachable_in_block(heights, other, am_candidate))
		return false;

	return true;
}

/**
 * Checks whether other node inputs depend on the am_candidate (via mem-proj).
 */
static bool prevents_AM(ir_node *const block, ir_node *const am_candidate,
                        ir_node *const other)
{
	if (get_nodes_block(other) != block)
		return false;

	if (is_Sync(other)) {
		for (int i = get_Sync_n_preds(other); i-- != 0;) {
			ir_node *const pred = get_Sync_pred(other, i);
			if (get_nodes_block(pred) == block && prevents_AM_one(pred, am_candidate))
				return true;
		}

		return false;
	}

	return prevents_AM_one(other, am_candidate);
}

static bool cmp_can_use_sub_flags(ir_node *cmp, ir_node *sub, bool *swap)
{
	ir_node *cmp_block = get_nodes_block(cmp);
	ir_node *sub_block = get_nodes_block(sub);

	if (!(block_dominates(cmp_block, sub_block) ||
	      block_dominates(sub_block, cmp_block))) {
		return false;
	}

	ir_node *cmp_left  = get_Cmp_left(cmp);
	ir_node *cmp_right = get_Cmp_right(cmp);
	ir_node *sub_left  = get_Sub_left(sub);
	ir_node *sub_right = get_Sub_right(sub);

	if (cmp_left == sub_left && cmp_right == sub_right) {
		*swap = false;
	} else if (cmp_left == sub_right && cmp_right == sub_left) {
		*swap = true;
	} else {
		return false;
	}

	ir_mode *sub_mode = get_irn_mode(sub_left);
	if (get_mode_size_bits(sub_mode) != 32 &&
	    (!be_upper_bits_clean(sub_left,  sub_mode) ||
	     !be_upper_bits_clean(sub_right, sub_mode))) {
		return false;
	}

	return true;
}

/**
 * return true if the users of the given value will be merged by later
 * optimization. This applies to multiple Cmp nodes (and maybe a Sub
 * node) with the same inputs.
 */
static bool users_will_merge(ir_node *proj)
{
	ir_node *sub = NULL;

	/* Check that there is one Sub and some Cmps. */
	foreach_out_edge(proj, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if (is_Sub(user)) {
			if (sub == NULL) {
				sub = user;
			} else {
				/* Two Subs will not merge */
				return false;
			}
		} else if (!is_Cmp(user)) {
			return false;
		}
	}

	if (sub == NULL) {
		/* Cmps only will not merge.
		 * (probably need to be rematerialized later anyway) */
		return false;
	}

	foreach_out_edge(proj, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if (is_Cmp(user)) {
			bool dump;
			if (!cmp_can_use_sub_flags(user, sub, &dump)) {
				return false;
			}
		}
	}
	return true;
}

/**
 * return true if the node is a Proj(Load) and could be used in source address
 * mode for another node. Will return only true if the @p other node is not
 * dependent on the memory of the Load (for binary operations use the other
 * input here, for unary operations use NULL).
 */
static bool ia32_use_source_address_mode(ir_node *block, ir_node *node,
                                         ir_node *other, ir_node *other2,
                                         match_flags_t flags)
{
	/* float constants are always available */
	if (is_Const(node)) {
		ir_mode *mode = get_irn_mode(node);
		if (mode_is_float(mode)) {
			ir_tarval *tv = get_Const_tarval(node);
			if (!tarval_ieee754_can_conv_lossless(tv, ia32_mode_float64))
				return false;
			if (ia32_cg_config.use_sse2) {
				if (is_simple_sse_Const(node))
					return false;
			} else if (is_simple_x87_Const(node)) {
				return false;
			}
			if (get_irn_n_edges(node) > 1)
				return false;
			return true;
		}
		return false;
	}

	if (!is_Proj(node))
		return false;
	ir_node *load = get_Proj_pred(node);
	if (!is_Load(load))
		return false;
	assert(get_Proj_num(node) == pn_Load_res);
	if (get_nodes_block(load) != block)
		return false;
	ir_mode *mode = get_irn_mode(node);
	/* we can't fold mode_E AM */
	if (mode == x86_mode_E)
		return false;
	/* we only use address mode if we're the only user of the load
	 * or the users will be merged later anyway */
	if (get_irn_n_edges(node) != (flags & match_two_users ? 2 : 1) &&
	    !users_will_merge(node))
		return false;
	/* in some edge cases with address mode we might reach the load normally
	 * and through some AM sequence, if it is already materialized then we
	 * can't create an AM node from it */
	if (be_is_transformed(node))
		return false;

	/* don't do AM if other node inputs depend on the load (via mem-proj) */
	if (other != NULL && prevents_AM(block, load, other))
		return false;

	if (other2 != NULL && prevents_AM(block, load, other2))
		return false;

	return true;
}

typedef struct ia32_address_mode_t ia32_address_mode_t;
struct ia32_address_mode_t {
	x86_address_t   addr;
	ir_node        *mem_proj;
	x86_insn_size_t size;
	ia32_op_type_t  op_type;
	ir_node        *new_op1;
	ir_node        *new_op2;
	bool            pinned       : 1;
	unsigned        commutative  : 1;
	unsigned        ins_permuted : 1;
};

static void ia32_create_address_mode(x86_address_t *addr, ir_node *ptr,
                                     x86_create_am_flags_t flags)
{
	x86_create_address_mode(addr, ptr, flags);
	adjust_relocation(&addr->imm);
}

static ir_node *transform_else_noreg(ir_node *const node)
{
	return node ? be_transform_node(node) : noreg_GP;
}

static void build_address_ptr(x86_address_t *const addr, ir_node *const ptr, ir_node *const mem, x86_create_am_flags_t const flags)
{
	/* construct load address */
	ia32_create_address_mode(addr, ptr, flags);
	addr->base  = transform_else_noreg(addr->base);
	addr->index = transform_else_noreg(addr->index);
	addr->mem   = be_transform_node(mem);
}

static void build_address(ia32_address_mode_t *am, ir_node *node,
                          x86_create_am_flags_t flags)
{
	x86_address_t *addr = &am->addr;

	/* floating point immediates */
	if (is_Const(node)) {
		ir_graph  *const irg    = get_irn_irg(node);
		ir_tarval *const tv     = get_Const_tarval(node);
		ir_entity *const entity = create_float_const_entity(tv, NULL);
		addr->base        = get_global_base(irg);
		addr->index       = noreg_GP;
		addr->mem         = nomem;
		addr->imm         = (x86_imm32_t) {
			.kind   = lconst_imm_kind,
			.entity = entity,
		};
		addr->variant     = lconst_variant,
		adjust_relocation(&addr->imm);
		addr->tls_segment = false;
		am->size          = x86_size_from_mode(get_type_mode(get_entity_type(entity)));
		am->pinned        = false;
		return;
	}

	ir_node *const load = get_Proj_pred(node);
	am->pinned   = get_irn_pinned(load);
	am->size     = x86_size_from_mode(get_Load_mode(load));
	am->mem_proj = get_Proj_for_pn(load, pn_Load_M);

	/* construct load address */
	ir_node *const ptr = get_Load_ptr(load);
	ir_node *const mem = get_Load_mem(load);
	build_address_ptr(addr, ptr, mem, flags);
}

static void set_address(ir_node *node, const x86_address_t *addr)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->addr.immediate = addr->imm;
	attr->addr.log_scale = addr->scale;
	attr->addr.variant   = addr->variant;
	if (addr->tls_segment)
		attr->addr.segment = X86_SEGMENT_GS;
	if (addr->imm.kind == X86_IMM_FRAMEENT)
		set_ia32_frame_use(node, IA32_FRAME_USE_AUTO);
}

static void set_indexed_ent(ir_node *const node, unsigned const scale, ir_entity *const ent)
{
	x86_address_t const addr = {
		.variant = ir_platform.pic_style != BE_PIC_NONE ? X86_ADDR_BASE_INDEX : X86_ADDR_INDEX,
		.scale   = scale,
		.imm     = {
			.kind   = lconst_imm_kind,
			.entity = ent,
		},
	};
	set_address(node, &addr);
	set_ia32_op_type(node, ia32_AddrModeS);
}

/**
 * Apply attributes of a given address mode to a node.
 */
static void set_am_attributes(ir_node *node, const ia32_address_mode_t *am)
{
	set_address(node, &am->addr);

	set_ia32_op_type(node, am->op_type);
	if (am->pinned && !get_irn_pinned(node))
		set_irn_pinned(node, true);
	if (am->commutative)
		set_ia32_commutative(node);
}

static bool is_float_downconv(const ir_node *node)
{
	if (!is_Conv(node))
		return false;
	ir_node *pred      = get_Conv_op(node);
	ir_mode *pred_mode = get_irn_mode(pred);
	ir_mode *mode      = get_irn_mode(node);
	return mode_is_float(pred_mode)
	    && get_mode_size_bits(mode) <= get_mode_size_bits(pred_mode);
}

static ir_node *ia32_skip_float_downconv(ir_node *node)
{
	while (is_float_downconv(node)) {
		node = get_Conv_op(node);
	}
	return node;
}

static ir_node *transform_sext(ir_node *const node)
{
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	/* normalize to a signed mode */
	switch (get_mode_size_bits(mode)) {
	case 8:  mode = mode_Bs; break;
	case 16: mode = mode_Hs; break;
	default:
		panic("invalid mode in sext: %+F", node);
	}
	return create_I2I_Conv(mode, dbgi, block, node);
}

static ir_node *transform_zext(ir_node *const node)
{
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	/* normalize to an unsigned mode */
	switch (get_mode_size_bits(mode)) {
	case 8:  mode = mode_Bu; break;
	case 16: mode = mode_Hu; break;
	default:
		panic("invalid mode in zext: %+F", node);
	}
	return create_I2I_Conv(mode, dbgi, block, node);
}

static ir_node *get_noreg(ir_graph *const irg, ir_mode *const mode)
{
	if (!mode_is_float(mode)) {
		return noreg_GP;
	} else if (ia32_cg_config.use_sse2) {
		return ia32_new_NoReg_xmm(irg);
	} else {
		return ia32_new_NoReg_fp(irg);
	}
}

/**
 * matches operands of a node into ia32 addressing/operand modes. This covers
 * usage of source address mode, immediates, operations with non 32-bit modes,
 * ...
 * The resulting data is filled into the @p am struct. block is the block
 * of the node whose arguments are matched. op1, op2 are the first and second
 * input that are matched (op1 may be NULL). other_op is another unrelated
 * input that is not matched! but which is needed sometimes to check if AM
 * for op1/op2 is legal.
 * @p flags describes the supported modes of the operation in detail.
 */
static void match_arguments(ia32_address_mode_t *am, ir_node *block,
                            ir_node *op1, ir_node *op2, ir_node *other_op,
                            match_flags_t flags)
{
	memset(am, 0, sizeof(am[0]));

	bool commutative           = (flags & match_commutative) != 0;
	bool use_am_and_immediates = (flags & match_am_and_immediates) != 0;
	bool use_am                = (flags & match_am) != 0;
	bool use_immediate         = (flags & match_immediate) != 0;
	assert(!use_am_and_immediates || use_immediate);

	assert(op2 != NULL);
	assert(!commutative || op1 != NULL);
	assert(use_am || !(flags & match_8bit_am));
	assert(use_am || !(flags & match_16bit_am));

	ir_mode *mode      = get_irn_mode(op2);
	int      mode_bits = get_mode_size_bits(mode);
	if ((mode_bits ==  8 && !(flags & match_8bit_am)) ||
	    (mode_bits == 16 && !(flags & match_16bit_am))) {
		use_am = false;
	}

	if (be_mode_needs_gp_reg(mode)) {
		if (flags & match_mode_neutral) {
			/* we can simply skip downconvs for mode neutral nodes: the upper bits
			 * can be random for these operations */
			op2 = be_skip_downconv(op2, true);
			if (op1 != NULL) {
				op1 = be_skip_downconv(op1, true);
			}
		} else {
			op2 = be_skip_sameconv(op2);
			if (op1 != NULL) {
				op1 = be_skip_sameconv(op1);
			}
		}
	}

	/* match immediates. firm nodes are normalized: constants are always on the
	 * op2 input */
	ir_node *new_op2 = NULL;
	if (!(flags & match_try_am) && use_immediate) {
		new_op2 = try_create_Immediate(op2, 'i');
	}

	ir_node *new_op1;
	if (new_op2 == NULL &&
	    use_am && ia32_use_source_address_mode(block, op2, op1, other_op, flags)) {
		build_address(am, op2, x86_create_am_normal);
		new_op1     = (op1 == NULL ? NULL : be_transform_node(op1));
		ir_graph *const irg = get_irn_irg(block);
		new_op2     = get_noreg(irg, mode);
		am->op_type = ia32_AddrModeS;
	} else if (commutative && (new_op2 == NULL || use_am_and_immediates) &&
	           use_am &&
	           ia32_use_source_address_mode(block, op1, op2, other_op, flags)) {
		build_address(am, op1, x86_create_am_normal);

		ir_graph *const irg   = get_irn_irg(block);
		ir_node  *const noreg = get_noreg(irg, mode);
		if (new_op2 != NULL) {
			new_op1 = noreg;
		} else {
			new_op1 = be_transform_node(op2);
			new_op2 = noreg;
			am->ins_permuted = true;
		}
		am->op_type = ia32_AddrModeS;
	} else {
		am->op_type      = ia32_Normal;
		am->addr.base    = noreg_GP;
		am->addr.index   = noreg_GP;
		am->addr.mem     = nomem;
		am->addr.variant = X86_ADDR_REG;

		if (flags & match_try_am) {
			am->new_op1 = NULL;
			am->new_op2 = NULL;
			return;
		}

		ir_node            *(*transform)(ir_node*);
		ir_mode        *const op2_mode = get_irn_mode(op2);
		x86_insn_size_t       size     = x86_size_from_mode(op2_mode);
		if (size == X86_SIZE_32 || flags & match_mode_neutral) {
			transform = &be_transform_node;
			size      = X86_SIZE_32;
		} else if (flags & match_sign_ext) {
			transform = &transform_sext;
			size      = X86_SIZE_32;
		} else if (flags & match_zero_ext) {
			transform = &transform_zext;
			size      = X86_SIZE_32;
		} else {
			transform = &be_transform_node;
		}
		new_op1 = op1 ? transform(op1) : NULL;
		if (!new_op2)
			new_op2 = transform(op2);
		am->size = size;
	}

	am->new_op1     = new_op1;
	am->new_op2     = new_op2;
	am->commutative = commutative;
}

/**
 * "Fixes" a node that uses address mode by turning it into mode_T
 * and returning a pn_ia32_res Proj.
 *
 * @param node  the node
 * @param am    its address mode
 *
 * @return a Proj(pn_ia32_res) if a memory address mode is used,
 *         node else
 */
static ir_node *fix_mem_proj(ir_node *const node, ia32_address_mode_t const *const am)
{
	if (am->mem_proj == NULL)
		return node;

	ir_node *load = get_Proj_pred(am->mem_proj);
	be_set_transformed_node(load, node);

	/* we have to create a mode_T so the old MemProj can attach to us */
	if (get_irn_mode(node) != mode_T) {
		set_irn_mode(node, mode_T);
		return be_new_Proj(node, pn_ia32_res);
	} else {
		return node;
	}
}

static ir_node *make_binop(ir_node *const node, ia32_address_mode_t const *const am, construct_binop_func *const func)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const res   = func(dbgi, block, am->addr.base, am->addr.index, am->addr.mem, am->new_op1, am->new_op2, am->size);
	set_am_attributes(res, am);
	return fix_mem_proj(res, am);
}

/**
 * Construct a standard binary operation, set AM and immediate if required.
 *
 * @param node  The original node for which the binop is created
 * @param op1   The first operand
 * @param op2   The second operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_binop(ir_node *node, ir_node *op1, ir_node *op2,
                          construct_binop_func *func, match_flags_t flags)
{
	ir_node *block = get_nodes_block(node);
	ia32_address_mode_t am;
	match_arguments(&am, block, op1, op2, NULL, flags);

	ir_node *const new_node = make_binop(node, &am, func);
	/* we can't use source address mode anymore when using immediates */
	if (!(flags & match_am_and_immediates) &&
	    (is_ia32_Immediate(am.new_op1) || is_ia32_Immediate(am.new_op2)))
		set_ia32_am_support(new_node, ia32_am_none);

	return new_node;
}

/**
 * Generic names for the inputs of an ia32 binary op.
 */
enum {
	n_ia32_l_binop_left,  /**< ia32 left input */
	n_ia32_l_binop_right, /**< ia32 right input */
	n_ia32_l_binop_eflags /**< ia32 eflags input */
};
COMPILETIME_ASSERT((int)n_ia32_l_binop_left   == (int)n_ia32_l_Adc_left,       n_Adc_left)
COMPILETIME_ASSERT((int)n_ia32_l_binop_right  == (int)n_ia32_l_Adc_right,      n_Adc_right)
COMPILETIME_ASSERT((int)n_ia32_l_binop_eflags == (int)n_ia32_l_Adc_eflags,     n_Adc_eflags)
COMPILETIME_ASSERT((int)n_ia32_l_binop_left   == (int)n_ia32_l_Sbb_minuend,    n_Sbb_minuend)
COMPILETIME_ASSERT((int)n_ia32_l_binop_right  == (int)n_ia32_l_Sbb_subtrahend, n_Sbb_subtrahend)
COMPILETIME_ASSERT((int)n_ia32_l_binop_eflags == (int)n_ia32_l_Sbb_eflags,     n_Sbb_eflags)

/**
 * Construct a binary operation which also consumes the eflags.
 *
 * @param node  The node to transform
 * @param func  The node constructor function
 * @param flags The match flags
 * @return      The constructor ia32 node
 */
static ir_node *gen_binop_flags(ir_node *node, construct_binop_flags_func *func,
                                match_flags_t flags)
{
	ir_node             *src_block  = get_nodes_block(node);
	ir_node             *op1        = get_irn_n(node, n_ia32_l_binop_left);
	ir_node             *op2        = get_irn_n(node, n_ia32_l_binop_right);
	ir_node             *eflags     = get_irn_n(node, n_ia32_l_binop_eflags);

	ia32_address_mode_t  am;
	match_arguments(&am, src_block, op1, op2, eflags, flags);

	dbg_info      *dbgi       = get_irn_dbg_info(node);
	ir_node       *block      = be_transform_node(src_block);
	ir_node       *new_eflags = be_transform_node(eflags);
	x86_address_t *addr       = &am.addr;
	ir_node       *new_node   = func(dbgi, block, addr->base, addr->index,
	                                 addr->mem, am.new_op1, am.new_op2,
	                                 new_eflags, am.size);
	set_am_attributes(new_node, &am);
	/* we can't use source address mode anymore when using immediates */
	if (!(flags & match_am_and_immediates) &&
	    (is_ia32_Immediate(am.new_op1) || is_ia32_Immediate(am.new_op2)))
		set_ia32_am_support(new_node, ia32_am_none);

	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

static ir_node *skip_float_upconv(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	assert(mode_is_float(mode));

	while (is_Conv(node)) {
		ir_node *pred      = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);

		/**
		 * suboptimal, but without this check the address mode matcher
		 * can incorrectly think that something has only 1 user
		 */
		if (get_irn_n_edges(node) > 1)
			break;

		if (!mode_is_float(pred_mode)
			|| get_mode_size_bits(pred_mode) > get_mode_size_bits(mode))
			break;
		node = pred;
		mode = pred_mode;
	}
	return node;
}

static void check_x87_floatmode(ir_mode *mode)
{
	if (mode != x86_mode_E)
		panic("x87 only supports x86 extended float mode");
}

/**
 * Construct a standard binary operation, set AM and immediate if required.
 *
 * @param op1   The first operand
 * @param op2   The second operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_binop_x87_float(ir_node *node, ir_node *op1, ir_node *op2,
                                    construct_binop_float_func *func)
{
	/* All operations are considered commutative, because there are reverse
	 * variants */
	match_flags_t  flags = match_commutative | match_am;
	ir_mode       *mode  = is_Div(node) ? get_Div_resmode(node)
	                                    : get_irn_mode(node);
	check_x87_floatmode(mode);

	op1 = skip_float_upconv(op1);
	op2 = skip_float_upconv(op2);

	ir_node *block = get_nodes_block(node);
	ia32_address_mode_t am;
	match_arguments(&am, block, op1, op2, NULL, flags);

	dbg_info      *dbgi      = get_irn_dbg_info(node);
	ir_node       *new_block = be_transform_node(block);
	ir_graph      *irg       = get_irn_irg(node);
	ir_node       *fpcw      = get_initial_fpcw(irg);
	x86_address_t *addr      = &am.addr;
	if (am.op_type == ia32_Normal)
		am.size = X86_SIZE_80;
	ir_node       *new_node  = func(dbgi, new_block, addr->base, addr->index,
	                                addr->mem, am.new_op1, am.new_op2, fpcw,
	                                am.size);
	set_am_attributes(new_node, &am);

	ia32_x87_attr_t *attr = get_ia32_x87_attr(new_node);
	attr->attr.ins_permuted = am.ins_permuted;

	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

static ir_node *skip_shift_amount_conv(ir_node *n)
{
	/* The shift amount can be any mode that is bigger than 5 bits, since all
	 * other bits are ignored anyway. */
	while (is_Conv(n) && get_irn_n_edges(n) == 1) {
		ir_node *const op   = get_Conv_op(n);
		ir_mode *const mode = get_irn_mode(op);
		if (!be_mode_needs_gp_reg(mode))
			break;
		assert(get_mode_size_bits(mode) >= 5);
		n = op;
	}
	return n;
}

/**
 * Construct a shift/rotate binary operation, sets AM and immediate if required.
 *
 * @param op1    The first operand
 * @param op2    The second operand
 * @param func   The node constructor function
 * @param func8  The node constructor function for an 8 bit operation
 * @return The constructed ia32 node.
 */
static ir_node *gen_shift_binop(ir_node *const node, ir_node *op1, ir_node *op2, construct_shift_func *const func, construct_shift_func *const func8, match_flags_t const flags)
{
	assert((flags & ~(match_mode_neutral | match_sign_ext | match_zero_ext)) == 0);

	ir_mode *const mode = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 32) {
		/* TODO: implement special cases for non-modulo shifts */
		panic("modulo shift!=32 not supported by ia32 backend");
	}

	x86_insn_size_t size = x86_size_from_mode(mode);
	ir_node *new_op1;
	if (flags & match_mode_neutral) {
		op1     = be_skip_downconv(op1, true);
		new_op1 = be_transform_node(op1);
		size    = X86_SIZE_32;
	} else {
		op1 = be_skip_sameconv(op1);
		if (size == X86_SIZE_32) {
			new_op1 = be_transform_node(op1);
		} else if (flags & match_sign_ext) {
			new_op1 = transform_sext(op1);
			size    = X86_SIZE_32;
		} else if (flags & match_zero_ext) {
			new_op1 = transform_zext(op1);
			size    = X86_SIZE_32;
		} else {
			new_op1 = be_transform_node(op1);
		}
	}

	op2 = skip_shift_amount_conv(op2);
	ir_node *new_op2 = create_immediate_or_transform(op2, 'I');

	dbg_info             *const dbgi      = get_irn_dbg_info(node);
	ir_node              *const new_block = be_transform_nodes_block(node);
	construct_shift_func *const cons      = size == X86_SIZE_8 ? func8 : func;
	return cons(dbgi, new_block, new_op1, new_op2, size);
}

/**
 * Construct a standard unary operation, set AM and immediate if required.
 *
 * @param op    The operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_unop(ir_node *node, ir_node *op, construct_unop_func *func,
                         match_flags_t flags)
{
	assert(flags == 0 || flags == match_mode_neutral);
	if (flags & match_mode_neutral)
		op = be_skip_downconv(op, true);

	ir_node  *new_op    = be_transform_node(op);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	return func(dbgi, new_block, new_op, X86_SIZE_32);
}

static ir_node *create_lea(dbg_info *const dbgi, ir_node *const block, ir_node *const base, ir_node *const idx, unsigned const log_scale, int32_t const offset)
{
	ir_node *const lea = new_bd_ia32_Lea(dbgi, block, base, idx);
	ia32_attr_t *const attr = get_ia32_attr(lea);
	attr->addr.immediate.offset = offset;
	attr->addr.log_scale        = log_scale;
	attr->addr.variant          = idx != noreg_GP ? X86_ADDR_BASE_INDEX: X86_ADDR_BASE;
	return lea;
}

static ir_node *create_lea_from_address(dbg_info *dbgi, ir_node *block,
                                        x86_address_t *addr)
{
	ir_node       *base = transform_else_noreg(addr->base);
	ir_node *const idx  = transform_else_noreg(addr->index);

	/* segment overrides are ineffective for Leas :-( so we have to patch
	 * around... */
	if (addr->tls_segment) {
		ir_node *tls_base = new_bd_ia32_LdTls(NULL, block);
		assert(addr->imm.entity != NULL);
		if (base == noreg_GP) {
			base = tls_base;
			addr->variant = addr->variant == X86_ADDR_INDEX
			              ? X86_ADDR_BASE_INDEX : X86_ADDR_BASE;
		} else {
			base = create_lea(dbgi, block, tls_base, base, 0, 0);
		}
		addr->tls_segment = false;
	}

	ir_node *res = new_bd_ia32_Lea(dbgi, block, base, idx);
	set_address(res, addr);

	return res;
}

/**
 * Returns non-zero if a given address mode has a symbolic or
 * numerical offset != 0.
 */
static bool am_has_immediates(const x86_address_t *addr)
{
	return addr->imm.offset != 0 || addr->imm.entity != NULL
		|| addr->imm.kind == X86_IMM_FRAMEENT;
}

typedef ir_node* (*new_shiftd_func)(dbg_info *dbgi, ir_node *block,
                                    ir_node *high, ir_node *low,
                                    ir_node *count);

/**
 * Transforms a l_ShlD/l_ShrD into a ShlD/ShrD. Those nodes have 3 data inputs:
 * op1 - target to be shifted
 * op2 - contains bits to be shifted into target
 * op3 - shift count
 * Only op3 can be an immediate.
 */
static ir_node *gen_64bit_shifts(ir_node *const node, ir_node *const high, ir_node *const low, ir_node *count, new_shiftd_func const func)
{
	dbg_info *const dbgi     = get_irn_dbg_info(node);
	ir_node  *const block    = be_transform_nodes_block(node);
	ir_node  *const new_high = be_transform_node(high);
	ir_node  *const new_low  = be_transform_node(low);

	count = skip_shift_amount_conv(count);
	ir_node *new_count = create_immediate_or_transform(count, 'I');

	ir_node *const new_node = func(dbgi, block, new_high, new_low, new_count);
	return new_node;
}

/**
 * Tests whether 2 values result in 'x' and '32-x' when interpreted as a shift
 * value.
 */
static bool is_complementary_shifts(ir_node *value1, ir_node *value2)
{
	if (is_Const(value1) && is_Const(value2)) {
		long const v1 = get_Const_long(value1);
		long const v2 = get_Const_long(value2);
		return v2 == 32 - v1;
	}
	return false;
}

static ir_node *match_64bit_shift(ir_node *node)
{
	ir_node *op1 = get_binop_left(node);
	ir_node *op2 = get_binop_right(node);
	assert(is_Or(node) || is_Add(node));

	if (is_Shr(op1)) {
		ir_node *tmp = op1;
		op1 = op2;
		op2 = tmp;
	}

	/* match ShlD operation */
	if (is_Shl(op1) && is_Shr(op2)) {
		ir_node *shl_right = get_Shl_right(op1);
		ir_node *shl_left  = get_Shl_left(op1);
		ir_node *shr_right = get_Shr_right(op2);
		ir_node *shr_left  = get_Shr_left(op2);
		/* constant ShlD operation */
		if (is_complementary_shifts(shl_right, shr_right))
			return gen_64bit_shifts(node, shl_left, shr_left, shl_right, new_bd_ia32_ShlD_imm);
		/* lower_dw produces the following for ShlD:
		 * Or(Shr(Shr(high,1),Not(c)),Shl(low,c)) */
		if (is_Shr(shr_left) && is_Not(shr_right)
			&& is_irn_one(get_Shr_right(shr_left))
		    && get_Not_op(shr_right) == shl_right) {
			ir_node *val_h = get_Shr_left(shr_left);
			return gen_64bit_shifts(node, shl_left, val_h, shl_right, new_bd_ia32_ShlD);
		}
		/* lower_dw produces the following for ShrD:
		 * Or(Shl(Shl(high,1),Not(c)), Shr(low,c)) */
		if (is_Shl(shl_left) && is_Not(shl_right)
		    && is_irn_one(get_Shl_right(shl_left))
		    && get_Not_op(shl_right) == shr_right) {
			ir_node *val_h = get_Shl_left(shl_left);
			return gen_64bit_shifts(node, shr_left, val_h, shr_right, new_bd_ia32_ShrD);
		}
	}

	return NULL;
}

static ir_node *gen_Rol(ir_node *node, ir_node *op1, ir_node *op2)
{
	return gen_shift_binop(node, op1, op2, &new_bd_ia32_Rol, &new_bd_ia32_Rol_8bit, match_none);
}

static ir_node *gen_Ror(ir_node *node, ir_node *op1, ir_node *op2)
{
	return gen_shift_binop(node, op1, op2, &new_bd_ia32_Ror, &new_bd_ia32_Ror_8bit, match_none);
}

/**
 * Creates an ia32 Add.
 *
 * @return the created ia32 Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	ir_mode  *mode = get_irn_mode(node);
	ir_node  *op1  = get_Add_left(node);
	ir_node  *op2  = get_Add_right(node);

	ir_node *rot_left;
	ir_node *rot_right;
	if (be_pattern_is_rotl(node, &rot_left, &rot_right)) {
		if (is_Minus(rot_right))
			return gen_Ror(node, rot_left, get_Minus_op(rot_right));
		return gen_Rol(node, rot_left, rot_right);
	}

	ir_node *const shift64 = match_64bit_shift(node);
	if (shift64)
		return shift64;

	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2)
			return gen_binop(node, op1, op2, new_bd_ia32_Adds,
			                 match_commutative | match_am);
		else
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fadd);
	}

	x86_mark_non_am(node);

	/**
	 * Rules for an Add:
	 *   0. Immediate Trees (example Add(Address, Const) -> Const)
	 *   1. Add with immediate -> Lea
	 *   2. Add with possible source address mode -> Add
	 *   3. Otherwise -> Lea
	 */
	x86_address_t addr;
	ia32_create_address_mode(&addr, node, x86_create_am_force);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);

	/* a constant? */
	if (!addr.base && !addr.index)
		return new_bd_ia32_Const(dbgi, new_block, &addr.imm);
	/* add with immediate? */
	ir_node *add_immediate_op = NULL;
	if (addr.index == NULL) {
		add_immediate_op = addr.base;
	} else if (addr.base == NULL && addr.scale == 0) {
		add_immediate_op = addr.index;
	}

	if (add_immediate_op != NULL) {
		if (!am_has_immediates(&addr)) {
#ifdef DEBUG_libfirm
			be_warningf(node, "found unoptimized Add x,0");
#endif
			return be_transform_node(add_immediate_op);
		}

		return create_lea_from_address(dbgi, new_block, &addr);
	}

	/* test if we can use source address mode */
	ia32_address_mode_t am;
	match_arguments(&am, block, op1, op2, NULL, match_commutative
	                | match_mode_neutral | match_am | match_immediate | match_try_am);

	/* construct an Add with source address mode */
	if (am.op_type == ia32_AddrModeS)
		return make_binop(node, &am, new_bd_ia32_Add);

	/* otherwise construct a lea */
	return create_lea_from_address(dbgi, new_block, &addr);
}

/**
 * Creates an ia32 Mul.
 *
 * @return the created ia32 Mul node
 */
static ir_node *gen_Mul(ir_node *node)
{
	ir_node *op1  = get_Mul_left(node);
	ir_node *op2  = get_Mul_right(node);
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2)
			return gen_binop(node, op1, op2, new_bd_ia32_Muls,
			                 match_commutative | match_am);
		else
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fmul);
	}

	ir_node      *const block = get_nodes_block(node);
	ia32_address_mode_t am;
	match_arguments(&am, block, op1, op2, NULL, match_commutative | match_am | match_mode_neutral | match_immediate | match_am_and_immediates);

	construct_binop_func *const func = is_ia32_Immediate(am.new_op2) ? new_bd_ia32_IMulImm : new_bd_ia32_IMul;
	return make_binop(node, &am, func);
}

/**
 * Creates an ia32 Mulh.
 * Note: Mul produces a 64Bit result and Mulh returns the upper 32 bit of
 * this result while Mul returns the lower 32 bit.
 *
 * @return the created ia32 Mulh node
 */
static ir_node *gen_Mulh(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (get_mode_size_bits(mode) != 32) {
		panic("Mulh without 32bit size not supported in ia32 backend (%+F)",
		      node);
	}

	ir_node  *op1  = get_Mulh_left(node);
	ir_node  *op2  = get_Mulh_right(node);
	ir_node  *proj_res_high;
	if (mode_is_signed(mode)) {
		ir_node *new_node = gen_binop(node, op1, op2, new_bd_ia32_IMul1OP,
		                              match_commutative | match_am);
		proj_res_high = be_new_Proj(new_node, pn_ia32_IMul1OP_res_high);
	} else {
		ir_node *new_node = gen_binop(node, op1, op2, new_bd_ia32_Mul,
		                              match_commutative | match_am);
		proj_res_high = be_new_Proj(new_node, pn_ia32_Mul_res_high);
	}
	return proj_res_high;
}

static bool is_Shl_1(ir_node *const node)
{
	return is_Shl(node) && is_irn_one(get_Shl_left(node));
}

/**
 * Creates an ia32 And.
 *
 * @return The created ia32 And node
 */
static ir_node *gen_And(ir_node *node)
{
	ir_node *op1 = get_And_left(node);
	ir_node *op2 = get_And_right(node);
	assert(!mode_is_float(get_irn_mode(node)));

	/* is it a zero extension? */
	if (is_Const(op2)) {
		long const v = get_Const_long(op2);
		if (v == 0xFF || v == 0xFFFF) {
			ir_mode *src_mode;
			if (v == 0xFF) {
				src_mode = mode_Bu;
			} else {
				assert(v == 0xFFFF);
				src_mode = mode_Hu;
			}
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = get_nodes_block(node);
			return create_I2I_Conv(src_mode, dbgi, block, op1);
		}
	}
	return gen_binop(node, op1, op2, new_bd_ia32_And,
	                 match_commutative | match_mode_neutral | match_am
	                 | match_immediate);
}

static ir_node *gen_Or(ir_node *node)
{
	ir_node *rot_left;
	ir_node *rot_right;
	if (be_pattern_is_rotl(node, &rot_left, &rot_right)) {
		if (is_Minus(rot_right))
			return gen_Ror(node, rot_left, get_Minus_op(rot_right));
		return gen_Rol(node, rot_left, rot_right);
	}

	ir_node *res = match_64bit_shift(node);
	if (res != NULL)
		return res;

	ir_node *op1 = get_Or_left(node);
	ir_node *op2 = get_Or_right(node);
	return gen_binop(node, op1, op2, new_bd_ia32_Or,
	                 match_commutative | match_mode_neutral
	                 | match_am | match_immediate);
}

/**
 * Creates an ia32 Eor.
 *
 * @return The created ia32 Eor node
 */
static ir_node *gen_Eor(ir_node *node)
{
	assert(!mode_is_float(get_irn_mode(node)));
	ir_node *op1 = get_Eor_left(node);
	ir_node *op2 = get_Eor_right(node);
	return gen_binop(node, op1, op2, new_bd_ia32_Xor, match_commutative
	                 | match_mode_neutral | match_am | match_immediate);
}

/**
 * Creates an ia32 Sub.
 *
 * @return The created ia32 Sub node
 */
static ir_node *gen_Sub(ir_node *node)
{
	ir_node *op1  = get_Sub_left(node);
	ir_node *op2  = get_Sub_right(node);
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2)
			return gen_binop(node, op1, op2, new_bd_ia32_Subs, match_am);
		else
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fsub);
	}

	if (is_Const(op2))
		be_warningf(node, "found unoptimized Sub with Const");

	ir_node *ia32_sub = gen_binop(node, op1, op2, new_bd_ia32_Sub, match_mode_neutral
	                              | match_am | match_immediate);

	/* A Cmp node that has the same operands as this Sub will use
	 * this Sub's flags result. To be prepared for that, we change
	 * every Sub's mode to mode_T. If gen_binop has produced a Sub
	 * with source address mode, this has already happened and
	 * get_binop returns a Proj rather than a Sub. */
	if (is_ia32_Sub(ia32_sub)) {
		set_irn_mode(ia32_sub, mode_T);
		return be_new_Proj(ia32_sub, pn_ia32_Sub_res);
	} else {
		/* ia32_sub is actually a Proj, the Sub itself is
		 * already in mode_T. */
		assert(is_Proj(ia32_sub));
		return ia32_sub;
	}
}

static ir_node *transform_AM_mem(ir_node *const block,
                                 ir_node *const src_val,
                                 ir_node *const src_mem,
                                 ir_node *const am_mem)
{
	if (is_NoMem(am_mem)) {
		return be_transform_node(src_mem);
	} else if (is_Proj(src_val) &&
	           is_Proj(src_mem) &&
	           get_Proj_pred(src_val) == get_Proj_pred(src_mem)) {
		/* avoid memory loop */
		return am_mem;
	} else if (is_Proj(src_val) && is_Sync(src_mem)) {
		int const arity = get_Sync_n_preds(src_mem);

		ir_node **ins = ALLOCAN(ir_node*, arity + 1);

		/* NOTE: This sometimes produces dead-code because the old sync in
		 * src_mem might not be used anymore, we should detect this case
		 * and kill the sync... */
		ir_node *const ptr_pred = get_Proj_pred(src_val);
		int            n        = 0;
		for (int i = arity - 1; i >= 0; --i) {
			ir_node *const pred = get_Sync_pred(src_mem, i);

			/* avoid memory loop */
			if (is_Proj(pred) && get_Proj_pred(pred) == ptr_pred)
				continue;

			ins[n++] = be_transform_node(pred);
		}

		if (n == 1 && ins[0] == am_mem) {
			/* creating a new Sync and relying on CSE may fail,
			 * if am_mem is a ProjM, which does not yet verify. */
			return am_mem;
		}

		ins[n++] = am_mem;
		return new_r_Sync(block, n, ins);
	} else {
		ir_node *ins[2] = { be_transform_node(src_mem), am_mem };
		return new_r_Sync(block, 2, ins);
	}
}

/**
 * Create a 32bit to 64bit signed extension.
 *
 * @param dbgi   debug info
 * @param block  the block where node nodes should be placed
 * @param val    the value to extend
 * @param orig   the original node
 */
static ir_node *create_sex_32_64(dbg_info *const dbgi, ir_node *const block, ir_node *const val)
{
	if (ia32_cg_config.use_short_sex_eax) {
		return new_bd_ia32_Cltd(dbgi, block, val);
	} else {
		ir_graph *const irg   = get_irn_irg(block);
		ir_node  *const imm31 = ia32_create_Immediate(irg, 31);
		return new_bd_ia32_Sar(dbgi, block, val, imm31, X86_SIZE_32);
	}
}

/**
 * Generates an ia32 Div with additional infrastructure for the
 * register allocator if needed.
 */
static ir_node *create_Div(ir_node *const node, ir_node *const op1, ir_node *const op2, ir_node *const mem, ir_mode *const mode)
{
	/* the upper bits have random contents for smaller modes */
	ia32_address_mode_t  am;
	ir_node       *const mem_pin_skip = skip_Pin(mem);
	ir_node       *const old_block    = get_nodes_block(node);
	bool           const is_signed    = mode_is_signed(mode);

	/* Prevent AM in case of Pin(Sync(...)) since the Pin applies to all
	 * memory partitions (aka Sync operands), and not only the consumed one.
	 * Thus, we would lose dependencies when folding AM. */
	match_flags_t const flags = (is_Pin(mem) && is_Sync(mem_pin_skip) ? match_none : match_am) | (is_signed ? match_sign_ext : match_zero_ext);
	match_arguments(&am, old_block, op1, op2, mem_pin_skip, flags);

	/* Beware: We don't need a Sync, if the memory predecessor of the Div node
	 * is the memory of the consumed address. We can have only the second op as
	 * address in Div nodes, so check only op2. */
	ir_node       *const op2_skip = be_skip_sameconv(op2);
	ir_node       *const block    = be_transform_node(old_block);
	x86_address_t *const addr     = &am.addr;
	ir_node       *const new_mem  = transform_AM_mem(block, op2_skip, mem_pin_skip, addr->mem);

	dbg_info *const dbgi = get_irn_dbg_info(node);
	ir_node        *ext;
	ir_node      *(*cons)(dbg_info *db, ir_node *block, ir_node *base, ir_node *index, ir_node *mem, ir_node *op1, ir_node *op2, ir_node *ext, x86_insn_size_t size);
	if (is_signed) {
		ext  = create_sex_32_64(dbgi, block, am.new_op1);
		cons = new_bd_ia32_IDiv;
	} else {
		x86_imm32_t imm = { .offset = 0 };
		ext  = new_bd_ia32_Const(dbgi, block, &imm);
		cons = new_bd_ia32_Div;
	}
	ir_node *const new_node = cons(dbgi, block, addr->base, addr->index, new_mem, am.new_op2, am.new_op1, ext, X86_SIZE_32);

	ir_set_throws_exception(new_node, ir_throws_exception(node));
	set_irn_pinned(new_node, get_irn_pinned(node));
	set_am_attributes(new_node, &am);
	return fix_mem_proj(new_node, &am);
}

/**
 * Generates an ia32 Mod.
 */
static ir_node *gen_Mod(ir_node *node)
{
	ir_node *const op1  = get_Mod_left(node);
	ir_node *const op2  = get_Mod_right(node);
	ir_node *const mem  = get_Mod_mem(node);
	ir_mode *const mode = get_Mod_resmode(node);
	return create_Div(node, op1, op2, mem, mode);
}

/**
 * Generates an ia32 Div.
 */
static ir_node *gen_Div(ir_node *node)
{
	ir_node *const op1  = get_Div_left(node);
	ir_node *const op2  = get_Div_right(node);
	ir_mode *const mode = get_Div_resmode(node);
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			return gen_binop(node, op1, op2, new_bd_ia32_Divs, match_am);
		} else {
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fdiv);
		}
	}

	ir_node *const mem = get_Div_mem(node);
	return create_Div(node, op1, op2, mem, mode);
}

/**
 * Creates an ia32 Shl.
 *
 * @return The created ia32 Shl node
 */
static ir_node *gen_Shl(ir_node *node)
{
	ir_node *left  = get_Shl_left(node);
	ir_node *right = get_Shl_right(node);

	/* special case Shl x,1 => Lea x,x because Lea has fewer register
	 * constraints */
	if (is_irn_one(right)) {
		dbg_info *dbgi      = get_irn_dbg_info(node);
		ir_node  *new_block = be_transform_nodes_block(node);
		ir_node  *new_left  = be_transform_node(left);
		return create_lea(dbgi, new_block, new_left, new_left, 0, 0);
	}

	return gen_shift_binop(node, left, right, &new_bd_ia32_Shl, &new_bd_ia32_Shl_8bit, match_mode_neutral);
}

/**
 * Creates an ia32 Shr.
 *
 * @return The created ia32 Shr node
 */
static ir_node *gen_Shr(ir_node *node)
{
	ir_node *left  = get_Shr_left(node);
	ir_node *right = get_Shr_right(node);

	return gen_shift_binop(node, left, right, &new_bd_ia32_Shr, &new_bd_ia32_Shr_8bit, match_zero_ext);
}

/**
 * Creates an ia32 Sar.
 *
 * @return The created ia32 Shrs node
 */
static ir_node *gen_Shrs(ir_node *node)
{
	ir_node *left  = be_skip_sameconv(get_Shrs_left(node));
	ir_node *right = get_Shrs_right(node);

	if (is_Const(right)) {
		long const val = get_Const_long(right);
		if (val == 31 && get_irn_n_edges(left) > 1) {
			/* this is a sign extension */
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = be_transform_nodes_block(node);
			ir_node  *new_op = be_transform_node(left);

			return create_sex_32_64(dbgi, block, new_op);
		}
	}

	/* 8 or 16 bit sign extension? */
	if (is_Const(right) && is_Shl(left)) {
		ir_node *shl_left  = get_Shl_left(left);
		ir_node *shl_right = get_Shl_right(left);
		if (is_Const(shl_right)) {
			ir_tarval *tv1 = get_Const_tarval(right);
			ir_tarval *tv2 = get_Const_tarval(shl_right);
			if (tv1 == tv2 && tarval_is_long(tv1)) {
				long val = get_tarval_long(tv1);
				if (val == 16 || val == 24) {
					dbg_info *dbgi  = get_irn_dbg_info(node);
					ir_node  *block = get_nodes_block(node);

					ir_mode *src_mode;
					if (val == 24) {
						src_mode = mode_Bs;
					} else {
						assert(val == 16);
						src_mode = mode_Hs;
					}
					ir_node *const res = create_I2I_Conv(src_mode, dbgi, block, shl_left);
					/* The Shl might have further users.  If its left operand was folded
					 * into the movsx, then these users must use the movsx and its Projs.
					 * The latter is needed to avoid duplicate Projs. */
					if (!be_is_transformed(shl_left))
						be_set_transformed_node(shl_left, res);
					return res;
				}
			}
		}
	}

	return gen_shift_binop(node, left, right, &new_bd_ia32_Sar, &new_bd_ia32_Sar_8bit, match_sign_ext);
}

/**
 * Transforms a Minus node.
 *
 * @return The created ia32 Minus node
 */
static ir_node *gen_Minus(ir_node *node)
{
	ir_node  *op    = get_Minus_op(node);
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);

	if (mode_is_float(mode)) {
		ir_node *new_op = be_transform_node(op);
		if (ia32_cg_config.use_sse2) {
			/* TODO: non-optimal... if we have many xXors, then we should
			 * rather create a load for the const and use that instead of
			 * several AM nodes... */
			ir_graph       *const irg       = get_irn_irg(block);
			ir_node        *const noreg_xmm = ia32_new_NoReg_xmm(irg);
			ir_node        *const base      = get_global_base(irg);
			x86_insn_size_t const size      = x86_size_from_mode(mode);
			ir_node        *const new_node  = new_bd_ia32_Xorp(dbgi, block, base, noreg_GP, nomem, new_op, noreg_xmm, size);

			ir_entity      *const ent
				= ia32_gen_fp_known_const(size == X86_SIZE_32 ? ia32_SSIGN
				                                              : ia32_DSIGN);

			set_am_const_entity(new_node, ent);
			set_ia32_op_type(new_node, ia32_AddrModeS);
			return new_node;
		} else {
			return new_bd_ia32_fchs(dbgi, block, new_op);
		}
	} else {
		return gen_unop(node, op, new_bd_ia32_Neg, match_mode_neutral);
	}
}

/**
 * Transforms a Not node.
 *
 * @return The created ia32 Not node
 */
static ir_node *gen_Not(ir_node *node)
{
	assert(get_irn_mode(node) != mode_b); /* should be lowered already */
	ir_node *op = get_Not_op(node);

	if (is_Shl_1(op)) {
		/* ~(1 << x) -> Rol(~1, x) */
		dbg_info   *const dbgi    = get_irn_dbg_info(node);
		ir_node    *const block   = be_transform_nodes_block(node);
		x86_imm32_t const imm     = { .offset = -2 };
		ir_node    *const m2      = new_bd_ia32_Const(NULL, block, &imm);
		ir_node    *const val     = get_Shl_right(op);
		ir_node    *const new_val = be_transform_node(val);
		return new_bd_ia32_Rol(dbgi, block, m2, new_val, X86_SIZE_32);
	}

	if (is_Shr(op)) {
		ir_node *const shrl = get_Shr_left(op);
		if (is_Const(shrl) && get_tarval_lowest_bit(get_Const_tarval(shrl)) == 31) {
			/* ~(0x80000000 >> x) -> Ror(~0x80000000, x) */
			dbg_info   *const dbgi    = get_irn_dbg_info(node);
			ir_node    *const block   = be_transform_nodes_block(node);
			x86_imm32_t const imm     = { .offset = 0x7FFFFFFF };
			ir_node    *const m2      = new_bd_ia32_Const(NULL, block, &imm);
			ir_node    *const val     = get_Shr_right(op);
			ir_node    *const new_val = be_transform_node(val);
			return new_bd_ia32_Ror(dbgi, block, m2, new_val, X86_SIZE_32);
		}
	}

	return gen_unop(node, op, new_bd_ia32_Not, match_mode_neutral);
}

static ir_node *create_float_abs(dbg_info *const dbgi, ir_node *const new_block, ir_node *const op, bool const negate)
{
	ir_mode *mode   = get_irn_mode(op);
	ir_node *new_op = be_transform_node(op);
	assert(mode_is_float(mode));

	ir_node *new_node;
	if (ia32_cg_config.use_sse2) {
		ir_graph       *const irg      = get_irn_irg(new_block);
		ir_node        *const noreg_fp = ia32_new_NoReg_xmm(irg);
		ir_node        *const base     = get_global_base(irg);
		x86_insn_size_t const size     = x86_size_from_mode(mode);
		new_node = new_bd_ia32_Andp(dbgi, new_block, base, noreg_GP, nomem,
		                            new_op, noreg_fp, size);

		ir_entity *ent = ia32_gen_fp_known_const(size == X86_SIZE_32
		                                         ? ia32_SABS : ia32_DABS);

		set_am_const_entity(new_node, ent);
		set_ia32_op_type(new_node, ia32_AddrModeS);

		/* TODO, implement -Abs case */
		assert(!negate);
	} else {
		check_x87_floatmode(mode);
		new_node = new_bd_ia32_fabs(dbgi, new_block, new_op);
		if (negate)
			new_node = new_bd_ia32_fchs(dbgi, new_block, new_node);
	}

	return new_node;
}

/**
 * Create a bt instruction for x & (1 << n) and place it into the block of cmp.
 */
static ir_node *gen_bt(ir_node *cmp, ir_node *x, ir_node *n)
{
	dbg_info *dbgi      = get_irn_dbg_info(cmp);
	ir_node  *new_block = be_transform_nodes_block(cmp);
	ir_node  *op1       = be_transform_node(x);
	ir_node  *op2       = be_transform_node(n);

	return new_bd_ia32_Bt(dbgi, new_block, op1, op2, X86_SIZE_32);
}

x86_condition_code_t ir_relation_to_x86_condition_code(ir_relation relation,
                                                       ir_mode *mode,
                                                       bool overflow_possible)
{
	if (mode_is_float(mode)) {
		switch (relation) {
		case ir_relation_equal:              return x86_cc_float_equal;
		case ir_relation_less:               return x86_cc_float_below;
		case ir_relation_less_equal:         return x86_cc_float_below_equal;
		case ir_relation_greater:            return x86_cc_float_above;
		case ir_relation_greater_equal:      return x86_cc_float_above_equal;
		case ir_relation_less_greater:       return x86_cc_not_equal;
		case ir_relation_less_equal_greater: return x86_cc_not_parity;
		case ir_relation_unordered:          return x86_cc_parity;
		case ir_relation_unordered_equal:    return x86_cc_equal;
		case ir_relation_unordered_less:   return x86_cc_float_unordered_below;
		case ir_relation_unordered_less_equal:
		                             return x86_cc_float_unordered_below_equal;
		case ir_relation_unordered_greater:
		                             return x86_cc_float_unordered_above;
		case ir_relation_unordered_greater_equal:
		                             return x86_cc_float_unordered_above_equal;
		case ir_relation_unordered_less_greater:
		                             return x86_cc_float_not_equal;
		case ir_relation_false:
		case ir_relation_true:
			/* should we introduce a jump always/jump never? */
			break;
		}
		panic("unexpected float pnc");
	} else if (mode_is_signed(mode)) {
		switch (relation) {
		case ir_relation_unordered_equal:
		case ir_relation_equal:                return x86_cc_equal;
		case ir_relation_unordered_less:
		case ir_relation_less:
			return overflow_possible ? x86_cc_less : x86_cc_sign;
		case ir_relation_unordered_less_equal:
		case ir_relation_less_equal:           return x86_cc_less_equal;
		case ir_relation_unordered_greater:
		case ir_relation_greater:              return x86_cc_greater;
		case ir_relation_unordered_greater_equal:
		case ir_relation_greater_equal:
			return overflow_possible ? x86_cc_greater_equal : x86_cc_not_sign;
		case ir_relation_unordered_less_greater:
		case ir_relation_less_greater:         return x86_cc_not_equal;
		case ir_relation_less_equal_greater:
		case ir_relation_unordered:
		case ir_relation_false:
		case ir_relation_true:
			/* introduce jump always/jump never? */
			break;
		}
		panic("unexpected pnc");
	} else {
		switch (relation) {
		case ir_relation_unordered_equal:
		case ir_relation_equal:         return x86_cc_equal;
		case ir_relation_unordered_less:
		case ir_relation_less:          return x86_cc_below;
		case ir_relation_unordered_less_equal:
		case ir_relation_less_equal:    return x86_cc_below_equal;
		case ir_relation_unordered_greater:
		case ir_relation_greater:       return x86_cc_above;
		case ir_relation_unordered_greater_equal:
		case ir_relation_greater_equal: return x86_cc_above_equal;
		case ir_relation_unordered_less_greater:
		case ir_relation_less_greater:  return x86_cc_not_equal;
		case ir_relation_less_equal_greater:
		case ir_relation_unordered:
		case ir_relation_false:
		case ir_relation_true:
			/* introduce jump always/jump never? */
			break;
		}
		panic("unexpected pnc");
	}
}

static bool is_bt_relation(ir_relation const relation, ir_mode *const mode)
{
	return
		relation == ir_relation_equal ? true :
		mode_is_signed(mode)          ? relation == ir_relation_less_greater :
		(relation & ir_relation_greater_equal) == ir_relation_greater;
}

static ir_node *get_flags_node(ir_node *cmp, x86_condition_code_t *cc_out)
{
	if (is_Const(cmp)) {
		/* For -O0 and some enabled optimizations,
		 * we might end up with mode_b constants. */
		*cc_out = is_Const_null(cmp) ? x86_cc_above_equal : x86_cc_below;

		dbg_info *dbgi  = get_irn_dbg_info(cmp);
		ir_node  *block = be_transform_nodes_block(cmp);
		return new_bd_ia32_Stc(dbgi, block);
	}

	/* must have a Cmp as input */
	ir_relation relation = get_Cmp_relation(cmp);
	ir_node    *l        = get_Cmp_left(cmp);
	ir_node    *r        = get_Cmp_right(cmp);
	ir_mode    *mode     = get_irn_mode(l);

	/* check for bit-test */
	if (ia32_cg_config.use_bt && is_bt_relation(relation, mode) && is_And(l)) {
		ir_node *la = get_And_left(l);
		ir_node *ra = get_And_right(l);
		if (is_Shl(ra)) {
			ir_node *tmp = la;
			la = ra;
			ra = tmp;
		}
		if (is_Shl(la)) {
			if (is_Shl_1(la) && is_irn_null(r)) {
				/* (1 << n) & ra) */
				ir_node *n     = get_Shl_right(la);
				ir_node *flags = gen_bt(cmp, ra, n);
				/* the bit is copied into the CF flag */
				if (relation & ir_relation_equal)
					*cc_out = x86_cc_above_equal; /* test for CF=0 */
				else
					*cc_out = x86_cc_below;       /* test for CF=1 */
				return flags;
			}
		}
	}

	ir_node *flags = be_transform_node(cmp);

	/* If cmp has merged with a Sub during the transformation, its
	 * relation may have turned around. This is why we can set
	 * *cc_out only now. */
	l        = get_Cmp_left(cmp);
	r        = get_Cmp_right(cmp);
	relation = get_Cmp_relation(cmp);

	/* the middle-end tries to eliminate impossible relations, so a ptr <> 0
	 * test becomes ptr > 0. But for x86 an equal comparison is preferable to
	 * a >0 (we can sometimes eliminate the cmp in favor of flags produced by
	 * a predecessor node). So add the < bit.
	 * (Note that we do not want to produce <=> (which can happen for
	 * unoptimized code), because no x86 flag can represent that */
	if (!(relation & ir_relation_equal) && relation & ir_relation_less_greater)
		relation |= get_negated_relation(ir_get_possible_cmp_relations(l, r)) & ir_relation_less_greater;

	bool const overflow_possible = !is_irn_null(r);

	/* just do a normal transformation of the Cmp */
	*cc_out = ir_relation_to_x86_condition_code(relation, mode,
	                                            overflow_possible);

	return flags;
}

/**
 * Transforms a Load.
 *
 * @return the created ia32 Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	ir_node  *const block   = be_transform_nodes_block(node);
	ir_node  *const ptr     = get_Load_ptr(node);
	ir_node  *const mem     = get_Load_mem(node);
	dbg_info *const dbgi    = get_irn_dbg_info(node);
	ir_mode  *const mode    = get_Load_mode(node);

	/* construct load address */
	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem, x86_create_am_normal);
	ir_node *const base    = addr.base;
	ir_node *const idx     = addr.index;
	ir_node *const new_mem = addr.mem;

	x86_insn_size_t const size = x86_size_from_mode(mode);
	ir_node *new_node;
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			new_node = new_bd_ia32_xLoad(dbgi, block, base, idx, new_mem, size);
		} else {
			new_node = new_bd_ia32_fld(dbgi, block, base, idx, new_mem,
			                           size);
		}
	} else {
		/* create a conv node with address mode for smaller modes */
		if (get_mode_size_bits(mode) < 32) {
			bool sign_extend = mode_is_signed(mode);
			new_node = new_bd_ia32_Conv_I2I(dbgi, block, base, idx, new_mem,
			                                noreg_GP, size, sign_extend);
			/* Conv_I2I is not mode_T by default. */
			set_irn_mode(new_node, mode_T);
		} else {
			new_node = new_bd_ia32_Load(dbgi, block, base, idx, new_mem, size,
			                            false);
		}
	}
	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(new_node, throws_exception);

	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeS);
	set_address(new_node, &addr);

	if (!get_irn_pinned(node)) {
		assert((int)pn_ia32_xLoad_res == (int)pn_ia32_fld_res
		       && (int)pn_ia32_fld_res == (int)pn_ia32_Load_res
		       && (int)pn_ia32_Load_res == (int)pn_ia32_res);
		arch_add_irn_flags(new_node, arch_irn_flag_rematerializable);
	}

	return new_node;
}

static ir_node *use_dest_am(ir_node *block, ir_node *node, ir_node *mem,
                            ir_node *ptr, ir_node *other, match_flags_t flags)
{
	if (!is_Proj(node))
		return NULL;

	/* we only use address mode if we're the only user of the load
	 * or the users will be merged later anyway */
	if (get_irn_n_edges(node) != (flags & match_two_users ? 2 : 1) &&
	    !users_will_merge(node))
		return NULL;

	ir_node *const load = get_Proj_pred(node);
	if (!is_Load(load))
		return NULL;
	if (get_nodes_block(load) != block)
		return NULL;

	/* store should have the same pointer as the load */
	if (get_Load_ptr(load) != ptr)
		return NULL;

	/* don't do AM if other node inputs depend on the load (via mem-proj) */
	if (other != NULL                   &&
	    get_nodes_block(other) == block &&
	    heights_reachable_in_block(heights, other, load)) {
		return NULL;
	}

	if (prevents_AM(block, load, mem))
		return NULL;
	/* Store should be attached to the load via mem */
	assert(heights_reachable_in_block(heights, mem, load));
	return load;
}

static ir_node *start_dest_am(ia32_address_mode_t *const am, ir_node *const block, ir_node *const op, ir_node *const mem)
{
	memset(am, 0, sizeof(*am));
	build_address(am, op, x86_create_am_double_use);
	return transform_AM_mem(block, op, mem, am->addr.mem);
}

static void finish_dest_am(ir_node *const new_node, ir_node *const load, ia32_address_mode_t const *const am)
{
	set_address(new_node, &am->addr);
	set_ia32_op_type(new_node, ia32_AddrModeD);

	/* The Proj M of the Load can have other users.  This might cause the the Load
	 * and its Proj M are transformmed twice -- as part of the RWM operation and
	 * by itself.
	 * Transform the Proj M of the Load and attach it to the RMW operation to
	 * ensure that the Proj M of the Load and the one of the Store are both
	 * transformed into the same Proj M. */
	be_set_transformed_node(load, new_node);
	ir_node *const load_M     = get_Proj_for_pn(load, pn_Load_M);
	ir_node *const new_load_M = be_transform_node(load_M);
	set_Proj_pred(new_load_M, new_node);
}

static ir_node *dest_am_binop(ir_node *node, ir_node *op1, ir_node *op2,
                              ir_node *mem, ir_node *ptr, x86_insn_size_t size,
                              construct_binop_dest_func *func,
                              construct_binop_dest_func *func8bit,
                              match_flags_t flags, const char imm_mode)
{
	assert(flags & match_immediate); /* there is no destam node without... */

	ir_node *const src_block = get_nodes_block(node);
	ir_node       *load      = use_dest_am(src_block, op1, mem, ptr, op2, flags);
	if (!load) {
		if (!(flags & match_commutative))
			return NULL;
		ir_node *const tmp = op1;
		op1 = op2;
		op2 = tmp;
		load = use_dest_am(src_block, op1, mem, ptr, op2, flags);
		if (!load)
			return NULL;
	}

	ia32_address_mode_t              am;
	ir_node                   *const new_op   = create_immediate_or_transform(op2, imm_mode);
	ir_node                   *const block    = be_transform_node(src_block);
	ir_node                   *const new_mem  = start_dest_am(&am, block, op1, mem);
	dbg_info                  *const dbgi     = get_irn_dbg_info(node);
	construct_binop_dest_func *const cons     = size == X86_SIZE_8 ? func8bit : func;
	ir_node                   *const new_node = cons(dbgi, block, am.addr.base, am.addr.index, new_mem, new_op, size);
	finish_dest_am(new_node, load, &am);
	be_set_transformed_node(node, new_node);
	return new_node;
}

static ir_node *dest_am_unop(ir_node *node, ir_node *op, ir_node *mem,
                             ir_node *ptr, x86_insn_size_t const size,
                             construct_unop_dest_func *func)
{
	ir_node *const src_block = get_nodes_block(node);
	ir_node *const load      = use_dest_am(src_block, op, mem, ptr, NULL, match_none);
	if (!load)
		return NULL;

	ia32_address_mode_t am;
	ir_node      *const block    = be_transform_node(src_block);
	ir_node      *const new_mem  = start_dest_am(&am, block, op, mem);
	dbg_info     *const dbgi     = get_irn_dbg_info(node);
	ir_node      *const new_node = func(dbgi, block, am.addr.base,
	                                    am.addr.index, new_mem, size);
	finish_dest_am(new_node, load, &am);
	return new_node;
}

static ir_node *try_create_SetMem(ir_node *node, ir_node *ptr, ir_node *mem)
{
	ir_mode        *const mode = get_irn_mode(node);
	x86_insn_size_t const size = x86_size_from_mode(mode);
	if (size != X86_SIZE_8)
		return NULL;

	bool           negated;
	ir_node *const mux_true  = get_Mux_true(node);
	ir_node *const mux_false = get_Mux_false(node);
	if (is_irn_one(mux_true) && is_irn_null(mux_false)) {
		negated = false;
	} else if (is_irn_null(mux_true) && is_irn_one(mux_false)) {
		negated = true;
	} else {
		return NULL;
	}

	x86_condition_code_t cc;
	ir_node       *const cond  = get_Mux_sel(node);
	ir_node       *const flags = get_flags_node(cond, &cc);
	/* we can't handle the float special cases with SetM */
	if (cc & x86_cc_additional_float_cases)
		return NULL;
	if (negated)
		cc = x86_negate_condition_code(cc);

	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem, x86_create_am_normal);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *new_node  = new_bd_ia32_SetccMem(dbgi, new_block, addr.base,
	                                           addr.index, addr.mem, flags, cc);
	set_address(new_node, &addr);
	set_ia32_op_type(new_node, ia32_AddrModeD);
	return new_node;
}

static ir_node *try_create_dest_am(ir_node *node)
{
	ir_node *val  = get_Store_value(node);
	ir_mode *mode = get_irn_mode(val);

	/* handle only GP modes for now... */
	if (!be_mode_needs_gp_reg(mode))
		return NULL;

	/* store must be the only user of the val node */
	if (get_irn_n_edges(val) > 1)
		return NULL;

	/* value must be in the same block */
	if (get_nodes_block(node) != get_nodes_block(val))
		return NULL;

	x86_insn_size_t const size = x86_size_from_mode(mode);
	ir_node *ptr  = get_Store_ptr(node);
	ir_node *mem  = get_Store_mem(node);
	ir_node *new_node;
	switch (get_irn_opcode(val)) {
	case iro_Add: {
		ir_node *rot_left;
		ir_node *rot_right;
		if (be_pattern_is_rotl(val, &rot_left, &rot_right)) {
			if (is_Minus(rot_right)) {
				rot_right = get_Minus_op(rot_right);
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         size, new_bd_ia32_RorMem,
				                         new_bd_ia32_RorMem,
				                         match_immediate | match_two_users, 'i');
			} else {
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         size, new_bd_ia32_RolMem,
				                         new_bd_ia32_RolMem,
				                         match_immediate | match_two_users, 'i');
			}
			break;
		}

		ir_node *op1 = get_Add_left(val);
		ir_node *op2 = get_Add_right(val);
		if (ia32_cg_config.use_incdec) {
			if (is_irn_one(op2)) {
				new_node = dest_am_unop(val, op1, mem, ptr, size,
				                        new_bd_ia32_IncMem);
				break;
			} else if (is_Const_Minus_1(op2)) {
				new_node = dest_am_unop(val, op1, mem, ptr, size,
				                        new_bd_ia32_DecMem);
				break;
			}
		}
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_AddMem, new_bd_ia32_AddMem_8bit,
		                         match_commutative | match_immediate, 'i');
		break;
	}
	case iro_Sub: {
		ir_node *op1 = get_Sub_left(val);
		ir_node *op2 = get_Sub_right(val);
		if (is_Const(op2))
			be_warningf(val, "found unoptimized Sub with Const");
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_SubMem, new_bd_ia32_SubMem_8bit,
		                         match_immediate, 'i');
		break;
	}
	case iro_And: {
		ir_node *op1 = get_And_left(val);
		ir_node *op2 = get_And_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_AndMem, new_bd_ia32_AndMem_8bit,
		                         match_commutative | match_immediate, 'i');
		break;
	}
	case iro_Or: {
		ir_node *rot_left;
		ir_node *rot_right;
		if (be_pattern_is_rotl(val, &rot_left, &rot_right)) {
			if (is_Minus(rot_right)) {
				rot_right = get_Minus_op(rot_right);
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         size, new_bd_ia32_RorMem,
				                         new_bd_ia32_RorMem,
				                         match_immediate | match_two_users, 'i');
			} else {
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         size, new_bd_ia32_RolMem,
				                         new_bd_ia32_RolMem,
				                         match_immediate | match_two_users, 'i');
			}
			break;
		}

		ir_node *op1 = get_Or_left(val);
		ir_node *op2 = get_Or_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_OrMem, new_bd_ia32_OrMem_8bit,
		                         match_commutative | match_immediate, 'i');
		break;
	}
	case iro_Eor: {
		ir_node *op1 = get_Eor_left(val);
		ir_node *op2 = get_Eor_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_XorMem, new_bd_ia32_XorMem_8bit,
		                         match_commutative | match_immediate, 'i');
		break;
	}
	case iro_Shl: {
		ir_node *op1 = get_Shl_left(val);
		ir_node *op2 = get_Shl_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_ShlMem, new_bd_ia32_ShlMem,
		                         match_immediate, 'I');
		break;
	}
	case iro_Shr: {
		ir_node *op1 = get_Shr_left(val);
		ir_node *op2 = get_Shr_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_ShrMem, new_bd_ia32_ShrMem,
		                         match_immediate, 'I');
		break;
	}
	case iro_Shrs: {
		ir_node *op1 = get_Shrs_left(val);
		ir_node *op2 = get_Shrs_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, size,
		                         new_bd_ia32_SarMem, new_bd_ia32_SarMem,
		                         match_immediate, 'I');
		break;
	}
	case iro_Mux:
		new_node = try_create_SetMem(val, ptr, mem);
		break;
	case iro_Minus: {
		ir_node *op1 = get_Minus_op(val);
		new_node = dest_am_unop(val, op1, mem, ptr, size, new_bd_ia32_NegMem);
		break;
	}
	case iro_Not: {
		/* should be lowered already */
		ir_node *op1 = get_Not_op(val);
		new_node = dest_am_unop(val, op1, mem, ptr, size, new_bd_ia32_NotMem);
		break;
	}
	default:
		return NULL;
	}

	if (new_node != NULL && !get_irn_pinned(new_node) && get_irn_pinned(node))
		set_irn_pinned(new_node, true);

	return new_node;
}

static bool possible_int_mode_for_fp(ir_mode *mode)
{
	if (!mode_is_signed(mode))
		return false;
	unsigned size = get_mode_size_bits(mode);
	if (size != 16 && size != 32)
		return false;
	return true;
}

static bool is_float_to_int_conv(const ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (!possible_int_mode_for_fp(mode))
		return false;

	if (!is_Conv(node))
		return false;
	ir_node *conv_op   = get_Conv_op(node);
	ir_mode *conv_mode = get_irn_mode(conv_op);

	if (!mode_is_float(conv_mode))
		return false;

	return true;
}

/**
 * Transform a Store(floatConst) into a sequence of
 * integer stores.
 *
 * @return the created ia32 Store node
 */
static ir_node *gen_float_const_Store(ir_node *node, ir_node *cns)
{
	ir_mode   *cns_mode   = get_irn_mode(cns);
	unsigned   size_bytes = get_mode_size_bytes(cns_mode);
	ir_tarval *tv         = get_Const_tarval(cns);
	ir_node   *new_block  = be_transform_nodes_block(node);
	ir_node   *ptr        = get_Store_ptr(node);
	ir_node   *mem        = get_Store_mem(node);
	dbg_info  *dbgi       = get_irn_dbg_info(node);
	int        ofs        = 0;
	int        i          = 0;
	ir_node   *ins[4];

	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem, x86_create_am_normal);

	do {
		unsigned        val;
		unsigned        delta;
		x86_insn_size_t size;
		if (size_bytes >= 4) {
			val   = be_get_tv_bits32(tv, ofs);
			delta = 4;
			size  = X86_SIZE_32;
		} else if (size_bytes >= 2) {
			val   = get_tarval_sub_bits(tv, ofs)
			        | (get_tarval_sub_bits(tv, ofs + 1) <<  8);
			delta = 2;
			size  = X86_SIZE_16;
		} else {
			panic("invalid size of Store float to mem (%+F)", node);
		}
		ir_graph *const irg = get_irn_irg(new_block);
		ir_node  *const imm = ia32_create_Immediate(irg, val);

		ir_node *const new_node
			= new_bd_ia32_Store(dbgi, new_block, addr.base, addr.index,
			                    addr.mem, imm, size);
		ir_node *const new_mem = be_new_Proj(new_node, pn_ia32_Store_M);

		int throws_exception = ir_throws_exception(node);
		ir_set_throws_exception(new_node, throws_exception);
		set_irn_pinned(new_node, get_irn_pinned(node));
		set_ia32_op_type(new_node, ia32_AddrModeD);
		set_address(new_node, &addr);

		assert(i < 4);
		ins[i++] = new_mem;

		size_bytes -= delta;
		ofs        += delta;
		addr.imm.offset += delta;
	} while (size_bytes != 0);

	if (i > 1) {
		return new_rd_Sync(dbgi, new_block, i, ins);
	} else {
		return get_Proj_pred(ins[0]);
	}
}

/**
 * Generate a fist or fisttp instruction.
 */
static ir_node *gen_fist(dbg_info *dbgi, ir_node *block, ir_node *base,
                         ir_node *index, ir_node *mem,  ir_node *val,
                         x86_insn_size_t const size)
{
	ir_node *res;
	if (ia32_cg_config.use_fisttp) {
		/* Note: fisttp ALWAYS pop the tos. We have to ensure here that the
		 * value is copied if other users exists */
		res = new_bd_ia32_fisttp(dbgi, block, base, index, mem, val, size);
	} else {
		ir_graph *const irg        = get_irn_irg(block);
		ir_node  *const trunc_mode = ia32_new_Fpu_truncate(irg);
		/* 64bit int store is only available as a pop variant */
		if (size == X86_SIZE_64) {
			res = new_bd_ia32_fistp(dbgi, block, base, index, mem, val,
			                        trunc_mode, size);
		} else {
			assert(size == X86_SIZE_16 || size == X86_SIZE_32);
			res = new_bd_ia32_fist(dbgi, block, base, index, mem, val,
			                       trunc_mode, size);
		}
	}
	return res;
}

/** Create a fst or fstp instruction. */
static ir_node *create_fst(dbg_info *dbgi, ir_node *block, ir_node *base,
                           ir_node *index, ir_node *mem, ir_node *val,
                           x86_insn_size_t const size)
{
	ir_node *res;
	if (size > X86_SIZE_64) {
		/* We only have a pop variant for mode_E stores. */
		assert(size == X86_SIZE_80);
		res = new_bd_ia32_fstp(dbgi, block, base, index, mem, val, size);
	} else {
		assert(size == X86_SIZE_32 || size == X86_SIZE_64);
		res = new_bd_ia32_fst(dbgi, block, base, index, mem, val, size);
	}
	return res;
}

/**
 * Create a store of a value.
 * @return the created ia32 Store node
 */
static ir_node *create_store(dbg_info *dbgi, ir_node *new_block,
                             ir_node *value, const x86_address_t *addr)
{
	ir_mode        *const mode = get_irn_mode(value);
	x86_insn_size_t const size = x86_size_from_mode(mode);
	ir_node *store;
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			ir_node *new_val = be_transform_node(value);
			store = new_bd_ia32_xStore(dbgi, new_block, addr->base, addr->index,
			                           addr->mem, new_val, size);
		} else {
			value = ia32_skip_float_downconv(value);
			ir_node *new_val = be_transform_node(value);
			store = create_fst(dbgi, new_block, addr->base, addr->index,
			                   addr->mem, new_val, size);
		}
	} else if (!ia32_cg_config.use_sse2 && is_float_to_int_conv(value)) {
		value = get_Conv_op(value);
		ir_node *new_val = be_transform_node(value);
		store = gen_fist(dbgi, new_block, addr->base, addr->index, addr->mem,
		                 new_val, size);
	} else if (!ia32_cg_config.use_sse2 && is_Bitcast(value)) {
		ir_node *op = get_Bitcast_op(value);
		assert(mode_is_float(get_irn_mode(op)));
		ir_node *new_op  = be_transform_node(op);
		store = create_fst(dbgi, new_block, addr->base, addr->index,
		                   addr->mem, new_op, size);
	} else {
		/* The size must match for an immediate, so do not skip Convs. */
		ir_node *new_val = try_create_Immediate(value, 'i');
		if (!new_val) {
			value   = be_skip_downconv(value, false);
			new_val = be_transform_node(value);
		}
		assert(mode != mode_b);

		store = size == X86_SIZE_8
			? new_bd_ia32_Store_8bit(dbgi, new_block, addr->base, addr->index, addr->mem, new_val, size)
			: new_bd_ia32_Store     (dbgi, new_block, addr->base, addr->index, addr->mem, new_val, size);
	}
	set_ia32_op_type(store, ia32_AddrModeD);
	set_address(store, addr);
	return store;
}

/**
 * Transforms a Store.
 *
 * @return the created ia32 Store node
 */
static ir_node *gen_Store(ir_node *node)
{
	ir_node *val  = get_Store_value(node);
	ir_mode *mode = get_irn_mode(val);

	if (mode_is_float(mode) && is_Const(val)) {
		/* We can transform every floating const store
		   into a sequence of integer stores.
		   If the constant is already in a register,
		   it would be better to use it, but we don't
		   have this information here. */
		return gen_float_const_Store(node, val);
	}

	/* check for destination address mode */
	ir_node *destam_node = try_create_dest_am(node);
	if (destam_node != NULL)
		return destam_node;

	/* construct address */
	ir_node *ptr = get_Store_ptr(node);
	ir_node *mem = get_Store_mem(node);
	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem, x86_create_am_normal);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *store     = create_store(dbgi, new_block, val, &addr);

	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(store, throws_exception);
	set_irn_pinned(store, get_irn_pinned(node));
	return store;
}

/**
 * Transforms a Switch.
 *
 * @return the created ia32 SwitchJmp node
 */
static ir_node *gen_Switch(ir_node *node)
{
	ir_node  *sel      = get_Switch_selector(node);
	ir_node  *new_sel  = be_transform_node(sel);
	ir_mode  *sel_mode = get_irn_mode(sel);

	assert(get_mode_size_bits(sel_mode) <= 32);
	assert(!mode_is_signed(sel_mode));
	sel = be_skip_sameconv(sel);
	if (get_mode_size_bits(sel_mode) < 32)
		new_sel = transform_zext(sel);

	ir_type   *const utype = get_unknown_type();
	ir_entity *const entity
		= new_global_entity(irp->dummy_owner, id_unique("TBL"), utype,
		                    ir_visibility_private,
		                    IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	ir_graph              *irg   = get_irn_irg(node);
	const ir_switch_table *table = get_Switch_table(node);
	table = ir_switch_table_duplicate(irg, table);

	dbg_info *const dbgi     = get_irn_dbg_info(node);
	ir_node  *const block    = be_transform_nodes_block(node);
	ir_node  *const base     = get_global_base(irg);
	unsigned  const n_outs   = get_Switch_n_outs(node);

	ir_node *switchjmp;
	ir_node *table_am;
	be_pic_style_t const pic_style = ir_platform.pic_style;
	if (pic_style == BE_PIC_NONE) {
		switchjmp = new_bd_ia32_SwitchJmp(dbgi, block, base, new_sel, n_outs,
		                                  table, entity);
		table_am = switchjmp;
	} else {
		assert(pic_style == BE_PIC_ELF_PLT
		    || pic_style == BE_PIC_ELF_NO_PLT
		    || pic_style == BE_PIC_MACH_O);
		ir_node *const add = new_bd_ia32_Add(dbgi, block, base, new_sel, nomem,
		                                     base, noreg_GP, X86_SIZE_32);
		set_ia32_commutative(add);
		table_am = add;
		switchjmp = new_bd_ia32_SwitchJmp(dbgi, block, add, noreg_GP, n_outs,
		                                  table, entity);
	}
	set_indexed_ent(table_am, 2, entity);
	return switchjmp;
}

/**
 * Transform a Cond node.
 */
static ir_node *gen_Cond(ir_node *node)
{
	/* we get flags from a Cmp */
	ir_node              *sel   = get_Cond_selector(node);
	x86_condition_code_t  cc;
	ir_node              *flags = get_flags_node(sel, &cc);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	return new_bd_ia32_Jcc(dbgi, new_block, flags, cc);
}

static ir_node *create_Fucom(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *left      = get_Cmp_left(node);
	ir_node  *new_left  = be_transform_node(left);
	ir_node  *right     = get_Cmp_right(node);
	ir_mode  *cmp_mode  = get_irn_mode(left);
	check_x87_floatmode(cmp_mode);

	ir_node  *new_node;
	if (ia32_cg_config.use_fucomi) {
		ir_node *new_right = be_transform_node(right);
		new_node = new_bd_ia32_Fucomi(dbgi, new_block, new_left,
		                              new_right, 0);
		set_ia32_commutative(new_node);
	} else {
		if (is_irn_null(right)) {
			new_node = new_bd_ia32_FtstFnstsw(dbgi, new_block, new_left, 0);
		} else {
			ir_node *new_right = be_transform_node(right);
			new_node = new_bd_ia32_FucomFnstsw(dbgi, new_block, new_left,
			                                   new_right, 0);
			set_ia32_commutative(new_node);
		}

		new_node = new_bd_ia32_Sahf(dbgi, new_block, new_node);
	}

	return new_node;
}

static ir_node *create_Ucomi(ir_node *node)
{
	ir_node  *src_block = get_nodes_block(node);
	ir_node  *left      = get_Cmp_left(node);
	ir_node  *right     = get_Cmp_right(node);

	ia32_address_mode_t  am;
	match_arguments(&am, src_block, left, right, NULL,
	                match_commutative | match_am);

	x86_address_t *addr = &am.addr;
	ir_node  *new_block = be_transform_node(src_block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_node  = new_bd_ia32_Ucomis(dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.ins_permuted);
	set_am_attributes(new_node, &am);
	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

static bool ia32_mux_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	ir_node *mux_true  = get_Mux_true(node);
	ir_node *mux_false = get_Mux_false(node);
	ir_mode *mux_mode  = get_irn_mode(node);
	/* mux nodes which get transformed to the set instruction are not clean */
	if (is_Const(mux_true) && is_Const(mux_false)
		&& get_mode_size_bits(mux_mode) == 8) {
		return false;
	}
	return be_upper_bits_clean(mux_true, mode)
		&& be_upper_bits_clean(mux_false, mode);
}

/**
 * If the given Sub node is used by a Store, transforms the Store,
 * otherwise transforms the Sub node.
 *
 * Thus, a SubMem is generated if possible.
 */
static ir_node *transform_sub_or_store(ir_node *sub)
{
	int outs = get_irn_n_outs(sub);

	if (outs == 1) {
		ir_node *succ = get_irn_out(sub, 0);
		if (is_Store(succ) && !be_is_transformed(succ)) {
			ir_node *new_store = try_create_dest_am(succ);
			if (new_store) {
				assert(is_ia32_SubMem(new_store));
				be_set_transformed_node(succ, new_store);
				return new_store;
			}
		}
	}
	return be_transform_node(sub);
}

static ir_node *try_get_sub_flags(ir_node *cmp, ir_node *sub, bool *swap)
{
	if (!cmp_can_use_sub_flags(cmp, sub, swap)) {
		return NULL;
	}

	ir_node *ia32_sub = skip_Proj(transform_sub_or_store(sub));
	if (is_ia32_Sub(ia32_sub)) {
		return be_get_or_make_Proj_for_pn(ia32_sub, pn_ia32_Sub_flags);
	} else if (is_ia32_SubMem(ia32_sub)) {
		return be_get_or_make_Proj_for_pn(ia32_sub, pn_ia32_SubMem_flags);
	} else {
		panic("unknown variant of Sub at %+F", ia32_sub);
	}
}

static ir_node *find_parallel_sub(ir_node *cmp, bool *swap)
{
	ir_node *cmp_left = get_Cmp_left(cmp);
	foreach_out_edge(cmp_left, outedge) {
		ir_node *succ = get_edge_src_irn(outedge);

		if (is_Sub(succ)) {
			ir_node *flags = try_get_sub_flags(cmp, succ, swap);
			if (flags) {
				return flags;
			}
		}
	}

	return NULL;
}

/**
 * Generate code for a Cmp.
 *
 * Cmp nodes must only be transformed in @c get_flags_node, or the
 * caller must make sure to recheck the Cmp's relation.
 */
static ir_node *gen_Cmp(ir_node *node)
{
	ir_node *left     = get_Cmp_left(node);
	ir_mode *cmp_mode = get_irn_mode(left);
	if (mode_is_float(cmp_mode)) {
		if (ia32_cg_config.use_sse2) {
			return create_Ucomi(node);
		} else {
			return create_Fucom(node);
		}
	}
	assert(be_mode_needs_gp_reg(cmp_mode));

	/* Prefer the Test instruction, when encountering (x & y) ==/!= 0 */
	ia32_address_mode_t am;
	x86_address_t     *addr      = &am.addr;
	dbg_info          *dbgi      = get_irn_dbg_info(node);
	ir_node           *block     = get_nodes_block(node);
	ir_node           *new_block = be_transform_node(block);
	ir_node           *right     = get_Cmp_right(node);
	ir_node           *new_node;
	if (is_irn_null(right)         &&
	    is_And(left)               &&
	    get_irn_n_edges(left) == 1) {
		/* Test(and_left, and_right) */
		ir_node *and_left  = get_And_left(left);
		ir_node *and_right = get_And_right(left);

		match_arguments(&am, block, and_left, and_right, NULL,
		                match_commutative |
		                match_am | match_8bit_am | match_16bit_am |
		                match_am_and_immediates | match_immediate);

		/* use 32bit compare mode if possible since the opcode is smaller */
		if (am.op_type == ia32_Normal &&
		    be_upper_bits_clean(and_left, cmp_mode) &&
		    be_upper_bits_clean(and_right, cmp_mode)) {
		    am.size = X86_SIZE_32;
		}

		new_node = am.size == X86_SIZE_8
			? new_bd_ia32_Test_8bit(dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.size, am.ins_permuted)
			: new_bd_ia32_Test     (dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.size, am.ins_permuted);
	} else {
		/* Cmp(left, right) */

		/* Try to find a Sub whose flags we can use. */
		bool swap = false;
		ir_node *sub_flags = find_parallel_sub(node, &swap);
		if (sub_flags) {
			/* A matching Sub was found, but the Cmp lies
			 * the wrong way round. Turn the Cmp. We trust
			 * that the caller pays attention to that
			 * (get_flags_node does). */
			if (swap) {
				ir_relation rel = get_Cmp_relation(node);
				set_Cmp_relation(node, get_inversed_relation(rel));
				set_Cmp_left(node, right);
				set_Cmp_right(node, left);
			}
			new_node = sub_flags;
			return new_node;
		}
		match_arguments(&am, block, left, right, NULL,
		                match_commutative |
		                match_am | match_8bit_am | match_16bit_am |
		                match_am_and_immediates | match_immediate);
		/* use 32bit compare mode if possible since the opcode is smaller */
		if (am.op_type == ia32_Normal &&
		    be_upper_bits_clean(left, cmp_mode) &&
		    be_upper_bits_clean(right, cmp_mode)) {
			am.size = X86_SIZE_32;
		}

		new_node = am.size == X86_SIZE_8
			? new_bd_ia32_Cmp_8bit(dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.size, am.ins_permuted)
			: new_bd_ia32_Cmp     (dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.size, am.ins_permuted);
	}
	set_am_attributes(new_node, &am);
	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

static ir_node *create_CMov(ir_node *node, ir_node *flags, ir_node *new_flags,
                            x86_condition_code_t cc)
{
	ir_node  *val_true  = get_Mux_true(node);
	ir_node  *val_false = get_Mux_false(node);
	assert(ia32_cg_config.use_cmov);
	assert(be_mode_needs_gp_reg(get_irn_mode(val_true)));

	ia32_address_mode_t am;
	ir_node *block = get_nodes_block(node);
	match_arguments(&am, block, val_false, val_true, flags,
	                match_commutative | match_am | match_16bit_am | match_mode_neutral);

	if (am.ins_permuted)
		cc = x86_negate_condition_code(cc);

	x86_address_t *addr      = &am.addr;
	dbg_info      *dbgi      = get_irn_dbg_info(node);
	ir_node       *new_block = be_transform_node(block);
	ir_node *new_node = new_bd_ia32_CMovcc(dbgi, new_block, addr->base,
	                                       addr->index, addr->mem, am.new_op1,
	                                       am.new_op2, new_flags, X86_SIZE_32,
	                                       cc);
	set_am_attributes(new_node, &am);
	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

/**
 * Creates a ia32 Setcc instruction.
 */
static ir_node *create_set_32bit(dbg_info *dbgi, ir_node *new_block,
                                 ir_node *flags, x86_condition_code_t cc,
                                 ir_node *orig_node)
{
	ir_mode *mode     = get_irn_mode(orig_node);
	ir_node *new_node = new_bd_ia32_Setcc(dbgi, new_block, flags, cc);

	/* we might need to conv the result up */
	if (get_mode_size_bits(mode) > 8) {
		new_node = new_bd_ia32_Conv_I2I_8bit(dbgi, new_block, noreg_GP,
		                                     noreg_GP, nomem, new_node,
		                                     X86_SIZE_8, false);
	}

	return new_node;
}

/**
 * Create instruction for an unsigned Difference or Zero.
 */
static ir_node *create_doz(ir_node *psi, ir_node *a, ir_node *b)
{
	ir_node *new_node = gen_binop(psi, a, b, new_bd_ia32_Sub,
		match_mode_neutral | match_am | match_immediate | match_two_users);

	ir_node *sub;
	if (is_Proj(new_node)) {
		sub = get_Proj_pred(new_node);
	} else {
		sub = new_node;
		set_irn_mode(sub, mode_T);
		new_node = be_new_Proj(sub, pn_ia32_Sub_res);
	}
	assert(is_ia32_Sub(sub));
	ir_node *const eflags = be_new_Proj(sub, pn_ia32_Sub_flags);

	dbg_info *const dbgi  = get_irn_dbg_info(psi);
	ir_node  *const block = get_nodes_block(new_node);
	ir_node  *const sbb   = new_bd_ia32_Sbb0(dbgi, block, eflags, X86_SIZE_32);
	ir_node  *const notn  = new_bd_ia32_Not(dbgi, block, sbb, X86_SIZE_32);

	new_node = new_bd_ia32_And(dbgi, block, noreg_GP, noreg_GP, nomem,
	                           new_node, notn, X86_SIZE_32);
	set_ia32_commutative(new_node);
	return new_node;
}

/**
 * Create an const array of two float consts.
 *
 * @param c0        the first constant
 * @param c1        the second constant
 * @param new_mode  IN/OUT for the mode of the constants, if NULL
 *                  smallest possible mode will be used
 */
static ir_entity *ia32_create_const_array(ir_node *c0, ir_node *c1,
                                          ir_mode **new_mode)
{
	ir_mode   *mode = *new_mode;
	ir_tarval *tv0  = get_Const_tarval(c0);
	ir_tarval *tv1  = get_Const_tarval(c1);
	if (mode == NULL) {
		/* detect the best mode for the constants */
		mode = get_tarval_mode(tv0);

		if (mode != ia32_mode_float32) {
			if (tarval_ieee754_can_conv_lossless(tv0, ia32_mode_float32) &&
			    tarval_ieee754_can_conv_lossless(tv1, ia32_mode_float32)) {
				mode = ia32_mode_float32;
				tv0 = tarval_convert_to(tv0, mode);
				tv1 = tarval_convert_to(tv1, mode);
			} else if (mode != ia32_mode_float64) {
				if (tarval_ieee754_can_conv_lossless(tv0, ia32_mode_float64) &&
				    tarval_ieee754_can_conv_lossless(tv1, ia32_mode_float64)) {
					mode = ia32_mode_float64;
					tv0 = tarval_convert_to(tv0, mode);
					tv1 = tarval_convert_to(tv1, mode);
				}
			}
		}

	}

	ir_type *tp = get_type_for_mode(mode);
	tp = ia32_create_float_array(tp);

	ir_entity *ent
		= new_global_entity(get_glob_type(), id_unique("C"), tp,
		                    ir_visibility_private,
		                    IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	ir_initializer_t *initializer = create_initializer_compound(2);

	set_initializer_compound_value(initializer, 0, create_initializer_tarval(tv0));
	set_initializer_compound_value(initializer, 1, create_initializer_tarval(tv1));

	set_entity_initializer(ent, initializer);

	*new_mode = mode;
	return ent;
}

/**
 * Possible transformations for creating a Setcc.
 */
enum setcc_transform_insn {
	SETCC_TR_ADD,
	SETCC_TR_LEA,
	SETCC_TR_LEAxx,
	SETCC_TR_SHL,
	SETCC_TR_NEG,
	SETCC_TR_NOT,
	SETCC_TR_AND,
	SETCC_TR_SET,
	SETCC_TR_OR,
};

typedef struct setcc_step_t {
	enum setcc_transform_insn transform;
	int32_t                   val;
	unsigned                  log_scale;
} setcc_step_t;

typedef struct setcc_transform {
	unsigned             num_steps;
	x86_condition_code_t cc;
	setcc_step_t steps[4];
} setcc_transform_t;

/**
 * Setcc can only handle 0 and 1 result.
 * Find a transformation that creates 0 and 1 from
 * tv_t and tv_f.
 */
static void find_const_transform(x86_condition_code_t cc,
                                 ir_tarval *t, ir_tarval *f,
                                 setcc_transform_t *res)
{
	unsigned step = 0;

	/* Expand to GP register width, so no carry bit gets lost when calculating new
	 * constants. */
	t = tarval_convert_to(t, ia32_mode_gp);
	f = tarval_convert_to(f, ia32_mode_gp);

	if (tarval_is_null(t)) {
		ir_tarval *tmp = t;
		t = f;
		f = tmp;
		cc = x86_negate_condition_code(cc);
	} else if (tarval_cmp(t, f) == ir_relation_less) {
		// Ensure that t is the bigger one
		ir_tarval *tmp = t;
		t = f;
		f = tmp;
		cc = x86_negate_condition_code(cc);
	}
	res->cc = cc;

	if (!tarval_is_null(f)) {
		if (tarval_is_all_one(t)) {
			res->steps[step].transform = SETCC_TR_OR;
		} else {
			/* Normalize f to zero. */
			ir_tarval *t_sub = tarval_sub(t, f);

			t = t_sub;
			res->steps[step].transform = SETCC_TR_ADD;

			if (t == tarval_bad)
				panic("constant subtract failed");
		}

		if (!tarval_is_long(f))
			panic("tarval is not long");

		res->steps[step].val = get_tarval_long(f);
		++step;
		f = tarval_sub(f, f);
		assert(tarval_is_null(f));
	}

	if (tarval_is_one(t)) {
		res->steps[step].transform = SETCC_TR_SET;
		res->num_steps = ++step;
		return;
	}

	if (tarval_is_all_one(t)) {
		res->steps[step].transform = SETCC_TR_NEG;
		++step;
		res->steps[step].transform = SETCC_TR_SET;
		res->num_steps = ++step;
		return;
	}
	if (tarval_is_long(t)) {
		long v = get_tarval_long(t);

		res->steps[step].val = 0;
		switch (v) {
		case 9:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = SETCC_TR_LEAxx;
			res->steps[step].log_scale = 3; /* (a << 3) + a */
			break;
		case 8:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = res->steps[step].val == 0 ? SETCC_TR_SHL : SETCC_TR_LEA;
			res->steps[step].log_scale = 3; /* (a << 3) */
			break;
		case 5:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = SETCC_TR_LEAxx;
			res->steps[step].log_scale = 2; /* (a << 2) + a */
			break;
		case 4:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = res->steps[step].val == 0 ? SETCC_TR_SHL : SETCC_TR_LEA;
			res->steps[step].log_scale = 2; /* (a << 2) */
			break;
		case 3:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = SETCC_TR_LEAxx;
			res->steps[step].log_scale = 1; /* (a << 1) + a */
			break;
		case 2:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = SETCC_TR_LEAxx;
			res->steps[step].log_scale = 0; /* (a << 0) + a */
			break;
		case 1:
			res->num_steps = step;
			return;
		default:
			if (get_tarval_popcount(t) != 1) {
				res->steps[step].transform = SETCC_TR_AND;
				res->steps[step].val       = v;
				++step;
				res->steps[step].transform = SETCC_TR_NEG;
			} else {
				int val = get_tarval_lowest_bit(t);
				assert(val >= 0);

				res->steps[step].transform = SETCC_TR_SHL;
				res->steps[step].log_scale = val;
			}
		}
		++step;
		res->steps[step].transform = SETCC_TR_SET;
		res->num_steps = ++step;
		return;
	}
	panic("tarval is not long");
}

static ir_node *create_Conv_I2I(dbg_info *dbgi, ir_node *block, ir_node *base,
                                ir_node *index, ir_node *mem, ir_node *val,
                                x86_insn_size_t const size,
                                bool const sign_extend)
{
	ir_node *(*func)(dbg_info*, ir_node*, ir_node*, ir_node*, ir_node*,
	                 ir_node*, x86_insn_size_t, bool);

	func = size == X86_SIZE_8 ? new_bd_ia32_Conv_I2I_8bit
	                          : new_bd_ia32_Conv_I2I;
	return func(dbgi, block, base, index, mem, val, size, sign_extend);
}

/**
 * Transforms a Mux node into some code sequence.
 *
 * @return The transformed node.
 */
static ir_node *gen_Mux(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *mux_true  = get_Mux_true(node);
	ir_node  *mux_false = get_Mux_false(node);
	ir_node  *sel       = get_Mux_sel(node);
	ir_mode  *mode      = get_irn_mode(node);

	int is_abs = ir_mux_is_abs(sel, mux_false, mux_true);
	if (is_abs != 0) {
		if (be_mode_needs_gp_reg(mode)) {
			be_warningf(node, "integer abs not transformed");
		} else {
			ir_node *op = ir_get_abs_op(sel, mux_false, mux_true);
			return create_float_abs(dbgi, new_block, op, is_abs < 0);
		}
	}

	/* Note: a Mux node uses a Load two times IFF it's used in the compare AND in the result */
	if (mode_is_float(mode)) {
		ir_node    *cmp_left  = get_Cmp_left(sel);
		ir_node    *cmp_right = get_Cmp_right(sel);
		ir_relation relation  = get_Cmp_relation(sel);

		if (ia32_cg_config.use_sse2) {
			if (relation == ir_relation_less || relation == ir_relation_less_equal) {
				if (cmp_left == mux_true && cmp_right == mux_false) {
					/* Mux(a <= b, a, b) => MIN */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_Mins,
			                 match_commutative | match_am | match_two_users);
				} else if (cmp_left == mux_false && cmp_right == mux_true) {
					/* Mux(a <= b, b, a) => MAX */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_Maxs,
			                 match_commutative | match_am | match_two_users);
				}
			} else if (relation == ir_relation_greater || relation == ir_relation_greater_equal) {
				if (cmp_left == mux_true && cmp_right == mux_false) {
					/* Mux(a >= b, a, b) => MAX */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_Maxs,
			                 match_commutative | match_am | match_two_users);
				} else if (cmp_left == mux_false && cmp_right == mux_true) {
					/* Mux(a >= b, b, a) => MIN */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_Mins,
			                 match_commutative | match_am | match_two_users);
				}
			}
		}

		if (is_Const(mux_true) && is_Const(mux_false)) {
			x86_condition_code_t cc;
			ir_node *flags    = get_flags_node(sel, &cc);
			ir_node *new_node = create_set_32bit(dbgi, new_block, flags, cc, node);
			ir_mode *new_mode;
			if (ia32_cg_config.use_sse2) {
				/* cannot load from different mode on SSE */
				new_mode = mode;
			} else {
				/* x87 can load any mode */
				new_mode = NULL;
			}

			ir_entity *array = ia32_create_const_array(mux_false, mux_true,
			                                           &new_mode);
			unsigned log_scale;
			switch (get_type_size(get_type_for_mode(new_mode))) {
			case 12:
				new_node = create_lea(dbgi, new_block, new_node, new_node, 1, 0);
				/* FALLTHROUGH */
			case 4:
				log_scale = 2;
				break;

			case 16:
				new_node = create_lea(dbgi, new_block, new_node, new_node, 0, 0);
				/* FALLTHROUGH */
			case 8:
				log_scale = 3;
				break;

			default:
				panic("unsupported constant size");
			}

			typedef ir_node *(cons_t)(dbg_info*, ir_node*, ir_node*, ir_node*, ir_node*, x86_insn_size_t);

			cons_t         *const cons = ia32_cg_config.use_sse2 ? &new_bd_ia32_xLoad : &new_bd_ia32_fld;
			ir_graph       *const irg  = get_irn_irg(new_block);
			ir_node        *const base = get_global_base(irg);
			x86_insn_size_t const size = x86_size_from_mode(new_mode);
			ir_node        *const load = cons(dbgi, new_block, base, new_node, nomem, size);
			set_indexed_ent(load, log_scale, array);

			return be_new_Proj(load, pn_ia32_res);
		}
		panic("cannot transform floating point Mux");

	} else {
		assert(be_mode_needs_gp_reg(mode));

		if (is_Cmp(sel)) {
			ir_node    *cmp_left  = get_Cmp_left(sel);
			ir_node    *cmp_right = get_Cmp_right(sel);
			ir_relation relation  = get_Cmp_relation(sel);
			ir_node    *val_true  = mux_true;
			ir_node    *val_false = mux_false;

			if (is_irn_null(val_true)) {
				ir_node *tmp = val_false;
				val_false = val_true;
				val_true  = tmp;
				relation  = get_negated_relation(relation);
			}
			if (is_irn_null(val_false) && is_Sub(val_true)) {
				if ((relation & ~ir_relation_equal) == ir_relation_greater
					&& get_Sub_left(val_true) == cmp_left
					&& get_Sub_right(val_true) == cmp_right) {
					return create_doz(node, cmp_left, cmp_right);
				}
				if ((relation & ~ir_relation_equal) == ir_relation_less
					&& get_Sub_left(val_true) == cmp_right
					&& get_Sub_right(val_true) == cmp_left) {
					return create_doz(node, cmp_right, cmp_left);
				}
			}
		}

		x86_condition_code_t cc;
		ir_node *flags = get_flags_node(sel, &cc);
		ir_node *new_node;
		if (is_Const(mux_true) && is_Const(mux_false)) {
			/* both are const, good */
			ir_tarval *tv_true  = get_Const_tarval(mux_true);
			ir_tarval *tv_false = get_Const_tarval(mux_false);
			setcc_transform_t res;

			find_const_transform(cc, tv_true, tv_false, &res);
			new_node = node;
			for (unsigned step = res.num_steps; step-- != 0;) {
				setcc_step_t const *const s = &res.steps[step];
				switch (s->transform) {
				case SETCC_TR_ADD: {
					new_node = create_lea(dbgi, new_block, new_node, noreg_GP, 0, s->val);
					continue;
				}

				case SETCC_TR_LEA: {
					new_node = new_bd_ia32_Lea(dbgi, new_block, noreg_GP, new_node);
					ia32_attr_t *const attr = get_ia32_attr(new_node);
					attr->addr.variant          = X86_ADDR_INDEX;
					attr->addr.log_scale        = s->log_scale;
					attr->addr.immediate.offset = s->val;
					continue;
				}

				case SETCC_TR_LEAxx: {
					new_node = create_lea(dbgi, new_block, new_node, new_node, s->log_scale, s->val);
					continue;
				}

				case SETCC_TR_SHL: {
					ir_graph *const irg = get_irn_irg(new_block);
					ir_node  *const imm
						= ia32_create_Immediate(irg, s->log_scale);
					new_node = new_bd_ia32_Shl(dbgi, new_block, new_node, imm,
					                           X86_SIZE_32);
					continue;
				}

				case SETCC_TR_NEG:
					new_node = new_bd_ia32_Neg(dbgi, new_block, new_node,
					                           X86_SIZE_32);
					continue;

				case SETCC_TR_NOT:
					new_node = new_bd_ia32_Not(dbgi, new_block, new_node,
					                           X86_SIZE_32);
					continue;

				case SETCC_TR_AND: {
					ir_graph *const irg = get_irn_irg(new_block);
					ir_node  *const imm = ia32_create_Immediate(irg, s->val);
					new_node = new_bd_ia32_And(dbgi, new_block, noreg_GP,
					                           noreg_GP, nomem, new_node, imm,
					                           X86_SIZE_32);
					continue;
				}

				case SETCC_TR_SET:
					new_node = create_set_32bit(dbgi, new_block, flags, res.cc, node);
					continue;

				case SETCC_TR_OR: {
					ir_graph *const irg = get_irn_irg(new_block);
					ir_node  *const imm = ia32_create_Immediate(irg, s->val);
					new_node = new_bd_ia32_Or(dbgi, new_block, noreg_GP,
					                          noreg_GP, nomem, new_node, imm,
					                          X86_SIZE_32);
					continue;
				}
				}
				panic("unknown setcc transform");
			}
		} else {
			new_node = create_CMov(node, sel, flags, cc);
		}
		return new_node;
	}
}

static void force_int_stackent(ir_node *node, x86_insn_size_t size)
{
	ia32_frame_use_t frame_use;
	if (size == X86_SIZE_64) {
		frame_use = IA32_FRAME_USE_64BIT;
	} else {
		assert(size == X86_SIZE_32);
		frame_use = IA32_FRAME_USE_32BIT;
	}
	set_ia32_frame_use(node, frame_use);
}

/**
 * Create a conversion from x87 state register to general purpose.
 */
static ir_node *gen_x87_fp_to_gp(ir_node *node)
{
	ir_node  *block  = be_transform_nodes_block(node);
	ir_node  *op     = get_Conv_op(node);
	ir_node  *new_op = be_transform_node(op);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_graph *irg    = get_irn_irg(block);
	ir_node  *frame  = get_irg_frame(irg);

	ir_mode *mode = get_irn_mode(node);
	assert(get_mode_size_bits(mode) <= 32);
	/* exception we can only store signed 32 bit integers, so for unsigned
	   we store a 64bit (signed) integer and load the lower bits */
	x86_insn_size_t size = X86_SIZE_32;
	if (get_mode_size_bits(mode) == 32 && !mode_is_signed(mode))
		size = X86_SIZE_64;

	ir_node *fist = gen_fist(dbgi, block, frame, noreg_GP, nomem, new_op, size);
	ia32_attr_t *const fist_attr = get_ia32_attr(fist);
	fist_attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(fist, ia32_AddrModeD);
	arch_add_irn_flags(fist, arch_irn_flag_spill);

	assert((unsigned)pn_ia32_fist_M == (unsigned) pn_ia32_fisttp_M);
	ir_node *const mem = be_new_Proj(fist, pn_ia32_fist_M);

	force_int_stackent(fist, size);

	/* do a Load */
	ir_node *load = new_bd_ia32_Load(dbgi, block, frame, noreg_GP, mem,
	                                 X86_SIZE_32, false);

	ia32_attr_t *const load_attr = get_ia32_attr(load);
	load_attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(load, ia32_AddrModeS);
	force_int_stackent(load, size);

	return be_new_Proj(load, pn_ia32_Load_res);
}

/**
 * Creates a x87 Conv by placing a Store and a Load
 */
static ir_node *gen_x87_conv(dbg_info *const dbgi, x86_insn_size_t const size, ir_node *const node)
{
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *frame = get_irg_frame(irg);

	ir_node *store = create_fst(dbgi, block, frame, noreg_GP, nomem, node,
	                            size);
	ia32_attr_t *const store_attr = get_ia32_attr(store);
	store_attr->addr.variant = X86_ADDR_BASE;
	set_ia32_frame_use(store, IA32_FRAME_USE_AUTO);
	set_ia32_op_type(store, ia32_AddrModeD);
	arch_add_irn_flags(store, arch_irn_flag_spill);

	ir_node *const store_mem = be_new_Proj(store, pn_ia32_fst_M);

	ir_node *load = new_bd_ia32_fld(dbgi, block, frame, noreg_GP, store_mem,
	                                size);
	ia32_attr_t *const load_attr = get_ia32_attr(load);
	load_attr->addr.variant = X86_ADDR_BASE;
	set_ia32_frame_use(load, IA32_FRAME_USE_AUTO);
	set_ia32_op_type(load, ia32_AddrModeS);

	return be_new_Proj(load, pn_ia32_fld_res);
}

static void store_gp(dbg_info *dbgi, ia32_address_mode_t *am, ir_node *block,
                     ir_node *value, bool extend_unsigned)
{
	ir_mode *mode = get_irn_mode(value);
	if (!extend_unsigned) {
		match_arguments(am, block, NULL, value, NULL, match_am | match_try_am);
		if (am->op_type == ia32_AddrModeS)
			return;
	} else if (possible_int_mode_for_fp(mode)) {
		match_arguments(am, block, NULL, value, NULL,
		                match_am | match_try_am | match_sign_ext | match_16bit_am);
		if (am->op_type == ia32_AddrModeS)
			return;
	}

	ir_node  *new_node = be_transform_node(value);

	/* first convert to 32 bit signed if necessary */
	x86_insn_size_t size = x86_size_from_mode(mode);
	if (size < X86_SIZE_32) {
		if (!be_upper_bits_clean(value, mode)) {
			bool sign_extend = mode_is_signed(mode);
			new_node = create_Conv_I2I(dbgi, block, noreg_GP, noreg_GP, nomem,
			                           new_node, size, sign_extend);
		}
		size = X86_SIZE_32;
	}

	/* do a store */
	ir_graph *irg       = get_irn_irg(block);
	ir_node  *frame     = get_irg_frame(irg);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *store     = new_bd_ia32_Store(dbgi, new_block, frame, noreg_GP,
	                                        nomem, new_node, X86_SIZE_32);

	ia32_attr_t *const attr = get_ia32_attr(store);
	attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(store, ia32_AddrModeD);
	arch_add_irn_flags(store, arch_irn_flag_spill);

	ir_node *store_mem = be_new_Proj(store, pn_ia32_Store_M);

	/* exception for 32bit unsigned, do a 64bit spill+load */
	x86_insn_size_t store_size;
	if (!mode_is_signed(mode) && extend_unsigned) {
		ir_node *in[2];
		/* store a zero */
		ir_node *const zero_const     = ia32_create_Immediate(irg, 0);
		ir_node *const zero_store     = new_bd_ia32_Store(dbgi, new_block, frame, noreg_GP, nomem, zero_const, X86_SIZE_32);
		ir_node *const zero_store_mem = be_new_Proj(zero_store, pn_ia32_Store_M);

		set_ia32_op_type(zero_store, ia32_AddrModeD);
		ia32_attr_t *const attr = get_ia32_attr(zero_store);
		attr->addr.variant          = X86_ADDR_BASE;
		attr->addr.immediate.offset = 4;
		arch_add_irn_flags(zero_store, arch_irn_flag_spill);
		set_ia32_frame_use(zero_store, IA32_FRAME_USE_64BIT);

		in[0] = zero_store_mem;
		in[1] = store_mem;

		store_mem  = new_rd_Sync(dbgi, new_block, 2, in);
		store_size = X86_SIZE_64;
	} else {
		store_size = X86_SIZE_32;
	}
	force_int_stackent(store, store_size);

	memset(am, 0, sizeof(*am));
	x86_address_t *addr = &am->addr;
	addr->base    = frame;
	addr->index   = noreg_GP;
	addr->mem     = store_mem;
	addr->imm     = (x86_imm32_t) { .kind = X86_IMM_FRAMEENT };
	addr->variant = X86_ADDR_BASE;
	am->op_type   = ia32_AddrModeS;
	am->size      = store_size;
	am->pinned    = false;
}

/**
 * Create a conversion from general purpose to x87 register
 */
static ir_node *gen_x87_gp_to_fp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *op        = get_Conv_op(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	ia32_address_mode_t am;
	store_gp(dbgi, &am, block, op, true);

	const x86_address_t *addr = &am.addr;
	ir_node *const fild     = new_bd_ia32_fild(dbgi, new_block, addr->base,
	                                           addr->index, addr->mem,
	                                           am.size);
	ir_node *const new_node = be_new_Proj(fild, pn_ia32_fild_res);
	set_am_attributes(fild, &am);
	if (addr->imm.kind == X86_IMM_FRAMEENT && addr->imm.entity == NULL)
		force_int_stackent(fild, am.size);

	fix_mem_proj(fild, &am);
	return new_node;
}

/**
 * Create a conversion from one integer mode into another one
 */
static ir_node *create_I2I_Conv(ir_mode *const src_mode, dbg_info *const dbgi, ir_node *const block, ir_node *op)
{
#ifdef DEBUG_libfirm
	if (is_Const(op))
		be_warningf(op, "unoptimized conv after constant");
#endif

	op = be_skip_downconv(op, true);

	if (be_upper_bits_clean(op, src_mode))
		return be_transform_node(op);

	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, op, NULL,
	                match_am | match_8bit_am | match_16bit_am);

	x86_insn_size_t const size        = x86_size_from_mode(src_mode);
	bool            const sign_extend = mode_is_signed(src_mode);
	x86_address_t *addr = &am.addr;
	ir_node *new_block = be_transform_node(block);
	ir_node *new_node = create_Conv_I2I(dbgi, new_block, addr->base,
	                                    addr->index, addr->mem, am.new_op2,
	                                    size, sign_extend);
	set_am_attributes(new_node, &am);
	/* match_arguments assume that out-mode = in-mode, this isn't true here
	 * so fix it */
	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

/**
 * Transforms a Conv node.
 *
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ir_node *node)
{
	ir_node *op        = get_Conv_op(node);
	ir_mode *src_mode  = get_irn_mode(op);
	ir_mode *tgt_mode  = get_irn_mode(node);
	int      src_bits  = get_mode_size_bits(src_mode);
	int      tgt_bits  = get_mode_size_bits(tgt_mode);

	assert(!mode_is_int(src_mode) || src_bits <= 32);
	assert(!mode_is_int(tgt_mode) || tgt_bits <= 32);

	if (src_mode == tgt_mode) {
		/* this should be optimized already, but who knows... */
		DEBUG_ONLY(be_warningf(node, "pointless Conv");)
		DB((dbg, LEVEL_1, "killed Conv(mode, mode) ..."));
		return be_transform_node(op);
	}

	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *res;
	if (mode_is_float(src_mode)) {
		ir_node *new_op = be_transform_node(op);
		/* we convert from float ... */
		if (mode_is_float(tgt_mode)) {
			x86_insn_size_t const size = x86_size_from_mode(tgt_mode);
			/* ... to float */
			if (ia32_cg_config.use_sse2) {
				DB((dbg, LEVEL_1, "create Conv(float, float) ..."));
				res = new_bd_ia32_Conv_FP2FP(dbgi, new_block, noreg_GP, noreg_GP, nomem, new_op, size);
			} else {
				if (src_bits < tgt_bits) {
					DB((dbg, LEVEL_1, "killed Conv(float, float) ..."));
					return new_op;
				} else {
					return gen_x87_conv(dbgi, size, new_op);
				}
			}
		} else {
			/* ... to int */
			DB((dbg, LEVEL_1, "create Conv(float, int) ..."));
			if (ia32_cg_config.use_sse2) {
				res = new_bd_ia32_Conv_FP2I(dbgi, new_block, noreg_GP, noreg_GP, nomem, new_op, x86_size_from_mode(src_mode));
			} else {
				return gen_x87_fp_to_gp(node);
			}
		}
	} else {
		/* we convert from int ... */
		if (mode_is_float(tgt_mode)) {
			/* ... to float */
			DB((dbg, LEVEL_1, "create Conv(int, float) ..."));
			x86_insn_size_t const tgt_size = x86_size_from_mode(tgt_mode);
			if (ia32_cg_config.use_sse2) {
				ir_node *new_op = be_transform_node(op);
				res = new_bd_ia32_Conv_I2FP(dbgi, new_block, noreg_GP, noreg_GP, nomem, new_op, tgt_size);
			} else {
				unsigned int_mantissa   = get_mode_size_bits(src_mode) - (mode_is_signed(src_mode) ? 1 : 0);
				unsigned float_mantissa = get_mode_mantissa_size(tgt_mode);
				ir_node *res = gen_x87_gp_to_fp(node);

				/* we need a float-conv, if the int mode has more bits than the
				 * float mantissa */
				if (float_mantissa < int_mantissa)
					res = gen_x87_conv(dbgi, tgt_size, res);
				return res;
			}
		} else {
			/* to int */
			if (src_bits >= tgt_bits) {
				DB((dbg, LEVEL_1, "omitting unnecessary Conv(%+F, %+F) ...",
				    src_mode, tgt_mode));
				return be_transform_node(op);
			}

			res = create_I2I_Conv(src_mode, dbgi, block, op);
			return res;
		}
	}

	return res;
}

static void store_fp(dbg_info *dbgi, ia32_address_mode_t *am, ir_node *block,
                     ir_node *value)
{
	match_arguments(am, block, NULL, value, NULL, match_am | match_try_am);
	if (am->op_type == ia32_AddrModeS)
		return;
	/* TODO: match_am if value is a freshly loaded float value */
	ir_node        *const new_block = be_transform_node(block);
	ir_node        *const new_value = be_transform_node(value);
	ir_graph       *const irg       = get_irn_irg(block);
	ir_node        *const frame     = get_irg_frame(irg);
	ir_mode        *const mode      = get_irn_mode(value);
	x86_insn_size_t const size      = x86_size_from_mode(mode);

	ir_node *fst = create_fst(dbgi, new_block, frame, noreg_GP, nomem,
	                          new_value, size);
	ia32_attr_t *const attr = get_ia32_attr(fst);
	attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(fst, ia32_AddrModeD);
	arch_add_irn_flags(fst, arch_irn_flag_spill);
	force_int_stackent(fst, size);
	ir_node *const mem = be_new_Proj(fst, pn_ia32_fst_M);

	memset(am, 0, sizeof(*am));
	x86_address_t *addr = &am->addr;
	addr->variant = X86_ADDR_BASE;
	addr->base    = frame;
	addr->index   = noreg_GP;
	addr->mem     = mem;
	addr->imm     = (x86_imm32_t) { .kind = X86_IMM_FRAMEENT };
	am->op_type   = ia32_AddrModeS;
	am->size      = size;
	am->pinned    = false;
}

static ir_node *gen_Bitcast(ir_node *const node)
{
	ir_mode *dst_mode = get_irn_mode(node);
	ir_node *op       = get_Bitcast_op(node);
	ir_mode *src_mode = get_irn_mode(op);
	/* TODO: SSE2 variant (a nop?) */

	/* bitcast means float->int or int->float switch which only works with
	 * load/store on ia32 */
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ia32_address_mode_t am;
	switch (get_mode_arithmetic(src_mode)) {
	case irma_twos_complement:
		store_gp(dbgi, &am, block, op, false);
		break;
	case irma_ieee754:
	case irma_x86_extended_float:
		store_fp(dbgi, &am, block, op);
		break;
	default:
		panic("unexpected src mode in Bitcast");
	}

	ir_node                   *ld;
	ir_node                   *res;
	ir_node             *const new_block = be_transform_node(block);
	x86_address_t const *const addr      = &am.addr;
	x86_insn_size_t      const size      = x86_size_from_mode(dst_mode);
	switch (get_mode_arithmetic(dst_mode)) {
	case irma_ieee754:
	case irma_x86_extended_float:
		ld  = new_bd_ia32_fld(dbgi, new_block, addr->base, addr->index,
		                      addr->mem, size);
		res = be_new_Proj(ld, pn_ia32_fld_res);
		break;

	case irma_twos_complement:
		ld  = new_bd_ia32_Load(dbgi, new_block, addr->base, addr->index,
		                       addr->mem, size, false);
		res = be_new_Proj(ld, pn_ia32_Load_res);
		break;

	default:
		panic("unexpected dst mode in Bitcast");
	}
	am.size = size;
	set_am_attributes(ld, &am);
	if (get_ia32_frame_use(ld) != IA32_FRAME_USE_NONE)
		force_int_stackent(ld, size);
	fix_mem_proj(ld, &am);
	return res;
}

static ir_node *create_immediate_or_transform(ir_node *const node,
                                              const char immediate_mode)
{
	ir_node *new_node = try_create_Immediate(node, immediate_mode);
	if (new_node == NULL) {
		new_node = be_transform_node(node);
	}
	return new_node;
}

static ir_node *gen_Member(ir_node *node)
{
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);

	x86_address_t addr;
	ia32_create_address_mode(&addr, node, x86_create_am_force);

	return create_lea_from_address(dbgi, block, &addr);
}

static ir_node *gen_Start(ir_node *node)
{
	x86_cconv_t const *const cconv = current_cconv;

	be_start_out outs[N_IA32_REGISTERS] = { [REG_ESP] = BE_START_IGNORE };

	/* function parameters in registers */
	for (size_t i = 0, n = cconv->n_parameters; i != n; ++i) {
		reg_or_stackslot_t const *const param = &cconv->parameters[i];
		arch_register_t    const *const reg   = param->reg;
		if (reg)
			outs[reg->global_index] = BE_START_REG;
	}

	/* callee saves */
	for (size_t i = 0; i < N_IA32_REGISTERS; ++i) {
		if (rbitset_is_set(cconv->callee_saves, i))
			outs[i] = BE_START_REG;
	}
	if (!cconv->omit_fp)
		outs[REG_EBP] = BE_START_IGNORE;

	ir_graph *const irg = get_irn_irg(node);
	return be_new_Start(irg, outs);
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	unsigned  pn  = get_Proj_num(node);
	be_transform_node(get_Proj_pred(node));

	switch ((pn_Start)pn) {
	case pn_Start_M:
		return be_get_Start_mem(irg);
	case pn_Start_T_args:
		return new_r_Bad(irg, mode_T);
	case pn_Start_P_frame_base:
		return current_cconv->omit_fp ? get_initial_sp(irg)
		                              : get_initial_fp(irg);
	}
	panic("unexpected Start Proj: %u", pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_graph                 *const irg   = get_irn_irg(node);
	unsigned                  const pn    = get_Proj_num(node);
	reg_or_stackslot_t const *const param = &current_cconv->parameters[pn];
	/* stack parameter should have been lowered to loads already */
	assert(param->reg != NULL);
	/* argument transmitted in register */
	return be_get_Start_proj(irg, param->reg);
}

static ir_node *gen_Return(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *mem       = get_Return_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_node  *sp        = get_initial_sp(irg);
	unsigned  n_res     = get_Return_n_ress(node);
	x86_cconv_t    *cconv = current_cconv;

	/* estimate number of return values */
	unsigned       p              = n_ia32_Ret_first_result;
	unsigned const n_callee_saves = rbitset_popcount(cconv->callee_saves, N_IA32_REGISTERS);
	unsigned const n_ins          = p + n_res + n_callee_saves;

	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);

	in[n_ia32_Ret_mem]   = new_mem;
	reqs[n_ia32_Ret_mem] = arch_memory_req;

	in[n_ia32_Ret_stack]   = sp;
	reqs[n_ia32_Ret_stack] = &ia32_single_reg_req_gp_esp;

	/* result values */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &current_cconv->results[i];
		in[p]   = new_res_value;
		reqs[p] = slot->reg->single_req;
		++p;
	}
	/* callee saves */
	for (unsigned i = 0; i < N_IA32_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->callee_saves, i))
			continue;
		arch_register_t const *const reg = &ia32_registers[i];
		in[p]   = be_get_Start_proj(irg, reg);
		reqs[p] = reg->single_req;
		++p;
	}
	assert(p == n_ins);

	ir_node *const ret = new_bd_ia32_Ret(dbgi, new_block, n_ins, in, reqs, current_cconv->sp_delta);
	be_stack_record_chain(&stack_env, ret, n_ia32_Ret_stack, NULL);
	return ret;
}

static ir_node *gen_Alloc(ir_node *node)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_nodes_block(node);
	ir_node  *const mem       = get_Alloc_mem(node);
	ir_node  *const new_mem   = be_transform_node(mem);
	ir_graph *const irg       = get_irn_irg(node);
	ir_node  *const stack     = get_initial_sp(irg);
	/* TODO: match address mode for size... */
	ir_node  *const size      = get_Alloc_size(node);
	ir_node  *const new_size  = create_immediate_or_transform(size, 'i');
	ir_node  *const new_node  = new_bd_ia32_SubSP(dbgi, new_block, noreg_GP, noreg_GP, new_mem, stack, new_size);

	ir_node *const stack_proj = be_new_Proj_reg(new_node, pn_ia32_SubSP_stack, &ia32_registers[REG_ESP]);
	be_stack_record_chain(&stack_env, new_node, n_ia32_SubSP_stack, stack_proj);

	return new_node;
}

static ir_node *gen_Proj_Alloc(ir_node *node)
{

	ir_node *alloc = get_Proj_pred(node);
	ir_node *subsp = be_transform_node(alloc);
	switch ((pn_Alloc)get_Proj_num(node)) {
	case pn_Alloc_M:   return be_new_Proj(subsp, pn_ia32_SubSP_M);
	case pn_Alloc_res: return be_new_Proj(subsp, pn_ia32_SubSP_addr);
	}
	panic("invalid Proj->Alloc %+F", node);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (be_mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		req = &ia32_class_reg_req_gp;
	} else if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			req = &ia32_class_reg_req_xmm;
		} else {
			req = &ia32_class_reg_req_fp;
		}
	} else {
		req = arch_memory_req;
	}
	return be_transform_phi(node, req);
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	return new_bd_ia32_Jmp(dbgi, new_block);
}

static void adjust_pc_relative_relocation(ir_node *node)
{
	if (!is_ia32_Immediate(node))
		return;
	ia32_immediate_attr_t *attr = get_ia32_immediate_attr(node);
	if (attr->imm.kind == X86_IMM_ADDR)
		attr->imm.kind = X86_IMM_PCREL;
}

/**
 * Transform IJmp
 */
static ir_node *gen_IJmp(ir_node *node)
{
	ir_node *block = get_nodes_block(node);
	ir_node *op    = get_IJmp_target(node);

	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, op, NULL, match_am | match_immediate);
	adjust_pc_relative_relocation(am.new_op2);

	x86_address_t *addr      = &am.addr;
	dbg_info      *dbgi      = get_irn_dbg_info(node);
	ir_node       *new_block = be_transform_node(block);
	ir_node       *new_node  = new_bd_ia32_IJmp(dbgi, new_block, addr->base,
	                                            addr->index, addr->mem,
	                                            am.new_op2);
	set_am_attributes(new_node, &am);

	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

static ir_node *gen_ia32_l_Add(ir_node *node)
{
	ir_node *left    = get_irn_n(node, n_ia32_l_Add_left);
	ir_node *right   = get_irn_n(node, n_ia32_l_Add_right);
	ir_node *lowered = gen_binop(node, left, right, new_bd_ia32_Add,
	                             match_commutative | match_am | match_immediate
	                             | match_mode_neutral);

	if (is_Proj(lowered)) {
		lowered = get_Proj_pred(lowered);
	} else {
		assert(is_ia32_Add(lowered));
		set_irn_mode(lowered, mode_T);
	}

	return lowered;
}

static ir_node *gen_ia32_l_Adc(ir_node *node)
{
	return gen_binop_flags(node, new_bd_ia32_Adc,
	                       match_commutative | match_am | match_immediate
	                       | match_mode_neutral);
}

/**
 * Transforms a l_MulS into a "real" MulS node.
 *
 * @return the created ia32 Mul node
 */
static ir_node *gen_ia32_l_Mul(ir_node *node)
{
	ir_node *left  = get_irn_n(node, n_ia32_l_Mul_left);
	ir_node *right = get_irn_n(node, n_ia32_l_Mul_right);
	return gen_binop(node, left, right, new_bd_ia32_Mul,
	                 match_commutative | match_am | match_mode_neutral);
}

/**
 * Transforms a l_IMulS into a "real" IMul1OPS node.
 *
 * @return the created ia32 IMul1OP node
 */
static ir_node *gen_ia32_l_IMul(ir_node *node)
{
	ir_node *left  = get_irn_n(node, n_ia32_l_IMul_left);
	ir_node *right = get_irn_n(node, n_ia32_l_IMul_right);
	return gen_binop(node, left, right, new_bd_ia32_IMul1OP,
	                 match_commutative | match_am | match_mode_neutral);
}

static ir_node *gen_ia32_l_Sub(ir_node *node)
{
	ir_node *left    = get_irn_n(node, n_ia32_l_Sub_minuend);
	ir_node *right   = get_irn_n(node, n_ia32_l_Sub_subtrahend);
	ir_node *lowered = gen_binop(node, left, right, new_bd_ia32_Sub,
	                             match_am | match_immediate
	                             | match_mode_neutral);

	if (is_Proj(lowered)) {
		lowered = get_Proj_pred(lowered);
	} else {
		assert(is_ia32_Sub(lowered));
		set_irn_mode(lowered, mode_T);
	}
	return lowered;
}

static ir_node *gen_ia32_l_Sbb(ir_node *node)
{
	return gen_binop_flags(node, new_bd_ia32_Sbb,
	                       match_am | match_immediate | match_mode_neutral);
}

static ir_node *gen_ia32_l_Minus64(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *low       = get_irn_n(node, n_ia32_l_Minus64_low);
	ir_node  *high      = get_irn_n(node, n_ia32_l_Minus64_high);
	ir_node  *new_low   = be_transform_node(low);
	ir_node  *new_high  = be_transform_node(high);
	return new_bd_ia32_Minus64(dbgi, new_block, new_low, new_high);
}

static ir_node *gen_ia32_l_LLtoFloat(ir_node *node)
{
	if (ia32_cg_config.use_sse2)
		panic("not implemented for SSE2");

	ir_node  *block        = be_transform_nodes_block(node);
	ir_graph *irg          = get_irn_irg(block);
	dbg_info *dbgi         = get_irn_dbg_info(node);
	ir_node  *frame        = get_irg_frame(irg);
	ir_node  *val_low      = get_irn_n(node, n_ia32_l_LLtoFloat_val_low);
	ir_node  *val_high     = get_irn_n(node, n_ia32_l_LLtoFloat_val_high);
	ir_node  *new_val_low  = be_transform_node(val_low);
	ir_node  *new_val_high = be_transform_node(val_high);

	/* do a store */
	ir_node *store_low = new_bd_ia32_Store(dbgi, block, frame, noreg_GP, nomem,
	                                       new_val_low, X86_SIZE_32);
	ir_node *store_high = new_bd_ia32_Store(dbgi, block, frame, noreg_GP, nomem,
	                                        new_val_high, X86_SIZE_32);

	ir_node *const mem_low  = be_new_Proj(store_low,  pn_ia32_Store_M);
	ir_node *const mem_high = be_new_Proj(store_high, pn_ia32_Store_M);

	set_ia32_op_type(store_low, ia32_AddrModeD);
	set_ia32_op_type(store_high, ia32_AddrModeD);
	arch_add_irn_flags(store_low, arch_irn_flag_spill);
	arch_add_irn_flags(store_high, arch_irn_flag_spill);
	force_int_stackent(store_low, X86_SIZE_64);
	force_int_stackent(store_high, X86_SIZE_64);
	ia32_attr_t *const attr_low  = get_ia32_attr(store_low);
	attr_low->addr.variant = X86_ADDR_BASE;
	ia32_attr_t *const attr_high = get_ia32_attr(store_high);
	attr_high->addr.variant          = X86_ADDR_BASE;
	attr_high->addr.immediate.offset = 4;

	ir_node *in[2] = { mem_low, mem_high };
	ir_node *sync = new_rd_Sync(dbgi, block, ARRAY_SIZE(in), in);

	/* do a fild */
	ir_node *fild = new_bd_ia32_fild(dbgi, block, frame, noreg_GP, sync,
	                                 X86_SIZE_64);
	ia32_attr_t *const fild_attr = get_ia32_attr(fild);
	fild_attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(fild, ia32_AddrModeS);
	force_int_stackent(fild, X86_SIZE_64);

	ir_node *res = be_new_Proj(fild, pn_ia32_fild_res);

	if (!mode_is_signed(get_irn_mode(val_high))) {
		ir_node *const base  = get_global_base(irg);
		ir_node *const count = ia32_create_Immediate(irg, 31);
		ir_node *const index = new_bd_ia32_Shr(dbgi, block, new_val_high, count, X86_SIZE_32);
		ir_node *const noreg = ia32_new_NoReg_fp(irg);
		ir_node *const fpcw  = get_initial_fpcw(irg);
		res = new_bd_ia32_fadd(dbgi, block, base, index, nomem, res, noreg, fpcw, X86_SIZE_32);
		set_indexed_ent(res, 2, ia32_gen_fp_known_const(ia32_ULLBIAS));
	}
	return res;
}

static ir_node *gen_ia32_l_FloattoLL(ir_node *node)
{
	ir_node  *block     = be_transform_nodes_block(node);
	ir_graph *irg       = get_irn_irg(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *frame     = get_irg_frame(irg);
	ir_node  *val       = get_irn_n(node, n_ia32_l_FloattoLL_val);
	ir_node  *new_val   = be_transform_node(val);

	ir_node *fist = gen_fist(dbgi, block, frame, noreg_GP, nomem, new_val,
	                         X86_SIZE_64);
	ia32_attr_t *const attr = get_ia32_attr(fist);
	attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(fist, ia32_AddrModeD);
	arch_add_irn_flags(fist, arch_irn_flag_spill);
	force_int_stackent(fist, X86_SIZE_64);

	assert((unsigned)pn_ia32_fist_M == (unsigned) pn_ia32_fisttp_M);
	return be_new_Proj(fist, pn_ia32_fist_M);
}

static ir_node *gen_Proj_l_FloattoLL(ir_node *node)
{
	ir_node  *block    = be_transform_nodes_block(node);
	ir_graph *irg      = get_irn_irg(block);
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	ir_node  *frame    = get_irg_frame(irg);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	unsigned  pn       = get_Proj_num(node);

	ir_node *load = new_bd_ia32_Load(dbgi, block, frame, noreg_GP, new_pred,
	                                 X86_SIZE_32, false);
	ia32_attr_t *const attr = get_ia32_attr(load);
	attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(load, ia32_AddrModeS);
	force_int_stackent(load, X86_SIZE_64);

	if (pn == pn_ia32_l_FloattoLL_res_high) {
		ia32_attr_t *const attr = get_ia32_attr(load);
		attr->addr.immediate.offset = 4;
	} else {
		assert(pn == pn_ia32_l_FloattoLL_res_low);
	}

	return be_new_Proj(load, pn_ia32_Load_res);
}

/**
 * Transform and renumber the Projs from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *pred = get_Proj_pred(node);
	unsigned  pn   = get_Proj_num(node);

	/* loads might be part of source address mode matches, so we don't
	 * transform the ProjMs yet (with the exception of loads whose result is
	 * not used)
	 */
	if (pn == pn_Load_M && get_irn_n_edges(pred) > 1) {
		/* this is needed, because sometimes we have loops that are only
		   reachable through the ProjM */
		be_enqueue_operands(node);
		/* do it in 2 steps, to silence firm verifier */
		ir_node *const res = new_r_Proj(pred, mode_M, pn_Load_M);
		set_Proj_num(res, pn_ia32_M);
		return res;
	}

	/* renumber the proj */
	ir_node *const new_pred = be_transform_node(pred);
	assert(is_ia32_Conv_I2I(new_pred) || is_ia32_Load(new_pred) || is_ia32_fld(new_pred) || is_ia32_xLoad(new_pred));

	switch ((pn_Load)pn) {
	case pn_Load_res:
		return be_new_Proj(new_pred, pn_ia32_res);
	case pn_Load_M:
		return be_new_Proj(new_pred, pn_ia32_M);
	case pn_Load_X_except:
		/* This Load might raise an exception. Mark it. */
		set_ia32_exc_label(new_pred, 1);
		return be_new_Proj(new_pred, pn_ia32_X_except);
	case pn_Load_X_regular:
		return be_new_Proj(new_pred, pn_ia32_X_regular);
	}

	panic("no idea how to transform Proj(Load) %+F", node);
}

static ir_node *create_proj_for_store(ir_node *store, pn_Store pn)
{
	if (get_irn_mode(store) == mode_M) {
		if (pn == pn_Store_M)
			return store;
	} else if (is_ia32_Store(store) || is_ia32_fist(store) || is_ia32_fistp(store) || is_ia32_fisttp(store) || is_ia32_xStore(store) || is_ia32_fst(store) || is_ia32_fstp(store)) {
		switch (pn) {
		case pn_Store_M:         return be_new_Proj(store, pn_ia32_st_M);
		case pn_Store_X_except:  return be_new_Proj(store, pn_ia32_st_X_except);
		case pn_Store_X_regular: return be_new_Proj(store, pn_ia32_st_X_regular);
		}
	} else if (get_ia32_op_type(store) == ia32_AddrModeD) {
		if (pn == pn_Store_M) {
			/* A Proj M for the RMW operation was created by finish_dest_am() already,
			 * so just fetch it. */
			return get_Proj_for_pn(store, pn_ia32_M);
		}
		panic("exception control flow for destination AM not implemented yet");
	}

	panic("no idea how to transform Proj(Store) at %+F", store);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	pn_Store  pn       = get_Proj_num(node);
	/* in case of DestAM we might have to skip the result Proj */
	if (is_Proj(new_pred))
		new_pred = get_Proj_pred(new_pred);

	return create_proj_for_store(new_pred, pn);
}

/**
 * Transform and renumber the Projs from a Div or Mod instruction.
 */
static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	unsigned  proj     = get_Proj_num(node);

	assert((unsigned)pn_ia32_Div_M == (unsigned)pn_ia32_IDiv_M);
	assert((unsigned)pn_ia32_Div_div_res == (unsigned)pn_ia32_IDiv_div_res);

	switch ((pn_Div)proj) {
	case pn_Div_M:
		if (is_ia32_Div(new_pred) || is_ia32_IDiv(new_pred)) {
			return be_new_Proj(new_pred, pn_ia32_Div_M);
		} else if (is_ia32_Divs(new_pred)) {
			return be_new_Proj(new_pred, pn_ia32_Divs_M);
		} else if (is_ia32_fdiv(new_pred)) {
			return be_new_Proj(new_pred, pn_ia32_fdiv_M);
		} else {
			panic("Div transformed to unexpected thing %+F", new_pred);
		}
	case pn_Div_res:
		if (is_ia32_Div(new_pred) || is_ia32_IDiv(new_pred)) {
			return be_new_Proj(new_pred, pn_ia32_Div_div_res);
		} else if (is_ia32_Divs(new_pred)) {
			return be_new_Proj(new_pred, pn_ia32_Divs_res);
		} else if (is_ia32_fdiv(new_pred)) {
			return be_new_Proj(new_pred, pn_ia32_fdiv_res);
		} else {
			panic("Div transformed to unexpected thing %+F", new_pred);
		}
	case pn_Div_X_except:
		set_ia32_exc_label(new_pred, 1);
		return be_new_Proj(new_pred, pn_ia32_Div_X_except);
	case pn_Div_X_regular:
		return be_new_Proj(new_pred, pn_ia32_Div_X_regular);
	}

	panic("no idea how to transform proj->Div");
}

/**
 * Transform and renumber the Projs from a Div or Mod instruction.
 */
static ir_node *gen_Proj_Mod(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	unsigned  proj     = get_Proj_num(node);

	assert(is_ia32_Div(new_pred) || is_ia32_IDiv(new_pred));
	assert((unsigned)pn_ia32_Div_M == (unsigned)pn_ia32_IDiv_M);
	assert((unsigned)pn_ia32_Div_mod_res == (unsigned)pn_ia32_IDiv_mod_res);

	switch ((pn_Mod)proj) {
	case pn_Mod_M:
		return be_new_Proj(new_pred, pn_ia32_Div_M);
	case pn_Mod_res:
		return be_new_Proj(new_pred, pn_ia32_Div_mod_res);
	case pn_Mod_X_except:
		set_ia32_exc_label(new_pred, 1);
		return be_new_Proj(new_pred, pn_ia32_Div_X_except);
	case pn_Mod_X_regular:
		return be_new_Proj(new_pred, pn_ia32_Div_X_regular);
	}
	panic("no idea how to transform proj->Mod");
}

static ir_node *gen_CopyB(ir_node *node)
{
	ir_node  *block    = be_transform_nodes_block(node);
	ir_node  *src      = get_CopyB_src(node);
	ir_node  *new_src  = be_transform_node(src);
	ir_node  *dst      = get_CopyB_dst(node);
	ir_node  *new_dst  = be_transform_node(dst);
	ir_node  *mem      = get_CopyB_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	int      size      = get_type_size(get_CopyB_type(node));
	int      rem;

	/* If we have to copy more than 32 bytes, we use REP MOVSx and */
	/* then we need the size explicitly in ECX.                    */
	ir_node *projm;
	if (size >= 32 * 4) {
		rem = size & 0x3; /* size % 4 */
		size >>= 2;

		x86_imm32_t imm = { .offset = size };
		ir_node *cnst  = new_bd_ia32_Const(dbgi, block, &imm);
		ir_node *copyb = new_bd_ia32_CopyB(dbgi, block, new_dst, new_src, cnst,
		                                   new_mem, rem);
		projm = be_new_Proj(copyb, pn_ia32_CopyB_M);
	} else {
		if (size == 0)
			be_warningf(node, "unoptimized CopyB with size <4");
		ir_node *copyb = new_bd_ia32_CopyB_i(dbgi, block, new_dst, new_src,
		                                     new_mem, size);
		projm = be_new_Proj(copyb, pn_ia32_CopyB_i_M);
	}
	return projm;
}

static bool callee_is_plt(ir_node *callee)
{
	return be_is_Relocation(callee)
	    && be_get_Relocation_kind(callee) == X86_IMM_PLT;
}

static ir_node *gen_Call(ir_node *node)
{
	/* Construct arguments. */
	ia32_address_mode_t am;
	ir_node      *const old_block = get_nodes_block(node);
	ir_node      *const callee    = get_Call_ptr(node);
	ir_node      *      mem       = get_Call_mem(node);
	match_arguments(&am, old_block, NULL, callee, mem, match_am | match_immediate);
	adjust_pc_relative_relocation(am.new_op2);

	ir_type                    *const type     = get_Call_type(node);
	x86_cconv_t                *const cconv    = ia32_decide_calling_convention(type, NULL);
	ir_graph                   *const irg      = get_irn_irg(node);
	unsigned                          in_arity = n_ia32_Call_first_argument;
	bool                        const has_fpcw = !ia32_cg_config.use_softfloat;
	bool                        const is_plt   = callee_is_plt(callee);
	unsigned                    const n_ins
		= has_fpcw + in_arity + cconv->n_param_regs + is_plt;
	ir_node                   **const in       = ALLOCAN(ir_node*, n_ins);
	arch_register_req_t const **const in_req   = be_allocate_in_reqs(irg, n_ins);

	record_returns_twice(irg, type);

	arch_register_req_t const *const req_gp = &ia32_class_reg_req_gp;
	in[n_ia32_Call_base]       = am.addr.base;
	in_req[n_ia32_Call_base]   = req_gp;
	in[n_ia32_Call_index]      = am.addr.index;
	in_req[n_ia32_Call_index]  = req_gp;
	/* Memory input will be set later. */
	in[n_ia32_Call_callee]     = am.new_op2;
	in_req[n_ia32_Call_callee] = req_gp;

	ir_node *const block           = be_transform_node(old_block);
	ir_node *const stack           = get_initial_sp(irg);
	unsigned const param_stacksize = cconv->param_stacksize;
	/* Always construct an IncSP, so calls do not accidentally CSE. */
	ir_node *const callframe       = ia32_new_IncSP(block, stack, param_stacksize, false);
	in[n_ia32_Call_stack]     = callframe;
	in_req[n_ia32_Call_stack] = &ia32_single_reg_req_gp_esp;

	if (has_fpcw) {
		unsigned const fpcwi = in_arity++;
		in[fpcwi]     = get_initial_fpcw(irg);
		in_req[fpcwi] = &ia32_single_reg_req_fpcw_fpcw;
	}

	unsigned        sync_arity = 0;
	unsigned  const n_params   = get_Call_n_params(node);
	ir_node **const sync_ins   = ALLOCAN(ir_node*, n_params + 1);
	mem = transform_AM_mem(block, callee, mem, am.addr.mem);

	dbg_info *const dbgi = get_irn_dbg_info(node);
	for (unsigned p = 0; p < n_params; ++p) {
		ir_node                  *const value      = get_Call_param(node, p);
		reg_or_stackslot_t const *const param      = &cconv->parameters[p];
		ir_type                  *const param_type = get_method_param_type(type, p);
		if (is_aggregate_type(param_type)) {
			/* Copy aggregate arguments into the callframe. */
			ir_node *const lea       = create_lea(dbgi, block, callframe, noreg_GP, 0, param->offset);
			ir_node *const new_value = be_transform_node(value);
			unsigned const size      = get_type_size(param_type);
			ir_node *const copyb     = new_bd_ia32_CopyB_i(dbgi, block, lea, new_value, mem, size);
			sync_ins[sync_arity++]   = be_new_Proj(copyb, pn_ia32_CopyB_i_M);
		} else if (param->reg) {
			/* Value transmitted in register. */
			unsigned const parami = in_arity++;
			in[parami]     = be_transform_node(value);
			in_req[parami] = param->reg->single_req;
		} else {
			/* Value transmitted on callframe. */
			x86_address_t const store_addr = {
				.variant = X86_ADDR_BASE,
				.base    = callframe,
				.index   = noreg_GP,
				.mem     = mem,
				.imm     = {
					.offset = param->offset
				},
			};
			ir_node *const store = create_store(dbgi, block, value, &store_addr);
			sync_ins[sync_arity++] = create_proj_for_store(store, pn_Store_M);
		}
	}

	if (sync_arity == 0) {
		sync_ins[sync_arity++] = mem;
	}

	/* PIC calls need the GOT address in ebx */
	be_add_pressure_t add_pressure = 0;
	if (is_plt) {
		unsigned goti = in_arity++;
		in[goti]      = ia32_get_pic_base(irg);
		in_req[goti]  = &ia32_single_reg_req_gp_ebx;
		/* We cannot pair up the "ebx" with any output, causing additional
		 * register pressure */
		--add_pressure;
	}
	assert(in_arity == n_ins);
	assert(sync_arity <= n_params + 1);

	/* Memory input. */
	in[n_ia32_Call_mem]     = be_make_Sync(block, sync_arity, sync_ins);
	in_req[n_ia32_Call_mem] = arch_memory_req;

	/* Count outputs. */
	unsigned       o              = pn_ia32_Call_first_result;
	unsigned const n_reg_results  = cconv->n_reg_results;
	unsigned const n_caller_saves = rbitset_popcount(cconv->caller_saves, N_IA32_REGISTERS);
	unsigned const n_out          = o + n_reg_results + n_caller_saves;

	/* Create node. */
	ir_node *const call = new_bd_ia32_Call(dbgi, block, in_arity, in, in_req,
	                                       n_out, cconv->sp_delta,
	                                       n_reg_results);
	arch_set_additional_pressure(call, &ia32_reg_classes[CLASS_ia32_gp],
	                             add_pressure);

	if (get_irn_pinned(node))
		set_irn_pinned(call, true);

	set_am_attributes(call, &am);
	ir_node *const res = fix_mem_proj(call, &am);

	/* Construct outputs. */
	arch_set_irn_register_req_out(call, pn_ia32_Call_mem, arch_memory_req);

	arch_copy_irn_out_info(call, pn_ia32_Call_stack, callframe);

	unsigned const n_ress = get_method_n_ress(type);
	for (unsigned r = 0; r < n_ress; ++r) {
		arch_register_t const *const reg = cconv->results[r].reg;
		arch_set_irn_register_req_out(call, o++, reg->single_req);
		/* Run the x87 simulator, if the call returns a float value. */
		if (reg->cls == &ia32_reg_classes[CLASS_ia32_fp])
			ia32_request_x87_sim(irg);
	}

	/* Caller saves. */
	unsigned const *const allocatable_regs = be_birg_from_irg(irg)->allocatable_regs;
	for (unsigned i = 0; i < N_IA32_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->caller_saves, i))
			continue;
		arch_register_t const *const reg = &ia32_registers[i];
		unsigned               const op  = o++;
		arch_set_irn_register_req_out(call, op, reg->single_req);
		if (!rbitset_is_set(allocatable_regs, reg->global_index))
			arch_set_irn_register_out(call, op, reg);
	}
	assert(o == n_out);

	/* IncSp to destroy callframe. */
	ir_node       *new_stack   = be_new_Proj(call, pn_ia32_Call_stack);
	unsigned const reduce_size = param_stacksize - cconv->sp_delta;
	if (reduce_size > 0)
		new_stack = ia32_new_IncSP(block, new_stack, -(int)reduce_size, false);
	be_stack_record_chain(&stack_env, callframe, n_be_IncSP_pred, new_stack);

	x86_free_calling_convention(cconv);

	return res;
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *call     = get_Proj_pred(node);
	ir_node *new_call = be_transform_node(call);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return be_new_Proj(new_call, pn_ia32_Call_mem);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("unexpected Call proj %+F", node);
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	ir_node *const call     = get_Proj_pred(get_Proj_pred(node));
	ir_node *const new_call = be_transform_node(call);
	unsigned const pn       = get_Proj_num(node);
	unsigned const new_pn   = pn_ia32_Call_first_result + pn;
	return be_new_Proj(new_call, new_pn);
}

/**
 * Transform Builtin trap
 */
static ir_node *gen_trap(ir_node *node)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_nodes_block(node);
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));
	return new_bd_ia32_UD2(dbgi, block, mem);
}

/**
 * Transform Builtin debugbreak
 */
static ir_node *gen_debugbreak(ir_node *node)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_nodes_block(node);
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));
	return new_bd_ia32_Breakpoint(dbgi, block, mem);
}

static ir_node *create_frame_load(dbg_info *const dbgi, ir_node *const block,
                                  ir_node *const base, x86_imm32_t const imm)
{
	ir_node *const load = new_bd_ia32_Load(dbgi, block, base, noreg_GP, nomem,
	                                       X86_SIZE_32, false);
	set_ia32_op_type(load, ia32_AddrModeS);
	ia32_attr_t *const attr = get_ia32_attr(load);
	attr->addr.variant   = X86_ADDR_BASE;
	attr->addr.immediate = imm;
	return be_new_Proj(load, pn_ia32_Load_res);
}

static ir_node *climb_frame(dbg_info *const dbgi, ir_node *const block,
                            unsigned const levels)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *ptr = get_irg_frame(irg);
	for (unsigned i = 0; i < levels; ++i) {
		x86_imm32_t const imm = i == 0
			? (x86_imm32_t) { .kind = X86_IMM_FRAMEOFFSET, .offset = -4 }
			: (x86_imm32_t) { .kind = X86_IMM_VALUE,       .offset =  0 };
		ptr = create_frame_load(dbgi, block, ptr, imm);
	}
	return ptr;
}

static ir_node *gen_return_address(ir_node *node)
{
	dbg_info     *const dbgi     = get_irn_dbg_info(node);
	ir_node      *const block    = be_transform_nodes_block(node);
	ir_node      *const n_frames = get_Builtin_param(node, 0);
	unsigned long const value    = get_Const_long(n_frames);
	if (value > 256)
		panic("Enormeous level argument for return_address builtin");

	ir_node *const base = climb_frame(dbgi, block, value);
	x86_imm32_t const imm = value == 0
		? (x86_imm32_t) { .kind = X86_IMM_FRAMEOFFSET, .offset = 0 }
		: (x86_imm32_t) { .kind = X86_IMM_VALUE,       .offset = 4 };
	return create_frame_load(dbgi, block, base, imm);
}

static ir_node *gen_frame_address(ir_node *node)
{
	dbg_info     *const dbgi     = get_irn_dbg_info(node);
	ir_node      *const block    = be_transform_nodes_block(node);
	ir_node      *const n_frames = get_Builtin_param(node, 0);
	unsigned long const value    = get_Const_long(n_frames);
	if (value > 256)
		panic("Enormeous level argument for frame_address builtin");

	return climb_frame(dbgi, block, value);
}

/**
 * Transform Builtin frame_address
 */
static ir_node *gen_prefetch(ir_node *node)
{
	if (!ia32_cg_config.use_sse_prefetch && !ia32_cg_config.use_3dnow_prefetch) {
		/* no prefetch at all, route memory */
		return be_transform_node(get_Builtin_mem(node));
	}

	size_t const n_params = get_Builtin_n_params(node);
	long   const rw       = n_params > 1 ? get_Const_long(get_Builtin_param(node, 1)) : 0;

	/* construct load address */
	ir_node      *const ptr     = get_Builtin_param(node, 0);
	ir_node      *const old_mem = get_Builtin_mem(node);
	x86_address_t addr;
	build_address_ptr(&addr, ptr, old_mem, x86_create_am_normal);
	ir_node  *const base  = addr.base;
	ir_node  *const idx   = addr.index;
	ir_node  *const mem   = addr.mem;
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);

	ir_node *new_node;
	if (rw == 1 && ia32_cg_config.use_3dnow_prefetch) {
		/* we have 3DNow!, this was already checked above */
		new_node = new_bd_ia32_PrefetchW(dbgi, block, base, idx, mem);
	} else if (ia32_cg_config.use_sse_prefetch) {
		/* note: rw == 1 is IGNORED in that case */
		long const locality = n_params > 2 ? get_Const_long(get_Builtin_param(node, 2)) : 3;

		/* SSE style prefetch */
		switch (locality) {
		case 0:
			new_node = new_bd_ia32_PrefetchNTA(dbgi, block, base, idx, mem);
			break;
		case 1:
			new_node = new_bd_ia32_PrefetchT2(dbgi, block, base, idx, mem);
			break;
		case 2:
			new_node = new_bd_ia32_PrefetchT1(dbgi, block, base, idx, mem);
			break;
		default:
			new_node = new_bd_ia32_PrefetchT0(dbgi, block, base, idx, mem);
			break;
		}
	} else {
		assert(ia32_cg_config.use_3dnow_prefetch);
		/* 3DNow! style prefetch */
		new_node = new_bd_ia32_Prefetch(dbgi, block, base, idx, mem);
	}

	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeS);
	set_address(new_node, &addr);
	return new_node;
}

/**
 * Transform bsf like node
 */
static ir_node *gen_unop_AM(ir_node *const node, construct_binop_dest_func *const func, match_flags_t const flags)
{
	ir_node            *param = get_Builtin_param(node, 0);
	ir_node            *block = get_nodes_block(node);
	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, param, NULL, flags);

	dbg_info       *const dbgi      = get_irn_dbg_info(node);
	ir_node        *const new_block = be_transform_node(block);
	x86_address_t  *const addr      = &am.addr;
	x86_insn_size_t const size      = x86_size_from_mode(get_irn_mode(param));
	ir_node  *cnt = func(dbgi, new_block, addr->base, addr->index, addr->mem,
	                     am.new_op2, size);
	set_am_attributes(cnt, &am);
	return fix_mem_proj(cnt, &am);
}

/**
 * Transform builtin ffs.
 */
static ir_node *gen_ffs(ir_node *node)
{
	ir_node *bsf  = gen_unop_AM(node, new_bd_ia32_Bsf, match_am);
	ir_node *real = skip_Proj(bsf);

	/* bsf x */
	if (get_irn_mode(real) != mode_T) {
		set_irn_mode(real, mode_T);
		bsf = be_new_Proj(real, pn_ia32_Bsf_res);
	}

	ir_node *const flag = be_new_Proj(real, pn_ia32_flags);

	/* sete */
	dbg_info *dbgi  = get_irn_dbg_info(real);
	ir_node  *block = get_nodes_block(real);
	ir_node  *set   = new_bd_ia32_Setcc(dbgi, block, flag, x86_cc_equal);

	/* conv to 32bit */
	ir_node *conv = new_bd_ia32_Conv_I2I_8bit(dbgi, block, noreg_GP, noreg_GP,
	                                          nomem, set, X86_SIZE_8, false);

	/* neg */
	ir_node *neg = new_bd_ia32_Neg(dbgi, block, conv, X86_SIZE_32);

	/* or */
	ir_node *orn = new_bd_ia32_Or(dbgi, block, noreg_GP, noreg_GP, nomem, bsf,
	                              neg, X86_SIZE_32);
	set_ia32_commutative(orn);

	/* add 1 */
	ir_node *add1 = create_lea(dbgi, block, orn, noreg_GP, 0, 1);
	return add1;
}

/**
 * Transform builtin clz.
 */
static ir_node *gen_clz(ir_node *node)
{
	ir_node  *bsr   = gen_unop_AM(node, new_bd_ia32_Bsr, match_am);
	ir_node  *real  = skip_Proj(bsr);
	dbg_info *dbgi  = get_irn_dbg_info(real);
	ir_node  *block = get_nodes_block(real);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *imm   = ia32_create_Immediate(irg, 31);

	return new_bd_ia32_Xor(dbgi, block, noreg_GP, noreg_GP, nomem, bsr, imm,
	                       X86_SIZE_32);
}

/**
 * Transform builtin ctz.
 */
static ir_node *gen_ctz(ir_node *node)
{
	return gen_unop_AM(node, new_bd_ia32_Bsf, match_am);
}

/**
 * Transform builtin parity.
 */
static ir_node *gen_parity(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *param     = get_Builtin_param(node, 0);
	ir_node  *new_param = be_transform_node(param);

	/* the x86 parity bit is stupid: it only looks at the lowest byte,
	 * so we have to do complicated xoring first.
	 * (we should also better lower this before the backend so we still have a
	 * chance for CSE, constant folding and other goodies for some of these
	 * operations) */
	ir_graph *const irg   = get_irn_irg(new_block);
	ir_node  *const count = ia32_create_Immediate(irg, 16);
	ir_node  *const shr   = new_bd_ia32_Shr(dbgi, new_block, new_param, count,
	                                        X86_SIZE_32);
	ir_node  *const xorn  = new_bd_ia32_Xor(dbgi, new_block, noreg_GP,
	                                        noreg_GP, nomem, shr, new_param,
	                                        X86_SIZE_32);
	ir_node  *const xor2  = new_bd_ia32_XorHighLow(dbgi, new_block, xorn);
	set_ia32_commutative(xorn);

	ir_node *const flags = be_new_Proj(xor2, pn_ia32_XorHighLow_flags);

	/* setp */
	ir_node *new_node = new_bd_ia32_Setcc(dbgi, new_block, flags,
	                                      x86_cc_not_parity);

	/* conv to 32bit */
	return new_bd_ia32_Conv_I2I_8bit(dbgi, new_block, noreg_GP, noreg_GP, nomem, new_node, X86_SIZE_8, false);
}

/**
 * Transform builtin popcount
 */
static ir_node *gen_popcount(ir_node *node)
{
	/* builtin lowerer should have replaced the popcount if !use_popcount */
	assert(ia32_cg_config.use_popcnt);

	return gen_unop_AM(node, new_bd_ia32_Popcnt, match_am | match_16bit_am | match_zero_ext);
}

/**
 * Transform builtin byte swap.
 */
static ir_node *gen_bswap(ir_node *node)
{
	ir_node  *param     = get_Builtin_param(node, 0);
	ir_node  *new_param = be_transform_node(param);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_mode  *mode      = get_irn_mode(param);
	unsigned  size      = get_mode_size_bits(mode);

	switch (size) {
	case 32:
		if (ia32_cg_config.use_bswap) {
			/* swap available */
			return new_bd_ia32_Bswap(dbgi, new_block, new_param, X86_SIZE_32);
		} else {
			ir_graph *const irg  = get_irn_irg(new_block);
			ir_node  *const i8   = ia32_create_Immediate(irg, 8);
			ir_node  *const rol1 = new_bd_ia32_Rol(dbgi, new_block, new_param,
			                                       i8, X86_SIZE_16);
			ir_node  *const i16  = ia32_create_Immediate(irg, 16);
			ir_node  *const rol2 = new_bd_ia32_Rol(dbgi, new_block, rol1, i16,
			                                       X86_SIZE_32);
			ir_node  *const rol3 = new_bd_ia32_Rol(dbgi, new_block, rol2, i8,
			                                       X86_SIZE_16);
			return rol3;
		}

	case 16: {
		ir_graph *const irg = get_irn_irg(new_block);
		ir_node  *const i8  = ia32_create_Immediate(irg, 8);
		ir_node  *const rol = new_bd_ia32_Rol(dbgi, new_block, new_param, i8,
		                                      X86_SIZE_16);
		return rol;
	}

	default:
		panic("invalid bswap size (%d)", size);
	}
}

/**
 * Transform builtin outport.
 */
static ir_node *gen_outport(ir_node *node)
{
	ir_node        *const param = get_Builtin_param(node, 0);
	ir_node        *const port  = create_immediate_or_transform(param, 'N');
	ir_node        *const oldv  = get_Builtin_param(node, 1);
	ir_mode        *const mode  = get_irn_mode(oldv);
	ir_node        *const value = be_transform_node(oldv);
	ir_node        *const block = be_transform_nodes_block(node);
	ir_node        *const mem   = be_transform_node(get_Builtin_mem(node));
	dbg_info       *const dbgi  = get_irn_dbg_info(node);
	x86_insn_size_t const size  = x86_size_from_mode(mode);
	ir_node *res = new_bd_ia32_Outport(dbgi, block, port, value, mem, size);
	return res;
}

/**
 * Transform builtin inport.
 */
static ir_node *gen_inport(ir_node *node)
{
	ir_type        *const tp    = get_Builtin_type(node);
	ir_type        *const rstp  = get_method_res_type(tp, 0);
	ir_mode        *const mode  = get_type_mode(rstp);
	ir_node        *const param = get_Builtin_param(node, 0);
	ir_node        *const port  = create_immediate_or_transform(param, 'N');
	ir_node        *const block = be_transform_nodes_block(node);
	ir_node        *const mem   = be_transform_node(get_Builtin_mem(node));
	dbg_info       *const dbgi  = get_irn_dbg_info(node);
	x86_insn_size_t const size  = x86_size_from_mode(mode);
	ir_node *res = new_bd_ia32_Inport(dbgi, block, port, mem, size);
	return res;
}

/*
 * Transform saturating increment.
 */
static ir_node *gen_saturating_increment(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = be_transform_nodes_block(node);
	ir_node  *operand   = be_transform_node(get_Builtin_param(node, 0));
	ir_graph *irg       = get_irn_irg(block);
	ir_node  *one       = ia32_create_Immediate(irg, 1);
	ir_node  *increment = new_bd_ia32_Add(dbgi, block, noreg_GP, noreg_GP,
	                                      nomem, operand, one, X86_SIZE_32);
	set_irn_mode(increment, mode_T);
	set_ia32_commutative(increment);
	/* We cannot use source address mode and immediate at the same time. */
	set_ia32_am_support(increment, ia32_am_none);

	ir_node *const value  = be_new_Proj(increment, pn_ia32_Add_res);
	ir_node *const eflags = be_new_Proj(increment, pn_ia32_Add_flags);
	ir_node *const zero   = ia32_create_Immediate(irg, 0);
	return new_bd_ia32_Sbb(dbgi, block, noreg_GP, noreg_GP, nomem, value, zero, eflags, X86_SIZE_32);
}

static ir_node *gen_compare_swap(ir_node *node)
{
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *ptr     = get_Builtin_param(node, 0);
	ir_node  *old     = get_Builtin_param(node, 1);
	ir_node  *new     = get_Builtin_param(node, 2);
	ir_node  *mem     = get_Builtin_mem(node);
	ir_node  *new_old = be_transform_node(old);
	ir_node  *new_new = be_transform_node(new);
	ir_mode  *mode    = get_irn_mode(new);
	assert(get_irn_mode(old) == mode);

	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem, x86_create_am_normal);
	x86_insn_size_t size = x86_size_from_mode(mode);
	ir_node *new_node = new_bd_ia32_CmpXChgMem(dbgi, block, addr.base, addr.index, addr.mem, new_old, new_new, size);
	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeD);
	set_address(new_node, &addr);
	return new_node;
}

static ir_node *create_lea_frameaddress(dbg_info *const dbgi,
                                        ir_node *const block,
                                        ir_entity *const entity)
{
	ir_graph *const irg   = get_irn_irg(block);
	ir_node  *const frame = get_irg_frame(irg);
	ir_node  *const lea   = new_bd_ia32_Lea(dbgi, block, frame, noreg_GP);
	set_ia32_frame_use(lea, IA32_FRAME_USE_AUTO);
	ia32_attr_t *const attr = get_ia32_attr(lea);
	attr->addr = (x86_addr_t) {
		.immediate = {
			.kind   = X86_IMM_FRAMEENT,
			.entity = entity,
		},
		.variant = X86_ADDR_BASE,
	};
	return lea;
}

static ir_node *gen_va_start(ir_node *node)
{
	if (initial_va_list == NULL) {
		dbg_info *dbgi  = get_irn_dbg_info(node);
		ir_graph *irg   = get_irn_irg(node);
		ir_node  *block = get_irg_start_block(irg);
		initial_va_list = create_lea_frameaddress(dbgi, block,
		                                          current_cconv->va_start_addr);
	}

	return initial_va_list;
}

/**
 * Transform Builtin node.
 */
static ir_node *gen_Builtin(ir_node *node)
{
	ir_builtin_kind kind = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_trap:
		return gen_trap(node);
	case ir_bk_debugbreak:
		return gen_debugbreak(node);
	case ir_bk_return_address:
		return gen_return_address(node);
	case ir_bk_frame_address:
		return gen_frame_address(node);
	case ir_bk_prefetch:
		return gen_prefetch(node);
	case ir_bk_ffs:
		return gen_ffs(node);
	case ir_bk_clz:
		return gen_clz(node);
	case ir_bk_ctz:
		return gen_ctz(node);
	case ir_bk_parity:
		return gen_parity(node);
	case ir_bk_popcount:
		return gen_popcount(node);
	case ir_bk_bswap:
		return gen_bswap(node);
	case ir_bk_outport:
		return gen_outport(node);
	case ir_bk_inport:
		return gen_inport(node);
	case ir_bk_saturating_increment:
		return gen_saturating_increment(node);
	case ir_bk_compare_swap:
		return gen_compare_swap(node);
	case ir_bk_va_start:
		return gen_va_start(node);
	case ir_bk_may_alias:
	case ir_bk_va_arg:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

/**
 * Transform Proj(Builtin) node.
 */
static ir_node *gen_Proj_Builtin(ir_node *proj)
{
	ir_node         *node     = get_Proj_pred(proj);
	ir_node         *new_node = be_transform_node(node);
	ir_builtin_kind  kind     = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_ffs:
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_bswap:
	case ir_bk_saturating_increment:
		assert(get_Proj_num(proj) == pn_Builtin_max + 1);
		return new_node;
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_prefetch:
	case ir_bk_outport:
		assert(get_Proj_num(proj) == pn_Builtin_M);
		return new_node;
	case ir_bk_inport:
		if (get_Proj_num(proj) == pn_Builtin_max + 1) {
			return be_new_Proj(new_node, pn_ia32_Inport_res);
		} else {
			assert(get_Proj_num(proj) == pn_Builtin_M);
			return be_new_Proj(new_node, pn_ia32_Inport_M);
		}
	case ir_bk_compare_swap:
		assert(is_ia32_CmpXChgMem(new_node));
		if (get_Proj_num(proj) == pn_Builtin_M) {
			return be_new_Proj(new_node, pn_ia32_CmpXChgMem_M);
		} else {
			assert(get_Proj_num(proj) == pn_Builtin_max + 1);
			return be_new_Proj(new_node, pn_ia32_CmpXChgMem_res);
		}
	case ir_bk_va_start:
		switch(get_Proj_num(proj)) {
		case pn_Builtin_M: {
			ir_node *mem = get_Builtin_mem(node);
			return be_transform_node(mem);
		}
		case pn_Builtin_max + 1:
			return new_node;
		}
		break;
	case ir_bk_may_alias:
	case ir_bk_va_arg:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

ir_node *ia32_new_IncSP(ir_node *block, ir_node *old_sp, int offset,
                        bool no_align)
{
	ir_node *const incsp = be_new_IncSP(block, old_sp, offset, no_align);
	arch_add_irn_flags(incsp, arch_irn_flag_modify_flags);
	return incsp;
}

static ir_node *gen_ASM(ir_node *node)
{
	ia32_request_x87_sim(get_irn_irg(node)); /* asm might have fp operands. */
	return x86_match_ASM(node, &ia32_asm_constraints);
}

static ir_node *gen_Proj_Proj(ir_node *node)
{
	ir_node *pred      = get_Proj_pred(node);
	ir_node *pred_pred = get_Proj_pred(pred);
	if (is_Start(pred_pred)) {
		return gen_Proj_Proj_Start(node);
	} else if (is_Call(pred_pred)) {
		return gen_Proj_Proj_Call(node);
	}
	panic("unexpected Proj(Proj(%+F))", pred_pred);
}

/**
 * Enters all transform functions into the generic pointer
 */
static void register_transformers(void)
{
	/* first clear the generic function pointer for all ops */
	be_start_transform_setup();

	be_set_transform_function(op_Add,              gen_Add);
	be_set_transform_function(op_Address,          gen_Address);
	be_set_transform_function(op_Alloc,            gen_Alloc);
	be_set_transform_function(op_And,              gen_And);
	be_set_transform_function(op_ASM,              gen_ASM);
	be_set_transform_function(op_Bitcast,          gen_Bitcast);
	be_set_transform_function(op_Builtin,          gen_Builtin);
	be_set_transform_function(op_Call,             gen_Call);
	be_set_transform_function(op_Cmp,              gen_Cmp);
	be_set_transform_function(op_Cond,             gen_Cond);
	be_set_transform_function(op_Const,            gen_Const);
	be_set_transform_function(op_Conv,             gen_Conv);
	be_set_transform_function(op_CopyB,            gen_CopyB);
	be_set_transform_function(op_Div,              gen_Div);
	be_set_transform_function(op_Eor,              gen_Eor);
	be_set_transform_function(op_ia32_GetEIP,      be_duplicate_node);
	be_set_transform_function(op_ia32_l_Adc,       gen_ia32_l_Adc);
	be_set_transform_function(op_ia32_l_Add,       gen_ia32_l_Add);
	be_set_transform_function(op_ia32_l_FloattoLL, gen_ia32_l_FloattoLL);
	be_set_transform_function(op_ia32_l_IMul,      gen_ia32_l_IMul);
	be_set_transform_function(op_ia32_l_LLtoFloat, gen_ia32_l_LLtoFloat);
	be_set_transform_function(op_ia32_l_Minus64,   gen_ia32_l_Minus64);
	be_set_transform_function(op_ia32_l_Mul,       gen_ia32_l_Mul);
	be_set_transform_function(op_ia32_l_Sbb,       gen_ia32_l_Sbb);
	be_set_transform_function(op_ia32_l_Sub,       gen_ia32_l_Sub);
	be_set_transform_function(op_IJmp,             gen_IJmp);
	be_set_transform_function(op_Jmp,              gen_Jmp);
	be_set_transform_function(op_Load,             gen_Load);
	be_set_transform_function(op_Member,           gen_Member);
	be_set_transform_function(op_Minus,            gen_Minus);
	be_set_transform_function(op_Mod,              gen_Mod);
	be_set_transform_function(op_Mul,              gen_Mul);
	be_set_transform_function(op_Mulh,             gen_Mulh);
	be_set_transform_function(op_Mux,              gen_Mux);
	be_set_transform_function(op_Not,              gen_Not);
	be_set_transform_function(op_Or,               gen_Or);
	be_set_transform_function(op_Phi,              gen_Phi);
	be_set_transform_function(op_Return,           gen_Return);
	be_set_transform_function(op_Shl,              gen_Shl);
	be_set_transform_function(op_Shr,              gen_Shr);
	be_set_transform_function(op_Shrs,             gen_Shrs);
	be_set_transform_function(op_Start,            gen_Start);
	be_set_transform_function(op_Store,            gen_Store);
	be_set_transform_function(op_Sub,              gen_Sub);
	be_set_transform_function(op_Switch,           gen_Switch);
	be_set_transform_function(op_Unknown,          gen_Unknown);
	be_set_transform_function(op_be_Relocation,    gen_be_Relocation);
	be_set_transform_proj_function(op_Alloc,            gen_Proj_Alloc);
	be_set_transform_proj_function(op_Builtin,          gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,             gen_Proj_Call);
	be_set_transform_proj_function(op_Div,              gen_Proj_Div);
	be_set_transform_proj_function(op_ia32_l_Adc,       be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Add,       be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_FloattoLL, gen_Proj_l_FloattoLL);
	be_set_transform_proj_function(op_ia32_l_IMul,      be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_LLtoFloat, be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Minus64,   be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Mul,       be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Sbb,       be_gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Sub,       be_gen_Proj_default);
	be_set_transform_proj_function(op_Load,             gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,              gen_Proj_Mod);
	be_set_transform_proj_function(op_Proj,             gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,            gen_Proj_Start);
	be_set_transform_proj_function(op_Store,            gen_Proj_Store);

	be_set_upper_bits_clean_function(op_Mux, ia32_mux_upper_bits_clean);
}

static void ia32_pretransform_node(ir_graph *irg)
{
	nomem    = get_irg_no_mem(irg);
	noreg_GP = ia32_new_NoReg_gp(irg);
}

/* do the transformation */
void ia32_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	switch (ir_platform.pic_style) {
	case BE_PIC_NONE:
		lconst_variant  = X86_ADDR_JUST_IMM;
		lconst_imm_kind = X86_IMM_ADDR;
		break;
	case BE_PIC_MACH_O:
		lconst_variant  = X86_ADDR_BASE;
		lconst_imm_kind = X86_IMM_PICBASE_REL;
		break;
	case BE_PIC_ELF_PLT:
	case BE_PIC_ELF_NO_PLT:
		lconst_variant  = X86_ADDR_BASE;
		lconst_imm_kind = X86_IMM_GOTOFF;
		break;
	}
	/* fix get_eip mode ia32_pic sets it to mode_P */
	ir_node *const get_eip = ia32_get_irg_data(irg)->get_eip;
	if (get_eip != NULL)
		set_irn_mode(get_eip, ia32_mode_gp);

	register_transformers();

	be_stack_init(&stack_env);
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *mtp    = get_entity_type(entity);
	current_cconv = ia32_decide_calling_convention(mtp, irg);
	x86_layout_param_entities(irg, current_cconv, IA32_REGISTER_SIZE);
	be_add_parameter_entity_stores(irg);
	x86_create_parameter_loads(irg, current_cconv);

	be_timer_push(T_HEIGHTS);
	heights = heights_new(irg);
	be_timer_pop(T_HEIGHTS);
	x86_calculate_non_address_mode_nodes(irg);

	/* the transform phase is not safe for CSE (yet) because several nodes get
	 * attributes set after their creation */
	int cse_last = get_opt_cse();
	set_opt_cse(0);

	be_transform_graph(irg, ia32_pretransform_node);

	set_opt_cse(cse_last);

	x86_free_non_address_mode_nodes();
	heights_free(heights);
	heights = NULL;
	be_stack_finish(&stack_env);
	x86_free_calling_convention(current_cconv);
	initial_va_list = NULL;
}

void ia32_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.transform");
}
