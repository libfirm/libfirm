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
#include <limits.h>
#include <stdbool.h>

#include "irargs_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "iredges_t.h"
#include "irouts.h"
#include "irgmod.h"
#include "ircons.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "debug.h"
#include "irdom.h"
#include "iropt.h"
#include "panic.h"
#include "array.h"
#include "heights.h"

#include "benode.h"
#include "besched.h"
#include "beabi.h"
#include "beutil.h"
#include "betranshlp.h"
#include "be_t.h"

#include "bearch_ia32_t.h"
#include "ia32_common_transform.h"
#include "ia32_nodes_attr.h"
#include "ia32_transform.h"
#include "ia32_new_nodes.h"
#include "ia32_dbg_stat.h"
#include "ia32_optimize.h"
#include "ia32_architecture.h"
#include "x86_address_mode.h"

#include "gen_ia32_regalloc_if.h"

/* define this to construct SSE constants instead of load them */
#undef CONSTRUCT_SSE_CONST

#define mode_fp     (ia32_reg_classes[CLASS_ia32_fp].mode)
#define mode_xmm    (ia32_reg_classes[CLASS_ia32_xmm].mode)

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static ir_node *old_initial_fpcw;
static ir_node *initial_fpcw;
bool            ia32_no_pic_adjust;

typedef ir_node *construct_binop_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op1,
        ir_node *op2);

typedef ir_node *construct_binop_flags_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op1, ir_node *op2,
        ir_node *flags);

typedef ir_node *construct_shift_func(dbg_info *db, ir_node *block,
        ir_node *op1, ir_node *op2);

typedef ir_node *construct_binop_dest_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op);

typedef ir_node *construct_unop_dest_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem);

typedef ir_node *construct_binop_float_func(dbg_info *db, ir_node *block,
        ir_node *base, ir_node *index, ir_node *mem, ir_node *op1, ir_node *op2,
        ir_node *fpcw);

typedef ir_node *construct_unop_func(dbg_info *db, ir_node *block, ir_node *op);

static ir_node *create_immediate_or_transform(ir_node *node);

static ir_node *create_I2I_Conv(ir_mode *src_mode, ir_mode *tgt_mode,
                                dbg_info *dbgi, ir_node *block,
                                ir_node *op, ir_node *orig_node);

/* its enough to have those once */
static ir_node *nomem;
static ir_node *noreg_GP;

/** a list to postprocess all calls */
static ir_node **call_list;
static ir_type **call_types;

/** Return non-zero is a node represents the 0 constant. */
static bool is_Const_0(ir_node *node)
{
	return is_Const(node) && is_Const_null(node);
}

/** Return non-zero is a node represents the 1 constant. */
static bool is_Const_1(ir_node *node)
{
	return is_Const(node) && is_Const_one(node);
}

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

	if (mode == ia32_mode_float32)
		return true;

	if (tarval_is_null(tv)
#ifdef CONSTRUCT_SSE_CONST
	    || tarval_is_one(tv)
#endif
	   )
		return true;
#ifdef CONSTRUCT_SSE_CONST
	if (mode == ia32_mode_float64) {
		unsigned val = get_tarval_sub_bits(tv, 0) |
			(get_tarval_sub_bits(tv, 1) << 8) |
			(get_tarval_sub_bits(tv, 2) << 16) |
			(get_tarval_sub_bits(tv, 3) << 24);
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
	ia32_irg_data_t *irg_data = ia32_get_irg_data(irg);
	ir_node         *block;
	ir_node         *get_eip = irg_data->get_eip;
	if (get_eip != NULL)
		return get_eip;

	block             = get_irg_start_block(irg);
	get_eip           = new_bd_ia32_GetEIP(NULL, block);
	irg_data->get_eip = get_eip;

	return get_eip;
}

/**
 * return NoREG or pic_base in case of PIC.
 * This is necessary as base address for newly created symbols
 */
static ir_node *get_global_base(ir_graph *const irg)
{
	if (be_options.pic) {
		return ia32_get_pic_base(irg);
	}

	return noreg_GP;
}

/**
 * Transforms a Const.
 */
static ir_node *gen_Const(ir_node *node)
{
	ir_node   *old_block = get_nodes_block(node);
	ir_node   *block     = be_transform_node(old_block);
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_mode   *mode      = get_irn_mode(node);
	ir_tarval *tv        = get_Const_tarval(node);

	if (mode_is_float(mode)) {
		ir_graph         *irg      = get_irn_irg(node);
		const arch_env_t *arch_env = be_get_irg_arch_env(irg);
		ia32_isa_t       *isa      = (ia32_isa_t*) arch_env;
		ir_node          *res      = NULL;
		ir_node          *load;

		if (ia32_cg_config.use_sse2) {
			if (tarval_is_null(tv)) {
				load = new_bd_ia32_xZero(dbgi, block);
				set_ia32_ls_mode(load, mode);
				res  = load;
#ifdef CONSTRUCT_SSE_CONST
			} else if (tarval_is_one(tv)) {
				int     cnst  = mode == ia32_mode_float32 ? 26 : 55;
				ir_node *imm1 = ia32_create_Immediate(irg, NULL, 0, cnst);
				ir_node *imm2 = ia32_create_Immediate(irg, NULL, 0, 2);
				ir_node *pslld, *psrld;

				load = new_bd_ia32_xAllOnes(dbgi, block);
				set_ia32_ls_mode(load, mode);
				pslld = new_bd_ia32_xPslld(dbgi, block, load, imm1);
				set_ia32_ls_mode(pslld, mode);
				psrld = new_bd_ia32_xPsrld(dbgi, block, pslld, imm2);
				set_ia32_ls_mode(psrld, mode);
				res = psrld;
#endif /* CONSTRUCT_SSE_CONST */
			} else if (mode == ia32_mode_float32) {
				/* we can place any 32bit constant by using a movd gp, sse */
				unsigned val = get_tarval_sub_bits(tv, 0) |
				               (get_tarval_sub_bits(tv, 1) << 8) |
				               (get_tarval_sub_bits(tv, 2) << 16) |
				               (get_tarval_sub_bits(tv, 3) << 24);
				ir_node *cnst = new_bd_ia32_Const(dbgi, block, NULL, 0, val);
				load = new_bd_ia32_xMovd(dbgi, block, cnst);
				set_ia32_ls_mode(load, mode);
				res = load;
			} else {
#ifdef CONSTRUCT_SSE_CONST
				if (mode == ia32_mode_float64) {
					unsigned val = get_tarval_sub_bits(tv, 0) |
						(get_tarval_sub_bits(tv, 1) << 8) |
						(get_tarval_sub_bits(tv, 2) << 16) |
						(get_tarval_sub_bits(tv, 3) << 24);
					if (val == 0) {
						ir_node *imm32 = ia32_create_Immediate(irg, NULL, 0, 32);
						ir_node *cnst, *psllq;

						/* fine, lower 32bit are zero, produce 32bit value */
						val = get_tarval_sub_bits(tv, 4) |
							(get_tarval_sub_bits(tv, 5) << 8) |
							(get_tarval_sub_bits(tv, 6) << 16) |
							(get_tarval_sub_bits(tv, 7) << 24);
						cnst = new_bd_ia32_Const(dbgi, block, NULL, 0, 0, val);
						load = new_bd_ia32_xMovd(dbgi, block, cnst);
						set_ia32_ls_mode(load, mode);
						psllq = new_bd_ia32_xPsllq(dbgi, block, load, imm32);
						set_ia32_ls_mode(psllq, mode);
						res = psllq;
						goto end;
					}
				}
#endif /* CONSTRUCT_SSE_CONST */
				ir_entity *floatent
					= ia32_create_float_const_entity(isa, tv, NULL);

				ir_node *base = get_global_base(irg);
				load = new_bd_ia32_xLoad(dbgi, block, base, noreg_GP, nomem,
				                         mode);
				set_irn_pinned(load, op_pin_state_floats);
				set_ia32_op_type(load, ia32_AddrModeS);
				set_ia32_am_ent(load, floatent);
				arch_add_irn_flags(load, arch_irn_flag_rematerializable);
				res = new_r_Proj(load, mode_xmm, pn_ia32_xLoad_res);
			}
		} else {
			if (tarval_is_null(tv)) {
				load = new_bd_ia32_fldz(dbgi, block);
				res  = load;
			} else if (tarval_is_one(tv)) {
				load = new_bd_ia32_fld1(dbgi, block);
				res  = load;
			} else {
				ir_entity *floatent
					= ia32_create_float_const_entity(isa, tv, NULL);
				/* create_float_const_ent is smart and sometimes creates
				   smaller entities */
				ir_mode *ls_mode  = get_type_mode(get_entity_type(floatent));
				ir_node *base     = get_global_base(irg);
				load = new_bd_ia32_fld(dbgi, block, base, noreg_GP, nomem,
				                       ls_mode);
				set_irn_pinned(load, op_pin_state_floats);
				set_ia32_op_type(load, ia32_AddrModeS);
				set_ia32_am_ent(load, floatent);
				arch_add_irn_flags(load, arch_irn_flag_rematerializable);
				res = new_r_Proj(load, mode_fp, pn_ia32_fld_res);
			}
		}
#ifdef CONSTRUCT_SSE_CONST
end:
#endif /* CONSTRUCT_SSE_CONST */
		SET_IA32_ORIG_NODE(load, node);
		return res;
	} else { /* non-float mode */
		tv = tarval_convert_to(tv, ia32_mode_gp);

		if (tv == get_tarval_bad() || tv == get_tarval_unknown() ||
		    tv == NULL) {
			panic("couldn't convert constant tarval (%+F)", node);
		}
		long val = get_tarval_long(tv);

		ir_node *cnst = new_bd_ia32_Const(dbgi, block, NULL, 0, val);
		SET_IA32_ORIG_NODE(cnst, node);

		return cnst;
	}
}

/**
 * Transforms an Address.
 */
static ir_node *gen_Address(ir_node *node)
{
	ir_node  *old_block = get_nodes_block(node);
	ir_node  *block     = be_transform_node(old_block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_mode  *mode      = get_irn_mode(node);

	if (!ia32_mode_needs_gp_reg(mode))
		panic("unexpected mode for Address");

	ir_entity *entity = get_Address_entity(node);
	ir_node   *cnst;
	if (is_tls_entity(entity)) {
		ir_node *tls_base = new_bd_ia32_LdTls(NULL, block);
		ir_node *lea      = new_bd_ia32_Lea(dbgi, block, tls_base, noreg_GP);
		set_ia32_am_ent(lea, entity);
		cnst = lea;
	} else {
		cnst = new_bd_ia32_Const(dbgi, block, entity, 0, 0);
	}
	SET_IA32_ORIG_NODE(cnst, node);
	return cnst;
}

static ir_type *make_array_type(ir_type *tp)
{
	unsigned alignment = get_type_alignment_bytes(tp);
	unsigned size      = get_type_size_bytes(tp);
	ir_type *res       = new_type_array(tp);
	set_type_alignment_bytes(res, alignment);
	set_array_size_int(res, 2);
	if (alignment > size)
		size = alignment;
	set_type_size_bytes(res, 2 * size);
	set_type_state(res, layout_fixed);
	return res;
}

/**
 * Create a float[2] array type for the given atomic type.
 *
 * @param tp  the atomic type
 */
static ir_type *ia32_create_float_array(ir_type *tp)
{
	ir_mode *mode = get_type_mode(tp);
	ir_type *arr;

	if (mode == ia32_mode_float32) {
		static ir_type *float_F;

		arr = float_F;
		if (arr == NULL)
			arr = float_F = make_array_type(tp);
	} else if (mode == ia32_mode_float64) {
		static ir_type *float_D;

		arr = float_D;
		if (arr == NULL)
			arr = float_D = make_array_type(tp);
	} else {
		static ir_type *float_E;

		arr = float_E;
		if (arr == NULL)
			arr = float_E = make_array_type(tp);
	}
	return arr;
}

/* Generates an entity for a known FP const (used for FP Neg + Abs) */
ir_entity *ia32_gen_fp_known_const(ir_graph *const irg, ia32_known_const_t kct)
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
		const arch_env_t *arch_env = be_get_irg_arch_env(irg);
		ia32_isa_t       *isa      = (ia32_isa_t*) arch_env;
		const char       *cnst_str = names[kct].cnst_str;
		ident            *name     = new_id_from_str(names[kct].name);
		ir_mode          *mode;
		switch (names[kct].mode) {
		case 0:  mode = ia32_mode_gp; break;
		case 1:  mode = mode_Lu; break;
		case 2:  mode = ia32_mode_float32;  break;
		default: panic("internal compiler error");
		}
		ir_tarval *tv = new_tarval_from_str(cnst_str, strlen(cnst_str), mode);

		if (kct == ia32_ULLBIAS) {
			ir_type *type  = ia32_get_prim_type(ia32_mode_float32);
			ir_type *atype = ia32_create_float_array(type);

			ent = new_entity(get_glob_type(), name, atype);

			set_entity_ld_ident(ent, name);
			set_entity_visibility(ent, ir_visibility_private);
			add_entity_linkage(ent, IR_LINKAGE_CONSTANT);

			ir_initializer_t *initializer = create_initializer_compound(2);
			set_initializer_compound_value(initializer, 0,
				create_initializer_tarval(get_mode_null(mode)));
			set_initializer_compound_value(initializer, 1,
				create_initializer_tarval(tv));
			set_entity_initializer(ent, initializer);
		} else {
			ent = ia32_create_float_const_entity(isa, tv, name);
		}
		/* cache the entry */
		ent_cache[kct] = ent;
	}

	return ent_cache[kct];
}

/**
 * return true if the users of the given value will be merged by later
 * optimization. This applies to multiple Cmp nodes (and maybe a Sub
 * node) with the same inputs.
 */
static bool users_will_merge(ir_node *proj)
{
	ir_node *first     = NULL;
	ir_node *block     = NULL;
	ir_node *left      = NULL;
	ir_node *right     = NULL;
	bool     found_sub = false;

	foreach_out_edge(proj, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if (first == NULL) {
			if (is_Cmp(user) || is_Sub(user)) {
				// Take the first user as a sample to compare
				// the next ones to.
				first     = user;
				block     = get_nodes_block(user);
				left      = get_binop_left(user);
				right     = get_binop_right(user);
				found_sub = is_Sub(user);
			} else {
				return false;
			}
		} else {
			if (get_nodes_block(user) != block) {
				return false;
			}

			if (is_Cmp(user) || is_Sub(user)) {
				ir_node *user_left  = get_binop_left(user);
				ir_node *user_right = get_binop_right(user);

				if (found_sub && is_Sub(user)) {
					// Two subs will not be merged
					return false;
				}
				found_sub |= is_Sub(user);

				if ((is_Sub(user) || is_Sub(first)) &&
				    user_left == right && user_right == left) {
					continue;
				}
				if (user_left != left || user_right != right) {
					return false;
				}
			} else {
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
			} else {
				if (is_simple_x87_Const(node))
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
	assert(get_Proj_proj(node) == pn_Load_res);
	if (get_nodes_block(load) != block)
		return false;
	ir_mode *mode = get_irn_mode(node);
	/* we can't fold mode_E AM */
	if (mode == ia32_mode_E)
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
	if (other != NULL && ia32_prevents_AM(block, load, other))
		return false;

	if (other2 != NULL && ia32_prevents_AM(block, load, other2))
		return false;

	return true;
}

typedef struct ia32_address_mode_t ia32_address_mode_t;
struct ia32_address_mode_t {
	x86_address_t   addr;
	ir_mode        *ls_mode;
	ir_node        *mem_proj;
	ir_node        *am_node;
	ia32_op_type_t  op_type;
	ir_node        *new_op1;
	ir_node        *new_op2;
	op_pin_state    pinned;
	unsigned        commutative  : 1;
	unsigned        ins_permuted : 1;
};

static void build_address_ptr(x86_address_t *addr, ir_node *ptr, ir_node *mem)
{
	/* construct load address */
	memset(addr, 0, sizeof(addr[0]));
	x86_create_address_mode(addr, ptr, x86_create_am_normal);

	addr->base  = addr->base  ? be_transform_node(addr->base)  : noreg_GP;
	addr->index = addr->index ? be_transform_node(addr->index) : noreg_GP;
	addr->mem   = be_transform_node(mem);
}

static void build_address(ia32_address_mode_t *am, ir_node *node,
                          x86_create_am_flags_t flags)
{
	x86_address_t *addr = &am->addr;

	/* floating point immediates */
	if (is_Const(node)) {
		ir_graph         *irg      = get_irn_irg(node);
		const arch_env_t *arch_env = be_get_irg_arch_env(irg);
		ia32_isa_t       *isa      = (ia32_isa_t*) arch_env;
		ir_tarval        *tv       = get_Const_tarval(node);
		ir_entity *entity = ia32_create_float_const_entity(isa, tv, NULL);
		addr->base        = get_global_base(irg);
		addr->index       = noreg_GP;
		addr->mem         = nomem;
		addr->entity      = entity;
		addr->tls_segment = false;
		addr->use_frame   = false;
		am->ls_mode       = get_type_mode(get_entity_type(entity));
		am->pinned        = op_pin_state_floats;
		return;
	}

	ir_node *load    = get_Proj_pred(node);
	ir_node *ptr     = get_Load_ptr(load);
	ir_node *mem     = get_Load_mem(load);
	ir_node *new_mem = be_transform_node(mem);
	am->pinned       = get_irn_pinned(load);
	am->ls_mode      = get_Load_mode(load);
	am->mem_proj     = be_get_Proj_for_pn(load, pn_Load_M);
	am->am_node      = node;

	/* construct load address */
	x86_create_address_mode(addr, ptr, flags);

	addr->base  = addr->base  ? be_transform_node(addr->base)  : noreg_GP;
	addr->index = addr->index ? be_transform_node(addr->index) : noreg_GP;
	addr->mem   = new_mem;
}

static void set_address(ir_node *node, const x86_address_t *addr)
{
	set_ia32_am_scale(node, addr->scale);
	set_ia32_am_ent(node, addr->entity);
	set_ia32_am_offs_int(node, addr->offset);
	set_ia32_am_tls_segment(node, addr->tls_segment);
	set_ia32_frame_ent(node, addr->frame_entity);
	if (addr->use_frame)
		set_ia32_use_frame(node);
}

/**
 * Apply attributes of a given address mode to a node.
 */
static void set_am_attributes(ir_node *node, const ia32_address_mode_t *am)
{
	set_address(node, &am->addr);

	set_ia32_op_type(node, am->op_type);
	set_ia32_ls_mode(node, am->ls_mode);
	if (am->pinned == op_pin_state_pinned) {
		/* beware: some nodes are already pinned and did not allow to change the state */
		if (get_irn_pinned(node) != op_pin_state_pinned)
			set_irn_pinned(node, op_pin_state_pinned);
	}
	if (am->commutative)
		set_ia32_commutative(node);
}

/**
 * Check, if a given node is a Down-Conv, i.e. an integer Conv
 * from a mode with more bits to a mode with fewer bits.
 *
 * @param node   the node
 * @return non-zero if node is a Down-Conv
 */
static bool is_downconv(const ir_node *node)
{
	if (!is_Conv(node))
		return false;

	ir_mode *src_mode  = get_irn_mode(get_Conv_op(node));
	ir_mode *dest_mode = get_irn_mode(node);
	return
		ia32_mode_needs_gp_reg(src_mode)  &&
		ia32_mode_needs_gp_reg(dest_mode) &&
		get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

/**
 * Skip all Down-Conv's on a given node and return the resulting node.
 *
 * Moreover, we only skip Conv nodes that has at most one user.
 */
ir_node *ia32_skip_downconv(ir_node *node)
{
	while (is_downconv(node)) {
		/* we only want to skip the conv when we're the only user
		 * (because this test is used in the context of address-mode selection
		 *  and we don't want to use address mode for multiple users) */
		if (get_irn_n_edges(node) > 1)
			break;

		node = get_Conv_op(node);
	}

	return node;
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

static bool is_sameconv(ir_node *node)
{
	if (!is_Conv(node))
		return false;

	/* we only want to skip the conv when we're the only user
	 * (because this test is used in the context of address-mode selection
	 *  and we don't want to use address mode for multiple users) */
	if (get_irn_n_edges(node) > 1)
		return false;

	ir_mode *src_mode  = get_irn_mode(get_Conv_op(node));
	ir_mode *dest_mode = get_irn_mode(node);
	return
		ia32_mode_needs_gp_reg(src_mode)  &&
		ia32_mode_needs_gp_reg(dest_mode) &&
		get_mode_size_bits(dest_mode) == get_mode_size_bits(src_mode);
}

/** Skip all signedness convs */
static ir_node *ia32_skip_sameconv(ir_node *node)
{
	while (is_sameconv(node)) {
		node = get_Conv_op(node);
	}

	return node;
}

static ir_node *transform_sext(ir_node *node, ir_node *orig_node)
{
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	/* normalize to a signed mode */
	switch (get_mode_size_bits(mode)) {
	case 8:  mode = mode_Bs; break;
	case 16: mode = mode_Hs; break;
	default:
		panic("ia32: invalid mode in sest: %+F", node);
	}
	return create_I2I_Conv(mode, ia32_mode_gp, dbgi, block, node, orig_node);
}

static ir_node *transform_zext(ir_node *node, ir_node *orig_node)
{
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	/* normalize to an unsigned mode */
	switch (get_mode_size_bits(mode)) {
	case 8:  mode = mode_Bu; break;
	case 16: mode = mode_Hu; break;
	default:
		panic("ia32: invalid mode in zest: %+F", node);
	}
	return create_I2I_Conv(mode, ia32_mode_gp, dbgi, block, node, orig_node);
}

static ir_node *transform_upconv(ir_node *node, ir_node *orig_node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_signed(mode)) {
		return transform_sext(node, orig_node);
	} else {
		return transform_zext(node, orig_node);
	}
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

	/* we can simply skip downconvs for mode neutral nodes: the upper bits
	 * can be random for these operations */
	if (flags & match_mode_neutral) {
		op2 = ia32_skip_downconv(op2);
		if (op1 != NULL) {
			op1 = ia32_skip_downconv(op1);
		}
	} else {
		op2 = ia32_skip_sameconv(op2);
		if (op1 != NULL) {
			op1 = ia32_skip_sameconv(op1);
		}
	}

	/* match immediates. firm nodes are normalized: constants are always on the
	 * op2 input */
	ir_node *new_op2 = NULL;
	if (!(flags & match_try_am) && use_immediate) {
		new_op2 = ia32_try_create_Immediate(op2, 'i');
	}

	ir_node *new_op1;
	if (new_op2 == NULL &&
	    use_am && ia32_use_source_address_mode(block, op2, op1, other_op, flags)) {
		build_address(am, op2, x86_create_am_normal);
		new_op1     = (op1 == NULL ? NULL : be_transform_node(op1));
		ir_graph *const irg = get_Block_irg(block);
		new_op2     = get_noreg(irg, mode);
		am->op_type = ia32_AddrModeS;
	} else if (commutative && (new_op2 == NULL || use_am_and_immediates) &&
		       use_am &&
		       ia32_use_source_address_mode(block, op1, op2, other_op, flags)) {
		build_address(am, op1, x86_create_am_normal);

		ir_graph *const irg   = get_Block_irg(block);
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
		am->op_type = ia32_Normal;

		if (flags & match_try_am) {
			am->new_op1 = NULL;
			am->new_op2 = NULL;
			return;
		}

		mode = get_irn_mode(op2);
		if (get_mode_size_bits(mode) != 32
			&& (flags & (match_mode_neutral | match_upconv | match_zero_ext))) {
			if (flags & match_upconv) {
				new_op1 = (op1 == NULL ? NULL : transform_upconv(op1, op1));
				if (new_op2 == NULL)
					new_op2 = transform_upconv(op2, op2);
			} else if (flags & match_zero_ext) {
				new_op1 = (op1 == NULL ? NULL : transform_zext(op1, op1));
				if (new_op2 == NULL)
					new_op2 = transform_zext(op2, op2);
			} else {
				new_op1 = (op1 == NULL ? NULL : be_transform_node(op1));
				if (new_op2 == NULL)
					new_op2 = be_transform_node(op2);
				assert(flags & match_mode_neutral);
			}
			mode = ia32_mode_gp;
		} else {
			new_op1 = (op1 == NULL ? NULL : be_transform_node(op1));
			if (new_op2 == NULL)
				new_op2 = be_transform_node(op2);
		}
		am->ls_mode = mode;
	}
	x86_address_t *addr = &am->addr;
	if (addr->base == NULL)
		addr->base = noreg_GP;
	if (addr->index == NULL)
		addr->index = noreg_GP;
	if (addr->mem == NULL)
		addr->mem = nomem;

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
static ir_node *fix_mem_proj(ir_node *node, ia32_address_mode_t *am)
{
	if (am->mem_proj == NULL)
		return node;

	/* we have to create a mode_T so the old MemProj can attach to us */
	ir_mode *mode = get_irn_mode(node);
	ir_node *load = get_Proj_pred(am->mem_proj);

	be_set_transformed_node(load, node);

	if (mode != mode_T) {
		set_irn_mode(node, mode_T);
		return new_r_Proj(node, mode, pn_ia32_res);
	} else {
		return node;
	}
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

	dbg_info      *dbgi      = get_irn_dbg_info(node);
	ir_node       *new_block = be_transform_node(block);
	x86_address_t *addr      = &am.addr;
	ir_node       *new_node  = func(dbgi, new_block, addr->base, addr->index,
	                                addr->mem, am.new_op1, am.new_op2);
	set_am_attributes(new_node, &am);
	/* we can't use source address mode anymore when using immediates */
	if (!(flags & match_am_and_immediates) &&
	    (is_ia32_Immediate(am.new_op1) || is_ia32_Immediate(am.new_op2)))
		set_ia32_am_support(new_node, ia32_am_none);
	SET_IA32_ORIG_NODE(new_node, node);

	new_node = fix_mem_proj(new_node, &am);
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
	                                 new_eflags);
	set_am_attributes(new_node, &am);
	/* we can't use source address mode anymore when using immediates */
	if (!(flags & match_am_and_immediates) &&
	    (is_ia32_Immediate(am.new_op1) || is_ia32_Immediate(am.new_op2)))
		set_ia32_am_support(new_node, ia32_am_none);
	SET_IA32_ORIG_NODE(new_node, node);

	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

static ir_node *get_fpcw(void)
{
	if (initial_fpcw != NULL)
		return initial_fpcw;

	initial_fpcw = be_transform_node(old_initial_fpcw);
	return initial_fpcw;
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
	if (mode != ia32_mode_E) {
		panic("ia32: x87 only supports x86 extended float mode");
	}
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
	x86_address_t *addr = &am.addr;
	ir_node       *new_node  = func(dbgi, new_block, addr->base, addr->index,
	                                addr->mem, am.new_op1, am.new_op2,
	                                get_fpcw());
	if (am.op_type == ia32_Normal)
		am.ls_mode = ia32_mode_E;
	set_am_attributes(new_node, &am);

	ia32_x87_attr_t *attr = get_ia32_x87_attr(new_node);
	attr->attr.data.ins_permuted = am.ins_permuted;

	SET_IA32_ORIG_NODE(new_node, node);

	new_node = fix_mem_proj(new_node, &am);
	return new_node;
}

/**
 * Construct a shift/rotate binary operation, sets AM and immediate if required.
 *
 * @param op1   The first operand
 * @param op2   The second operand
 * @param func  The node constructor function
 * @return The constructed ia32 node.
 */
static ir_node *gen_shift_binop(ir_node *node, ir_node *op1, ir_node *op2,
                                construct_shift_func *func,
                                match_flags_t flags)
{
	ir_mode *mode = get_irn_mode(node);

	assert(!mode_is_float(mode));
	assert(flags & match_immediate);
	assert((flags & ~(match_mode_neutral | match_sign_ext | match_zero_ext | match_upconv | match_immediate)) == 0);

	if (get_mode_modulo_shift(mode) != 32) {
		/* TODO: implement special cases for non-modulo shifts */
		panic("modulo shift!=32 not supported by ia32 backend");
	}

	ir_node *new_op1;
	if (flags & match_mode_neutral) {
		op1     = ia32_skip_downconv(op1);
		new_op1 = be_transform_node(op1);
	} else {
		op1 = ia32_skip_sameconv(op1);
		if (get_mode_size_bits(mode) != 32) {
			if (flags & match_upconv) {
				new_op1 = transform_upconv(op1, node);
			} else if (flags & match_sign_ext) {
				new_op1 = transform_sext(op1, node);
			} else if (flags & match_zero_ext) {
				new_op1 = transform_zext(op1, node);
			} else {
				/* match_mode_neutral not handled here because it makes no
				 * sense for shift operations */
				panic("ia32 code selection failed for %+F", node);
			}
		} else {
			new_op1 = be_transform_node(op1);
		}
	}

	/* the shift amount can be any mode that is bigger than 5 bits, since all
	 * other bits are ignored anyway */
	while (is_Conv(op2) && get_irn_n_edges(op2) == 1) {
		ir_node *const op = get_Conv_op(op2);
		if (mode_is_float(get_irn_mode(op)))
			break;
		op2 = op;
		assert(get_mode_size_bits(get_irn_mode(op2)) >= 5);
	}
	ir_node *new_op2 = create_immediate_or_transform(op2);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_node  = func(dbgi, new_block, new_op1, new_op2);
	SET_IA32_ORIG_NODE(new_node, node);

	/* lowered shift instruction may have a dependency operand, handle it here */
	if (get_irn_arity(node) == 3) {
		/* we have a dependency */
		ir_node* dep = get_irn_n(node, 2);
		if (get_irn_n_edges(dep) > 1) {
			/* ... which has at least one user other than 'node' */
			ir_node *new_dep = be_transform_node(dep);
			add_irn_dep(new_node, new_dep);
		}
	}

	return new_node;
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
	if (flags & match_mode_neutral) {
		op = ia32_skip_downconv(op);
	}

	ir_node  *new_op    = be_transform_node(op);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_node  = func(dbgi, new_block, new_op);

	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
}

static ir_node *create_lea_from_address(dbg_info *dbgi, ir_node *block,
                                        x86_address_t *addr)
{
	ir_node *base = addr->base;
	if (base == NULL) {
		base = noreg_GP;
	} else {
		base = be_transform_node(base);
	}

	ir_node *idx = addr->index;
	if (idx == NULL) {
		idx = noreg_GP;
	} else {
		idx = be_transform_node(idx);
	}

	/* segment overrides are ineffective for Leas :-( so we have to patch
	 * around... */
	if (addr->tls_segment) {
		ir_node *tls_base = new_bd_ia32_LdTls(NULL, block);
		assert(addr->entity != NULL);
		if (base == noreg_GP)
			base = tls_base;
		else
			base = new_bd_ia32_Lea(dbgi, block, tls_base, base);
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
	return addr->offset != 0 || addr->entity != NULL
		|| addr->frame_entity || addr->use_frame;
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
static ir_node *gen_64bit_shifts(dbg_info *dbgi, ir_node *block,
                                 ir_node *high, ir_node *low, ir_node *count,
                                 new_shiftd_func func)
{
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_high  = be_transform_node(high);
	ir_node  *new_low   = be_transform_node(low);

	/* the shift amount can be any mode that is bigger than 5 bits, since all
	 * other bits are ignored anyway */
	while (is_Conv(count)              &&
	       get_irn_n_edges(count) == 1 &&
	       mode_is_int(get_irn_mode(count))) {
		assert(get_mode_size_bits(get_irn_mode(count)) >= 5);
		count = get_Conv_op(count);
	}
	ir_node *new_count = create_immediate_or_transform(count);

	ir_node *new_node = func(dbgi, new_block, new_high, new_low, new_count);
	return new_node;
}

/**
 * Tests whether 2 values result in 'x' and '32-x' when interpreted as a shift
 * value.
 */
static bool is_complementary_shifts(ir_node *value1, ir_node *value2)
{
	if (is_Const(value1) && is_Const(value2)) {
		ir_tarval *tv1 = get_Const_tarval(value1);
		ir_tarval *tv2 = get_Const_tarval(value2);
		if (tarval_is_long(tv1) && tarval_is_long(tv2)) {
			long v1 = get_tarval_long(tv1);
			long v2 = get_tarval_long(tv2);
			return v1 <= v2 && v2 == 32-v1;
		}
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
		if (is_complementary_shifts(shl_right, shr_right)) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = get_nodes_block(node);
			return gen_64bit_shifts(dbgi, block, shl_left, shr_left, shl_right,
			                        new_bd_ia32_ShlD);
		}
		/* constant ShrD operation */
		if (is_complementary_shifts(shr_right, shl_right)) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = get_nodes_block(node);
			return gen_64bit_shifts(dbgi, block, shr_left, shl_left, shr_right,
			                        new_bd_ia32_ShrD);
		}
		/* lower_dw produces the following for ShlD:
		 * Or(Shr(Shr(high,1),Not(c)),Shl(low,c)) */
		if (is_Shr(shr_left) && is_Not(shr_right)
			&& is_Const_1(get_Shr_right(shr_left))
		    && get_Not_op(shr_right) == shl_right) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = get_nodes_block(node);
			ir_node  *val_h = get_Shr_left(shr_left);
			return gen_64bit_shifts(dbgi, block, shl_left, val_h, shl_right,
			                        new_bd_ia32_ShlD);
		}
		/* lower_dw produces the following for ShrD:
		 * Or(Shl(Shl(high,1),Not(c)), Shr(low,c)) */
		if (is_Shl(shl_left) && is_Not(shl_right)
		    && is_Const_1(get_Shl_right(shl_left))
		    && get_Not_op(shl_right) == shr_right) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = get_nodes_block(node);
			ir_node  *val_h = get_Shl_left(shl_left);
		    return gen_64bit_shifts(dbgi, block, shr_left, val_h, shr_right,
		                            new_bd_ia32_ShrD);
		}
	}

	return NULL;
}

static ir_node *gen_Rol(ir_node *node, ir_node *op1, ir_node *op2)
{
	return gen_shift_binop(node, op1, op2, new_bd_ia32_Rol, match_immediate);
}

static ir_node *gen_Ror(ir_node *node, ir_node *op1, ir_node *op2)
{
	return gen_shift_binop(node, op1, op2, new_bd_ia32_Ror, match_immediate);
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

	ir_node *new_node = match_64bit_shift(node);
	if (new_node != NULL)
		return new_node;

	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2)
			return gen_binop(node, op1, op2, new_bd_ia32_xAdd,
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
	memset(&addr, 0, sizeof(addr));
	x86_create_address_mode(&addr, node, x86_create_am_force);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);

	/* a constant? */
	if (addr.base == NULL && addr.index == NULL) {
		new_node = new_bd_ia32_Const(dbgi, new_block, addr.entity, 0, addr.offset);
		SET_IA32_ORIG_NODE(new_node, node);
		return new_node;
	}
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
			ir_fprintf(stderr, "Optimization warning: Add x,0 (%+F) found\n",
					   node);
#endif
			return be_transform_node(add_immediate_op);
		}

		new_node = create_lea_from_address(dbgi, new_block, &addr);
		SET_IA32_ORIG_NODE(new_node, node);
		return new_node;
	}

	/* test if we can use source address mode */
	ia32_address_mode_t am;
	match_arguments(&am, block, op1, op2, NULL, match_commutative
			| match_mode_neutral | match_am | match_immediate | match_try_am);

	/* construct an Add with source address mode */
	if (am.op_type == ia32_AddrModeS) {
		x86_address_t *am_addr = &am.addr;
		new_node = new_bd_ia32_Add(dbgi, new_block, am_addr->base,
		                         am_addr->index, am_addr->mem, am.new_op1,
		                         am.new_op2);
		set_am_attributes(new_node, &am);
		SET_IA32_ORIG_NODE(new_node, node);

		new_node = fix_mem_proj(new_node, &am);

		return new_node;
	}

	/* otherwise construct a lea */
	new_node = create_lea_from_address(dbgi, new_block, &addr);
	SET_IA32_ORIG_NODE(new_node, node);
	return new_node;
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
			return gen_binop(node, op1, op2, new_bd_ia32_xMul,
			                 match_commutative | match_am);
		else
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fmul);
	}
	return gen_binop(node, op1, op2, new_bd_ia32_IMul,
	                 match_commutative | match_am | match_mode_neutral |
	                 match_immediate | match_am_and_immediates);
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

	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node  *op1  = get_Mulh_left(node);
	ir_node  *op2  = get_Mulh_right(node);
	ir_node  *proj_res_high;
	if (mode_is_signed(mode)) {
		ir_node *new_node = gen_binop(node, op1, op2, new_bd_ia32_IMul1OP,
		                              match_commutative | match_am);
		proj_res_high = new_rd_Proj(dbgi, new_node, ia32_mode_gp,
		                            pn_ia32_IMul1OP_res_high);
	} else {
		ir_node *new_node = gen_binop(node, op1, op2, new_bd_ia32_Mul,
		                              match_commutative | match_am);
		proj_res_high = new_rd_Proj(dbgi, new_node, ia32_mode_gp,
		                            pn_ia32_Mul_res_high);
	}
	return proj_res_high;
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
		ir_tarval *tv = get_Const_tarval(op2);
		long       v  = get_tarval_long(tv);

		if (v == 0xFF || v == 0xFFFF) {
			ir_mode *src_mode;
			if (v == 0xFF) {
				src_mode = mode_Bu;
			} else {
				assert(v == 0xFFFF);
				src_mode = mode_Hu;
			}
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = get_nodes_block(node);
			ir_node  *res    = create_I2I_Conv(src_mode, ia32_mode_gp, dbgi,
			                                   block, op1, node);
			return res;
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

	assert(!mode_is_float(get_irn_mode(node)));
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
			return gen_binop(node, op1, op2, new_bd_ia32_xSub, match_am);
		else
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fsub);
	}

	if (is_Const(op2)) {
		ir_fprintf(stderr, "Optimization warning: found sub with const (%+F)\n",
		           node);
	}

	ir_node *ia32_sub = gen_binop(node, op1, op2, new_bd_ia32_Sub, match_mode_neutral
	                              | match_am | match_immediate);

	/* A Cmp node that has the same operands as this Sub will use
	 * this Sub's flags result. To be prepared for that, we change
	 * every Sub's mode to mode_T. If gen_binop has produced a Sub
	 * with source address mode, this has already happened and
	 * get_binop returns a Proj rather than a Sub. */
	if (is_ia32_Sub(ia32_sub)) {
		ir_mode *old_mode = get_irn_mode(ia32_sub);
		set_irn_mode(ia32_sub, mode_T);
		return new_r_Proj(ia32_sub, old_mode, pn_ia32_Sub_res);
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

		ir_node **ins = ALLOCAN(ir_node*, arity+1);

		/* NOTE: This sometimes produces dead-code because the old sync in
		 * src_mem might not be used anymore, we should detect this case
		 * and kill the sync... */
		int n = 0;
		for (int i = arity - 1; i >= 0; --i) {
			ir_node *const pred = get_Sync_pred(src_mem, i);

			/* avoid memory loop */
			ir_node *const ptr_pred = get_Proj_pred(src_val);
			if (is_Proj(pred) && get_Proj_pred(pred) == ptr_pred)
				continue;

			ins[n++] = be_transform_node(pred);
		}

		if (n==1 && ins[0] == am_mem) {
			return am_mem;
			/* creating a new Sync and relying on CSE may fail,
			 * if am_mem is a ProjM, which does not yet verify. */
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
static ir_node *create_sex_32_64(dbg_info *dbgi, ir_node *block,
                                 ir_node *val, const ir_node *orig)
{
	(void)orig;

	ir_node *res;
	if (ia32_cg_config.use_short_sex_eax) {
		ir_node *pval = new_bd_ia32_ProduceVal(dbgi, block);
		res = new_bd_ia32_Cltd(dbgi, block, val, pval);
	} else {
		ir_graph *const irg   = get_Block_irg(block);
		ir_node  *const imm31 = ia32_create_Immediate(irg, NULL, 31);
		res = new_bd_ia32_Sar(dbgi, block, val, imm31);
	}
	SET_IA32_ORIG_NODE(res, orig);
	return res;
}

/**
 * Generates an ia32 Div with additional infrastructure for the
 * register allocator if needed.
 */
static ir_node *create_Div(ir_node *node)
{
	/* the upper bits have random contents for smaller modes */
	ir_node *op1;
	ir_node *op2;
	ir_node *mem;
	ir_mode *mode;
	switch (get_irn_opcode(node)) {
	case iro_Div:
		op1     = get_Div_left(node);
		op2     = get_Div_right(node);
		mem     = get_Div_mem(node);
		mode    = get_Div_resmode(node);
		break;
	case iro_Mod:
		op1     = get_Mod_left(node);
		op2     = get_Mod_right(node);
		mem     = get_Mod_mem(node);
		mode    = get_Mod_resmode(node);
		break;
	default:
		panic("invalid divmod node %+F", node);
	}

	ia32_address_mode_t  am;
	ir_node             *mem_pin_skip = skip_Pin(mem);
	ir_node             *block        = get_nodes_block(node);
	match_arguments(&am, block, op1, op2, mem_pin_skip, match_am|match_upconv);

	/* Beware: We don't need a Sync, if the memory predecessor of the Div node
	   is the memory of the consumed address. We can have only the second op as
	   address in Div nodes, so check only op2. */
	x86_address_t *addr    = &am.addr;
	ir_node       *new_mem = transform_AM_mem(block, op2, mem_pin_skip,
	                                          addr->mem);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_node;
	if (mode_is_signed(mode)) {
		ir_node *sign_extension
			= create_sex_32_64(dbgi, new_block, am.new_op1, node);
		new_node = new_bd_ia32_IDiv(dbgi, new_block, addr->base,
		                            addr->index, new_mem, am.new_op2,
		                            am.new_op1, sign_extension);
	} else {
		ir_node *sign_extension
			= new_bd_ia32_Const(dbgi, new_block, NULL, 0, 0);
		new_node = new_bd_ia32_Div(dbgi, new_block, addr->base,
		                           addr->index, new_mem, am.new_op2,
		                           am.new_op1, sign_extension);
	}
	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(new_node, throws_exception);

	set_irn_pinned(new_node, get_irn_pinned(node));

	set_am_attributes(new_node, &am);
	SET_IA32_ORIG_NODE(new_node, node);

	new_node = fix_mem_proj(new_node, &am);

	return new_node;
}

/**
 * Generates an ia32 Mod.
 */
static ir_node *gen_Mod(ir_node *node)
{
	return create_Div(node);
}

/**
 * Generates an ia32 Div.
 */
static ir_node *gen_Div(ir_node *node)
{
	ir_mode *mode = get_Div_resmode(node);
	if (mode_is_float(mode)) {
		ir_node *op1 = get_Div_left(node);
		ir_node *op2 = get_Div_right(node);

		if (ia32_cg_config.use_sse2) {
			return gen_binop(node, op1, op2, new_bd_ia32_xDiv, match_am);
		} else {
			return gen_binop_x87_float(node, op1, op2, new_bd_ia32_fdiv);
		}
	}

	return create_Div(node);
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
	if (is_Const_1(right)) {
		dbg_info *dbgi      = get_irn_dbg_info(node);
		ir_node  *block     = get_nodes_block(node);
		ir_node  *new_block = be_transform_node(block);
		ir_node  *new_left  = be_transform_node(left);
		ir_node  *new_node
			= new_bd_ia32_Lea(dbgi, new_block, new_left, new_left);
		SET_IA32_ORIG_NODE(new_node, node);
		return new_node;
	}

	return gen_shift_binop(node, left, right, new_bd_ia32_Shl,
	                       match_mode_neutral | match_immediate);
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

	return gen_shift_binop(node, left, right, new_bd_ia32_Shr,
	                       match_immediate | match_zero_ext);
}

/**
 * Creates an ia32 Sar.
 *
 * @return The created ia32 Shrs node
 */
static ir_node *gen_Shrs(ir_node *node)
{
	ir_node *left  = get_Shrs_left(node);
	ir_node *right = get_Shrs_right(node);

	if (is_Const(right)) {
		ir_tarval *tv  = get_Const_tarval(right);
		long       val = get_tarval_long(tv);
		if (val == 31 && get_irn_n_edges(left) > 1) {
			/* this is a sign extension */
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = be_transform_node(get_nodes_block(node));
			ir_node  *new_op = be_transform_node(left);

			return create_sex_32_64(dbgi, block, new_op, node);
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
					ir_node *res = create_I2I_Conv(src_mode, ia32_mode_gp, dbgi,
					                               block, shl_left, node);
					return res;
				}
			}
		}
	}

	return gen_shift_binop(node, left, right, new_bd_ia32_Sar,
	                       match_immediate | match_sign_ext);
}

/**
 * Transforms a Minus node.
 *
 * @return The created ia32 Minus node
 */
static ir_node *gen_Minus(ir_node *node)
{
	ir_node  *op    = get_Minus_op(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);

	ir_node  *new_node;
	if (mode_is_float(mode)) {
		ir_node *new_op = be_transform_node(op);
		if (ia32_cg_config.use_sse2) {
			/* TODO: non-optimal... if we have many xXors, then we should
			 * rather create a load for the const and use that instead of
			 * several AM nodes... */
			ir_graph *const irg       = get_Block_irg(block);
			ir_node  *const noreg_xmm = ia32_new_NoReg_xmm(irg);
			ir_node  *const base      = get_global_base(irg);
			new_node = new_bd_ia32_xXor(dbgi, block, base, noreg_GP, nomem, new_op, noreg_xmm);

			int        size = get_mode_size_bits(mode);
			ir_entity *ent  = ia32_gen_fp_known_const(irg, size == 32 ? ia32_SSIGN : ia32_DSIGN);

			set_ia32_am_ent(new_node, ent);
			set_ia32_op_type(new_node, ia32_AddrModeS);
			set_ia32_ls_mode(new_node, mode);
		} else {
			new_node = new_bd_ia32_fchs(dbgi, block, new_op);
		}
	} else {
		new_node = gen_unop(node, op, new_bd_ia32_Neg, match_mode_neutral);
	}

	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
}

/**
 * Transforms a Not node.
 *
 * @return The created ia32 Not node
 */
static ir_node *gen_Not(ir_node *node)
{
	assert(get_irn_mode(node) != mode_b); /* should be lowered already */
	assert(!mode_is_float(get_irn_mode(node)));

	ir_node *op = get_Not_op(node);
	return gen_unop(node, op, new_bd_ia32_Not, match_mode_neutral);
}

static ir_node *create_float_abs(dbg_info *dbgi, ir_node *block, ir_node *op,
                                 bool negate, ir_node *node)
{
	ir_node *new_block = be_transform_node(block);
	ir_mode *mode      = get_irn_mode(op);
	ir_node *new_op    = be_transform_node(op);
	assert(mode_is_float(mode));

	ir_node *new_node;
	if (ia32_cg_config.use_sse2) {
		ir_graph *const irg      = get_Block_irg(new_block);
		ir_node  *const noreg_fp = ia32_new_NoReg_xmm(irg);
		ir_node  *const base     = get_global_base(irg);
		new_node = new_bd_ia32_xAnd(dbgi, new_block, base, noreg_GP, nomem, new_op, noreg_fp);

		int        size = get_mode_size_bits(mode);
		ir_entity *ent  = ia32_gen_fp_known_const(irg, size == 32 ? ia32_SABS : ia32_DABS);

		set_ia32_am_ent(new_node, ent);

		SET_IA32_ORIG_NODE(new_node, node);

		set_ia32_op_type(new_node, ia32_AddrModeS);
		set_ia32_ls_mode(new_node, mode);

		/* TODO, implement -Abs case */
		assert(!negate);
	} else {
		check_x87_floatmode(mode);
		new_node = new_bd_ia32_fabs(dbgi, new_block, new_op);
		SET_IA32_ORIG_NODE(new_node, node);
		if (negate) {
			new_node = new_bd_ia32_fchs(dbgi, new_block, new_node);
			SET_IA32_ORIG_NODE(new_node, node);
		}
	}

	return new_node;
}

/**
 * Create a bt instruction for x & (1 << n) and place it into the block of cmp.
 */
static ir_node *gen_bt(ir_node *cmp, ir_node *x, ir_node *n)
{
	dbg_info *dbgi      = get_irn_dbg_info(cmp);
	ir_node  *block     = get_nodes_block(cmp);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *op1       = be_transform_node(x);
	ir_node  *op2       = be_transform_node(n);

	return new_bd_ia32_Bt(dbgi, new_block, op1, op2);
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
		panic("Unexpected float pnc");
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
		panic("Unexpected pnc");
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
		panic("Unexpected pnc");
	}
}

static ir_node *get_flags_node(ir_node *cmp, x86_condition_code_t *cc_out)
{
	/* must have a Cmp as input */
	ir_relation relation = get_Cmp_relation(cmp);
	ir_node    *l        = get_Cmp_left(cmp);
	ir_node    *r        = get_Cmp_right(cmp);
	ir_mode    *mode     = get_irn_mode(l);

	/* check for bit-test */
	if (ia32_cg_config.use_bt
	    && (relation == ir_relation_equal
	        || (mode_is_signed(mode) && relation == ir_relation_less_greater)
	        || (!mode_is_signed(mode) && ((relation & ir_relation_greater_equal) == ir_relation_greater)))
	    && is_And(l)) {
		ir_node *la = get_And_left(l);
		ir_node *ra = get_And_right(l);
		if (is_Shl(ra)) {
			ir_node *tmp = la;
			la = ra;
			ra = tmp;
		}
		if (is_Shl(la)) {
			ir_node *c = get_Shl_left(la);
			if (is_Const_1(c) && is_Const_0(r)) {
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
	relation = get_Cmp_relation(cmp);

	/* the middle-end tries to eliminate impossible relations, so a ptr <> 0
	 * test becomes ptr > 0. But for x86 an equal comparison is preferable to
	 * a >0 (we can sometimes eliminate the cmp in favor of flags produced by
	 * a predecessor node). So add the < bit.
	 * (Note that we do not want to produce <=> (which can happen for
	 * unoptimized code), because no x86 flag can represent that */
	if (!(relation & ir_relation_equal) && relation & ir_relation_less_greater)
		relation |= get_negated_relation(ir_get_possible_cmp_relations(l, r)) & ir_relation_less_greater;

	bool overflow_possible = true;
	if (is_Const(r) && is_Const_null(r))
		overflow_possible = false;

	/* just do a normal transformation of the Cmp */
	*cc_out = ir_relation_to_x86_condition_code(relation, mode,
	                                            overflow_possible);

	return flags;
}

static void create_transformed_address_mode(x86_address_t *addr,
                                            ir_node *ptr,
                                            x86_create_am_flags_t flags)
{
	memset(addr, 0, sizeof(*addr));
	x86_create_address_mode(addr, ptr, flags);
	ir_node *base = addr->base;
	base = base == NULL ? noreg_GP : be_transform_node(base);
	addr->base = base;

	ir_node *idx = addr->index;
	idx = idx == NULL ? noreg_GP : be_transform_node(idx);
	addr->index = idx;
}

/**
 * Transforms a Load.
 *
 * @return the created ia32 Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	ir_node  *old_block = get_nodes_block(node);
	ir_node  *block     = be_transform_node(old_block);
	ir_node  *ptr       = get_Load_ptr(node);
	ir_node  *mem       = get_Load_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_mode  *mode      = get_Load_mode(node);

	/* construct load address */
	x86_address_t addr;
	create_transformed_address_mode(&addr, ptr, x86_create_am_normal);
	ir_node *base = addr.base;
	ir_node *idx  = addr.index;

	ir_node *new_node;
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			new_node = new_bd_ia32_xLoad(dbgi, block, base, idx, new_mem,
			                             mode);
		} else {
			new_node = new_bd_ia32_fld(dbgi, block, base, idx, new_mem,
			                            mode);
		}
	} else {
		assert(mode != mode_b);

		/* create a conv node with address mode for smaller modes */
		if (get_mode_size_bits(mode) < 32) {
			new_node = new_bd_ia32_Conv_I2I(dbgi, block, base, idx,
			                                new_mem, noreg_GP, mode);
		} else {
			new_node = new_bd_ia32_Load(dbgi, block, base, idx, new_mem);
			mode     = ia32_mode_gp;
		}
	}
	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(new_node, throws_exception);

	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeS);
	set_ia32_ls_mode(new_node, mode);
	set_address(new_node, &addr);

	if (get_irn_pinned(node) == op_pin_state_floats) {
		assert((int)pn_ia32_xLoad_res == (int)pn_ia32_fld_res
				&& (int)pn_ia32_fld_res == (int)pn_ia32_Load_res
				&& (int)pn_ia32_Load_res == (int)pn_ia32_res);
		arch_add_irn_flags(new_node, arch_irn_flag_rematerializable);
	}

	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
}

static bool use_dest_am(ir_node *block, ir_node *node, ir_node *mem,
                        ir_node *ptr, ir_node *other, match_flags_t flags)
{
	if (!is_Proj(node))
		return false;

	/* we only use address mode if we're the only user of the load
	 * or the users will be merged later anyway */
	if (get_irn_n_edges(node) != (flags & match_two_users ? 2 : 1) &&
	    !users_will_merge(node))
		return false;

	ir_node *load = get_Proj_pred(node);
	if (!is_Load(load))
		return false;
	if (get_nodes_block(load) != block)
		return false;

	/* store should have the same pointer as the load */
	if (get_Load_ptr(load) != ptr)
		return false;

	/* don't do AM if other node inputs depend on the load (via mem-proj) */
	if (other != NULL                   &&
	    get_nodes_block(other) == block &&
	    heights_reachable_in_block(ia32_heights, other, load)) {
		return false;
	}

	if (ia32_prevents_AM(block, load, mem))
		return false;
	/* Store should be attached to the load via mem */
	assert(heights_reachable_in_block(ia32_heights, mem, load));
	return true;
}

static ir_node *dest_am_binop(ir_node *node, ir_node *op1, ir_node *op2,
                              ir_node *mem, ir_node *ptr, ir_mode *mode,
                              construct_binop_dest_func *func,
                              construct_binop_dest_func *func8bit,
							  match_flags_t flags)
{
	assert(flags & match_immediate); /* there is no destam node without... */
	bool commutative = (flags & match_commutative) != 0;

	ia32_address_mode_t am;
	memset(&am, 0, sizeof(am));
	ir_node *src_block = get_nodes_block(node);
	ir_node *new_op;
	if (use_dest_am(src_block, op1, mem, ptr, op2, flags)) {
		build_address(&am, op1, x86_create_am_double_use);
		new_op = create_immediate_or_transform(op2);
	} else if (commutative && use_dest_am(src_block, op2, mem, ptr, op1, flags)) {
		build_address(&am, op2, x86_create_am_double_use);
		new_op = create_immediate_or_transform(op1);
	} else {
		return NULL;
	}

	x86_address_t *addr = &am.addr;
	if (addr->base == NULL)
		addr->base = noreg_GP;
	if (addr->index == NULL)
		addr->index = noreg_GP;
	if (addr->mem == NULL)
		addr->mem = nomem;

	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_node(src_block);
	ir_node  *new_mem = transform_AM_mem(block, am.am_node, mem, addr->mem);

	ir_node *new_node;
	if (get_mode_size_bits(mode) == 8) {
		new_node = func8bit(dbgi, block, addr->base, addr->index, new_mem,
		                    new_op);
	} else {
		new_node = func(dbgi, block, addr->base, addr->index, new_mem, new_op);
	}
	set_address(new_node, addr);
	set_ia32_op_type(new_node, ia32_AddrModeD);
	set_ia32_ls_mode(new_node, mode);
	SET_IA32_ORIG_NODE(new_node, node);

	ir_node *new_proj = new_r_Proj(new_node, mode_M, pn_ia32_destAM_M);

	be_set_transformed_node(get_Proj_pred(am.mem_proj), new_proj);
	ir_node *mem_proj = be_transform_node(am.mem_proj);
	be_set_transformed_node(am.mem_proj, new_proj);
	be_set_transformed_node(mem_proj, new_proj);

	return new_node;
}

static ir_node *dest_am_unop(ir_node *node, ir_node *op, ir_node *mem,
                             ir_node *ptr, ir_mode *mode,
                             construct_unop_dest_func *func)
{
	ir_node  *src_block = get_nodes_block(node);

	if (!use_dest_am(src_block, op, mem, ptr, NULL, (match_flags_t)0))
		return NULL;

	ia32_address_mode_t am;
	memset(&am, 0, sizeof(am));
	build_address(&am, op, x86_create_am_double_use);

	x86_address_t *addr     = &am.addr;
	dbg_info      *dbgi     = get_irn_dbg_info(node);
	ir_node       *block    = be_transform_node(src_block);
	ir_node       *new_mem  = transform_AM_mem(block, am.am_node, mem,
	                                           addr->mem);
	ir_node       *new_node = func(dbgi, block, addr->base, addr->index,
	                               new_mem);
	set_address(new_node, addr);
	set_ia32_op_type(new_node, ia32_AddrModeD);
	set_ia32_ls_mode(new_node, mode);
	SET_IA32_ORIG_NODE(new_node, node);

	ir_node *new_proj = new_r_Proj(new_node, mode_M, pn_ia32_destAM_M);

	be_set_transformed_node(get_Proj_pred(am.mem_proj), new_proj);
	ir_node *mem_proj = be_transform_node(am.mem_proj);
	be_set_transformed_node(am.mem_proj, new_proj);
	be_set_transformed_node(mem_proj, new_proj);

	return new_node;
}

static ir_node *try_create_SetMem(ir_node *node, ir_node *ptr, ir_node *mem)
{
	ir_mode              *mode      = get_irn_mode(node);
	ir_node              *mux_true  = get_Mux_true(node);
	ir_node              *mux_false = get_Mux_false(node);

	if (get_mode_size_bits(mode) != 8)
		return NULL;

	bool negated;
	if (is_Const_1(mux_true) && is_Const_0(mux_false)) {
		negated = false;
	} else if (is_Const_0(mux_true) && is_Const_1(mux_false)) {
		negated = true;
	} else {
		return NULL;
	}

	x86_condition_code_t cc;
	ir_node *cond  = get_Mux_sel(node);
	ir_node *flags = get_flags_node(cond, &cc);
	/* we can't handle the float special cases with SetM */
	if (cc & x86_cc_additional_float_cases)
		return NULL;
	if (negated)
		cc = x86_negate_condition_code(cc);

	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_node  = new_bd_ia32_SetccMem(dbgi, new_block, addr.base,
	                                           addr.index, addr.mem, flags, cc);
	set_address(new_node, &addr);
	set_ia32_op_type(new_node, ia32_AddrModeD);
	set_ia32_ls_mode(new_node, mode);
	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
}

static ir_node *try_create_dest_am(ir_node *node)
{
	ir_node  *val  = get_Store_value(node);
	ir_mode  *mode = get_irn_mode(val);
	unsigned  bits = get_mode_size_bits(mode);

	/* handle only GP modes for now... */
	if (!ia32_mode_needs_gp_reg(mode))
		return NULL;

	for (;;) {
		/* store must be the only user of the val node */
		if (get_irn_n_edges(val) > 1)
			return NULL;
		/* skip pointless convs */
		if (is_Conv(val)) {
			ir_node *conv_op   = get_Conv_op(val);
			ir_mode *pred_mode = get_irn_mode(conv_op);
			if (!ia32_mode_needs_gp_reg(pred_mode))
				break;
			if (pred_mode == mode_b || bits <= get_mode_size_bits(pred_mode)) {
				val = conv_op;
				continue;
			}
		}
		break;
	}

	/* value must be in the same block */
	if (get_nodes_block(node) != get_nodes_block(val))
		return NULL;

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
				                         mode, new_bd_ia32_RorMem,
				                         new_bd_ia32_RorMem,
				                         match_immediate | match_two_users);
			} else {
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         mode, new_bd_ia32_RolMem,
				                         new_bd_ia32_RolMem,
				                         match_immediate | match_two_users);
			}
			break;
		}

		ir_node *op1 = get_Add_left(val);
		ir_node *op2 = get_Add_right(val);
		if (ia32_cg_config.use_incdec) {
			if (is_Const_1(op2)) {
				new_node = dest_am_unop(val, op1, mem, ptr, mode,
				                        new_bd_ia32_IncMem);
				break;
			} else if (is_Const_Minus_1(op2)) {
				new_node = dest_am_unop(val, op1, mem, ptr, mode,
				                        new_bd_ia32_DecMem);
				break;
			}
		}
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_AddMem, new_bd_ia32_AddMem_8bit,
		                         match_commutative | match_immediate);
		break;
	}
	case iro_Sub: {
		ir_node *op1 = get_Sub_left(val);
		ir_node *op2 = get_Sub_right(val);
		if (is_Const(op2)) {
			ir_fprintf(stderr, "Optimization warning: not-normalized sub ,C found\n");
		}
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_SubMem, new_bd_ia32_SubMem_8bit,
		                         match_immediate);
		break;
	}
	case iro_And: {
		ir_node *op1 = get_And_left(val);
		ir_node *op2 = get_And_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_AndMem, new_bd_ia32_AndMem_8bit,
		                         match_commutative | match_immediate);
		break;
	}
	case iro_Or: {
		ir_node *rot_left;
		ir_node *rot_right;
		if (be_pattern_is_rotl(val, &rot_left, &rot_right)) {
			if (is_Minus(rot_right)) {
				rot_right = get_Minus_op(rot_right);
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         mode, new_bd_ia32_RorMem,
				                         new_bd_ia32_RorMem,
				                         match_immediate | match_two_users);
			} else {
				new_node = dest_am_binop(val, rot_left, rot_right, mem, ptr,
				                         mode, new_bd_ia32_RolMem,
				                         new_bd_ia32_RolMem,
				                         match_immediate | match_two_users);
			}
			break;
		}

		ir_node *op1 = get_Or_left(val);
		ir_node *op2 = get_Or_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_OrMem, new_bd_ia32_OrMem_8bit,
		                         match_commutative | match_immediate);
		break;
	}
	case iro_Eor: {
		ir_node *op1 = get_Eor_left(val);
		ir_node *op2 = get_Eor_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_XorMem, new_bd_ia32_XorMem_8bit,
		                         match_commutative | match_immediate);
		break;
	}
	case iro_Shl: {
		ir_node *op1 = get_Shl_left(val);
		ir_node *op2 = get_Shl_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_ShlMem, new_bd_ia32_ShlMem,
		                         match_immediate);
		break;
	}
	case iro_Shr: {
		ir_node *op1 = get_Shr_left(val);
		ir_node *op2 = get_Shr_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_ShrMem, new_bd_ia32_ShrMem,
		                         match_immediate);
		break;
	}
	case iro_Shrs: {
		ir_node *op1 = get_Shrs_left(val);
		ir_node *op2 = get_Shrs_right(val);
		new_node = dest_am_binop(val, op1, op2, mem, ptr, mode,
		                         new_bd_ia32_SarMem, new_bd_ia32_SarMem,
		                         match_immediate);
		break;
	}
	case iro_Mux:
		new_node = try_create_SetMem(val, ptr, mem);
		break;
	case iro_Minus: {
		ir_node *op1 = get_Minus_op(val);
		new_node = dest_am_unop(val, op1, mem, ptr, mode, new_bd_ia32_NegMem);
		break;
	}
	case iro_Not: {
		/* should be lowered already */
		assert(mode != mode_b);
		ir_node *op1 = get_Not_op(val);
		new_node = dest_am_unop(val, op1, mem, ptr, mode, new_bd_ia32_NotMem);
		break;
	}
	default:
		return NULL;
	}

	if (new_node != NULL && get_irn_pinned(new_node) != op_pin_state_pinned
	    && get_irn_pinned(node) == op_pin_state_pinned) {
		set_irn_pinned(new_node, op_pin_state_pinned);
	}

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
	ir_mode   *cns_mode  = get_irn_mode(cns);
	unsigned   size      = get_mode_size_bytes(cns_mode);
	ir_tarval *tv        = get_Const_tarval(cns);
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
	ir_node   *ptr       = get_Store_ptr(node);
	ir_node   *mem       = get_Store_mem(node);
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	int        ofs       = 0;
	int        i         = 0;
	ir_node   *ins[4];

	x86_address_t addr;
	build_address_ptr(&addr, ptr, mem);

	do {
		unsigned val;
		unsigned delta;
		ir_mode *mode;
		if (size >= 4) {
			val= (unsigned)get_tarval_sub_bits(tv, ofs)            |
			    ((unsigned)get_tarval_sub_bits(tv, ofs + 1) <<  8) |
			    ((unsigned)get_tarval_sub_bits(tv, ofs + 2) << 16) |
			    ((unsigned)get_tarval_sub_bits(tv, ofs + 3) << 24);
			delta = 4;
			mode  = ia32_mode_gp;
		} else if (size >= 2) {
			val= get_tarval_sub_bits(tv, ofs)            |
			    (get_tarval_sub_bits(tv, ofs + 1) <<  8);
			delta = 2;
			mode  = mode_Hu;
		} else {
			panic("invalid size of Store float to mem (%+F)", node);
		}
		ir_graph *const irg = get_Block_irg(new_block);
		ir_node  *const imm = ia32_create_Immediate(irg, NULL, val);

		ir_node *new_node = new_bd_ia32_Store(dbgi, new_block, addr.base,
			addr.index, addr.mem, imm);
		ir_node *new_mem  = new_r_Proj(new_node, mode_M, pn_ia32_Store_M);

		int throws_exception = ir_throws_exception(node);
		ir_set_throws_exception(new_node, throws_exception);
		set_irn_pinned(new_node, get_irn_pinned(node));
		set_ia32_op_type(new_node, ia32_AddrModeD);
		set_ia32_ls_mode(new_node, mode);
		set_address(new_node, &addr);
		SET_IA32_ORIG_NODE(new_node, node);

		assert(i < 4);
		ins[i++] = new_mem;

		size -= delta;
		ofs  += delta;
		addr.offset += delta;
	} while (size != 0);

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
                          ir_node *index, ir_node *mem,  ir_node *val)
{
	if (ia32_cg_config.use_fisttp) {
		/* Note: fisttp ALWAYS pop the tos. We have to ensure here that the
		 * value is copied if other users exists */
		return new_bd_ia32_fisttp(dbgi, block, base, index, mem, val);
	} else {
		ir_graph *const irg        = get_Block_irg(block);
		ir_node  *const trunc_mode = ia32_new_Fpu_truncate(irg);

		/* do a fist */
		return new_bd_ia32_fist(dbgi, block, base, index, mem, val, trunc_mode);
	}
}

/**
 * Transforms a general (no special case) Store.
 *
 * @return the created ia32 Store node
 */
static ir_node *gen_general_Store(ir_node *node)
{
	/* check for destination address mode */
	ir_node *new_node = try_create_dest_am(node);
	if (new_node != NULL)
		return new_node;

	/* construct store address */
	ir_node *ptr = get_Store_ptr(node);
	x86_address_t addr;
	create_transformed_address_mode(&addr, ptr, x86_create_am_normal);
	ir_node *mem = get_Store_mem(node);
	addr.mem = be_transform_node(mem);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *val       = get_Store_value(node);
	ir_mode  *mode      = get_irn_mode(val);
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			ir_node *new_val = be_transform_node(val);
			new_node = new_bd_ia32_xStore(dbgi, new_block, addr.base,
			                              addr.index, addr.mem, new_val);
		} else {
			val = ia32_skip_float_downconv(val);
			ir_node *new_val = be_transform_node(val);
			new_node = new_bd_ia32_fst(dbgi, new_block, addr.base,
			                            addr.index, addr.mem, new_val, mode);
		}
	} else if (!ia32_cg_config.use_sse2 && is_float_to_int_conv(val)) {
		val = get_Conv_op(val);
		ir_node *new_val = be_transform_node(val);
		new_node = gen_fist(dbgi, new_block, addr.base, addr.index, addr.mem,
		                    new_val);
	} else if (!ia32_cg_config.use_sse2 && is_Bitcast(val)) {
		ir_node *op      = get_Bitcast_op(val);
		ir_mode *op_mode = get_irn_mode(op);
		assert(mode_is_float(op_mode));
		ir_node *new_op  = be_transform_node(op);
		new_node = new_bd_ia32_fst(dbgi, new_block, addr.base, addr.index,
		                           addr.mem, new_op, op_mode);
		mode = op_mode;
	} else {
		unsigned dest_bits = get_mode_size_bits(mode);
		while (is_downconv(val)
		       && get_mode_size_bits(get_irn_mode(val)) >= dest_bits) {
		    val = get_Conv_op(val);
		}
		ir_node *new_val = create_immediate_or_transform(val);
		assert(mode != mode_b);

		new_node = dest_bits == 8
			? new_bd_ia32_Store_8bit(dbgi, new_block, addr.base, addr.index, addr.mem, new_val)
			: new_bd_ia32_Store     (dbgi, new_block, addr.base, addr.index, addr.mem, new_val);
	}
	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(new_node, throws_exception);

	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeD);
	set_ia32_ls_mode(new_node, mode);

	set_address(new_node, &addr);
	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
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
	return gen_general_Store(node);
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
	assert(!mode_is_float(sel_mode));
	sel = ia32_skip_sameconv(sel);
	if (get_mode_size_bits(sel_mode) < 32)
		new_sel = transform_upconv(sel, node);

	ir_type   *const utype = get_unknown_type();
	ir_entity *const entity
		= new_entity(irp->dummy_owner, id_unique("TBL%u"), utype);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	ir_graph              *irg   = get_irn_irg(node);
	const ir_switch_table *table = get_Switch_table(node);
	table = ir_switch_table_duplicate(irg, table);

	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	unsigned  n_outs   = get_Switch_n_outs(node);
	ir_node  *new_node = new_bd_ia32_SwitchJmp(dbgi, block, noreg_GP, new_sel,
	                                           n_outs, table);
	set_ia32_am_scale(new_node, 2);
	set_ia32_am_ent(new_node, entity);
	set_ia32_op_type(new_node, ia32_AddrModeS);
	set_ia32_ls_mode(new_node, ia32_mode_gp);
	SET_IA32_ORIG_NODE(new_node, node);
	// FIXME This seems wrong. GCC uses PIC for switch on OS X.
	get_ia32_attr(new_node)->data.am_sc_no_pic_adjust = true;

	return new_node;
}

/**
 * Transform a Cond node.
 */
static ir_node *gen_Cond(ir_node *node)
{
	/* we get flags from a Cmp */
	ir_node              *sel = get_Cond_selector(node);
	x86_condition_code_t  cc;
	ir_node *flags = get_flags_node(sel, &cc);

	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_node  = new_bd_ia32_Jcc(dbgi, new_block, flags, cc);
	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
}

static ir_node *create_Fucom(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
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
		SET_IA32_ORIG_NODE(new_node, node);
	} else {
		if (is_Const_0(right)) {
			new_node = new_bd_ia32_FtstFnstsw(dbgi, new_block, new_left, 0);
		} else {
			ir_node *new_right = be_transform_node(right);
			new_node = new_bd_ia32_FucomFnstsw(dbgi, new_block, new_left,
			                                   new_right, 0);
			set_ia32_commutative(new_node);
		}

		SET_IA32_ORIG_NODE(new_node, node);

		new_node = new_bd_ia32_Sahf(dbgi, new_block, new_node);
		SET_IA32_ORIG_NODE(new_node, node);
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
	ir_node  *new_node  = new_bd_ia32_Ucomi(dbgi, new_block, addr->base,
	                                        addr->index, addr->mem, am.new_op1,
	                                        am.new_op2, am.ins_permuted);
	set_am_attributes(new_node, &am);

	SET_IA32_ORIG_NODE(new_node, node);

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
		if (is_Store(succ)) {
			ir_node *new_store = be_transform_node(succ);
			if (is_ia32_SubMem(new_store)) {
				return new_store;
			} else {
				ir_node *result = get_irn_n(new_store, n_ia32_Store_val);
				if (is_Proj(result)) result = get_Proj_pred(result);
				return result;
			}
		}
	}
	return be_transform_node(sub);
}

static ir_node *try_get_sub_flags(ir_node *cmp, ir_node *sub, bool *swap)
{
	ir_node *cmp_block = get_nodes_block(cmp);
	ir_node *sub_block = get_nodes_block(sub);

	if (!(block_dominates(cmp_block, sub_block) ||
	      block_dominates(sub_block, cmp_block))) {
		    return NULL;
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
		return NULL;
	}

	ir_node *ia32_sub = skip_Proj(transform_sub_or_store(sub));
	if (is_ia32_Sub(ia32_sub)) {
		return new_r_Proj(ia32_sub, ia32_mode_flags, pn_ia32_Sub_flags);
	} else if (is_ia32_SubMem(ia32_sub)) {
		return new_r_Proj(ia32_sub, ia32_mode_flags, pn_ia32_SubMem_flags);
	} else {
		panic("Unknown variant of Sub at %+F\n", ia32_sub);
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
	assert(ia32_mode_needs_gp_reg(cmp_mode));

	/* Prefer the Test instruction, when encountering (x & y) ==/!= 0 */
	ia32_address_mode_t am;
	x86_address_t     *addr      = &am.addr;
	dbg_info          *dbgi      = get_irn_dbg_info(node);
	ir_node           *block     = get_nodes_block(node);
	ir_node           *new_block = be_transform_node(block);
	ir_node           *right     = get_Cmp_right(node);
	ir_node           *new_node;
	if (is_Const_0(right)          &&
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
			cmp_mode = ia32_mode_gp;
		}

		new_node = get_mode_size_bits(cmp_mode) == 8
			? new_bd_ia32_Test_8bit(dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.ins_permuted)
			: new_bd_ia32_Test     (dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.ins_permuted);
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
				ir_node *left = get_Cmp_left(node);
				ir_node *right = get_Cmp_right(node);
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
			cmp_mode = ia32_mode_gp;
		}

		new_node = get_mode_size_bits(cmp_mode) == 8
			? new_bd_ia32_Cmp_8bit(dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.ins_permuted)
			: new_bd_ia32_Cmp     (dbgi, new_block, addr->base, addr->index, addr->mem, am.new_op1, am.new_op2, am.ins_permuted);
	}
	set_am_attributes(new_node, &am);
	set_ia32_ls_mode(new_node, cmp_mode);

	SET_IA32_ORIG_NODE(new_node, node);

	new_node = fix_mem_proj(new_node, &am);

	return new_node;
}

static ir_node *create_CMov(ir_node *node, ir_node *flags, ir_node *new_flags,
                            x86_condition_code_t cc)
{
	ir_node  *val_true  = get_Mux_true(node);
	ir_node  *val_false = get_Mux_false(node);
	assert(ia32_cg_config.use_cmov);
	assert(ia32_mode_needs_gp_reg(get_irn_mode(val_true)));

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
	                                       am.new_op2, new_flags, cc);
	set_am_attributes(new_node, &am);

	SET_IA32_ORIG_NODE(new_node, node);

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
	SET_IA32_ORIG_NODE(new_node, orig_node);

	/* we might need to conv the result up */
	if (get_mode_size_bits(mode) > 8) {
		new_node = new_bd_ia32_Conv_I2I_8bit(dbgi, new_block, noreg_GP,
		                                     noreg_GP, nomem, new_node,
		                                     mode_Bu);
		SET_IA32_ORIG_NODE(new_node, orig_node);
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

	ir_mode *mode = get_irn_mode(psi);
	ir_node *sub;
	if (is_Proj(new_node)) {
		sub = get_Proj_pred(new_node);
	} else {
		sub = new_node;
		set_irn_mode(sub, mode_T);
		new_node = new_r_Proj(sub, mode, pn_ia32_res);
	}
	assert(is_ia32_Sub(sub));
	ir_node  *eflags = new_r_Proj(sub, ia32_mode_gp, pn_ia32_Sub_flags);

	dbg_info *dbgi   = get_irn_dbg_info(psi);
	ir_node  *block  = get_nodes_block(new_node);
	ir_node  *sbb    = new_bd_ia32_Sbb0(dbgi, block, eflags);
	set_ia32_ls_mode(sbb, ia32_mode_gp);
	ir_node  *notn  = new_bd_ia32_Not(dbgi, block, sbb);

	new_node = new_bd_ia32_And(dbgi, block, noreg_GP, noreg_GP, nomem,
	                           new_node, notn);
	set_ia32_ls_mode(new_node, ia32_mode_gp);
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

	ir_type *tp = ia32_get_prim_type(mode);
	tp = ia32_create_float_array(tp);

	ir_entity *ent = new_entity(get_glob_type(), id_unique("C%u"), tp);

	set_entity_ld_ident(ent, get_entity_ident(ent));
	set_entity_visibility(ent, ir_visibility_private);
	add_entity_linkage(ent, IR_LINKAGE_CONSTANT);

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
	SETCC_TR_ADDxx,
	SETCC_TR_LEA,
	SETCC_TR_LEAxx,
	SETCC_TR_SHL,
	SETCC_TR_NEG,
	SETCC_TR_NOT,
	SETCC_TR_AND,
	SETCC_TR_SET,
};

typedef struct setcc_transform {
	unsigned             num_steps;
	x86_condition_code_t cc;
	struct {
		enum setcc_transform_insn  transform;
		long val;
		int  scale;
	} steps[4];
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

	/* Normalize f to zero. */
	if (!tarval_is_null(f)) {
		ir_tarval *t_sub = tarval_sub(t, f, NULL);

		t = t_sub;
		res->steps[step].transform = SETCC_TR_ADD;

		if (t == tarval_bad)
			panic("constant subtract failed");
		if (!tarval_is_long(f))
			panic("tarval is not long");

		res->steps[step].val = get_tarval_long(f);
		++step;
		f = tarval_sub(f, f, NULL);
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
			res->steps[step].scale     = 3; /* (a << 3) + a */
			break;
		case 8:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = res->steps[step].val == 0 ? SETCC_TR_SHL : SETCC_TR_LEA;
			res->steps[step].scale     = 3; /* (a << 3) */
			break;
		case 5:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = SETCC_TR_LEAxx;
			res->steps[step].scale     = 2; /* (a << 2) + a */
			break;
		case 4:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = res->steps[step].val == 0 ? SETCC_TR_SHL : SETCC_TR_LEA;
			res->steps[step].scale     = 2; /* (a << 2) */
			break;
		case 3:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = SETCC_TR_LEAxx;
			res->steps[step].scale     = 1; /* (a << 1) + a */
			break;
		case 2:
			if (step > 0 && res->steps[step - 1].transform == SETCC_TR_ADD)
				--step;
			res->steps[step].transform = res->steps[step].val == 0 ? SETCC_TR_SHL : SETCC_TR_LEA;
			res->steps[step].scale     = 1; /* (a << 1) */
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
				res->steps[step].scale     = val;
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
                                ir_mode *mode)
{
	ir_node *(*func)(dbg_info*, ir_node*, ir_node*, ir_node*, ir_node*,
	                 ir_node*, ir_mode*);

	func = get_mode_size_bits(mode) == 8 ?
		new_bd_ia32_Conv_I2I_8bit : new_bd_ia32_Conv_I2I;
	return func(dbgi, block, base, index, mem, val, mode);
}

/**
 * Transforms a Mux node into some code sequence.
 *
 * @return The transformed node.
 */
static ir_node *gen_Mux(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *mux_true  = get_Mux_true(node);
	ir_node  *mux_false = get_Mux_false(node);
	ir_node  *sel       = get_Mux_sel(node);
	ir_mode  *mode      = get_irn_mode(node);

	assert(get_irn_mode(sel) == mode_b);

	int is_abs = ir_mux_is_abs(sel, mux_false, mux_true);
	if (is_abs != 0) {
		if (ia32_mode_needs_gp_reg(mode)) {
			ir_fprintf(stderr, "Optimization warning: Integer abs %+F not transformed\n",
			           node);
		} else {
			ir_node *op = ir_get_abs_op(sel, mux_false, mux_true);
			return create_float_abs(dbgi, block, op, is_abs < 0, node);
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
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_xMin,
			                 match_commutative | match_am | match_two_users);
				} else if (cmp_left == mux_false && cmp_right == mux_true) {
					/* Mux(a <= b, b, a) => MAX */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_xMax,
			                 match_commutative | match_am | match_two_users);
				}
			} else if (relation == ir_relation_greater || relation == ir_relation_greater_equal) {
				if (cmp_left == mux_true && cmp_right == mux_false) {
					/* Mux(a >= b, a, b) => MAX */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_xMax,
			                 match_commutative | match_am | match_two_users);
				} else if (cmp_left == mux_false && cmp_right == mux_true) {
					/* Mux(a >= b, b, a) => MIN */
					return gen_binop(node, cmp_left, cmp_right, new_bd_ia32_xMin,
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

			ia32_address_mode_t am;
			am.addr.entity = ia32_create_const_array(mux_false, mux_true, &new_mode);

			unsigned scale;
			if (new_mode == ia32_mode_float32) {
				scale = 2;
			} else if (new_mode == ia32_mode_float64) {
				scale = 3;
			} else if (new_mode == ia32_mode_E) {
				/* arg, shift 16 NOT supported */
				scale = 3;
				new_node = new_bd_ia32_Lea(dbgi, new_block, new_node, new_node);
			} else {
				panic("Unsupported constant size");
			}

			ir_graph *const irg = get_Block_irg(new_block);
			am.ls_mode            = new_mode;
			am.addr.base          = get_global_base(irg);
			am.addr.index         = new_node;
			am.addr.mem           = nomem;
			am.addr.offset        = 0;
			am.addr.scale         = scale;
			am.addr.use_frame     = 0;
			am.addr.tls_segment   = false;
			am.addr.frame_entity  = NULL;
			am.mem_proj           = am.addr.mem;
			am.op_type            = ia32_AddrModeS;
			am.new_op1            = NULL;
			am.new_op2            = NULL;
			am.pinned             = op_pin_state_floats;
			am.commutative        = 1;
			am.ins_permuted       = false;

			ir_node *load;
			if (ia32_cg_config.use_sse2) {
				load = new_bd_ia32_xLoad(dbgi, block, am.addr.base,
				                         am.addr.index, am.addr.mem, new_mode);
			} else {
				load = new_bd_ia32_fld(dbgi, block, am.addr.base,
				                       am.addr.index, am.addr.mem, new_mode);
			}
			set_irn_pinned(load, op_pin_state_floats);
			set_am_attributes(load, &am);

			return new_r_Proj(load, mode_fp, pn_ia32_res);
		}
		panic("cannot transform floating point Mux");

	} else {
		assert(ia32_mode_needs_gp_reg(mode));

		if (is_Cmp(sel)) {
			ir_node    *cmp_left  = get_Cmp_left(sel);
			ir_node    *cmp_right = get_Cmp_right(sel);
			ir_relation relation  = get_Cmp_relation(sel);
			ir_node    *val_true  = mux_true;
			ir_node    *val_false = mux_false;

			if (is_Const(val_true) && is_Const_null(val_true)) {
				ir_node *tmp = val_false;
				val_false = val_true;
				val_true  = tmp;
				relation  = get_negated_relation(relation);
			}
			if (is_Const_0(val_false) && is_Sub(val_true)) {
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
				ir_node *imm;

				switch (res.steps[step].transform) {
				case SETCC_TR_ADD:
					new_node = new_bd_ia32_Lea(dbgi, new_block, new_node, noreg_GP);
					add_ia32_am_offs_int(new_node, res.steps[step].val);
					SET_IA32_ORIG_NODE(new_node, node);
					break;
				case SETCC_TR_ADDxx:
					new_node = new_bd_ia32_Lea(dbgi, new_block, new_node, new_node);
					SET_IA32_ORIG_NODE(new_node, node);
					break;
				case SETCC_TR_LEA:
					new_node = new_bd_ia32_Lea(dbgi, new_block, noreg_GP, new_node);
					set_ia32_am_scale(new_node, res.steps[step].scale);
					set_ia32_am_offs_int(new_node, res.steps[step].val);
					SET_IA32_ORIG_NODE(new_node, node);
					break;
				case SETCC_TR_LEAxx:
					new_node = new_bd_ia32_Lea(dbgi, new_block, new_node, new_node);
					set_ia32_am_scale(new_node, res.steps[step].scale);
					set_ia32_am_offs_int(new_node, res.steps[step].val);
					SET_IA32_ORIG_NODE(new_node, node);
					break;

				case SETCC_TR_SHL: {
					ir_graph *const irg = get_Block_irg(new_block);
					imm = ia32_immediate_from_long(irg, res.steps[step].scale);
					SET_IA32_ORIG_NODE(imm, node);
					new_node = new_bd_ia32_Shl(dbgi, new_block, new_node, imm);
					SET_IA32_ORIG_NODE(new_node, node);
					break;
				}

				case SETCC_TR_NEG:
					new_node = new_bd_ia32_Neg(dbgi, new_block, new_node);
					SET_IA32_ORIG_NODE(new_node, node);
					break;
				case SETCC_TR_NOT:
					new_node = new_bd_ia32_Not(dbgi, new_block, new_node);
					SET_IA32_ORIG_NODE(new_node, node);
					break;

				case SETCC_TR_AND: {
					ir_graph *const irg = get_Block_irg(new_block);
					imm = ia32_immediate_from_long(irg, res.steps[step].val);
					SET_IA32_ORIG_NODE(imm, node);
					new_node = new_bd_ia32_And(dbgi, new_block, noreg_GP, noreg_GP, nomem, new_node, imm);
					SET_IA32_ORIG_NODE(new_node, node);
					break;
				}

				case SETCC_TR_SET:
					new_node = create_set_32bit(dbgi, new_block, flags, res.cc, node);
					break;
				default:
					panic("unknown setcc transform");
				}
			}
		} else {
			new_node = create_CMov(node, sel, flags, cc);
		}
		return new_node;
	}
}

static void force_int_stackent(ir_node *node, ir_mode *mode)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	if (get_mode_size_bits(mode) == 64) {
		attr->data.need_64bit_stackent = true;
	} else {
		assert(get_mode_size_bits(mode) == 32);
		attr->data.need_32bit_stackent = true;
	}
}

/**
 * Create a conversion from x87 state register to general purpose.
 */
static ir_node *gen_x87_fp_to_gp(ir_node *node)
{
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *op     = get_Conv_op(node);
	ir_node  *new_op = be_transform_node(op);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_graph *irg    = get_Block_irg(block);
	ir_node  *frame  = get_irg_frame(irg);

	ir_node *fist = gen_fist(dbgi, block, frame, noreg_GP, nomem, new_op);
	set_irn_pinned(fist, op_pin_state_floats);
	set_ia32_use_frame(fist);
	set_ia32_op_type(fist, ia32_AddrModeD);
	arch_add_irn_flags(fist, arch_irn_flag_spill);

	assert((long)pn_ia32_fist_M == (long) pn_ia32_fisttp_M);
	ir_node *mem = new_r_Proj(fist, mode_M, pn_ia32_fist_M);

	ir_mode *mode = get_irn_mode(node);
	assert(get_mode_size_bits(mode) <= 32);
	/* exception we can only store signed 32 bit integers, so for unsigned
	   we store a 64bit (signed) integer and load the lower bits */
	ir_mode *ls_mode = ia32_mode_gp;
	if (get_mode_size_bits(mode) == 32 && !mode_is_signed(mode)) {
		ls_mode = mode_Ls;
	}
	set_ia32_ls_mode(fist, ls_mode);
	force_int_stackent(fist, ls_mode);
	SET_IA32_ORIG_NODE(fist, node);

	/* do a Load */
	ir_node *load = new_bd_ia32_Load(dbgi, block, frame, noreg_GP, mem);

	set_irn_pinned(load, op_pin_state_floats);
	set_ia32_use_frame(load);
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_ls_mode(load, ia32_mode_gp);
	force_int_stackent(load, ls_mode);
	SET_IA32_ORIG_NODE(load, node);

	return new_r_Proj(load, ia32_mode_gp, pn_ia32_Load_res);
}

/**
 * Creates a x87 Conv by placing a Store and a Load
 */
static ir_node *gen_x87_conv(ir_mode *tgt_mode, ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_Block_irg(block);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *frame = get_irg_frame(irg);

	ir_node *store = new_bd_ia32_fst(dbgi, block, frame, noreg_GP, nomem, node,
	                                 tgt_mode);
	set_irn_pinned(store, op_pin_state_floats);
	set_ia32_use_frame(store);
	set_ia32_op_type(store, ia32_AddrModeD);
	arch_add_irn_flags(store, arch_irn_flag_spill);
	SET_IA32_ORIG_NODE(store, node);

	ir_node *store_mem = new_r_Proj(store, mode_M, pn_ia32_fst_M);

	ir_node *load = new_bd_ia32_fld(dbgi, block, frame, noreg_GP, store_mem,
	                                tgt_mode);
	set_irn_pinned(load, op_pin_state_floats);
	set_ia32_use_frame(load);
	set_ia32_op_type(load, ia32_AddrModeS);
	SET_IA32_ORIG_NODE(load, node);

	ir_node *new_node = new_r_Proj(load, ia32_mode_E, pn_ia32_fld_res);
	return new_node;
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
						match_am | match_try_am | match_upconv | match_16bit_am);
		if (am->op_type == ia32_AddrModeS)
			return;
	}

	ir_node  *new_node = be_transform_node(value);

	/* first convert to 32 bit signed if necessary */
	if (get_mode_size_bits(mode) < 32) {
		if (!be_upper_bits_clean(value, mode)) {
			new_node = create_Conv_I2I(dbgi, block, noreg_GP, noreg_GP, nomem,
			                           new_node, mode);
		}
		mode = mode_Is;
	}

	/* do a store */
	ir_graph *irg       = get_Block_irg(block);
	ir_node  *frame     = get_irg_frame(irg);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *store     = new_bd_ia32_Store(dbgi, new_block, frame, noreg_GP,
	                                        nomem, new_node);

	set_irn_pinned(store, op_pin_state_floats);
	set_ia32_use_frame(store);
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, ia32_mode_gp);
	arch_add_irn_flags(store, arch_irn_flag_spill);

	ir_node *store_mem = new_r_Proj(store, mode_M, pn_ia32_Store_M);

	/* exception for 32bit unsigned, do a 64bit spill+load */
	ir_mode *store_mode;
	if (!mode_is_signed(mode) && extend_unsigned) {
		ir_node *in[2];
		/* store a zero */
		ir_node *zero_const = ia32_create_Immediate(irg, NULL, 0);

		ir_node *zero_store = new_bd_ia32_Store(dbgi, new_block, frame,
		                                        noreg_GP, nomem, zero_const);
		ir_node *zero_store_mem = new_r_Proj(zero_store, mode_M, pn_ia32_Store_M);

		set_irn_pinned(zero_store, op_pin_state_floats);
		set_ia32_use_frame(zero_store);
		set_ia32_op_type(zero_store, ia32_AddrModeD);
		add_ia32_am_offs_int(zero_store, 4);
		set_ia32_ls_mode(zero_store, ia32_mode_gp);
		arch_add_irn_flags(zero_store, arch_irn_flag_spill);
		ia32_attr_t *zero_store_attr = get_ia32_attr(zero_store);
		zero_store_attr->data.need_64bit_stackent = true;

		in[0] = zero_store_mem;
		in[1] = store_mem;

		store_mem  = new_rd_Sync(dbgi, new_block, 2, in);
		store_mode = mode_Ls;
		force_int_stackent(zero_store, store_mode);
	} else {
		store_mode = ia32_mode_gp;
	}
	force_int_stackent(store, store_mode);

	memset(am, 0, sizeof(*am));
	x86_address_t *addr = &am->addr;
	addr->base      = frame;
	addr->index     = noreg_GP;
	addr->mem       = store_mem;
	addr->use_frame = true;
	am->op_type     = ia32_AddrModeS;
	am->ls_mode     = store_mode;
	am->pinned      = op_pin_state_floats;
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
	ir_node *fild     = new_bd_ia32_fild(dbgi, new_block, addr->base,
	                                     addr->index, addr->mem);
	ir_node *new_node = new_r_Proj(fild, mode_fp, pn_ia32_fild_res);
	if (addr->use_frame && addr->entity == NULL
	    && get_mode_arithmetic(am.ls_mode) != irma_twos_complement)
		force_int_stackent(fild, am.ls_mode);

	set_am_attributes(fild, &am);
	SET_IA32_ORIG_NODE(fild, node);

	fix_mem_proj(fild, &am);
	return new_node;
}

/**
 * Create a conversion from one integer mode into another one
 */
static ir_node *create_I2I_Conv(ir_mode *src_mode, ir_mode *tgt_mode,
                                dbg_info *dbgi, ir_node *block, ir_node *op,
                                ir_node *node)
{
	(void)node;
	(void)tgt_mode;
	assert(get_mode_size_bits(src_mode) < get_mode_size_bits(tgt_mode));

#ifdef DEBUG_libfirm
	if (is_Const(op)) {
		ir_fprintf(stderr, "Optimization warning: conv after constant %+F\n",
		           op);
	}
#endif

	while(is_downconv(op)) {
		op = get_Conv_op(op);
	}

	if (be_upper_bits_clean(op, src_mode)) {
		return be_transform_node(op);
	}

	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, op, NULL,
	                match_am | match_8bit_am | match_16bit_am);

	x86_address_t *addr = &am.addr;
	ir_node *new_block = be_transform_node(block);
	ir_node *new_node = create_Conv_I2I(dbgi, new_block, addr->base,
	                                    addr->index, addr->mem, am.new_op2,
	                                    src_mode);
	set_am_attributes(new_node, &am);
	/* match_arguments assume that out-mode = in-mode, this isn't true here
	 * so fix it */
	set_ia32_ls_mode(new_node, src_mode);
	SET_IA32_ORIG_NODE(new_node, node);
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

	/* modeB -> X should already be lowered by the lower_mode_b pass */
	if (src_mode == mode_b) {
		panic("ConvB not lowered %+F", node);
	}

	if (src_mode == tgt_mode) {
		/* this should be optimized already, but who knows... */
		DEBUG_ONLY(ir_fprintf(stderr, "Debug warning: conv %+F is pointless\n", node);)
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
			/* ... to float */
			if (ia32_cg_config.use_sse2) {
				DB((dbg, LEVEL_1, "create Conv(float, float) ..."));
				res = new_bd_ia32_Conv_FP2FP(dbgi, new_block, noreg_GP,
				                             noreg_GP, nomem, new_op);
				set_ia32_ls_mode(res, tgt_mode);
			} else {
				if (src_bits < tgt_bits) {
					DB((dbg, LEVEL_1, "killed Conv(float, float) ..."));
					return new_op;
				} else {
					ir_node *res = gen_x87_conv(tgt_mode, new_op);
					SET_IA32_ORIG_NODE(get_Proj_pred(res), node);
					return res;
				}
			}
		} else {
			/* ... to int */
			DB((dbg, LEVEL_1, "create Conv(float, int) ..."));
			if (ia32_cg_config.use_sse2) {
				res = new_bd_ia32_Conv_FP2I(dbgi, new_block, noreg_GP, noreg_GP,
				                            nomem, new_op);
				set_ia32_ls_mode(res, src_mode);
			} else {
				return gen_x87_fp_to_gp(node);
			}
		}
	} else {
		/* we convert from int ... */
		if (mode_is_float(tgt_mode)) {
			/* ... to float */
			DB((dbg, LEVEL_1, "create Conv(int, float) ..."));
			if (ia32_cg_config.use_sse2) {
				ir_node *new_op = be_transform_node(op);
				res = new_bd_ia32_Conv_I2FP(dbgi, new_block, noreg_GP, noreg_GP,
				                            nomem, new_op);
				set_ia32_ls_mode(res, tgt_mode);
			} else {
				unsigned int_mantissa   = get_mode_size_bits(src_mode) - (mode_is_signed(src_mode) ? 1 : 0);
				unsigned float_mantissa = get_mode_mantissa_size(tgt_mode);
				ir_node *res = gen_x87_gp_to_fp(node);

				/* we need a float-conv, if the int mode has more bits than the
				 * float mantissa */
				if (float_mantissa < int_mantissa) {
					res = gen_x87_conv(tgt_mode, res);
					SET_IA32_ORIG_NODE(get_Proj_pred(res), node);
				}
				return res;
			}
		} else if (tgt_mode == mode_b) {
			/* mode_b lowering already took care that we only have 0/1 values */
			DB((dbg, LEVEL_1, "omitting unnecessary Conv(%+F, %+F) ...",
			    src_mode, tgt_mode));
			return be_transform_node(op);
		} else {
			/* to int */
			if (src_bits >= tgt_bits) {
				DB((dbg, LEVEL_1, "omitting unnecessary Conv(%+F, %+F) ...",
				    src_mode, tgt_mode));
				return be_transform_node(op);
			}

			res = create_I2I_Conv(src_mode, tgt_mode, dbgi, block, op, node);
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
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_value = be_transform_node(value);
	ir_graph *irg       = get_Block_irg(block);
	ir_node  *frame     = get_irg_frame(irg);
	ir_mode  *mode      = get_irn_mode(value);

	ir_node *fst = new_bd_ia32_fst(dbgi, new_block, frame, noreg_GP, nomem,
	                               new_value, mode);
	set_irn_pinned(fst, op_pin_state_floats);
	set_ia32_use_frame(fst);
	set_ia32_op_type(fst, ia32_AddrModeD);
	arch_add_irn_flags(fst, arch_irn_flag_spill);
	force_int_stackent(fst, mode);
	ir_node *mem = new_r_Proj(fst, mode_M, pn_ia32_fst_M);

	memset(am, 0, sizeof(*am));
	x86_address_t *addr = &am->addr;
	addr->base      = frame;
	addr->index     = noreg_GP;
	addr->mem       = mem;
	addr->use_frame = true;
	am->op_type     = ia32_AddrModeS;
	am->ls_mode     = mode;
	am->pinned      = op_pin_state_floats;
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
		panic("ia32: unexpected src mode in Bitcast");
	}

	ir_node *new_block = be_transform_node(block);
	ir_node *res;
	switch (get_mode_arithmetic(dst_mode)) {
	case irma_ieee754:
	case irma_x86_extended_float: {
		const x86_address_t *addr = &am.addr;
		ir_node *fld      = new_bd_ia32_fld(dbgi, new_block, addr->base,
		                                    addr->index, addr->mem, dst_mode);
		force_int_stackent(fld, dst_mode);
		res = new_r_Proj(fld, mode_fp, pn_ia32_fld_res);

		am.ls_mode = dst_mode;
		set_am_attributes(fld, &am);
		SET_IA32_ORIG_NODE(fld, node);
		fix_mem_proj(fld, &am);
		break;
	}
	case irma_twos_complement: {
		const x86_address_t *addr = &am.addr;
		ir_node *ld = new_bd_ia32_Load(dbgi, new_block, addr->base, addr->index,
		                               addr->mem);
		force_int_stackent(ld, dst_mode);
		res = new_r_Proj(ld, ia32_mode_gp, pn_ia32_Load_res);
		am.ls_mode = dst_mode;
		set_am_attributes(ld, &am);
		SET_IA32_ORIG_NODE(ld, node);
		fix_mem_proj(ld, &am);
		break;
	}
	default:
		panic("ia32: unexpected dst mode in Bitcast");
	}
	return res;
}

static ir_node *create_immediate_or_transform(ir_node *const node)
{
	ir_node *new_node = ia32_try_create_Immediate(node, 'i');
	if (new_node == NULL) {
		new_node = be_transform_node(node);
	}
	return new_node;
}

/**
 * Transforms a FrameAddr into an ia32 Add.
 */
static ir_node *gen_Member(ir_node *node)
{

	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *ptr   = get_Member_ptr(node);
	/* the only non-lowered member nodes should select entities from the
	 * stackframe */
	if (!is_Proj(ptr) || !be_is_Start(get_Proj_pred(ptr)))
		panic("%+F not lowered", node);
	ir_node   *new_ptr  = be_transform_node(ptr);
	dbg_info  *dbgi     = get_irn_dbg_info(node);
	ir_node   *new_node = new_bd_ia32_Lea(dbgi, block, new_ptr, noreg_GP);
	ir_entity *entity   = get_Member_entity(node);
	set_ia32_frame_ent(new_node, entity);
	set_ia32_use_frame(new_node);

	SET_IA32_ORIG_NODE(new_node, node);
	return new_node;
}

/**
 * In case SSE is used we need to copy the result from XMM0 to FPU TOS before return.
 */
static ir_node *gen_be_Return(ir_node *node)
{
	if (be_Return_get_n_rets(node) < 1 || !ia32_cg_config.use_sse2) {
		return be_duplicate_node(node);
	}

	ir_node   *block    = be_transform_node(get_nodes_block(node));
	ir_graph  *irg      = get_Block_irg(block);
	ir_entity *ent      = get_irg_entity(irg);
	ir_type   *tp       = get_entity_type(ent);
	ir_type   *res_type = get_method_res_type(tp, 0);

	if (!is_Primitive_type(res_type)) {
		return be_duplicate_node(node);
	}

	ir_mode *mode = get_type_mode(res_type);
	if (!mode_is_float(mode)) {
		return be_duplicate_node(node);
	}

	assert(get_method_n_ress(tp) == 1);

	ir_node *frame = get_irg_frame(irg);

	/* store xmm0 onto stack */
	ir_node  *ret_val     = get_irn_n(node, n_be_Return_val);
	assert(ret_val != NULL);
	ir_node  *ret_mem     = get_irn_n(node, n_be_Return_mem);
	ir_node  *new_ret_val = be_transform_node(ret_val);
	ir_node  *new_ret_mem = be_transform_node(ret_mem);
	dbg_info *dbgi        = get_irn_dbg_info(node);
	ir_node  *sse_store   = new_bd_ia32_xStoreSimple(dbgi, block, frame,
	                                                 noreg_GP, new_ret_mem,
	                                                 new_ret_val);
	set_irn_pinned(sse_store, op_pin_state_floats);
	set_ia32_ls_mode(sse_store, mode);
	set_ia32_op_type(sse_store, ia32_AddrModeD);
	set_ia32_use_frame(sse_store);
	arch_add_irn_flags(sse_store, arch_irn_flag_spill);
	ir_node *store_mem = new_r_Proj(sse_store, mode_M, pn_ia32_xStoreSimple_M);

	/* load into x87 register */
	ir_node *fld = new_bd_ia32_fld(dbgi, block, frame, noreg_GP, store_mem,
	                               mode);
	set_irn_pinned(fld, op_pin_state_floats);
	set_ia32_op_type(fld, ia32_AddrModeS);
	set_ia32_use_frame(fld);

	ir_node *mproj = new_r_Proj(fld, mode_M, pn_ia32_fld_M);
	fld   = new_r_Proj(fld, mode_fp, pn_ia32_fld_res);

	/* create a new return */
	int       arity = get_irn_arity(node);
	ir_node **in    = ALLOCAN(ir_node*, arity);
	unsigned  pop   = be_Return_get_pop(node);
	foreach_irn_in(node, i, op) {
		if (op == ret_val) {
			in[i] = fld;
		} else if (op == ret_mem) {
			in[i] = mproj;
		} else {
			in[i] = be_transform_node(op);
		}
	}
	ir_node *const new_node = be_new_Return(dbgi, block, arity, pop, arity, in);
	copy_node_attr(irg, node, new_node);

	return new_node;
}

/**
 * Transform a be_AddSP into an ia32_SubSP.
 */
static ir_node *gen_be_AddSP(ir_node *node)
{
	ir_node *sz       = get_irn_n(node, n_be_AddSP_size);
	ir_node *sp       = get_irn_n(node, n_be_AddSP_old_sp);
	ir_node *new_node = gen_binop(node, sp, sz, new_bd_ia32_SubSP,
	                              match_am | match_immediate);
	assert(is_ia32_SubSP(new_node));
	arch_set_irn_register_out(new_node, pn_ia32_SubSP_stack,
	                          &ia32_registers[REG_ESP]);
	return new_node;
}

/**
 * Transform a be_SubSP into an ia32_AddSP
 */
static ir_node *gen_be_SubSP(ir_node *node)
{
	ir_node *sz       = get_irn_n(node, n_be_SubSP_size);
	ir_node *sp       = get_irn_n(node, n_be_SubSP_old_sp);
	ir_node *new_node = gen_binop(node, sp, sz, new_bd_ia32_AddSP,
	                              match_am | match_immediate);
	assert(is_ia32_AddSP(new_node));
	arch_set_irn_register_out(new_node, pn_ia32_AddSP_stack,
	                          &ia32_registers[REG_ESP]);
	return new_node;
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (ia32_mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		req  = ia32_reg_classes[CLASS_ia32_gp].class_req;
	} else if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			req  = ia32_reg_classes[CLASS_ia32_xmm].class_req;
		} else {
			req  = ia32_reg_classes[CLASS_ia32_fp].class_req;
		}
	} else {
		req = arch_no_register_req;
	}
	return be_transform_phi(node, req);
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_node  = new_bd_ia32_Jmp(dbgi, new_block);
	SET_IA32_ORIG_NODE(new_node, node);
	return new_node;
}

/**
 * Transform IJmp
 */
static ir_node *gen_IJmp(ir_node *node)
{
	ir_node *block = get_nodes_block(node);
	ir_node *op    = get_IJmp_target(node);
	assert(get_irn_mode(op) == mode_P);

	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, op, NULL,
	                match_am | match_immediate | match_upconv);

	x86_address_t *addr      = &am.addr;
	dbg_info      *dbgi      = get_irn_dbg_info(node);
	ir_node       *new_block = be_transform_node(block);
	ir_node       *new_node  = new_bd_ia32_IJmp(dbgi, new_block, addr->base,
	                                            addr->index, addr->mem,
	                                            am.new_op2);
	set_am_attributes(new_node, &am);
	SET_IA32_ORIG_NODE(new_node, node);

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
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *low       = get_irn_n(node, n_ia32_l_Minus64_low);
	ir_node  *high      = get_irn_n(node, n_ia32_l_Minus64_high);
	ir_node  *new_low   = be_transform_node(low);
	ir_node  *new_high  = be_transform_node(high);
	return new_bd_ia32_Minus64(dbgi, new_block, new_low, new_high);
}

static ir_node *gen_ia32_l_LLtoFloat(ir_node *node)
{
	if (ia32_cg_config.use_sse2) {
		panic("not implemented for SSE2");
	}

	ir_node  *src_block    = get_nodes_block(node);
	ir_node  *block        = be_transform_node(src_block);
	ir_graph *irg          = get_Block_irg(block);
	dbg_info *dbgi         = get_irn_dbg_info(node);
	ir_node  *frame        = get_irg_frame(irg);
	ir_node  *val_low      = get_irn_n(node, n_ia32_l_LLtoFloat_val_low);
	ir_node  *val_high     = get_irn_n(node, n_ia32_l_LLtoFloat_val_high);
	ir_node  *new_val_low  = be_transform_node(val_low);
	ir_node  *new_val_high = be_transform_node(val_high);

	/* do a store */
	ir_node *store_low = new_bd_ia32_Store(dbgi, block, frame, noreg_GP, nomem,
	                                       new_val_low);
	ir_node *store_high = new_bd_ia32_Store(dbgi, block, frame, noreg_GP, nomem,
	                                        new_val_high);
	SET_IA32_ORIG_NODE(store_low,  node);
	SET_IA32_ORIG_NODE(store_high, node);

	ir_node *mem_low  = new_r_Proj(store_low, mode_M, pn_ia32_Store_M);
	ir_node *mem_high = new_r_Proj(store_high, mode_M, pn_ia32_Store_M);

	set_irn_pinned(store_low, op_pin_state_floats);
	set_irn_pinned(store_high, op_pin_state_floats);
	set_ia32_use_frame(store_low);
	set_ia32_use_frame(store_high);
	set_ia32_op_type(store_low, ia32_AddrModeD);
	set_ia32_op_type(store_high, ia32_AddrModeD);
	set_ia32_ls_mode(store_low, ia32_mode_gp);
	set_ia32_ls_mode(store_high, ia32_mode_gp);
	arch_add_irn_flags(store_low, arch_irn_flag_spill);
	arch_add_irn_flags(store_high, arch_irn_flag_spill);
	force_int_stackent(store_low, mode_Ls);
	force_int_stackent(store_high, mode_Ls);
	add_ia32_am_offs_int(store_high, 4);

	ir_node *in[2] = { mem_low, mem_high };
	ir_node *sync = new_rd_Sync(dbgi, block, ARRAY_SIZE(in), in);

	/* do a fild */
	ir_node *fild = new_bd_ia32_fild(dbgi, block, frame, noreg_GP, sync);
	set_irn_pinned(fild, op_pin_state_floats);
	set_ia32_use_frame(fild);
	set_ia32_op_type(fild, ia32_AddrModeS);
	set_ia32_ls_mode(fild, mode_Ls);
	force_int_stackent(fild, mode_Ls);

	SET_IA32_ORIG_NODE(fild, node);

	ir_node *res = new_r_Proj(fild, mode_fp, pn_ia32_fild_res);

	if (!mode_is_signed(get_irn_mode(val_high))) {
		ir_node *count = ia32_create_Immediate(irg, NULL, 31);

		ia32_address_mode_t am;
		am.addr.base         = get_global_base(irg);
		am.addr.index        = new_bd_ia32_Shr(dbgi, block, new_val_high, count);
		am.addr.mem          = nomem;
		am.addr.offset       = 0;
		am.addr.scale        = 2;
		am.addr.entity       = ia32_gen_fp_known_const(irg, ia32_ULLBIAS);
		am.addr.tls_segment  = false;
		am.addr.use_frame    = 0;
		am.addr.frame_entity = NULL;
		am.ls_mode           = ia32_mode_float32;
		am.mem_proj          = nomem;
		am.op_type           = ia32_AddrModeS;
		am.new_op1           = res;
		am.new_op2           = ia32_new_NoReg_fp(irg);
		am.pinned            = op_pin_state_floats;
		am.commutative       = 1;
		am.ins_permuted      = false;

		ir_node *fadd = new_bd_ia32_fadd(dbgi, block, am.addr.base,
		                                 am.addr.index, am.addr.mem,
		                                 am.new_op1, am.new_op2, get_fpcw());
		set_am_attributes(fadd, &am);

		set_irn_mode(fadd, mode_T);
		res = new_r_Proj(fadd, mode_fp, pn_ia32_res);
	}
	return res;
}

static ir_node *gen_ia32_l_FloattoLL(ir_node *node)
{
	ir_node  *src_block = get_nodes_block(node);
	ir_node  *block     = be_transform_node(src_block);
	ir_graph *irg       = get_Block_irg(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *frame     = get_irg_frame(irg);
	ir_node  *val       = get_irn_n(node, n_ia32_l_FloattoLL_val);
	ir_node  *new_val   = be_transform_node(val);

	ir_node *fist = gen_fist(dbgi, block, frame, noreg_GP, nomem, new_val);
	set_irn_pinned(fist, op_pin_state_floats);
	SET_IA32_ORIG_NODE(fist, node);
	set_ia32_use_frame(fist);
	set_ia32_op_type(fist, ia32_AddrModeD);
	set_ia32_ls_mode(fist, mode_Ls);
	arch_add_irn_flags(fist, arch_irn_flag_spill);
	force_int_stackent(fist, mode_Ls);

	assert((long)pn_ia32_fist_M == (long) pn_ia32_fisttp_M);
	return new_r_Proj(fist, mode_M, pn_ia32_fist_M);
}

static ir_node *gen_Proj_l_FloattoLL(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_graph *irg      = get_Block_irg(block);
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	ir_node  *frame    = get_irg_frame(irg);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      pn       = get_Proj_proj(node);

	ir_node *load = new_bd_ia32_Load(dbgi, block, frame, noreg_GP, new_pred);
	set_irn_pinned(load, op_pin_state_floats);
	SET_IA32_ORIG_NODE(load, node);
	set_ia32_use_frame(load);
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_ls_mode(load, ia32_mode_gp);
	force_int_stackent(load, mode_Ls);

	if (pn == pn_ia32_l_FloattoLL_res_high) {
		add_ia32_am_offs_int(load, 4);
	} else {
		assert(pn == pn_ia32_l_FloattoLL_res_low);
	}

	ir_node *proj = new_r_Proj(load, ia32_mode_gp, pn_ia32_Load_res);
	return proj;
}

/**
 * Transform the Projs of an AddSP.
 */
static ir_node *gen_Proj_be_AddSP(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      proj     = get_Proj_proj(node);

	if (proj == pn_be_AddSP_sp) {
		ir_node *res = new_rd_Proj(dbgi, new_pred, ia32_mode_gp,
		                           pn_ia32_SubSP_stack);
		arch_set_irn_register(res, &ia32_registers[REG_ESP]);
		return res;
	} else if (proj == pn_be_AddSP_res) {
		return new_rd_Proj(dbgi, new_pred, ia32_mode_gp,
		                   pn_ia32_SubSP_addr);
	} else if (proj == pn_be_AddSP_M) {
		return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_SubSP_M);
	}

	panic("No idea how to transform proj->AddSP");
}

/**
 * Transform the Projs of a SubSP.
 */
static ir_node *gen_Proj_be_SubSP(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      proj     = get_Proj_proj(node);

	if (proj == pn_be_SubSP_sp) {
		ir_node *res = new_rd_Proj(dbgi, new_pred, ia32_mode_gp,
		                           pn_ia32_AddSP_stack);
		arch_set_irn_register(res, &ia32_registers[REG_ESP]);
		return res;
	} else if (proj == pn_be_SubSP_M) {
		return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_AddSP_M);
	}

	panic("No idea how to transform proj->SubSP");
}

/**
 * Transform and renumber the Projs from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *pred = get_Proj_pred(node);
	dbg_info *dbgi = get_irn_dbg_info(node);
	long      proj = get_Proj_proj(node);

	/* loads might be part of source address mode matches, so we don't
	 * transform the ProjMs yet (with the exception of loads whose result is
	 * not used)
	 */
	if (is_Load(pred) && proj == pn_Load_M && get_irn_n_edges(pred) > 1) {
		/* this is needed, because sometimes we have loops that are only
		   reachable through the ProjM */
		be_enqueue_preds(node);
		/* do it in 2 steps, to silence firm verifier */
		ir_node *res = new_rd_Proj(dbgi, pred, mode_M, pn_Load_M);
		set_Proj_proj(res, pn_ia32_mem);
		return res;
	}

	/* renumber the proj */
	ir_node *new_pred = be_transform_node(pred);
	if (is_ia32_Load(new_pred)) {
		switch ((pn_Load)proj) {
		case pn_Load_res:
			return new_rd_Proj(dbgi, new_pred, ia32_mode_gp, pn_ia32_Load_res);
		case pn_Load_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_Load_M);
		case pn_Load_X_except:
			/* This Load might raise an exception. Mark it. */
			set_ia32_exc_label(new_pred, 1);
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Load_X_except);
		case pn_Load_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Load_X_regular);
		}
	} else if (is_ia32_Conv_I2I(new_pred)) {
		set_irn_mode(new_pred, mode_T);
		switch ((pn_Load)proj) {
		case pn_Load_res:
			return new_rd_Proj(dbgi, new_pred, ia32_mode_gp, pn_ia32_res);
		case pn_Load_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_mem);
		case pn_Load_X_except:
			/* This Load might raise an exception. Mark it. */
			set_ia32_exc_label(new_pred, 1);
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Conv_I2I_X_except);
		case pn_Load_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Conv_I2I_X_regular);
		}
	} else if (is_ia32_xLoad(new_pred)) {
		switch ((pn_Load)proj) {
		case pn_Load_res:
			return new_rd_Proj(dbgi, new_pred, mode_xmm, pn_ia32_xLoad_res);
		case pn_Load_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_xLoad_M);
		case pn_Load_X_except:
			/* This Load might raise an exception. Mark it. */
			set_ia32_exc_label(new_pred, 1);
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_xLoad_X_except);
		case pn_Load_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_xLoad_X_regular);
		}
	} else if (is_ia32_fld(new_pred)) {
		switch ((pn_Load)proj) {
		case pn_Load_res:
			return new_rd_Proj(dbgi, new_pred, mode_fp, pn_ia32_fld_res);
		case pn_Load_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_fld_M);
		case pn_Load_X_except:
			/* This Load might raise an exception. Mark it. */
			set_ia32_exc_label(new_pred, 1);
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fld_X_except);
		case pn_Load_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fld_X_regular);
		}
	} else {
		/* can happen for ProJMs when source address mode happened for the
		   node */

		/* however it should not be the result proj, as that would mean the
		   load had multiple users and should not have been used for
		   SourceAM */
		if (proj != pn_Load_M) {
			panic("internal error: transformed node not a Load");
		}
		return new_rd_Proj(dbgi, new_pred, mode_M, 1);
	}

	panic("No idea how to transform Proj(Load) %+F", node);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      pn       = get_Proj_proj(node);

	if (is_Proj(new_pred)) {
		new_pred = get_Proj_pred(new_pred);
	}

	if (is_ia32_Store(new_pred)) {
		switch ((pn_Store)pn) {
		case pn_Store_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_Store_M);
		case pn_Store_X_except:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Store_X_except);
		case pn_Store_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Store_X_regular);
		}
	} else if (is_ia32_fist(new_pred)) {
		switch ((pn_Store)pn) {
		case pn_Store_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_fist_M);
		case pn_Store_X_except:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fist_X_except);
		case pn_Store_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fist_X_regular);
		}
	} else if (is_ia32_fisttp(new_pred)) {
		switch ((pn_Store)pn) {
		case pn_Store_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_fisttp_M);
		case pn_Store_X_except:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fisttp_X_except);
		case pn_Store_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fisttp_X_regular);
		}
	} else if (is_ia32_fst(new_pred)) {
		switch ((pn_Store)pn) {
		case pn_Store_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_fst_M);
		case pn_Store_X_except:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fst_X_except);
		case pn_Store_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_fst_X_regular);
		}
	} else if (is_ia32_xStore(new_pred)) {
		switch ((pn_Store)pn) {
		case pn_Store_M:
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_xStore_M);
		case pn_Store_X_except:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_xStore_X_except);
		case pn_Store_X_regular:
			return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_xStore_X_regular);
		}
	} else if (is_Sync(new_pred)) {
		/* hack for the case that gen_float_const_Store produced a Sync */
		if (pn == pn_Store_M) {
			return new_pred;
		}
		panic("exception control flow not implemented yet");
	} else if (get_ia32_op_type(new_pred) == ia32_AddrModeD) {
		/* destination address mode */
		if (pn == pn_Store_M) {
			if (get_irn_mode(new_pred) == mode_T) {
				return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_destAM_M);
			} else {
				return new_pred;
			}
		}
		panic("exception control flow for destination AM not implemented yet");
	}

	panic("No idea how to transform Proj(Store) %+F", node);
}

/**
 * Transform and renumber the Projs from a Div or Mod instruction.
 */
static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      proj     = get_Proj_proj(node);

	assert((long)pn_ia32_Div_M == (long)pn_ia32_IDiv_M);
	assert((long)pn_ia32_Div_div_res == (long)pn_ia32_IDiv_div_res);

	switch ((pn_Div)proj) {
	case pn_Div_M:
		if (is_ia32_Div(new_pred) || is_ia32_IDiv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_Div_M);
		} else if (is_ia32_xDiv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_xDiv_M);
		} else if (is_ia32_fdiv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_fdiv_M);
		} else {
			panic("Div transformed to unexpected thing %+F", new_pred);
		}
	case pn_Div_res:
		if (is_ia32_Div(new_pred) || is_ia32_IDiv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, ia32_mode_gp, pn_ia32_Div_div_res);
		} else if (is_ia32_xDiv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_xmm, pn_ia32_xDiv_res);
		} else if (is_ia32_fdiv(new_pred)) {
			return new_rd_Proj(dbgi, new_pred, mode_fp, pn_ia32_fdiv_res);
		} else {
			panic("Div transformed to unexpected thing %+F", new_pred);
		}
	case pn_Div_X_except:
		set_ia32_exc_label(new_pred, 1);
		return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Div_X_except);
	case pn_Div_X_regular:
		return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Div_X_regular);
	}

	panic("No idea how to transform proj->Div");
}

/**
 * Transform and renumber the Projs from a Div or Mod instruction.
 */
static ir_node *gen_Proj_Mod(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      proj     = get_Proj_proj(node);

	assert(is_ia32_Div(new_pred) || is_ia32_IDiv(new_pred));
	assert((long)pn_ia32_Div_M == (long)pn_ia32_IDiv_M);
	assert((long)pn_ia32_Div_mod_res == (long)pn_ia32_IDiv_mod_res);

	switch ((pn_Mod)proj) {
	case pn_Mod_M:
		return new_rd_Proj(dbgi, new_pred, mode_M, pn_ia32_Div_M);
	case pn_Mod_res:
		return new_rd_Proj(dbgi, new_pred, ia32_mode_gp, pn_ia32_Div_mod_res);
	case pn_Mod_X_except:
		set_ia32_exc_label(new_pred, 1);
		return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Div_X_except);
	case pn_Mod_X_regular:
		return new_rd_Proj(dbgi, new_pred, mode_X, pn_ia32_Div_X_regular);
	}
	panic("No idea how to transform proj->Mod");
}

static ir_node *gen_be_Call(ir_node *node)
{
	ir_node        *const src_block = get_nodes_block(node);
	ir_node        *const block     = be_transform_node(src_block);
	ir_type        *const call_tp   = be_Call_get_type(node);

	/* Run the x87 simulator if the call returns a float value */
	if (get_method_n_ress(call_tp) > 0) {
		ir_type *const res_type = get_method_res_type(call_tp, 0);
		ir_mode *const res_mode = get_type_mode(res_type);

		if (res_mode != NULL && mode_is_float(res_mode)) {
			ir_graph *const irg = get_Block_irg(block);
			ia32_request_x87_sim(irg);
		}
	}

	/* We do not want be_Call direct calls */
	assert(be_Call_get_entity(node) == NULL);

	/* special case for PIC trampoline calls */
	bool old_no_pic_adjust = ia32_no_pic_adjust;
	ia32_no_pic_adjust     = be_options.pic;

	ir_node *const src_ptr = get_irn_n(node, n_be_Call_ptr);
	ir_node *const src_mem = get_irn_n(node, n_be_Call_mem);
	ia32_address_mode_t am;
	match_arguments(&am, src_block, NULL, src_ptr, src_mem,
	                match_am | match_immediate | match_upconv);

	ia32_no_pic_adjust = old_no_pic_adjust;

	int      i    = get_irn_arity(node)-1;
	ir_node *fpcw = be_transform_node(get_irn_n(node, i--));
	ir_node *eax  = noreg_GP;
	ir_node *ecx  = noreg_GP;
	ir_node *edx  = noreg_GP;
	for (; i >= n_be_Call_first_arg; --i) {
		arch_register_req_t const *const req
			= arch_get_irn_register_req_in(node, i);
		ir_node *const reg_parm = be_transform_node(get_irn_n(node, i));

		assert(req->type == arch_register_req_type_limited);
		assert(req->cls == &ia32_reg_classes[CLASS_ia32_gp]);

		switch (*req->limited) {
		case 1 << REG_GP_EAX: assert(eax == noreg_GP); eax = reg_parm; break;
		case 1 << REG_GP_ECX: assert(ecx == noreg_GP); ecx = reg_parm; break;
		case 1 << REG_GP_EDX: assert(edx == noreg_GP); edx = reg_parm; break;
		default: panic("Invalid GP register for register parameter");
		}
	}

	unsigned  const pop    = be_Call_get_pop(node);
	ir_node  *const src_sp = get_irn_n(node, n_be_Call_sp);
	ir_node  *const sp     = be_transform_node(src_sp);
	dbg_info *const dbgi   = get_irn_dbg_info(node);

	x86_address_t *const addr = &am.addr;
	ir_node *mem  = transform_AM_mem(block, src_ptr, src_mem, addr->mem);
	ir_node *call = new_bd_ia32_Call(dbgi, block, addr->base, addr->index, mem,
	                                 am.new_op2, sp, fpcw, eax, ecx, edx, pop,
	                                 call_tp);
	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(call, throws_exception);
	set_am_attributes(call, &am);
	arch_set_irn_register_out(call, pn_ia32_Call_stack,
	                          &ia32_registers[REG_ESP]);
	arch_set_irn_register_out(call, pn_ia32_Call_fpcw,
	                          &ia32_registers[REG_FPCW]);
	call = fix_mem_proj(call, &am);

	if (get_irn_pinned(node) == op_pin_state_pinned)
		set_irn_pinned(call, op_pin_state_pinned);

	SET_IA32_ORIG_NODE(call, node);

	if (ia32_cg_config.use_sse2) {
		/* remember this call for post-processing */
		ARR_APP1(ir_node *, call_list, call);
		ARR_APP1(ir_type *, call_types, be_Call_get_type(node));
	}

	return call;
}

/**
 * Transform Builtin trap
 */
static ir_node *gen_trap(ir_node *node)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));
	return new_bd_ia32_UD2(dbgi, block, mem);
}

/**
 * Transform Builtin debugbreak
 */
static ir_node *gen_debugbreak(ir_node *node)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));
	return new_bd_ia32_Breakpoint(dbgi, block, mem);
}

/**
 * Transform Builtin return_address
 */
static ir_node *gen_return_address(ir_node *node)
{
	ir_node      *param = get_Builtin_param(node, 0);
	dbg_info     *dbgi  = get_irn_dbg_info(node);
	ir_node      *block = be_transform_node(get_nodes_block(node));
	ir_graph     *irg   = get_irn_irg(node);
	ir_node      *ptr   = get_irg_frame(irg);
	ir_tarval    *tv    = get_Const_tarval(param);
	unsigned long value = get_tarval_long(tv);
	if (value > 0) {
		ir_node *const cfr = new_bd_ia32_ClimbFrame(dbgi, block, ptr, value);
		ptr = new_r_Proj(cfr, ia32_mode_gp, pn_ia32_ClimbFrame_res);
	}

	/* load the return address from this frame */
	ir_node *load = new_bd_ia32_Load(dbgi, block, ptr, noreg_GP, nomem);

	set_irn_pinned(load, get_irn_pinned(node));
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_ls_mode(load, ia32_mode_gp);

	set_ia32_am_offs_int(load, 0);
	set_ia32_use_frame(load);
	set_ia32_frame_ent(load, ia32_get_return_address_entity(irg));

	if (get_irn_pinned(node) == op_pin_state_floats) {
		assert((int)pn_ia32_xLoad_res == (int)pn_ia32_fld_res
				&& (int)pn_ia32_fld_res == (int)pn_ia32_Load_res
				&& (int)pn_ia32_Load_res == (int)pn_ia32_res);
		arch_add_irn_flags(load, arch_irn_flag_rematerializable);
	}

	SET_IA32_ORIG_NODE(load, node);
	return new_r_Proj(load, ia32_mode_gp, pn_ia32_Load_res);
}

/**
 * Transform Builtin frame_address
 */
static ir_node *gen_frame_address(ir_node *node)
{
	ir_node      *param = get_Builtin_param(node, 0);
	dbg_info     *dbgi  = get_irn_dbg_info(node);
	ir_tarval    *tv    = get_Const_tarval(param);
	ir_graph     *irg   = get_irn_irg(node);
	ir_node      *block = be_transform_node(get_nodes_block(node));
	ir_node      *ptr   = get_irg_frame(irg);
	unsigned long value = get_tarval_long(tv);
	if (value > 0) {
		ir_node *const cfr = new_bd_ia32_ClimbFrame(dbgi, block, ptr, value);
		ptr = new_r_Proj(cfr, ia32_mode_gp, pn_ia32_ClimbFrame_res);
	}

	/* load the frame address from this frame */
	ir_node *load = new_bd_ia32_Load(dbgi, block, ptr, noreg_GP, nomem);

	set_irn_pinned(load, get_irn_pinned(node));
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_ls_mode(load, ia32_mode_gp);

	ir_entity *ent = ia32_get_frame_address_entity(irg);
	if (ent != NULL) {
		set_ia32_am_offs_int(load, 0);
		set_ia32_use_frame(load);
		set_ia32_frame_ent(load, ent);
	} else {
		/* will fail anyway, but gcc does this: */
		set_ia32_am_offs_int(load, 0);
	}

	if (get_irn_pinned(node) == op_pin_state_floats) {
		assert((int)pn_ia32_xLoad_res == (int)pn_ia32_fld_res
				&& (int)pn_ia32_fld_res == (int)pn_ia32_Load_res
				&& (int)pn_ia32_Load_res == (int)pn_ia32_res);
		arch_add_irn_flags(load, arch_irn_flag_rematerializable);
	}

	SET_IA32_ORIG_NODE(load, node);
	return new_r_Proj(load, ia32_mode_gp, pn_ia32_Load_res);
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

	ir_node   *param = get_Builtin_param(node, 1);
	ir_tarval *tv    = get_Const_tarval(param);
	long       rw    = get_tarval_long(tv);

	/* construct load address */
	ir_node      *ptr = get_Builtin_param(node, 0);
	x86_address_t addr;
	create_transformed_address_mode(&addr, ptr, x86_create_am_normal);
	ir_node  *base  = addr.base;
	ir_node  *idx   = addr.index;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));

	ir_node *new_node;
	if (rw == 1 && ia32_cg_config.use_3dnow_prefetch) {
		/* we have 3DNow!, this was already checked above */
		new_node = new_bd_ia32_PrefetchW(dbgi, block, base, idx, mem);
	} else if (ia32_cg_config.use_sse_prefetch) {
		/* note: rw == 1 is IGNORED in that case */
		ir_node   *param    = get_Builtin_param(node, 2);
		ir_tarval *tv       = get_Const_tarval(param);
		long       locality = get_tarval_long(tv);

		/* SSE style prefetch */
		switch (locality) {
		case 0:
			new_node = new_bd_ia32_PrefetchNTA(dbgi, block, base, idx, mem);
			break;
		case 1:
			new_node = new_bd_ia32_Prefetch2(dbgi, block, base, idx, mem);
			break;
		case 2:
			new_node = new_bd_ia32_Prefetch1(dbgi, block, base, idx, mem);
			break;
		default:
			new_node = new_bd_ia32_Prefetch0(dbgi, block, base, idx, mem);
			break;
		}
	} else {
		assert(ia32_cg_config.use_3dnow_prefetch);
		/* 3DNow! style prefetch */
		new_node = new_bd_ia32_Prefetch(dbgi, block, base, idx, mem);
	}

	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeS);
	set_ia32_ls_mode(new_node, mode_Bu);
	set_address(new_node, &addr);

	SET_IA32_ORIG_NODE(new_node, node);

	return new_r_Proj(new_node, mode_M, pn_ia32_Prefetch_M);
}

/**
 * Transform bsf like node
 */
static ir_node *gen_unop_AM(ir_node *node, construct_binop_dest_func *func)
{
	ir_node            *param = get_Builtin_param(node, 0);
	ir_node            *block = get_nodes_block(node);
	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, param, NULL, match_am);

	dbg_info      *dbgi      = get_irn_dbg_info(node);
	ir_node       *new_block = be_transform_node(block);
	x86_address_t *addr      = &am.addr;
	ir_node       *cnt       = func(dbgi, new_block, addr->base, addr->index,
	                                 addr->mem, am.new_op2);
	set_am_attributes(cnt, &am);
	set_ia32_ls_mode(cnt, get_irn_mode(param));

	SET_IA32_ORIG_NODE(cnt, node);
	return fix_mem_proj(cnt, &am);
}

/**
 * Transform builtin ffs.
 */
static ir_node *gen_ffs(ir_node *node)
{
	ir_node *bsf  = gen_unop_AM(node, new_bd_ia32_Bsf);
	ir_node *real = skip_Proj(bsf);

	/* bsf x */
	if (get_irn_mode(real) != mode_T) {
		set_irn_mode(real, mode_T);
		bsf = new_r_Proj(real, ia32_mode_gp, pn_ia32_res);
	}

	ir_node *flag = new_r_Proj(real, mode_b, pn_ia32_flags);

	/* sete */
	dbg_info *dbgi  = get_irn_dbg_info(real);
	ir_node  *block = get_nodes_block(real);
	ir_node  *set   = new_bd_ia32_Setcc(dbgi, block, flag, x86_cc_equal);
	SET_IA32_ORIG_NODE(set, node);

	/* conv to 32bit */
	ir_node *conv = new_bd_ia32_Conv_I2I_8bit(dbgi, block, noreg_GP, noreg_GP,
	                                          nomem, set, mode_Bu);
	SET_IA32_ORIG_NODE(conv, node);

	/* neg */
	ir_node *neg = new_bd_ia32_Neg(dbgi, block, conv);

	/* or */
	ir_node *orn = new_bd_ia32_Or(dbgi, block, noreg_GP, noreg_GP, nomem, bsf,
	                              neg);
	set_ia32_ls_mode(orn, ia32_mode_gp);
	set_ia32_commutative(orn);

	/* add 1 */
	ir_node *add = new_bd_ia32_Lea(dbgi, block, orn, noreg_GP);
	add_ia32_am_offs_int(add, 1);
	return add;
}

/**
 * Transform builtin clz.
 */
static ir_node *gen_clz(ir_node *node)
{
	ir_node  *bsr   = gen_unop_AM(node, new_bd_ia32_Bsr);
	ir_node  *real  = skip_Proj(bsr);
	dbg_info *dbgi  = get_irn_dbg_info(real);
	ir_node  *block = get_nodes_block(real);
	ir_graph *irg   = get_Block_irg(block);
	ir_node  *imm   = ia32_create_Immediate(irg, NULL, 31);

	return new_bd_ia32_Xor(dbgi, block, noreg_GP, noreg_GP, nomem, bsr, imm);
}

/**
 * Transform builtin ctz.
 */
static ir_node *gen_ctz(ir_node *node)
{
	return gen_unop_AM(node, new_bd_ia32_Bsf);
}

/**
 * Transform builtin parity.
 */
static ir_node *gen_parity(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *param     = get_Builtin_param(node, 0);
	ir_node  *new_param = be_transform_node(param);

	/* the x86 parity bit is stupid: it only looks at the lowest byte,
	 * so we have to do complicated xoring first.
	 * (we should also better lower this before the backend so we still have a
	 * chance for CSE, constant folding and other goodies for some of these
	 * operations) */
	ir_graph *const irg   = get_Block_irg(new_block);
	ir_node  *const count = ia32_create_Immediate(irg, NULL, 16);
	ir_node  *const shr   = new_bd_ia32_Shr(dbgi, new_block, new_param, count);
	ir_node  *const xorn  = new_bd_ia32_Xor(dbgi, new_block, noreg_GP,
	                                        noreg_GP, nomem, shr, new_param);
	ir_node  *const xor2  = new_bd_ia32_XorHighLow(dbgi, new_block, xorn);
	set_ia32_ls_mode(xorn, ia32_mode_gp);
	set_ia32_commutative(xorn);

	set_irn_mode(xor2, mode_T);
	ir_node *flags = new_r_Proj(xor2, ia32_mode_gp, pn_ia32_XorHighLow_flags);

	/* setp */
	ir_node *new_node = new_bd_ia32_Setcc(dbgi, new_block, flags,
	                                      x86_cc_not_parity);
	SET_IA32_ORIG_NODE(new_node, node);

	/* conv to 32bit */
	new_node = new_bd_ia32_Conv_I2I_8bit(dbgi, new_block, noreg_GP, noreg_GP,
	                                     nomem, new_node, mode_Bu);
	SET_IA32_ORIG_NODE(new_node, node);
	return new_node;
}

/**
 * Transform builtin popcount
 */
static ir_node *gen_popcount(ir_node *node)
{
	/* builtin lowerer should have replaced the popcount if !use_popcount */
	assert(ia32_cg_config.use_popcnt);

	ir_node            *param = get_Builtin_param(node, 0);
	ir_node            *block = get_nodes_block(node);
	ia32_address_mode_t am;
	match_arguments(&am, block, NULL, param, NULL,
					match_am | match_16bit_am | match_upconv);

	x86_address_t *addr = &am.addr;
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *cnt       = new_bd_ia32_Popcnt(dbgi, new_block, addr->base,
	                                         addr->index, addr->mem,
	                                         am.new_op2);
	set_am_attributes(cnt, &am);
	set_ia32_ls_mode(cnt, get_irn_mode(param));

	SET_IA32_ORIG_NODE(cnt, node);
	return fix_mem_proj(cnt, &am);
}

/**
 * Transform builtin byte swap.
 */
static ir_node *gen_bswap(ir_node *node)
{
	ir_node  *param     = be_transform_node(get_Builtin_param(node, 0));
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_mode  *mode      = get_irn_mode(param);
	unsigned  size      = get_mode_size_bits(mode);

	switch (size) {
	case 32:
		if (ia32_cg_config.use_bswap) {
			/* swap available */
			return new_bd_ia32_Bswap(dbgi, new_block, param);
		} else {
			ir_graph *const irg  = get_Block_irg(new_block);
			ir_node  *const i8   = ia32_create_Immediate(irg, NULL, 8);
			ir_node  *const rol1 = new_bd_ia32_Rol(dbgi, new_block, param, i8);
			ir_node  *const i16  = ia32_create_Immediate(irg, NULL, 16);
			ir_node  *const rol2 = new_bd_ia32_Rol(dbgi, new_block, rol1, i16);
			ir_node  *const rol3 = new_bd_ia32_Rol(dbgi, new_block, rol2, i8);
			set_ia32_ls_mode(rol1, mode_Hu);
			set_ia32_ls_mode(rol2, ia32_mode_gp);
			set_ia32_ls_mode(rol3, mode_Hu);
			return rol3;
		}

	case 16:
		/* swap16 always available */
		return new_bd_ia32_Bswap16(dbgi, new_block, param);

	default:
		panic("Invalid bswap size (%d)", size);
	}
}

/**
 * Transform builtin outport.
 */
static ir_node *gen_outport(ir_node *node)
{
	ir_node  *port  = create_immediate_or_transform(get_Builtin_param(node, 0));
	ir_node  *oldv  = get_Builtin_param(node, 1);
	ir_mode  *mode  = get_irn_mode(oldv);
	ir_node  *value = be_transform_node(oldv);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *res   = new_bd_ia32_Outport(dbgi, block, port, value, mem);
	set_ia32_ls_mode(res, mode);
	return res;
}

/**
 * Transform builtin inport.
 */
static ir_node *gen_inport(ir_node *node)
{
	ir_type  *tp    = get_Builtin_type(node);
	ir_type  *rstp  = get_method_res_type(tp, 0);
	ir_mode  *mode  = get_type_mode(rstp);
	ir_node  *port  = create_immediate_or_transform(get_Builtin_param(node, 0));
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *mem   = be_transform_node(get_Builtin_mem(node));
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *res   = new_bd_ia32_Inport(dbgi, block, port, mem);
	set_ia32_ls_mode(res, mode);
	return res;
}

/*
 * Transform saturating increment.
 */
static ir_node *gen_saturating_increment(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = be_transform_node(get_nodes_block(node));
	ir_node  *operand   = be_transform_node(get_Builtin_param(node, 0));
	ir_graph *irg       = get_Block_irg(block);
	ir_node  *one       = ia32_create_Immediate(irg, NULL, 1);
	ir_node  *increment = new_bd_ia32_Add(dbgi, block, noreg_GP, noreg_GP,
	                                      nomem, operand, one);
	set_irn_mode(increment, mode_T);
	set_ia32_ls_mode(increment, ia32_mode_gp);
	set_ia32_commutative(increment);
	/* We cannot use source address mode and immediate at the same time. */
	set_ia32_am_support(increment, ia32_am_none);
	SET_IA32_ORIG_NODE(increment, node);

	ir_node *value  = new_rd_Proj(dbgi, increment, ia32_mode_gp, pn_ia32_Add_res);
	ir_node *eflags = new_rd_Proj(dbgi, increment, ia32_mode_gp, pn_ia32_Add_flags);
	ir_node *zero   = ia32_create_Immediate(irg, NULL, 0);
	ir_node *sbb    = new_bd_ia32_Sbb(dbgi, block, noreg_GP, noreg_GP, nomem,
	                                  value, zero, eflags);
	set_ia32_ls_mode(sbb, ia32_mode_gp);
	SET_IA32_ORIG_NODE(sbb, node);

	return sbb;
}

static ir_node *gen_compare_swap(ir_node *node)
{
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *ptr     = get_Builtin_param(node, 0);
	ir_node  *old     = get_Builtin_param(node, 1);
	ir_node  *new     = get_Builtin_param(node, 2);
	ir_node  *mem     = get_Builtin_mem(node);
	ir_node  *new_old = be_transform_node(old);
	ir_node  *new_new = be_transform_node(new);
	ir_node  *new_mem = be_transform_node(mem);
	ir_mode  *mode    = get_irn_mode(new);
	assert(get_irn_mode(old) == mode);

	x86_address_t addr;
	create_transformed_address_mode(&addr, ptr, x86_create_am_normal);
	ir_node *base     = addr.base;
	ir_node *idx      = addr.index;
	ir_node *new_node = new_bd_ia32_CmpXChgMem(dbgi, block, base, idx, new_mem,
	                                           new_old, new_new);
	set_irn_pinned(new_node, get_irn_pinned(node));
	set_ia32_op_type(new_node, ia32_AddrModeD);
	set_ia32_ls_mode(new_node, mode);
	set_address(new_node, &addr);
	SET_IA32_ORIG_NODE(new_node, node);
	return new_node;
}

/**
 * Transform a builtin inner trampoline
 */
static ir_node *gen_inner_trampoline(ir_node *node)
{
	ir_node  *ptr       = get_Builtin_param(node, 0);
	ir_node  *callee    = get_Builtin_param(node, 1);
	ir_node  *env       = be_transform_node(get_Builtin_param(node, 2));
	ir_node  *mem       = get_Builtin_mem(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	/* construct store address */
	x86_address_t addr;
	create_transformed_address_mode(&addr, ptr, x86_create_am_normal);
	addr.mem = be_transform_node(mem);

	ir_graph *const irg = get_Block_irg(new_block);
	/* mov  ecx, <env> */
	ir_node *val   = ia32_create_Immediate(irg, NULL, 0xB9);
	ir_node *store = new_bd_ia32_Store_8bit(dbgi, new_block, addr.base,
	                                        addr.index, addr.mem, val);
	set_irn_pinned(store, get_irn_pinned(node));
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, mode_Bu);
	set_address(store, &addr);
	addr.mem = store;
	addr.offset += 1;

	store = new_bd_ia32_Store(dbgi, new_block, addr.base,
	                          addr.index, addr.mem, env);
	set_irn_pinned(store, get_irn_pinned(node));
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, ia32_mode_gp);
	set_address(store, &addr);
	addr.mem = store;
	addr.offset += 4;

	/* jmp rel <callee> */
	val   = ia32_create_Immediate(irg, NULL, 0xE9);
	store = new_bd_ia32_Store_8bit(dbgi, new_block, addr.base, addr.index, addr.mem, val);
	set_irn_pinned(store, get_irn_pinned(node));
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, mode_Bu);
	set_address(store, &addr);
	addr.mem = store;
	addr.offset += 1;

	ir_node *trampoline = be_transform_node(ptr);

	/* the callee is typically an immediate */
	ir_node *rel;
	if (is_Address(callee)) {
		rel = new_bd_ia32_Const(dbgi, new_block, get_Address_entity(callee), 0, -10);
	} else {
		rel = new_bd_ia32_Lea(dbgi, new_block, be_transform_node(callee), noreg_GP);
		add_ia32_am_offs_int(rel, -10);
	}
	rel = new_bd_ia32_Sub(dbgi, new_block, noreg_GP, noreg_GP, nomem, rel, trampoline);

	store = new_bd_ia32_Store(dbgi, new_block, addr.base,
	                          addr.index, addr.mem, rel);
	set_irn_pinned(store, get_irn_pinned(node));
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, ia32_mode_gp);
	set_address(store, &addr);

	ir_node *in[] = { store, trampoline };
	return new_r_Tuple(new_block, ARRAY_SIZE(in), in);
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
	case ir_bk_inner_trampoline:
		return gen_inner_trampoline(node);
	case ir_bk_saturating_increment:
		return gen_saturating_increment(node);
	case ir_bk_compare_swap:
		return gen_compare_swap(node);
	case ir_bk_may_alias:
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
		assert(get_Proj_proj(proj) == pn_Builtin_max+1);
		return new_node;
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_prefetch:
	case ir_bk_outport:
		assert(get_Proj_proj(proj) == pn_Builtin_M);
		return new_node;
	case ir_bk_inport:
		if (get_Proj_proj(proj) == pn_Builtin_max+1) {
			return new_r_Proj(new_node, get_irn_mode(proj), pn_ia32_Inport_res);
		} else {
			assert(get_Proj_proj(proj) == pn_Builtin_M);
			return new_r_Proj(new_node, mode_M, pn_ia32_Inport_M);
		}
	case ir_bk_inner_trampoline:
		if (get_Proj_proj(proj) == pn_Builtin_max+1) {
			return get_Tuple_pred(new_node, 1);
		} else {
			assert(get_Proj_proj(proj) == pn_Builtin_M);
			return get_Tuple_pred(new_node, 0);
		}
	case ir_bk_compare_swap:
		assert(is_ia32_CmpXChgMem(new_node));
		if (get_Proj_proj(proj) == pn_Builtin_M) {
			return new_r_Proj(new_node, mode_M, pn_ia32_CmpXChgMem_M);
		} else {
			assert(get_Proj_proj(proj) == pn_Builtin_max+1);
			return new_r_Proj(new_node, ia32_mode_gp, pn_ia32_CmpXChgMem_res);
		}
	case ir_bk_may_alias:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_be_IncSP(ir_node *node)
{
	ir_node *res = be_duplicate_node(node);
	arch_add_irn_flags(res, arch_irn_flag_modify_flags);
	return res;
}

/**
 * Transform the Projs from a be_Call.
 */
static ir_node *gen_Proj_be_Call(ir_node *node)
{
	ir_node  *call     = get_Proj_pred(node);
	ir_node  *new_call = be_transform_node(call);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      proj     = get_Proj_proj(node);

	long     new_pn;
	ir_mode *mode;
	switch ((pn_be_Call)proj) {
	case pn_be_Call_M:
		new_pn = pn_ia32_Call_M;
		mode   = mode_M;
		break;
	case pn_be_Call_sp:
		new_pn = pn_ia32_Call_stack;
		mode   = ia32_mode_gp;
		break;
	case pn_be_Call_X_regular:
		new_pn = pn_ia32_Call_X_regular;
		mode   = mode_X;
		break;
	case pn_be_Call_X_except:
		new_pn = pn_ia32_Call_X_except;
		mode   = mode_X;
		break;
	default: {
		assert(mode_is_data(get_irn_mode(node)));
		arch_register_req_t const *const req = arch_get_irn_register_req(node);
		assert(proj >= pn_be_Call_first_res);
		assert(arch_register_req_is(req, limited));

		be_foreach_out(new_call, i) {
			arch_register_req_t const *const new_req
				= arch_get_irn_register_req_out(new_call, i);
			if (!arch_register_req_is(new_req, limited)
			    || new_req->cls      != req->cls
			    || *new_req->limited != *req->limited)
				continue;

			new_pn = i;
			mode   = new_req->cls->mode;
			goto found;
		}
		panic("no matching out requirement found");
found:;
	}
	}

	ir_node *res = new_rd_Proj(dbgi, new_call, mode, new_pn);
	return res;
}

static ir_node *gen_Proj_ASM(ir_node *node)
{
	ir_mode *mode     = get_irn_mode(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	long     pos      = get_Proj_proj(node);

	if (mode == mode_M) {
		pos = arch_get_irn_n_outs(new_pred)-1;
	} else if (mode_is_int(mode) || mode_is_reference(mode)) {
		mode = ia32_mode_gp;
	} else if (mode_is_float(mode)) {
		mode = ia32_mode_E;
	} else {
		panic("unexpected proj mode at ASM");
	}

	return new_r_Proj(new_pred, mode, pos);
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	long     proj = get_Proj_proj(node);
	switch (proj) {
	case pn_Start_X_initial_exec: {
		ir_node  *block     = get_nodes_block(pred);
		ir_node  *new_block = be_transform_node(block);
		dbg_info *dbgi      = get_irn_dbg_info(node);
		/* we exchange the ProjX with a jump */
		ir_node  *jump      = new_rd_Jmp(dbgi, new_block);

		return jump;
	}
	}
	return be_duplicate_node(node);
}

static ir_node *gen_Proj_default(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	ir_mode *mode = get_irn_mode(node);
	if (ia32_mode_needs_gp_reg(mode)) {
		ir_node *new_pred = be_transform_node(pred);
		ir_node *new_proj = new_r_Proj(new_pred, ia32_mode_gp,
									   get_Proj_proj(node));
		new_proj->node_nr = node->node_nr;
		return new_proj;
	}
	return be_duplicate_node(node);
}

/**
 * Enters all transform functions into the generic pointer
 */
static void register_transformers(void)
{
	/* first clear the generic function pointer for all ops */
	be_start_transform_setup();

	for (unsigned opc = iro_first; opc <= iro_last; ++opc) {
		ir_op *op = ir_get_opcode(opc);
		be_set_transform_proj_function(op, gen_Proj_default);
	}

	be_set_transform_function(op_Add,              gen_Add);
	be_set_transform_function(op_Address,          gen_Address);
	be_set_transform_function(op_And,              gen_And);
	be_set_transform_function(op_ASM,              ia32_gen_ASM);
	be_set_transform_function(op_be_AddSP,         gen_be_AddSP);
	be_set_transform_function(op_be_Call,          gen_be_Call);
	be_set_transform_function(op_be_IncSP,         gen_be_IncSP);
	be_set_transform_function(op_be_Return,        gen_be_Return);
	be_set_transform_function(op_be_SubSP,         gen_be_SubSP);
	be_set_transform_function(op_Bitcast,          gen_Bitcast);
	be_set_transform_function(op_Builtin,          gen_Builtin);
	be_set_transform_function(op_Cmp,              gen_Cmp);
	be_set_transform_function(op_Cond,             gen_Cond);
	be_set_transform_function(op_Const,            gen_Const);
	be_set_transform_function(op_Conv,             gen_Conv);
	be_set_transform_function(op_CopyB,            ia32_gen_CopyB);
	be_set_transform_function(op_Div,              gen_Div);
	be_set_transform_function(op_Eor,              gen_Eor);
	be_set_transform_function(op_ia32_GetEIP,      be_duplicate_node);
	be_set_transform_function(op_ia32_l_Adc,       gen_ia32_l_Adc);
	be_set_transform_function(op_ia32_l_Add,       gen_ia32_l_Add);
	be_set_transform_function(op_ia32_Leave,       be_duplicate_node);
	be_set_transform_function(op_ia32_l_FloattoLL, gen_ia32_l_FloattoLL);
	be_set_transform_function(op_ia32_l_IMul,      gen_ia32_l_IMul);
	be_set_transform_function(op_ia32_l_LLtoFloat, gen_ia32_l_LLtoFloat);
	be_set_transform_function(op_ia32_l_Minus64,   gen_ia32_l_Minus64);
	be_set_transform_function(op_ia32_l_Mul,       gen_ia32_l_Mul);
	be_set_transform_function(op_ia32_l_Sbb,       gen_ia32_l_Sbb);
	be_set_transform_function(op_ia32_l_Sub,       gen_ia32_l_Sub);
	be_set_transform_function(op_ia32_NoReg_FP,    be_duplicate_node);
	be_set_transform_function(op_ia32_NoReg_GP,    be_duplicate_node);
	be_set_transform_function(op_ia32_NoReg_XMM,   be_duplicate_node);
	be_set_transform_function(op_ia32_PopEbp,      be_duplicate_node);
	be_set_transform_function(op_ia32_Push,        be_duplicate_node);
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
	be_set_transform_function(op_Shl,              gen_Shl);
	be_set_transform_function(op_Shr,              gen_Shr);
	be_set_transform_function(op_Shrs,             gen_Shrs);
	be_set_transform_function(op_Store,            gen_Store);
	be_set_transform_function(op_Sub,              gen_Sub);
	be_set_transform_function(op_Switch,           gen_Switch);
	be_set_transform_function(op_Unknown,          ia32_gen_Unknown);
	be_set_transform_proj_function(op_ASM,              gen_Proj_ASM);
	be_set_transform_proj_function(op_be_AddSP,         gen_Proj_be_AddSP);
	be_set_transform_proj_function(op_be_Call,          gen_Proj_be_Call);
	be_set_transform_proj_function(op_be_Start,         be_duplicate_node);
	be_set_transform_proj_function(op_be_SubSP,         gen_Proj_be_SubSP);
	be_set_transform_proj_function(op_Builtin,          gen_Proj_Builtin);
	be_set_transform_proj_function(op_Div,              gen_Proj_Div);
	be_set_transform_proj_function(op_ia32_l_Adc,       gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Add,       gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_FloattoLL, gen_Proj_l_FloattoLL);
	be_set_transform_proj_function(op_ia32_l_IMul,      gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_LLtoFloat, gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Mul,       gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Sbb,       gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Sub,       gen_Proj_default);
	be_set_transform_proj_function(op_ia32_l_Minus64,   gen_Proj_default);
	be_set_transform_proj_function(op_Load,             gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,              gen_Proj_Mod);
	be_set_transform_proj_function(op_Start,            gen_Proj_Start);
	be_set_transform_proj_function(op_Store,            gen_Proj_Store);

	be_set_upper_bits_clean_function(op_Mux, ia32_mux_upper_bits_clean);
}

/**
 * Pre-transform all unknown and noreg nodes.
 */
static void ia32_pretransform_node(ir_graph *const irg)
{
	ia32_irg_data_t *irg_data = ia32_get_irg_data(irg);

	irg_data->noreg_gp       = be_pre_transform_node(irg_data->noreg_gp);
	irg_data->noreg_fp       = be_pre_transform_node(irg_data->noreg_fp);
	irg_data->noreg_xmm      = be_pre_transform_node(irg_data->noreg_xmm);
	irg_data->get_eip        = be_pre_transform_node(irg_data->get_eip);
	irg_data->fpu_trunc_mode = be_pre_transform_node(irg_data->fpu_trunc_mode);

	nomem    = get_irg_no_mem(irg);
	noreg_GP = ia32_new_NoReg_gp(irg);
}

/**
 * Post-process all calls if we are in SSE mode.
 * The ABI requires that the results are in st0, copy them
 * to a xmm register.
 */
static void postprocess_fp_call_results(void)
{
	for (size_t i = 0, n = ARR_LEN(call_list); i < n; ++i) {
		ir_node *call = call_list[i];
		ir_type *mtp  = call_types[i];

		for (int j = get_method_n_ress(mtp) - 1; j >= 0; --j) {
			ir_type *res_tp = get_method_res_type(mtp, j);
			if (!is_atomic_type(res_tp)) {
				/* no floating point return */
				continue;
			}
			ir_mode *res_mode = get_type_mode(res_tp);
			if (!mode_is_float(res_mode)) {
				/* no floating point return */
				continue;
			}

			ir_node *res     = be_get_Proj_for_pn(call, pn_ia32_Call_st0 + j);
			ir_node *new_res = NULL;

			/* now patch the users */
			foreach_out_edge_safe(res, edge) {
				ir_node *succ = get_edge_src_irn(edge);

				/* ignore Keeps */
				if (be_is_Keep(succ))
					continue;

				if (is_ia32_xStore(succ)) {
					/* an xStore can be patched into an vfst */
					dbg_info *db    = get_irn_dbg_info(succ);
					ir_node  *block = get_nodes_block(succ);
					ir_node  *base  = get_irn_n(succ, n_ia32_xStore_base);
					ir_node  *idx   = get_irn_n(succ, n_ia32_xStore_index);
					ir_node  *mem   = get_irn_n(succ, n_ia32_xStore_mem);
					ir_node  *value = get_irn_n(succ, n_ia32_xStore_val);
					ir_mode  *mode  = get_ia32_ls_mode(succ);
					ir_node  *st    = new_bd_ia32_fst(db, block, base, idx,
					                                  mem, value, mode);
					ia32_copy_am_attrs(st, succ);
					set_irn_pinned(st, get_irn_pinned(succ));
					set_ia32_op_type(st, ia32_AddrModeD);

					assert((long)pn_ia32_xStore_M == (long)pn_ia32_fst_M);
					assert((long)pn_ia32_xStore_X_regular == (long)pn_ia32_fst_X_regular);
					assert((long)pn_ia32_xStore_X_except == (long)pn_ia32_fst_X_except);

					exchange(succ, st);
					continue;
				}

				if (new_res == NULL) {
					ir_node  *const block    = get_nodes_block(call);
					ir_graph *const irg      = get_Block_irg(block);
					ir_node  *const frame    = get_irg_frame(irg);
					ir_node  *const old_mem  = be_get_Proj_for_pn(call, pn_ia32_Call_M);
					ir_node  *const call_mem = new_r_Proj(call, mode_M, pn_ia32_Call_M);

					/* store st(0) on stack */
					dbg_info *db   = get_irn_dbg_info(call);
					ir_node  *vfst = new_bd_ia32_fst(db, block, frame, noreg_GP,
					                                 call_mem, res, res_mode);
					set_irn_pinned(vfst, op_pin_state_floats);
					set_ia32_op_type(vfst, ia32_AddrModeD);
					set_ia32_use_frame(vfst);
					arch_add_irn_flags(vfst, arch_irn_flag_spill);

					ir_node *vfst_mem = new_r_Proj(vfst, mode_M, pn_ia32_fst_M);

					/* load into SSE register */
					ir_node *xld = new_bd_ia32_xLoad(db, block, frame, noreg_GP,
					                                 vfst_mem, res_mode);
					set_irn_pinned(xld, op_pin_state_floats);
					set_ia32_op_type(xld, ia32_AddrModeS);
					set_ia32_use_frame(xld);

					new_res = new_r_Proj(xld, res_mode, pn_ia32_xLoad_res);
					ir_node *new_mem = new_r_Proj(xld, mode_M, pn_ia32_xLoad_M);

					if (old_mem != NULL) {
						edges_reroute(old_mem, new_mem);
						kill_node(old_mem);
					}
				}
				set_irn_n(succ, get_edge_src_pos(edge), new_res);
			}
		}
	}
}

/* do the transformation */
void ia32_transform_graph(ir_graph *irg)
{
	register_transformers();
	initial_fpcw       = NULL;
	ia32_no_pic_adjust = false;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	old_initial_fpcw = be_get_initial_reg_value(irg, &ia32_registers[REG_FPCW]);

	be_timer_push(T_HEIGHTS);
	ia32_heights = heights_new(irg);
	be_timer_pop(T_HEIGHTS);
	x86_calculate_non_address_mode_nodes(irg);

	/* the transform phase is not safe for CSE (yet) because several nodes get
	 * attributes set after their creation */
	int cse_last = get_opt_cse();
	set_opt_cse(0);

	call_list  = NEW_ARR_F(ir_node *, 0);
	call_types = NEW_ARR_F(ir_type *, 0);
	be_transform_graph(irg, ia32_pretransform_node);

	if (ia32_cg_config.use_sse2)
		postprocess_fp_call_results();
	DEL_ARR_F(call_types);
	DEL_ARR_F(call_list);

	set_opt_cse(cse_last);

	x86_free_non_address_mode_nodes();
	heights_free(ia32_heights);
	ia32_heights = NULL;
}

void ia32_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.transform");
}
