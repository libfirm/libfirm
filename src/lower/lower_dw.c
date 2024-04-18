/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower double word operations, i.e. 64bit -> 32bit, 32bit -> 16bit
 * @date    8.10.2004
 * @author  Michael Beck
 */
#include "lower_dw.h"

#include "array.h"
#include "constbits.h"
#include "dbginfo_t.h"
#include "debug.h"
#include "ircons_t.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irflag.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irnodeset.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "lowering.h"
#include "panic.h"
#include "pdeq.h"
#include "pmap.h"
#include "set.h"
#include "target_t.h"
#include "tv_t.h"
#include "type_t.h"
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/** A map from (op, imode, omode) to Intrinsic functions entities. */
static set *intrinsic_fkt;

/** A map from (imode, omode) to conv function types. */
static set *conv_types;

/** A map from a method type to its lowered type. */
static pmap *lowered_type;

/** A map from a builtin type to its lower and higher type. */
static pmap *lowered_builtin_type_high;
static pmap *lowered_builtin_type_low;

/** The types for the binop and unop intrinsics. */
static ir_type *binop_tp_u;
static ir_type *binop_tp_s;
static ir_type *unop_tp_u;
static ir_type *unop_tp_s;
static ir_type *tp_s;
static ir_type *tp_u;
static ir_type *tp_l_s;
static ir_type *tp_l_u;

static ir_nodeset_t created_mux_nodes;

/** the debug handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An entry in the (op, imode, omode) -> entity map.
 */
typedef struct op_mode_entry {
	const ir_op   *op;    /**< the op */
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_entity     *ent;   /**< the associated entity of this (op, imode, omode)
	                           triple */
} op_mode_entry_t;

/**
 * An entry in the (imode, omode) -> tp map.
 */
typedef struct conv_tp_entry {
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_type       *mtd;   /**< the associated method type of this
	                           (imode, omode) pair */
} conv_tp_entry_t;

enum lower_flags {
	MUST_BE_LOWERED = (1 << 0),  /**< graph must be lowered */
	CF_CHANGED      = (1 << 1),  /**< control flow was changed */
};

/**
 * The lower environment.
 */
typedef struct lower_dw_env_t {
	lower64_entry_t **entries;       /**< entries per node */
	ir_graph         *irg;
	struct obstack    obst;          /**< obstack for temporary data */
	ir_tarval        *tv_mode_bytes; /**< a tarval containing the number of
	                                      bytes in the lowered modes */
	deq_t             waitq;         /**< a wait queue of all nodes that must be
	                                      handled later */
	ir_node         **lowered_phis;  /**< list of lowered phis */
	lwrdw_param_t     p;             /**< transformation parameter */
	unsigned          flags;         /**< some flags */
	unsigned          n_entries;     /**< number of entries */
} lower_dw_env_t;

static lower_dw_env_t env;

static void lower_node(ir_node *node);

bool needs_lowering(ir_mode const *const mode)
{
	return get_mode_size_bits(mode) == env.p.doubleword_size
	    && get_mode_arithmetic(mode) == irma_twos_complement;
}

static bool type_needs_lowering(ir_type const *const tp)
{
	if (is_Primitive_type(tp)) {
		ir_mode *mode = get_type_mode(tp);
		if (needs_lowering(mode))
			return true;
	}
	return false;
}

static void lower_mode(void (*const set)(ir_type*, size_t, ir_type*), ir_type *const mtd, ir_mode *const mode)
{
	size_t n = 0;
	if (!needs_lowering(mode)) {
		ir_type *const tp = get_type_for_mode(mode);
		set(mtd, n++, tp);
	} else if (!mode_is_signed(mode)) {
		set(mtd, n++, tp_l_u);
		set(mtd, n++, tp_u);
	} else if (ir_target_big_endian()) {
		set(mtd, n++, tp_l_s);
		set(mtd, n++, tp_u);
	} else {
		set(mtd, n++, tp_l_u);
		set(mtd, n++, tp_s);
	}
}

ir_mode *get_high_mode(ir_mode *const mode)
{
	return mode_is_signed(mode) ? env.p.word_signed : env.p.word_unsigned;
}

/**
 * Create a method type for a Conv emulation from imode to omode.
 */
static ir_type *get_conv_type(ir_mode *imode, ir_mode *omode)
{
	conv_tp_entry_t key;
	key.imode = imode;
	key.omode = omode;
	key.mtd   = NULL;

	unsigned hash = hash_ptr(imode) ^ hash_ptr(omode);
	conv_tp_entry_t *entry
		= set_insert(conv_tp_entry_t, conv_types, &key, sizeof(key), hash);
	ir_type *mtd = entry->mtd;
	if (mtd != NULL)
		return mtd;

	size_t n_param = 1;
	size_t n_res   = 1;
	if (needs_lowering(imode))
		n_param = 2;
	if (needs_lowering(omode))
		n_res = 2;

	/* create a new method type */
	mtd = new_type_method(n_param, n_res, false, cc_cdecl_set, mtp_no_property);

	/* set param types and result types */
	lower_mode(&set_method_param_type, mtd, imode);
	lower_mode(&set_method_res_type,   mtd, omode);

	entry->mtd = mtd;
	return mtd;
}

/**
 * Add an additional control flow input to a block.
 * Patch all Phi nodes. The new Phi inputs are copied from
 * old input number nr.
 */
static void add_block_cf_input_nr(ir_node *block, int nr, ir_node *cf)
{
	int n_cfgpreds = get_Block_n_cfgpreds(block);
	assert(nr < n_cfgpreds);
	ir_node **in = ALLOCAN(ir_node*, n_cfgpreds+1);
	for (int i = 0; i < n_cfgpreds; ++i)
		in[i] = get_Block_cfgpred(block, i);
	in[n_cfgpreds] = cf;
	set_irn_in(block, n_cfgpreds+1, in);

	foreach_out_edge(block, edge) {
		ir_node *phi = get_edge_src_irn(edge);
		if (!is_Phi(phi))
			continue;

		foreach_irn_in(phi, i, pred)
			in[i] = pred;
		in[n_cfgpreds] = in[nr];
		set_irn_in(phi, n_cfgpreds+1, in);
	}
}

/**
 * Add an additional control flow input to a block.
 * Patch all Phi nodes. The new Phi inputs are copied from
 * old input from cf tmpl.
 */
static void add_block_cf_input(ir_node *block, ir_node *tmpl, ir_node *cf)
{
	int nr = -1;
	for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(block); i < n_cfgpreds;
	     ++i) {
		if (get_Block_cfgpred(block, i) == tmpl) {
			nr = i;
			break;
		}
	}
	assert(nr >= 0);
	add_block_cf_input_nr(block, nr, cf);
}

/**
 * Return the "operational" mode of a Firm node.
 */
static ir_mode *get_irn_op_mode(ir_node *node)
{
	switch (get_irn_opcode(node)) {
	case iro_Load:
		return get_Load_mode(node);
	case iro_Store:
		return get_irn_mode(get_Store_value(node));
	case iro_Div:
		return get_irn_mode(get_Div_left(node));
	case iro_Mod:
		return get_irn_mode(get_Mod_left(node));
	case iro_Cmp:
		return get_irn_mode(get_Cmp_left(node));
	default:
		return get_irn_mode(node);
	}
}

/**
 * Walker, prepare the node links and determine which nodes need to be lowered
 * at all.
 */
static void prepare_links(ir_node *node, void *data)
{
	(void)data;
	ir_mode *mode = get_irn_op_mode(node);

	if (needs_lowering(mode)) {
		unsigned idx = get_irn_idx(node);
		/* ok, found a node that will be lowered */
		lower64_entry_t *link = OALLOCZ(&env.obst, lower64_entry_t);

		assert(idx < env.n_entries);
		env.entries[idx] = link;
		env.flags |= MUST_BE_LOWERED;
	} else if (is_Conv(node)) {
		/* Conv nodes have two modes */
		const ir_node *pred = get_Conv_op(node);
		ir_mode *dest_mode = get_irn_mode(pred);

		if (needs_lowering(dest_mode)) {
			/* must lower this node either but don't need a link */
			env.flags |= MUST_BE_LOWERED;
		}
		return;
	} else if (is_Call(node)) {
		/* Special case:  If the result of the Call is never used, we won't
		 * find a Proj with a mode that potentially triggers MUST_BE_LOWERED
		 * to be set.  Thus, if we see a call, we check its result types and
		 * decide whether MUST_BE_LOWERED has to be set.
		 */
		const ir_type *tp = get_Call_type(node);
		for (size_t i = 0, n_res = get_method_n_ress(tp); i < n_res; ++i) {
			const ir_type *rtp = get_method_res_type(tp, i);
			if (type_needs_lowering(rtp))
				env.flags |= MUST_BE_LOWERED;
		}
	}
}

lower64_entry_t *get_node_entry(ir_node *node)
{
	unsigned idx = get_irn_idx(node);
	assert(idx < env.n_entries);
	return env.entries[idx];
}

void ir_set_dw_lowered(ir_node *old, ir_node *new_low, ir_node *new_high)
{
	lower64_entry_t *entry = get_node_entry(old);
	entry->low_word  = new_low;
	entry->high_word = new_high;
}

ir_mode *ir_get_low_unsigned_mode(void)
{
	return env.p.word_unsigned;
}

ir_mode *get_node_high_mode(ir_node *const node)
{
	return get_high_mode(get_irn_op_mode(node));
}

/**
 * Translate a Constant: create two.
 */
static void lower_Const(ir_node *const node)
{
	ir_graph  *irg      = get_irn_irg(node);
	dbg_info  *dbg      = get_irn_dbg_info(node);
	ir_mode   *low_mode = env.p.word_unsigned;
	ir_tarval *tv       = get_Const_tarval(node);
	ir_tarval *tv_l     = tarval_convert_to(tv, low_mode);
	ir_node   *res_low  = new_rd_Const(dbg, irg, tv_l);
	ir_tarval *tv_shrs  = tarval_shrs_unsigned(tv, get_mode_size_bits(low_mode));
	ir_mode   *mode     = get_node_high_mode(node);
	ir_tarval *tv_h     = tarval_convert_to(tv_shrs, mode);
	ir_node   *res_high = new_rd_Const(dbg, irg, tv_h);

	ir_set_dw_lowered(node, res_low, res_high);
}

/**
 * Translate a Load: create two.
 */
static void lower_Load(ir_node *const node)
{
	ir_mode  *low_mode = env.p.word_unsigned;
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *adr      = get_Load_ptr(node);
	ir_node  *mem      = get_Load_mem(node);
	ir_type  *type     = get_Load_type(node);
	ir_node  *cnst     = new_r_Const(irg, env.tv_mode_bytes);
	ir_node  *block    = get_nodes_block(node);
	ir_node *low;
	ir_node *high;
	if (ir_target_big_endian()) {
		low  = new_r_Add(block, adr, cnst);
		high = adr;
	} else {
		low  = adr;
		high = new_r_Add(block, adr, cnst);
	}

	/* create two loads */
	ir_cons_flags volatility
		= get_Load_volatility(node) == volatility_is_volatile ? cons_volatile
		                                                      : cons_none;
	dbg_info *dbg   = get_irn_dbg_info(node);
	low             = new_rd_Load(dbg, block, mem,    low,  low_mode, type, volatility);
	ir_node *proj_m = new_r_Proj(low, mode_M, pn_Load_M);
	ir_mode *mode   = get_node_high_mode(node);
	high            = new_rd_Load(dbg, block, proj_m, high, mode,     type, volatility);

	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		switch ((pn_Load)get_Proj_num(proj)) {
		case pn_Load_M:
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Load_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		case pn_Load_X_regular:
			set_Proj_pred(proj, high);
			break;
		case pn_Load_res: {       /* Result of load operation. */
			ir_node *res_low  = new_r_Proj(low,  low_mode, pn_Load_res);
			ir_node *res_high = new_r_Proj(high, mode,     pn_Load_res);
			ir_set_dw_lowered(proj, res_low, res_high);
			break;
		}
		}
		/* mark this proj: we have handled it already, otherwise we might fall
		 * into out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Store: create two.
 */
static void lower_Store(ir_node *node)
{
	ir_node               *value = get_Store_value(node);
	const lower64_entry_t *entry = get_node_entry(value);

	ir_graph *irg   = get_irn_irg(node);
	ir_node  *adr   = get_Store_ptr(node);
	ir_node  *mem   = get_Store_mem(node);
	ir_type  *type  = get_Store_type(node);
	ir_node  *block = get_nodes_block(node);
	ir_node  *cnst  = new_r_Const(irg, env.tv_mode_bytes);
	ir_node  *low;
	ir_node  *high;
	if (ir_target_big_endian()) {
		low  = new_r_Add(block, adr, cnst);
		high = adr;
	} else {
		low  = adr;
		high = new_r_Add(block, adr, cnst);
	}

	/* create two Stores */
	ir_cons_flags volatility
		= get_Store_volatility(node) == volatility_is_volatile ? cons_volatile
		                                                       : cons_none;
	dbg_info *dbg   = get_irn_dbg_info(node);
	low = new_rd_Store(dbg, block, mem, low,  entry->low_word, type, volatility);
	ir_node *proj_m = new_r_Proj(low, mode_M, pn_Store_M);
	high = new_rd_Store(dbg, block, proj_m, high, entry->high_word, type, volatility);

	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		switch ((pn_Store)get_Proj_num(proj)) {
		case pn_Store_M:         /* Memory result. */
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Store_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		case pn_Store_X_regular:
			set_Proj_pred(proj, high);
			break;
		}
		/* mark this proj: we have handled it already, otherwise we might fall
		 * into out new nodes. */
		mark_irn_visited(proj);
	}
}

static ir_entity *create_libgcc_entity(ir_type *const method, ir_op const *const op, ir_mode const *const imode, ir_mode const *const omode)
{
	const char *name;
	if (op == op_Mul) {
		name = "__muldi3";
	} else if (op == op_Div) {
		name = mode_is_signed(imode) ? "__divdi3" : "__udivdi3";
	} else if (op == op_Mod) {
		name = mode_is_signed(imode) ? "__moddi3" : "__umoddi3";
	} else if (op == op_Conv) {
		if (mode_is_float(imode)) {
			assert(get_mode_size_bits(omode) == 64);
			if (get_mode_size_bits(imode) == 64) {
				name = mode_is_signed(omode) ? "__fixdfdi" : "__fixunsdfdi";
			} else if (get_mode_size_bits(imode) == 32) {
				name = mode_is_signed(omode) ? "__fixsfdi" : "__fixunssfdi";
			} else {
				assert(get_mode_size_bits(imode) == 128);
				panic("can't conver long double to long long yet");
			}
		} else if (mode_is_float(omode)) {
			assert(get_mode_size_bits(imode) == 64);
			if (get_mode_size_bits(omode) == 64) {
				name = mode_is_signed(imode) ? "__floatdidf" : "__floatundidf";
			} else if (get_mode_size_bits(omode) == 32) {
				name = mode_is_signed(imode) ? "__floatdisf" : "__floatundisf";
			} else {
				assert(get_mode_size_bits(omode) == 128);
				panic("can't convert long long to long double yet");
			}
		} else {
			panic("can't lower 64bit Conv");
		}
	} else {
		panic("cannot lower unexpected 64bit operation %s", get_op_name(op));
	}

	return create_compilerlib_entity(name, method);
}

/**
 * Return a node containing the address of the intrinsic emulation function.
 *
 * @param method  the method type of the emulation function
 * @param op      the emulated ir_op
 * @param imode   the input mode of the emulated opcode
 * @param omode   the output mode of the emulated opcode
 * @param env     the lower environment
 */
static ir_node *get_intrinsic_address(ir_type *method, ir_op *op,
                                      ir_mode *imode, ir_mode *omode)
{
	op_mode_entry_t key;
	key.op    = op;
	key.imode = imode;
	key.omode = omode;
	key.ent   = NULL;

	unsigned hash = hash_ptr(op) ^ hash_ptr(imode) ^ (hash_ptr(omode) << 8);
	op_mode_entry_t *entry
		= set_insert(op_mode_entry_t, intrinsic_fkt, &key, sizeof(key), hash);
	ir_entity *ent = entry->ent;
	if (ent == NULL) {
		/* create a new one */
		ent = create_libgcc_entity(method, op, imode, omode);

		assert(ent && "Intrinsic creator must return an entity");
		entry->ent = ent;
	}
	return new_r_Address(env.irg, ent);
}

/**
 * Translate a Div.
 *
 * Create an intrinsic Call.
 */
static void lower_Div(ir_node *const node)
{
	ir_node  *left   = get_Div_left(node);
	ir_node  *right  = get_Div_right(node);
	ir_node  *block  = get_nodes_block(node);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_mode  *mode   = get_node_high_mode(node);
	ir_type  *mtp    = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	ir_mode  *opmode = get_irn_op_mode(node);
	ir_node  *addr   = get_intrinsic_address(mtp, get_irn_op(node), opmode,
	                                         opmode);

	ir_node  *in[4];
	if (ir_target_big_endian()) {
		in[0] = get_lowered_high(left);
		in[1] = get_lowered_low(left);
		in[2] = get_lowered_high(right);
		in[3] = get_lowered_low(right);
	} else {
		in[0] = get_lowered_low(left);
		in[1] = get_lowered_high(left);
		in[2] = get_lowered_low(right);
		in[3] = get_lowered_high(right);
	}
	ir_node *mem     = get_Div_mem(node);
	ir_node *call    = new_rd_Call(dbgi, block, mem, addr, 4, in, mtp);
	ir_node *resproj = new_r_Proj(call, mode_T, pn_Call_T_result);
	set_irn_pinned(call, get_irn_pinned(node));

	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		switch ((pn_Div)get_Proj_num(proj)) {
		case pn_Div_M:
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_M);
			break;
		case pn_Div_X_regular:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_regular);
			break;
		case pn_Div_X_except:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_except);
			break;
		case pn_Div_res:
			if (ir_target_big_endian()) {
				ir_node *res_low  = new_r_Proj(resproj, env.p.word_unsigned, 1);
				ir_node *res_high = new_r_Proj(resproj, mode,                0);
				ir_set_dw_lowered(proj, res_low, res_high);
			} else {
				ir_node *res_low  = new_r_Proj(resproj, env.p.word_unsigned, 0);
				ir_node *res_high = new_r_Proj(resproj, mode,                1);
				ir_set_dw_lowered(proj, res_low, res_high);
			}
			break;
		}
		/* mark this proj: we have handled it already, otherwise we might fall
		 * into out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Mod.
 *
 * Create an intrinsic Call.
 */
static void lower_Mod(ir_node *const node)
{
	ir_node  *left   = get_Mod_left(node);
	ir_node  *right  = get_Mod_right(node);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *block  = get_nodes_block(node);
	ir_mode  *mode   = get_node_high_mode(node);
	ir_type  *mtp    = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	ir_mode  *opmode = get_irn_op_mode(node);
	ir_node  *addr   = get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode);

	ir_node *in[4];
	if (ir_target_big_endian()) {
		in[0] = get_lowered_high(left);
		in[1] = get_lowered_low(left);
		in[2] = get_lowered_high(right);
		in[3] = get_lowered_low(right);
	} else {
		in[0] = get_lowered_low(left);
		in[1] = get_lowered_high(left);
		in[2] = get_lowered_low(right);
		in[3] = get_lowered_high(right);
	}
	ir_node *mem     = get_Mod_mem(node);
	ir_node *call    = new_rd_Call(dbgi, block, mem, addr, 4, in, mtp);
	ir_node *resproj = new_r_Proj(call, mode_T, pn_Call_T_result);
	set_irn_pinned(call, get_irn_pinned(node));

	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		switch ((pn_Mod)get_Proj_num(proj)) {
		case pn_Mod_M:
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_M);
			break;
		case pn_Mod_X_regular:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_regular);
			break;
		case pn_Mod_X_except:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_except);
			break;
		case pn_Mod_res:
			if (ir_target_big_endian()) {
				ir_node *res_low  = new_r_Proj(resproj, env.p.word_unsigned, 1);
				ir_node *res_high = new_r_Proj(resproj, mode,                0);
				ir_set_dw_lowered(proj, res_low, res_high);
			} else {
				ir_node *res_low  = new_r_Proj(resproj, env.p.word_unsigned, 0);
				ir_node *res_high = new_r_Proj(resproj, mode,                1);
				ir_set_dw_lowered(proj, res_low, res_high);
			}
			break;
		}
		/* mark this proj: we have handled it already, otherwise we might fall
		 * into out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a binop.
 *
 * Create an intrinsic Call.
 */
static void lower_binop(ir_node *const node)
{
	ir_node  *left  = get_binop_left(node);
	ir_node  *right = get_binop_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(block);
	ir_mode  *mode  = get_node_high_mode(node);
	ir_type  *mtp   = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	ir_node  *addr  = get_intrinsic_address(mtp, get_irn_op(node), mode, mode);

	ir_node  *in[4];
	if (ir_target_big_endian()) {
		in[0] = get_lowered_high(left);
		in[1] = get_lowered_low(left);
		in[2] = get_lowered_high(right);
		in[3] = get_lowered_low(right);
	} else {
		in[0] = get_lowered_low(left);
		in[1] = get_lowered_high(left);
		in[2] = get_lowered_low(right);
		in[3] = get_lowered_high(right);
	}
	ir_node *mem     = get_irg_no_mem(irg);
	ir_node *call    = new_rd_Call(dbgi, block, mem, addr, 4, in, mtp);
	ir_node *resproj = new_r_Proj(call, mode_T, pn_Call_T_result);
	set_irn_pinned(call, get_irn_pinned(node));

	if (ir_target_big_endian()) {
		ir_node *res_low  = new_r_Proj(resproj, env.p.word_unsigned, 1);
		ir_node *res_high = new_r_Proj(resproj, mode,                0);
		ir_set_dw_lowered(node, res_low, res_high);
	} else {
		ir_node *res_low  = new_r_Proj(resproj, env.p.word_unsigned, 0);
		ir_node *res_high = new_r_Proj(resproj, mode,                1);
		ir_set_dw_lowered(node, res_low, res_high);
	}
}

static ir_node *create_conv(ir_node *block, ir_node *node, ir_mode *dest_mode)
{
	if (get_irn_mode(node) == dest_mode)
		return node;
	return new_r_Conv(block, node, dest_mode);
}

static void move_node(ir_node *node, ir_node *to_bl)
{
	set_nodes_block(node, to_bl);
	if (get_irn_mode(node) != mode_T)
		return;
	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;
		move_node(proj, to_bl);
	}
}

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move(ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	move_node(node, to_bl);

	/* We must not move predecessors of Phi nodes, even if they are in
	 * from_bl. (because these are values from an earlier loop iteration
	 * which are not predecessors of node here) */
	if (is_Phi(node))
		return;

	/* recursion ... */
	foreach_irn_in(node, i, pred) {
		ir_mode *pred_mode = get_irn_mode(pred);
		if (get_nodes_block(pred) == from_bl)
			move(pred, from_bl, to_bl);
		if (needs_lowering(pred_mode)) {
			ir_node *pred_low  = get_lowered_low(pred);
			ir_node *pred_high = get_lowered_high(pred);
			if (get_nodes_block(pred_low) == from_bl)
				move(pred_low, from_bl, to_bl);
			if (pred_high != NULL && get_nodes_block(pred_high) == from_bl)
				move(pred_high, from_bl, to_bl);
		}
	}
}

ir_node *part_block_dw(ir_node *node)
{
	ir_graph *irg        = get_irn_irg(node);
	ir_node  *old_block  = get_nodes_block(node);
	int       n_cfgpreds = get_Block_n_cfgpreds(old_block);
	ir_node **cfgpreds   = get_Block_cfgpred_arr(old_block);
	ir_node  *new_block  = new_r_Block(irg, n_cfgpreds, cfgpreds);

	/* old_block has no predecessors anymore for now */
	set_irn_in(old_block, 0, NULL);

	/* move node and its predecessors to new_block */
	move(node, old_block, new_block);

	/* move Phi nodes and constants to new_block */
	foreach_out_edge_safe(old_block, edge) {
		ir_node *blnode = get_edge_src_irn(edge);
		ir_node *skip   = skip_Proj(skip_Proj(blnode));
		if (is_Phi(skip) || is_irn_start_block_placed(skip))
			set_nodes_block(blnode, new_block);
	}
	if (old_block == get_irg_start_block(irg))
		set_irg_start_block(irg, new_block);
	return old_block;
}

void set_dw_control_flow_changed(void)
{
	env.flags |= CF_CHANGED;
}

typedef ir_node* (*new_rd_shr_func)(dbg_info *dbgi, ir_node *block,
                                    ir_node *left, ir_node *right);

static void lower_shr_helper(ir_node *const node, new_rd_shr_func const new_rd_shrs)
{
	ir_node  *right         = get_binop_right(node);
	ir_node  *left          = get_binop_left(node);
	ir_mode  *shr_mode      = get_irn_mode(node);
	unsigned  modulo_shift  = get_mode_modulo_shift(shr_mode);
	ir_mode  *low_unsigned  = env.p.word_unsigned;
	ir_mode  *mode          = get_node_high_mode(node);
	unsigned  modulo_shift2 = get_mode_modulo_shift(mode);
	ir_graph *irg           = get_irn_irg(node);
	ir_node  *left_low      = get_lowered_low(left);
	ir_node  *left_high     = get_lowered_high(left);
	dbg_info *dbgi          = get_irn_dbg_info(node);

	/* this version is optimized for modulo shift architectures
	 * (and can't handle anything else) */
	if (modulo_shift != get_mode_size_bits(shr_mode)
	    || modulo_shift2 << 1 != modulo_shift) {
		panic("Shr lowering only implemented for modulo shift shr operations");
	}
	if (!is_po2_or_zero(modulo_shift) || !is_po2_or_zero(modulo_shift2)) {
		panic("Shr lowering only implemented for power-of-2 modes");
	}
	/* without 2-complement the -x instead of (bit_width-x) trick won't work */
	if (get_mode_arithmetic(shr_mode) != irma_twos_complement) {
		panic("Shr lowering only implemented for two-complement modes");
	}

	ir_node *block = get_nodes_block(node);

	/* if the right operand is a 64bit value, we're only interested in the
	 * lower word */
	assert(!mode_is_signed(get_irn_mode(right)));
	if (needs_lowering(get_irn_mode(right))) {
		right = get_lowered_low(right);
	} else {
		/* shift should never have signed mode on the right */
		right = create_conv(block, right, low_unsigned);
	}

	ir_node *lower_block = part_block_dw(node);
	env.flags |= CF_CHANGED;
	block = get_nodes_block(node);

	/* add a Cmp to test if highest bit is set <=> whether we shift more
	 * than half the word width */
	ir_node *cnst  = new_r_Const_long(irg, low_unsigned, modulo_shift2);
	ir_node *andn  = new_r_And(block, right, cnst);
	ir_node *cnst2 = new_r_Const_null(irg, low_unsigned);
	ir_node *cmp   = new_rd_Cmp(dbgi, block, andn, cnst2, ir_relation_equal);
	ir_node *cond       = new_rd_Cond(dbgi, block, cmp);
	ir_node *proj_true  = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node *proj_false = new_r_Proj(cond, mode_X, pn_Cond_false);

	/* the true block => shift_width < 1word */
	/* In theory the low value (for 64bit shifts) is:
	 *    Or(High << (32-x)), Low >> x)
	 * In practice High << 32-x will fail when x is zero (since we have
	 * modulo shift and 32 will be 0). So instead we use:
	 *    Or(High<<1<<~x, Low >> x)
	 */
	ir_node *true_in[1]   = { proj_true };
	ir_node *block_true   = new_r_Block(irg, ARRAY_SIZE(true_in), true_in);
	ir_node *tres_high    = new_rd_shrs(dbgi, block_true, left_high, right);
	ir_node *shift_low    = new_rd_Shr(dbgi, block_true, left_low, right);
	ir_node *not_shiftval = new_rd_Not(dbgi, block_true, right);
	ir_node *tconv        = create_conv(block_true, left_high,
	                                    low_unsigned);
	ir_node *one          = new_r_Const_one(irg, low_unsigned);
	ir_node *carry0       = new_rd_Shl(dbgi, block_true, tconv, one);
	ir_node *carry1       = new_rd_Shl(dbgi, block_true, carry0, not_shiftval);
	ir_node *tres_low     = new_rd_Or(dbgi, block_true, shift_low, carry1);

	/* false block => shift_width > 1word */
	ir_node *false_in[1] = { proj_false };
	ir_node *block_false = new_r_Block(irg, ARRAY_SIZE(false_in), false_in);
	ir_node *fconv       = create_conv(block_false, left_high, low_unsigned);
	ir_node *fres_low    = new_rd_shrs(dbgi, block_false, fconv, right);
	int      cnsti       = modulo_shift2 - 1;
	ir_node *cnst3       = new_r_Const_long(irg, low_unsigned, cnsti);
	ir_node *fres_high;
	if (new_rd_shrs == new_rd_Shrs) {
		fres_high = new_rd_shrs(dbgi, block_false, left_high, cnst3);
	} else {
		fres_high = new_r_Const_null(irg, mode);
	}

	/* patch lower block */
	ir_node *lower_in[]    = { new_r_Jmp(block_true), new_r_Jmp(block_false) };
	ir_node *phi_low_in[]  = { tres_low,  fres_low };
	ir_node *phi_high_in[] = { tres_high, fres_high };
	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi_low  = new_r_Phi(lower_block, ARRAY_SIZE(phi_low_in),
	                              phi_low_in, low_unsigned);
	ir_node *phi_high = new_r_Phi(lower_block, ARRAY_SIZE(phi_high_in),
	                              phi_high_in, mode);
	ir_set_dw_lowered(node, phi_low, phi_high);
}

static void lower_Shr(ir_node *const node)
{
	lower_shr_helper(node, new_rd_Shr);
}

static void lower_Shrs(ir_node *const node)
{
	lower_shr_helper(node, new_rd_Shrs);
}

static void lower_Shl(ir_node *const node)
{
	ir_node  *right         = get_binop_right(node);
	ir_node  *left          = get_binop_left(node);
	ir_mode  *shr_mode      = get_irn_mode(node);
	unsigned  modulo_shift  = get_mode_modulo_shift(shr_mode);
	ir_mode  *low_unsigned  = env.p.word_unsigned;
	ir_mode  *mode          = get_node_high_mode(node);
	unsigned  modulo_shift2 = get_mode_modulo_shift(mode);
	ir_graph *irg           = get_irn_irg(node);
	ir_node  *left_low      = get_lowered_low(left);
	ir_node  *left_high     = get_lowered_high(left);
	dbg_info *dbgi          = get_irn_dbg_info(node);
	ir_node  *lower_block   = get_nodes_block(node);

	/* this version is optimized for modulo shift architectures
	 * (and can't handle anything else) */
	if (modulo_shift != get_mode_size_bits(shr_mode)
	    || modulo_shift2<<1 != modulo_shift) {
		panic("Shl lowering only implemented for modulo shift shl operations");
	}
	if (!is_po2_or_zero(modulo_shift) || !is_po2_or_zero(modulo_shift2)) {
		panic("Shl lowering only implemented for power-of-2 modes");
	}
	/* without 2-complement the -x instead of (bit_width-x) trick won't work */
	if (get_mode_arithmetic(shr_mode) != irma_twos_complement) {
		panic("Shl lowering only implemented for two-complement modes");
	}

	/* if the right operand is a 64bit value, we're only interested in the
	 * lower word */
	assert(!mode_is_signed(get_irn_mode(right)));
	if (needs_lowering(get_irn_mode(right))) {
		right = get_lowered_low(right);
	} else {
		/* shift should never have signed mode on the right */
		right = create_conv(lower_block, right, low_unsigned);
	}

	part_block_dw(node);
	env.flags |= CF_CHANGED;
	ir_node *block = get_nodes_block(node);

	/* add a Cmp to test if highest bit is set <=> whether we shift more
	 * than half the word width */
	ir_node *cnst       = new_r_Const_long(irg, low_unsigned, modulo_shift2);
	ir_node *andn       = new_r_And(block, right, cnst);
	ir_node *cnst2      = new_r_Const_null(irg, low_unsigned);
	ir_node *cmp        = new_rd_Cmp(dbgi, block, andn, cnst2,
	                                 ir_relation_equal);
	ir_node *cond       = new_rd_Cond(dbgi, block, cmp);
	ir_node *proj_true  = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node *proj_false = new_r_Proj(cond, mode_X, pn_Cond_false);

	/* the true block => shift_width < 1word */
	ir_node *tin[]        = { proj_true };
	ir_node *block_true   = new_r_Block(irg, ARRAY_SIZE(tin), tin);

	ir_node *tres_low     = new_rd_Shl(dbgi, block_true, left_low, right);
	ir_node *shift_high   = new_rd_Shl(dbgi, block_true, left_high, right);
	ir_node *not_shiftval = new_rd_Not(dbgi, block_true, right);
	ir_node *conv         = create_conv(block_true, left_low, mode);
	ir_node *one          = new_r_Const_one(irg, low_unsigned);
	ir_node *carry0       = new_rd_Shr(dbgi, block_true, conv, one);
	ir_node *carry1       = new_rd_Shr(dbgi, block_true, carry0, not_shiftval);
	ir_node *tres_high    = new_rd_Or(dbgi, block_true, shift_high, carry1);

	/* false block => shift_width > 1word */
	ir_node *fin[1]      = { proj_false };
	ir_node *block_false = new_r_Block(irg, ARRAY_SIZE(fin), fin);
	ir_node *fres_low    = new_r_Const_null(irg, low_unsigned);
	ir_node *fconv       = create_conv(block_false, left_low, mode);
	ir_node *fres_high   = new_rd_Shl(dbgi, block_false, fconv, right);

	/* patch lower block */
	ir_node *lower_in[]    = { new_r_Jmp(block_true), new_r_Jmp(block_false) };
	ir_node *phi_low_in[]  = { tres_low, fres_low };
	ir_node *phi_high_in[] = { tres_high, fres_high };
	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi_low  = new_r_Phi(lower_block, ARRAY_SIZE(phi_low_in),
	                              phi_low_in, low_unsigned);
	ir_node *phi_high = new_r_Phi(lower_block, ARRAY_SIZE(phi_high_in),
	                              phi_high_in, mode);
	ir_set_dw_lowered(node, phi_low, phi_high);
}

/**
 * Translate a Minus operation.
 *
 * res_low = -low
 * res_high = -high - (new_low > 0)
 */
static void lower_Minus(ir_node *const node)
{
	dbg_info *const dbgi         = get_irn_dbg_info(node);
	ir_graph *const irg          = get_irn_irg(node);
	ir_node  *const block        = get_nodes_block(node);
	ir_node  *const op           = get_Minus_op(node);
	lower64_entry_t const *const op_entry = get_node_entry(op);

	ir_mode  *const low_unsigned = get_irn_mode(op_entry->low_word);
	ir_node  *const res_low      = new_rd_Minus(dbgi, block, op_entry->low_word);
	ir_mode  *const mode         = get_node_high_mode(node);
	ir_node  *const tmp_high     = new_rd_Minus(dbgi, block, op_entry->high_word);

	ir_node  *const zero_low     = new_r_Const_null(irg, low_unsigned);
	ir_node  *const cmp_carry    = new_rd_Cmp(dbgi, block, res_low, zero_low, ir_relation_greater);
	ir_node  *const one          = new_r_Const(irg, get_mode_one(mode));
	ir_node  *const zero_high    = new_r_Const(irg, get_mode_null(mode));
	ir_node  *const carry        = new_rd_Mux(dbgi, block, cmp_carry, zero_high, one);

	ir_node  *const res_high     = new_rd_Sub(dbgi, block, tmp_high, carry);
	ir_set_dw_lowered(node, res_low, res_high);
}

static void lower_binop_additive(ir_node *const node, ir_node *(*const cons)(dbg_info*, ir_node*, ir_node*, ir_node*), ir_relation const relation)
{
	dbg_info *const dbg   = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);

	ir_node               *const left        = get_binop_left(node);
	lower64_entry_t const *const left_entry  = get_node_entry(left);
	ir_node               *const right       = get_binop_right(node);
	lower64_entry_t const *const right_entry = get_node_entry(right);

	ir_node  *const res_low   = cons(dbg, block, left_entry->low_word, right_entry->low_word);
	ir_node  *const cmp_carry = new_rd_Cmp(dbg, block, res_low, left_entry->low_word, relation);
	ir_graph *const irg       = get_irn_irg(node);
	ir_mode  *const mode      = get_node_high_mode(node);
	ir_node  *const one       = new_r_Const(irg, get_mode_one(mode));
	ir_node  *const zero      = new_r_Const(irg, get_mode_null(mode));
	ir_node  *const carry     = new_rd_Mux(dbg, block, cmp_carry, zero, one);
	ir_node  *const sum_high  = cons(dbg, block, left_entry->high_word, right_entry->high_word);
	ir_node  *const res_high  = cons(dbg, block, sum_high, carry);
	ir_set_dw_lowered(node, res_low, res_high);
}

static void lower_Add(ir_node *const node)
{
	lower_binop_additive(node, new_rd_Add, ir_relation_less);
}

static void lower_Sub(ir_node *const node)
{
	lower_binop_additive(node, new_rd_Sub, ir_relation_greater);
}

/**
 * Translate a logical binop by creating two logical binops.
 */
static void lower_binop_logical(ir_node *const node, ir_node *(*const constr_rd)(dbg_info *db, ir_node *block, ir_node *op1, ir_node *op2))
{
	ir_node               *left        = get_binop_left(node);
	ir_node               *right       = get_binop_right(node);
	const lower64_entry_t *left_entry  = get_node_entry(left);
	const lower64_entry_t *right_entry = get_node_entry(right);
	dbg_info              *dbgi        = get_irn_dbg_info(node);
	ir_node               *block       = get_nodes_block(node);
	ir_node               *res_low
		= constr_rd(dbgi, block, left_entry->low_word, right_entry->low_word);
	ir_node               *res_high
		= constr_rd(dbgi, block, left_entry->high_word, right_entry->high_word);
	ir_set_dw_lowered(node, res_low, res_high);
}

static void lower_And(ir_node *const node)
{
	lower_binop_logical(node, new_rd_And);
}

static void lower_Or(ir_node *const node)
{
	lower_binop_logical(node, new_rd_Or);
}

static void lower_Eor(ir_node *const node)
{
	lower_binop_logical(node, new_rd_Eor);
}

/**
 * Translate a Not.
 *
 * Create two logical Nots.
 */
static void lower_Not(ir_node *const node)
{
	ir_node               *op       = get_Not_op(node);
	const lower64_entry_t *op_entry = get_node_entry(op);
	dbg_info              *dbgi     = get_irn_dbg_info(node);
	ir_node               *block    = get_nodes_block(node);
	ir_node *res_low  = new_rd_Not(dbgi, block, op_entry->low_word);
	ir_node *res_high = new_rd_Not(dbgi, block, op_entry->high_word);
	ir_set_dw_lowered(node, res_low, res_high);
}

static void lower_Proj(ir_node *const node)
{
	ir_mode *mode = get_irn_mode(node);
	if (!needs_lowering(mode))
		return;
	/* skip tuples */
	ir_node *pred = get_Proj_pred(node);
	if (is_Tuple(pred)) {
		unsigned               pn    = get_Proj_num(node);
		ir_node               *op    = get_irn_n(pred, pn);
		const lower64_entry_t *entry = get_node_entry(op);
		ir_set_dw_lowered(node, entry->low_word, entry->high_word);
	}
}

static bool is_equality_cmp(const ir_node *node)
{
	ir_relation relation = get_Cmp_relation(node);
	ir_node    *right    = get_Cmp_right(node);
	ir_mode    *mode     = get_irn_mode(right);

	/* this probably makes no sense if unordered is involved */
	assert(!mode_is_float(mode));

	if (relation == ir_relation_equal || relation == ir_relation_less_greater)
		return true;

	/* Unsigned x > 0 behaves like !=. */
	if (is_irn_null(right) && !mode_is_signed(mode))
		return relation == ir_relation_greater;

	return false;
}

static ir_node *get_cfop_destination(const ir_node *cfop)
{
	const ir_edge_t *first = get_irn_out_edge_first(cfop);
	/* we should only have 1 destination */
	assert(get_irn_n_edges(cfop) == 1);
	return get_edge_src_irn(first);
}

static void lower_Switch(ir_node *const node)
{
	ir_node *selector = get_Switch_selector(node);
	ir_mode *mode     = get_irn_mode(selector);
	if (needs_lowering(mode)) {
		/* we can't really handle Switch with 64bit offsets */
		panic("Switch with 64bit jumptable not supported");
	}
	lower_node(selector);
}

/**
 * Translate a Cond.
 */
static void lower_Cond(ir_node *const node)
{
	ir_node *sel = get_Cond_selector(node);
	if (!is_Cmp(sel)) {
		lower_node(sel);
		return;
	}

	ir_node *left     = get_Cmp_left(sel);
	ir_mode *cmp_mode = get_irn_mode(left);
	if (!needs_lowering(cmp_mode)) {
		lower_node(sel);
		return;
	}

	ir_node *right  = get_Cmp_right(sel);
	lower_node(left);
	lower_node(right);
	const lower64_entry_t *lentry = get_node_entry(left);
	const lower64_entry_t *rentry = get_node_entry(right);

	/* all right, build the code */
	ir_node *projT = NULL;
	ir_node *projF = NULL;
	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;
		unsigned proj_nr = get_Proj_num(proj);

		if (proj_nr == pn_Cond_true) {
			assert(projT == NULL && "more than one Proj(true)");
			projT = proj;
		} else {
			assert(proj_nr == pn_Cond_false);
			assert(projF == NULL && "more than one Proj(false)");
			projF = proj;
		}
		mark_irn_visited(proj);
	}
	assert(projT && projF);

	/* create a new high compare */
	ir_node    *block    = get_nodes_block(node);
	ir_graph   *irg      = get_irn_irg(node);
	dbg_info   *dbg      = get_irn_dbg_info(sel);
	ir_relation relation = get_Cmp_relation(sel);

	if (is_equality_cmp(sel)) {
		/* x ==/!= y ==> or(x_low^y_low,x_high^y_high) ==/!= 0 */
		ir_mode *mode       = env.p.word_unsigned;
		ir_node *low_left   = new_rd_Conv(dbg, block, lentry->low_word, mode);
		ir_node *high_left  = new_rd_Conv(dbg, block, lentry->high_word, mode);
		ir_node *low_right  = new_rd_Conv(dbg, block, rentry->low_word, mode);
		ir_node *high_right = new_rd_Conv(dbg, block, rentry->high_word, mode);
		ir_node *xor_low    = new_rd_Eor(dbg, block, low_left, low_right);
		ir_node *xor_high   = new_rd_Eor(dbg, block, high_left, high_right);
		ir_node *ornode     = new_rd_Or(dbg, block, xor_low, xor_high);
		ir_node *null       = new_r_Const_null(irg, mode);
		ir_node *cmp        = new_rd_Cmp(dbg, block, ornode, null, relation);
		set_Cond_selector(node, cmp);
		return;
	}

	assert(relation != ir_relation_equal);
	assert(relation != ir_relation_less_greater);

	/* a rel b <==> a_h REL b_h || (a_h == b_h && a_l rel b_l) */
	ir_node *dstT, *dstF, *newbl_eq, *newbl_l;
	ir_node *projEqF;

	dstT = get_cfop_destination(projT);
	dstF = get_cfop_destination(projF);

	ir_node *irn = new_rd_Cmp(dbg, block, lentry->high_word,
	                          rentry->high_word,
	                          relation & ~ir_relation_equal);
	dbg = get_irn_dbg_info(node);
	irn = new_rd_Cond(dbg, block, irn);

	ir_node *projHT = new_r_Proj(irn, mode_X, pn_Cond_true);
	mark_irn_visited(projHT);

	ir_node *projHF = new_r_Proj(irn, mode_X, pn_Cond_false);
	mark_irn_visited(projHF);

	newbl_eq = new_r_Block(irg, 1, &projHF);

	irn = new_rd_Cmp(dbg, block, lentry->high_word, rentry->high_word,
	                 ir_relation_equal);
	irn = new_rd_Cond(dbg, newbl_eq, irn);

	projEqF = new_r_Proj(irn, mode_X, pn_Cond_false);
	mark_irn_visited(projEqF);

	ir_node *proj = new_r_Proj(irn, mode_X, pn_Cond_true);
	mark_irn_visited(proj);

	newbl_l = new_r_Block(irg, 1, &proj);

	dbg = get_irn_dbg_info(sel);
	irn = new_rd_Cmp(dbg, newbl_l, lentry->low_word, rentry->low_word,
	                 relation);
	dbg = get_irn_dbg_info(node);
	irn = new_rd_Cond(dbg, newbl_l, irn);

	proj = new_r_Proj(irn, mode_X, pn_Cond_true);
	mark_irn_visited(proj);
	add_block_cf_input(dstT, projT, proj);

	proj = new_r_Proj(irn, mode_X, pn_Cond_false);
	mark_irn_visited(proj);
	add_block_cf_input(dstF, projF, proj);

	exchange(projT, projHT);
	exchange(projF, projEqF);

	/* we have changed the control flow */
	env.flags |= CF_CHANGED;
}

/**
 * Translate a Conv to higher_signed
 */
static void lower_Conv_to_Ll(ir_node *node)
{
	ir_mode  *omode        = get_irn_mode(node);
	ir_node  *op           = get_Conv_op(node);
	ir_mode  *imode        = get_irn_mode(op);
	ir_graph *irg          = get_irn_irg(node);
	ir_node  *block        = get_nodes_block(node);
	dbg_info *dbg          = get_irn_dbg_info(node);
	ir_node  *res_low;
	ir_node  *res_high;

	ir_mode  *low_unsigned = env.p.word_unsigned;
	ir_mode  *low_signed   = get_high_mode(omode);

	if (get_mode_arithmetic(imode) == irma_twos_complement) {
		if (needs_lowering(imode)) {
			/* a Conv from Lu to Ls or Ls to Lu */
			const lower64_entry_t *op_entry = get_node_entry(op);
			res_low  = op_entry->low_word;
			res_high = new_rd_Conv(dbg, block, op_entry->high_word, low_signed);
		} else {
			/* simple case: create a high word */
			if (imode != low_unsigned)
				op = new_rd_Conv(dbg, block, op, low_unsigned);

			res_low = op;

			if (mode_is_signed(imode)) {
				int      c       = get_mode_size_bits(low_signed) - 1;
				ir_node *cnst    = new_r_Const_long(irg, low_unsigned, c);
				if (get_irn_mode(op) != low_signed)
					op = new_rd_Conv(dbg, block, op, low_signed);
				res_high = new_rd_Shrs(dbg, block, op, cnst);
			} else {
				res_high = new_r_Const_null(irg, low_signed);
			}
		}
	} else {
		ir_node *irn, *call;
		ir_type *mtp = get_conv_type(imode, omode);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode);
		call = new_rd_Call(dbg, block, get_irg_no_mem(irg), irn, 1, &op, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(call, mode_T, pn_Call_T_result);

		if (ir_target_big_endian()) {
			res_low  = new_r_Proj(irn, low_unsigned, 1);
			res_high = new_r_Proj(irn, low_signed,   0);
		} else {
			res_low  = new_r_Proj(irn, low_unsigned, 0);
			res_high = new_r_Proj(irn, low_signed, 1);
		}
	}
	ir_set_dw_lowered(node, res_low, res_high);
}

/**
 * Translate a Conv from higher_unsigned
 */
static void lower_Conv_from_Ll(ir_node *node)
{
	ir_node               *op    = get_Conv_op(node);
	ir_mode               *omode = get_irn_mode(node);
	ir_node               *block = get_nodes_block(node);
	dbg_info              *dbg   = get_irn_dbg_info(node);
	ir_graph              *irg   = get_irn_irg(node);
	const lower64_entry_t *entry = get_node_entry(op);

	if (get_mode_arithmetic(omode) == irma_twos_complement) {
		/* simple case: create a high word */
		set_Conv_op(node, entry->low_word);
	} else {
		ir_node *in[2];
		if (ir_target_big_endian()) {
			in[0] = entry->high_word;
			in[1] = entry->low_word;
		} else {
			in[0] = entry->low_word;
			in[1] = entry->high_word;
		}

		ir_node *nomem    = get_irg_no_mem(irg);
		ir_mode *imode    = get_irn_mode(op);
		ir_type *mtp      = get_conv_type(imode, omode);
		ir_node *adr
			= get_intrinsic_address(mtp, get_irn_op(node), imode, omode);
		ir_node *call     = new_rd_Call(dbg, block, nomem, adr, 2, in, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		ir_node *res_proj = new_r_Proj(call, mode_T, pn_Call_T_result);
		ir_node *res      = new_r_Proj(res_proj, omode, 0);

		exchange(node, res);
	}
}

/**
 * lower Cmp
 */
static void lower_Cmp(ir_node *const cmp)
{
	ir_node  *l        = get_Cmp_left(cmp);
	ir_mode  *cmp_mode = get_irn_mode(l);
	if (!needs_lowering(cmp_mode))
		return;

	ir_node               *r        = get_Cmp_right(cmp);
	const lower64_entry_t *lentry   = get_node_entry(l);
	const lower64_entry_t *rentry   = get_node_entry(r);
	ir_relation            relation = get_Cmp_relation(cmp);
	ir_node               *block    = get_nodes_block(cmp);
	dbg_info              *dbg      = get_irn_dbg_info(cmp);

	/* easy case for x ==/!= 0 (see lower_Cond for details) */
	if (is_equality_cmp(cmp)) {
		ir_graph *irg        = get_irn_irg(cmp);
		ir_mode  *mode       = env.p.word_unsigned;
		ir_node  *low_left   = new_rd_Conv(dbg, block, lentry->low_word, mode);
		ir_node  *high_left  = new_rd_Conv(dbg, block, lentry->high_word, mode);
		ir_node  *low_right  = new_rd_Conv(dbg, block, rentry->low_word, mode);
		ir_node  *high_right = new_rd_Conv(dbg, block, rentry->high_word, mode);
		ir_node  *xor_low    = new_rd_Eor(dbg, block, low_left, low_right);
		ir_node  *xor_high   = new_rd_Eor(dbg, block, high_left, high_right);
		ir_node  *ornode     = new_rd_Or(dbg, block, xor_low, xor_high);
		ir_node  *null       = new_r_Const_null(irg, mode);
		ir_node  *new_cmp    = new_rd_Cmp(dbg, block, ornode, null, relation);
		exchange(cmp, new_cmp);
		return;
	}

	assert(relation != ir_relation_equal);
	assert(relation != ir_relation_less_greater);

	/* a rel b <==> a_h REL b_h || (a_h == b_h && a_l rel b_l) */
	ir_node *high1 = new_rd_Cmp(dbg, block, lentry->high_word,
	                            rentry->high_word,
	                            relation & ~ir_relation_equal);
	ir_node *low  = new_rd_Cmp(dbg, block, lentry->low_word,
	                           rentry->low_word, relation);
	ir_node *high = new_rd_Cmp(dbg, block, lentry->high_word,
	                           rentry->high_word, ir_relation_equal);
	ir_node *t    = new_rd_And(dbg, block, low, high);
	ir_node *res  = new_rd_Or(dbg, block, high1, t);
	exchange(cmp, res);
}

void ir_default_lower_dw_Conv(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	if (needs_lowering(mode)) {
		lower_Conv_to_Ll(node);
	} else {
		ir_mode *op_mode = get_irn_mode(get_Conv_op(node));

		if (needs_lowering(op_mode)) {
			lower_Conv_from_Ll(node);
		}
	}
}

static void lower_Bitcast(ir_node *const node)
{
	ir_mode *dst_mode = get_irn_mode(node);
	ir_node *op       = get_Bitcast_op(node);
	ir_mode *src_mode = get_irn_mode(op);
	ir_mode *other_mode;
	if (needs_lowering(dst_mode)) {
		other_mode = src_mode;
		goto transform;
	} else if (needs_lowering(src_mode)) {
		other_mode = dst_mode;
		goto transform;
	}
	return;

transform:
	assert(get_mode_size_bits(src_mode) == get_mode_size_bits(dst_mode));

	/** The bitcast is implemented with 2 integer stores+1 double load or
	 * 1 double store+2 integer loads */
	ir_graph  *irg         = get_irn_irg(node);
	ir_type   *frame_type  = get_irg_frame_type(irg);
	ident     *id          = id_unique("bitcast");
	ir_entity *entity      = new_entity(frame_type, id,
	                                    get_type_for_mode(other_mode));
	ir_node   *block       = get_nodes_block(node);
	ir_node   *frame_ptr   = get_irg_frame(irg);
	ir_node   *addr        = new_r_Member(block, frame_ptr, entity);
	dbg_info  *dbgi        = get_irn_dbg_info(node);
	ir_node   *nomem       = get_irg_no_mem(irg);
	ir_node   *cnst        = new_r_Const(irg, env.tv_mode_bytes);
	ir_node   *low         = addr;
	ir_node   *high        = new_r_Add(block, addr, cnst);
	ir_type   *src_type    = get_type_for_mode(src_mode);
	/* big endian requires different order for lower/higher word */
	if (ir_target_big_endian()) {
		ir_node *tmp = low;
		low  = high;
		high = tmp;
	}

	if (needs_lowering(src_mode)) {
		const lower64_entry_t *entry = get_node_entry(op);

		ir_node  *store0 = new_rd_Store(dbgi, block, nomem, high,
		                                entry->high_word, src_type, cons_floats);
		ir_node  *mem0   = new_r_Proj(store0, mode_M, pn_Store_M);
		ir_node  *store1 = new_rd_Store(dbgi, block, nomem, low,
		                                entry->low_word, src_type, cons_floats);
		ir_node  *mem1   = new_r_Proj(store1, mode_M, pn_Store_M);
		ir_node  *in[]   = { mem0, mem1 };
		ir_node  *sync   = new_r_Sync(block, ARRAY_SIZE(in), in);

		ir_node  *load   = new_rd_Load(dbgi, block, sync,  addr,
		                               dst_mode, src_type, cons_floats);
		ir_node  *res    = new_r_Proj(load, dst_mode, pn_Load_res);
		exchange(node, res);
	} else {
		assert(needs_lowering(dst_mode));
		ir_node *store     = new_rd_Store(dbgi, block, nomem, addr, op,
		                                  src_type, cons_floats);
		ir_node *mem       = new_r_Proj(store, mode_M, pn_Store_M);
		ir_node *load_low  = new_rd_Load(dbgi, block, mem, low,
		                                 env.p.word_unsigned, src_type, cons_floats);
		ir_node *res_low   = new_r_Proj(load_low, env.p.word_unsigned,
		                                pn_Load_res);
		ir_mode *mode      = get_node_high_mode(node);
		ir_node *load_high = new_rd_Load(dbgi, block, mem, high,
		                                 mode, src_type, cons_floats);
		ir_node *res_high  = new_r_Proj(load_high, mode, pn_Load_res);
		ir_set_dw_lowered(node, res_low, res_high);
	}
}

static void fix_parameter_entities(ir_graph *irg, ir_type *orig_mtp)
{
	size_t      orig_n_params      = get_method_n_params(orig_mtp);
	ir_entity **parameter_entities;

	parameter_entities = ALLOCANZ(ir_entity*, orig_n_params);

	ir_type *frame_type = get_irg_frame_type(irg);
	size_t   n          = get_compound_n_members(frame_type);
	size_t   i;
	size_t   n_param;

	/* collect parameter entities */
	for (i = 0; i < n; ++i) {
		ir_entity *entity = get_compound_member(frame_type, i);
		size_t     p;
		if (!is_parameter_entity(entity))
			continue;
		p = get_entity_parameter_number(entity);
		if (p == IR_VA_START_PARAMETER_NUMBER)
			continue;
		assert(p < orig_n_params);
		assert(parameter_entities[p] == NULL);
		parameter_entities[p] = entity;
	}

	/* adjust indices */
	n_param = 0;
	for (i = 0; i < orig_n_params; ++i, ++n_param) {
		ir_entity *entity = parameter_entities[i];
		ir_type   *tp;

		if (entity != NULL)
			set_entity_parameter_number(entity, n_param);

		tp = get_method_param_type(orig_mtp, i);
		if (type_needs_lowering(tp)) {
			++n_param;
			/* Note that we cannot split the parameter entity up, because
			 * we have to guarantee that we have a continuous 64bit entity.
			 * As an example imaging an arm function with the first half
			 * of the doubleword ending up in a register, the 2nd half
			 * on the stack. If we would have split the entities they would
			 * not end up continuously. */
			if (entity != NULL) {
				assert(!entity->attr.parameter.is_lowered_doubleword);
				entity->attr.parameter.is_lowered_doubleword = true;
			}
		}
	}
}

static size_t lower_type(void (*const set)(ir_type*, size_t, ir_type*), ir_type *const mtp, size_t n, ir_type *const tp)
{
	if (is_Primitive_type(tp)) {
		ir_mode *const mode = get_type_mode(tp);
		if (needs_lowering(mode)) {
			if (!mode_is_signed(mode)) {
				set(mtp, n++, tp_l_u);
				set(mtp, n++, tp_u);
			} else if (ir_target_big_endian()) {
				set(mtp, n++, tp_l_s);
				set(mtp, n++, tp_u);
			} else {
				set(mtp, n++, tp_l_u);
				set(mtp, n++, tp_s);
			}
			return n;
		}
	}
	set(mtp, n++, tp);
	return n;
}

/**
 * Lower the method type.
 * @param env  the lower environment
 * @param mtp  the method type to lower
 * @return the lowered type
 */
static ir_type *lower_mtp(ir_type *mtp)
{
	ir_type *res = pmap_get(ir_type, lowered_type, mtp);
	if (res != NULL)
		return res;
	if (type_visited(mtp))
		return mtp;
	mark_type_visited(mtp);

	size_t const orig_n_params   = get_method_n_params(mtp);
	size_t const orig_n_res      = get_method_n_ress(mtp);
	size_t       n_param         = orig_n_params;
	size_t       n_res           = orig_n_res;
	bool         must_be_lowered = false;

	/* count new number of params */
	for (size_t i = orig_n_params; i-- > 0;) {
		ir_type *tp = get_method_param_type(mtp, i);
		if (type_needs_lowering(tp)) {
			++n_param;
			must_be_lowered = true;
		}
	}

	/* count new number of results */
	for (size_t i = orig_n_res; i-- > 0;) {
		ir_type *tp = get_method_res_type(mtp, i);
		if (type_needs_lowering(tp)) {
			++n_res;
			must_be_lowered = true;
		}
	}
	if (!must_be_lowered) {
		set_type_link(mtp, NULL);
		return mtp;
	}

	bool                      const is_variadic = is_method_variadic(mtp);
	unsigned                  const cc_mask     = get_method_calling_convention(mtp);
	mtp_additional_properties const props       = get_method_additional_properties(mtp);
	res = new_type_method(n_param, n_res, is_variadic, cc_mask, props);
	set_type_dbg_info(res, get_type_dbg_info(mtp));

	/* set param types and result types */
	n_param = 0;
	for (size_t i = 0; i < orig_n_params; ++i) {
		ir_type *tp = get_method_param_type(mtp, i);
		n_param = lower_type(&set_method_param_type, res, n_param, tp);
	}
	n_res = 0;
	for (size_t i = 0; i < orig_n_res; ++i) {
		ir_type *tp = get_method_res_type(mtp, i);
		n_res = lower_type(&set_method_res_type, res, n_res, tp);
	}

	set_type_link(res, mtp);

	mark_type_visited(res);
	pmap_insert(lowered_type, mtp, res);
	return res;
}

/**
 * Translate a Return.
 */
static void lower_Return(ir_node *const node)
{
	/* check if this return must be lowered */
	bool need_conv = false;
	for (int i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred  = get_Return_res(node, i);
		ir_mode *rmode = get_irn_op_mode(pred);

		if (needs_lowering(rmode))
			need_conv = true;
	}
	if (!need_conv)
		return;

	ir_graph  *irg = get_irn_irg(node);
	ir_entity *ent = get_irg_entity(irg);
	ir_type   *mtp = get_entity_type(ent);

	/* create a new in array */
	ir_node **in = ALLOCAN(ir_node*, get_method_n_ress(mtp)+1);
	int j = 0;
	in[j++] = get_Return_mem(node);

	for (int i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred      = get_Return_res(node, i);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (needs_lowering(pred_mode)) {
			const lower64_entry_t *entry = get_node_entry(pred);
			if (ir_target_big_endian()) {
				in[j++] = entry->high_word;
				in[j++] = entry->low_word;
			} else {
				in[j++] = entry->low_word;
				in[j++] = entry->high_word;
			}
		} else {
			in[j++] = pred;
		}
	}
	assert((size_t)j == get_method_n_ress(mtp)+1);

	set_irn_in(node, j, in);
}

/**
 * Translate the parameters.
 */
static void lower_Start(ir_node *const node)
{
	/* if type link is NULL then the type was not lowered, hence no changes
	 * at Start necessary */
	ir_graph  *irg      = get_irn_irg(node);
	ir_entity *ent      = get_irg_entity(irg);
	ir_type   *mtp      = get_entity_type(ent);
	ir_type   *orig_mtp = (ir_type*)get_type_link(mtp);
	if (orig_mtp == NULL)
		return;


	/* Calculate mapping of proj numbers in new_projs */
	size_t    n_params  = get_method_n_params(orig_mtp);
	unsigned *new_projs = ALLOCAN(unsigned, n_params);
	for (size_t i = 0, j = 0; i < n_params; ++i, ++j) {
		ir_type *ptp = get_method_param_type(orig_mtp, i);

		new_projs[i] = j;
		if (type_needs_lowering(ptp))
			++j;
	}

	/* find args Proj */
	ir_node *const args = get_Proj_for_pn(node, pn_Start_T_args);
	if (args == NULL)
		return;

	/* fix all Proj's and create new ones */
	foreach_out_edge_safe(args, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		unsigned proj_nr = get_Proj_num(proj);
		ir_mode *mode    = get_irn_mode(proj);
		if (!needs_lowering(mode)) {
			unsigned new_pn = new_projs[proj_nr];
			set_Proj_num(proj, new_pn);
			continue;
		}

		ir_mode *const mode_h = get_high_mode(mode);

		/* Switch off CSE or we might get an already existing Proj. */
		int old_cse = get_opt_cse();
		set_opt_cse(0);
		dbg_info *dbg    = get_irn_dbg_info(proj);
		ir_mode  *mode_l = env.p.word_unsigned;
		ir_node  *pred   = get_Proj_pred(proj);
		ir_node  *res_low;
		ir_node  *res_high;
		if (ir_target_big_endian()) {
			res_high = new_rd_Proj(dbg, pred, mode_h, new_projs[proj_nr]);
			res_low  = new_rd_Proj(dbg, pred, mode_l, new_projs[proj_nr] + 1);
		} else {
			res_low  = new_rd_Proj(dbg, pred, mode_l, new_projs[proj_nr]);
			res_high = new_rd_Proj(dbg, pred, mode_h, new_projs[proj_nr] + 1);
		}
		set_opt_cse(old_cse);
		ir_set_dw_lowered(proj, res_low, res_high);
	}
}

/**
 * Translate a Call.
 */
static void lower_Call(ir_node *const node)
{
	ir_type *tp         = get_Call_type(node);
	bool     need_lower = false;
	size_t   n_params   = get_method_n_params(tp);
	for (size_t p = 0; p < n_params; ++p) {
		ir_type *ptp = get_method_param_type(tp, p);

		if (type_needs_lowering(ptp)) {
			need_lower = true;
			break;
		}
	}
	size_t    n_res       = get_method_n_ress(tp);
	unsigned *res_numbers = ALLOCAN(unsigned, n_res);
	if (n_res > 0) {
		for (size_t i = 0, j = 0; i < n_res; ++i, ++j) {
			ir_type *ptp = get_method_res_type(tp, i);

			res_numbers[i] = j;
			if (type_needs_lowering(ptp)) {
				need_lower = true;
				++j;
			}
		}
	}

	if (!need_lower)
		return;

	/* let's lower it */
	tp = lower_mtp(tp);
	set_Call_type(node, tp);

	size_t    j  = 2;
	ir_node **in = ALLOCAN(ir_node*, get_method_n_params(tp)+2);
	in[0] = get_Call_mem(node);
	in[1] = get_Call_ptr(node);
	for (size_t i = 0; i < n_params; ++i) {
		ir_node *pred      = get_Call_param(node, i);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (needs_lowering(pred_mode)) {
			const lower64_entry_t *pred_entry = get_node_entry(pred);
			if (ir_target_big_endian()) {
				in[j++] = pred_entry->high_word;
				in[j++] = pred_entry->low_word;
			} else {
				in[j++] = pred_entry->low_word;
				in[j++] = pred_entry->high_word;
			}
		} else {
			in[j++] = pred;
		}
	}
	set_irn_in(node, j, in);

	/* find results T */
	ir_node *resproj = get_Proj_for_pn(node, pn_Call_T_result);
	if (resproj == NULL)
		return;

	/* fix the results */
	foreach_out_edge_safe(resproj, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		unsigned proj_nr   = get_Proj_num(proj);
		ir_mode *proj_mode = get_irn_mode(proj);
		if (!needs_lowering(proj_mode)) {
			unsigned new_nr = res_numbers[proj_nr];
			set_Proj_num(proj, new_nr);
			continue;
		}
		ir_mode  *mode_h = get_high_mode(proj_mode);
		dbg_info *dbg    = get_irn_dbg_info(proj);
		ir_mode  *mode_l = env.p.word_unsigned;
		ir_node  *pred   = get_Proj_pred(proj);
		ir_node  *res_low;
		ir_node  *res_high;
		if (ir_target_big_endian()) {
			res_high = new_rd_Proj(dbg, pred, mode_h, res_numbers[proj_nr]);
			res_low  = new_rd_Proj(dbg, pred, mode_l, res_numbers[proj_nr] + 1);
		} else {
			res_low  = new_rd_Proj(dbg, pred, mode_l, res_numbers[proj_nr]);
			res_high = new_rd_Proj(dbg, pred, mode_h, res_numbers[proj_nr] + 1);
		}
		ir_set_dw_lowered(proj, res_low, res_high);
	}
}

/**
 * Translate an Unknown into two.
 */
static void lower_Unknown(ir_node *const node)
{
	ir_mode  *low_mode = env.p.word_unsigned;
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *res_low  = new_r_Unknown(irg, low_mode);
	ir_mode  *mode     = get_node_high_mode(node);
	ir_node  *res_high = new_r_Unknown(irg, mode);
	ir_set_dw_lowered(node, res_low, res_high);
}

/**
 * Translate a Bad into two.
 */
static void lower_Bad(ir_node *const node)
{
	ir_mode  *low_mode = env.p.word_unsigned;
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *res_low  = new_r_Bad(irg, low_mode);
	ir_mode  *mode     = get_node_high_mode(node);
	ir_node  *res_high = new_r_Bad(irg, mode);
	ir_set_dw_lowered(node, res_low, res_high);
}

/**
 * Translate a Phi.
 *
 * First step: just create two templates
 */
static void lower_Phi(ir_node *phi)
{
	/* enqueue predecessors */
	int arity = get_Phi_n_preds(phi);
	for (int i = 0; i < arity; ++i) {
		ir_node *pred = get_Phi_pred(phi, i);
		deq_push_pointer_right(&env.waitq, pred);
	}

	ir_mode *mode = get_irn_mode(phi);
	if (!needs_lowering(mode))
		return;

	/* first create a new in array */
	ir_node **in_l   = ALLOCAN(ir_node*, arity);
	ir_node **in_h   = ALLOCAN(ir_node*, arity);
	ir_graph *irg    = get_irn_irg(phi);
	ir_mode  *mode_l = env.p.word_unsigned;
	ir_mode  *mode_h = get_high_mode(mode);
	ir_node  *unk_l  = new_r_Dummy(irg, mode_l);
	ir_node  *unk_h  = new_r_Dummy(irg, mode_h);
	for (int i = 0; i < arity; ++i) {
		in_l[i] = unk_l;
		in_h[i] = unk_h;
	}

	dbg_info *dbg   = get_irn_dbg_info(phi);
	ir_node  *block = get_nodes_block(phi);
	ir_node  *phi_l = new_rd_Phi(dbg, block, arity, in_l, mode_l);
	ir_node  *phi_h = new_rd_Phi(dbg, block, arity, in_h, mode_h);

	ir_set_dw_lowered(phi, phi_l, phi_h);

	/* remember that we need to fixup the predecessors later */
	ARR_APP1(ir_node*, env.lowered_phis, phi);
}

static void fixup_phi(ir_node *phi)
{
	const lower64_entry_t *entry = get_node_entry(phi);
	ir_node               *phi_l = entry->low_word;
	ir_node               *phi_h = entry->high_word;

	/* exchange phi predecessors which are lowered by now */
	for (int i = 0, n_preds = get_Phi_n_preds(phi); i < n_preds; ++i) {
		ir_node               *pred       = get_Phi_pred(phi, i);
		const lower64_entry_t *pred_entry = get_node_entry(pred);

		set_Phi_pred(phi_l, i, pred_entry->low_word);
		set_Phi_pred(phi_h, i, pred_entry->high_word);
	}
}

/**
 * Translate a Mux.
 */
static void lower_Mux(ir_node *const mux)
{
	ir_node               *truen       = get_Mux_true(mux);
	ir_node               *falsen      = get_Mux_false(mux);
	ir_node               *sel         = get_Mux_sel(mux);
	const lower64_entry_t *true_entry  = get_node_entry(truen);
	const lower64_entry_t *false_entry = get_node_entry(falsen);
	ir_node               *true_l      = true_entry->low_word;
	ir_node               *true_h      = true_entry->high_word;
	ir_node               *false_l     = false_entry->low_word;
	ir_node               *false_h     = false_entry->high_word;
	dbg_info              *dbgi        = get_irn_dbg_info(mux);
	ir_node               *block       = get_nodes_block(mux);
	ir_node               *res_low     = new_rd_Mux(dbgi, block, sel, false_l, true_l);
	ir_node               *res_high    = new_rd_Mux(dbgi, block, sel, false_h, true_h);
	ir_set_dw_lowered(mux, res_low, res_high);
}

/**
 * Translate an ASM node.
 */
static void lower_ASM(ir_node *const asmn)
{
	ir_asm_constraint const *const constraints = get_ASM_constraints(asmn);
	for (size_t i = 0, n = get_ASM_n_constraints(asmn); i < n; ++i) {
		if (needs_lowering(constraints[i].mode))
			panic("lowering ASM unimplemented");
	}
}

typedef ir_type *get_type(ir_type const*, size_t);
typedef void     set_type(ir_type*, size_t, ir_type*);

static void lower_types_builtin(size_t const n, ir_type *const res, ir_type *const mtp, ir_type *const tp_s, ir_type *const tp_u, get_type *const get, set_type *const set)
{
	for (size_t i = 0; i < n; ++i) {
		ir_type *tp = get(mtp, i);
		if (is_Primitive_type(tp)) {
			ir_mode *const mode = get_type_mode(tp);
			if (needs_lowering(mode)) {
				if (mode_is_signed(mode)) {
					tp = tp_s;
				} else {
					tp = tp_u;
				}
			}
		}
		set(res, i, tp);
	}
}

static ir_type *lower_Builtin_type(pmap *const type_map, ir_type *const mtp, ir_type *const tp_s_l, ir_type *const tp_u)
{
	ir_type *res = pmap_get(ir_type, type_map, mtp);
	if (res)
		return res;

	/* check for double word parameter */
	size_t const n_params        = get_method_n_params(mtp);
	size_t const n_results       = get_method_n_ress(mtp);
	bool         must_be_lowered = false;
	for (size_t i = n_params; i-- > 0;) {
		ir_type *const tp = get_method_param_type(mtp, i);
		if (type_needs_lowering(tp)) {
			must_be_lowered = true;
			break;
		}
	}

	if (!must_be_lowered) {
		set_type_link(mtp, NULL);
		return mtp;
	}

	unsigned                  const cc_mask = get_method_calling_convention(mtp);
	mtp_additional_properties const props   = get_method_additional_properties(mtp);
	res = new_type_method(n_params, n_results, false, cc_mask, props);
	set_type_dbg_info(res, get_type_dbg_info(mtp));

	/* set param types and result types */
	lower_types_builtin(n_params,  res, mtp, tp_s_l, tp_u, &get_method_param_type, &set_method_param_type);
	lower_types_builtin(n_results, res, mtp, tp_s_l, tp_u, &get_method_res_type,   &set_method_res_type);

	pmap_insert(type_map, mtp, res);
	return res;
}

/**
 * Lower the builtin type to its higher part.
 * @param mtp  the builtin type to lower
 * @return the lowered type
 */
static ir_type *lower_Builtin_type_high(ir_type *mtp)
{
	ir_type *const tp_s_l = ir_target_big_endian() ? tp_s : tp_u;
	return lower_Builtin_type(lowered_builtin_type_high, mtp, tp_s_l, tp_u);
}

/**
 * Lower the builtin type to its lower part.
 * @param mtp  the builtin type to lower
 * @return the lowered type
 */
static ir_type *lower_Builtin_type_low(ir_type *mtp)
{
	ir_type *const tp_s_l = !ir_target_big_endian() ? tp_s : tp_u;
	return lower_Builtin_type(lowered_builtin_type_low, mtp, tp_s_l, tp_u);
}

/**
 * lowers a builtin which reduces a 64bit value to a simple summary value
 * (popcount, ffs, ...)
 */
static void lower_reduce_builtin(ir_node *const builtin)
{
	ir_builtin_kind  kind         = get_Builtin_kind(builtin);
	ir_node         *operand      = get_Builtin_param(builtin, 0);
	ir_mode         *operand_mode = get_irn_mode(operand);
	if (!needs_lowering(operand_mode))
		return;

	arch_allow_ifconv_func allow_ifconv = ir_target.allow_ifconv;
	const lower64_entry_t *entry = get_node_entry(operand);
	dbg_info *dbgi              = get_irn_dbg_info(builtin);
	ir_graph *irg               = get_irn_irg(builtin);
	ir_type  *type              = get_Builtin_type(builtin);
	ir_type  *lowered_type_high = lower_Builtin_type_high(type);
	ir_type  *lowered_type_low  = lower_Builtin_type_low(type);
	ir_type  *result_type       = get_method_res_type(lowered_type_low, 0);
	ir_mode  *result_mode       = get_type_mode(result_type);
	ir_node  *block             = get_nodes_block(builtin);
	ir_node  *mem               = get_Builtin_mem(builtin);
	ir_mode  *high_mode         = get_irn_mode(entry->high_word);
	ir_node  *in_high[]         = {entry->high_word};
	ir_node  *in_low[]          = {entry->low_word};

	assert(is_NoMem(mem));
	assert(get_irn_arity(builtin) == 2);

	ir_node *res;
	switch (kind) {
	case ir_bk_ffs: {
		ir_node *number_of_bits = new_r_Const_long(irg, result_mode, get_mode_size_bits(env.p.word_unsigned));
		ir_node *zero_high      = new_rd_Const_null(dbgi, irg, high_mode);
		ir_node *zero_unsigned  = new_rd_Const_null(dbgi, irg, env.p.word_unsigned);
		ir_node *zero_result    = new_rd_Const_null(dbgi, irg, result_mode);
		ir_node *cmp_low        = new_rd_Cmp(dbgi, block, entry->low_word, zero_unsigned, ir_relation_equal);
		ir_node *cmp_high       = new_rd_Cmp(dbgi, block, entry->high_word, zero_high, ir_relation_equal);
		ir_node *ffs_high       = new_rd_Builtin(dbgi, block, mem, 1, in_high, kind, lowered_type_high);
		ir_node *high_proj      = new_r_Proj(ffs_high, result_mode, pn_Builtin_max+1);
		ir_node *high           = new_rd_Add(dbgi, block, high_proj, number_of_bits);
		ir_node *ffs_low        = new_rd_Builtin(dbgi, block, mem, 1, in_low, kind, lowered_type_low);
		ir_node *low            = new_r_Proj(ffs_low, result_mode, pn_Builtin_max+1);
		ir_node *mux_high       = new_rd_Mux(dbgi, block, cmp_high, high, zero_result);
		if (!allow_ifconv(cmp_high, high, zero_result))
			ir_nodeset_insert(&created_mux_nodes, mux_high);

		res = new_rd_Mux(dbgi, block, cmp_low, low, mux_high);

		if (!allow_ifconv(cmp_low, low, mux_high))
			ir_nodeset_insert(&created_mux_nodes, res);
		break;
	}
	case ir_bk_clz: {
		ir_node *zero           = new_rd_Const_null(dbgi, irg, high_mode);
		ir_node *cmp_high       = new_rd_Cmp(dbgi, block, entry->high_word, zero, ir_relation_equal);
		ir_node *clz_high       = new_rd_Builtin(dbgi, block, mem, 1, in_high, kind, lowered_type_high);
		ir_node *high           = new_r_Proj(clz_high, result_mode, pn_Builtin_max+1);
		ir_node *clz_low        = new_rd_Builtin(dbgi, block, mem, 1, in_low, kind, lowered_type_low);
		ir_node *low_proj       = new_r_Proj(clz_low, result_mode, pn_Builtin_max+1);
		ir_mode *mode           = get_node_high_mode(builtin);
		ir_node *number_of_bits = new_r_Const_long(irg, result_mode, get_mode_size_bits(mode));
		ir_node *low            = new_rd_Add(dbgi, block, low_proj, number_of_bits);
		res = new_rd_Mux(dbgi, block, cmp_high, high, low);

		if (!allow_ifconv(cmp_high, high, low))
			ir_nodeset_insert(&created_mux_nodes, res);
		break;
	}
	case ir_bk_ctz: {
		ir_node *zero_unsigned  = new_rd_Const_null(dbgi, irg, env.p.word_unsigned);
		ir_node *cmp_low        = new_rd_Cmp(dbgi, block, entry->low_word, zero_unsigned, ir_relation_equal);
		ir_node *ffs_high       = new_rd_Builtin(dbgi, block, mem, 1, in_high, kind, lowered_type_high);
		ir_node *high_proj      = new_r_Proj(ffs_high, result_mode, pn_Builtin_max+1);
		ir_node *number_of_bits = new_r_Const_long(irg, result_mode, get_mode_size_bits(env.p.word_unsigned));
		ir_node *high           = new_rd_Add(dbgi, block, high_proj, number_of_bits);
		ir_node *ffs_low        = new_rd_Builtin(dbgi, block, mem, 1, in_low, kind, lowered_type_low);
		ir_node *low            = new_r_Proj(ffs_low, result_mode, pn_Builtin_max+1);
		res = new_rd_Mux(dbgi, block, cmp_low, low, high);

		if (!allow_ifconv(cmp_low, low, high))
			ir_nodeset_insert(&created_mux_nodes, res);
		break;
	}
	case ir_bk_popcount: {
		ir_node *popcount_high = new_rd_Builtin(dbgi, block, mem, 1, in_high, kind, lowered_type_high);
		ir_node *popcount_low  = new_rd_Builtin(dbgi, block, mem, 1, in_low, kind, lowered_type_low);
		ir_node *high          = new_r_Proj(popcount_high, result_mode, pn_Builtin_max+1);
		ir_node *low           = new_r_Proj(popcount_low, result_mode, pn_Builtin_max+1);

		res = new_rd_Add(dbgi, block, high, low);
		break;
	}
	case ir_bk_parity: {
		ir_node *const in[]   = { new_rd_Eor(dbgi, block, in_high[0], in_low[0]) };
		ir_node *const parity = new_rd_Builtin(dbgi, block, mem, 1, in, kind, lowered_type_high);
		res = new_r_Proj(parity, result_mode, pn_Builtin_max + 1);
		break;
	}
	default:
		panic("unexpected builtin");
	}

	ir_node *const in[] = {
		[pn_Builtin_M]       = mem,
		[pn_Builtin_max + 1] = res,
	};
	turn_into_tuple(builtin, ARRAY_SIZE(in), in);
}

/**
 * lowers builtins performing arithmetic (bswap)
 */
static void lower_arithmetic_builtin(ir_node *const builtin)
{
	ir_builtin_kind  kind         = get_Builtin_kind(builtin);
	ir_node         *operand      = get_Builtin_param(builtin, 0);
	ir_mode         *operand_mode = get_irn_mode(operand);
	if (!needs_lowering(operand_mode))
		return;

	dbg_info              *dbgi              = get_irn_dbg_info(builtin);
	ir_type               *type              = get_Builtin_type(builtin);
	ir_type               *lowered_type_high = lower_Builtin_type_high(type);
	ir_type               *lowered_type_low  = lower_Builtin_type_low(type);
	ir_node               *block             = get_nodes_block(builtin);
	ir_node               *mem               = get_Builtin_mem(builtin);
	const lower64_entry_t *entry             = get_node_entry(operand);
	ir_mode               *mode_high         = get_irn_mode(entry->high_word);

	ir_node               *res_high;
	ir_node               *res_low;
	switch (kind) {
	case ir_bk_bswap: {
		ir_node *in_high[] = { entry->high_word };
		ir_node *in_low[]  = { entry->low_word };
		ir_node *swap_high = new_rd_Builtin(dbgi, block, mem, 1, in_high, kind, lowered_type_high);
		ir_node *swap_low  = new_rd_Builtin(dbgi, block, mem, 1, in_low, kind, lowered_type_low);
		ir_node *high      = new_r_Proj(swap_high, mode_high, pn_Builtin_max+1);
		ir_node *low       = new_r_Proj(swap_low, env.p.word_unsigned, pn_Builtin_max+1);
		if (mode_high == env.p.word_signed) {
			res_high = new_rd_Conv(dbgi, block, low, env.p.word_signed);
			res_low  = new_rd_Conv(dbgi, block, high, env.p.word_unsigned);
		} else {
			res_high = low;
			res_low  = high;
		}
		break;
	}
	default:
		panic("unexpected builtin");
	}

	/* search result Proj */
	ir_node *const proj = get_Proj_for_pn(builtin, pn_Builtin_max + 1);
	if (proj)
		ir_set_dw_lowered(proj, res_low, res_high);
}

/**
 * Lower double word builtins.
 */
static void lower_Builtin(ir_node *const builtin)
{
	ir_builtin_kind kind = get_Builtin_kind(builtin);
	switch (kind) {
	case ir_bk_compare_swap:
	case ir_bk_debugbreak:
	case ir_bk_frame_address:
	case ir_bk_inport:
	case ir_bk_may_alias:
	case ir_bk_outport:
	case ir_bk_prefetch:
	case ir_bk_return_address:
	case ir_bk_saturating_increment:
	case ir_bk_trap:
	case ir_bk_va_start:
	case ir_bk_va_arg:
		/* Nothing to do/impossible to lower in a generic way */
		return;
	case ir_bk_bswap:
		lower_arithmetic_builtin(builtin);
		return;
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_ffs:
	case ir_bk_parity:
	case ir_bk_popcount:
		lower_reduce_builtin(builtin);
		return;
	}
	panic("unknown builtin");
}

/**
 * check for opcodes that must always be lowered.
 */
static bool always_lower(unsigned code)
{
	switch (code) {
	case iro_ASM:
	case iro_Bitcast:
	case iro_Builtin:
	case iro_Call:
	case iro_Cond:
	case iro_Conv:
	case iro_Proj:
	case iro_Return:
	case iro_Sel:
	case iro_Start:
	case iro_Switch:
		return true;
	default:
		return false;
	}
}

/**
 * Compare two op_mode_entry_t's.
 */
static int cmp_op_mode(const void *elt, const void *key, size_t size)
{
	(void)size;
	const op_mode_entry_t *e1 = (const op_mode_entry_t*)elt;
	const op_mode_entry_t *e2 = (const op_mode_entry_t*)key;
	return (e1->op != e2->op) | (e1->imode != e2->imode) | (e1->omode != e2->omode);
}

/**
 * Compare two conv_tp_entry_t's.
 */
static int cmp_conv_tp(const void *elt, const void *key, size_t size)
{
	(void)size;
	const conv_tp_entry_t *e1 = (const conv_tp_entry_t*)elt;
	const conv_tp_entry_t *e2 = (const conv_tp_entry_t*)key;
	return (e1->imode != e2->imode) | (e1->omode != e2->omode);
}

/**
 * Enter a lowering function into an ir_op.
 */
void ir_register_dw_lower_function(ir_op *op, lower_dw_func func)
{
	op->ops.generic = (op_func)func;
}

static void enqueue_preds(ir_node *node)
{
	foreach_irn_in(node, i, pred) {
		deq_push_pointer_right(&env.waitq, pred);
	}
}

static void lower_node(ir_node *node)
{
	if (irn_visited_else_mark(node))
		return;

	/* cycles are always broken at Phi and Block nodes. So we don't need special
	 * magic in all the other lower functions */
	if (is_Block(node)) {
		enqueue_preds(node);
		return;
	} else if (is_Phi(node)) {
		lower_Phi(node);
		return;
	}

	/* depth-first: descend into operands */
	if (!is_Block(node)) {
		ir_node *block = get_nodes_block(node);
		lower_node(block);
	}

	if (!is_Cond(node)) {
		foreach_irn_in(node, i, pred) {
			lower_node(pred);
		}
	}

	ir_op        *op   = get_irn_op(node);
	lower_dw_func func = (lower_dw_func) op->ops.generic;
	if (func == NULL)
		return;

	unsigned         idx   = get_irn_idx(node);
	lower64_entry_t *entry = idx < env.n_entries ? env.entries[idx] : NULL;
	if (entry != NULL || always_lower(get_irn_opcode(node))) {
		DB((dbg, LEVEL_1, "  %+F\n", node));
		func(node);
	}
}

static void clear_node_and_phi_links(ir_node *node, void *data)
{
	(void)data;
	if (get_irn_mode(node) == mode_T) {
		set_irn_link(node, node);
	} else {
		set_irn_link(node, NULL);
	}
	if (is_Block(node))
		set_Block_phis(node, NULL);
	else if (is_Phi(node))
		set_Phi_next(node, NULL);
}

static void lower_irg(ir_graph *irg)
{
	constbits_analyze(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	/* just here for debugging */
	obstack_init(&env.obst);

	unsigned n_idx = get_irg_last_idx(irg);
	env.n_entries = n_idx;
	env.entries   = NEW_ARR_FZ(lower64_entry_t*, n_idx);
	env.irg       = irg;
	env.flags     = 0;

	ir_entity *ent         = get_irg_entity(irg);
	ir_type   *mtp         = get_entity_type(ent);
	ir_type   *lowered_mtp = lower_mtp(mtp);

	if (lowered_mtp != mtp) {
		set_entity_type(ent, lowered_mtp);
		env.flags |= MUST_BE_LOWERED;

		fix_parameter_entities(irg, mtp);
	}

	/* first step: link all nodes and allocate data */
	ir_reserve_resources(irg, IR_RESOURCE_PHI_LIST | IR_RESOURCE_IRN_LINK);
	visit_all_identities(irg, clear_node_and_phi_links, NULL);
	irg_walk_graph(irg, NULL, prepare_links, NULL);

	if (env.flags & MUST_BE_LOWERED) {
		ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
		inc_irg_visited(irg);

		assert(deq_empty(&env.waitq));
		deq_push_pointer_right(&env.waitq, get_irg_end(irg));

		env.lowered_phis = NEW_ARR_F(ir_node*, 0);
		while (!deq_empty(&env.waitq)) {
			ir_node *node = deq_pop_pointer_left(ir_node, &env.waitq);
			lower_node(node);
		}

		/* we need to fixup phis */
		for (size_t i = 0, n = ARR_LEN(env.lowered_phis); i < n; ++i) {
			ir_node *phi = env.lowered_phis[i];
			fixup_phi(phi);
		}
		DEL_ARR_F(env.lowered_phis);
		ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
	}

	ir_free_resources(irg, IR_RESOURCE_PHI_LIST | IR_RESOURCE_IRN_LINK);

	DEL_ARR_F(env.entries);
	obstack_free(&env.obst, NULL);

	confirm_irg_properties(irg,
		!(env.flags & MUST_BE_LOWERED) ? IR_GRAPH_PROPERTIES_ALL :
		env.flags & CF_CHANGED ? IR_GRAPH_PROPERTIES_NONE
		                       : IR_GRAPH_PROPERTIES_CONTROL_FLOW);

	constbits_clear(irg);
}

void ir_prepare_dw_lowering(const lwrdw_param_t *params)
{
	assert(get_mode_size_bits(params->word_unsigned)
	       == (unsigned)params->doubleword_size/2);
	assert(get_mode_size_bits(params->word_signed)
	       == (unsigned)params->doubleword_size/2);
	assert(mode_is_signed(params->word_signed));
	assert(!mode_is_signed(params->word_unsigned));
	assert(get_mode_arithmetic(params->word_signed) == irma_twos_complement);
	assert(get_mode_arithmetic(params->word_unsigned) == irma_twos_complement);

	memset(&env, 0, sizeof(env));
	env.p = *params;

	FIRM_DBG_REGISTER(dbg, "firm.lower.dw");

	ir_clear_opcodes_generic_func();
	ir_register_dw_lower_function(op_ASM,     lower_ASM);
	ir_register_dw_lower_function(op_Add,     lower_Add);
	ir_register_dw_lower_function(op_And,     lower_And);
	ir_register_dw_lower_function(op_Bad,     lower_Bad);
	ir_register_dw_lower_function(op_Bitcast, lower_Bitcast);
	ir_register_dw_lower_function(op_Builtin, lower_Builtin);
	ir_register_dw_lower_function(op_Call,    lower_Call);
	ir_register_dw_lower_function(op_Cmp,     lower_Cmp);
	ir_register_dw_lower_function(op_Cond,    lower_Cond);
	ir_register_dw_lower_function(op_Const,   lower_Const);
	ir_register_dw_lower_function(op_Conv,    ir_default_lower_dw_Conv);
	ir_register_dw_lower_function(op_Div,     lower_Div);
	ir_register_dw_lower_function(op_Eor,     lower_Eor);
	ir_register_dw_lower_function(op_Load,    lower_Load);
	ir_register_dw_lower_function(op_Minus,   lower_Minus);
	ir_register_dw_lower_function(op_Mod,     lower_Mod);
	ir_register_dw_lower_function(op_Mul,     lower_binop);
	ir_register_dw_lower_function(op_Mux,     lower_Mux);
	ir_register_dw_lower_function(op_Not,     lower_Not);
	ir_register_dw_lower_function(op_Or,      lower_Or);
	ir_register_dw_lower_function(op_Proj,    lower_Proj);
	ir_register_dw_lower_function(op_Return,  lower_Return);
	ir_register_dw_lower_function(op_Shl,     lower_Shl);
	ir_register_dw_lower_function(op_Shr,     lower_Shr);
	ir_register_dw_lower_function(op_Shrs,    lower_Shrs);
	ir_register_dw_lower_function(op_Start,   lower_Start);
	ir_register_dw_lower_function(op_Store,   lower_Store);
	ir_register_dw_lower_function(op_Sub,     lower_Sub);
	ir_register_dw_lower_function(op_Switch,  lower_Switch);
	ir_register_dw_lower_function(op_Unknown, lower_Unknown);
}

/**
 * Callback to lower only the Mux nodes we created.
 */
static int lower_mux_cb(ir_node *mux)
{
	return ir_nodeset_contains(&created_mux_nodes, mux);
}

static ir_type *make_type_4_2(ir_type *const even, ir_type *const odd)
{
	ir_type *const tp = new_type_method(4, 2, false, cc_cdecl_set, mtp_no_property);
	set_method_param_type(tp, 0, even);
	set_method_param_type(tp, 1, odd);
	set_method_param_type(tp, 2, even);
	set_method_param_type(tp, 3, odd);
	set_method_res_type(tp, 0, even);
	set_method_res_type(tp, 1, odd);
	return tp;
}

static ir_type *make_type_2_2(ir_type *const even, ir_type *const odd)
{
	ir_type *const tp = new_type_method(2, 2, false, cc_cdecl_set, mtp_no_property);
	set_method_param_type(tp, 0, even);
	set_method_param_type(tp, 1, odd);
	set_method_res_type(tp, 0, even);
	set_method_res_type(tp, 1, odd);
	return tp;
}

/*
 * Do the lowering.
 */
void ir_lower_dw_ops(void)
{
	/* create the necessary maps */
	if (!intrinsic_fkt) {
		intrinsic_fkt = new_set(cmp_op_mode, iro_last + 1);
		conv_types    = new_set(cmp_conv_tp, 16);
		lowered_type              = pmap_create();
		lowered_builtin_type_low  = pmap_create();
		lowered_builtin_type_high = pmap_create();
		tp_u = get_type_for_mode(env.p.word_unsigned);
		tp_s = get_type_for_mode(env.p.word_signed);
		tp_l_u = new_type_primitive(env.p.word_unsigned);
		tp_l_u->flags |= tf_lowered_dw;
		tp_l_s = new_type_primitive(env.p.word_signed);
		tp_l_s->flags |= tf_lowered_dw;

		ir_type *const even = ir_target_big_endian() ? tp_l_s : tp_l_u;
		ir_type *const odd  = ir_target_big_endian() ? tp_u   : tp_s;
		binop_tp_u = make_type_4_2(tp_l_u, tp_u);
		binop_tp_s = make_type_4_2(even, odd);
		unop_tp_u  = make_type_2_2(tp_l_u, tp_u);
		unop_tp_s  = make_type_2_2(even, odd);
	}

	ir_mode *offset_mode = get_reference_offset_mode(mode_P);
	env.tv_mode_bytes = new_tarval_from_long(env.p.doubleword_size/(2*8),
	                                         offset_mode);
	deq_init(&env.waitq);

	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_LINK
	                         | IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	/* transform all graphs */
	foreach_irp_irg(i, irg) {
		ir_nodeset_init(&created_mux_nodes);

		lower_irg(irg);

		if (ir_nodeset_size(&created_mux_nodes) > 0)
			lower_mux(irg, lower_mux_cb);

		ir_nodeset_destroy(&created_mux_nodes);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_LINK | IRP_RESOURCE_TYPE_VISITED);
	deq_free(&env.waitq);
}
