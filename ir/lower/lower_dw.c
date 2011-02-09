/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Lower Double word operations, ie 64bit -> 32bit, 32bit -> 16bit etc.
 * @date    8.10.2004
 * @author  Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "error.h"
#include "lowering.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "tv_t.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "firmstat.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irflag.h"
#include "irtools.h"
#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "pdeq.h"
#include "irdump.h"
#include "array_t.h"
#include "irpass_t.h"

typedef struct lower_env_t lower_env_t;

/**
 * The type of a lower function.
 *
 * @param node   the node to be lowered
 * @param env    the lower environment
 */
typedef void (*lower_func)(ir_node *node, ir_mode *mode, lower_env_t *env);

/** A map from (op, imode, omode) to Intrinsic functions entities. */
static set *intrinsic_fkt;

/** A map from (imode, omode) to conv function types. */
static set *conv_types;

/** A map from a method type to its lowered type. */
static pmap *lowered_type;

/** The types for the binop and unop intrinsics. */
static ir_type *binop_tp_u, *binop_tp_s, *unop_tp_u, *unop_tp_s, *shiftop_tp_u, *shiftop_tp_s, *tp_s, *tp_u;

/** the debug handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An entry in the (op, imode, omode) -> entity map.
 */
typedef struct op_mode_entry {
	const ir_op   *op;    /**< the op */
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_entity     *ent;   /**< the associated entity of this (op, imode, omode) triple */
} op_mode_entry_t;

/**
 * An entry in the (imode, omode) -> tp map.
 */
typedef struct conv_tp_entry {
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_type       *mtd;   /**< the associated method type of this (imode, omode) pair */
} conv_tp_entry_t;

/**
 * Every double word node will be replaced,
 * we need some store to hold the replacement:
 */
typedef struct node_entry_t {
	ir_node *low_word;    /**< the low word */
	ir_node *high_word;   /**< the high word */
} node_entry_t;

enum lower_flags {
	MUST_BE_LOWERED = 1,  /**< graph must be lowered */
	CF_CHANGED      = 2,  /**< control flow was changed */
};

/**
 * The lower environment.
 */
struct lower_env_t {
	node_entry_t **entries;       /**< entries per node */
	ir_graph      *irg;
	struct obstack obst;          /**< an obstack holding the temporary data */
	ir_type   *l_mtp;              /**< lowered method type of the current method */
	ir_tarval *tv_mode_bytes;     /**< a tarval containing the number of bytes in the lowered modes */
	ir_tarval *tv_mode_bits;      /**< a tarval containing the number of bits in the lowered modes */
	pdeq      *waitq;              /**< a wait queue of all nodes that must be handled later */
	ir_node  **lowered_phis;       /**< list of lowered phis */
	pmap      *proj_2_block;       /**< a map from ProjX to its destination blocks */
	ir_mode   *high_signed;        /**< doubleword signed type */
	ir_mode   *high_unsigned;      /**< doubleword unsigned type */
	ir_mode   *low_signed;         /**< word signed type */
	ir_mode   *low_unsigned;       /**< word unsigned type */
	ident     *first_id;           /**< .l for little and .h for big endian */
	ident     *next_id;            /**< .h for little and .l for big endian */
	const lwrdw_param_t *params;  /**< transformation parameter */
	unsigned flags;               /**< some flags */
	unsigned n_entries;           /**< number of entries */
	ir_type  *value_param_tp;     /**< the old value param type */
};

/**
 * Create a method type for a Conv emulation from imode to omode.
 */
static ir_type *get_conv_type(ir_mode *imode, ir_mode *omode, lower_env_t *env)
{
	conv_tp_entry_t key, *entry;
	ir_type *mtd;

	key.imode = imode;
	key.omode = omode;
	key.mtd   = NULL;

	entry = (conv_tp_entry_t*)set_insert(conv_types, &key, sizeof(key), HASH_PTR(imode) ^ HASH_PTR(omode));
	if (! entry->mtd) {
		int n_param = 1, n_res = 1;

		if (imode == env->high_signed || imode == env->high_unsigned)
			n_param = 2;
		if (omode == env->high_signed || omode == env->high_unsigned)
			n_res = 2;

		/* create a new one */
		mtd = new_type_method(n_param, n_res);

		/* set param types and result types */
		n_param = 0;
		if (imode == env->high_signed) {
			set_method_param_type(mtd, n_param++, tp_u);
			set_method_param_type(mtd, n_param++, tp_s);
		} else if (imode == env->high_unsigned) {
			set_method_param_type(mtd, n_param++, tp_u);
			set_method_param_type(mtd, n_param++, tp_u);
		} else {
			ir_type *tp = get_type_for_mode(imode);
			set_method_param_type(mtd, n_param++, tp);
		}

		n_res = 0;
		if (omode == env->high_signed) {
			set_method_res_type(mtd, n_res++, tp_u);
			set_method_res_type(mtd, n_res++, tp_s);
		} else if (omode == env->high_unsigned) {
			set_method_res_type(mtd, n_res++, tp_u);
			set_method_res_type(mtd, n_res++, tp_u);
		} else {
			ir_type *tp = get_type_for_mode(omode);
			set_method_res_type(mtd, n_res++, tp);
		}
		entry->mtd = mtd;
	} else {
		mtd = entry->mtd;
	}
	return mtd;
}

/**
 * Add an additional control flow input to a block.
 * Patch all Phi nodes. The new Phi inputs are copied from
 * old input number nr.
 */
static void add_block_cf_input_nr(ir_node *block, int nr, ir_node *cf)
{
	int i, arity = get_irn_arity(block);
	ir_node **in, *phi;

	assert(nr < arity);

	NEW_ARR_A(ir_node *, in, arity + 1);
	for (i = 0; i < arity; ++i)
		in[i] = get_irn_n(block, i);
	in[i] = cf;

	set_irn_in(block, i + 1, in);

	for (phi = get_Block_phis(block); phi != NULL; phi = get_Phi_next(phi)) {
		for (i = 0; i < arity; ++i)
			in[i] = get_irn_n(phi, i);
		in[i] = in[nr];
		set_irn_in(phi, i + 1, in);
	}
}

/**
 * Add an additional control flow input to a block.
 * Patch all Phi nodes. The new Phi inputs are copied from
 * old input from cf tmpl.
 */
static void add_block_cf_input(ir_node *block, ir_node *tmpl, ir_node *cf)
{
	int i, arity = get_irn_arity(block);
	int nr = 0;

	for (i = 0; i < arity; ++i) {
		if (get_irn_n(block, i) == tmpl) {
			nr = i;
			break;
		}
	}
	assert(i < arity);
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
static void prepare_links(lower_env_t *env, ir_node *node)
{
	ir_mode      *mode = get_irn_op_mode(node);
	node_entry_t *link;
	int           i;

	if (mode == env->high_signed || mode == env->high_unsigned) {
		unsigned idx = get_irn_idx(node);
		/* ok, found a node that will be lowered */
		link = OALLOCZ(&env->obst, node_entry_t);

		if (idx >= env->n_entries) {
			/* enlarge: this happens only for Rotl nodes which is RARELY */
			unsigned old   = env->n_entries;
			unsigned n_idx = idx + (idx >> 3);

			ARR_RESIZE(node_entry_t *, env->entries, n_idx);
			memset(&env->entries[old], 0, (n_idx - old) * sizeof(env->entries[0]));
			env->n_entries = n_idx;
		}
		env->entries[idx] = link;
		env->flags |= MUST_BE_LOWERED;
	} else if (is_Conv(node)) {
		/* Conv nodes have two modes */
		ir_node *pred = get_Conv_op(node);
		mode = get_irn_mode(pred);

		if (mode == env->high_signed || mode == env->high_unsigned) {
			/* must lower this node either but don't need a link */
			env->flags |= MUST_BE_LOWERED;
		}
		return;
	}

	if (is_Proj(node)) {
		/* link all Proj nodes to its predecessor:
		   Note that Tuple Proj's and its Projs are linked either. */
		ir_node *pred = get_Proj_pred(node);

		set_irn_link(node, get_irn_link(pred));
		set_irn_link(pred, node);
	} else if (is_Phi(node)) {
		/* link all Phi nodes to its block */
		ir_node *block = get_nodes_block(node);
		add_Block_phi(block, node);
	} else if (is_Block(node)) {
		/* fill the Proj -> Block map */
		for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
			ir_node *pred = get_Block_cfgpred(node, i);

			if (is_Proj(pred))
				pmap_insert(env->proj_2_block, pred, node);
		}
	}
}

static node_entry_t *get_node_entry(lower_env_t *env, ir_node *node)
{
	unsigned idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	return env->entries[idx];
}

static void set_lowered(lower_env_t *env, ir_node *old,
                        ir_node *new_low, ir_node *new_high)
{
	node_entry_t *entry = get_node_entry(env, old);
	entry->low_word  = new_low;
	entry->high_word = new_high;
}

/**
 * Translate a Constant: create two.
 */
static void lower_Const(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph  *irg      = get_irn_irg(node);
	dbg_info  *dbg      = get_irn_dbg_info(node);
	ir_mode   *low_mode = env->low_unsigned;
	ir_tarval *tv       = get_Const_tarval(node);
	ir_tarval *tv_l     = tarval_convert_to(tv, low_mode);
	ir_node   *res_low  = new_rd_Const(dbg, irg, tv_l);
	ir_tarval *tv_shrs  = tarval_shrs(tv, env->tv_mode_bits);
	ir_tarval *tv_h     = tarval_convert_to(tv_shrs, mode);
	ir_node   *res_high = new_rd_Const(dbg, irg, tv_h);

	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a Load: create two.
 */
static void lower_Load(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_mode    *low_mode = env->low_unsigned;
	ir_graph   *irg = get_irn_irg(node);
	ir_node    *adr = get_Load_ptr(node);
	ir_node    *mem = get_Load_mem(node);
	ir_node    *low, *high, *proj;
	dbg_info   *dbg;
	ir_node    *block = get_nodes_block(node);
	ir_cons_flags volatility = get_Load_volatility(node) == volatility_is_volatile
	                         ? cons_volatile : cons_none;

	if (env->params->little_endian) {
		low  = adr;
		high = new_r_Add(block, adr, new_r_Const(irg, env->tv_mode_bytes), get_irn_mode(adr));
	} else {
		low  = new_r_Add(block, adr, new_r_Const(irg, env->tv_mode_bytes), get_irn_mode(adr));
		high = adr;
	}

	/* create two loads */
	dbg  = get_irn_dbg_info(node);
	low  = new_rd_Load(dbg, block, mem,  low,  low_mode, volatility);
	proj = new_r_Proj(low, mode_M, pn_Load_M);
	high = new_rd_Load(dbg, block, proj, high, mode, volatility);

	set_lowered(env, node, low, high);

	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Load_M:         /* Memory result. */
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Load_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		case pn_Load_res: {       /* Result of load operation. */
			ir_node *res_low  = new_r_Proj(low,  low_mode, pn_Load_res);
			ir_node *res_high = new_r_Proj(high, mode,     pn_Load_res);
			set_lowered(env, proj, res_low, res_high);
			break;
		}
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall
		 * into out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Store: create two.
 */
static void lower_Store(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph     *irg;
	ir_node      *block, *adr, *mem;
	ir_node      *low, *high, *proj;
	dbg_info     *dbg;
	ir_node            *value = get_Store_value(node);
	const node_entry_t *entry = get_node_entry(env, value);
	ir_cons_flags volatility = get_Store_volatility(node) == volatility_is_volatile
	                           ? cons_volatile : cons_none;
	(void) mode;

	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	irg = get_irn_irg(node);
	adr = get_Store_ptr(node);
	mem = get_Store_mem(node);
	block = get_nodes_block(node);

	if (env->params->little_endian) {
		low  = adr;
		high = new_r_Add(block, adr, new_r_Const(irg, env->tv_mode_bytes), get_irn_mode(adr));
	} else {
		low  = new_r_Add(block, adr, new_r_Const(irg, env->tv_mode_bytes), get_irn_mode(adr));
		high = adr;
	}

	/* create two Stores */
	dbg = get_irn_dbg_info(node);
	low  = new_rd_Store(dbg, block, mem, low,  entry->low_word, volatility);
	proj = new_r_Proj(low, mode_M, pn_Store_M);
	high = new_rd_Store(dbg, block, proj, high, entry->high_word, volatility);

	set_lowered(env, node, low, high);

	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Store_M:         /* Memory result. */
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Store_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
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
                                      ir_mode *imode, ir_mode *omode,
                                      lower_env_t *env)
{
	symconst_symbol sym;
	ir_entity *ent;
	op_mode_entry_t key, *entry;

	key.op    = op;
	key.imode = imode;
	key.omode = omode;
	key.ent   = NULL;

	entry = (op_mode_entry_t*)set_insert(intrinsic_fkt, &key, sizeof(key),
				HASH_PTR(op) ^ HASH_PTR(imode) ^ (HASH_PTR(omode) << 8));
	if (! entry->ent) {
		/* create a new one */
		ent = env->params->create_intrinsic(method, op, imode, omode, env->params->ctx);

		assert(ent && "Intrinsic creator must return an entity");
		entry->ent = ent;
	} else {
		ent = entry->ent;
	}
	sym.entity_p = ent;
	return new_r_SymConst(env->irg, mode_P_code, sym, symconst_addr_ent);
}

/**
 * Translate a Div.
 *
 * Create an intrinsic Call.
 */
static void lower_Div(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node            *left        = get_Div_left(node);
	ir_node            *right       = get_Div_right(node);
	const node_entry_t *left_entry  = get_node_entry(env, left);
	const node_entry_t *right_entry = get_node_entry(env, right);
	ir_node            *block       = get_nodes_block(node);
	dbg_info           *dbgi        = get_irn_dbg_info(node);
	ir_type            *mtp
		= mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	ir_mode            *opmode = get_irn_op_mode(node);
	ir_node            *addr
		= get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode, env);
	ir_node  *in[4] = {
		left_entry->low_word, left_entry->high_word,
		right_entry->low_word, right_entry->high_word };
	ir_node            *call
		= new_rd_Call(dbgi, block, get_Div_mem(node), addr, 4, in, mtp);
	ir_node            *resproj = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_node            *proj;

	set_irn_pinned(call, get_irn_pinned(node));

	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Div_M:         /* Memory result. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M);
			break;
		case pn_Div_X_except:  /* Execution result if exception occurred. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Div_res: {
			/* Result of computation. */
			ir_node *res_low  = new_r_Proj(resproj, env->low_unsigned, 0);
			ir_node *res_high = new_r_Proj(resproj, mode,              1);
			set_lowered(env, proj, res_low, res_high);
			break;
		}
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Mod.
 *
 * Create an intrinsic Call.
 */
static void lower_Mod(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node            *left        = get_Mod_left(node);
	ir_node            *right       = get_Mod_right(node);
	const node_entry_t *left_entry  = get_node_entry(env, left);
	const node_entry_t *right_entry = get_node_entry(env, right);
	ir_node            *in[4]       = {
		left_entry->low_word, left_entry->high_word,
		right_entry->low_word, right_entry->high_word
	};
	dbg_info           *dbgi        = get_irn_dbg_info(node);
	ir_node            *block       = get_nodes_block(node);
	ir_type            *mtp
		= mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	ir_mode            *opmode = get_irn_op_mode(node);
	ir_node            *addr
		= get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode, env);
	ir_node            *call
		= new_rd_Call(dbgi, block, get_Mod_mem(node), addr, 4, in, mtp);
	ir_node            *resproj = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_node            *proj;
	set_irn_pinned(call, get_irn_pinned(node));

	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Mod_M:         /* Memory result. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M);
			break;
		case pn_Mod_X_except:  /* Execution result if exception occurred. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Mod_res: {
			/* Result of computation. */
			ir_node *res_low  = new_r_Proj(resproj, env->low_unsigned, 0);
			ir_node *res_high = new_r_Proj(resproj, mode,              1);
			set_lowered(env, proj, res_low, res_high);
			break;
		}
		default:
			assert(0 && "unexpected Proj number");
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
static void lower_binop(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node            *left        = get_binop_left(node);
	ir_node            *right       = get_binop_right(node);
	const node_entry_t *left_entry  = get_node_entry(env, left);
	const node_entry_t *right_entry = get_node_entry(env, right);
	ir_node            *in[4]       = {
		left_entry->low_word, left_entry->high_word,
		right_entry->low_word, right_entry->high_word
	};
	dbg_info           *dbgi        = get_irn_dbg_info(node);
	ir_node            *block       = get_nodes_block(node);
	ir_graph           *irg         = get_irn_irg(block);
	ir_type            *mtp
		= mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	ir_node            *addr
		= get_intrinsic_address(mtp, get_irn_op(node), mode, mode, env);
	ir_node            *call
		= new_rd_Call(dbgi, block, get_irg_no_mem(irg), addr, 4, in, mtp);
	ir_node            *resproj  = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_node            *res_low  = new_r_Proj(resproj, env->low_unsigned, 0);
	ir_node            *res_high = new_r_Proj(resproj, mode,              1);
	set_irn_pinned(call, get_irn_pinned(node));
	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a Shiftop.
 *
 * Create an intrinsic Call.
 */
static void lower_Shiftop(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node            *block      = get_nodes_block(node);
	ir_node            *left       = get_binop_left(node);
	const node_entry_t *left_entry = get_node_entry(env, left);
	ir_node            *right      = get_binop_right(node);
	ir_node            *in[3]      = {
		left_entry->low_word, left_entry->high_word,
		/* it should be safe to conv to low_unsigned */
		new_r_Conv(block, right, env->low_unsigned)
	};
	dbg_info           *dbgi       = get_irn_dbg_info(node);
	ir_graph           *irg        = get_irn_irg(block);
	ir_type            *mtp
		= mode_is_signed(mode) ? shiftop_tp_s : shiftop_tp_u;
	ir_node            *addr
		= get_intrinsic_address(mtp, get_irn_op(node), mode, mode, env);
	ir_node            *call
		= new_rd_Call(dbgi, block, get_irg_no_mem(irg), addr, 3, in, mtp);
	ir_node            *resproj  = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_node            *res_low  = new_r_Proj(resproj, env->low_unsigned, 0);
	ir_node            *res_high = new_r_Proj(resproj, mode,              1);

	set_irn_pinned(call, get_irn_pinned(node));
	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a Shr and handle special cases.
 */
static void lower_Shr(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *right = get_Shr_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		ir_tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
		    get_tarval_long(tv) >= (long)get_mode_size_bits(mode)) {
			ir_node *block        = get_nodes_block(node);
			ir_node *left         = get_Shr_left(node);
			ir_mode *low_unsigned = env->low_unsigned;
			long shf_cnt = get_tarval_long(tv) - get_mode_size_bits(mode);
			const node_entry_t *left_entry = get_node_entry(env, left);
			ir_node *res_low;
			ir_node *res_high;

			left = left_entry->high_word;

			/* convert high word into low_unsigned mode if necessary */
			if (get_irn_mode(left) != low_unsigned)
				left = new_r_Conv(block, left, low_unsigned);

			if (shf_cnt > 0) {
				ir_node *c = new_r_Const_long(irg, low_unsigned, shf_cnt);
				res_low = new_r_Shr(block, left, c, low_unsigned);
			} else {
				res_low = left;
			}
			res_high = new_r_Const(irg, get_mode_null(mode));
			set_lowered(env, node, res_low, res_high);

			return;
		}
	}
	lower_Shiftop(node, mode, env);
}

/**
 * Translate a Shl and handle special cases.
 */
static void lower_Shl(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *right = get_Shl_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		ir_tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv)) {
			long value = get_tarval_long(tv);
		    if (value >= (long)get_mode_size_bits(mode)) {
				/* simple case: shift above the lower word */
				ir_mode *mode_l;
				ir_node *block = get_nodes_block(node);
				ir_node *left = get_Shl_left(node);
				ir_node *c;
				long shf_cnt = get_tarval_long(tv) - get_mode_size_bits(mode);
				const node_entry_t *left_entry = get_node_entry(env, left);
				ir_node  *res_low;
				ir_node  *res_high;

				left = left_entry->low_word;
				left = new_r_Conv(block, left, mode);

				mode_l = env->low_unsigned;
				if (shf_cnt > 0) {
					c        = new_r_Const_long(irg, mode_l, shf_cnt);
					res_high = new_r_Shl(block, left, c, mode);
				} else {
					res_high = left;
				}
				res_low = new_r_Const(irg, get_mode_null(mode_l));
				set_lowered(env, node, res_low, res_high);

				return;
			}
			if (value == 1) {
				/* left << 1 == left + left */
				ir_node            *left        = get_binop_left(node);
				const node_entry_t *left_entry  = get_node_entry(env, left);
				ir_node            *in[4]       = {
					left_entry->low_word, left_entry->high_word,
					left_entry->low_word, left_entry->high_word,
				};
				dbg_info           *dbgi        = get_irn_dbg_info(node);
				ir_node            *block       = get_nodes_block(node);
				ir_graph           *irg         = get_irn_irg(block);
				ir_type            *mtp
					= mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
				ir_node            *addr
					= get_intrinsic_address(mtp, op_Add, mode, mode, env);
				ir_node            *call
					= new_rd_Call(dbgi, block, get_irg_no_mem(irg), addr, 4, in, mtp);
				ir_node            *resproj  = new_r_Proj(call, mode_T, pn_Call_T_result);
				ir_node            *res_low  = new_r_Proj(resproj, env->low_unsigned, 0);
				ir_node            *res_high = new_r_Proj(resproj, mode,              1);
				set_irn_pinned(call, get_irn_pinned(node));
				set_lowered(env, node, res_low, res_high);

				return;
			}
		}
	}
	lower_Shiftop(node, mode, env);
}

/**
 * Translate a Shrs and handle special cases.
 */
static void lower_Shrs(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *right = get_Shrs_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		ir_tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
		    get_tarval_long(tv) >= (long)get_mode_size_bits(mode)) {
			ir_node *block         = get_nodes_block(node);
			ir_node *left          = get_Shrs_left(node);
			ir_mode *low_unsigned  = env->low_unsigned;
			long     shf_cnt       = get_tarval_long(tv) - get_mode_size_bits(mode);
			const node_entry_t *left_entry = get_node_entry(env, left);
			ir_node *left_unsigned = left;
			ir_node *res_low;
			ir_node *res_high;
			ir_node *c;

			left = left_entry->high_word;

			/* convert high word into low_unsigned mode if necessary */
			if (get_irn_mode(left_unsigned) != low_unsigned)
				left_unsigned = new_r_Conv(block, left, low_unsigned);

			if (shf_cnt > 0) {
				c       = new_r_Const_long(irg, low_unsigned, shf_cnt);
				res_low = new_r_Shrs(block, left_unsigned, c, low_unsigned);
			} else {
				res_low = left_unsigned;
			}

			c        = new_r_Const(irg, get_mode_all_one(low_unsigned));
			res_high = new_r_Shrs(block, left, c, mode);
			set_lowered(env, node, res_low, res_high);
			return;
		}
	}
	lower_Shiftop(node, mode, env);
}

/**
 * Rebuild Rotl nodes into Or(Shl, Shr) and prepare all nodes.
 */
static void prepare_links_and_handle_rotl(ir_node *node, void *env)
{
	lower_env_t *lenv = (lower_env_t*)env;

	if (is_Rotl(node)) {
		ir_mode  *mode = get_irn_op_mode(node);
		ir_node  *right;
		ir_node  *left, *shl, *shr, *ornode, *block, *sub, *c;
		ir_mode  *omode, *rmode;
		ir_graph *irg;
		dbg_info *dbg;
		optimization_state_t state;

		if (mode != lenv->high_signed && mode != lenv->high_unsigned) {
			prepare_links(lenv, node);
			return;
		}

		/* replace the Rotl(x,y) by an Or(Shl(x,y), Shr(x,64-y)) */
		right = get_Rotl_right(node);
		irg   = get_irn_irg(node);
		dbg   = get_irn_dbg_info(node);
		omode = get_irn_mode(node);
		left  = get_Rotl_left(node);
		block = get_nodes_block(node);
		shl   = new_rd_Shl(dbg, block, left, right, omode);
		rmode = get_irn_mode(right);
		c     = new_r_Const_long(irg, rmode, get_mode_size_bits(omode));
		sub   = new_rd_Sub(dbg, block, c, right, rmode);
		shr   = new_rd_Shr(dbg, block, left, sub, omode);

		/* switch optimization off here, or we will get the Rotl back */
		save_optimization_state(&state);
		set_opt_algebraic_simplification(0);
		ornode = new_rd_Or(dbg, block, shl, shr, omode);
		restore_optimization_state(&state);

		exchange(node, ornode);

		/* do lowering on the new nodes */
		prepare_links(lenv, shl);
		prepare_links(lenv, c);
		prepare_links(lenv, sub);
		prepare_links(lenv, shr);
		prepare_links(lenv, ornode);
		return;
	}

	prepare_links(lenv, node);
}

/**
 * Translate an Unop.
 *
 * Create an intrinsic Call.
 */
static void lower_Unop(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node            *op       = get_unop_op(node);
	const node_entry_t *op_entry = get_node_entry(env, op);
	ir_node            *in[2]    = { op_entry->low_word, op_entry->high_word };
	dbg_info           *dbgi     = get_irn_dbg_info(node);
	ir_node            *block    = get_nodes_block(node);
	ir_graph           *irg      = get_irn_irg(block);
	ir_type            *mtp      = mode_is_signed(mode) ? unop_tp_s : unop_tp_u;
	ir_op              *irop     = get_irn_op(node);
	ir_node            *addr     = get_intrinsic_address(mtp, irop, mode, mode, env);
	ir_node            *nomem    = get_irg_no_mem(irg);
	ir_node            *call     = new_rd_Call(dbgi, block, nomem, addr, 2, in, mtp);
	ir_node            *resproj  = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_node            *res_low  = new_r_Proj(resproj, env->low_unsigned, 0);
	ir_node            *res_high = new_r_Proj(resproj, mode,              1);
	set_irn_pinned(call, get_irn_pinned(node));
	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a logical binop.
 *
 * Create two logical binops.
 */
static void lower_binop_logical(ir_node *node, ir_mode *mode, lower_env_t *env,
								ir_node *(*constr_rd)(dbg_info *db, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) )
{
	ir_node            *left        = get_binop_left(node);
	ir_node            *right       = get_binop_right(node);
	const node_entry_t *left_entry  = get_node_entry(env, left);
	const node_entry_t *right_entry = get_node_entry(env, right);
	dbg_info           *dbgi        = get_irn_dbg_info(node);
	ir_node            *block       = get_nodes_block(node);
	ir_node            *res_low
		= constr_rd(dbgi, block, left_entry->low_word, right_entry->low_word,
		            env->low_unsigned);
	ir_node            *res_high
		= constr_rd(dbgi, block, left_entry->high_word, right_entry->high_word,
		            mode);
	set_lowered(env, node, res_low, res_high);
}

static void lower_And(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	lower_binop_logical(node, mode, env, new_rd_And);
}

static void lower_Or(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	lower_binop_logical(node, mode, env, new_rd_Or);
}

static void lower_Eor(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	lower_binop_logical(node, mode, env, new_rd_Eor);
}

/**
 * Translate a Not.
 *
 * Create two logical Nots.
 */
static void lower_Not(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node            *op       = get_Not_op(node);
	const node_entry_t *op_entry = get_node_entry(env, op);
	dbg_info           *dbgi     = get_irn_dbg_info(node);
	ir_node            *block    = get_nodes_block(node);
	ir_node            *res_low
		= new_rd_Not(dbgi, block, op_entry->low_word, env->low_unsigned);
	ir_node            *res_high
		= new_rd_Not(dbgi, block, op_entry->high_word, mode);
	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a Cond.
 */
static void lower_Cond(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node *cmp, *left, *right, *block;
	ir_node *sel = get_Cond_selector(node);
	ir_mode *m = get_irn_mode(sel);
	ir_mode *cmp_mode;
	const node_entry_t *lentry, *rentry;
	ir_node  *proj, *projT = NULL, *projF = NULL;
	ir_node  *new_bl, *cmpH, *cmpL, *irn;
	ir_node  *projHF, *projHT;
	ir_node  *dst_blk;
	pn_Cmp   pnc;
	ir_graph *irg;
	dbg_info *dbg;

	(void) mode;

	if (m != mode_b) {
		if (m == env->high_signed || m == env->high_unsigned) {
			/* bad we can't really handle Switch with 64bit offsets */
			panic("Cond with 64bit jumptable not supported");
		}
		return;
	}

	if (!is_Proj(sel))
		return;

	cmp = get_Proj_pred(sel);
	if (!is_Cmp(cmp))
		return;

	left     = get_Cmp_left(cmp);
	cmp_mode = get_irn_mode(left);
	if (cmp_mode != env->high_signed && cmp_mode != env->high_unsigned)
		return;

	right  = get_Cmp_right(cmp);
	lentry = get_node_entry(env, left);
	rentry = get_node_entry(env, right);

	/* all right, build the code */
	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		long proj_nr = get_Proj_proj(proj);

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
	block = get_nodes_block(node);
	irg   = get_Block_irg(block);
	dbg   = get_irn_dbg_info(cmp);
	pnc   = get_Proj_pn_cmp(sel);

	if (is_Const(right) && is_Const_null(right)) {
		if (pnc == pn_Cmp_Eq || pnc == pn_Cmp_Lg) {
			/* x ==/!= 0 ==> or(low,high) ==/!= 0 */
			ir_mode *mode   = env->low_unsigned;
			ir_node *low    = new_r_Conv(block, lentry->low_word, mode);
			ir_node *high   = new_r_Conv(block, lentry->high_word, mode);
			ir_node *ornode = new_rd_Or(dbg, block, low, high, mode);
			ir_node *cmp    = new_rd_Cmp(dbg, block, ornode, new_r_Const_long(irg, mode, 0));

			ir_node *proj = new_r_Proj(cmp, mode_b, pnc);
			set_Cond_selector(node, proj);
			return;
		}
	}

	cmpH = new_rd_Cmp(dbg, block, lentry->high_word, rentry->high_word);

	if (pnc == pn_Cmp_Eq) {
		/* simple case:a == b <==> a_h == b_h && a_l == b_l */
		pmap_entry *entry = pmap_find(env->proj_2_block, projF);

		assert(entry);
		dst_blk = (ir_node*)entry->value;

		irn = new_r_Proj(cmpH, mode_b, pn_Cmp_Eq);
		dbg = get_irn_dbg_info(node);
		irn = new_rd_Cond(dbg, block, irn);

		projHF = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(projHF);
		exchange(projF, projHF);

		projHT = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(projHT);

		new_bl = new_r_Block(irg, 1, &projHT);

		dbg   = get_irn_dbg_info(cmp);
		cmpL = new_rd_Cmp(dbg, new_bl, lentry->low_word, rentry->low_word);
		irn = new_r_Proj(cmpL, mode_b, pn_Cmp_Eq);
		dbg = get_irn_dbg_info(node);
		irn = new_rd_Cond(dbg, new_bl, irn);

		proj = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(proj);
		add_block_cf_input(dst_blk, projHF, proj);

		proj = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(proj);
		exchange(projT, proj);
	} else if (pnc == pn_Cmp_Lg) {
		/* simple case:a != b <==> a_h != b_h || a_l != b_l */
		pmap_entry *entry = pmap_find(env->proj_2_block, projT);

		assert(entry);
		dst_blk = (ir_node*)entry->value;

		irn = new_r_Proj(cmpH, mode_b, pn_Cmp_Lg);
		dbg = get_irn_dbg_info(node);
		irn = new_rd_Cond(dbg, block, irn);

		projHT = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(projHT);
		exchange(projT, projHT);

		projHF = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(projHF);

		new_bl = new_r_Block(irg, 1, &projHF);

		dbg   = get_irn_dbg_info(cmp);
		cmpL = new_rd_Cmp(dbg, new_bl, lentry->low_word, rentry->low_word);
		irn = new_r_Proj(cmpL, mode_b, pn_Cmp_Lg);
		dbg = get_irn_dbg_info(node);
		irn = new_rd_Cond(dbg, new_bl, irn);

		proj = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(proj);
		add_block_cf_input(dst_blk, projHT, proj);

		proj = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(proj);
		exchange(projF, proj);
	} else {
		/* a rel b <==> a_h REL b_h || (a_h == b_h && a_l rel b_l) */
		ir_node *dstT, *dstF, *newbl_eq, *newbl_l;
		pmap_entry *entry;

		entry = pmap_find(env->proj_2_block, projT);
		assert(entry);
		dstT = (ir_node*)entry->value;

		entry = pmap_find(env->proj_2_block, projF);
		assert(entry);
		dstF = (ir_node*)entry->value;

		irn = new_r_Proj(cmpH, mode_b, pnc & ~pn_Cmp_Eq);
		dbg = get_irn_dbg_info(node);
		irn = new_rd_Cond(dbg, block, irn);

		projHT = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(projHT);
		exchange(projT, projHT);
		projT = projHT;

		projHF = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(projHF);

		newbl_eq = new_r_Block(irg, 1, &projHF);

		irn = new_r_Proj(cmpH, mode_b, pn_Cmp_Eq);
		irn = new_rd_Cond(dbg, newbl_eq, irn);

		proj = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(proj);
		exchange(projF, proj);
		projF = proj;

		proj = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(proj);

		newbl_l = new_r_Block(irg, 1, &proj);

		dbg   = get_irn_dbg_info(cmp);
		cmpL = new_rd_Cmp(dbg, newbl_l, lentry->low_word, rentry->low_word);
		irn = new_r_Proj(cmpL, mode_b, pnc);
		dbg = get_irn_dbg_info(node);
		irn = new_rd_Cond(dbg, newbl_l, irn);

		proj = new_r_Proj(irn, mode_X, pn_Cond_true);
		mark_irn_visited(proj);
		add_block_cf_input(dstT, projT, proj);

		proj = new_r_Proj(irn, mode_X, pn_Cond_false);
		mark_irn_visited(proj);
		add_block_cf_input(dstF, projF, proj);
	}

	/* we have changed the control flow */
	env->flags |= CF_CHANGED;
}

/**
 * Translate a Conv to higher_signed
 */
static void lower_Conv_to_Ll(ir_node *node, lower_env_t *env)
{
	ir_mode  *omode        = get_irn_mode(node);
	ir_node  *op           = get_Conv_op(node);
	ir_mode  *imode        = get_irn_mode(op);
	ir_graph *irg          = get_irn_irg(node);
	ir_node  *block        = get_nodes_block(node);
	dbg_info *dbg          = get_irn_dbg_info(node);
	ir_node  *res_low;
	ir_node  *res_high;

	ir_mode  *low_unsigned = env->low_unsigned;
	ir_mode  *low_signed
		= mode_is_signed(omode) ? env->low_signed : low_unsigned;

	if (mode_is_int(imode) || mode_is_reference(imode)) {
		if (imode == env->high_signed || imode == env->high_unsigned) {
			/* a Conv from Lu to Ls or Ls to Lu */
			const node_entry_t *op_entry = get_node_entry(env, op);
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
				res_high = new_rd_Shrs(dbg, block, op, cnst, low_signed);
			} else {
				res_high = new_r_Const(irg, get_mode_null(low_signed));
			}
		}
	} else if (imode == mode_b) {
		res_low  = new_rd_Conv(dbg, block, op, low_unsigned);
		res_high = new_r_Const(irg, get_mode_null(low_signed));
	} else {
		ir_node *irn, *call;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, env);
		call = new_rd_Call(dbg, block, get_irg_no_mem(irg), irn, 1, &op, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(call, mode_T, pn_Call_T_result);

		res_low  = new_r_Proj(irn, low_unsigned, 0);
		res_high = new_r_Proj(irn, low_signed, 1);
	}
	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a Conv from higher_unsigned
 */
static void lower_Conv_from_Ll(ir_node *node, lower_env_t *env)
{
	ir_node            *op    = get_Conv_op(node);
	ir_mode            *omode = get_irn_mode(node);
	ir_node            *block = get_nodes_block(node);
	dbg_info           *dbg   = get_irn_dbg_info(node);
	ir_graph           *irg   = get_irn_irg(node);
	const node_entry_t *entry = get_node_entry(env, op);

	if (mode_is_int(omode) || mode_is_reference(omode)) {
		op = entry->low_word;

		/* simple case: create a high word */
		if (omode != env->low_unsigned)
			op = new_rd_Conv(dbg, block, op, omode);

		set_Conv_op(node, op);
	} else if (omode == mode_b) {
		/* llu ? true : false  <=> (low|high) ? true : false */
		ir_mode *mode   = env->low_unsigned;
		ir_node *ornode = new_rd_Or(dbg, block, entry->low_word,
		                            entry->high_word, mode);
		set_Conv_op(node, ornode);
	} else {
		ir_node *irn, *call, *in[2];
		ir_mode *imode = get_irn_mode(op);
		ir_type *mtp   = get_conv_type(imode, omode, env);

		irn   = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, env);
		in[0] = entry->low_word;
		in[1] = entry->high_word;

		call = new_rd_Call(dbg, block, get_irg_no_mem(irg), irn, 2, in, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(call, mode_T, pn_Call_T_result);

		exchange(node, new_r_Proj(irn, omode, 0));
	}
}

/**
 * lower boolean Proj(Cmp)
 */
static void lower_Proj_Cmp(lower_env_t *env, ir_node *proj)
{
	ir_node  *cmp  = get_Proj_pred(proj);
	ir_node  *l    = get_Cmp_left(cmp);
	ir_mode  *mode = get_irn_mode(l);
	ir_node  *r, *low, *high, *t, *res;
	pn_Cmp    pnc;
	ir_node  *blk;
	dbg_info *db;
	const node_entry_t *lentry;
	const node_entry_t *rentry;

	if (mode != env->high_signed && mode != env->high_unsigned) {
		return;
	}

	r      = get_Cmp_right(cmp);
	lentry = get_node_entry(env, l);
	rentry = get_node_entry(env, r);
	pnc    = get_Proj_pn_cmp(proj);
	blk    = get_nodes_block(cmp);
	db     = get_irn_dbg_info(cmp);
	low    = new_rd_Cmp(db, blk, lentry->low_word, rentry->low_word);
	high   = new_rd_Cmp(db, blk, lentry->high_word, rentry->high_word);

	if (pnc == pn_Cmp_Eq) {
		/* simple case:a == b <==> a_h == b_h && a_l == b_l */
		res = new_rd_And(db, blk,
			new_r_Proj(low, mode_b, pnc),
			new_r_Proj(high, mode_b, pnc),
			mode_b);
	} else if (pnc == pn_Cmp_Lg) {
		/* simple case:a != b <==> a_h != b_h || a_l != b_l */
		res = new_rd_Or(db, blk,
			new_r_Proj(low, mode_b, pnc),
			new_r_Proj(high, mode_b, pnc),
			mode_b);
	} else {
		/* a rel b <==> a_h REL b_h || (a_h == b_h && a_l rel b_l) */
		t = new_rd_And(db, blk,
			new_r_Proj(low, mode_b, pnc),
			new_r_Proj(high, mode_b, pn_Cmp_Eq),
			mode_b);
		res = new_rd_Or(db, blk,
			new_r_Proj(high, mode_b, pnc & ~pn_Cmp_Eq),
			t,
			mode_b);
	}
	exchange(proj, res);
}

static void lower_Proj(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_node *pred = get_Proj_pred(node);
	(void) mode;
	if (is_Cmp(pred)) {
		lower_Proj_Cmp(env, node);
	}
}

/**
 * Translate a Conv.
 */
static void lower_Conv(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	mode = get_irn_mode(node);

	if (mode == env->high_signed || mode == env->high_unsigned) {
		lower_Conv_to_Ll(node, env);
	} else {
		ir_mode *op_mode = get_irn_mode(get_Conv_op(node));

		if (op_mode == env->high_signed	|| op_mode == env->high_unsigned) {
			lower_Conv_from_Ll(node, env);
		}
	}
}

/**
 * Remember the new argument index of this value type entity in the lowered
 * method type.
 *
 * @param ent  the entity
 * @param pos  the argument index of this entity
 */
static inline void set_entity_arg_idx(ir_entity *ent, int pos)
{
	set_entity_link(ent, INT_TO_PTR(pos));
}

/**
 * Retrieve the argument index of a value type entity.
 *
 * @param ent  the entity
 */
static int get_entity_arg_idx(const ir_entity *ent) {
	return (int)PTR_TO_INT(get_entity_link(ent));
}

/**
 * Lower the method type.
 *
 * @param env  the lower environment
 * @param mtp  the method type to lower
 *
 * @return the lowered type
 */
static ir_type *lower_mtp(lower_env_t *env, ir_type *mtp)
{
	pmap_entry *entry;
	ident      *lid;
	ir_type    *res, *value_type;

	if (is_lowered_type(mtp))
		return mtp;

	entry = pmap_find(lowered_type, mtp);
	if (! entry) {
		int i, orig_n_params, orig_n_res, n_param, n_res;

		/* count new number of params */
		n_param = orig_n_params = get_method_n_params(mtp);
		for (i = orig_n_params - 1; i >= 0; --i) {
			ir_type *tp = get_method_param_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->high_signed ||
					mode == env->high_unsigned)
					++n_param;
			}
		}

		/* count new number of results */
		n_res = orig_n_res = get_method_n_ress(mtp);
		for (i = orig_n_res - 1; i >= 0; --i) {
			ir_type *tp = get_method_res_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->high_signed ||
					mode == env->high_unsigned)
					++n_res;
			}
		}

		res = new_type_method(n_param, n_res);

		/* set param types and result types */
		for (i = n_param = 0; i < orig_n_params; ++i) {
			ir_type *tp = get_method_param_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->high_signed) {
					set_method_param_type(res, n_param++, tp_u);
					set_method_param_type(res, n_param++, tp_s);
				} else if (mode == env->high_unsigned) {
					set_method_param_type(res, n_param++, tp_u);
					set_method_param_type(res, n_param++, tp_u);
				} else {
					set_method_param_type(res, n_param++, tp);
				}
			} else {
				set_method_param_type(res, n_param++, tp);
			}
		}
		for (i = n_res = 0; i < orig_n_res; ++i) {
			ir_type *tp = get_method_res_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->high_signed) {
					set_method_res_type(res, n_res++, tp_u);
					set_method_res_type(res, n_res++, tp_s);
				} else if (mode == env->high_unsigned) {
					set_method_res_type(res, n_res++, tp_u);
					set_method_res_type(res, n_res++, tp_u);
				} else {
					set_method_res_type(res, n_res++, tp);
				}
			} else {
				set_method_res_type(res, n_res++, tp);
			}
		}
		set_lowered_type(mtp, res);
		pmap_insert(lowered_type, mtp, res);

		value_type = get_method_value_param_type(mtp);
		if (value_type != NULL) {
			/* this creates a new value parameter type */
			(void)get_method_value_param_ent(res, 0);

			/* set new param positions for all entities of the value type */
			for (i = n_param = 0; i < orig_n_params; ++i) {
				ir_type   *tp  = get_method_param_type(mtp, i);
				ident     *id  = get_method_param_ident(mtp, i);
				ir_entity *ent = get_method_value_param_ent(mtp, i);

				set_entity_arg_idx(ent, n_param);
				if (is_Primitive_type(tp)) {
					ir_mode *mode = get_type_mode(tp);

					if (mode == env->high_signed || mode == env->high_unsigned) {
						if (id != NULL) {
							lid = id_mangle(id, env->first_id);
							set_method_param_ident(res, n_param, lid);
							set_entity_ident(get_method_value_param_ent(res, n_param), lid);
							lid = id_mangle(id, env->next_id);
							set_method_param_ident(res, n_param + 1, lid);
							set_entity_ident(get_method_value_param_ent(res, n_param + 1), lid);
						}
						n_param += 2;
						continue;
					}
				}
				if (id != NULL) {
					set_method_param_ident(res, n_param, id);
					set_entity_ident(get_method_value_param_ent(res, n_param), id);
				}
				++n_param;
			}

			set_lowered_type(value_type, get_method_value_param_type(res));
		}
	} else {
		res = (ir_type*)entry->value;
	}
	return res;
}

/**
 * Translate a Return.
 */
static void lower_Return(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph  *irg = get_irn_irg(node);
	ir_entity *ent = get_irg_entity(irg);
	ir_type   *mtp = get_entity_type(ent);
	ir_node  **in;
	int        i, j, n;
	int        need_conv = 0;
	(void) mode;

	/* check if this return must be lowered */
	for (i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred = get_Return_res(node, i);
		ir_mode *mode = get_irn_op_mode(pred);

		if (mode == env->high_signed ||	mode == env->high_unsigned)
			need_conv = 1;
	}
	if (! need_conv)
		return;

	ent = get_irg_entity(irg);
	mtp = get_entity_type(ent);

	mtp = lower_mtp(env, mtp);
	set_entity_type(ent, mtp);

	/* create a new in array */
	NEW_ARR_A(ir_node *, in, get_method_n_ress(mtp) + 1);
	in[0] = get_Return_mem(node);

	for (j = i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred      = get_Return_res(node, i);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (pred_mode == env->high_signed || pred_mode == env->high_unsigned) {
			const node_entry_t *entry = get_node_entry(env, pred);
			in[++j] = entry->low_word;
			in[++j] = entry->high_word;
		} else {
			in[++j] = pred;
		}
	}

	set_irn_in(node, j+1, in);
}

/**
 * Translate the parameters.
 */
static void lower_Start(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_graph  *irg = get_irn_irg(node);
	ir_entity *ent = get_irg_entity(irg);
	ir_type   *tp  = get_entity_type(ent);
	ir_type   *mtp;
	long      *new_projs;
	int       i, j, n_params, rem;
	ir_node   *proj, *args;
	(void) mode;

	if (is_lowered_type(tp)) {
		mtp = get_associated_type(tp);
	} else {
		mtp = tp;
	}
	assert(! is_lowered_type(mtp));

	n_params = get_method_n_params(mtp);
	if (n_params <= 0)
		return;

	NEW_ARR_A(long, new_projs, n_params);

	/* first check if we have parameters that must be fixed */
	for (i = j = 0; i < n_params; ++i, ++j) {
		ir_type *tp = get_method_param_type(mtp, i);

		new_projs[i] = j;
		if (is_Primitive_type(tp)) {
			ir_mode *mode = get_type_mode(tp);

			if (mode == env->high_signed ||
				mode == env->high_unsigned)
				++j;
		}
	}
	if (i == j)
		return;

	mtp = lower_mtp(env, mtp);
	set_entity_type(ent, mtp);

	/* switch off optimization for new Proj nodes or they might be CSE'ed
	   with not patched one's */
	rem = get_optimize();
	set_optimize(0);

	/* ok, fix all Proj's and create new ones */
	args = get_irg_args(irg);
	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		ir_node *pred = get_Proj_pred(proj);
		long proj_nr;
		ir_mode *mode;
		ir_mode *mode_l;
		ir_mode *mode_h;
		ir_node *res_low;
		ir_node *res_high;
		dbg_info *dbg;

		/* do not visit this node again */
		mark_irn_visited(proj);

		if (pred != args)
			continue;

		proj_nr = get_Proj_proj(proj);
		set_Proj_proj(proj, new_projs[proj_nr]);

		mode = get_irn_mode(proj);
		mode_l = env->low_unsigned;
		if (mode == env->high_signed) {
			mode_h = env->low_signed;
		} else if (mode == env->high_unsigned) {
			mode_h = env->low_unsigned;
		} else {
			continue;
		}

		dbg = get_irn_dbg_info(proj);
		res_low  = new_rd_Proj(dbg, args, mode_l, new_projs[proj_nr]);
		res_high = new_rd_Proj(dbg, args, mode_h, new_projs[proj_nr] + 1);
		set_lowered(env, proj, res_low, res_high);
	}
	set_optimize(rem);
}

/**
 * Translate a Call.
 */
static void lower_Call(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_type  *tp = get_Call_type(node);
	ir_type  *call_tp;
	ir_node  **in, *proj, *results;
	int      n_params, n_res;
	bool     need_lower = false;
	int      i, j;
	long     *res_numbers = NULL;
	(void) mode;

	if (is_lowered_type(tp)) {
		call_tp = get_associated_type(tp);
	} else {
		call_tp = tp;
	}

	assert(! is_lowered_type(call_tp));

	n_params = get_method_n_params(call_tp);
	for (i = 0; i < n_params; ++i) {
		ir_type *tp = get_method_param_type(call_tp, i);

		if (is_Primitive_type(tp)) {
			ir_mode *mode = get_type_mode(tp);

			if (mode == env->high_signed || mode == env->high_unsigned) {
				need_lower = true;
				break;
			}
		}
	}
	n_res = get_method_n_ress(call_tp);
	if (n_res > 0) {
		NEW_ARR_A(long, res_numbers, n_res);

		for (i = j = 0; i < n_res; ++i, ++j) {
			ir_type *tp = get_method_res_type(call_tp, i);

			res_numbers[i] = j;
			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->high_signed || mode == env->high_unsigned) {
					need_lower = true;
					++j;
				}
			}
		}
	}

	if (! need_lower)
		return;

	/* let's lower it */
	call_tp = lower_mtp(env, call_tp);
	set_Call_type(node, call_tp);

	NEW_ARR_A(ir_node *, in, get_method_n_params(call_tp) + 2);

	in[0] = get_Call_mem(node);
	in[1] = get_Call_ptr(node);

	for (j = 2, i = 0; i < n_params; ++i) {
		ir_node *pred      = get_Call_param(node, i);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (pred_mode == env->high_signed || pred_mode == env->high_unsigned) {
			const node_entry_t *pred_entry = get_node_entry(env, pred);
			in[j++] = pred_entry->low_word;
			in[j++] = pred_entry->high_word;
		} else {
			in[j++] = pred;
		}
	}

	set_irn_in(node, j, in);

	/* fix the results */
	results = NULL;
	for (proj = (ir_node*)get_irn_link(node); proj;
	     proj = (ir_node*)get_irn_link(proj)) {
		long proj_nr = get_Proj_proj(proj);

		if (proj_nr == pn_Call_T_result && get_Proj_pred(proj) == node) {
			/* found the result proj */
			results = proj;
			break;
		}
	}

	if (results != NULL) {    /* there are results */
		int rem = get_optimize();

		/* switch off optimization for new Proj nodes or they might be CSE'ed
		   with not patched one's */
		set_optimize(0);
		for (i = j = 0, proj = (ir_node*)get_irn_link(results); proj;
		     proj = (ir_node*)get_irn_link(proj), ++i, ++j) {
			if (get_Proj_pred(proj) == results) {
				long      proj_nr   = get_Proj_proj(proj);
				ir_mode  *proj_mode = get_irn_mode(proj);
				ir_mode  *mode_l;
				ir_mode  *mode_h;
				ir_node  *res_low;
				ir_node  *res_high;
				dbg_info *dbg;

				/* found a result */
				mark_irn_visited(proj);

				set_Proj_proj(proj, res_numbers[proj_nr]);

				mode_l = env->low_unsigned;
				if (proj_mode == env->high_signed) {
					mode_h = env->low_signed;
				} else if (proj_mode == env->high_unsigned) {
					mode_h = env->low_unsigned;
				} else {
					continue;
				}

				dbg      = get_irn_dbg_info(proj);
				res_low  = new_rd_Proj(dbg, results, mode_l, res_numbers[proj_nr]);
				res_high = new_rd_Proj(dbg, results, mode_h, res_numbers[proj_nr] + 1);
				set_lowered(env, proj, res_low, res_high);
			}
		}
		set_optimize(rem);
	}
}

/**
 * Translate an Unknown into two.
 */
static void lower_Unknown(ir_node *node, ir_mode *mode, lower_env_t *env)
{
	ir_mode  *low_mode = env->low_unsigned;
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *res_low  = new_r_Unknown(irg, low_mode);
	ir_node  *res_high = new_r_Unknown(irg, mode);
	set_lowered(env, node, res_low, res_high);
}

/**
 * Translate a Phi.
 *
 * First step: just create two templates
 */
static void lower_Phi(lower_env_t *env, ir_node *phi)
{
	ir_mode  *mode = get_irn_mode(phi);
	int       i;
	int       arity;
	ir_node **in_l;
	ir_node **in_h;
	ir_node  *unk_l;
	ir_node  *unk_h;
	ir_node  *phi_l;
	ir_node  *phi_h;
	dbg_info *dbg;
	ir_node  *block;
	ir_graph *irg;
	ir_mode  *mode_l;
	ir_mode  *mode_h;

	/* enqueue predecessors */
	arity = get_Phi_n_preds(phi);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_Phi_pred(phi, i);
		pdeq_putr(env->waitq, pred);
	}

	if (mode != env->high_signed && mode != env->high_unsigned)
		return;

	/* first create a new in array */
	NEW_ARR_A(ir_node *, in_l, arity);
	NEW_ARR_A(ir_node *, in_h, arity);
	irg    = get_irn_irg(phi);
	mode_l = env->low_unsigned;
	mode_h = mode == env->high_signed ? env->low_signed : env->low_unsigned;
	unk_l  = new_r_Dummy(irg, mode_l);
	unk_h  = new_r_Dummy(irg, mode_h);
	for (i = 0; i < arity; ++i) {
		in_l[i] = unk_l;
		in_h[i] = unk_h;
	}

	dbg   = get_irn_dbg_info(phi);
	block = get_nodes_block(phi);
	phi_l = new_rd_Phi(dbg, block, arity, in_l, mode_l);
	phi_h = new_rd_Phi(dbg, block, arity, in_h, mode_h);

	set_lowered(env, phi, phi_l, phi_h);

	/* remember that we need to fixup the predecessors later */
	ARR_APP1(ir_node*, env->lowered_phis, phi);

	/* Don't forget to link the new Phi nodes into the block.
	 * Beware that some Phis might be optimized away. */
	if (is_Phi(phi_l))
		add_Block_phi(block, phi_l);
	if (is_Phi(phi_h))
		add_Block_phi(block, phi_h);
}

static void fixup_phi(lower_env_t *env, ir_node *phi)
{
	const node_entry_t *entry = get_node_entry(env, phi);
	ir_node            *phi_l = entry->low_word;
	ir_node            *phi_h = entry->high_word;
	int                 arity = get_Phi_n_preds(phi);
	int                 i;

	/* exchange phi predecessors which are lowered by now */
	for (i = 0; i < arity; ++i) {
		ir_node            *pred       = get_Phi_pred(phi, i);
		const node_entry_t *pred_entry = get_node_entry(env, pred);

		set_Phi_pred(phi_l, i, pred_entry->low_word);
		set_Phi_pred(phi_h, i, pred_entry->high_word);
	}
}

/**
 * Translate a Mux.
 */
static void lower_Mux(ir_node *mux, ir_mode *mode, lower_env_t *env)
{
	ir_node            *truen       = get_Mux_true(mux);
	ir_node            *falsen      = get_Mux_false(mux);
	ir_node            *sel         = get_Mux_sel(mux);
	const node_entry_t *true_entry  = get_node_entry(env, truen);
	const node_entry_t *false_entry = get_node_entry(env, falsen);
	ir_node            *true_l      = true_entry->low_word;
	ir_node            *true_h      = true_entry->high_word;
	ir_node            *false_l     = false_entry->low_word;
	ir_node            *false_h     = false_entry->high_word;
	dbg_info           *dbgi        = get_irn_dbg_info(mux);
	ir_node            *block       = get_nodes_block(mux);
	ir_node            *res_low
		= new_rd_Mux(dbgi, block, sel, false_l, true_l, env->low_unsigned);
	ir_node            *res_high
		= new_rd_Mux(dbgi, block, sel, false_h, true_h, mode);
	set_lowered(env, mux, res_low, res_high);
}

/**
 * Translate an ASM node.
 */
static void lower_ASM(ir_node *asmn, ir_mode *mode, lower_env_t *env)
{
	ir_mode *his = env->high_signed;
	ir_mode *hiu = env->high_unsigned;
	int      i;
	ir_node *n;

	(void)mode;

	for (i = get_irn_arity(asmn) - 1; i >= 0; --i) {
		ir_mode *op_mode = get_irn_mode(get_irn_n(asmn, i));
		if (op_mode == his || op_mode == hiu) {
			panic("lowering ASM unimplemented");
		}
	}

	for (n = asmn;;) {
		ir_mode *proj_mode;

		n = (ir_node*)get_irn_link(n);
		if (n == NULL)
			break;

		proj_mode = get_irn_mode(n);
		if (proj_mode == his || proj_mode == hiu) {
			panic("lowering ASM unimplemented");
		}
	}
}

/**
 * Translate a Sel node.
 */
static void lower_Sel(ir_node *sel, ir_mode *mode, lower_env_t *env)
{
	(void) mode;

	/* we must only lower value parameter Sels if we change the
	   value parameter type. */
	if (env->value_param_tp != NULL) {
		ir_entity *ent = get_Sel_entity(sel);
	    if (get_entity_owner(ent) == env->value_param_tp) {
			int pos = get_entity_arg_idx(ent);

			ent = get_method_value_param_ent(env->l_mtp, pos);
			set_Sel_entity(sel, ent);
		}
	}
}

/**
 * check for opcodes that must always be lowered.
 */
static bool always_lower(unsigned code)
{
	switch (code) {
	case iro_ASM:
	case iro_Proj:
	case iro_Start:
	case iro_Call:
	case iro_Return:
	case iro_Cond:
	case iro_Conv:
	case iro_Sel:
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
	const op_mode_entry_t *e1 = (const op_mode_entry_t*)elt;
	const op_mode_entry_t *e2 = (const op_mode_entry_t*)key;
	(void) size;

	return (e1->op != e2->op) | (e1->imode != e2->imode) | (e1->omode != e2->omode);
}

/**
 * Compare two conv_tp_entry_t's.
 */
static int cmp_conv_tp(const void *elt, const void *key, size_t size)
{
	const conv_tp_entry_t *e1 = (const conv_tp_entry_t*)elt;
	const conv_tp_entry_t *e2 = (const conv_tp_entry_t*)key;
	(void) size;

	return (e1->imode != e2->imode) | (e1->omode != e2->omode);
}

/**
 * Enter a lowering function into an ir_op.
 */
static void enter_lower_func(ir_op *op, lower_func func)
{
	op->ops.generic = (op_func)func;
}

/**
 * Returns non-zero if a method type must be lowered.
 *
 * @param mtp  the method type
 */
static bool mtp_must_be_lowered(lower_env_t *env, ir_type *mtp)
{
	int n_params = get_method_n_params(mtp);
	int i;

	/* first check if we have parameters that must be fixed */
	for (i = 0; i < n_params; ++i) {
		ir_type *tp = get_method_param_type(mtp, i);

		if (is_Primitive_type(tp)) {
			ir_mode *mode = get_type_mode(tp);

			if (mode == env->high_signed ||
				mode == env->high_unsigned)
				return true;
		}
	}
	return false;
}

/* Determine which modes need to be lowered */
static void setup_modes(lower_env_t *env)
{
	unsigned           size_bits           = env->params->doubleword_size;
	ir_mode           *doubleword_signed   = NULL;
	ir_mode           *doubleword_unsigned = NULL;
	size_t             n_modes             = get_irp_n_modes();
	ir_mode_arithmetic arithmetic;
	unsigned           modulo_shift;
	size_t             i;

	/* search for doubleword modes... */
	for (i = 0; i < n_modes; ++i) {
		ir_mode *mode = get_irp_mode(i);
		if (!mode_is_int(mode))
			continue;
		if (get_mode_size_bits(mode) != size_bits)
			continue;
		if (mode_is_signed(mode)) {
			if (doubleword_signed != NULL) {
				/* sigh - the lowerer should really just lower all mode with
				 * size_bits it finds. Unfortunately this required a bigger
				 * rewrite. */
				panic("multiple double word signed modes found");
			}
			doubleword_signed = mode;
		} else {
			if (doubleword_unsigned != NULL) {
				/* sigh - the lowerer should really just lower all mode with
				 * size_bits it finds. Unfortunately this required a bigger
				 * rewrite. */
				panic("multiple double word unsigned modes found");
			}
			doubleword_unsigned = mode;
		}
	}
	if (doubleword_signed == NULL || doubleword_unsigned == NULL) {
		panic("Couldn't find doubleword modes");
	}

	arithmetic   = get_mode_arithmetic(doubleword_signed);
	modulo_shift = get_mode_modulo_shift(doubleword_signed);

	assert(get_mode_size_bits(doubleword_unsigned) == size_bits);
	assert(size_bits % 2 == 0);
	assert(get_mode_sign(doubleword_signed) == 1);
	assert(get_mode_sign(doubleword_unsigned) == 0);
	assert(get_mode_sort(doubleword_signed) == irms_int_number);
	assert(get_mode_sort(doubleword_unsigned) == irms_int_number);
	assert(get_mode_arithmetic(doubleword_unsigned) == arithmetic);
	assert(get_mode_modulo_shift(doubleword_unsigned) == modulo_shift);

	/* try to guess a sensible modulo shift for the new mode.
	 * (This is IMO another indication that this should really be a node
	 *  attribute instead of a mode thing) */
	if (modulo_shift == size_bits) {
		modulo_shift = modulo_shift / 2;
	} else if (modulo_shift == 0) {
		/* fine */
	} else {
		panic("Don't know what new modulo shift to use for lowered doubleword mode");
	}
	size_bits /= 2;

	/* produce lowered modes */
	env->high_signed   = doubleword_signed;
	env->high_unsigned = doubleword_unsigned;
	env->low_signed    = new_ir_mode("WS", irms_int_number, size_bits, 1,
	                                 arithmetic, modulo_shift);
	env->low_unsigned  = new_ir_mode("WU", irms_int_number, size_bits, 0,
	                                 arithmetic, modulo_shift);
}

static void enqueue_preds(lower_env_t *env, ir_node *node)
{
	int arity = get_irn_arity(node);
	int i;

	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		pdeq_putr(env->waitq, pred);
	}
}

static void lower_node(lower_env_t *env, ir_node *node)
{
	int           arity;
	int           i;
	lower_func    func;
	ir_op        *op;
	ir_mode      *mode;
	unsigned      idx;
	node_entry_t *entry;

	if (irn_visited(node))
		return;
	mark_irn_visited(node);

	/* cycles are always broken at Phi and Block nodes. So we don't need special
	 * magic in all the other lower functions */
	if (is_Block(node)) {
		enqueue_preds(env, node);
		return;
	} else if (is_Phi(node)) {
		lower_Phi(env, node);
		return;
	}

	/* depth-first: descend into operands */
	if (!is_Block(node)) {
		ir_node *block = get_nodes_block(node);
		lower_node(env, block);
	}

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		lower_node(env, pred);
	}

	op   = get_irn_op(node);
	func = (lower_func) op->ops.generic;
	if (func == NULL)
		return;

	idx   = get_irn_idx(node);
	entry = idx < env->n_entries ? env->entries[idx] : NULL;
	if (entry != NULL || always_lower(get_irn_opcode(node))) {
		mode = get_irn_op_mode(node);
		if (mode == env->high_signed) {
			mode = env->low_signed;
		} else {
			mode = env->low_unsigned;
		}
		DB((dbg, LEVEL_1, "  %+F\n", node));
		func(node, mode, env);
	}
}

static void lower_irg(lower_env_t *env, ir_graph *irg)
{
	ir_entity *ent;
	ir_type   *mtp;
	unsigned   n_idx;

	obstack_init(&env->obst);

	n_idx = get_irg_last_idx(irg);
	n_idx = n_idx + (n_idx >> 2);  /* add 25% */
	env->n_entries = n_idx;
	env->entries   = NEW_ARR_F(node_entry_t*, n_idx);
	memset(env->entries, 0, sizeof(env->entries[0]) * n_idx);

	env->irg            = irg;
	env->l_mtp          = NULL;
	env->flags          = 0;
	env->proj_2_block   = pmap_create();
	env->value_param_tp = NULL;

	ent = get_irg_entity(irg);
	mtp = get_entity_type(ent);

	if (mtp_must_be_lowered(env, mtp)) {
		ir_type *ltp = lower_mtp(env, mtp);
		env->flags |= MUST_BE_LOWERED;
		set_entity_type(ent, ltp);
		env->l_mtp = ltp;
		env->value_param_tp = get_method_value_param_type(mtp);
	}

	/* first step: link all nodes and allocate data */
	ir_reserve_resources(irg, IR_RESOURCE_PHI_LIST | IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, firm_clear_node_and_phi_links,
	               prepare_links_and_handle_rotl, env);

	if (env->flags & MUST_BE_LOWERED) {
		size_t i;
		ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
		inc_irg_visited(irg);

		assert(pdeq_empty(env->waitq));
		pdeq_putr(env->waitq, get_irg_end(irg));

		env->lowered_phis = NEW_ARR_F(ir_node*, 0);
		while (!pdeq_empty(env->waitq)) {
			ir_node *node = (ir_node*)pdeq_getl(env->waitq);
			lower_node(env, node);
		}

		/* we need to fixup phis */
		for (i = 0; i < ARR_LEN(env->lowered_phis); ++i) {
			ir_node *phi = env->lowered_phis[i];
			fixup_phi(env, phi);
		}
		DEL_ARR_F(env->lowered_phis);


		ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

		/* outs are invalid, we changed the graph */
		set_irg_outs_inconsistent(irg);

		if (env->flags & CF_CHANGED) {
			/* control flow changed, dominance info is invalid */
			set_irg_doms_inconsistent(irg);
			set_irg_extblk_inconsistent(irg);
			set_irg_loopinfo_inconsistent(irg);
		}
	}

	ir_free_resources(irg, IR_RESOURCE_PHI_LIST | IR_RESOURCE_IRN_LINK);

	pmap_destroy(env->proj_2_block);
	DEL_ARR_F(env->entries);
	obstack_free(&env->obst, NULL);
}

/*
 * Do the lowering.
 */
void lower_dw_ops(const lwrdw_param_t *param)
{
	lower_env_t lenv;
	size_t      i, n;

	assert(param != NULL);
	FIRM_DBG_REGISTER(dbg, "firm.lower.dw");

	memset(&lenv, 0, sizeof(lenv));
	lenv.params = param;
	setup_modes(&lenv);

	/* create the necessary maps */
	if (! intrinsic_fkt)
		intrinsic_fkt = new_set(cmp_op_mode, iro_Last + 1);
	if (! conv_types)
		conv_types = new_set(cmp_conv_tp, 16);
	if (! lowered_type)
		lowered_type = pmap_create();

	/* create a primitive unsigned and signed type */
	if (! tp_u)
		tp_u = get_type_for_mode(lenv.low_unsigned);
	if (! tp_s)
		tp_s = get_type_for_mode(lenv.low_signed);

	/* create method types for the created binop calls */
	if (! binop_tp_u) {
		binop_tp_u = new_type_method(4, 2);
		set_method_param_type(binop_tp_u, 0, tp_u);
		set_method_param_type(binop_tp_u, 1, tp_u);
		set_method_param_type(binop_tp_u, 2, tp_u);
		set_method_param_type(binop_tp_u, 3, tp_u);
		set_method_res_type(binop_tp_u, 0, tp_u);
		set_method_res_type(binop_tp_u, 1, tp_u);
	}
	if (! binop_tp_s) {
		binop_tp_s = new_type_method(4, 2);
		set_method_param_type(binop_tp_s, 0, tp_u);
		set_method_param_type(binop_tp_s, 1, tp_s);
		set_method_param_type(binop_tp_s, 2, tp_u);
		set_method_param_type(binop_tp_s, 3, tp_s);
		set_method_res_type(binop_tp_s, 0, tp_u);
		set_method_res_type(binop_tp_s, 1, tp_s);
	}
	if (! shiftop_tp_u) {
		shiftop_tp_u = new_type_method(3, 2);
		set_method_param_type(shiftop_tp_u, 0, tp_u);
		set_method_param_type(shiftop_tp_u, 1, tp_u);
		set_method_param_type(shiftop_tp_u, 2, tp_u);
		set_method_res_type(shiftop_tp_u, 0, tp_u);
		set_method_res_type(shiftop_tp_u, 1, tp_u);
	}
	if (! shiftop_tp_s) {
		shiftop_tp_s = new_type_method(3, 2);
		set_method_param_type(shiftop_tp_s, 0, tp_u);
		set_method_param_type(shiftop_tp_s, 1, tp_s);
		set_method_param_type(shiftop_tp_s, 2, tp_u);
		set_method_res_type(shiftop_tp_s, 0, tp_u);
		set_method_res_type(shiftop_tp_s, 1, tp_s);
	}
	if (! unop_tp_u) {
		unop_tp_u = new_type_method(2, 2);
		set_method_param_type(unop_tp_u, 0, tp_u);
		set_method_param_type(unop_tp_u, 1, tp_u);
		set_method_res_type(unop_tp_u, 0, tp_u);
		set_method_res_type(unop_tp_u, 1, tp_u);
	}
	if (! unop_tp_s) {
		unop_tp_s = new_type_method(2, 2);
		set_method_param_type(unop_tp_s, 0, tp_u);
		set_method_param_type(unop_tp_s, 1, tp_s);
		set_method_res_type(unop_tp_s, 0, tp_u);
		set_method_res_type(unop_tp_s, 1, tp_s);
	}

	clear_irp_opcodes_generic_func();
	enter_lower_func(op_Add,     lower_binop);
	enter_lower_func(op_And,     lower_And);
	enter_lower_func(op_ASM,     lower_ASM);
	enter_lower_func(op_Call,    lower_Call);
	enter_lower_func(op_Cond,    lower_Cond);
	enter_lower_func(op_Const,   lower_Const);
	enter_lower_func(op_Conv,    lower_Conv);
	enter_lower_func(op_Div,     lower_Div);
	enter_lower_func(op_Eor,     lower_Eor);
	enter_lower_func(op_Load,    lower_Load);
	enter_lower_func(op_Minus,   lower_Unop);
	enter_lower_func(op_Mod,     lower_Mod);
	enter_lower_func(op_Mul,     lower_binop);
	enter_lower_func(op_Mux,     lower_Mux);
	enter_lower_func(op_Not,     lower_Not);
	enter_lower_func(op_Or,      lower_Or);
	enter_lower_func(op_Proj,    lower_Proj);
	enter_lower_func(op_Return,  lower_Return);
	enter_lower_func(op_Sel,     lower_Sel);
	enter_lower_func(op_Shl,     lower_Shl);
	enter_lower_func(op_Shr,     lower_Shr);
	enter_lower_func(op_Shrs,    lower_Shrs);
	enter_lower_func(op_Start,   lower_Start);
	enter_lower_func(op_Store,   lower_Store);
	enter_lower_func(op_Sub,     lower_binop);
	enter_lower_func(op_Unknown, lower_Unknown);

	lenv.tv_mode_bytes = new_tarval_from_long(param->doubleword_size/(2*8), lenv.low_unsigned);
	lenv.tv_mode_bits  = new_tarval_from_long(param->doubleword_size/2, lenv.low_unsigned);
	lenv.waitq         = new_pdeq();
	lenv.first_id      = new_id_from_chars(param->little_endian ? ".l" : ".h", 2);
	lenv.next_id       = new_id_from_chars(param->little_endian ? ".h" : ".l", 2);

	/* transform all graphs */
	for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);
		lower_irg(&lenv, irg);
	}
	del_pdeq(lenv.waitq);
}

/* Default implementation. */
ir_entity *def_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                    const ir_mode *imode, const ir_mode *omode,
                                    void *context)
{
	char buf[64];
	ident *id;
	ir_entity *ent;
	(void) context;

	if (imode == omode) {
		snprintf(buf, sizeof(buf), "__l%s%s", get_op_name(op), get_mode_name(imode));
	} else {
		snprintf(buf, sizeof(buf), "__l%s%s%s", get_op_name(op),
			get_mode_name(imode), get_mode_name(omode));
	}
	id = new_id_from_str(buf);

	ent = new_entity(get_glob_type(), id, method);
	set_entity_ld_ident(ent, get_entity_ident(ent));
	return ent;
}
