/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <assert.h>

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
#include "xmalloc.h"

/** A map from mode to a primitive type. */
static pmap *prim_types;

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
typedef struct _op_mode_entry {
	const ir_op   *op;    /**< the op */
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_entity     *ent;   /**< the associated entity of this (op, imode, omode) triple */
} op_mode_entry_t;

/**
 * An entry in the (imode, omode) -> tp map.
 */
typedef struct _conv_tp_entry {
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_type       *mtd;   /**< the associated method type of this (imode, omode) pair */
} conv_tp_entry_t;

/**
 * Every double word node will be replaced,
 * we need some store to hold the replacement:
 */
typedef struct _node_entry_t {
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
typedef struct _lower_env_t {
	node_entry_t **entries;       /**< entries per node */
	struct obstack obst;          /**< an obstack holding the temporary data */
	tarval   *tv_mode_bytes;      /**< a tarval containing the number of bytes in the lowered modes */
	tarval   *tv_mode_bits;       /**< a tarval containing the number of bits in the lowered modes */
	pdeq     *waitq;              /**< a wait queue of all nodes that must be handled later */
	pmap     *proj_2_block;       /**< a map from ProjX to its destination blocks */
	const lwrdw_param_t *params;  /**< transformation parameter */
	unsigned flags;               /**< some flags */
	int      n_entries;           /**< number of entries */
} lower_env_t;

/**
 * Get a primitive mode for a mode.
 */
static ir_type *get_primitive_type(ir_mode *mode) {
	pmap_entry *entry = pmap_find(prim_types, mode);
	ir_type *tp;
	char buf[64];

	if (entry)
		return entry->value;

	snprintf(buf, sizeof(buf), "_prim_%s", get_mode_name(mode));
	tp = new_type_primitive(new_id_from_str(buf), mode);

	pmap_insert(prim_types, mode, tp);
	return tp;
}  /* get_primitive_type */

/**
 * Create a method type for a Conv emulation from imode to omode.
 */
static ir_type *get_conv_type(ir_mode *imode, ir_mode *omode, lower_env_t *env) {
	conv_tp_entry_t key, *entry;
	ir_type *mtd;

	key.imode = imode;
	key.omode = omode;
	key.mtd   = NULL;

	entry = set_insert(conv_types, &key, sizeof(key), HASH_PTR(imode) ^ HASH_PTR(omode));
	if (! entry->mtd) {
		int n_param = 1, n_res = 1;
		char buf[64];

		if (imode == env->params->high_signed || imode == env->params->high_unsigned)
			n_param = 2;
		if (omode == env->params->high_signed || omode == env->params->high_unsigned)
			n_res = 2;

		/* create a new one */
		snprintf(buf, sizeof(buf), "LConv%s%s", get_mode_name(imode), get_mode_name(omode));
		mtd = new_type_method(new_id_from_str(buf), n_param, n_res);

		/* set param types and result types */
		n_param = 0;
		if (imode == env->params->high_signed) {
			set_method_param_type(mtd, n_param++, tp_u);
			set_method_param_type(mtd, n_param++, tp_s);
		} else if (imode == env->params->high_unsigned) {
			set_method_param_type(mtd, n_param++, tp_u);
			set_method_param_type(mtd, n_param++, tp_u);
		} else {
			ir_type *tp = get_primitive_type(imode);
			set_method_param_type(mtd, n_param++, tp);
		}  /* if */

		n_res = 0;
		if (omode == env->params->high_signed) {
			set_method_res_type(mtd, n_res++, tp_u);
			set_method_res_type(mtd, n_res++, tp_s);
		} else if (omode == env->params->high_unsigned) {
			set_method_res_type(mtd, n_res++, tp_u);
			set_method_res_type(mtd, n_res++, tp_u);
		} else {
			ir_type *tp = get_primitive_type(omode);
			set_method_res_type(mtd, n_res++, tp);
		}  /* if */
		entry->mtd = mtd;
	} else {
		mtd = entry->mtd;
	}  /* if */
	return mtd;
}  /* get_conv_type */

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

	for (phi = get_irn_link(block); phi; phi = get_irn_link(phi)) {
		for (i = 0; i < arity; ++i)
			in[i] = get_irn_n(phi, i);
		in[i] = in[nr];
		set_irn_in(phi, i + 1, in);
	}  /* for */
}  /* add_block_cf_input_nr */

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
		}  /* if */
	}  /* for */
	assert(i < arity);
	add_block_cf_input_nr(block, nr, cf);
}  /* add_block_cf_input */

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
	case iro_DivMod:
		return get_irn_mode(get_DivMod_left(node));
	case iro_Div:
		return get_irn_mode(get_Div_left(node));
	case iro_Mod:
		return get_irn_mode(get_Mod_left(node));
	case iro_Cmp:
		return get_irn_mode(get_Cmp_left(node));
	default:
		return get_irn_mode(node);
	}  /* switch */
}  /* get_irn_op_mode */

/**
 * Walker, prepare the node links.
 */
static void prepare_links(ir_node *node, void *env)
{
	lower_env_t  *lenv = env;
	ir_mode      *mode = get_irn_op_mode(node);
	node_entry_t *link;
	int          i;

	if (mode == lenv->params->high_signed ||
		mode == lenv->params->high_unsigned) {
		/* ok, found a node that will be lowered */
		link = obstack_alloc(&lenv->obst, sizeof(*link));

		memset(link, 0, sizeof(*link));

		lenv->entries[get_irn_idx(node)] = link;
		lenv->flags |= MUST_BE_LOWERED;
	} else if (get_irn_op(node) == op_Conv) {
		/* Conv nodes have two modes */
		ir_node *pred = get_Conv_op(node);
		mode = get_irn_mode(pred);

		if (mode == lenv->params->high_signed ||
			mode == lenv->params->high_unsigned) {
			/* must lower this node either but don't need a link */
			lenv->flags |= MUST_BE_LOWERED;
		}  /* if */
		return;
	}  /* if */

	if (is_Proj(node)) {
		/* link all Proj nodes to its predecessor:
		   Note that Tuple Proj's and its Projs are linked either. */
		ir_node *pred = get_Proj_pred(node);

		set_irn_link(node, get_irn_link(pred));
		set_irn_link(pred, node);
	} else if (is_Phi(node)) {
		/* link all Phi nodes to its block */
		ir_node *block = get_nodes_block(node);

		set_irn_link(node, get_irn_link(block));
		set_irn_link(block, node);
	} else if (is_Block(node)) {
		/* fill the Proj -> Block map */
		for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
			ir_node *pred = get_Block_cfgpred(node, i);

			if (is_Proj(pred))
				pmap_insert(lenv->proj_2_block, pred, node);
		}  /* for */
	}  /* if */
}  /* prepare_links */

/**
 * Translate a Constant: create two.
 */
static void lower_Const(ir_node *node, ir_mode *mode, lower_env_t *env) {
	tarval   *tv, *tv_l, *tv_h;
	ir_node  *low, *high;
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	int      idx;
	ir_graph *irg = current_ir_graph;
	ir_mode  *low_mode = env->params->low_unsigned;

	tv   = get_Const_tarval(node);

	tv_l = tarval_convert_to(tv, low_mode);
	low  = new_rd_Const(dbg, irg, block, low_mode, tv_l);

	tv_h = tarval_convert_to(tarval_shrs(tv, env->tv_mode_bits), mode);
	high = new_rd_Const(dbg, irg, block, mode, tv_h);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = low;
	env->entries[idx]->high_word = high;
}  /* lower_Const */

/**
 * Translate a Load: create two.
 */
static void lower_Load(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_mode  *low_mode = env->params->low_unsigned;
	ir_graph *irg = current_ir_graph;
	ir_node  *adr = get_Load_ptr(node);
	ir_node  *mem = get_Load_mem(node);
	ir_node  *low, *high, *proj;
	dbg_info *dbg;
	ir_node  *block = get_nodes_block(node);
	int      idx;

	if (env->params->little_endian) {
		low  = adr;
		high = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
	} else {
		low  = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
		high = adr;
	}  /* if */

	/* create two loads */
	dbg  = get_irn_dbg_info(node);
	low  = new_rd_Load(dbg, irg, block, mem,  low,  low_mode);
	proj = new_r_Proj(irg, block, low, mode_M, pn_Load_M);
	high = new_rd_Load(dbg, irg, block, proj, high, mode);

	set_Load_volatility(low,  get_Load_volatility(node));
	set_Load_volatility(high, get_Load_volatility(node));

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = low;
	env->entries[idx]->high_word = high;

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		idx = get_irn_idx(proj);

		switch (get_Proj_proj(proj)) {
		case pn_Load_M:         /* Memory result. */
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Load_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		case pn_Load_res:       /* Result of load operation. */
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(irg, block, low,  low_mode, pn_Load_res);
			env->entries[idx]->high_word = new_r_Proj(irg, block, high, mode,     pn_Load_res);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}  /* switch */
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}  /* for */
}  /* lower_Load */

/**
 * Translate a Store: create two.
 */
static void lower_Store(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg;
	ir_node  *block, *adr, *mem;
	ir_node  *low, *high, *irn, *proj;
	dbg_info *dbg;
	int      idx;
	node_entry_t *entry;
	(void) node;
	(void) mode;

	irn = get_Store_value(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	irg = current_ir_graph;
	adr = get_Store_ptr(node);
	mem = get_Store_mem(node);
	block = get_nodes_block(node);

	if (env->params->little_endian) {
		low  = adr;
		high = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
	} else {
		low  = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
		high = adr;
	}  /* if */

	/* create two Stores */
	dbg = get_irn_dbg_info(node);
	low  = new_rd_Store(dbg, irg, block, mem, low,  entry->low_word);
	proj = new_r_Proj(irg, block, low, mode_M, pn_Store_M);
	high = new_rd_Store(dbg, irg, block, proj, high, entry->high_word);

	set_Store_volatility(low,  get_Store_volatility(node));
	set_Store_volatility(high, get_Store_volatility(node));

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = low;
	env->entries[idx]->high_word = high;

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		idx = get_irn_idx(proj);

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
		}  /* switch */
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}  /* for */
}  /* lower_Store */

/**
 * Return a node containing the address of the intrinsic emulation function.
 *
 * @param method  the method type of the emulation function
 * @param op      the emulated ir_op
 * @param imode   the input mode of the emulated opcode
 * @param omode   the output mode of the emulated opcode
 * @param block   where the new mode is created
 * @param env     the lower environment
 */
static ir_node *get_intrinsic_address(ir_type *method, ir_op *op,
                                      ir_mode *imode, ir_mode *omode,
                                      ir_node *block, lower_env_t *env) {
	symconst_symbol sym;
	ir_entity *ent;
	op_mode_entry_t key, *entry;

	key.op    = op;
	key.imode = imode;
	key.omode = omode;
	key.ent   = NULL;

	entry = set_insert(intrinsic_fkt, &key, sizeof(key),
				HASH_PTR(op) ^ HASH_PTR(imode) ^ (HASH_PTR(omode) << 8));
	if (! entry->ent) {
		/* create a new one */
		ent = env->params->create_intrinsic(method, op, imode, omode, env->params->ctx);

		assert(ent && "Intrinsic creator must return an entity");
		entry->ent = ent;
	} else {
		ent = entry->ent;
	}  /* if */
	sym.entity_p = ent;
	return new_r_SymConst(current_ir_graph, block, sym, symconst_addr_ent);
}  /* get_intrinsic_address */

/**
 * Translate a Div.
 *
 * Create an intrinsic Call.
 */
static void lower_Div(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn, *call, *proj;
	ir_node  *in[4];
	ir_mode  *opmode;
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_Div_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_Div_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg   = current_ir_graph;

	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	opmode = get_irn_op_mode(node);
	irn = get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode, block, env);
	call = new_rd_Call(dbg, irg, block, get_Div_mem(node),
		irn, 4, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));
	irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Div_M:         /* Memory result. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M_except);
			break;
		case pn_Div_X_except:  /* Execution result if exception occurred. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Div_res:       /* Result of computation. */
			idx = get_irn_idx(proj);
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, irn, env->params->low_unsigned, 0);
			env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, irn, mode,                      1);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}  /* switch */
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}  /* for */
}  /* lower_Div */

/**
 * Translate a Mod.
 *
 * Create an intrinsic Call.
 */
static void lower_Mod(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *proj, *irn, *call;
	ir_node  *in[4];
	ir_mode  *opmode;
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_Mod_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_Mod_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg   = current_ir_graph;

	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	opmode = get_irn_op_mode(node);
	irn = get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode, block, env);
	call = new_rd_Call(dbg, irg, block, get_Mod_mem(node),
		irn, 4, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));
	irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Mod_M:         /* Memory result. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M_except);
			break;
		case pn_Mod_X_except:  /* Execution result if exception occurred. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Mod_res:       /* Result of computation. */
			idx = get_irn_idx(proj);
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, env->params->low_unsigned, 0);
			env->entries[idx]->high_word = new_r_Proj(irg, block, irn, mode,                      1);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}  /* switch */
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}  /* for */
}  /* lower_Mod */

/**
 * Translate a DivMod.
 *
 * Create two intrinsic Calls.
 */
static void lower_DivMod(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *proj, *irn, *mem, *callDiv, *callMod;
	ir_node  *resDiv = NULL;
	ir_node  *resMod = NULL;
	ir_node  *in[4];
	ir_mode  *opmode;
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;
	unsigned flags = 0;
	ir_graph *irg;

	/* check if both results are needed */
	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_DivMod_res_div: flags |= 1; break;
		case pn_DivMod_res_mod: flags |= 2; break;
		default: break;
		}  /* switch */
	}  /* for */

	irn   = get_DivMod_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_DivMod_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg   = current_ir_graph;

	mem = get_DivMod_mem(node);

	callDiv = callMod = NULL;
	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	if (flags & 1) {
		opmode = get_irn_op_mode(node);
		irn = get_intrinsic_address(mtp, op_Div, opmode, opmode, block, env);
		callDiv = new_rd_Call(dbg, irg, block, mem,
			irn, 4, in, mtp);
		set_irn_pinned(callDiv, get_irn_pinned(node));
		resDiv = new_r_Proj(irg, block, callDiv, mode_T, pn_Call_T_result);
	}  /* if */
	if (flags & 2) {
		if (flags & 1)
			mem = new_r_Proj(irg, block, callDiv, mode_M, pn_Call_M);
		opmode = get_irn_op_mode(node);
		irn = get_intrinsic_address(mtp, op_Mod, opmode, opmode, block, env);
		callMod = new_rd_Call(dbg, irg, block, mem,
			irn, 4, in, mtp);
		set_irn_pinned(callMod, get_irn_pinned(node));
		resMod = new_r_Proj(irg, block, callMod, mode_T, pn_Call_T_result);
	}  /* if */

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_DivMod_M:         /* Memory result. */
			/* reroute to the first call */
			set_Proj_pred(proj, callDiv ? callDiv : (callMod ? callMod : mem));
			set_Proj_proj(proj, pn_Call_M_except);
			break;
		case pn_DivMod_X_except:  /* Execution result if exception occurred. */
			/* reroute to the first call */
			set_Proj_pred(proj, callDiv ? callDiv : (callMod ? callMod : mem));
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_DivMod_res_div:   /* Result of Div. */
			idx = get_irn_idx(proj);
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(irg, block, resDiv, env->params->low_unsigned, 0);
			env->entries[idx]->high_word = new_r_Proj(irg, block, resDiv, mode,                      1);
			break;
		case pn_DivMod_res_mod:   /* Result of Mod. */
			idx = get_irn_idx(proj);
			env->entries[idx]->low_word  = new_r_Proj(irg, block, resMod, env->params->low_unsigned, 0);
			env->entries[idx]->high_word = new_r_Proj(irg, block, resMod, mode,                      1);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}  /* switch */
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}  /* for */
}  /* lower_DivMod */

/**
 * Translate a Binop.
 *
 * Create an intrinsic Call.
 */
static void lower_Binop(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *in[4];
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_binop_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_binop_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg   = current_ir_graph;

	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	irn = get_intrinsic_address(mtp, get_irn_op(node), mode, mode, block, env);
	irn = new_rd_Call(dbg, irg, block, get_irg_no_mem(current_ir_graph),
		irn, 4, in, mtp);
	set_irn_pinned(irn, get_irn_pinned(node));
	irn = new_r_Proj(irg, block, irn, mode_T, pn_Call_T_result);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, env->params->low_unsigned, 0);
	env->entries[idx]->high_word = new_r_Proj(irg, block, irn, mode,                      1);
}  /* lower_Binop */

/**
 * Translate a Shiftop.
 *
 * Create an intrinsic Call.
 */
static void lower_Shiftop(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *in[3];
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_binop_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	/* The shift count is always mode_Iu in firm, so there is no need for lowering */
	in[2] = get_binop_right(node);

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg  = current_ir_graph;

	mtp = mode_is_signed(mode) ? shiftop_tp_s : shiftop_tp_u;
	irn = get_intrinsic_address(mtp, get_irn_op(node), mode, mode, block, env);
	irn = new_rd_Call(dbg, irg, block, get_irg_no_mem(current_ir_graph),
		irn, 3, in, mtp);
	set_irn_pinned(irn, get_irn_pinned(node));
	irn = new_r_Proj(irg, block, irn, mode_T, pn_Call_T_result);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, env->params->low_unsigned, 0);
	env->entries[idx]->high_word = new_r_Proj(irg, block, irn, mode,                      1);
}  /* lower_Shiftop */

/**
 * Translate a Shr and handle special cases.
 */
static void lower_Shr(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *right = get_Shr_right(node);
	ir_graph *irg = current_ir_graph;

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) >= get_mode_size_bits(mode)) {
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Shr_left(node);
			ir_node *c;
			long shf_cnt = get_tarval_long(tv) - get_mode_size_bits(mode);
			int idx = get_irn_idx(left);

			left = env->entries[idx]->high_word;
			idx = get_irn_idx(node);

			if (shf_cnt > 0) {
				c = new_r_Const_long(irg, block, mode_Iu, shf_cnt);
				env->entries[idx]->low_word = new_r_Shr(irg, block, left, c, mode);
			} else {
				env->entries[idx]->low_word = left;
			}  /* if */
			env->entries[idx]->high_word = new_r_Const(irg, block, mode, get_mode_null(mode));

			return;
		}  /* if */
	}  /* if */
	lower_Shiftop(node, mode, env);
}  /* lower_Shr */

/**
 * Translate a Shl and handle special cases.
 */
static void lower_Shl(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *right = get_Shl_right(node);
	ir_graph *irg = current_ir_graph;

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) >= get_mode_size_bits(mode)) {
			ir_mode *mode_l;
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Shl_left(node);
			ir_node *c;
			long shf_cnt = get_tarval_long(tv) - get_mode_size_bits(mode);
			int idx = get_irn_idx(left);

			left = new_r_Conv(irg, block, env->entries[idx]->low_word, mode);
			idx = get_irn_idx(node);

			if (shf_cnt > 0) {
				c = new_r_Const_long(irg, block, mode_Iu, shf_cnt);
				env->entries[idx]->high_word = new_r_Shl(irg, block, left, c, mode);
			} else {
				env->entries[idx]->high_word = left;
			}  /* if */
			mode_l = env->params->low_unsigned;
			env->entries[idx]->low_word  = new_r_Const(irg, block, mode_l, get_mode_null(mode_l));

			return;
		}  /* if */
	}  /* if */
	lower_Shiftop(node, mode, env);
}  /* lower_Shl */

/**
 * Translate a Shrs and handle special cases.
 */
static void lower_Shrs(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *right = get_Shrs_right(node);
	ir_graph *irg = current_ir_graph;

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) >= get_mode_size_bits(mode)) {
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Shrs_left(node);
			long shf_cnt = get_tarval_long(tv) - get_mode_size_bits(mode);
			ir_node *c;
			int idx = get_irn_idx(left);

			left = env->entries[idx]->high_word;
			idx = get_irn_idx(node);

			if (shf_cnt > 0) {
				c = new_r_Const_long(irg, block, mode_Iu, shf_cnt);
				env->entries[idx]->low_word = new_r_Shrs(irg, block, left, c, mode);
			} else {
				env->entries[idx]->low_word = left;
			}  /* if */
			c = new_r_Const_long(irg, block, mode_Iu, get_mode_size_bits(mode) - 1);
			env->entries[idx]->high_word = new_r_Shrs(irg, block, left, c, mode);

			return;
		}  /* if */
	}  /* if */
	lower_Shiftop(node, mode, env);
}  /* lower_Shrs */

/**
 * Translate a Rot and handle special cases.
 */
static void lower_Rot(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *right = get_Rot_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) == get_mode_size_bits(mode)) {
			ir_node *left = get_Rot_left(node);
			ir_node *h, *l;
			int idx = get_irn_idx(left);

			l = env->entries[idx]->low_word;
			h = env->entries[idx]->high_word;
			idx = get_irn_idx(node);

			env->entries[idx]->low_word  = h;
			env->entries[idx]->high_word = l;

			return;
		}  /* if */
	}  /* if */
	lower_Shiftop(node, mode, env);
}  /* lower_Rot */

/**
 * Translate an Unop.
 *
 * Create an intrinsic Call.
 */
static void lower_Unop(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *in[2];
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_unop_op(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg   = current_ir_graph;

	mtp = mode_is_signed(mode) ? unop_tp_s : unop_tp_u;
	irn = get_intrinsic_address(mtp, get_irn_op(node), mode, mode, block, env);
	irn = new_rd_Call(dbg, irg, block, get_irg_no_mem(current_ir_graph),
		irn, 2, in, mtp);
	set_irn_pinned(irn, get_irn_pinned(node));
	irn = new_r_Proj(irg, block, irn, mode_T, pn_Call_T_result);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, env->params->low_unsigned, 0);
	env->entries[idx]->high_word = new_r_Proj(irg, block, irn, mode,                      1);
}  /* lower_Unop */

/**
 * Translate a logical Binop.
 *
 * Create two logical Binops.
 */
static void lower_Binop_logical(ir_node *node, ir_mode *mode, lower_env_t *env,
								ir_node *(*constr_rd)(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) ) {
	ir_node  *block, *irn;
	ir_node  *lop_l, *lop_h, *rop_l, *rop_h;
	dbg_info *dbg;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_binop_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	lop_l = entry->low_word;
	lop_h = entry->high_word;

	irn   = get_binop_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	rop_l = entry->low_word;
	rop_h = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	irg = current_ir_graph;
	env->entries[idx]->low_word  = constr_rd(dbg, irg, block, lop_l, rop_l, env->params->low_unsigned);
	env->entries[idx]->high_word = constr_rd(dbg, irg, block, lop_h, rop_h, mode);
}  /* lower_Binop_logical */

/** create a logical operation tranformation */
#define lower_logical(op)                                                \
static void lower_##op(ir_node *node, ir_mode *mode, lower_env_t *env) { \
	lower_Binop_logical(node, mode, env, new_rd_##op);                   \
}

lower_logical(And)
lower_logical(Or)
lower_logical(Eor)

/**
 * Translate a Not.
 *
 * Create two logical Nots.
 */
static void lower_Not(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *op_l, *op_h;
	dbg_info *dbg;
	int      idx;
	ir_graph *irg;
	node_entry_t *entry;

	irn   = get_Not_op(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	op_l = entry->low_word;
	op_h = entry->high_word;

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	irg   = current_ir_graph;

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_rd_Not(dbg, current_ir_graph, block, op_l, env->params->low_unsigned);
	env->entries[idx]->high_word = new_rd_Not(dbg, current_ir_graph, block, op_h, mode);
}  /* lower_Not */

/**
 * Translate a Cond.
 */
static void lower_Cond(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node *cmp, *left, *right, *block;
	ir_node *sel = get_Cond_selector(node);
	ir_mode *m = get_irn_mode(sel);
	int     idx;
	(void) mode;

	if (m == mode_b) {
		node_entry_t *lentry, *rentry;
		ir_node  *proj, *projT = NULL, *projF = NULL;
		ir_node  *new_bl, *cmpH, *cmpL, *irn;
		ir_node  *projHF, *projHT;
		ir_node  *dst_blk;
		ir_graph *irg;
		pn_Cmp   pnc;
		dbg_info *dbg;

		if(!is_Proj(sel))
			return;

		cmp   = get_Proj_pred(sel);
		if(!is_Cmp(cmp))
			return;

		left  = get_Cmp_left(cmp);
		idx   = get_irn_idx(left);
		lentry = env->entries[idx];

		if (! lentry) {
			/* a normal Cmp */
			return;
		}  /* if */

		right = get_Cmp_right(cmp);
		idx   = get_irn_idx(right);
		rentry = env->entries[idx];
		assert(rentry);

		if (! lentry->low_word || !rentry->low_word) {
			/* not yet ready */
			pdeq_putr(env->waitq, node);
			return;
		}  /* if */

		/* all right, build the code */
		for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
			long proj_nr = get_Proj_proj(proj);

			if (proj_nr == pn_Cond_true) {
				assert(projT == NULL && "more than one Proj(true)");
				projT = proj;
			} else {
				assert(proj_nr == pn_Cond_false);
				assert(projF == NULL && "more than one Proj(false)");
				projF = proj;
			}  /* if */
			mark_irn_visited(proj);
		}  /* for */
		assert(projT && projF);

		/* create a new high compare */
		block = get_nodes_block(cmp);
		dbg   = get_irn_dbg_info(cmp);
		irg   = current_ir_graph;

		cmpH = new_rd_Cmp(dbg, irg, block, lentry->high_word, rentry->high_word);

		pnc = get_Proj_proj(sel);
		if (pnc == pn_Cmp_Eq) {
			/* simple case:a == b <==> a_h == b_h && a_l == b_l */
			pmap_entry *entry = pmap_find(env->proj_2_block, projF);

			assert(entry);
			dst_blk = entry->value;

			irn = new_r_Proj(irg, block, cmpH, mode_b, pn_Cmp_Eq);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, block, irn);

			projHF = new_r_Proj(irg, block, irn, mode_X, pn_Cond_false);
			mark_irn_visited(projHF);
			exchange(projF, projHF);

			projHT = new_r_Proj(irg, block, irn, mode_X, pn_Cond_true);
			mark_irn_visited(projHT);

			new_bl = new_r_Block(irg, 1, &projHT);

			dbg   = get_irn_dbg_info(cmp);
			cmpL = new_rd_Cmp(dbg, irg, new_bl, lentry->low_word, rentry->low_word);
			irn = new_r_Proj(irg, new_bl, cmpL, mode_b, pn_Cmp_Eq);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, new_bl, irn);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			add_block_cf_input(dst_blk, projHF, proj);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);
			exchange(projT, proj);
		} else if (pnc == pn_Cmp_Lg) {
			/* simple case:a != b <==> a_h != b_h || a_l != b_l */
			pmap_entry *entry = pmap_find(env->proj_2_block, projT);

			assert(entry);
			dst_blk = entry->value;

			irn = new_r_Proj(irg, block, cmpH, mode_b, pn_Cmp_Lg);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, block, irn);

			projHT = new_r_Proj(irg, block, irn, mode_X, pn_Cond_true);
			mark_irn_visited(projHT);
			exchange(projT, projHT);

			projHF = new_r_Proj(irg, block, irn, mode_X, pn_Cond_false);
			mark_irn_visited(projHF);

			new_bl = new_r_Block(irg, 1, &projHF);

			dbg   = get_irn_dbg_info(cmp);
			cmpL = new_rd_Cmp(dbg, irg, new_bl, lentry->low_word, rentry->low_word);
			irn = new_r_Proj(irg, new_bl, cmpL, mode_b, pn_Cmp_Lg);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, new_bl, irn);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);
			add_block_cf_input(dst_blk, projHT, proj);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			exchange(projF, proj);
		} else {
			/* a rel b <==> a_h REL b_h || (a_h == b_h && a_l rel b_l) */
			ir_node *dstT, *dstF, *newbl_eq, *newbl_l;
			pmap_entry *entry;

			entry = pmap_find(env->proj_2_block, projT);
			assert(entry);
			dstT = entry->value;

			entry = pmap_find(env->proj_2_block, projF);
			assert(entry);
			dstF = entry->value;

			irn = new_r_Proj(irg, block, cmpH, mode_b, pnc & ~pn_Cmp_Eq);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, block, irn);

			projHT = new_r_Proj(irg, block, irn, mode_X, pn_Cond_true);
			mark_irn_visited(projHT);
			exchange(projT, projHT);
			projT = projHT;

			projHF = new_r_Proj(irg, block, irn, mode_X, pn_Cond_false);
			mark_irn_visited(projHF);

			newbl_eq = new_r_Block(irg, 1, &projHF);

			irn = new_r_Proj(irg, newbl_eq, cmpH, mode_b, pn_Cmp_Eq);
			irn = new_rd_Cond(dbg, irg, newbl_eq, irn);

			proj = new_r_Proj(irg, newbl_eq, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			exchange(projF, proj);
			projF = proj;

			proj = new_r_Proj(irg, newbl_eq, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);

			newbl_l = new_r_Block(irg, 1, &proj);

			dbg   = get_irn_dbg_info(cmp);
			cmpL = new_rd_Cmp(dbg, irg, newbl_l, lentry->low_word, rentry->low_word);
			irn = new_r_Proj(irg, newbl_l, cmpL, mode_b, pnc);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, newbl_l, irn);

			proj = new_r_Proj(irg, newbl_l, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);
			add_block_cf_input(dstT, projT, proj);

			proj = new_r_Proj(irg, newbl_l, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			add_block_cf_input(dstF, projF, proj);
		}  /* if */

		/* we have changed the control flow */
		env->flags |= CF_CHANGED;
	} else {
		idx = get_irn_idx(sel);

		if (env->entries[idx]) {
			/*
			   Bad, a jump-table with double-word index.
			   This should not happen, but if it does we handle
			   it like a Conv were between (in other words, ignore
			   the high part.
			 */

			if (! env->entries[idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}  /* if */
			set_Cond_selector(node, env->entries[idx]->low_word);
		}  /* if */
	}  /* if */
}  /* lower_Cond */

/**
 * Translate a Conv to higher_signed
 */
static void lower_Conv_to_Ls(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *imode = get_irn_mode(op);
	ir_mode  *dst_mode_l = env->params->low_unsigned;
	ir_mode  *dst_mode_h = env->params->low_signed;
	int      idx = get_irn_idx(node);
	ir_graph *irg = current_ir_graph;
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	assert(idx < env->n_entries);

	if (mode_is_int(imode) || mode_is_reference(imode)) {
		if (imode == env->params->high_unsigned) {
			/* a Conv from Lu to Ls */
			int op_idx = get_irn_idx(op);

			if (! env->entries[op_idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}  /* if */
			env->entries[idx]->low_word  = new_rd_Conv(dbg, irg, block, env->entries[op_idx]->low_word,  dst_mode_l);
			env->entries[idx]->high_word = new_rd_Conv(dbg, irg, block, env->entries[op_idx]->high_word, dst_mode_h);
		} else {
			/* simple case: create a high word */
			if (imode != dst_mode_l)
				op = new_rd_Conv(dbg, irg, block, op, dst_mode_l);

			env->entries[idx]->low_word  = op;

			if (mode_is_signed(imode)) {
				ir_node *op_conv = new_rd_Conv(dbg, irg, block, op, dst_mode_h);
				env->entries[idx]->high_word = new_rd_Shrs(dbg, irg, block, op_conv,
					new_Const_long(mode_Iu, get_mode_size_bits(dst_mode_h) - 1), dst_mode_h);
			} else {
				env->entries[idx]->high_word = new_Const(dst_mode_h, get_mode_null(dst_mode_h));
			}  /* if */
		}  /* if */
	} else {
		ir_node *irn, *call;
		ir_mode *omode = env->params->high_signed;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 1, &op, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, dst_mode_l, 0);
		env->entries[idx]->high_word = new_r_Proj(irg, block, irn, dst_mode_h, 1);
	}  /* if */
}  /* lower_Conv_to_Ls */

/**
 * Translate a Conv to higher_unsigned
 */
static void lower_Conv_to_Lu(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *imode = get_irn_mode(op);
	ir_mode  *dst_mode = env->params->low_unsigned;
	int      idx = get_irn_idx(node);
	ir_graph *irg = current_ir_graph;
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	assert(idx < env->n_entries);

	if (mode_is_int(imode) || mode_is_reference(imode)) {
		if (imode == env->params->high_signed) {
			/* a Conv from Ls to Lu */
			int op_idx = get_irn_idx(op);

			if (! env->entries[op_idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}  /* if */
			env->entries[idx]->low_word  = new_rd_Conv(dbg, irg, block, env->entries[op_idx]->low_word, dst_mode);
			env->entries[idx]->high_word = new_rd_Conv(dbg, irg, block, env->entries[op_idx]->high_word, dst_mode);
		} else {
			/* simple case: create a high word */
			if (imode != dst_mode)
				op = new_rd_Conv(dbg, irg, block, op, dst_mode);

			env->entries[idx]->low_word  = op;

			if (mode_is_signed(imode)) {
				env->entries[idx]->high_word = new_rd_Shrs(dbg, irg, block, op,
					new_Const_long(mode_Iu, get_mode_size_bits(dst_mode) - 1), dst_mode);
			} else {
				env->entries[idx]->high_word = new_Const(dst_mode, get_mode_null(dst_mode));
			}  /* if */
		}  /* if */
	} else {
		ir_node *irn, *call;
		ir_mode *omode = env->params->high_unsigned;
		ir_type *mtp = get_conv_type(imode, omode, env);

		/* do an intrinsic call */
		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 1, &op, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, dst_mode, 0);
		env->entries[idx]->high_word = new_r_Proj(irg, block, irn, dst_mode, 1);
	}  /* if */
}  /* lower_Conv_to_Lu */

/**
 * Translate a Conv from higher_signed
 */
static void lower_Conv_from_Ls(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *omode = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	int      idx = get_irn_idx(op);
	ir_graph *irg = current_ir_graph;

	assert(idx < env->n_entries);

	if (! env->entries[idx]->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	if (mode_is_int(omode) || mode_is_reference(omode)) {
		op = env->entries[idx]->low_word;

		/* simple case: create a high word */
		if (omode != env->params->low_signed)
			op = new_rd_Conv(dbg, irg, block, op, omode);

		set_Conv_op(node, op);
	} else {
		ir_node *irn, *call, *in[2];
		ir_mode *imode = env->params->high_signed;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		in[0] = env->entries[idx]->low_word;
		in[1] = env->entries[idx]->high_word;

		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 2, in, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		exchange(node, new_r_Proj(irg, block, irn, omode, 0));
	}  /* if */
}  /* lower_Conv_from_Ls */

/**
 * Translate a Conv from higher_unsigned
 */
static void lower_Conv_from_Lu(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *omode = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	int      idx = get_irn_idx(op);
	ir_graph *irg = current_ir_graph;

	assert(idx < env->n_entries);

	if (! env->entries[idx]->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}  /* if */

	if (mode_is_int(omode) || mode_is_reference(omode)) {
		op = env->entries[idx]->low_word;

		/* simple case: create a high word */
		if (omode != env->params->low_unsigned)
			op = new_rd_Conv(dbg, irg, block, op, omode);

		set_Conv_op(node, op);
	} else {
		ir_node *irn, *call, *in[2];
		ir_mode *imode = env->params->high_unsigned;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		in[0] = env->entries[idx]->low_word;
		in[1] = env->entries[idx]->high_word;

		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 2, in, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		exchange(node, new_r_Proj(irg, block, irn, omode, 0));
	}  /* if */
}  /* lower_Conv_from_Lu */

/**
 * Translate a Conv.
 */
static void lower_Conv(ir_node *node, ir_mode *mode, lower_env_t *env) {
	mode = get_irn_mode(node);

	if (mode == env->params->high_signed) {
		lower_Conv_to_Ls(node, env);
	} else if (mode == env->params->high_unsigned) {
		lower_Conv_to_Lu(node, env);
	} else {
		ir_mode *mode = get_irn_mode(get_Conv_op(node));

		if (mode == env->params->high_signed) {
			lower_Conv_from_Ls(node, env);
		} else if (mode == env->params->high_unsigned) {
			lower_Conv_from_Lu(node, env);
		}  /* if */
	}  /* if */
}  /* lower_Conv */

/**
 * Lower the method type.
 */
static ir_type *lower_mtp(ir_type *mtp, lower_env_t *env) {
	pmap_entry *entry;
	ident      *id;
	ir_type    *res;

	if (is_lowered_type(mtp))
		return mtp;

	entry = pmap_find(lowered_type, mtp);
	if (! entry) {
		int i, n, r, n_param, n_res;

		/* count new number of params */
		n_param = n = get_method_n_params(mtp);
		for (i = n_param - 1; i >= 0; --i) {
			ir_type *tp = get_method_param_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed ||
					mode == env->params->high_unsigned)
					++n_param;
			}  /* if */
		}  /* for */

		/* count new number of results */
		n_res = r = get_method_n_ress(mtp);
		for (i = n_res - 1; i >= 0; --i) {
			ir_type *tp = get_method_res_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed ||
					mode == env->params->high_unsigned)
					++n_res;
			}  /* if */
		}  /* for */

		id = mangle_u(new_id_from_chars("L", 1), get_type_ident(mtp));
		res = new_type_method(id, n_param, n_res);

		/* set param types and result types */
		for (i = n_param = 0; i < n; ++i) {
			ir_type *tp = get_method_param_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed) {
					set_method_param_type(res, n_param++, tp_u);
					set_method_param_type(res, n_param++, tp_s);
				} else if (mode == env->params->high_unsigned) {
					set_method_param_type(res, n_param++, tp_u);
					set_method_param_type(res, n_param++, tp_u);
				} else {
					set_method_param_type(res, n_param++, tp);
				}  /* if */
			} else {
				set_method_param_type(res, n_param++, tp);
			}  /* if */
		}  /* for */
		for (i = n_res = 0; i < r; ++i) {
			ir_type *tp = get_method_res_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed) {
					set_method_res_type(res, n_res++, tp_u);
					set_method_res_type(res, n_res++, tp_s);
				} else if (mode == env->params->high_unsigned) {
					set_method_res_type(res, n_res++, tp_u);
					set_method_res_type(res, n_res++, tp_u);
				} else {
					set_method_res_type(res, n_res++, tp);
				}  /* if */
			} else {
				set_method_res_type(res, n_res++, tp);
			}  /* if */
		}  /* for */
		set_lowered_type(mtp, res);
		pmap_insert(lowered_type, mtp, res);
	} else {
		res = entry->value;
	}  /* if */
	return res;
}  /* lower_mtp */

/**
 * Translate a Return.
 */
static void lower_Return(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph  *irg = current_ir_graph;
	ir_entity *ent = get_irg_entity(irg);
	ir_type   *mtp = get_entity_type(ent);
	ir_node   **in;
	int       i, j, n, idx;
	int       need_conv = 0;
	(void) mode;

	/* check if this return must be lowered */
	for (i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred = get_Return_res(node, i);
		ir_mode *mode = get_irn_op_mode(pred);

		if (mode == env->params->high_signed ||
			mode == env->params->high_unsigned) {
			idx = get_irn_idx(pred);
			if (! env->entries[idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}  /* if */
			need_conv = 1;
		}  /* if */
	}  /* for */
	if (! need_conv)
		return;

	ent = get_irg_entity(irg);
	mtp = get_entity_type(ent);

	mtp = lower_mtp(mtp, env);
	set_entity_type(ent, mtp);

	/* create a new in array */
	NEW_ARR_A(ir_node *, in, get_method_n_ress(mtp) + 1);
	in[0] = get_Return_mem(node);

	for (j = i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred = get_Return_res(node, i);

		idx = get_irn_idx(pred);
		assert(idx < env->n_entries);

		if (env->entries[idx]) {
			in[++j] = env->entries[idx]->low_word;
			in[++j] = env->entries[idx]->high_word;
		} else {
			in[++j] = pred;
		}  /* if */
	}  /* for */

	set_irn_in(node, j+1, in);
}  /* lower_Return */

/**
 * Translate the parameters.
 */
static void lower_Start(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph  *irg = current_ir_graph;
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
	}  /* if */
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

			if (mode == env->params->high_signed ||
				mode == env->params->high_unsigned)
				++j;
		}  /* if */
	}  /* for */
	if (i == j)
		return;

	mtp = lower_mtp(mtp, env);
	set_entity_type(ent, mtp);

	/* switch off optimization for new Proj nodes or they might be CSE'ed
	   with not patched one's */
	rem = get_optimize();
	set_optimize(0);

	/* ok, fix all Proj's and create new ones */
	args = get_irg_args(irg);
	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		ir_node *pred = get_Proj_pred(proj);
		long proj_nr;
		int idx;
		ir_mode *mode;
		dbg_info *dbg;

		/* do not visit this node again */
		mark_irn_visited(proj);

		if (pred != args)
			continue;

		proj_nr = get_Proj_proj(proj);
		set_Proj_proj(proj, new_projs[proj_nr]);

		idx = get_irn_idx(proj);
		if (env->entries[idx]) {
			ir_mode *low_mode = env->params->low_unsigned;

			mode = get_irn_mode(proj);

			if (mode == env->params->high_signed) {
				mode = env->params->low_signed;
			} else {
				mode = env->params->low_unsigned;
			}  /* if */

			dbg = get_irn_dbg_info(proj);
			env->entries[idx]->low_word  =
				new_rd_Proj(dbg, irg, get_nodes_block(proj), args, low_mode, new_projs[proj_nr]);
			env->entries[idx]->high_word =
				new_rd_Proj(dbg, irg, get_nodes_block(proj), args, mode, new_projs[proj_nr] + 1);
		}  /* if */
	}  /* for */
	set_optimize(rem);
}  /* lower_Start */

/**
 * Translate a Call.
 */
static void lower_Call(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg = current_ir_graph;
	ir_type  *tp = get_Call_type(node);
	ir_type  *call_tp;
	ir_node  **in, *proj, *results;
	int      n_params, n_res, need_lower = 0;
	int      i, j;
	long     *res_numbers = NULL;
	(void) mode;

	if (is_lowered_type(tp)) {
		call_tp = get_associated_type(tp);
	} else {
		call_tp = tp;
	}  /* if */

	assert(! is_lowered_type(call_tp));

	n_params = get_method_n_params(call_tp);
	for (i = 0; i < n_params; ++i) {
		ir_type *tp = get_method_param_type(call_tp, i);

		if (is_Primitive_type(tp)) {
			ir_mode *mode = get_type_mode(tp);

			if (mode == env->params->high_signed ||
				mode == env->params->high_unsigned) {
				need_lower = 1;
				break;
			}  /* if */
		}  /* if */
	}  /* for */
	n_res = get_method_n_ress(call_tp);
	if (n_res > 0) {
		NEW_ARR_A(long, res_numbers, n_res);

		for (i = j = 0; i < n_res; ++i, ++j) {
			ir_type *tp = get_method_res_type(call_tp, i);

			res_numbers[i] = j;
			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed ||
					mode == env->params->high_unsigned) {
					need_lower = 1;
					++j;
				}  /* if */
			}  /* if */
		}  /* for */
	}  /* if */

	if (! need_lower)
		return;

	/* let's lower it */
	call_tp = lower_mtp(call_tp, env);
	set_Call_type(node, call_tp);

	NEW_ARR_A(ir_node *, in, get_method_n_params(call_tp) + 2);

	in[0] = get_Call_mem(node);
	in[1] = get_Call_ptr(node);

	for (j = 2, i = 0; i < n_params; ++i) {
		ir_node *pred = get_Call_param(node, i);
		int     idx = get_irn_idx(pred);

		if (env->entries[idx]) {
			if (! env->entries[idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}
			in[j++] = env->entries[idx]->low_word;
			in[j++] = env->entries[idx]->high_word;
		} else {
			in[j++] = pred;
		}  /* if */
	}  /* for */

	set_irn_in(node, j, in);

	/* fix the results */
	results = NULL;
	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		long proj_nr = get_Proj_proj(proj);

		if (proj_nr == pn_Call_T_result && get_Proj_pred(proj) == node) {
			/* found the result proj */
			results = proj;
			break;
		}  /* if */
	}  /* for */

	if (results) {		/* there are results */
		int rem = get_optimize();

		/* switch off optimization for new Proj nodes or they might be CSE'ed
		   with not patched one's */
		set_optimize(0);
		for (i = j = 0, proj = get_irn_link(results); proj; proj = get_irn_link(proj), ++i, ++j) {
			if (get_Proj_pred(proj) == results) {
				long proj_nr = get_Proj_proj(proj);
				int idx;

				/* found a result */
				set_Proj_proj(proj, res_numbers[proj_nr]);
				idx = get_irn_idx(proj);
				if (env->entries[idx]) {
					ir_mode *mode = get_irn_mode(proj);
					ir_mode *low_mode = env->params->low_unsigned;
					dbg_info *dbg;

					if (mode == env->params->high_signed) {
						mode = env->params->low_signed;
					} else {
						mode = env->params->low_unsigned;
					}  /* if */

					dbg = get_irn_dbg_info(proj);
					env->entries[idx]->low_word  =
						new_rd_Proj(dbg, irg, get_nodes_block(proj), results, low_mode, res_numbers[proj_nr]);
					env->entries[idx]->high_word =
						new_rd_Proj(dbg, irg, get_nodes_block(proj), results, mode, res_numbers[proj_nr] + 1);
				}  /* if */
				mark_irn_visited(proj);
			}  /* if */
		}  /* for */
		set_optimize(rem);
	}
}  /* lower_Call */

/**
 * Translate an Unknown into two.
 */
static void lower_Unknown(ir_node *node, ir_mode *mode, lower_env_t *env) {
	int idx = get_irn_idx(node);
	ir_graph *irg = current_ir_graph;

	env->entries[idx]->low_word  =
	env->entries[idx]->high_word = new_r_Unknown(irg, mode);
}  /* lower_Unknown */

/**
 * Translate a Phi.
 *
 * First step: just create two templates
 */
static void lower_Phi(ir_node *phi, ir_mode *mode, lower_env_t *env) {
	ir_mode  *mode_l = env->params->low_unsigned;
	ir_graph *irg = current_ir_graph;
	ir_node  *block;
	ir_node  *unk_l;
	ir_node  *unk_h;
	ir_node  **inl, **inh;
	dbg_info *dbg;
	int      idx, i, arity = get_Phi_n_preds(phi);
	int      enq = 0;

	idx = get_irn_idx(phi);
	if (env->entries[idx]->low_word) {
		/* Phi nodes already build, check for inputs */
		ir_node *phil = env->entries[idx]->low_word;
		ir_node *phih = env->entries[idx]->high_word;

		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_Phi_pred(phi, i);
			int     idx = get_irn_idx(pred);

			if (env->entries[idx]->low_word) {
				set_Phi_pred(phil, i, env->entries[idx]->low_word);
				set_Phi_pred(phih, i, env->entries[idx]->high_word);
			} else {
				/* still not ready */
				pdeq_putr(env->waitq, phi);
				return;
			}  /* if */
		}  /* for */
	}  /* if */

	/* first create a new in array */
	NEW_ARR_A(ir_node *, inl, arity);
	NEW_ARR_A(ir_node *, inh, arity);
	unk_l = new_r_Unknown(irg, mode_l);
	unk_h = new_r_Unknown(irg, mode);

	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_Phi_pred(phi, i);
		int     idx = get_irn_idx(pred);

		if (env->entries[idx]->low_word) {
			inl[i] = env->entries[idx]->low_word;
			inh[i] = env->entries[idx]->high_word;
		} else {
			inl[i] = unk_l;
			inh[i] = unk_h;
			enq = 1;
		}  /* if */
	}  /* for */

	dbg   = get_irn_dbg_info(phi);
	block = get_nodes_block(phi);

	idx = get_irn_idx(phi);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_rd_Phi(dbg, irg, block, arity, inl, mode_l);
	env->entries[idx]->high_word = new_rd_Phi(dbg, irg, block, arity, inh, mode);

	if (enq) {
		/* not yet finished */
		pdeq_putr(env->waitq, phi);
	}  /* if */
}  /* lower_Phi */

/**
 * Translate a Psi.
 */
static void lower_Psi(ir_node *psi, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg = current_ir_graph;
	ir_node  *block, *val;
	ir_node  **valsl, **valsh, **conds;
	dbg_info *dbg;
	int      idx, i, n_conds = get_Psi_n_conds(psi);

	/* first create a new in array */
	NEW_ARR_A(ir_node *, valsl, n_conds + 1);
	NEW_ARR_A(ir_node *, valsh, n_conds + 1);

	for (i = 0; i < n_conds; ++i) {
		val = get_Psi_val(psi, i);
		idx = get_irn_idx(val);
		if (env->entries[idx]->low_word) {
			/* Values already build */
			valsl[i] = env->entries[idx]->low_word;
			valsh[i] = env->entries[idx]->high_word;
		} else {
			/* still not ready */
			pdeq_putr(env->waitq, psi);
			return;
		}  /* if */
	}  /* for */
	val = get_Psi_default(psi);
	idx = get_irn_idx(val);
	if (env->entries[idx]->low_word) {
		/* Values already build */
		valsl[i] = env->entries[idx]->low_word;
		valsh[i] = env->entries[idx]->high_word;
	} else {
		/* still not ready */
		pdeq_putr(env->waitq, psi);
		return;
	}  /* if */


	NEW_ARR_A(ir_node *, conds, n_conds);
	for (i = 0; i < n_conds; ++i) {
		conds[i] = get_Psi_cond(psi, i);
	}  /* for */

	dbg   = get_irn_dbg_info(psi);
	block = get_nodes_block(psi);

	idx = get_irn_idx(psi);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_rd_Psi(dbg, irg, block, n_conds, conds, valsl, mode);
	env->entries[idx]->high_word = new_rd_Psi(dbg, irg, block, n_conds, conds, valsh, mode);
}  /* lower_Psi */

/**
 * check for opcodes that must always be lowered.
 */
static int always_lower(ir_opcode code) {
	switch (code) {
	case iro_Proj:
	case iro_Start:
	case iro_Call:
	case iro_Return:
	case iro_Cond:
	case iro_Conv:
		return 1;
	default:
		return 0;
	}  /* switch */
}  /* always_lower */

/**
 * lower boolean Proj(Cmp)
 */
static ir_node *lower_boolean_Proj_Cmp(ir_node *proj, ir_node *cmp, lower_env_t *env) {
	int      lidx, ridx;
	ir_node  *l, *r, *low, *high, *t, *res;
	pn_Cmp   pnc;
	ir_node  *blk;
	ir_graph *irg = current_ir_graph;
	dbg_info *db;

	l    = get_Cmp_left(cmp);
	lidx = get_irn_idx(l);
	if (! env->entries[lidx]->low_word) {
		/* still not ready */
		return NULL;
	}  /* if */

	r    = get_Cmp_right(cmp);
	ridx = get_irn_idx(r);
	if (! env->entries[ridx]->low_word) {
		/* still not ready */
		return NULL;
	}  /* if */

	pnc  = get_Proj_proj(proj);
	blk  = get_nodes_block(cmp);
	db   = get_irn_dbg_info(cmp);
	low  = new_rd_Cmp(db, irg, blk, env->entries[lidx]->low_word, env->entries[ridx]->low_word);
	high = new_rd_Cmp(db, irg, blk, env->entries[lidx]->high_word, env->entries[ridx]->high_word);

	if (pnc == pn_Cmp_Eq) {
		/* simple case:a == b <==> a_h == b_h && a_l == b_l */
		res = new_rd_And(db, irg, blk,
			new_r_Proj(irg, blk, low, mode_b, pnc),
			new_r_Proj(irg, blk, high, mode_b, pnc),
			mode_b);
	} else if (pnc == pn_Cmp_Lg) {
		/* simple case:a != b <==> a_h != b_h || a_l != b_l */
		res = new_rd_Or(db, irg, blk,
			new_r_Proj(irg, blk, low, mode_b, pnc),
			new_r_Proj(irg, blk, high, mode_b, pnc),
			mode_b);
	} else {
		/* a rel b <==> a_h REL b_h || (a_h == b_h && a_l rel b_l) */
		t = new_rd_And(db, irg, blk,
			new_r_Proj(irg, blk, low, mode_b, pnc),
			new_r_Proj(irg, blk, high, mode_b, pn_Cmp_Eq),
			mode_b);
		res = new_rd_Or(db, irg, blk,
			new_r_Proj(irg, blk, high, mode_b, pnc & ~pn_Cmp_Eq),
			t,
			mode_b);
	}  /* if */
	return res;
}  /* lower_boolean_Proj_Cmp */

/**
 * The type of a lower function.
 *
 * @param node   the node to be lowered
 * @param mode   the low mode for the destination node
 * @param env    the lower environment
 */
typedef void (*lower_func)(ir_node *node, ir_mode *mode, lower_env_t *env);

/**
 * Lower a node.
 */
static void lower_ops(ir_node *node, void *env)
{
	lower_env_t  *lenv = env;
	node_entry_t *entry;
	int          idx = get_irn_idx(node);
	ir_mode      *mode = get_irn_mode(node);

	if (mode == mode_b || is_Psi(node)) {
		int i;

		for (i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node *proj = get_irn_n(node, i);

			if (is_Proj(proj)) {
				ir_node *cmp = get_Proj_pred(proj);

				if (is_Cmp(cmp)) {
					ir_node *arg = get_Cmp_left(cmp);

					mode = get_irn_mode(arg);
					if (mode == lenv->params->high_signed ||
						mode == lenv->params->high_unsigned) {
						ir_node *res = lower_boolean_Proj_Cmp(proj, cmp, lenv);

						if (res == NULL) {
							/* could not lower because predecessors not ready */
							waitq_put(lenv->waitq, node);
							return;
						}  /* if */
						set_irn_n(node, i, res);
					}  /* if */
				}  /* if */
			}  /* if */
		}  /* for */
	}  /* if */

	entry = idx < lenv->n_entries ? lenv->entries[idx] : NULL;
	if (entry || always_lower(get_irn_opcode(node))) {
		ir_op      *op = get_irn_op(node);
		lower_func func = (lower_func)op->ops.generic;

		if (func) {
			mode = get_irn_op_mode(node);

			if (mode == lenv->params->high_signed)
				mode = lenv->params->low_signed;
			else
				mode = lenv->params->low_unsigned;

			DB((dbg, LEVEL_1, "  %+F\n", node));
			func(node, mode, lenv);
		}  /* if */
	}  /* if */
}  /* lower_ops */

#define IDENT(s)  new_id_from_chars(s, sizeof(s)-1)

/**
 * Compare two op_mode_entry_t's.
 */
static int cmp_op_mode(const void *elt, const void *key, size_t size) {
	const op_mode_entry_t *e1 = elt;
	const op_mode_entry_t *e2 = key;
	(void) size;

	return (e1->op - e2->op) | (e1->imode - e2->imode) | (e1->omode - e2->omode);
}  /* cmp_op_mode */

/**
 * Compare two conv_tp_entry_t's.
 */
static int cmp_conv_tp(const void *elt, const void *key, size_t size) {
	const conv_tp_entry_t *e1 = elt;
	const conv_tp_entry_t *e2 = key;
	(void) size;

	return (e1->imode - e2->imode) | (e1->omode - e2->omode);
}  /* static int cmp_conv_tp */

/*
 * Do the lowering.
 */
void lower_dw_ops(const lwrdw_param_t *param)
{
	lower_env_t lenv;
	int i;
	ir_graph *rem;

	if (! param)
		return;

	if (! param->enable)
		return;

	FIRM_DBG_REGISTER(dbg, "firm.lower.dw");

	assert(2 * get_mode_size_bits(param->low_signed)   == get_mode_size_bits(param->high_signed));
	assert(2 * get_mode_size_bits(param->low_unsigned) == get_mode_size_bits(param->high_unsigned));
	assert(get_mode_size_bits(param->low_signed) == get_mode_size_bits(param->low_unsigned));

	/* create the necessary maps */
	if (! prim_types)
		prim_types = pmap_create();
	if (! intrinsic_fkt)
		intrinsic_fkt = new_set(cmp_op_mode, iro_MaxOpcode);
	if (! conv_types)
		conv_types = new_set(cmp_conv_tp, 16);
	if (! lowered_type)
		lowered_type = pmap_create();

	/* create a primitive unsigned and signed type */
	if (! tp_u)
		tp_u = get_primitive_type(param->low_unsigned);
	if (! tp_s)
		tp_s = get_primitive_type(param->low_signed);

	/* create method types for the created binop calls */
	if (! binop_tp_u) {
		binop_tp_u = new_type_method(IDENT("binop_u_intrinsic"), 4, 2);
		set_method_param_type(binop_tp_u, 0, tp_u);
		set_method_param_type(binop_tp_u, 1, tp_u);
		set_method_param_type(binop_tp_u, 2, tp_u);
		set_method_param_type(binop_tp_u, 3, tp_u);
		set_method_res_type(binop_tp_u, 0, tp_u);
		set_method_res_type(binop_tp_u, 1, tp_u);
	}  /* if */
	if (! binop_tp_s) {
		binop_tp_s = new_type_method(IDENT("binop_s_intrinsic"), 4, 2);
		set_method_param_type(binop_tp_s, 0, tp_u);
		set_method_param_type(binop_tp_s, 1, tp_s);
		set_method_param_type(binop_tp_s, 2, tp_u);
		set_method_param_type(binop_tp_s, 3, tp_s);
		set_method_res_type(binop_tp_s, 0, tp_u);
		set_method_res_type(binop_tp_s, 1, tp_s);
	}  /* if */
	if (! shiftop_tp_u) {
		shiftop_tp_u = new_type_method(IDENT("shiftop_u_intrinsic"), 3, 2);
		set_method_param_type(shiftop_tp_u, 0, tp_u);
		set_method_param_type(shiftop_tp_u, 1, tp_u);
		set_method_param_type(shiftop_tp_u, 2, tp_u);
		set_method_res_type(shiftop_tp_u, 0, tp_u);
		set_method_res_type(shiftop_tp_u, 1, tp_u);
	}  /* if */
	if (! shiftop_tp_s) {
		shiftop_tp_s = new_type_method(IDENT("shiftop_s_intrinsic"), 3, 2);
		set_method_param_type(shiftop_tp_s, 0, tp_u);
		set_method_param_type(shiftop_tp_s, 1, tp_s);
		/* beware: shift count is always mode_Iu */
		set_method_param_type(shiftop_tp_s, 2, tp_u);
		set_method_res_type(shiftop_tp_s, 0, tp_u);
		set_method_res_type(shiftop_tp_s, 1, tp_s);
	}  /* if */
	if (! unop_tp_u) {
		unop_tp_u = new_type_method(IDENT("unop_u_intrinsic"), 2, 2);
		set_method_param_type(unop_tp_u, 0, tp_u);
		set_method_param_type(unop_tp_u, 1, tp_u);
		set_method_res_type(unop_tp_u, 0, tp_u);
		set_method_res_type(unop_tp_u, 1, tp_u);
	}  /* if */
	if (! unop_tp_s) {
		unop_tp_s = new_type_method(IDENT("unop_s_intrinsic"), 2, 2);
		set_method_param_type(unop_tp_s, 0, tp_u);
		set_method_param_type(unop_tp_s, 1, tp_s);
		set_method_res_type(unop_tp_s, 0, tp_u);
		set_method_res_type(unop_tp_s, 1, tp_s);
	}  /* if */

	lenv.tv_mode_bytes = new_tarval_from_long(get_mode_size_bytes(param->low_unsigned), mode_Iu);
	lenv.tv_mode_bits  = new_tarval_from_long(get_mode_size_bits(param->low_unsigned), mode_Iu);
	lenv.waitq         = new_pdeq();
	lenv.params        = param;

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

#define LOWER2(op, fkt)   op_##op->ops.generic = (op_func)fkt
#define LOWER(op)         LOWER2(op, lower_##op)
#define LOWER_BIN(op)     LOWER2(op, lower_Binop)
#define LOWER_UN(op)      LOWER2(op, lower_Unop)

	/* the table of all operations that must be lowered follows */
	LOWER(Load);
	LOWER(Store);
	LOWER(Const);
	LOWER(And);
	LOWER(Or);
	LOWER(Eor);
	LOWER(Not);
	LOWER(Cond);
	LOWER(Return);
	LOWER(Call);
	LOWER(Unknown);
	LOWER(Phi);
	LOWER(Psi);
	LOWER(Start);

	LOWER_BIN(Add);
	LOWER_BIN(Sub);
	LOWER_BIN(Mul);
	LOWER(Shl);
	LOWER(Shr);
	LOWER(Shrs);
	LOWER(Rot);
	LOWER(DivMod);
	LOWER(Div);
	LOWER(Mod);
	LOWER_UN(Abs);
	LOWER_UN(Minus);

	LOWER(Conv);

#undef LOWER_UN
#undef LOWER_BIN
#undef LOWER
#undef LOWER2

	/* transform all graphs */
	rem = current_ir_graph;
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		int n_idx;

		obstack_init(&lenv.obst);

		n_idx = get_irg_last_idx(irg);
		lenv.n_entries = n_idx;
		lenv.entries   = xmalloc(n_idx * sizeof(lenv.entries[0]));
		memset(lenv.entries, 0, n_idx * sizeof(lenv.entries[0]));

		/* first step: link all nodes and allocate data */
		lenv.flags = 0;
		lenv.proj_2_block = pmap_create();
		irg_walk_graph(irg, firm_clear_link, prepare_links, &lenv);

		if (lenv.flags & MUST_BE_LOWERED) {
			DB((dbg, LEVEL_1, "Lowering graph %+F\n", irg));

			/* must do some work */
			irg_walk_graph(irg, NULL, lower_ops, &lenv);

			/* last step: all waiting nodes */
			DB((dbg, LEVEL_1, "finishing waiting nodes:\n"));
			current_ir_graph = irg;
			while (! pdeq_empty(lenv.waitq)) {
				ir_node *node = pdeq_getl(lenv.waitq);

				lower_ops(node, &lenv);
			}  /* while */

			/* outs are invalid, we changed the graph */
			set_irg_outs_inconsistent(irg);

			if (lenv.flags & CF_CHANGED) {
				/* control flow changed, dominance info is invalid */
				set_irg_doms_inconsistent(irg);
				set_irg_extblk_inconsistent(irg);
				set_irg_loopinfo_inconsistent(irg);
			}  /* if */
		}  /* if */
		pmap_destroy(lenv.proj_2_block);
		free(lenv.entries);
		obstack_free(&lenv.obst, NULL);
	}  /* for */
	del_pdeq(lenv.waitq);
	current_ir_graph = rem;
}  /* lower_dw_ops */

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
	}  /* if */
	id = new_id_from_str(buf);

	ent = new_entity(get_glob_type(), id, method);
	set_entity_ld_ident(ent, get_entity_ident(ent));
	set_entity_visibility(ent, visibility_external_allocated);
	return ent;
}  /* def_create_intrinsic_fkt */
