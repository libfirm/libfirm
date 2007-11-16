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
 * @brief   Load/Store optimizations.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <string.h>

#include "iroptimize.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irvrfy.h"
#include "tv_t.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "array.h"
#include "irhooks.h"
#include "iredges.h"
#include "irtools.h"
#include "opt_polymorphy.h"
#include "irmemory.h"
#include "xmalloc.h"
#include "irphase_t.h"
#include "irgopt.h"
#include "debug.h"

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

#ifdef DO_CACHEOPT
#include "cacheopt/cachesim.h"
#endif

#undef IMAX
#define IMAX(a,b)	((a) > (b) ? (a) : (b))

#define MAX_PROJ	IMAX(pn_Load_max, pn_Store_max)

enum changes_t {
	DF_CHANGED = 1,       /**< data flow changed */
	CF_CHANGED = 2,       /**< control flow changed */
};

/**
 * walker environment
 */
typedef struct _walk_env_t {
	struct obstack obst;          /**< list of all stores */
	unsigned changes;             /**< a bitmask of graph changes */
} walk_env_t;

/**
 * flags for Load/Store
 */
enum ldst_flags_t {
	LDST_VISITED = 1              /**< if set, this Load/Store is already visited */
};

/** A Load/Store info. */
typedef struct _ldst_info_t {
	ir_node  *projs[MAX_PROJ];    /**< list of Proj's of this node */
	ir_node  *exc_block;          /**< the exception block if available */
	int      exc_idx;             /**< predecessor index in the exception block */
	unsigned flags;               /**< flags */
	unsigned visited;             /**< visited counter for breaking loops */
} ldst_info_t;

/**
 * flags for control flow.
 */
enum block_flags_t {
	BLOCK_HAS_COND = 1,      /**< Block has conditional control flow */
	BLOCK_HAS_EXC  = 2       /**< Block has exceptional control flow */
};

/**
 * a Block info.
 */
typedef struct _block_info_t {
	unsigned flags;               /**< flags for the block */
} block_info_t;

/** the master visited flag for loop detection. */
static unsigned master_visited = 0;

#define INC_MASTER()       ++master_visited
#define MARK_NODE(info)    (info)->visited = master_visited
#define NODE_VISITED(info) (info)->visited >= master_visited

/**
 * get the Load/Store info of a node
 */
static ldst_info_t *get_ldst_info(ir_node *node, struct obstack *obst) {
	ldst_info_t *info = get_irn_link(node);

	if (! info) {
		info = obstack_alloc(obst, sizeof(*info));
		memset(info, 0, sizeof(*info));
		set_irn_link(node, info);
	}
	return info;
}  /* get_ldst_info */

/**
 * get the Block info of a node
 */
static block_info_t *get_block_info(ir_node *node, struct obstack *obst) {
	block_info_t *info = get_irn_link(node);

	if (! info) {
		info = obstack_alloc(obst, sizeof(*info));
		memset(info, 0, sizeof(*info));
		set_irn_link(node, info);
	}
	return info;
}  /* get_block_info */

/**
 * update the projection info for a Load/Store
 */
static unsigned update_projs(ldst_info_t *info, ir_node *proj)
{
	long nr = get_Proj_proj(proj);

	assert(0 <= nr && nr <= MAX_PROJ && "Wrong proj from LoadStore");

	if (info->projs[nr]) {
		/* there is already one, do CSE */
		exchange(proj, info->projs[nr]);
		return DF_CHANGED;
	}
	else {
		info->projs[nr] = proj;
		return 0;
	}
}  /* update_projs */

/**
 * update the exception block info for a Load/Store node.
 *
 * @param info   the load/store info struct
 * @param block  the exception handler block for this load/store
 * @param pos    the control flow input of the block
 */
static unsigned update_exc(ldst_info_t *info, ir_node *block, int pos)
{
	assert(info->exc_block == NULL && "more than one exception block found");

	info->exc_block = block;
	info->exc_idx   = pos;
	return 0;
}  /* update_exc */

/** Return the number of uses of an address node */
#define get_irn_n_uses(adr)     get_irn_n_edges(adr)

/**
 * walker, collects all Load/Store/Proj nodes
 *
 * walks from Start -> End
 */
static void collect_nodes(ir_node *node, void *env)
{
	ir_op       *op = get_irn_op(node);
	ir_node     *pred, *blk, *pred_blk;
	ldst_info_t *ldst_info;
	walk_env_t  *wenv = env;

	if (op == op_Proj) {
		ir_node *adr;
		ir_op *op;

		pred = get_Proj_pred(node);
		op   = get_irn_op(pred);

		if (op == op_Load) {
			ldst_info = get_ldst_info(pred, &wenv->obst);

			wenv->changes |= update_projs(ldst_info, node);

			if ((ldst_info->flags & LDST_VISITED) == 0) {
				adr = get_Load_ptr(pred);
				ldst_info->flags |= LDST_VISITED;
			}

			/*
			 * Place the Proj's to the same block as the
			 * predecessor Load. This is always ok and prevents
			 * "non-SSA" form after optimizations if the Proj
			 * is in a wrong block.
			 */
			blk      = get_nodes_block(node);
			pred_blk = get_nodes_block(pred);
			if (blk != pred_blk) {
				wenv->changes |= DF_CHANGED;
				set_nodes_block(node, pred_blk);
			}
		} else if (op == op_Store) {
			ldst_info = get_ldst_info(pred, &wenv->obst);

			wenv->changes |= update_projs(ldst_info, node);

			if ((ldst_info->flags & LDST_VISITED) == 0) {
				adr = get_Store_ptr(pred);
				ldst_info->flags |= LDST_VISITED;
			}

			/*
			* Place the Proj's to the same block as the
			* predecessor Store. This is always ok and prevents
			* "non-SSA" form after optimizations if the Proj
			* is in a wrong block.
			*/
			blk      = get_nodes_block(node);
			pred_blk = get_nodes_block(pred);
			if (blk != pred_blk) {
				wenv->changes |= DF_CHANGED;
				set_nodes_block(node, pred_blk);
			}
		}
	} else if (op == op_Block) {
		int i;

		for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
			ir_node      *pred_block, *proj;
			block_info_t *bl_info;
			int          is_exc = 0;

			pred = proj = get_Block_cfgpred(node, i);

			if (is_Proj(proj)) {
				pred   = get_Proj_pred(proj);
				is_exc = get_Proj_proj(proj) == pn_Generic_X_except;
			}

			/* ignore Bad predecessors, they will be removed later */
			if (is_Bad(pred))
				continue;

			pred_block = get_nodes_block(pred);
			bl_info    = get_block_info(pred_block, &wenv->obst);

			if (is_fragile_op(pred) && is_exc)
				bl_info->flags |= BLOCK_HAS_EXC;
			else if (is_irn_forking(pred))
				bl_info->flags |= BLOCK_HAS_COND;

			if (is_exc && (get_irn_op(pred) == op_Load || get_irn_op(pred) == op_Store)) {
				ldst_info = get_ldst_info(pred, &wenv->obst);

				wenv->changes |= update_exc(ldst_info, node, i);
			}
		}
	}
}  /* collect_nodes */

/**
 * Returns an entity if the address ptr points to a constant one.
 *
 * @param ptr  the address
 *
 * @return an entity or NULL
 */
static ir_entity *find_constant_entity(ir_node *ptr)
{
	for (;;) {
		ir_op *op = get_irn_op(ptr);

		if (op == op_SymConst && (get_SymConst_kind(ptr) == symconst_addr_ent)) {
			ir_entity *ent = get_SymConst_entity(ptr);
			if (variability_constant == get_entity_variability(ent))
				return ent;
			return NULL;
		} else if (op == op_Sel) {
			ir_entity *ent = get_Sel_entity(ptr);
			ir_type   *tp  = get_entity_owner(ent);

			/* Do not fiddle with polymorphism. */
			if (is_Class_type(get_entity_owner(ent)) &&
				((get_entity_n_overwrites(ent)    != 0) ||
				(get_entity_n_overwrittenby(ent) != 0)   ) )
				return NULL;

			if (is_Array_type(tp)) {
				/* check bounds */
				int i, n;

				for (i = 0, n = get_Sel_n_indexs(ptr); i < n; ++i) {
					ir_node *bound;
					tarval *tlower, *tupper;
					ir_node *index = get_Sel_index(ptr, i);
					tarval *tv     = computed_value(index);

					/* check if the index is constant */
					if (tv == tarval_bad)
						return NULL;

					bound  = get_array_lower_bound(tp, i);
					tlower = computed_value(bound);
					bound  = get_array_upper_bound(tp, i);
					tupper = computed_value(bound);

					if (tlower == tarval_bad || tupper == tarval_bad)
						return NULL;

					if (tarval_cmp(tv, tlower) & pn_Cmp_Lt)
						return NULL;
					if (tarval_cmp(tupper, tv) & pn_Cmp_Lt)
						return NULL;

					/* ok, bounds check finished */
				}
			}

			if (variability_constant == get_entity_variability(ent))
				return ent;

			/* try next */
			ptr = get_Sel_ptr(ptr);
		} else
			return NULL;
	}
}  /* find_constant_entity */

/**
 * Return the Selection index of a Sel node from dimension n
 */
static long get_Sel_array_index_long(ir_node *n, int dim) {
	ir_node *index = get_Sel_index(n, dim);
	assert(is_Const(index));
	return get_tarval_long(get_Const_tarval(index));
}  /* get_Sel_array_index_long */

/**
 * Returns the accessed component graph path for an
 * node computing an address.
 *
 * @param ptr    the node computing the address
 * @param depth  current depth in steps upward from the root
 *               of the address
 */
static compound_graph_path *rec_get_accessed_path(ir_node *ptr, int depth) {
	compound_graph_path *res = NULL;
	ir_entity           *root, *field;
	int                 path_len, pos;

	if (get_irn_op(ptr) == op_SymConst) {
		/* a SymConst. If the depth is 0, this is an access to a global
		 * entity and we don't need a component path, else we know
		 * at least it's length.
		 */
		assert(get_SymConst_kind(ptr) == symconst_addr_ent);
		root = get_SymConst_entity(ptr);
		res = (depth == 0) ? NULL : new_compound_graph_path(get_entity_type(root), depth);
	} else {
		assert(get_irn_op(ptr) == op_Sel);
		/* it's a Sel, go up until we find the root */
		res = rec_get_accessed_path(get_Sel_ptr(ptr), depth+1);

		/* fill up the step in the path at the current position */
		field    = get_Sel_entity(ptr);
		path_len = get_compound_graph_path_length(res);
		pos      = path_len - depth - 1;
		set_compound_graph_path_node(res, pos, field);

		if (is_Array_type(get_entity_owner(field))) {
			assert(get_Sel_n_indexs(ptr) == 1 && "multi dim arrays not implemented");
			set_compound_graph_path_array_index(res, pos, get_Sel_array_index_long(ptr, 0));
		}
	}
	return res;
}  /* rec_get_accessed_path */

/** Returns an access path or NULL.  The access path is only
 *  valid, if the graph is in phase_high and _no_ address computation is used.
 */
static compound_graph_path *get_accessed_path(ir_node *ptr) {
	return rec_get_accessed_path(ptr, 0);
}  /* get_accessed_path */

/* forward */
static void reduce_adr_usage(ir_node *ptr);

/**
 * Update a Load that may lost it's usage.
 */
static void handle_load_update(ir_node *load) {
	ldst_info_t *info = get_irn_link(load);

	/* do NOT touch volatile loads for now */
	if (get_Load_volatility(load) == volatility_is_volatile)
		return;

	if (! info->projs[pn_Load_res] && ! info->projs[pn_Load_X_except]) {
		ir_node *ptr = get_Load_ptr(load);
		ir_node *mem = get_Load_mem(load);

		/* a Load which value is neither used nor exception checked, remove it */
		exchange(info->projs[pn_Load_M], mem);
		if (info->projs[pn_Load_X_regular])
			exchange(info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
		exchange(load, new_Bad());
		reduce_adr_usage(ptr);
	}
}  /* handle_load_update */

/**
 * A Use of an address node is vanished. Check if this was a Proj
 * node and update the counters.
 */
static void reduce_adr_usage(ir_node *ptr) {
	if (is_Proj(ptr)) {
		if (get_irn_n_edges(ptr) <= 0) {
			/* this Proj is dead now */
			ir_node *pred = get_Proj_pred(ptr);

			if (is_Load(pred)) {
				ldst_info_t *info = get_irn_link(pred);
				info->projs[get_Proj_proj(ptr)] = NULL;

				/* this node lost it's result proj, handle that */
				handle_load_update(pred);
			}
		}
	}
}  /* reduce_adr_usage */

/**
 * Check, if an already existing value of mode old_mode can be converted
 * into the needed one new_mode without loss.
 */
static int can_use_stored_value(ir_mode *old_mode, ir_mode *new_mode) {
	if (old_mode == new_mode)
		return 1;

	/* if both modes are two-complement ones, we can always convert the
	   Stored value into the needed one. */
	if (get_mode_size_bits(old_mode) >= get_mode_size_bits(new_mode) &&
		  get_mode_arithmetic(old_mode) == irma_twos_complement &&
		  get_mode_arithmetic(new_mode) == irma_twos_complement)
		return 1;
	return 0;
}  /* can_use_stored_value */

/**
 * Follow the memory chain as long as there are only Loads
 * and alias free Stores and try to replace current Load or Store
 * by a previous ones.
 * Note that in unreachable loops it might happen that we reach
 * load again, as well as we can fall into a cycle.
 * We break such cycles using a special visited flag.
 *
 * INC_MASTER() must be called before dive into
 */
static unsigned follow_Mem_chain(ir_node *load, ir_node *curr) {
	unsigned res = 0;
	ldst_info_t *info = get_irn_link(load);
	ir_node *pred;
	ir_node *ptr       = get_Load_ptr(load);
	ir_node *mem       = get_Load_mem(load);
	ir_mode *load_mode = get_Load_mode(load);

	for (pred = curr; load != pred; ) {
		ldst_info_t *pred_info = get_irn_link(pred);

		/*
		 * BEWARE: one might think that checking the modes is useless, because
		 * if the pointers are identical, they refer to the same object.
		 * This is only true in strong typed languages, not in C were the following
		 * is possible a = *(ir_type1 *)p; b = *(ir_type2 *)p ...
		 */
		if (is_Store(pred) && get_Store_ptr(pred) == ptr &&
		    can_use_stored_value(get_irn_mode(get_Store_value(pred)), load_mode)) {
			/*
			 * a Load immediately after a Store -- a read after write.
			 * We may remove the Load, if both Load & Store does not have an exception handler
			 * OR they are in the same MacroBlock. In the latter case the Load cannot
			 * throw an exception when the previous Store was quiet.
			 *
			 * Why we need to check for Store Exception? If the Store cannot
			 * be executed (ROM) the exception handler might simply jump into
			 * the load MacroBlock :-(
			 * We could make it a little bit better if we would know that the exception
			 * handler of the Store jumps directly to the end...
			 */
			if ((pred_info->projs[pn_Store_X_except] == NULL && info->projs[pn_Load_X_except] == NULL) ||
			    get_nodes_MacroBlock(load) == get_nodes_MacroBlock(pred)) {
				ir_node *value = get_Store_value(pred);

				DBG_OPT_RAW(load, value);

				/* add an convert if needed */
				if (get_irn_mode(get_Store_value(pred)) != load_mode) {
					value = new_r_Conv(current_ir_graph, get_nodes_block(load), value, load_mode);
				}

				if (info->projs[pn_Load_M])
					exchange(info->projs[pn_Load_M], mem);

				/* no exception */
				if (info->projs[pn_Load_X_except]) {
					exchange( info->projs[pn_Load_X_except], new_Bad());
					res |= CF_CHANGED;
				}
				if (info->projs[pn_Load_X_regular]) {
					exchange( info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
					res |= CF_CHANGED;
				}

				if (info->projs[pn_Load_res])
					exchange(info->projs[pn_Load_res], value);

				exchange(load, new_Bad());
				reduce_adr_usage(ptr);
				return res | DF_CHANGED;
			}
		} else if (is_Load(pred) && get_Load_ptr(pred) == ptr &&
		           can_use_stored_value(get_Load_mode(pred), load_mode)) {
			/*
			 * a Load after a Load -- a read after read.
			 * We may remove the second Load, if it does not have an exception handler
			 * OR they are in the same MacroBlock. In the later case the Load cannot
			 * throw an exception when the previous Load was quiet.
			 *
			 * Here, there is no need to check if the previous Load has an exception
			 * hander because they would have exact the same exception...
			 */
			if (info->projs[pn_Load_X_except] == NULL || get_nodes_MacroBlock(load) == get_nodes_MacroBlock(pred)) {
				ir_node *value;

				DBG_OPT_RAR(load, pred);

				/* the result is used */
				if (info->projs[pn_Load_res]) {
					if (pred_info->projs[pn_Load_res] == NULL) {
						/* create a new Proj again */
						pred_info->projs[pn_Load_res] = new_r_Proj(current_ir_graph, get_nodes_block(pred), pred, get_Load_mode(pred), pn_Load_res);
					}
					value = pred_info->projs[pn_Load_res];

					/* add an convert if needed */
					if (get_Load_mode(pred) != load_mode) {
						value = new_r_Conv(current_ir_graph, get_nodes_block(load), value, load_mode);
					}

					exchange(info->projs[pn_Load_res], value);
				}

				if (info->projs[pn_Load_M])
					exchange(info->projs[pn_Load_M], mem);

				/* no exception */
				if (info->projs[pn_Load_X_except]) {
					exchange(info->projs[pn_Load_X_except], new_Bad());
					res |= CF_CHANGED;
				}
				if (info->projs[pn_Load_X_regular]) {
					exchange( info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
					res |= CF_CHANGED;
				}

				exchange(load, new_Bad());
				reduce_adr_usage(ptr);
				return res |= DF_CHANGED;
			}
		}

		if (is_Store(pred)) {
			/* check if we can pass through this store */
			ir_alias_relation rel = get_alias_relation(
				current_ir_graph,
				get_Store_ptr(pred),
				get_irn_mode(get_Store_value(pred)),
				ptr, load_mode);
			/* if the might be an alias, we cannot pass this Store */
			if (rel != no_alias)
				break;
			pred = skip_Proj(get_Store_mem(pred));
		} else if (get_irn_op(pred) == op_Load) {
			pred = skip_Proj(get_Load_mem(pred));
		} else {
			/* follow only Load chains */
			break;
		}

		/* check for cycles */
		if (NODE_VISITED(pred_info))
			break;
		MARK_NODE(pred_info);
	}

	if (is_Sync(pred)) {
		int i;

		/* handle all Sync predecessors */
		for (i = get_Sync_n_preds(pred) - 1; i >= 0; --i) {
			res |= follow_Mem_chain(load, skip_Proj(get_Sync_pred(pred, i)));
			if (res)
				return res;
		}
	}

	return res;
}  /* follow_Mem_chain */

/**
 * optimize a Load
 *
 * @param load  the Load node
 */
static unsigned optimize_load(ir_node *load)
{
	ldst_info_t *info = get_irn_link(load);
	ir_node *mem, *ptr, *new_node;
	ir_entity *ent;
	unsigned res = 0;

	/* do NOT touch volatile loads for now */
	if (get_Load_volatility(load) == volatility_is_volatile)
		return 0;

	/* the address of the load to be optimized */
	ptr = get_Load_ptr(load);

	/*
	 * Check if we can remove the exception from a Load:
	 * This can be done, if the address is from an Sel(Alloc) and
	 * the Sel type is a subtype of the allocated type.
	 *
	 * This optimizes some often used OO constructs,
	 * like x = new O; x->t;
	 */
	if (info->projs[pn_Load_X_except]) {
		if (is_Sel(ptr)) {
			ir_node *mem = get_Sel_mem(ptr);

			/* FIXME: works with the current FE, but better use the base */
			if (is_Alloc(skip_Proj(mem))) {
				/* ok, check the types */
				ir_entity *ent    = get_Sel_entity(ptr);
				ir_type   *s_type = get_entity_type(ent);
				ir_type   *a_type = get_Alloc_type(mem);

				if (is_SubClass_of(s_type, a_type)) {
					/* ok, condition met: there can't be an exception because
					* Alloc guarantees that enough memory was allocated */

					exchange(info->projs[pn_Load_X_except], new_Bad());
					info->projs[pn_Load_X_except] = NULL;
					exchange(info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
					info->projs[pn_Load_X_regular] = NULL;
					res |= CF_CHANGED;
				}
			}
		} else if (is_Alloc(skip_Proj(skip_Cast(ptr)))) {
				/* simple case: a direct load after an Alloc. Firm Alloc throw
				 * an exception in case of out-of-memory. So, there is no way for an
				 * exception in this load.
				 * This code is constructed by the "exception lowering" in the Jack compiler.
				 */
				exchange(info->projs[pn_Load_X_except], new_Bad());
				info->projs[pn_Load_X_except] = NULL;
				exchange(info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
				info->projs[pn_Load_X_regular] = NULL;
				res |= CF_CHANGED;
		}
	}

	/* The mem of the Load. Must still be returned after optimization. */
	mem  = get_Load_mem(load);

	if (! info->projs[pn_Load_res] && ! info->projs[pn_Load_X_except]) {
		/* a Load which value is neither used nor exception checked, remove it */
		exchange(info->projs[pn_Load_M], mem);

		if (info->projs[pn_Load_X_regular]) {
			/* should not happen, but if it does, remove it */
			exchange(info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
			res |= CF_CHANGED;
		}
		exchange(load, new_Bad());
		reduce_adr_usage(ptr);
		return res | DF_CHANGED;
	}

	/* Load from a constant polymorphic field, where we can resolve
	   polymorphism. */
	new_node = transform_node_Load(load);
	if (new_node != load) {
		if (info->projs[pn_Load_M]) {
			exchange(info->projs[pn_Load_M], mem);
			info->projs[pn_Load_M] = NULL;
		}
		if (info->projs[pn_Load_X_except]) {
			exchange(info->projs[pn_Load_X_except], new_Bad());
			info->projs[pn_Load_X_except] = NULL;
			res |= CF_CHANGED;
		}
		if (info->projs[pn_Load_X_regular]) {
			exchange(info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
			info->projs[pn_Load_X_regular] = NULL;
			res |= CF_CHANGED;
		}
		if (info->projs[pn_Load_res])
			exchange(info->projs[pn_Load_res], new_node);

		exchange(load, new_Bad());
		reduce_adr_usage(ptr);
		return res | DF_CHANGED;
	}

	/* check if we can determine the entity that will be loaded */
	ent = find_constant_entity(ptr);
	if (ent) {
		if ((allocation_static == get_entity_allocation(ent)) &&
			(visibility_external_allocated != get_entity_visibility(ent))) {
			/* a static allocation that is not external: there should be NO exception
			 * when loading. */

			/* no exception, clear the info field as it might be checked later again */
			if (info->projs[pn_Load_X_except]) {
				exchange(info->projs[pn_Load_X_except], new_Bad());
				info->projs[pn_Load_X_except] = NULL;
				res |= CF_CHANGED;
			}
			if (info->projs[pn_Load_X_regular]) {
				exchange(info->projs[pn_Load_X_regular], new_r_Jmp(current_ir_graph, get_nodes_block(load)));
				info->projs[pn_Load_X_regular] = NULL;
				res |= CF_CHANGED;
			}

			if (variability_constant == get_entity_variability(ent)) {
				if (is_atomic_entity(ent)) {
					/* Might not be atomic after
					   lowering of Sels.  In this
					   case we could also load, but
					   it's more complicated. */
					/* more simpler case: we load the content of a constant value:
					 * replace it by the constant itself
					 */

					/* no memory */
					if (info->projs[pn_Load_M]) {
						exchange(info->projs[pn_Load_M], mem);
						res |= DF_CHANGED;
					}
					/* no result :-) */
					if (info->projs[pn_Load_res]) {
						if (is_atomic_entity(ent)) {
							ir_node *c = copy_const_value(get_irn_dbg_info(load), get_atomic_ent_value(ent));

							DBG_OPT_RC(load, c);
							exchange(info->projs[pn_Load_res], c);
							res |= DF_CHANGED;
						}
					}
					exchange(load, new_Bad());
					reduce_adr_usage(ptr);
					return res;
				} else {
					compound_graph_path *path = get_accessed_path(ptr);

					if (path) {
						ir_node *c;

						assert(is_proper_compound_graph_path(path, get_compound_graph_path_length(path)-1));
						/*
						{
							int j;
							for (j = 0; j < get_compound_graph_path_length(path); ++j) {
								ir_entity *node = get_compound_graph_path_node(path, j);
								fprintf(stdout, ".%s", get_entity_name(node));
								if (is_Array_type(get_entity_owner(node)))
									fprintf(stdout, "[%d]", get_compound_graph_path_array_index(path, j));
							}
							printf("\n");
						}
						*/

						c = get_compound_ent_value_by_path(ent, path);
						free_compound_graph_path(path);

						/* printf("  cons: "); DDMN(c); */

						if (info->projs[pn_Load_M]) {
							exchange(info->projs[pn_Load_M], mem);
							res |= DF_CHANGED;
						}
						if (info->projs[pn_Load_res]) {
							exchange(info->projs[pn_Load_res], copy_const_value(get_irn_dbg_info(load), c));
							res |= DF_CHANGED;
						}
						exchange(load, new_Bad());
						reduce_adr_usage(ptr);
						return res;
					} else {
						/*  We can not determine a correct access path.  E.g., in jack, we load
						a byte from an object to generate an exception.   Happens in test program
						Reflectiontest.
						printf(">>>>>>>>>>>>> Found access to constant entity %s in function %s\n", get_entity_name(ent),
						get_entity_name(get_irg_entity(current_ir_graph)));
						printf("  load: "); DDMN(load);
						printf("  ptr:  "); DDMN(ptr);
						*/
					}
				}
			}
		}
	}

	/* Check, if the address of this load is used more than once.
	 * If not, this load cannot be removed in any case. */
	if (get_irn_n_uses(ptr) <= 1)
		return res;

	/*
	 * follow the memory chain as long as there are only Loads
	 * and try to replace current Load or Store by a previous one.
	 * Note that in unreachable loops it might happen that we reach
	 * load again, as well as we can fall into a cycle.
	 * We break such cycles using a special visited flag.
	 */
	INC_MASTER();
	res = follow_Mem_chain(load, skip_Proj(mem));
	return res;
}  /* optimize_load */

/**
 * Check whether a value of mode new_mode would completely overwrite a value
 * of mode old_mode in memory.
 */
static int is_completely_overwritten(ir_mode *old_mode, ir_mode *new_mode)
{
	return get_mode_size_bits(new_mode) >= get_mode_size_bits(old_mode);
}  /* is_completely_overwritten */

/**
 * follow the memory chain as long as there are only Loads and alias free Stores.
 *
 * INC_MASTER() must be called before dive into
 */
static unsigned follow_Mem_chain_for_Store(ir_node *store, ir_node *curr) {
	unsigned res = 0;
	ldst_info_t *info = get_irn_link(store);
	ir_node *pred;
	ir_node *ptr = get_Store_ptr(store);
	ir_node *mem = get_Store_mem(store);
	ir_node *value = get_Store_value(store);
	ir_mode *mode  = get_irn_mode(value);
	ir_node *block = get_nodes_block(store);
	ir_node *mblk  = get_Block_MacroBlock(block);

	for (pred = curr; pred != store;) {
		ldst_info_t *pred_info = get_irn_link(pred);

		/*
		 * BEWARE: one might think that checking the modes is useless, because
		 * if the pointers are identical, they refer to the same object.
		 * This is only true in strong typed languages, not is C were the following
		 * is possible *(ir_type1 *)p = a; *(ir_type2 *)p = b ...
		 * However, if the mode that is written have a bigger  or equal size the the old
		 * one, the old value is completely overwritten and can be killed ...
		 */
		if (is_Store(pred) && get_Store_ptr(pred) == ptr &&
		    get_nodes_MacroBlock(pred) == mblk &&
		    is_completely_overwritten(get_irn_mode(get_Store_value(pred)), mode)) {
			/*
			 * a Store after a Store in the same block -- a write after write.
			 * We may remove the first Store, if it does not have an exception handler.
			 *
			 * TODO: What, if both have the same exception handler ???
			 */
			if (get_Store_volatility(pred) != volatility_is_volatile && !pred_info->projs[pn_Store_X_except]) {
				DBG_OPT_WAW(pred, store);
				exchange(pred_info->projs[pn_Store_M], get_Store_mem(pred));
				exchange(pred, new_Bad());
				reduce_adr_usage(ptr);
				return DF_CHANGED;
			}
		} else if (is_Load(pred) && get_Load_ptr(pred) == ptr &&
		           value == pred_info->projs[pn_Load_res]) {
			/*
			 * a Store of a value after a Load -- a write after read.
			 * We may remove the second Store, if it does not have an exception handler.
			 */
			if (! info->projs[pn_Store_X_except]) {
				DBG_OPT_WAR(store, pred);
				exchange(info->projs[pn_Store_M], mem);
				exchange(store, new_Bad());
				reduce_adr_usage(ptr);
				return DF_CHANGED;
			}
		}

		if (is_Store(pred)) {
			/* check if we can pass thru this store */
			ir_alias_relation rel = get_alias_relation(
				current_ir_graph,
				get_Store_ptr(pred),
				get_irn_mode(get_Store_value(pred)),
				ptr, mode);
			/* if the might be an alias, we cannot pass this Store */
			if (rel != no_alias)
				break;
			pred = skip_Proj(get_Store_mem(pred));
		} else if (get_irn_op(pred) == op_Load) {
			pred = skip_Proj(get_Load_mem(pred));
		} else {
			/* follow only Load chains */
			break;
		}

		/* check for cycles */
		if (NODE_VISITED(pred_info))
			break;
		MARK_NODE(pred_info);
	}

	if (is_Sync(pred)) {
		int i;

		/* handle all Sync predecessors */
		for (i = get_Sync_n_preds(pred) - 1; i >= 0; --i) {
			res |= follow_Mem_chain_for_Store(store, skip_Proj(get_Sync_pred(pred, i)));
			if (res)
				break;
		}
	}
	return res;
}  /* follow_Mem_chain_for_Store */

/**
 * optimize a Store
 *
 * @param store  the Store node
 */
static unsigned optimize_store(ir_node *store) {
	ir_node *ptr, *mem;

	if (get_Store_volatility(store) == volatility_is_volatile)
		return 0;

	ptr = get_Store_ptr(store);

	/* Check, if the address of this Store is used more than once.
	 * If not, this Store cannot be removed in any case. */
	if (get_irn_n_uses(ptr) <= 1)
		return 0;

	mem = get_Store_mem(store);

	/* follow the memory chain as long as there are only Loads */
	INC_MASTER();
	return follow_Mem_chain_for_Store(store, skip_Proj(mem));
}  /* optimize_store */

/**
 * walker, optimizes Phi after Stores to identical places:
 * Does the following optimization:
 * @verbatim
 *
 *   val1   val2   val3          val1  val2  val3
 *    |      |      |               \    |    /
 *  Store  Store  Store              \   |   /
 *      \    |    /                   PhiData
 *       \   |   /                       |
 *        \  |  /                      Store
 *          PhiM
 *
 * @endverbatim
 * This reduces the number of stores and allows for predicated execution.
 * Moves Stores back to the end of a function which may be bad.
 *
 * This is only possible if the predecessor blocks have only one successor.
 */
static unsigned optimize_phi(ir_node *phi, walk_env_t *wenv)
{
	int i, n;
	ir_node *store, *old_store, *ptr, *block, *phi_block, *phiM, *phiD, *exc, *projM;
	ir_mode *mode;
	ir_node **inM, **inD, **projMs;
	int *idx;
	dbg_info *db = NULL;
	ldst_info_t *info;
	block_info_t *bl_info;
	unsigned res = 0;

	/* Must be a memory Phi */
	if (get_irn_mode(phi) != mode_M)
		return 0;

	n = get_Phi_n_preds(phi);
	if (n <= 0)
		return 0;

	/* must be only one user */
	projM = get_Phi_pred(phi, 0);
	if (get_irn_n_edges(projM) != 1)
		return 0;

	store = skip_Proj(projM);
	old_store = store;
	if (get_irn_op(store) != op_Store)
		return 0;

	block = get_nodes_block(store);

	/* abort on dead blocks */
	if (is_Block_dead(block))
		return 0;

	/* check if the block is post dominated by Phi-block
	   and has no exception exit */
	bl_info = get_irn_link(block);
	if (bl_info->flags & BLOCK_HAS_EXC)
		return 0;

	phi_block = get_nodes_block(phi);
	if (! block_strictly_postdominates(phi_block, block))
		return 0;

	/* this is the address of the store */
	ptr  = get_Store_ptr(store);
	mode = get_irn_mode(get_Store_value(store));
	info = get_irn_link(store);
	exc  = info->exc_block;

	for (i = 1; i < n; ++i) {
		ir_node *pred = get_Phi_pred(phi, i);

		if (get_irn_n_edges(pred) != 1)
			return 0;

		pred = skip_Proj(pred);
		if (!is_Store(pred))
			return 0;

		if (ptr != get_Store_ptr(pred) || mode != get_irn_mode(get_Store_value(pred)))
			return 0;

		info = get_irn_link(pred);

		/* check, if all stores have the same exception flow */
		if (exc != info->exc_block)
			return 0;

		/* abort on dead blocks */
		block = get_nodes_block(pred);
		if (is_Block_dead(block))
			return 0;

		/* check if the block is post dominated by Phi-block
		   and has no exception exit. Note that block must be different from
		   Phi-block, else we would move a Store from end End of a block to its
		   Start... */
		bl_info = get_irn_link(block);
		if (bl_info->flags & BLOCK_HAS_EXC)
			return 0;
		if (block == phi_block || ! block_postdominates(phi_block, block))
			return 0;
	}

	/*
	 * ok, when we are here, we found all predecessors of a Phi that
	 * are Stores to the same address and size. That means whatever
	 * we do before we enter the block of the Phi, we do a Store.
	 * So, we can move the Store to the current block:
	 *
	 *   val1    val2    val3          val1  val2  val3
	 *    |       |       |               \    |    /
	 * | Str | | Str | | Str |             \   |   /
	 *      \     |     /                   PhiData
	 *       \    |    /                       |
	 *        \   |   /                       Str
	 *           PhiM
	 *
	 * Is only allowed if the predecessor blocks have only one successor.
	 */

	NEW_ARR_A(ir_node *, projMs, n);
	NEW_ARR_A(ir_node *, inM, n);
	NEW_ARR_A(ir_node *, inD, n);
	NEW_ARR_A(int, idx, n);

	/* Prepare: Collect all Store nodes.  We must do this
	   first because we otherwise may loose a store when exchanging its
	   memory Proj.
	 */
	for (i = n - 1; i >= 0; --i) {
		ir_node *store;

		projMs[i] = get_Phi_pred(phi, i);
		assert(is_Proj(projMs[i]));

		store = get_Proj_pred(projMs[i]);
		info  = get_irn_link(store);

		inM[i] = get_Store_mem(store);
		inD[i] = get_Store_value(store);
		idx[i] = info->exc_idx;
	}
	block = get_nodes_block(phi);

	/* second step: create a new memory Phi */
	phiM = new_rd_Phi(get_irn_dbg_info(phi), current_ir_graph, block, n, inM, mode_M);

	/* third step: create a new data Phi */
	phiD = new_rd_Phi(get_irn_dbg_info(phi), current_ir_graph, block, n, inD, mode);

	/* rewire memory and kill the node */
	for (i = n - 1; i >= 0; --i) {
		ir_node *proj  = projMs[i];

		if(is_Proj(proj)) {
			ir_node *store = get_Proj_pred(proj);
			exchange(proj, inM[i]);
			kill_node(store);
		}
	}

	/* fourth step: create the Store */
	store = new_rd_Store(db, current_ir_graph, block, phiM, ptr, phiD);
#ifdef DO_CACHEOPT
	co_set_irn_name(store, co_get_irn_ident(old_store));
#endif

	projM = new_rd_Proj(NULL, current_ir_graph, block, store, mode_M, pn_Store_M);

	info = get_ldst_info(store, &wenv->obst);
	info->projs[pn_Store_M] = projM;

	/* fifths step: repair exception flow */
	if (exc) {
		ir_node *projX = new_rd_Proj(NULL, current_ir_graph, block, store, mode_X, pn_Store_X_except);

		info->projs[pn_Store_X_except] = projX;
		info->exc_block                = exc;
		info->exc_idx                  = idx[0];

		for (i = 0; i < n; ++i) {
			set_Block_cfgpred(exc, idx[i], projX);
		}

		if (n > 1) {
			/* the exception block should be optimized as some inputs are identical now */
		}

		res |= CF_CHANGED;
	}

	/* sixth step: replace old Phi */
	exchange(phi, projM);

	return res | DF_CHANGED;
}  /* optimize_phi */

/**
 * walker, do the optimizations
 */
static void do_load_store_optimize(ir_node *n, void *env) {
	walk_env_t *wenv = env;

	switch (get_irn_opcode(n)) {

	case iro_Load:
		wenv->changes |= optimize_load(n);
		break;

	case iro_Store:
		wenv->changes |= optimize_store(n);
		break;

	case iro_Phi:
		wenv->changes |= optimize_phi(n, wenv);
		break;

	default:
		;
	}
}  /* do_load_store_optimize */

/** A scc. */
typedef struct scc {
	ir_node *head;		/**< the head of the list */
} scc;

/** A node entry. */
typedef struct node_entry {
	unsigned DFSnum;    /**< the DFS number of this node */
	unsigned low;       /**< the low number of this node */
	ir_node  *header;   /**< the header of this node */
	int      in_stack;  /**< flag, set if the node is on the stack */
	ir_node  *next;     /**< link to the next node the the same scc */
	scc      *pscc;     /**< the scc of this node */
	unsigned POnum;     /**< the post order number for blocks */
} node_entry;

/** A loop entry. */
typedef struct loop_env {
	ir_phase ph;           /**< the phase object */
	ir_node  **stack;      /**< the node stack */
	int      tos;          /**< tos index */
	unsigned nextDFSnum;   /**< the current DFS number */
	unsigned POnum;        /**< current post order number */

	unsigned changes;      /**< a bitmask of graph changes */
} loop_env;

/**
* Gets the node_entry of a node
*/
static node_entry *get_irn_ne(ir_node *irn, loop_env *env) {
	ir_phase   *ph = &env->ph;
	node_entry *e  = phase_get_irn_data(&env->ph, irn);

	if (! e) {
		e = phase_alloc(ph, sizeof(*e));
		memset(e, 0, sizeof(*e));
		phase_set_irn_data(ph, irn, e);
	}
	return e;
}  /* get_irn_ne */

/**
 * Push a node onto the stack.
 *
 * @param env   the loop environment
 * @param n     the node to push
 */
static void push(loop_env *env, ir_node *n) {
	node_entry *e;

	if (env->tos == ARR_LEN(env->stack)) {
		int nlen = ARR_LEN(env->stack) * 2;
		ARR_RESIZE(ir_node *, env->stack, nlen);
	}
	env->stack[env->tos++] = n;
	e = get_irn_ne(n, env);
	e->in_stack = 1;
}  /* push */

/**
 * pop a node from the stack
 *
 * @param env   the loop environment
 *
 * @return  The topmost node
 */
static ir_node *pop(loop_env *env) {
	ir_node *n = env->stack[--env->tos];
	node_entry *e = get_irn_ne(n, env);

	e->in_stack = 0;
	return n;
}  /* pop */

/**
 * Check if irn is a region constant.
 * The block or irn must strictly dominate the header block.
 *
 * @param irn           the node to check
 * @param header_block  the header block of the induction variable
 */
static int is_rc(ir_node *irn, ir_node *header_block) {
	ir_node *block = get_nodes_block(irn);

	return (block != header_block) && block_dominates(block, header_block);
}  /* is_rc */

typedef struct phi_entry phi_entry;
struct phi_entry {
	ir_node   *phi;    /**< A phi with a region const memory. */
	int       pos;     /**< The position of the region const memory */
	ir_node   *load;   /**< the newly created load for this phi */
	phi_entry *next;
};

/**
 * Move loops out of loops if possible
 */
static void move_loads_in_loops(scc *pscc, loop_env *env) {
	ir_node   *phi, *load, *next, *other, *next_other;
	ir_entity *ent;
	int       j;
	phi_entry *phi_list = NULL;

	/* collect all outer memories */
	for (phi = pscc->head; phi != NULL; phi = next) {
		node_entry *ne = get_irn_ne(phi, env);
		next = ne->next;

		/* check all memory Phi's */
		if (! is_Phi(phi))
			continue;

		assert(get_irn_mode(phi) == mode_M && "DFS geturn non-memory Phi");

		for (j = get_irn_arity(phi) - 1; j >= 0; --j) {
			ir_node    *pred = get_irn_n(phi, j);
			node_entry *pe   = get_irn_ne(pred, env);

			if (pe->pscc != ne->pscc) {
				/* not in the same SCC, is region const */
				phi_entry *pe = phase_alloc(&env->ph, sizeof(*pe));

				pe->phi  = phi;
				pe->pos  = j;
				pe->next = phi_list;
				phi_list = pe;
			}
		}
	}
	/* no Phis no fun */
	assert(phi_list != NULL && "DFS found a loop without Phi");

	for (load = pscc->head; load; load = next) {
		ir_mode *load_mode;
		node_entry *ne = get_irn_ne(load, env);
		next = ne->next;

		if (is_Load(load)) {
			ldst_info_t *info = get_irn_link(load);
			ir_node     *ptr = get_Load_ptr(load);

			/* for now, we cannot handle Loads with exceptions */
			if (info->projs[pn_Load_res] == NULL || info->projs[pn_Load_X_regular] != NULL || info->projs[pn_Load_X_except] != NULL)
				continue;

			/* for now, we can only handle Load(SymConst) */
			if (! is_SymConst(ptr) || get_SymConst_kind(ptr) != symconst_addr_ent)
				continue;
			ent = get_SymConst_entity(ptr);
			load_mode = get_Load_mode(load);
			for (other = pscc->head; other != NULL; other = next_other) {
				node_entry *ne = get_irn_ne(other, env);
				next_other = ne->next;

				if (is_Store(other)) {
					ir_alias_relation rel = get_alias_relation(
						current_ir_graph,
						get_Store_ptr(other),
						get_irn_mode(get_Store_value(other)),
						ptr, load_mode);
					/* if the might be an alias, we cannot pass this Store */
					if (rel != no_alias)
						break;
				}
			}
			if (other == NULL) {
				ldst_info_t *ninfo;
				phi_entry   *pe;
				dbg_info    *db;

				/* for now, we cannot handle more than one input */
				if (phi_list->next != NULL)
					return;

				/* yep, no aliasing Store found, Load can be moved */
				DB((dbg, LEVEL_1, "  Found a Load that could be moved: %+F\n", load));

				db   = get_irn_dbg_info(load);
				for (pe = phi_list; pe != NULL; pe = pe->next) {
					int     pos   = pe->pos;
					ir_node *phi  = pe->phi;
					ir_node *blk  = get_nodes_block(phi);
					ir_node *pred = get_Block_cfgpred_block(blk, pos);
					ir_node *irn, *mem;

					pe->load = irn = new_rd_Load(db, current_ir_graph, pred, get_Phi_pred(phi, pos), ptr, load_mode);
					ninfo = get_ldst_info(irn, phase_obst(&env->ph));

					ninfo->projs[pn_Load_M] = mem = new_r_Proj(current_ir_graph, pred, irn, mode_M, pn_Load_M);
					set_Phi_pred(phi, pos, mem);

					ninfo->projs[pn_Load_res] = new_r_Proj(current_ir_graph, pred, irn, load_mode, pn_Load_res);

					DB((dbg, LEVEL_1, "  Created %+F in %+F\n", irn, pred));
				}

				/* now kill the old Load */
				exchange(info->projs[pn_Load_M], get_Load_mem(load));
				exchange(info->projs[pn_Load_res], ninfo->projs[pn_Load_res]);

				env->changes |= DF_CHANGED;
			}
		}
	}
}  /* move_loads_in_loops */

/**
 * Process a loop SCC.
 *
 * @param pscc  the SCC
 * @param env   the loop environment
 */
static void process_loop(scc *pscc, loop_env *env) {
	ir_node *irn, *next, *header = NULL;
	node_entry *b, *h = NULL;
	int j, only_phi, num_outside, process = 0;
	ir_node *out_rc;

	/* find the header block for this scc */
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		ir_node *block = get_nodes_block(irn);

		next = e->next;
		b = get_irn_ne(block, env);

		if (header) {
			if (h->POnum < b->POnum) {
				header = block;
				h      = b;
			}
		}
		else {
			header = block;
			h      = b;
		}
	}

	/* check if this scc contains only Phi, Loads or Stores nodes */
	only_phi    = 1;
	num_outside = 0;
	out_rc      = NULL;
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);

		next = e->next;
		switch (get_irn_opcode(irn)) {
		case iro_Call:
		case iro_CopyB:
			/* cannot handle Calls or CopyB yet */
			goto fail;
		case iro_Load:
			process = 1;
			if (get_Load_volatility(irn) == volatility_is_volatile) {
				/* cannot handle loops with volatile Loads */
				goto fail;
			}
			only_phi = 0;
			break;
		case iro_Store:
			if (get_Store_volatility(irn) == volatility_is_volatile) {
				/* cannot handle loops with volatile Stores */
				goto fail;
			}
			only_phi = 0;
			break;
		default:
			only_phi = 0;
			break;
		case iro_Phi:
			for (j = get_irn_arity(irn) - 1; j >= 0; --j) {
				ir_node *pred  = get_irn_n(irn, j);
				node_entry *pe = get_irn_ne(pred, env);

				if (pe->pscc != e->pscc) {
					/* not in the same SCC, must be a region const */
					if (! is_rc(pred, header)) {
						/* not a memory loop */
						goto fail;
					}
					if (! out_rc) {
						out_rc = pred;
						++num_outside;
					} else if (out_rc != pred) {
						++num_outside;
					}
				}
			}
			break;
		}
	}
	if (! process)
		goto fail;

	/* found a memory loop */
	DB((dbg, LEVEL_2, "  Found a memory loop:\n  "));
	if (only_phi && num_outside == 1) {
		/* a phi cycle with only one real predecessor can be collapsed */
		DB((dbg, LEVEL_2, "  Found an USELESS Phi cycle:\n  "));

		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = get_irn_ne(irn, env);
			next = e->next;
			e->header = NULL;
			exchange(irn, out_rc);
		}
		env->changes |= DF_CHANGED;
		return;
	}

	/* set the header for every node in this scc */
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		e->header = header;
		next = e->next;
		DB((dbg, LEVEL_2, " %+F,", irn));
	}
	DB((dbg, LEVEL_2, "\n"));

	move_loads_in_loops(pscc, env);

fail:
	;
}  /* process_loop */

/**
 * Process a SCC.
 *
 * @param pscc  the SCC
 * @param env   the loop environment
 */
static void process_scc(scc *pscc, loop_env *env) {
	ir_node *head = pscc->head;
	node_entry *e = get_irn_ne(head, env);

#ifdef DEBUG_libfirm
	{
		ir_node *irn, *next;

		DB((dbg, LEVEL_4, " SCC at %p:\n ", pscc));
		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = get_irn_ne(irn, env);

			next = e->next;

			DB((dbg, LEVEL_4, " %+F,", irn));
		}
		DB((dbg, LEVEL_4, "\n"));
	}
#endif

	if (e->next != NULL) {
		/* this SCC has more than one member */
		process_loop(pscc, env);
	}
}  /* process_scc */

/**
 * Do Tarjan's SCC algorithm and drive load/store optimization.
 *
 * @param irn  start at this node
 * @param env  the loop environment
 */
static void dfs(ir_node *irn, loop_env *env)
{
	int i, n;
	node_entry *node = get_irn_ne(irn, env);

	mark_irn_visited(irn);

	node->DFSnum = env->nextDFSnum++;
	node->low    = node->DFSnum;
	push(env, irn);

	/* handle preds */
	if (is_Phi(irn) || is_Sync(irn)) {
		n = get_irn_arity(irn);
		for (i = 0; i < n; ++i) {
			ir_node *pred = get_irn_n(irn, i);
			node_entry *o = get_irn_ne(pred, env);

			if (irn_not_visited(pred)) {
				dfs(pred, env);
				node->low = MIN(node->low, o->low);
			}
			if (o->DFSnum < node->DFSnum && o->in_stack)
				node->low = MIN(o->DFSnum, node->low);
		}
	} else if (is_fragile_op(irn)) {
		ir_node *pred = get_fragile_op_mem(irn);
		node_entry *o = get_irn_ne(pred, env);

		if (irn_not_visited(pred)) {
			dfs(pred, env);
			node->low = MIN(node->low, o->low);
		}
		if (o->DFSnum < node->DFSnum && o->in_stack)
			node->low = MIN(o->DFSnum, node->low);
	} else if (is_Proj(irn)) {
		ir_node *pred = get_Proj_pred(irn);
		node_entry *o = get_irn_ne(pred, env);

		if (irn_not_visited(pred)) {
			dfs(pred, env);
			node->low = MIN(node->low, o->low);
		}
		if (o->DFSnum < node->DFSnum && o->in_stack)
			node->low = MIN(o->DFSnum, node->low);
	}
	else {
		 /* IGNORE predecessors */
	}

	if (node->low == node->DFSnum) {
		scc *pscc = phase_alloc(&env->ph, sizeof(*pscc));
		ir_node *x;

		pscc->head = NULL;
		do {
			node_entry *e;

			x = pop(env);
			e = get_irn_ne(x, env);
			e->pscc    = pscc;
			e->next    = pscc->head;
			pscc->head = x;
		} while (x != irn);

		process_scc(pscc, env);
	}
}  /* dfs */

/**
 * Do the DFS on the memory edges a graph.
 *
 * @param irg  the graph to process
 * @param env  the loop environment
 */
static void do_dfs(ir_graph *irg, loop_env *env) {
	ir_graph *rem = current_ir_graph;
	ir_node  *endblk, *end;
	int      i;

	current_ir_graph = irg;
	inc_irg_visited(irg);

	/* visit all memory nodes */
	endblk = get_irg_end_block(irg);
	for (i = get_Block_n_cfgpreds(endblk) - 1; i >= 0; --i) {
		ir_node *pred = get_Block_cfgpred(endblk, i);

		if (is_Return(pred))
			dfs(get_Return_mem(pred), env);
		else if (is_Raise(pred))
			dfs(get_Raise_mem(pred), env);
		else if (is_fragile_op(pred))
			dfs(get_fragile_op_mem(pred), env);
		else {
			assert(0 && "Unknown EndBlock predecessor");
		}
	}

	/* visit the keep-alives */
	end = get_irg_end(irg);
	for (i = get_End_n_keepalives(end) - 1; i >= 0; --i) {
		ir_node *ka = get_End_keepalive(end, i);

		if (is_Phi(ka) && irn_not_visited(ka))
			dfs(ka, env);
	}
	current_ir_graph = rem;
}  /* do_dfs */

/**
 * Initialize new phase data. We do this always explicit, so return NULL here
 */
static void *init_loop_data(ir_phase *ph, ir_node *irn, void *data) {
	(void)ph;
	(void)irn;
	(void)data;
	return NULL;
}  /* init_loop_data */

/**
 * Optimize Loads/Stores in loops.
 *
 * @param irg  the graph
 */
static int optimize_loops(ir_graph *irg) {
	loop_env env;

	env.stack         = NEW_ARR_F(ir_node *, 128);
	env.tos           = 0;
	env.nextDFSnum    = 0;
	env.POnum         = 0;
	env.changes       = 0;
	phase_init(&env.ph, "ldstopt", irg, PHASE_DEFAULT_GROWTH, init_loop_data, NULL);

	/* calculate the SCC's and drive loop optimization. */
	do_dfs(irg, &env);

	DEL_ARR_F(env.stack);
	phase_free(&env.ph);

	return env.changes;
}  /* optimize_loops */

/*
 * do the load store optimization
 */
void optimize_load_store(ir_graph *irg) {
	walk_env_t env;

	FIRM_DBG_REGISTER(dbg, "firm.opt.ldstopt");

	assert(get_irg_phase_state(irg) != phase_building);
	assert(get_irg_pinned(irg) != op_pin_state_floats &&
		"LoadStore optimization needs pinned graph");

	/* we need landing pads */
	remove_critical_cf_edges(irg);

	edges_assure(irg);

	/* for Phi optimization post-dominators are needed ... */
	assure_postdoms(irg);

	if (get_opt_alias_analysis()) {
		assure_irg_address_taken_computed(irg);
		assure_irp_globals_address_taken_computed();
	}

	obstack_init(&env.obst);
	env.changes = 0;

	/* init the links, then collect Loads/Stores/Proj's in lists */
	master_visited = 0;
	irg_walk_graph(irg, firm_clear_link, collect_nodes, &env);

	/* now we have collected enough information, optimize */
	irg_walk_graph(irg, NULL, do_load_store_optimize, &env);

	env.changes |= optimize_loops(irg);

	obstack_free(&env.obst, NULL);

	/* Handle graph state */
	if (env.changes) {
		set_irg_outs_inconsistent(irg);
	}

	if (env.changes & CF_CHANGED) {
		/* is this really needed: Yes, control flow changed, block might
		have Bad() predecessors. */
		set_irg_doms_inconsistent(irg);
	}
}  /* optimize_load_store */
