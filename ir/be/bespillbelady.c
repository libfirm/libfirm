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
 * @brief       Beladys spillalgorithm.
 * @author      Daniel Grund, Matthias Braun
 * @date        20.09.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "obst.h"
#include "set.h"
#include "pset.h"
#include "irprintf_t.h"
#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "irgwalk.h"
#include "irloop.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "beutil.h"
#include "bearch_t.h"
#include "bespillbelady.h"
#include "beuses.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bechordal_t.h"
#include "bespilloptions.h"
#include "beloopana.h"
#include "beirg_t.h"
#include "bemodule.h"

#define DBG_SPILL   1
#define DBG_WSETS   2
#define DBG_FIX     4
#define DBG_DECIDE  8
#define DBG_START  16
#define DBG_SLOTS  32
#define DBG_TRACE  64
#define DBG_WORKSET 128
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An association between a node and a point in time.
 */
typedef struct _loc_t {
	ir_node *irn;            /**< A node. */
	unsigned time;           /**< A use time (see beuses.h). */
	int      reloaded_value; /**< the value is a reloaded value */
} loc_t;

typedef struct _workset_t {
	int len;			/**< current length */
	loc_t vals[0];		/**< inlined array of the values/distances in this working set */
} workset_t;

typedef struct _belady_env_t {
	struct obstack ob;
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	be_lv_t *lv;
	be_loopana_t *loop_ana;
	int n_regs;			/** number of regs in this reg-class */

	workset_t *ws;		/**< the main workset used while processing a block. ob-allocated */
	be_uses_t *uses;	/**< env for the next-use magic */
	ir_node *instr;		/**< current instruction */
	unsigned instr_nr;	/**< current instruction number (relative to block start) */
	pset *used;

	spill_env_t *senv;	/**< see bespill.h */
} belady_env_t;

static int loc_compare(const void *a, const void *b)
{
	const loc_t *p = a;
	const loc_t *q = b;
	return p->time - q->time;
}

/* debug helper */
static void workset_print(const workset_t *w)
{
	int i;

	for(i = 0; i < w->len; ++i) {
		ir_fprintf(stderr, "%+F %d (%d)\n", w->vals[i].irn, w->vals[i].time, w->vals[i].reloaded_value);
	}
	/* avoid unused warning */
	(void) workset_print;
}

/**
 * Alloc a new workset on obstack @p ob with maximum size @p max
 */
static INLINE workset_t *new_workset(belady_env_t *env, struct obstack *ob) {
	workset_t *res;
	size_t size = sizeof(*res) + (env->n_regs)*sizeof(res->vals[0]);
	res = obstack_alloc(ob, size);
	memset(res, 0, size);
	return res;
}

/**
 * Alloc a new instance on obstack and make it equal to @param ws
 */
static INLINE workset_t *workset_clone(belady_env_t *env, struct obstack *ob, workset_t *ws) {
	workset_t *res;
	size_t size = sizeof(*res) + (env->n_regs)*sizeof(res->vals[0]);
	res = obstack_alloc(ob, size);
	memcpy(res, ws, size);
	return res;
}

/**
 * Do NOT alloc anything. Make @param tgt equal to @param src.
 * returns @param tgt for convenience
 */
static INLINE workset_t *workset_copy(belady_env_t *env, workset_t *tgt, workset_t *src) {
	size_t size = sizeof(*src) + (env->n_regs)*sizeof(src->vals[0]);
	memcpy(tgt, src, size);
	return tgt;
}

/**
 * Overwrites the current content array of @param ws with the
 * @param count locations given at memory @param locs.
 * Set the length of @param ws to count.
 */
static INLINE void workset_bulk_fill(workset_t *workset, int count, const loc_t *locs) {
	workset->len = count;
	memcpy(&(workset->vals[0]), locs, count * sizeof(locs[0]));
}

/**
 * Inserts the value @p val into the workset, iff it is not
 * already contained. The workset must not be full.
 */
static INLINE void workset_insert(belady_env_t *env, workset_t *ws,
                                  ir_node *val, int reloaded_value)
{
	int i;
	/* check for current regclass */
	if (!arch_irn_consider_in_reg_alloc(env->arch, env->cls, val)) {
		//DBG((dbg, DBG_WORKSET, "Skipped %+F\n", val));
		return;
	}

	/* check if val is already contained */
	for(i=0; i<ws->len; ++i) {
		if (ws->vals[i].irn == val) {
			if(!ws->vals[i].reloaded_value)
				ws->vals[i].reloaded_value = reloaded_value;
			return;
		}
	}

	/* insert val */
	assert(ws->len < env->n_regs && "Workset already full!");
	ws->vals[ws->len].irn            = val;
	ws->vals[ws->len].reloaded_value = reloaded_value;
	ws->len++;
}

/**
 * Removes all entries from this workset
 */
static INLINE void workset_clear(workset_t *ws) {
	ws->len = 0;
}

/**
 * Removes the value @p val from the workset if present.
 */
static INLINE void workset_remove(workset_t *ws, ir_node *val) {
	int i;
	for(i=0; i<ws->len; ++i) {
		if (ws->vals[i].irn == val) {
			ws->vals[i] = ws->vals[--ws->len];
			return;
		}
	}
}

static INLINE int workset_contains(const workset_t *ws, const ir_node *val) {
	int i;
	for(i=0; i<ws->len; ++i) {
		if (ws->vals[i].irn == val)
			return 1;
	}

	return 0;
}

/**
 * Iterates over all values in the working set.
 * @p ws The workset to iterate
 * @p v  A variable to put the current value in
 * @p i  An integer for internal use
 */
#define workset_foreach(ws, v, i)	for(i=0; \
										v=(i < ws->len) ? ws->vals[i].irn : NULL, i < ws->len; \
										++i)

#define workset_set_time(ws, i, t) (ws)->vals[i].time=t
#define workset_get_time(ws, i) (ws)->vals[i].time
#define workset_set_length(ws, length) (ws)->len = length
#define workset_get_length(ws) ((ws)->len)
#define workset_get_val(ws, i) ((ws)->vals[i].irn)
#define workset_sort(ws) qsort((ws)->vals, (ws)->len, sizeof((ws)->vals[0]), loc_compare);

typedef struct _block_info_t {
	workset_t *ws_start, *ws_end;
	int processed;
} block_info_t;


static INLINE void *new_block_info(struct obstack *ob) {
	block_info_t *res = obstack_alloc(ob, sizeof(*res));
	res->ws_start = NULL;
	res->ws_end = NULL;
	res->processed = 0;

	return res;
}

#define get_block_info(block)        ((block_info_t *)get_irn_link(block))
#define set_block_info(block, info)  set_irn_link(block, info)

/**
 * @return The distance to the next use or 0 if irn has dont_spill flag set
 */
static INLINE unsigned get_distance(belady_env_t *env, ir_node *from, unsigned from_step, const ir_node *def, int skip_from_uses)
{
	be_next_use_t use;
	int flags = arch_irn_get_flags(env->arch, def);

	assert(! (flags & arch_irn_flags_ignore));

	use = be_get_next_use(env->uses, from, from_step, def, skip_from_uses);
	if(USES_IS_INFINITE(use.time))
		return USES_INFINITY;

	/* We have to keep nonspillable nodes in the workingset */
	if(flags & arch_irn_flags_dont_spill)
		return 0;

	return use.time;
}

/**
 * Performs the actions necessary to grant the request that:
 * - new_vals can be held in registers
 * - as few as possible other values are disposed
 * - the worst values get disposed
 *
 * @p is_usage indicates that the values in new_vals are used (not defined)
 * In this case reloads must be performed
 */
static void displace(belady_env_t *env, workset_t *new_vals, int is_usage) {
	ir_node *val;
	int     i, len, max_allowed, demand, iter;

	workset_t *ws         = env->ws;
	ir_node   **to_insert = alloca(env->n_regs * sizeof(*to_insert));

	/*
		1. Identify the number of needed slots and the values to reload
	*/
	demand = 0;
	workset_foreach(new_vals, val, iter) {
		/* mark value as used */
		if (is_usage)
			pset_insert_ptr(env->used, val);

		if (! workset_contains(ws, val)) {
			DBG((dbg, DBG_DECIDE, "    insert %+F\n", val));

			to_insert[demand++] = val;
			if (is_usage) {
				DBG((dbg, DBG_SPILL, "Reload %+F before %+F\n", val, env->instr));
				be_add_reload(env->senv, val, env->instr, env->cls, 1);
			}
		} else {
			DBG((dbg, DBG_DECIDE, "    %+F already in workset\n", val));
			assert(is_usage);
		}
	}
	//DBG((dbg, DBG_DECIDE, "    demand = %d\n", demand));

	/*
		2. Make room for at least 'demand' slots
	*/
	len         = workset_get_length(ws);
	max_allowed = env->n_regs - demand;

	/* Only make more free room if we do not have enough */
	if (len > max_allowed) {
		DBG((dbg, DBG_DECIDE, "    disposing %d values\n", ws->len - max_allowed));

		/* get current next-use distance */
		for (i = 0; i < ws->len; ++i) {
			unsigned dist = get_distance(env, env->instr, env->instr_nr, workset_get_val(ws, i), !is_usage);
			workset_set_time(ws, i, dist);
		}

		/* sort entries by increasing nextuse-distance*/
		workset_sort(ws);

		/*
			Logic for not needed live-ins: If a value is disposed
			before its first usage, remove it from start workset
			We don't do this for phis though
		*/
		for (i = max_allowed; i < ws->len; ++i) {
			ir_node *irn = ws->vals[i].irn;

			DBG((dbg, DBG_DECIDE, "    disposing %+F (%u)\n", irn,
			     workset_get_time(ws, i)));

			if(!USES_IS_INFINITE(ws->vals[i].time)
					&& !ws->vals[i].reloaded_value) {
				be_add_spill(env->senv, irn, env->instr);
			}

            if (is_Phi(irn))
                continue;

			if (! pset_find_ptr(env->used, irn)) {
				ir_node   *curr_bb  = get_nodes_block(env->instr);
				workset_t *ws_start = get_block_info(curr_bb)->ws_start;
				workset_remove(ws_start, irn);

				DBG((dbg, DBG_DECIDE, "    (and removing %+F from start workset)\n", irn));
			}
		}

		/* kill the last 'demand' entries in the array */
		workset_set_length(ws, max_allowed);
	}

	/*
		3. Insert the new values into the workset
	*/
	for (i = 0; i < demand; ++i) {
		workset_insert(env, env->ws, to_insert[i], 1);
	}
}

static void belady(ir_node *block, void *env);

/** Decides whether a specific node should be in the start workset or not
 *
 * @param env      belady environment
 * @param first
 * @param node     the node to test
 * @param block    the block of the node
 * @param loop     the loop of the node
 */
static loc_t to_take_or_not_to_take(belady_env_t *env, ir_node* first,
		                            ir_node *node, ir_node *block,
									ir_loop *loop)
{
	be_next_use_t next_use;
	loc_t loc;
	loc.time = USES_INFINITY;
	loc.irn = node;
	(void) block;

	if (!arch_irn_consider_in_reg_alloc(env->arch, env->cls, node)) {
		loc.time = USES_INFINITY;
		return loc;
	}

	/* We have to keep nonspillable nodes in the workingset */
	if(arch_irn_get_flags(env->arch, node) & arch_irn_flags_dont_spill) {
		loc.time = 0;
		DBG((dbg, DBG_START, "    %+F taken (dontspill node)\n", node, loc.time));
		return loc;
	}

	next_use = be_get_next_use(env->uses, first, 0, node, 0);
	if(USES_IS_INFINITE(next_use.time)) {
		// the nodes marked as live in shouldn't be dead, so it must be a phi
		assert(is_Phi(node));
		loc.time = USES_INFINITY;
		DBG((dbg, DBG_START, "    %+F not taken (dead)\n", node));
		if(is_Phi(node)) {
			be_spill_phi(env->senv, node);
		}
		return loc;
	}

	loc.time = next_use.time;

	if(next_use.outermost_loop >= get_loop_depth(loop)) {
		DBG((dbg, DBG_START, "    %+F taken (%u, loop %d)\n", node, loc.time, next_use.outermost_loop));
	} else {
		loc.time = USES_PENDING;
		DBG((dbg, DBG_START, "    %+F delayed (outerloopdepth %d < loopdetph %d)\n", node, next_use.outermost_loop, get_loop_depth(loop)));
	}
	return loc;
}

/*
 * Computes set of live-ins for each block with multiple predecessors
 * and notifies spill algorithm which phis need to be spilled
 */
static void compute_live_ins(ir_node *block, void *data) {
	belady_env_t  *env  = data;
	ir_loop       *loop = get_irn_loop(block);
	const be_lv_t *lv   = env->lv;
	block_info_t  *block_info;
	ir_node       *first, *irn;
	loc_t         loc, *starters, *delayed;
	int           i, len, ws_count;
	int	          free_slots, free_pressure_slots;
	unsigned      pressure;

	if (get_Block_n_cfgpreds(block) == 1 && get_irg_start_block(get_irn_irg(block)) != block)
		return;

	block_info = new_block_info(&env->ob);
	set_block_info(block, block_info);

	/* Collect all values living at start of block */
	starters = NEW_ARR_F(loc_t, 0);
	delayed  = NEW_ARR_F(loc_t, 0);

	DBG((dbg, DBG_START, "Living at start of %+F:\n", block));
	first = sched_first(block);

	/* check all Phis first */
	sched_foreach(block, irn) {
		if (! is_Phi(irn))
			break;

		loc = to_take_or_not_to_take(env, first, irn, block, loop);

		if (! USES_IS_INFINITE(loc.time)) {
			if (USES_IS_PENDING(loc.time))
				ARR_APP1(loc_t, delayed, loc);
			else
				ARR_APP1(loc_t, starters, loc);
		}
	}

	/* check all Live-Ins */
	be_lv_foreach(lv, block, be_lv_state_in, i) {
		ir_node *node = be_lv_get_irn(lv, block, i);

		loc = to_take_or_not_to_take(env, first, node, block, loop);

		if (! USES_IS_INFINITE(loc.time)) {
			if (USES_IS_PENDING(loc.time))
				ARR_APP1(loc_t, delayed, loc);
			else
				ARR_APP1(loc_t, starters, loc);
		}
	}

	pressure            = be_get_loop_pressure(env->loop_ana, env->cls, loop);
	assert(ARR_LEN(delayed) <= (signed)pressure);
	free_slots          = env->n_regs - ARR_LEN(starters);
	free_pressure_slots = env->n_regs - (pressure - ARR_LEN(delayed));
	free_slots          = MIN(free_slots, free_pressure_slots);
	/* append nodes delayed due to loop structure until start set is full */
	for (i = 0; i < ARR_LEN(delayed) && i < free_slots; ++i) {
		DBG((dbg, DBG_START, "    delayed %+F taken\n", delayed[i].irn));
		ARR_APP1(loc_t, starters, delayed[i]);
		delayed[i].irn = NULL;
	}

	/* spill all delayed phis which didn't make it into start workset */
	for ( ; i < ARR_LEN(delayed); ++i) {
		ir_node *irn = delayed[i].irn;
		if (irn && is_Phi(irn) && get_nodes_block(irn) == block) {
			DBG((dbg, DBG_START, "    spilling delayed phi %+F\n", irn));
			be_spill_phi(env->senv, irn);
		}
	}
	DEL_ARR_F(delayed);

	/* Sort start values by first use */
	qsort(starters, ARR_LEN(starters), sizeof(starters[0]), loc_compare);

	/* Copy the best ones from starters to start workset */
	ws_count             = MIN(ARR_LEN(starters), env->n_regs);
	block_info->ws_start = new_workset(env, &env->ob);
	workset_bulk_fill(block_info->ws_start, ws_count, starters);

	/* The phis of this block which are not in the start set have to be spilled later. */
	len = ARR_LEN(starters);
	for (i = ws_count; i < len; ++i) {
		irn = starters[i].irn;
		if (! is_Phi(irn) || get_nodes_block(irn) != block)
			continue;

		be_spill_phi(env->senv, irn);
	}

	DEL_ARR_F(starters);
}

/**
 * Collects all values live-in at block @p block and all phi results in this block.
 * Then it adds the best values (at most n_regs) to the blocks start_workset.
 * The phis among the remaining values get spilled: Introduce psudo-copies of
 *  their args to break interference and make it possible to spill them to the
 *  same spill slot.
 */
static block_info_t *compute_block_start_info(belady_env_t *env, ir_node *block) {
	ir_node *pred_block;
	block_info_t *res, *pred_info;

	/* Have we seen this block before? */
	res = get_block_info(block);
	if (res)
		return res;

	/* Create the block info for this block. */
	res = new_block_info(&env->ob);
	set_block_info(block, res);

	/* Use endset of predecessor block as startset */
	assert(get_Block_n_cfgpreds(block) == 1 && block != get_irg_start_block(get_irn_irg(block)));
	pred_block = get_Block_cfgpred_block(block, 0);
	pred_info = get_block_info(pred_block);

	/* if pred block has not been processed yet, do it now */
	if (pred_info == NULL || pred_info->processed == 0) {
		belady(pred_block, env);
		pred_info = get_block_info(pred_block);
	}

	/* now we have an end_set of pred */
	assert(pred_info->ws_end && "The recursive call (above) is supposed to compute an end_set");
	res->ws_start = workset_clone(env, &env->ob, pred_info->ws_end);

	return res;
}


/**
 * For the given block @p block, decide for each values
 * whether it is used from a register or is reloaded
 * before the use.
 */
static void belady(ir_node *block, void *data) {
	belady_env_t *env = data;
	workset_t *new_vals;
	ir_node *irn;
	int iter;
	block_info_t *block_info;

	/* make sure we have blockinfo (with startset) */
	block_info = get_block_info(block);
	if (block_info == NULL)
		block_info = compute_block_start_info(env, block);

	/* Don't do a block twice */
	if(block_info->processed)
		return;

	/* get the starting workset for this block */
	DBG((dbg, DBG_DECIDE, "\n"));
	DBG((dbg, DBG_DECIDE, "Decide for %+F\n", block));

	workset_copy(env, env->ws, block_info->ws_start);
	DBG((dbg, DBG_WSETS, "Start workset for %+F:\n", block));
	workset_foreach(env->ws, irn, iter)
		DBG((dbg, DBG_WSETS, "  %+F (%u)\n", irn, workset_get_time(env->ws, iter)));

	/* process the block from start to end */
	DBG((dbg, DBG_WSETS, "Processing...\n"));
	env->used = pset_new_ptr_default();
	env->instr_nr = 0;
	new_vals = new_workset(env, &env->ob);
	sched_foreach(block, irn) {
		int i, arity;
		assert(workset_get_length(env->ws) <= env->n_regs && "Too much values in workset!");

		/* projs are handled with the tuple value.
		 * Phis are no real instr (see insert_starters())
		 * instr_nr does not increase */
		if (is_Proj(irn) || is_Phi(irn)) {
			DBG((dbg, DBG_DECIDE, "  ...%+F skipped\n", irn));
			continue;
		}
		DBG((dbg, DBG_DECIDE, "  ...%+F\n", irn));

		/* set instruction in the workset */
		env->instr = irn;

		/* allocate all values _used_ by this instruction */
		workset_clear(new_vals);
		for(i = 0, arity = get_irn_arity(irn); i < arity; ++i) {
			/* (note that reloaded_value is not interesting here) */
			workset_insert(env, new_vals, get_irn_n(irn, i), 0);
		}
		displace(env, new_vals, 1);

		/* allocate all values _defined_ by this instruction */
		workset_clear(new_vals);
		if (get_irn_mode(irn) == mode_T) { /* special handling for tuples and projs */
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				workset_insert(env, new_vals, proj, 0);
			}
		} else {
			workset_insert(env, new_vals, irn, 0);
		}
		displace(env, new_vals, 0);

		env->instr_nr++;
	}
	del_pset(env->used);

	/* Remember end-workset for this block */
	block_info->ws_end = workset_clone(env, &env->ob, env->ws);
	block_info->processed = 1;
	DBG((dbg, DBG_WSETS, "End workset for %+F:\n", block));
	workset_foreach(block_info->ws_end, irn, iter)
		DBG((dbg, DBG_WSETS, "  %+F (%u)\n", irn, workset_get_time(block_info->ws_end, iter)));
}

/**
 * 'decide' is block-local and makes assumptions
 * about the set of live-ins. Thus we must adapt the
 * live-outs to the live-ins at each block-border.
 */
static void fix_block_borders(ir_node *block, void *data)
{
	ir_graph     *irg        = get_irn_irg(block);
	ir_node      *startblock = get_irg_start_block(irg);
	belady_env_t *env        = data;
	workset_t    *start_workset;
	int           arity;
	int           i;
	int           iter;

	if(block == startblock)
		return;

	DBG((dbg, DBG_FIX, "\n"));
	DBG((dbg, DBG_FIX, "Fixing %+F\n", block));

	start_workset = get_block_info(block)->ws_start;

	/* process all pred blocks */
	arity = get_irn_arity(block);
	for (i = 0; i < arity; ++i) {
		ir_node   *pred             = get_Block_cfgpred_block(block, i);
		workset_t *workset_pred_end = get_block_info(pred)->ws_end;
		ir_node   *node;

		DBG((dbg, DBG_FIX, "  Pred %+F\n", pred));

		/* spill all values not used anymore */
		workset_foreach(workset_pred_end, node, iter) {
			ir_node *n2;
			int      iter2;
			int      found = 0;
			workset_foreach(start_workset, n2, iter2) {
				if(n2 == node) {
					found = 1;
					break;
				}
				/* note that we do not look at phi inputs, becuase the values
				 * will be either live-end and need no spill or
				 * they have other users in which must be somewhere else in the
				 * workset */
			}

			if(!found && be_is_live_out(env->lv, pred, node)
					&& !workset_pred_end->vals[iter].reloaded_value) {
				ir_node *insert_point
					= be_get_end_of_block_insertion_point(pred);
				DBG((dbg, DBG_SPILL, "Spill %+F before %+F\n", node,
				     insert_point));
				be_add_spill(env->senv, node, insert_point);
			}
		}

		/* reload missing values in predecessors */
		workset_foreach(start_workset, node, iter) {
			/* if node is a phi of the current block we reload
			 * the corresponding argument, else node itself */
			if(is_Phi(node) && block == get_nodes_block(node)) {
				node = get_irn_n(node, i);

				/* we might have unknowns as argument for the phi */
				if(!arch_irn_consider_in_reg_alloc(env->arch, env->cls, node))
					continue;
			}

			/* check if node is in a register at end of pred */
			if(workset_contains(workset_pred_end, node))
				continue;

			/* node is not in memory at the end of pred -> reload it */
			DBG((dbg, DBG_FIX, "    reload %+F\n", node));
			DBG((dbg, DBG_SPILL, "Reload %+F before %+F,%d\n", node, block, i));
			be_add_reload_on_edge(env->senv, node, block, i, env->cls, 1);
		}
	}
}

/**
 * Do spilling for a register class on a graph using the belady heuristic.
 * In the transformed graph, the register pressure never exceeds the number
 * of available registers.
 *
 * @param birg  The backend graph
 * @param cls   The register class to spill
 */
static void be_spill_belady(be_irg_t *birg, const arch_register_class_t *cls) {
	be_spill_belady_spill_env(birg, cls, NULL);
}

void be_spill_belady_spill_env(be_irg_t *birg, const arch_register_class_t *cls, spill_env_t *spill_env) {
	belady_env_t env;
	ir_graph *irg = be_get_birg_irg(birg);
	int n_regs;

	n_regs = cls->n_regs - be_put_ignore_regs(birg, cls, NULL);
	be_liveness_assure_sets(be_assure_liveness(birg));

	/* construct control flow loop tree */
	if(! (get_irg_loopinfo_state(irg) & loopinfo_cf_consistent)) {
		construct_cf_backedges(irg);
	}

	be_clear_links(irg);

	/* init belady env */
	obstack_init(&env.ob);
	env.arch      = birg->main_env->arch_env;
	env.cls       = cls;
	env.lv        = be_get_birg_liveness(birg);
	env.n_regs    = n_regs;
	env.ws        = new_workset(&env, &env.ob);
	env.uses      = be_begin_uses(irg, env.lv);
	env.loop_ana  = be_new_loop_pressure(birg);
	if(spill_env == NULL) {
		env.senv = be_new_spill_env(birg);
	} else {
		env.senv = spill_env;
	}

	/* Decide which phi nodes will be spilled and place copies for them into the graph */
	irg_block_walk_graph(irg, compute_live_ins, NULL, &env);
	/* Fix high register pressure with belady algorithm */
	irg_block_walk_graph(irg, NULL, belady, &env);
	/* belady was block-local, fix the global flow by adding reloads on the edges */
	irg_block_walk_graph(irg, fix_block_borders, NULL, &env);

	be_end_uses(env.uses);
	be_free_loop_pressure(env.loop_ana);
	obstack_free(&env.ob, NULL);

	/* Insert spill/reload nodes into the graph and fix usages */
	be_insert_spills_reloads(env.senv);

	/* clean up */
	if(spill_env == NULL)
		be_delete_spill_env(env.senv);
}

void be_init_spillbelady(void)
{
	static be_spiller_t belady_spiller = {
		be_spill_belady
	};

	be_register_spiller("belady", &belady_spiller);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.belady");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillbelady);
