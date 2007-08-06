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
 * @brief       Beladys spillalgorithm version 2.
 * @author      Daniel Grund, Matthias Braun, Sebastian Hack
 * @date        01.08.2007
 * @version     $Id$
 *
 * The main differences to the original Belady are:
 * - The workset is empty at the start of a block
 *   There is no attempt to fill it with variables which
 *   are not used in the block.
 * - There is a global pass which tries to use the remaining
 *   capacity of the blocks to let global variables live through
 *   them.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <limits.h>

#include "obst.h"
#include "irnodeset.h"
#include "irbitset.h"
#include "irprintf_t.h"
#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "irgwalk.h"
#include "irloop.h"
#include "iredges_t.h"
#include "irphase_t.h"
#include "ircons_t.h"
#include "irprintf.h"
#include "execfreq.h"
#include "xmalloc.h"

#include "beutil.h"
#include "bearch_t.h"
#include "bespillbelady.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bechordal_t.h"
#include "bespilloptions.h"
#include "beloopana.h"
#include "beirg_t.h"
#include "bemodule.h"

#define DBG_SPILL     1
#define DBG_WSETS     2
#define DBG_FIX       4
#define DBG_DECIDE    8
#define DBG_START    16
#define DBG_SLOTS    32
#define DBG_TRACE    64
#define DBG_WORKSET 128
#define DBG_GLOBAL  256

#define DEAD     UINT_MAX
#define LIVE_END (DEAD-1)

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An association between a node and a point in time.
 */
typedef struct _loc_t {
  ir_node *irn;        /**< A node. */
  unsigned time;       /**< A use time (see beuses.h). */
  unsigned version;    /**< That is used in the global pass below.
						 For usage see the comments below.
						 In the local belady pass, this is not important. */
} loc_t;

typedef struct _workset_t {
	int len;			/**< current length */
	loc_t vals[0];		/**< inlined array of the values/distances in this working set */
} workset_t;

typedef struct _belady_env_t {
	struct obstack ob;
	ir_graph *irg;
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	be_lv_t *lv;
	ir_exec_freq *ef;

	ir_node **blocks;   /**< Array of all blocks. */
	int n_blocks;       /**< Number of blocks in the graph. */
	int n_regs;			/**< number of regs in this reg-class */
	workset_t *ws;		/**< the main workset used while processing a block. ob-allocated */
	ir_node *instr;		/**< current instruction */
	unsigned instr_nr;	/**< current instruction number (relative to block start) */

	spill_env_t *senv;	/**< see bespill.h */
} belady_env_t;


static int loc_compare(const void *a, const void *b)
{
	const loc_t *p = a;
	const loc_t *q = b;
	return (int) p->time - (int) q->time;
}

static INLINE void workset_print(const workset_t *w)
{
	int i;

	for(i = 0; i < w->len; ++i) {
		ir_fprintf(stderr, "%+F %d\n", w->vals[i].irn, w->vals[i].time);
	}
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
static INLINE void workset_insert(belady_env_t *env, workset_t *ws, ir_node *val) {
	int i;
	/* check for current regclass */
	if (!arch_irn_consider_in_reg_alloc(env->arch, env->cls, val)) {
		DBG((dbg, DBG_WORKSET, "Skipped %+F\n", val));
		return;
	}

	/* check if val is already contained */
	for(i=0; i<ws->len; ++i)
		if (ws->vals[i].irn == val)
			return;

	/* insert val */
	assert(ws->len < env->n_regs && "Workset already full!");
	ws->vals[ws->len++].irn = val;
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

static INLINE int workset_get_index(const workset_t *ws, const ir_node *val) {
	int i;
	for(i=0; i<ws->len; ++i) {
		if (ws->vals[i].irn == val)
			return i;
	}

	return -1;
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
#define workset_contains(ws, n) (workset_get_index(ws, n) >= 0)

typedef struct _block_info_t {
	belady_env_t *bel;
	const ir_node *bl;
	workset_t *ws_start, *ws_end;
	ir_phase next_uses;

	ir_node *first_non_in;   /**< First node in block which is not a phi.  */
	ir_node *last_ins;       /**< The instruction before which end of
							   block reloads will be inserted. */

	workset_t *entrance_reg; /**< That set will contain all values
								  transported into the block which
								  are used before they are displaced.
								  That means, we later have to care to
								  bring them into the block in a register
								  or reload them at the entry of the block. */

	int pressure; /**< The amount of registers which remain free
					in this block. This capacity can be used to let
					global variables, transported into other blocks,
					live through this block. */

	double exec_freq; /**< The execution frequency of this block. */
} block_info_t;

static INLINE void *new_block_info(belady_env_t *bel, ir_node *bl) {
	block_info_t *res = obstack_alloc(&bel->ob, sizeof(*res));
	memset(res, 0, sizeof(res[0]));
	res->first_non_in = NULL;
	res->last_ins = NULL;
	res->bel = bel;
	res->bl  = bl;
	res->entrance_reg = new_workset(bel, &bel->ob);
	res->exec_freq    = get_block_execfreq(bel->ef, bl);
	set_irn_link(bl, res);
	return res;
}

#define get_block_info(block)        ((block_info_t *)get_irn_link(block))
#define set_block_info(block, info)  set_irn_link(block, info)

static INLINE ir_node *block_info_get_last_ins(block_info_t *bi)
{
	if (!bi->last_ins)
		bi->last_ins = be_get_end_of_block_insertion_point(bi->bl);

	return bi->last_ins;
}

typedef struct _next_use_t {
	unsigned is_first_use : 1; /**< Indicate that this use is the first
								 in the block. Needed to identify
								 transport in values for the global
								 pass. */
	int step;                  /**< The time step of the use. */
	struct _next_use_t *next;  /**< The next use int this block
								 or NULL. */
} next_use_t;

static void *next_use_init(ir_phase *phase, ir_node *irn, void *old)
{
	(void) phase;
	(void) irn;
	(void) old;
	return NULL;
}

static void build_next_uses(block_info_t *bi)
{
	ir_node *irn;

	phase_init(&bi->next_uses, "next uses", bi->bel->irg, PHASE_DEFAULT_GROWTH, next_use_init, NULL);
	sched_foreach_reverse(bi->bl, irn) {
		int i, step = sched_get_time_step(irn);

		if (is_Phi(irn))
			break;

		for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
			ir_node *op = get_irn_n(irn, i);
			next_use_t *curr = phase_get_irn_data(&bi->next_uses, op);
			next_use_t *use  = phase_alloc(&bi->next_uses, sizeof(use[0]));

			assert(step >= 0);
			use->is_first_use = 1;
			use->step         = step;
			use->next         = curr;

			if (curr)
				curr->is_first_use = 0;

			phase_set_irn_data(&bi->next_uses, op, use);
		}
	}
}

#define get_current_use(bi, irn) 	 phase_get_irn_data(&(bi)->next_uses, (irn))

static INLINE void advance_current_use(block_info_t *bi, const ir_node *irn)
{
	next_use_t *use = get_current_use(bi, irn);

	assert(use);
	phase_set_irn_data(&bi->next_uses, irn, use->next);
}

static INLINE unsigned get_curr_distance(block_info_t *bi, const ir_node *irn, int is_usage)
{
	belady_env_t *env = bi->bel;
	next_use_t *use   = get_current_use(bi, irn);
	int curr_step     = sched_get_time_step(env->instr);
	int flags         = arch_irn_get_flags(env->arch, irn);

	assert(!(flags & arch_irn_flags_ignore));

	/* We have to keep nonspillable nodes in the workingset */
	if(flags & arch_irn_flags_dont_spill)
		return 0;

	if (!is_usage && use && use->step == curr_step)
		use = use->next;

	if (use) {
		assert(use->step >= curr_step);
		return use->step - curr_step;
	}

	return be_is_live_end(env->lv, bi->bl, irn) ? LIVE_END : DEAD;
}

static INLINE int is_local_phi(const ir_node *bl, const ir_node *irn)
{
	return is_Phi(irn) && get_nodes_block(irn) == bl;
}

/**
 * Check, if the value is something that is transported into a block.
 * That is, the value is defined elsewhere or defined by a Phi in the block.
 * @param env  The belady environment.
 * @param bl   The block in question.
 * @param irn  The node in question.
 * @return     1, if node is something transported into @p bl, 0 if not.
 * @note       The function will only give correct answers in the case
 *             where @p irn is unsed in the block @p bl which is always
 *             the case in our usage scenario.
 */
static INLINE int is_transport_in(const ir_node *bl, const ir_node *irn)
{
	return is_local_phi(bl, irn) || get_nodes_block(irn) != bl;
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
static void displace(block_info_t *bi, workset_t *new_vals, int is_usage) {
	belady_env_t *env       = bi->bel;
	workset_t    *ws        = env->ws;
	ir_node     **to_insert = alloca(env->n_regs * sizeof(to_insert[0]));

	int i, len, max_allowed, demand, iter;
	ir_node *val;

	/*
		1. Identify the number of needed slots and the values to reload
	*/
	demand = 0;
	workset_foreach(new_vals, val, iter) {
		/* mark value as used */

		if (! workset_contains(ws, val)) {
			DBG((dbg, DBG_DECIDE, "    insert %+F\n", val));
			to_insert[demand++] = val;
			if (is_usage) {
				int insert_reload = 1;
				next_use_t *use = get_current_use(bi, val);

				/*
				 * if we use a value which is transported in this block, i.e. a
				 * phi defined here or a live in, for the first time, we check
				 * if there is room for that guy to survive from the block's
				 * entrance to here or not.
				 */
				assert(use);
				assert(sched_get_time_step(env->instr) == use->step);
				if (is_transport_in(bi->bl, val) && use->is_first_use) {
					DBG((dbg, DBG_DECIDE, "entrance node %+F, pressure %d:\n", val, bi->pressure));
					if (bi->pressure < env->n_regs) {
						workset_insert(env, bi->entrance_reg, val);
						insert_reload = 0;
						++bi->pressure;
						DBG((dbg, DBG_DECIDE, "... no reload. must be considered at block start\n"));
					}
				}

				if (insert_reload) {
					DBG((dbg, DBG_SPILL, "Reload %+F before %+F\n", val, env->instr));
					be_add_reload(env->senv, val, env->instr, env->cls, 1);
				}
			}
		} else {
			assert(is_usage || "Defined value already in workset?!?");
			DBG((dbg, DBG_DECIDE, "    skip %+F\n", val));
		}
	}
	DBG((dbg, DBG_DECIDE, "    demand = %d\n", demand));

	/*
		2. Make room for at least 'demand' slots
	*/
	len         = workset_get_length(ws);
	max_allowed = env->n_regs - demand;

	/* Only make more free room if we do not have enough */
	if (len > max_allowed) {
		// int curr_step = sched_get_time_step(env->instr);

		DBG((dbg, DBG_DECIDE, "    disposing %d values\n", len - max_allowed));

		/* get current next-use distance */
		for (i = 0; i < ws->len; ++i) {
			ir_node *val  = workset_get_val(ws, i);
			unsigned dist = get_curr_distance(bi, val, is_usage);
			workset_set_time(ws, i, dist);
		}

		/* sort entries by increasing nextuse-distance*/
		workset_sort(ws);

		/* kill the last 'demand' entries in the array */
		workset_set_length(ws, max_allowed);
	}

	/*
		3. Insert the new values into the workset
		   Also, we update the pressure in the block info.
		   That is important for the global pass to decide
		   how many values can live through the block.
	*/
	for (i = 0; i < demand; ++i)
		workset_insert(env, env->ws, to_insert[i]);

	bi->pressure = MAX(bi->pressure, workset_get_length(env->ws));

}

/**
 * For the given block @p block, decide for each values
 * whether it is used from a register or is reloaded
 * before the use.
 */
static void belady(ir_node *block, void *data) {
	belady_env_t *env        = data;
	block_info_t *block_info = new_block_info(env, block);
	void *obst_state         = obstack_base(&env->ob);

	workset_t *new_vals;
	ir_node *irn;
	int iter;

	DBG((dbg, DBG_WSETS, "Processing %+F...\n", block_info->bl));
	new_vals = new_workset(env, &env->ob);
	workset_clear(env->ws);

	/* build the next use information for this block. */
	build_next_uses(block_info);

	env->instr_nr = 0;
	block_info->first_non_in = NULL;

	/* process the block from start to end */
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

		if (!block_info->first_non_in)
			block_info->first_non_in = irn;

		/* set instruction in the workset */
		env->instr = irn;

		/* allocate all values _used_ by this instruction */
		workset_clear(new_vals);
		for(i = 0, arity = get_irn_arity(irn); i < arity; ++i) {
			workset_insert(env, new_vals, get_irn_n(irn, i));
		}
		displace(block_info, new_vals, 1);

		/*
		 * set all used variables to the next use in their next_use_t list
		 * Also, kill all dead variables from the workset. They are only
		 * augmenting the pressure. Note, that a variable is dead
		 * if it has no further use in this block and is *not* live end
		 */
		for(i = 0, arity = get_irn_arity(irn); i < arity; ++i) {
			ir_node *op = get_irn_n(irn, i);
			next_use_t *use = get_current_use(block_info, op);

			assert(use);
			if (!use->next && !be_is_live_end(env->lv, block, op))
				workset_remove(env->ws, op);

			advance_current_use(block_info, op);
		}

		/* allocate all values _defined_ by this instruction */
		workset_clear(new_vals);
		if (get_irn_mode(irn) == mode_T) { /* special handling for tuples and projs */
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				workset_insert(env, new_vals, proj);
			}
		} else {
			workset_insert(env, new_vals, irn);
		}
		displace(block_info, new_vals, 0);

		env->instr_nr++;
	}

	phase_free(&block_info->next_uses);
	obstack_free(&env->ob, obst_state);

	/* Remember end-workset for this block */
	block_info->ws_end = workset_clone(env, &env->ob, env->ws);
	DBG((dbg, DBG_WSETS, "End workset for %+F:\n", block));
	workset_foreach(block_info->ws_end, irn, iter)
		DBG((dbg, DBG_WSETS, "  %+F (%u)\n", irn, workset_get_time(block_info->ws_end, iter)));
	DBG((dbg, DBG_WSETS, "Max pressure in block: %d\n", block_info->pressure));
}

/*
 _____ _                  _       _           _   ____            _
|_   _| |__   ___    __ _| | ___ | |__   __ _| | |  _ \ __ _ _ __| |_
  | | | '_ \ / _ \  / _` | |/ _ \| '_ \ / _` | | | |_) / _` | '__| __|
  | | | | | |  __/ | (_| | | (_) | |_) | (_| | | |  __/ (_| | |  | |_
  |_| |_| |_|\___|  \__, |_|\___/|_.__/ \__,_|_| |_|   \__,_|_|   \__|
                    |___/

*/

static int block_freq_gt(const void *a, const void *b)
{
	const ir_node * const *p = a;
	const ir_node * const *q = b;
	block_info_t *pi = get_block_info(*p);
	block_info_t *qi = get_block_info(*q);
	double diff = qi->exec_freq - pi->exec_freq;
	return (diff > 0) - (diff < 0);
}

typedef struct _block_end_state_t {
	ir_node *bl;
	ir_node *irn;
	double costs;
	workset_t *end_state;
	unsigned reload_at_end : 1;
	unsigned live_through  : 1;
} block_end_state_t;

typedef struct _global_end_state_t {
	belady_env_t *env;
	bitset_t *succ_phis;
	struct obstack obst;
	block_end_state_t *end_info;
	unsigned gauge;
	unsigned version;
} global_end_state_t;

typedef struct {
	void *obst_level;
	unsigned gauge;
} rollback_info_t;

static INLINE rollback_info_t trans_begin(global_end_state_t *ges)
{
	rollback_info_t rb;
	rb.obst_level = obstack_base(&ges->obst);
	rb.gauge      = ges->gauge;
	return rb;
}

static INLINE void trans_rollback(global_end_state_t *ges, rollback_info_t *rb)
{
	ges->gauge = rb->gauge;
	obstack_free(&ges->obst, rb->obst_level);
}

static block_end_state_t *get_block_end_state(global_end_state_t *state, ir_node *bl, ir_node *irn)
{
	unsigned i;

	for (i = 0; i < state->gauge; ++i) {
		block_end_state_t *bei = &state->end_info[i];
		if (bei->bl == bl && bei->irn == irn)
			return bei;
	}

	{
		block_info_t *bi = get_block_info(bl);
		block_end_state_t *curr;

		/* make sure we have room in the array */
		ARR_EXTO(block_end_state_t, state->end_info, (int) state->gauge);

		curr = &state->end_info[state->gauge];

		memset(curr, 0, sizeof(curr[0]));
		curr->bl  = bl;
		curr->irn = irn;
		curr->end_state = workset_clone(state->env, &state->obst, bi->ws_end);
		curr->costs = -1.0;
		++state->gauge;
		return curr;
	}
}

static double can_bring_in(global_end_state_t *ges, ir_node *bl, ir_node *irn, int level);

static double can_make_available_at_end(global_end_state_t *ges, ir_node *bl, ir_node *irn, int level)
{
	block_end_state_t *bes = get_block_end_state(ges, bl, irn);
	workset_t *end         = bes->end_state;
	block_info_t *bi       = get_block_info(bl);
	int n_regs             = bi->bel->n_regs;
	int index;

	DBG((dbg, DBG_GLOBAL, "\t%2Dcan make avail %+F at end of %+F (pressure %d)\n",
				level, irn, bl, bi->pressure));

	/*
	 * to make the value available at end,
	 * we have several cases here.
	 *
	 * - we already visited that block.
	 * - If the value is in the final end set, return 0.
	 *   somebody else already allocated it there.
	 * - If not and the final end set is already full,
	 *   we cannot make the value available at the end
	 *   of this block. return INFINITY.
	 * - Else (value not in final end set and there is room):
	 *   1) The value is in a register at the end of the local Belady pass.
	 *      Allocate a slot in  the final end set and return 0.
	 *   2) The value is not in the Belady end set:
	 *      If the block's capacity is < k then check what it costs
	 *      to transport the value from upper blocks to this block.
	 *      Compare that against the reload cost in this block. If
	 *      cheaper, do the other thing. If not, reload it here.
	 */

	/*
	 * we have been here before and already figured out some costs.
	 * so we can exit safely.
	 */
	if (bes->costs >= 0.0) {
		DBG((dbg, DBG_GLOBAL, "\t%2Dwe\'ve been here before\n", level));
		goto end;
	}

	/* if the end set contains it already, it is in a reg and it costs nothing
	 * to load it to one. */
	index = workset_get_index(end, irn);
	if (index >= 0) {
		unsigned ver = end->vals[index].version;
		DBG((dbg, DBG_GLOBAL, "\t%2Dnode is in the end set and is %s fixed\n",
					level, ver > ges->version ? "already" : "not yet"));

		/*
		 * if the version is older, the value is already fixed
		 * and cannot be removed from the end set. If not,
		 * we fix it here by giving it our version.
		 */
		if (ver < ges->version)
			end->vals[index].version = ges->version;

		bes->costs = 0.0;
		goto end;
	}

	/*
	 * Now we have two options:
	 * 1) Reload the value at the end of the block.
	 *    Therefore, perhaps, we have to erase another one from the workset.
	 *    This may only be done if it has not been fixed.
	 *    Since fixed means that a previous pass has decided that that value
	 *    *has* to stay in the end set.
	 * 2) we can try, if the capacity of the block allows it, to let
	 *    the value live through the block and make it available at
	 *    the entrance.
	 *
	 * First, we test the local (reload in this block) alternative
	 * and compare against the other alternative.
	 * Of course, we chose the cheaper one.
	 */

	{
		int len = workset_get_length(end);
		int slot = -1;
		int i;

		bes->costs = HUGE_VAL;

		/*
		 * look if there is room in the end array
		 * for the variable. Note that this does not
		 * means that the var is living through the block.
		 * There is just room at the *end*
		 */
		if (len < n_regs) {
			DBG((dbg, DBG_GLOBAL, "\t%2Dthe end set has %d free slots\n",
						level, n_regs - len));
			slot = len;
		} else {
			for (i = 0; i < len; ++i)
				if (end->vals[i].version < ges->version)
					break;

			if (i < len) {
				DBG((dbg, DBG_GLOBAL, "\t%2D%+F (slot %d) can be erased from the end set\n",
							level, end->vals[i].irn, i));
				slot = i;
			}
		}

		if (slot >= 0) {
			rollback_info_t rb  = trans_begin(ges);
			ir_node *ins_before = block_info_get_last_ins(bi);
			double reload_here  = be_get_reload_costs(bi->bel->senv, irn, ins_before);
			double bring_in     = bi->pressure < n_regs ? can_bring_in(ges, bl, irn, level + 1) : HUGE_VAL;

			DBG((dbg, DBG_GLOBAL, "\t%2Dthere is a free slot. capacity=%d, reload here=%f, bring in=%f\n",
						level, n_regs - bi->pressure, reload_here, bring_in));

			/*
			 * reloading here pays off; bringing the value in from elsewhere
			 * is too expensive, hence we drop that search by resetting
			 * the gauge.
			 */
			if (reload_here <= bring_in) {
				trans_rollback(ges, &rb);
				bes->costs = reload_here;
				bes->reload_at_end = 1;
			} else {
				bes->live_through = 1;
				bes->costs = bring_in;
			}

			end->vals[slot].irn     = irn;
			end->vals[slot].version = ges->version;
			end->len = MAX(end->len, slot + 1);
		}
	}

end:
	DBG((dbg, DBG_GLOBAL, "\t%2D-> %f\n", level, bes->costs));
	return bes->costs;
}

static double can_bring_in(global_end_state_t *ges, ir_node *bl, ir_node *irn, int level)
{
	double glob_costs = HUGE_VAL;

	DBG((dbg, DBG_GLOBAL, "\t%2Dcan bring in for %+F at block %+F\n", level, irn, bl));

	if (is_transport_in(bl, irn)) {
		int i, n           = get_irn_arity(bl);
		ir_node **nodes    = alloca(get_irn_arity(bl) * sizeof(nodes[0]));
		rollback_info_t rb = trans_begin(ges);


		glob_costs = 0.0;
		for (i = 0; i < n; ++i) {
			ir_node *pr = get_Block_cfgpred_block(bl, i);
			ir_node *op = is_local_phi(bl, irn) ? get_irn_n(irn, i) : irn;
			double c    = can_make_available_at_end(ges, pr, op, level + 1);

			if (c >= HUGE_VAL) {
				trans_rollback(ges, &rb);
				glob_costs = HUGE_VAL;
				goto end;
			}

			glob_costs += c;
		}
	}

end:
	DBG((dbg, DBG_GLOBAL, "\t%2D-> %f\n", level, glob_costs));
	return glob_costs;
}

static void materialize_and_commit_end_state(global_end_state_t *ges)
{
	belady_env_t *env = ges->env;
	unsigned i;

	DBG((dbg, DBG_GLOBAL, "\tmaterializing\n"));
	for (i = 0; i < ges->gauge; ++i) {
		block_end_state_t *bes = &ges->end_info[i];
		block_info_t *bi       = get_block_info(bes->bl);
		int idx, end_pressure;

		DBG((dbg, DBG_GLOBAL, "\t\t%+F in %+F, cost %f through: %d, rel: %d\n",
				bes->irn, bes->bl, bes->costs, bes->live_through, bes->reload_at_end));

		/* insert the reload if the val was reloaded at the block's end */
		if (bes->reload_at_end) {
			be_add_reload_at_end(env->senv, bes->irn, bes->bl, env->cls, 1);
			DBG((dbg, DBG_GLOBAL, "\t\tadding reload of %+F at end of %+F\n", bes->irn, bes->bl));
		}

		idx = workset_get_index(bes->end_state, bes->irn);

		if (is_local_phi(bes->bl, bes->irn) && bes->live_through)
			bitset_add_irn(ges->succ_phis, bes->irn);

		/*
		 * set the version number in the workset.
		 * That will mark this value as fixed in the end set
		 * and will prevent further investigations from removing
		 * it from there.
		 * Also "commit" the workset;
		 * by copying it back to the block's end workset.
		 */
		if (idx >= 0) {
			DBG((dbg, DBG_GLOBAL, "\t\tcommiting workset of %+F with version %x\n", bes->bl, ges->version));
			bes->end_state->vals[idx].version = ges->version;
			workset_copy(env, bi->ws_end, bes->end_state);
		}

		end_pressure = 0;
		for (idx = workset_get_length(bes->end_state) - 1; idx >= 0; --idx)
			if (bes->end_state->vals[idx].version >= ges->version)
				end_pressure += 1;

		/*
		 * if the variable is live through the block,
		 * update the pressure indicator.
		 */
		DBG((dbg, DBG_GLOBAL, "\t\told pressure %d, ", bi->pressure));

		bi->pressure = MAX(bi->pressure + bes->live_through, end_pressure);

		DBG((dbg, DBG_GLOBAL, "new pressure: %d, end pressure: %d, end length: %d\n",
					bi->pressure, end_pressure, workset_get_length(bes->end_state)));

	}
}

/**
 * Examine all irns which shall be in regs at the beginning of the
 * block.
 */
static void fix_block_borders(global_end_state_t *ges, ir_node *block) {
	block_info_t *bi  = get_block_info(block);
	belady_env_t *env = ges->env;

	ir_node *irn;
	int i;

	DBG((dbg, DBG_GLOBAL, "fixing block borders at %+F (%f)\n", block, bi->exec_freq));

	/* process all variables which shall be in a reg at
	 * the beginning of the block in the order of the next use. */
	workset_foreach(bi->entrance_reg, irn, i) {
		double local_costs = be_get_reload_costs(env->senv, irn, bi->first_non_in);
		double bring_in_costs;

		/* reset the gauge and create a new version. */
		ges->gauge    = 0;
		ges->version -= 1;

		DBG((dbg, DBG_GLOBAL, "\ttrans in var %+F, version %x\n", irn, ges->version));

		bring_in_costs = can_bring_in(ges, block, irn, 1);

		DBG((dbg, DBG_GLOBAL, "\tbring in: %f, local: %f", bring_in_costs, local_costs));

		/*
		 * we were not able to let the value arrive
		 * in a register at the entrance of the block
		 * or it is too costly, so we have to do the reload locally
		 */
		if (bring_in_costs > local_costs) {

			DBG((dbg, DBG_GLOBAL, " -> do local reload\n"));
			be_add_reload(env->senv, irn, bi->first_non_in, env->cls, 1);
		} else {
			/*
			 * if the transport-in was a phi (that is actually used in block)
			 * it will no longer remain and we have to spill it completely.
			 */
			if (is_local_phi(block, irn))
				bitset_add_irn(ges->succ_phis, irn);

			DBG((dbg, DBG_GLOBAL, " -> do remote reload\n"));
			materialize_and_commit_end_state(ges);
		}

		DBG((dbg, DBG_GLOBAL, "\n"));
	}
}

static void global_assign(belady_env_t *env)
{
	global_end_state_t ges;
	int i;

	obstack_init(&ges.obst);
	ges.gauge     = 0;
	ges.env       = env;
	ges.version   = -1;
	ges.end_info  = NEW_ARR_F(block_end_state_t, env->n_blocks);
	ges.succ_phis = bitset_irg_obstack_alloc(&env->ob, env->irg);

	/*
	 * sort the blocks according to execution frequency.
	 * That's not necessary for belady() but for the global pass later on.
	 */
	qsort(env->blocks, env->n_blocks, sizeof(env->blocks[0]), block_freq_gt);

	for (i = 0; i < env->n_blocks; ++i)
		fix_block_borders(&ges, env->blocks[i]);

	/*
	 * Now we spill phis which cannot be kept since they were replaced
	 * by reloads at the block entrances.
	 */
	for (i = 0; i < env->n_blocks; ++i) {
		ir_node *bl = env->blocks[i];
		ir_node *irn;

		sched_foreach(bl, irn) {
			if (!is_Phi(irn))
				break;

			if (arch_irn_consider_in_reg_alloc(env->arch, env->cls, irn)
					&& !bitset_contains_irn(ges.succ_phis, irn))
				be_spill_phi(env->senv, irn);
		}
	}

	DEL_ARR_F(ges.end_info);
}

static void collect_blocks(ir_node *bl, void *data)
{
	belady_env_t *env = data;
	++env->n_blocks;
	obstack_ptr_grow(&env->ob, bl);
}

void be_spill_belady_spill_env2(be_irg_t *birg, const arch_register_class_t *cls, spill_env_t *spill_env) {
	ir_graph *irg = be_get_birg_irg(birg);
	belady_env_t env;
	int i, n_regs;

	/* some special classes contain only ignore regs, nothing to do then */
	n_regs = cls->n_regs - be_put_ignore_regs(birg, cls, NULL);
	if(n_regs == 0)
		return;

	be_clear_links(irg);

	/* init belady env */
	obstack_init(&env.ob);
	env.irg       = irg;
	env.arch      = birg->main_env->arch_env;
	env.cls       = cls;
	env.lv        = be_get_birg_liveness(birg);
	env.n_regs    = n_regs;
	env.ws        = new_workset(&env, &env.ob);
	env.senv      = spill_env ? spill_env : be_new_spill_env(birg);
	env.ef        = be_get_birg_exec_freq(birg);
	env.n_blocks  = 0;

	irg_block_walk_graph(irg, NULL, collect_blocks, &env);
	obstack_ptr_grow(&env.ob, NULL);
	env.blocks = obstack_finish(&env.ob);

	/* Fix high register pressure in blocks with belady algorithm */
	for (i = 0; i < env.n_blocks; ++i)
		belady(env.blocks[i], &env);

	global_assign(&env);

	/* Insert spill/reload nodes into the graph and fix usages */
	be_insert_spills_reloads(env.senv);

	/* clean up */
	if(spill_env == NULL)
		be_delete_spill_env(env.senv);

	obstack_free(&env.ob, NULL);
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
	be_spill_belady_spill_env2(birg, cls, NULL);
}


void be_init_spillbelady2(void)
{
	static be_spiller_t belady_spiller = {
		be_spill_belady
	};

	be_register_spiller("belady2", &belady_spiller);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.belady2");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillbelady2);
