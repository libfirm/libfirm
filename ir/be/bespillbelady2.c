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
 * @author      Sebastian Hack, Matthias Braun, Daniel Grund
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
#include "dfs_t.h"
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

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>

#define DBG_SPILL     1
#define DBG_WSETS     2
#define DBG_FIX       4
#define DBG_DECIDE    8
#define DBG_START    16
#define DBG_SLOTS    32
#define DBG_TRACE    64
#define DBG_WORKSET 128
#define DBG_GLOBAL  256

#define ALREADY_SPILLED_FACTOR 2

#define DEAD       UINT_MAX
#define LIVE_END   (DEAD-1)
#define REMAT_DIST (DEAD-2)

static int already_spilled_factor = 2;
static int remat_live_range_ext   = 1;
static int global_pass_enabled    = 1;

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_INT           ("asf",    "already spilled factor",                             &already_spilled_factor),
	LC_OPT_ENT_BOOL          ("remat",  "rematerializable ops get infinite long live ranges", &remat_live_range_ext),
	LC_OPT_ENT_BOOL          ("global", "enable/disable the global pass",                     &global_pass_enabled),
	LC_OPT_LAST
};

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An association between a node and a point in time.
 */
typedef struct _loc_t {
  ir_node *irn;        /**< A node. */
  unsigned time;       /**< A use time.
						 In the global pass this is used
						 as the version number and not as a time.
						 Only to save space...
						*/
} loc_t;

typedef struct _workset_t {
	int len;			/**< current length */
	loc_t vals[0];		/**< inlined array of the values/distances in this working set */
} workset_t;

typedef struct _belady_env_t {
	struct obstack ob;
	ir_graph *irg;
	const dfs_t *dfs;
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	be_lv_t *lv;
	ir_exec_freq *ef;

	ir_node **blocks;   /**< Array of all blocks. */
	int n_blocks;       /**< Number of blocks in the graph. */
	int n_regs;			/**< number of regs in this reg-class */
	workset_t *ws;		/**< the main workset used while processing a block. ob-allocated */
	ir_node *instr;		/**< current instruction */
	int instr_nr;     	/**< current instruction number (relative to block start) */

	spill_env_t *senv;	/**< see bespill.h */
	bitset_t *spilled;  /**< bitset to keep all the irns which have already been spilled. */
} belady_env_t;


static int loc_compare(const void *a, const void *b)
{
	const loc_t *p = a;
	const loc_t *q = b;
	return (p->time > q->time) - (p->time < q->time);
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
		// DBG((dbg, DBG_WORKSET, "Skipped %+F\n", val));
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

typedef struct _bring_in_t bring_in_t;

typedef struct _block_info_t {
	belady_env_t *bel;
	ir_node *bl;
	int id;
	ir_phase next_uses;
	workset_t *ws_end;       /**< The end set after the local belady pass. */
	double exec_freq;        /**< The execution frequency of this block. */

	double reload_cost;      /**< Cost of a reload in this block. */
	ir_node *first_non_in;   /**< First node in block which is not a phi.  */
	ir_node *last_ins;       /**< The instruction before which end of
							   block reloads will be inserted. */

	int pressure;            /**< The amount of registers which remain free
				               in this block. This capacity can be used to let
				               global variables, transported into other blocks,
				               live through this block. */

	int front_pressure;      /**< The pressure right before the first
							   real (non-phi) node. At the beginning
							   of the global pass, this is 0. */
	struct list_head br_head; /**< List head for all bring_in variables. */

} block_info_t;

static INLINE void *new_block_info(belady_env_t *bel, int id)
{
	ir_node      *bl  = bel->blocks[id];
	block_info_t *res = obstack_alloc(&bel->ob, sizeof(*res));
	memset(res, 0, sizeof(res[0]));
	res->first_non_in = NULL;
	res->last_ins = NULL;
	res->bel = bel;
	res->bl  = bl;
	res->id  = id;
	res->exec_freq    = get_block_execfreq(bel->ef, bl);
	res->reload_cost  = bel->arch->isa->reload_cost * res->exec_freq;
	INIT_LIST_HEAD(&res->br_head);
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
	sched_timestep_t step;     /**< The time step of the use. */
	ir_node *irn;
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

	sched_renumber(bi->bl);

	phase_init(&bi->next_uses, "next uses", bi->bel->irg, PHASE_DEFAULT_GROWTH, next_use_init, NULL);
	sched_foreach_reverse(bi->bl, irn) {
		int i;

		if (is_Phi(irn))
			break;

		for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
			ir_node *op = get_irn_n(irn, i);
			next_use_t *curr = phase_get_irn_data(&bi->next_uses, op);
			next_use_t *use  = phase_alloc(&bi->next_uses, sizeof(use[0]));

			use->is_first_use = 1;
			use->step         = sched_get_time_step(irn);
			use->next         = curr;
			use->irn          = irn;

			if (curr) {
				curr->is_first_use = 0;
				assert(curr->step >= use->step);
			}

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

static __attribute__((unused)) int block_freq_gt(const void *a, const void *b)
{
	const ir_node * const *p = a;
	const ir_node * const *q = b;
	block_info_t *pi = get_block_info(*p);
	block_info_t *qi = get_block_info(*q);
	double diff = qi->exec_freq - pi->exec_freq;
	return (diff > 0) - (diff < 0);
}

static int block_freq_dfs_gt(const void *a, const void *b)
{
	const ir_node * const *p = a;
	const ir_node * const *q = b;
	block_info_t *pi = get_block_info(*p);
	block_info_t *qi = get_block_info(*q);
	double diff;

	if ((pi->exec_freq > 1.0 && qi->exec_freq > 1.0)
			|| (pi->exec_freq <= 1.0 && qi->exec_freq <= 1.0)) {

		const dfs_t *dfs = pi->bel->dfs;
		int pp = dfs_get_post_num(dfs, pi->bl);
		int pq = dfs_get_post_num(dfs, qi->bl);
		return pq - pp;
	}

	diff = qi->exec_freq - pi->exec_freq;
	return (diff > 0) - (diff < 0);
}

/*
   ____       _               ___
  | __ ) _ __(_)_ __   __ _  |_ _|_ __
  |  _ \| '__| | '_ \ / _` |  | || '_ \
  | |_) | |  | | | | | (_| |  | || | | |
  |____/|_|  |_|_| |_|\__, | |___|_| |_|
                      |___/

  Data structures to represent bring in variables.
*/

struct _bring_in_t {
	ir_node *irn;              /**< The node to bring in. */
	block_info_t *bi;          /**< The block to which bring in should happen. */
	int pressure_so_far;       /**< The maximal pressure till the first use of irn in bl. */
	ir_node *first_use;        /**< The first user of irn in bl. */
	sched_timestep_t use_step; /**< Schedule sttep of the first use. */

	int is_remat : 1;          /**< Is rematerializable. */
	int sect_pressure;         /**< Offset to maximum pressure in block. */
	struct list_head list;
};

static INLINE bring_in_t *new_bring_in(block_info_t *bi, ir_node *irn, const next_use_t *use)
{
	bring_in_t *br    = obstack_alloc(&bi->bel->ob, sizeof(br[0]));

	br->irn             = irn;
	br->bi              = bi;
	br->first_use       = use->irn;
	br->use_step        = use->step;
	br->is_remat        = be_is_rematerializable(bi->bel->senv, irn, use->irn);
	br->pressure_so_far = bi->pressure;
	br->sect_pressure   = bi->front_pressure;

	INIT_LIST_HEAD(&br->list);
	list_add_tail(&br->list, &bi->br_head);
	return br;
}

static int bring_in_cmp(const void *a, const void *b)
{
	const bring_in_t *p = *(const bring_in_t * const *) a;
	const bring_in_t *q = *(const bring_in_t * const *) b;
	double fp, fq;

	/* if one of both is a remat node, it will be done after the other. */
	if (p->is_remat != q->is_remat)
		return p->is_remat - q->is_remat;

	/* in the same block, the one further in the front has to be processed first!
	 * Otherwise the front_pressure 'trick' is not exact. */
	if (p->bi == q->bi)
		return p->use_step - q->use_step;

	fp = p->bi->exec_freq;
	fq = q->bi->exec_freq;

	/* if both have the same frequency, inspect the frequency of the definition */
	if (fp == fq) {
		double fdp = get_block_info(get_nodes_block(p->irn))->exec_freq;
		double fdq = get_block_info(get_nodes_block(q->irn))->exec_freq;

		/* if the defs of both have the same freq, we go for reverse dfs post order. */
		if (fdp == fdq) {
			const dfs_t *dfs = p->bi->bel->dfs;
			int pp = dfs_get_post_num(dfs, p->bi->bl);
			int pq = dfs_get_post_num(dfs, q->bi->bl);
			return pq - pp;
		}

		return (fdq > fdp) - (fdq < fdp);
	}

	return (fq > fp) - (fq < fp);
}

static INLINE unsigned get_curr_distance(block_info_t *bi, const ir_node *irn, int is_usage)
{
	belady_env_t *env          = bi->bel;
	sched_timestep_t curr_step = sched_get_time_step(env->instr);
	next_use_t *use            = get_current_use(bi, irn);
	int flags                  = arch_irn_get_flags(env->arch, irn);

	assert(!(flags & arch_irn_flags_ignore));

	/* We have to keep nonspillable nodes in the workingset */
	if(flags & arch_irn_flags_dont_spill)
		return 0;

	if (!is_usage && use && use->step == curr_step)
		use = use->next;

	if (use) {
		unsigned res  = use->step - curr_step;

		assert(use->step >= curr_step);

		if (res != 0) {
			if (remat_live_range_ext && be_is_rematerializable(env->senv, irn, use->irn))
				res = REMAT_DIST;
			else if (bitset_contains_irn(env->spilled, irn))
				res *= already_spilled_factor;
		}

		return res;
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
	return get_nodes_block(irn) != bl || is_Phi(irn);
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
			DBG((dbg, DBG_DECIDE, "\t\tinsert %+F\n", val));
			to_insert[demand++] = val;
			if (is_usage) {
				next_use_t *use = get_current_use(bi, val);

				/*
				 * if we use a value which is transported in this block, i.e. a
				 * phi defined here or a live in, for the first time, we check
				 * if there is room for that guy to survive from the block's
				 * entrance to here or not.
				 */
				assert(use);
				assert(sched_get_time_step(env->instr) == (int) use->step);
				if (is_transport_in(bi->bl, val) && use->is_first_use) {
					bring_in_t *bri = new_bring_in(bi, val, use);
					bri->first_use = env->instr;

					/* reset the section pressure, since a new section starts. */
					bi->front_pressure = 0;

					DBG((dbg, DBG_DECIDE, "\t\tbring in node %+F, pressure %d:\n", val, bi->pressure));
					DBG((dbg, DBG_DECIDE, "\t\tno reload. must be considered at block start\n"));
				}

				else {
					bitset_add_irn(env->spilled, val);
					DBG((dbg, DBG_SPILL, "\t\tReload %+F before %+F\n", val, env->instr));
					be_add_reload(env->senv, val, env->instr, env->cls, 1);
				}
			}
		} else {
			assert(is_usage || "Defined value already in workset?!?");
			DBG((dbg, DBG_DECIDE, "\t\tskip %+F\n", val));
		}
	}
	DBG((dbg, DBG_DECIDE, "\t\tdemand = %d\n", demand));

	/*
		2. Make room for at least 'demand' slots
	*/
	len         = workset_get_length(ws);
	max_allowed = env->n_regs - demand;

	/* Only make more free room if we do not have enough */
	if (len > max_allowed) {
		DBG((dbg, DBG_DECIDE, "\t\tdisposing %d values\n", len - max_allowed));

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

	/* TODO: simplify this expression? */
	bi->pressure       = MAX(bi->pressure,       workset_get_length(env->ws));
	bi->front_pressure = MAX(bi->front_pressure, workset_get_length(env->ws));
}

/**
 * For the given block @p block, decide for each values
 * whether it is used from a register or is reloaded
 * before the use.
 */
static void belady(belady_env_t *env, int id) {
	block_info_t *block_info = new_block_info(env, id);
	const ir_node *block     = block_info->bl;

	workset_t *new_vals;
	ir_node *irn;
	int iter;

	DBG((dbg, DBG_WSETS, "Belady on %+F\n", block_info->bl));
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
		if (is_Proj(irn) || is_Phi(irn))
			continue;
		DBG((dbg, DBG_DECIDE, "\t%+F\n", irn));

		if (!block_info->first_non_in)
			block_info->first_non_in = irn;

		/* set instruction in the workset */
		env->instr = irn;

		/* allocate all values _used_ by this instruction */
		workset_clear(new_vals);
		for(i = 0, arity = get_irn_arity(irn); i < arity; ++i) {
			workset_insert(env, new_vals, get_irn_n(irn, i));
		}
		DBG((dbg, DBG_DECIDE, "\t* uses\n"));
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
		DBG((dbg, DBG_DECIDE, "\t* defs\n"));
		displace(block_info, new_vals, 0);

		env->instr_nr++;
	}

	phase_free(&block_info->next_uses);

	/* Remember end-workset for this block */
	block_info->ws_end = workset_clone(env, &env->ob, env->ws);
	DBG((dbg, DBG_WSETS, "End workset for %+F:\n", block));
	workset_foreach(block_info->ws_end, irn, iter)
		DBG((dbg, DBG_WSETS, "  %+F (%u)\n", irn, workset_get_time(block_info->ws_end, iter)));
	DBG((dbg, DBG_WSETS, "Max pressure in block: %d\n", block_info->pressure));

	/* now, initialize the front pressure to 0. */
	block_info->front_pressure = 0;
}

/*
 _____ _                  _       _           _   ____            _
|_   _| |__   ___    __ _| | ___ | |__   __ _| | |  _ \ __ _ _ __| |_
  | | | '_ \ / _ \  / _` | |/ _ \| '_ \ / _` | | | |_) / _` | '__| __|
  | | | | | |  __/ | (_| | | (_) | |_) | (_| | | |  __/ (_| | |  | |_
  |_| |_| |_|\___|  \__, |_|\___/|_.__/ \__,_|_| |_|   \__,_|_|   \__|
                    |___/

*/

#define workset_set_version(ws, i, t) ((ws)->vals[(i)].time = (t))
#define workset_get_version(ws, i)    ((ws)->vals[(i)].time)

#define ver_oldest                        (0)
#define ver_youngest                      ((unsigned) -1)
#define ver_make_newer(v)                 ((v) + 1)
#define ver_is_older(v, w)                ((v) < (w))
#define ver_is_younger(v, w)              ((v) > (w))

enum {
	irn_act_none = 0,
	irn_act_reload,
	irn_act_live_through
};

typedef struct _block_state_t {
	struct _block_state_t *next;
	struct _block_state_t *next_intern;
	block_info_t *bi;
	int pressure;
	workset_t *end_state;
} block_state_t;

typedef struct _irn_action_t {
	struct _irn_action_t *next;
	ir_node *irn;
	const ir_node *bl;
	int act;
} irn_action_t;

typedef struct _global_end_state_t {
	belady_env_t *env;
	bitset_t *succ_phis;
	bitset_t *committed;
	struct obstack obst;
	void *reset_level;
	unsigned version;

	unsigned       *bs_tops_vers;
	block_state_t **bs_tops;
	block_state_t  *bs_top;
	irn_action_t   *ia_top;
} global_end_state_t;

typedef struct {
	void          *obst_level;
	block_state_t *bs_top;
	irn_action_t  *ia_top;
} rollback_info_t;

static INLINE block_state_t *get_block_state(global_end_state_t *ges, const block_info_t *bi)
{
	int id = bi->id;
	assert(!ver_is_younger(ges->bs_tops_vers[id], ges->version));
	return ver_is_older(ges->bs_tops_vers[id], ges->version) ? NULL : ges->bs_tops[bi->id];
}

static INLINE const workset_t *get_end_state(global_end_state_t *ges, block_info_t *bi)
{
	block_state_t *bs = get_block_state(ges, bi);
	return bs ? bs->end_state : bi->ws_end;
}

static block_state_t *new_block_state(global_end_state_t *ges, block_info_t *bi)
{
	block_state_t *bs = get_block_state(ges, bi);
	block_state_t *nw = obstack_alloc(&ges->obst, sizeof(nw[0]));

	nw->next_intern = bs;
	nw->next        = ges->bs_top;
	nw->bi          = bi;

	if (bs) {
		nw->pressure  = bs->pressure;
		nw->end_state = workset_clone(ges->env, &ges->obst, bs->end_state);
	}
	else {
		nw->pressure  = bi->pressure;
		nw->end_state = workset_clone(ges->env, &ges->obst, bi->ws_end);
	}

	ges->bs_top               = nw;
	ges->bs_tops[bi->id]      = nw;
	ges->bs_tops_vers[bi->id] = ges->version;
	return nw;
}

static irn_action_t *new_irn_action(global_end_state_t *ges, ir_node *irn, const ir_node *bl)
{
	irn_action_t *ia = obstack_alloc(&ges->obst, sizeof(ia[0]));

	ia->irn  = irn;
	ia->bl   = bl;
	ia->act  = irn_act_none;
	ia->next = ges->ia_top;
	ges->ia_top = ia;
	return ia;
}

static INLINE rollback_info_t trans_begin(global_end_state_t *ges)
{
	rollback_info_t rb;
	rb.obst_level = obstack_base(&ges->obst);
	rb.bs_top     = ges->bs_top;
	rb.ia_top     = ges->ia_top;
	return rb;
}

static INLINE void trans_rollback(global_end_state_t *ges, rollback_info_t *rb)
{
	block_state_t *bs;

	/* unwind all the stacks indiced with the block number */
	for (bs = ges->bs_top; bs != rb->bs_top; bs = bs->next) {
		unsigned id = bs->bi->id;
		ges->bs_tops[id] = bs->next_intern;
	}

	ges->ia_top = rb->ia_top;
	ges->bs_top = rb->bs_top;
	obstack_free(&ges->obst, rb->obst_level);
}


static double can_bring_in(global_end_state_t *ges, ir_node *bl, ir_node *irn, double limit, int level);

static double can_make_available_at_end(global_end_state_t *ges, ir_node *bl, ir_node *irn, double limit, int level)
{
	block_info_t *bi     = get_block_info(bl);
	const workset_t *end = get_end_state(ges, bi);
	double res;
	int index;

	DBG((dbg, DBG_GLOBAL, "\t%2Dcan make avail %+F at end of %+F\n", level, irn, bl));

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

	/* if the end set contains it already, it is in a reg and it costs nothing
	 * to load it to one. */
	index = workset_get_index(end, irn);
	if (index >= 0) {
		unsigned ver = workset_get_version(end, index);
		DBG((dbg, DBG_GLOBAL, "\t%2Dnode is in the end set and is %s fixed\n",
					level, ver_is_older(ver, ges->version) ? "already" : "not yet"));

		/*
		 * if the version is older, the value is already fixed
		 * and cannot be removed from the end set.
		 *
		 * If not, we will create a new block state for that block since
		 * we modify it by giving the end state a new version.
		 */
		if (ver_is_younger(ver, ges->version)) {
			block_state_t *bs = new_block_state(ges, bi);
			workset_set_version(bs->end_state, index, ges->version);
		}

		res = 0.0;
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
		int n_regs = bi->bel->n_regs;
		int len  = workset_get_length(end);
		int slot = -1;
		int i;

		res = HUGE_VAL;

		/*
		 * look if there is room in the end array
		 * for the variable. Note that this does not
		 * mean that the var can live through the block.
		 * There is just room at the *end*
		 */
		if (len < n_regs) {
			DBG((dbg, DBG_GLOBAL, "\t%2Dthe end set has %d free slots\n", level, n_regs - len));
			slot = len;
		} else {
			for (i = 0; i < len; ++i) {
				unsigned ver = workset_get_version(end, i);
				if (ver_is_younger(ver, ges->version))
					break;
			}

			if (i < len) {
				DBG((dbg, DBG_GLOBAL, "\t%2D%+F (slot %d) can be erased from the end set\n",
							level, end->vals[i].irn, i));
				slot = i;
			}
		}

		/*
		 * finally there is some room. we can at least reload the value.
		 * but we will try to let ot live through anyhow.
		 */
		if (slot >= 0) {
			irn_action_t *vs    = new_irn_action(ges, irn, bi->bl);
			block_state_t *bs   = new_block_state(ges, bi);
			workset_t *end      = bs->end_state;
			ir_node *ins_before = block_info_get_last_ins(bi);
			double reload_here  = be_get_reload_costs(bi->bel->senv, irn, ins_before);
			int pressure_ok     = bs->pressure < n_regs;

			if (reload_here < bi->reload_cost)
				reload_here = 0.0;

			/*
			 * No matter what we do, the value will be in the end set
			 * if the block from now on (of course only regarding the
			 * current state). Enter it and set the new length
			 * appropriately.
			 */
			end->vals[slot].irn     = irn;
			workset_set_version(end, slot, ges->version);
			workset_set_length(end, MAX(workset_get_length(end), slot + 1));

			vs->act = irn_act_reload;
			res     = reload_here;

			DBG((dbg, DBG_GLOBAL, "\t%2Dthere is a free slot. capacity=%d, reload here=%f, pressure %s\n",
						level, n_regs - bs->pressure, reload_here, pressure_ok ? "ok" : "insufficient"));


			/* look if we can bring the value in. */
			if (pressure_ok && reload_here > 0.0) {
				rollback_info_t rb = trans_begin(ges);
				double new_limit   = MIN(reload_here, limit);

				vs->act = irn_act_live_through;
				bs->pressure += 1;
				res = can_bring_in(ges, bl, irn, new_limit, level + 1);

				/*
				 * if bring in is too expensive re-adjust the pressure
				 * and roll back the state
				 */
				if (res >= reload_here) {
					bs->pressure -= 1;
					vs->act = irn_act_reload;
					trans_rollback(ges, &rb);
					res = reload_here;
				}
			}


			DBG((dbg, DBG_GLOBAL, "\t%2D%s\n", level,
						vs->act == irn_act_reload ? "reloading" : "bringing in"));
		}
	}

end:
	DBG((dbg, DBG_GLOBAL, "\t%2D-> %f\n", level, res));
	return res;
}

static double can_bring_in(global_end_state_t *ges, ir_node *bl, ir_node *irn, double limit, int level)
{
	belady_env_t *env = ges->env;
	double glob_costs = HUGE_VAL;

	DBG((dbg, DBG_GLOBAL, "\t%2Dcan bring in (max %f) for %+F at block %+F\n", level, limit, irn, bl));

	if (is_transport_in(bl, irn)) {
		int i, n           = get_irn_arity(bl);
		ir_node **nodes    = alloca(get_irn_arity(bl) * sizeof(nodes[0]));
		rollback_info_t rb = trans_begin(ges);

		glob_costs = 0.0;
		for (i = 0; i < n; ++i) {
			ir_node *pr = get_Block_cfgpred_block(bl, i);
			ir_node *op = is_local_phi(bl, irn) ? get_irn_n(irn, i) : irn;
			double c;

			/*
			 * there might by unknwons as operands of phis in that case
			 * we set the costs to zero, since they won't get spilled.
			 */
			if (arch_irn_consider_in_reg_alloc(env->arch, env->cls, op))
				c = can_make_available_at_end(ges, pr, op, limit - glob_costs, level + 1);
			else
				c = 0.0;

			glob_costs += c;

			if (glob_costs >= limit) {
				glob_costs = HUGE_VAL;
				trans_rollback(ges, &rb);
				goto end;
			}
		}
	}

end:
	DBG((dbg, DBG_GLOBAL, "\t%2D-> %f\n", level, glob_costs));
	return glob_costs;
}

static void materialize_and_commit_end_state(global_end_state_t *ges)
{
	belady_env_t *env = ges->env;
	irn_action_t *ia;
	block_state_t *bs;

	DBG((dbg, DBG_GLOBAL, "\tmaterializing\n"));

	/*
	 * Perform all the variable actions.
	 */
	for (ia = ges->ia_top; ia != NULL; ia = ia->next) {
		switch (ia->act) {
			case irn_act_live_through:
				if (is_local_phi(ia->bl, ia->irn)) {
					bitset_add_irn(ges->succ_phis, ia->irn);
					DBG((dbg, DBG_GLOBAL, "\t\tlive through phi kept alive: %+F\n", ia->irn));
				}
				break;
			case irn_act_reload:
				be_add_reload_at_end(env->senv, ia->irn, ia->bl, env->cls, 1);
				DBG((dbg, DBG_GLOBAL, "\t\tadding reload of %+F at end of %+F\n", ia->irn, ia->bl));
				break;
			default:
				DBG((dbg, DBG_GLOBAL, "\t\t%+F is in the end set of %+F\n", ia->irn, ia->bl));
		}
	}

	/*
	 * Commit the block end states
	 */
	for (bs = ges->bs_top; bs != NULL; bs = bs->next) {
		block_info_t *bi = bs->bi;

		if (!bitset_is_set(ges->committed, bi->id)) {
			DBG((dbg, DBG_GLOBAL, "\t\tcommiting workset of %+F with version %x\n", bi->bl, ges->version));
			// bes->bs->end_state->vals[idx].version = ges->version;
			workset_copy(env, bi->ws_end, bs->end_state);
			DBG((dbg, DBG_GLOBAL, "\t\told pressure: %d, new pressure: %d, end length: %d\n",
						bi->pressure, bs->pressure, workset_get_length(bs->end_state)));
			bi->pressure = bs->pressure;
			/* TODO: commit front pressure */
			bitset_set(ges->committed, bi->id);
		}
	}

	/* clear the committed bitset. the next call is expecting it. */
	bitset_clear_all(ges->committed);
}

static ir_node *better_spilled_here(const bring_in_t *br)
{
	const block_info_t *bi = br->bi;
	double spill_ef        = get_block_info(get_nodes_block(br->irn))->exec_freq;

	/*
	 * If the bring in node is a phi in the bring in block,
	 * we look at all definitions and sum up their execution frequencies,
	 * since spills will be placed there.
	 * (except for the case where an operand is also a phi which is spilled :-( )
	 * If that cost is higher than spilling the phi in that block, we opt for
	 * bringing the phi into the block and spill it there.
	 */
	if (is_local_phi(bi->bl, br->irn)) {
		ir_node *bl = bi->bl;
		int i;

		spill_ef = 0.0;
		for (i = get_Block_n_cfgpreds(bl) - 1; i >= 0; --i)
			spill_ef += get_block_info(get_Block_cfgpred_block(bl, i))->exec_freq;
	}

	return bi->exec_freq < spill_ef ? sched_prev(bi->first_non_in) : NULL;
}

static int get_max_pressure_so_far(const block_info_t *bi, const bring_in_t *br)
{
	const struct list_head *l;
	int res = INT_MIN;

	assert(br->bi == bi);
	for (l = &br->list; l != &bi->br_head; l = l->prev) {
		br  = list_entry(l, bring_in_t, list);
		res = MAX(res, br->sect_pressure);
	}

	/* finally consider the front pressure distance and add the reference line (the global block pressure) */
	return MAX(res, bi->front_pressure);
}

#define block_last_bring_in(bi)  list_entry((bi)->br_head.prev, bring_in_t, list)

#if 0
static int get_block_max_pressure(const block_info_t *bi)
{
	int max = get_max_pressure_so_far(bi, block_last_bring_in(bi));
	return MAX(bi->pressure, max);
}
#endif

/**
 * Try to bring a variable into a block.
 * @param ges   The state of all end sets.
 * @param block The block.
 * @param irn   The variable.
 */
static void optimize_variable(global_end_state_t *ges, bring_in_t *br)
{
	block_info_t *bi    = br->bi;
	ir_node *irn              = br->irn;
	ir_node *bl               = bi->bl;
	belady_env_t *env         = ges->env;
	void *reset_level         = obstack_base(&ges->obst);
	int k                     = env->n_regs;
	int pressure_upto_use     = get_max_pressure_so_far(bi, br);
	int front_pressure        = bi->front_pressure;
	ir_node *better_spill_loc = NULL;

	assert(front_pressure <= k);
	assert(pressure_upto_use <= k);

	DBG((dbg, DBG_GLOBAL, "fixing %+F at %+F (%f), front pr: %d, pr to use: %d, first use: %u\n",
				irn, bl, bi->exec_freq, front_pressure, pressure_upto_use, br->first_use));

	/*
	 * if we cannot bring the value to the use, let's see ifit would be worthwhile
	 * to bring the value to the beginning of the block to have a better spill
	 * location.
	 *
	 * better _spilled_here will return a node where the value can be spilled after
	 * or NULL if this block does not provide a better spill location.
	 */
	if (pressure_upto_use >= k && front_pressure < k)
		better_spill_loc = better_spilled_here(br);

	/*
	 * If either we can bring the value to the use or we should try
	 * to bring it here to do the spill here, let's try to bring it in.
	 */
	if (better_spill_loc || pressure_upto_use < k) {
		block_state_t *bs;
		double bring_in_costs, local_costs;
		rollback_info_t trans;
		int pressure_inc;

		/* process all variables which shall be in a reg at
		 * the beginning of the block in the order of the next use. */
		local_costs  = be_get_reload_costs(env->senv, irn, br->first_use);

		/* reset the lists */
		ges->bs_top  = NULL;
		ges->ia_top  = NULL;

		/* if the variable will live into this block, we must adapt the pressure.
		 * The new pressure is the MAX of:
		 * 1) the total block pressure
		 * 2) the pressure so far + the front pressure increase + 1
		 *
		 * If the second is larger than the first,
		 * we have to increment the total block pressure and hence
		 * save the old pressure to restire it in case of failing to
		 * bring the variable into the block in a register.
		 */
		trans = trans_begin(ges);
		bs    = new_block_state(ges, bi);
		pressure_inc = MAX(bs->pressure, better_spill_loc ? front_pressure : pressure_upto_use + 1);
		bs->pressure = pressure_inc;


		assert(bi->pressure <= k);
		DBG((dbg, DBG_GLOBAL, "\ttrans in var %+F, version %x\n", irn, ges->version));
		bring_in_costs = can_bring_in(ges, bl, irn, local_costs, 1);
		DBG((dbg, DBG_GLOBAL, "\tbring in: %f, local: %f\n", bring_in_costs, local_costs));

		/*
		 * Following cases can now occur:
		 * 1) There is room and costs ok
		 * 2) Cannot bring to use but can spill at begin and costs are ok
		 * 3) neither of both worked.
		 *
		 * following actions can be taken:
		 * a) commit changes
		 * b) mark phi as succeded if node was phi
		 * c) insert reload at use location
		 * d) give a spill location hint
		 *
		 * this is the case/action matrix
		 *   | abcd
		 * --+----------
		 * 1 | XX
		 * 2 | XXXX
		 * 3 |   X
		 */

		/* the costs were acceptable... */
		if (bring_in_costs < local_costs) {
			bring_in_t *iter;

			/*
			 * case 1 and first part of case 2:
			 * commit all the changes done. this manifests the bring-in action.
			 * if the transport-in was a phi (that is actually used in block)
			 * mark it in the succ_phis set to *not* phi spill it.
			 */
			materialize_and_commit_end_state(ges);
			if (is_local_phi(bl, irn))
				bitset_add_irn(ges->succ_phis, irn);

			pressure_inc = bi->pressure - pressure_inc;
			assert(pressure_inc >= 0);

			DBG((dbg, DBG_GLOBAL, "\t-> bring it in\n"));

			/* second half of case 2 */
			if (pressure_upto_use >= k) {
				DBG((dbg, DBG_GLOBAL, "\t-> use blocked. local reload: %+F, try spill at: %+F\n",
							br->first_use, better_spill_loc));
				be_add_reload2(env->senv, irn, br->first_use, better_spill_loc, env->cls, 1);
			}

			/*
			 * go from the last bring in use to the first and add all the variabled
			 * which additionally live through the block to their pressure.
			 * at the point were the actually treated use is, we have to increase
			 * the pressure by one more as the nrought in value starts to count.
			 * Finally, adjust the front pressure as well.
			 */
			list_for_each_entry_reverse(bring_in_t, iter, &bi->br_head, list) {
				if (iter == br)
					pressure_inc += pressure_upto_use < k;
				iter->sect_pressure += pressure_inc;
			}
			bi->front_pressure += pressure_inc;
		}

		/* case 3: nothing worked. insert normal reload and rollback. */
		else {
			DBG((dbg, DBG_GLOBAL, "\t-> bring in was too expensive. local reload: %+F\n", br->first_use));
			be_add_reload(env->senv, irn, br->first_use, env->cls, 1);
			trans_rollback(ges, &trans);
		}
	}

	/* there was no opportunity for optimization at all. reload and be sad ...  */
	else {
		DBG((dbg, DBG_GLOBAL, "\t-> can\'t do anything but reload before %+F\n", br->first_use));
		be_add_reload(env->senv, irn, br->first_use, env->cls, 1);
	}

	DBG((dbg, DBG_GLOBAL, "\n"));

	/* reset the obstack and create a new version. */
	obstack_free(&ges->obst, reset_level);
	ges->version = ver_make_newer(ges->version);
}

static bring_in_t **determine_global_order(belady_env_t *env)
{
	bring_in_t **res;
	bring_in_t *elm;
	int i, n = 0;

	for (i = env->n_blocks - 1; i >= 0; --i) {
		block_info_t *bi = get_block_info(env->blocks[i]);
		list_for_each_entry(bring_in_t, elm, &bi->br_head, list) {
			obstack_ptr_grow(&env->ob, elm);
			n += 1;
		}
	}

	obstack_ptr_grow(&env->ob, NULL);
	res = obstack_finish(&env->ob);
	qsort(res, n, sizeof(res[0]), bring_in_cmp);
	return res;
}

static void global_assign(belady_env_t *env)
{
	global_end_state_t ges;
	bring_in_t **br;
	int i, j;

	/*
	 * sort the blocks according to execution frequency.
	 * That's not necessary for belady() but for the global pass later on.
	 */
	qsort(env->blocks, env->n_blocks, sizeof(env->blocks[0]), block_freq_dfs_gt);

	memset(&ges, 0, sizeof(ges));
	obstack_init(&ges.obst);
	ges.env          = env;
	ges.version      = ver_make_newer(ver_oldest);
	ges.succ_phis    = bitset_irg_obstack_alloc(&ges.obst, env->irg);
	ges.committed    = bitset_obstack_alloc(&ges.obst, env->n_blocks);
	ges.bs_tops      = obstack_alloc(&ges.obst, sizeof(ges.bs_tops[0])      * env->n_blocks);
	ges.bs_tops_vers = obstack_alloc(&ges.obst, sizeof(ges.bs_tops_vers[0]) * env->n_blocks);

	/* invalidate all state stack pointer versions */
	for (i = 0; i < env->n_blocks; ++i) {
		block_info_t *bi = get_block_info(env->blocks[i]);
		ges.bs_tops_vers[i] = ver_oldest;

		/* Set all block end sets entries to the youngest version */
		for (j = workset_get_length(bi->ws_end) - 1; j >= 0; --j)
			workset_set_version(bi->ws_end, j, ver_youngest);
	}

	/* determine ordeer and optimize them */
	for (br = determine_global_order(env); *br; ++br)
		optimize_variable(&ges, *br);

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
}

static void collect_blocks(ir_node *bl, void *data)
{
	belady_env_t *env = data;
	++env->n_blocks;
	obstack_ptr_grow(&env->ob, bl);
}

/**
 * Do spilling for a register class on a graph using the belady heuristic.
 * In the transformed graph, the register pressure never exceeds the number
 * of available registers.
 *
 * @param birg  The backend graph
 * @param cls   The register class to spill
 */
void be_spill_belady(be_irg_t *birg, const arch_register_class_t *cls)
{
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
	env.irg        = irg;
	env.arch       = be_get_birg_arch_env(birg);
	env.cls        = cls;
	env.lv         = be_get_birg_liveness(birg);
	env.dfs        = env.lv->dfs;
	env.n_regs     = n_regs;
	env.ws         = new_workset(&env, &env.ob);
	env.senv       = be_new_spill_env(birg);
	env.ef         = be_get_birg_exec_freq(birg);
	env.spilled    = bitset_irg_obstack_alloc(&env.ob, irg);
	env.n_blocks   = 0;

	irg_block_walk_graph(irg, NULL, collect_blocks, &env);
	obstack_ptr_grow(&env.ob, NULL);
	env.blocks = obstack_finish(&env.ob);

	/* renumbering in the blocks gives nicer debug output as number are smaller. */
#ifdef DEBUG_libfirm
	for (i = 0; i < env.n_blocks; ++i)
		sched_renumber(env.blocks[i]);
#endif

	/* Fix high register pressure in blocks with belady algorithm */
	for (i = 0; i < env.n_blocks; ++i)
		belady(&env, i);

	global_assign(&env);

	/* Insert spill/reload nodes into the graph and fix usages */
	be_insert_spills_reloads(env.senv);

	/* clean up */
	be_delete_spill_env(env.senv);

	obstack_free(&env.ob, NULL);
}

void be_init_spillbelady2(void)
{
	lc_opt_entry_t *be_grp    = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *spill_grp = lc_opt_get_grp(be_grp, "spill");
	lc_opt_entry_t *bel2_grp  = lc_opt_get_grp(spill_grp, "belady2");

	static be_spiller_t belady_spiller = {
		be_spill_belady
	};

	lc_opt_add_table(bel2_grp, options);
	be_register_spiller("belady2", &belady_spiller);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.belady2");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillbelady2);
