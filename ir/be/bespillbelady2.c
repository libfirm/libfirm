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
	int id;

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
static void belady(belady_env_t *env, int id) {
	block_info_t *block_info = new_block_info(env, id);
	const ir_node *block     = block_info->bl;
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

#define workset_set_version(ws, i, t) ((ws)->vals[(i)].time = (t))
#define workset_get_version(ws, i)    ((ws)->vals[(i)].time)

#define ver_oldest                        (0)
#define ver_youngest                      ((unsigned) -1)
#define ver_make_newer(v)                 ((v) + 1)
#define ver_is_older(v, w)                ((v) < (w))
#define ver_is_younger(v, w)              ((v) > (w))

static int block_freq_gt(const void *a, const void *b)
{
	const ir_node * const *p = a;
	const ir_node * const *q = b;
	block_info_t *pi = get_block_info(*p);
	block_info_t *qi = get_block_info(*q);
	double diff = qi->exec_freq - pi->exec_freq;
	return (diff > 0) - (diff < 0);
}

enum {
	irn_act_none = 0,
	irn_act_reload,
	irn_act_live_through
};

typedef struct _block_state_t {
	struct _block_state_t *next;
	struct _block_state_t *next_intern;
	block_info_t *bi;
	unsigned pressure;
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

static INLINE block_state_t *get_block_state(global_end_state_t *ges, block_info_t *bi)
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
			int pressure_ok     = bs->pressure < (unsigned) n_regs;

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
			if (pressure_ok) {
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
			bitset_set(ges->committed, bi->id);
		}
	}

	/* clear the committed bitset. the next call is expecting it. */
	bitset_clear_all(ges->committed);
}

/**
 * Examine all irns which shall be in regs at the beginning of the
 * block.
 */
static void fix_block_borders(global_end_state_t *ges, ir_node *block) {
	block_info_t *bi  = get_block_info(block);
	belady_env_t *env = ges->env;
	void *reset_level = obstack_base(&ges->obst);

	ir_node *irn;
	int i;

	for (i = workset_get_length(bi->ws_end) - 1; i >= 0; --i)
		workset_set_version(bi->ws_end, i, ver_youngest);

	DBG((dbg, DBG_GLOBAL, "fixing block borders at %+F (%f)\n", block, bi->exec_freq));

	/* process all variables which shall be in a reg at
	 * the beginning of the block in the order of the next use. */
	workset_foreach(bi->entrance_reg, irn, i) {
		double local_costs = be_get_reload_costs(env->senv, irn, bi->first_non_in);
		double bring_in_costs;

		/* reset the lists */
		ges->bs_top  = NULL;
		ges->ia_top  = NULL;

		DBG((dbg, DBG_GLOBAL, "\ttrans in var %+F, version %x\n", irn, ges->version));

		bring_in_costs = can_bring_in(ges, block, irn, local_costs, 1);

		DBG((dbg, DBG_GLOBAL, "\tbring in: %f, local: %f", bring_in_costs, local_costs));

		/*
		 * we were not able to let the value arrive
		 * in a register at the entrance of the block
		 * or it is too costly, so we have to do the reload locally
		 */
		if (bring_in_costs >= local_costs) {
			DBG((dbg, DBG_GLOBAL, " -> do local reload\n"));
			be_add_reload(env->senv, irn, bi->first_non_in, env->cls, 1);
		} else {
			/*
			 * if the transport-in was a phi (that is actually used in block)
			 * it will no longer remain and we have to spill it completely.
			 */
			if (is_local_phi(block, irn))
				bitset_add_irn(ges->succ_phis, irn);

			DBG((dbg, DBG_GLOBAL, " -> bring it in\n"));
			materialize_and_commit_end_state(ges);
		}

		DBG((dbg, DBG_GLOBAL, "\n"));

		/* reset the obstack and create a new version. */
		obstack_free(&ges->obst, reset_level);
		ges->version = ver_make_newer(ges->version);
	}
}

static void global_assign(belady_env_t *env)
{
	global_end_state_t ges;
	int i;

	/*
	 * sort the blocks according to execution frequency.
	 * That's not necessary for belady() but for the global pass later on.
	 */
	qsort(env->blocks, env->n_blocks, sizeof(env->blocks[0]), block_freq_gt);

	memset(&ges, 0, sizeof(ges));
	obstack_init(&ges.obst);
	ges.env          = env;
	ges.version      = ver_make_newer(ver_oldest);
	ges.succ_phis    = bitset_irg_obstack_alloc(&ges.obst, env->irg);
	ges.committed    = bitset_obstack_alloc(&ges.obst, env->n_blocks);
	ges.bs_tops      = obstack_alloc(&ges.obst, sizeof(ges.bs_tops[0])      * env->n_blocks);
	ges.bs_tops_vers = obstack_alloc(&ges.obst, sizeof(ges.bs_tops_vers[0]) * env->n_blocks);

	for (i = 0; i < env->n_blocks; ++i)
		ges.bs_tops_vers[i] = ver_oldest;

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
	env.irg       = irg;
	env.arch      = birg->main_env->arch_env;
	env.cls       = cls;
	env.lv        = be_get_birg_liveness(birg);
	env.n_regs    = n_regs;
	env.ws        = new_workset(&env, &env.ob);
	env.senv      = be_new_spill_env(birg);
	env.ef        = be_get_birg_exec_freq(birg);
	env.n_blocks  = 0;

	irg_block_walk_graph(irg, NULL, collect_blocks, &env);
	obstack_ptr_grow(&env.ob, NULL);
	env.blocks = obstack_finish(&env.ob);

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
	static be_spiller_t belady_spiller = {
		be_spill_belady
	};

	be_register_spiller("belady2", &belady_spiller);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.belady2");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillbelady2);
