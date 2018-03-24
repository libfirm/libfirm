/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Beladys spillalgorithm.
 * @author      Daniel Grund, Matthias Braun
 * @date        20.09.2005
 */
#include "bearch.h"
#include "bechordal_t.h"
#include "beirg.h"
#include "belive.h"
#include "beloopana.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespill.h"
#include "bespillutil.h"
#include "beuses.h"
#include "beutil.h"
#include "debug.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irloop.h"
#include "irmode_t.h"
#include "irmode.h"
#include "irnode_t.h"
#include "irnode.h"
#include "irtools.h"
#include "obst.h"
#include "statev_t.h"
#include "util.h"
#include <stdbool.h>

#define DBG_SPILL     1
#define DBG_WSETS     2
#define DBG_FIX       4
#define DBG_DECIDE    8
#define DBG_START    16
#define DBG_SLOTS    32
#define DBG_TRACE    64
#define DBG_WORKSET 128
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define TIME_UNDEFINED 6666

/**
 * An association between a node and a point in time.
 */
typedef struct loc_t {
	ir_node          *node;
	unsigned          time;     /**< A use time (see beuses.h). */
	bool              spilled;  /**< value was already spilled on this path */
} loc_t;

typedef struct workset_t {
	unsigned len;     /**< current length */
	loc_t    vals[];  /**< array of the values/distances in this working set */
} workset_t;

static struct obstack               obst;
static const arch_register_class_t *cls;
static const be_lv_t               *lv;
static be_loopana_t                *loop_ana;
static unsigned                     n_regs;
static workset_t                   *ws;     /**< the main workset used while
	                                             processing a block. */
static be_uses_t                   *uses;   /**< env for the next-use magic */
static spill_env_t                 *senv;   /**< see bespill.h */
static ir_node                    **blocklist;
static workset_t                   *temp_workset;

static bool                         move_spills      = true;
static bool                         respectloopdepth = true;
static bool                         improve_known_preds = true;
/* factor to weight the different costs of reloading/rematerializing a node
   (see bespill.h be_get_reload_costs_no_weight) */
static int                          remat_bonus      = 10;

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_BOOL   ("movespills", "try to move spills out of loops", &move_spills),
	LC_OPT_ENT_BOOL   ("respectloopdepth", "outermost loop cutting", &respectloopdepth),
	LC_OPT_ENT_BOOL   ("improveknownpreds", "known preds cutting", &improve_known_preds),
	LC_OPT_ENT_INT    ("rematbonus", "give bonus to rematerialisable nodes", &remat_bonus),
	LC_OPT_LAST
};

/**
 * Alloc a new workset on obstack @p ob with maximum size @p max
 */
static workset_t *new_workset(void)
{
	return OALLOCFZ(&obst, workset_t, vals, n_regs);
}

/**
 * Compute the actual number of registers used by @param ws
 * taking into account the register widths of all contained nodes
 */
static unsigned workset_used_length(const workset_t *ws)
{
	unsigned used_length = 0;
	for (unsigned i = 0, len = ws->len; i < len; ++i) {
		used_length += arch_get_irn_register_req_width(ws->vals[i].node);
	}
	return used_length;
}

/**
 * Compute the actual number of registers used by the nodes contained in @param locs
 */
static unsigned loc_get_used_len(const loc_t *locs)
{
	unsigned used_length = 0;
	for (unsigned i = 0; i < ARR_LEN(locs); ++i) {
		used_length += arch_get_irn_register_req_width(locs[i].node);
	}
	return used_length;
}

/**
 * Copy workset @param src to @param tgt
 */
static void workset_copy(workset_t *dest, const workset_t *src)
{
	size_t size = sizeof(*src) + n_regs * sizeof(src->vals[0]);
	memcpy(dest, src, size);
}

/**
 * Alloc a new instance on obstack and make it equal to @param workset
 */
static workset_t *workset_clone(workset_t *workset)
{
	workset_t *res = OALLOCF(&obst, workset_t, vals, n_regs);
	workset_copy(res, workset);
	return res;
}

/**
 * Overwrites the current content array of @param ws with the
 * @param count locations given at memory @param locs.
 * Set the length of @param ws to count.
 */
static void workset_bulk_fill(workset_t *workset, int count, const loc_t *locs)
{
	workset->len = count;
	MEMCPY(&workset->vals[0], locs, count);
}

static loc_t *workset_contains(workset_t *const ws, ir_node const *const val)
{
	for (unsigned i = 0, len = ws->len; i < len; ++i) {
		loc_t *const loc = &ws->vals[i];
		if (loc->node == val)
			return loc;
	}

	return NULL;
}

/**
 * Inserts the value @p val into the workset, iff it is not
 * already contained. The workset must not be full.
 */
static void workset_insert(workset_t *workset, ir_node *val, bool spilled)
{
	/* check for current regclass */
	assert(arch_irn_consider_in_reg_alloc(cls, val));

	/* check if val is already contained */
	loc_t *const oloc = workset_contains(workset, val);
	if (oloc) {
		if (spilled)
			oloc->spilled = true;
		return;
	}

	/* insert val */
	assert(workset_used_length(workset) < n_regs && "Workset already full!");
	loc_t *loc   = &workset->vals[workset->len];
	loc->node    = val;
	loc->spilled = spilled;
	loc->time    = TIME_UNDEFINED;
	workset->len++;
}

/**
 * Removes all entries from this workset
 */
static void workset_clear(workset_t *workset)
{
	workset->len = 0;
}

/**
 * Removes the value @p val from the workset if present.
 */
static void workset_remove(workset_t *workset, ir_node *val)
{
	loc_t *const loc = workset_contains(workset, val);
	if (loc)
		*loc = workset->vals[--workset->len];
}

static int loc_compare(const void *a, const void *b)
{
	const loc_t   *p  = ((const loc_t*) a);
	const loc_t   *q  = ((const loc_t*) b);
	const unsigned pt = p->time;
	const unsigned qt = q->time;

	if (pt < qt)
		return -1;
	if (pt > qt)
		return 1;

	return get_irn_node_nr(p->node) - get_irn_node_nr(q->node);
}

static void workset_sort(workset_t *workset)
{
	QSORT(workset->vals, workset->len, loc_compare);
}

static inline unsigned workset_get_time(const workset_t *workset, unsigned idx)
{
	return workset->vals[idx].time;
}

static inline void workset_set_time(workset_t *workset, unsigned idx,
                                    unsigned time)
{
	workset->vals[idx].time = time;
}

static inline unsigned workset_get_length(const workset_t *workset)
{
	return workset->len;
}

static inline void workset_set_length(workset_t *workset, unsigned len)
{
	workset->len = len;
}

static inline ir_node *workset_get_val(const workset_t *workset, unsigned idx)
{
	return workset->vals[idx].node;
}

/**
 * Iterates over all values in the working set.
 * @p ws The workset to iterate
 * @p v  A variable to put the current value in
 * @p i  An integer for internal use
 */
#define workset_foreach(ws, v, i) \
	for (unsigned i = 0; i < ws->len ? v = ws->vals[i].node, 1 : 0; ++i)

typedef struct block_info_t {
	workset_t *start_workset;
	workset_t *end_workset;
} block_info_t;

static block_info_t *new_block_info(ir_node *block)
{
	block_info_t *info = OALLOCZ(&obst, block_info_t);
	set_irn_link(block, info);
	return info;
}

static inline block_info_t *get_block_info(const ir_node *block)
{
	return (block_info_t*)get_irn_link(block);
}

/**
 * @return The distance to the next use or 0 if irn has dont_spill flag set
 */
static unsigned get_distance(ir_node *from, const ir_node *def,
                             bool skip_from_uses)
{
	assert(!arch_irn_is_ignore(def));

	be_next_use_t use  = be_get_next_use(uses, from, def, skip_from_uses);
	unsigned      time = use.time;
	if (USES_IS_INFINITE(time))
		return USES_INFINITY;

	/* We have to keep nonspillable nodes in the workingset */
	if (arch_get_irn_flags(skip_Proj_const(def)) & arch_irn_flag_dont_spill)
		return 0;

	/* give some bonus to rematerialisable nodes */
	if (remat_bonus > 0) {
		unsigned costs = be_get_reload_costs_no_weight(senv, def, use.before);
		assert(costs * remat_bonus < 1000);
		time += 1000 - (costs * remat_bonus);
	}

	return time;
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
static void displace(workset_t *const new_vals, bool const is_usage,
                     ir_node *const instr, be_add_pressure_t const add_pressure)
{
	ir_node **to_insert = ALLOCAN(ir_node*, n_regs);
	bool     *spilled   = ALLOCAN(bool,     n_regs);

	/* 1. Identify the number of needed slots and the values to reload */
	unsigned  demand = 0;
	ir_node  *val    = NULL;
	workset_foreach(new_vals, val, iter) {
		bool reloaded = false;

		if (!workset_contains(ws, val)) {
			DB((dbg, DBG_DECIDE, "    insert %+F\n", val));
			if (is_usage) {
				DB((dbg, DBG_SPILL, "Reload %+F before %+F\n", val, instr));
				be_add_reload(senv, val, instr);
				reloaded = true;
			}
		} else {
			DB((dbg, DBG_DECIDE, "    %+F already in workset\n", val));
			assert(is_usage);
			/* remove the value from the current workset so it is not accidently
			 * spilled */
			workset_remove(ws, val);
		}
		spilled[iter]   = reloaded;
		to_insert[iter] = val;

		demand += arch_get_irn_register_req_width(val);

	}
	demand += add_pressure;

	/* 2. Make room for at least 'demand' slots */
	unsigned len           = workset_get_length(ws);
	int single_regs_needed = workset_used_length(ws) + demand - n_regs;
	assert(single_regs_needed <= (int)workset_used_length(ws));



	/* Only make more free room if we do not have enough */
	if (single_regs_needed > 0) {

		/* calculate current next-use distance for live values */
		for (unsigned i = 0; i < len; ++i) {
			ir_node  *val  = workset_get_val(ws, i);
			unsigned  dist = get_distance(instr, val, !is_usage);
			workset_set_time(ws, i, dist);
		}

		/* sort entries by increasing nextuse-distance*/
		workset_sort(ws);

		/* calculate actual spills needed */
		int spills_needed = 0;
		for (int i = len - 1; i >= 0 && single_regs_needed > 0; --i) {
			single_regs_needed -= arch_get_irn_register_req_width(ws->vals[i].node);
			spills_needed++;
		}
		DB((dbg, DBG_DECIDE, "    disposing %d values\n", spills_needed));

		for (int i = len - spills_needed; i < (int)len; ++i) {
			ir_node *val = ws->vals[i].node;
			DB((dbg, DBG_DECIDE, "    disposing node %+F (%u)\n", val,
			    workset_get_time(ws, i)));

			if (move_spills && !USES_IS_INFINITE(workset_get_time(ws, i))
			    && !ws->vals[i].spilled) {
				ir_node *after_pos = sched_prev(instr);
				DB((dbg, DBG_DECIDE, "Spill %+F after node %+F\n", val,
					after_pos));
				be_add_spill(senv, val, after_pos);
			}
		}

		/* kill the last 'spills_needed' entries in the array */
		workset_set_length(ws, len - spills_needed);
	}

	/* 3. Insert the new values into the workset */
	for (unsigned i = 0; i < new_vals->len; ++i) {
		ir_node *val = to_insert[i];
		workset_insert(ws, val, spilled[i]);
	}
}

typedef enum available_t {
	AVAILABLE_UNKNOWN    = 0U,
	AVAILABLE_EVERYWHERE = 1U << 0,
	AVAILABLE_NOWHERE    = 1U << 1,
	AVAILABLE_PARTLY     = AVAILABLE_EVERYWHERE | AVAILABLE_NOWHERE,
} available_t;

static available_t available_in_all_preds(workset_t* const* pred_worksets,
                                          size_t n_pred_worksets,
                                          const ir_node *value,
                                          bool is_local_phi)
{
	assert(n_pred_worksets > 0);

	/* value available in all preds? */
	available_t available = AVAILABLE_UNKNOWN;
	for (size_t i = 0; i < n_pred_worksets; ++i) {
		const ir_node *l_value;
		if (is_local_phi) {
			l_value = get_Phi_pred(value, i);
		} else {
			l_value = value;
		}

		available |= workset_contains(pred_worksets[i], l_value) ?
			AVAILABLE_EVERYWHERE : AVAILABLE_NOWHERE;
	}

	assert(available != AVAILABLE_UNKNOWN);
	return available;
}

/**
 * Decides whether a specific node should be in the start workset or not
 */
static loc_t to_take_or_not_to_take(ir_node* first, ir_node *node,
                                    ir_loop *loop, unsigned available)
{
	loc_t loc;
	loc.time    = USES_INFINITY;
	loc.node    = node;
	loc.spilled = false;

	/* We have to keep nonspillable nodes in the workingset */
	if (arch_get_irn_flags(skip_Proj_const(node)) & arch_irn_flag_dont_spill) {
		loc.time = 0;
		DB((dbg, DBG_START, "    %+F taken (dontspill node)\n", node, loc.time));
		return loc;
	}

	be_next_use_t next_use = be_get_next_use(uses, first, node, false);
	if (USES_IS_INFINITE(next_use.time)) {
		/* the nodes marked as live in shouldn't be dead, so it must be a phi */
		assert(is_Phi(node));
		loc.time = USES_INFINITY;
		DB((dbg, DBG_START, "    %+F not taken (dead)\n", node));
		return loc;
	}

	loc.time = next_use.time;
	if (improve_known_preds) {
		if (available == AVAILABLE_EVERYWHERE) {
			DB((dbg, DBG_START, "    %+F taken (%u, live in all preds)\n",
			    node, loc.time));
			return loc;
		} else if (available == AVAILABLE_NOWHERE) {
			DB((dbg, DBG_START, "    %+F not taken (%u, live in no pred)\n",
			    node, loc.time));
			loc.time = USES_INFINITY;
			return loc;
		}
	}

	if (!respectloopdepth || next_use.outermost_loop >= get_loop_depth(loop)) {
		DB((dbg, DBG_START, "    %+F taken (%u, loop %d)\n", node, loc.time,
		    next_use.outermost_loop));
	} else {
		loc.time = USES_PENDING;
		DB((dbg, DBG_START, "    %+F delayed (outerdepth %d < loopdepth %d)\n",
		    node, next_use.outermost_loop, get_loop_depth(loop)));
	}

	return loc;
}

/**
 * Computes the start-workset for a block with multiple predecessors. We assume
 * that at least 1 of the predeccesors is a back-edge which means we're at the
 * beginning of a loop. We try to reload as much values as possible now so they
 * don't get reloaded inside the loop.
 */
static void decide_start_workset(ir_node *const block)
{
	/* check predecessors */
	int         n_cfgpreds      = get_Block_n_cfgpreds(block);
	workset_t **pred_worksets   = ALLOCAN(workset_t*, n_cfgpreds);
	bool        all_preds_known = true;
	for (int i = 0; i < n_cfgpreds; ++i) {
		ir_node      *pred_block = get_Block_cfgpred_block(block, i);
		block_info_t *pred_info  = get_block_info(pred_block);

		if (pred_info == NULL) {
			pred_worksets[i] = NULL;
			all_preds_known   = false;
		} else {
			pred_worksets[i] = pred_info->end_workset;
		}
	}

	/* Collect all values living at start of block */
	loc_t *starters = NEW_ARR_F(loc_t, 0);
	loc_t *delayed  = NEW_ARR_F(loc_t, 0);

	DB((dbg, DBG_START, "Living at start of %+F:\n", block));
	ir_node *first = sched_first(block);

	/* check all Phis first */
	ir_loop *loop = get_irn_loop(block);
	sched_foreach_phi(block, node) {
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		available_t available = AVAILABLE_UNKNOWN;
		if (all_preds_known) {
			available = available_in_all_preds(pred_worksets, n_cfgpreds,
			                                   node, true);
		}

		loc_t loc = to_take_or_not_to_take(first, node, loop, available);
		if (!USES_IS_INFINITE(loc.time)) {
			if (USES_IS_PENDING(loc.time))
				ARR_APP1(loc_t, delayed, loc);
			else
				ARR_APP1(loc_t, starters, loc);
		} else {
			be_spill_phi(senv, node);
		}
	}

	/* check all Live-Ins */
	be_lv_foreach_cls(lv, block, be_lv_state_in, cls, node) {
		available_t available = AVAILABLE_UNKNOWN;
		if (all_preds_known) {
			available = available_in_all_preds(pred_worksets, n_cfgpreds,
			                                   node, false);
		}

		loc_t loc = to_take_or_not_to_take(first, node, loop, available);
		if (!USES_IS_INFINITE(loc.time)) {
			if (USES_IS_PENDING(loc.time))
				ARR_APP1(loc_t, delayed, loc);
			else
				ARR_APP1(loc_t, starters, loc);
		}
	}

	unsigned pressure = be_get_loop_pressure(loop_ana, cls, loop);
	assert(loc_get_used_len(delayed) <= pressure);
	int free_slots          = n_regs - loc_get_used_len(starters);
	int free_pressure_slots = n_regs - (pressure - loc_get_used_len(delayed));
	free_slots              = MIN(free_slots, free_pressure_slots);

	/* so far we only put nodes into the starters list that are used inside
	 * the loop. If register pressure in the loop is low then we can take some
	 * values and let them live through the loop */
	DB((dbg, DBG_START, "Loop pressure %d, taking %d delayed vals\n",
	    pressure, free_slots));
	if (free_slots > 0) {
		QSORT_ARR(delayed, loc_compare);

		for (size_t i = 0; i < ARR_LEN(delayed) && free_slots > 0; ++i) {
			loc_t *loc = & delayed[i];
			/* skip if we don't have enough free slots for this delayed node
			 * but continue, next nodes may require less free slots! */
			if ((free_slots - arch_get_irn_register_req_width(loc->node)) < 0) {
				continue;
			}
			if (!is_Phi(loc->node)) {
				/* don't use values which are dead in a known predecessors
				 * to not induce unnecessary reloads */
				for (int p = 0; p < n_cfgpreds; ++p) {
					ir_node      *pred_block = get_Block_cfgpred_block(block, p);
					block_info_t *pred_info  = get_block_info(pred_block);

					if (pred_info == NULL)
						continue;

					if (!workset_contains(pred_info->end_workset, loc->node)) {
						DB((dbg, DBG_START,
							"    delayed %+F not live at pred %+F\n", loc->node,
							pred_block));
						goto skip_delayed;
					}
				}
			}
			DB((dbg, DBG_START, "    delayed %+F taken\n", loc->node));
			ARR_APP1(loc_t, starters, *loc);
			free_slots -= arch_get_irn_register_req_width(loc->node);
			loc->node = NULL;
		skip_delayed:
			;
		}
	}

	/* spill phis (the actual phis not just their values) that are in this block
	 * but not in the start workset */
	for (size_t i = 0, len = ARR_LEN(delayed); i < len; ++i) {
		ir_node *node = delayed[i].node;
		if (node == NULL || !is_Phi(node) || get_nodes_block(node) != block)
			continue;

		DB((dbg, DBG_START, "    spilling delayed phi %+F\n", node));
		be_spill_phi(senv, node);
	}
	DEL_ARR_F(delayed);

	/* Sort start values by first use */
	QSORT_ARR(starters, loc_compare);

	/* count how many values can be copied from starters to start workset*/
	unsigned ws_count;
	if (loc_get_used_len(starters) <= n_regs) {
		ws_count = MIN((unsigned) ARR_LEN(starters), n_regs);
	} else {
		ws_count = ARR_LEN(starters);
		unsigned used_len = loc_get_used_len(starters);
		for (int i = ws_count - 1; i >= 0 && used_len > n_regs; --i) {
			used_len -= arch_get_irn_register_req_width(starters[i].node);
			ws_count -= 1;
		}
	}
	/* Copy the best ones from starters to start workset */
	assert(ws_count <= n_regs);
	workset_clear(ws);
	workset_bulk_fill(ws, ws_count, starters);

	/* spill phis (the actual phis not just their values) that are in this block
	 * but not in the start workset */
	for (size_t i = ws_count, len = ARR_LEN(starters); i < len; ++i) {
		ir_node *node = starters[i].node;
		if (!is_Phi(node) || get_nodes_block(node) != block)
			continue;

		DB((dbg, DBG_START, "    spilling phi %+F\n", node));
		be_spill_phi(senv, node);
	}
	DEL_ARR_F(starters);

	/* determine spill status of the values: If there's 1 pred block (which
	 * is no backedge) where the value is spilled then we must set it to
	 * spilled here. */
	for (unsigned i = 0; i < ws_count; ++i) {
		loc_t   *loc   = &ws->vals[i];
		ir_node *value = loc->node;

		/* phis from this block aren't spilled */
		if (get_nodes_block(value) == block) {
			assert(is_Phi(value));
			loc->spilled = false;
			continue;
		}

		/* determine if value was spilled on any predecessor */
		bool spilled = false;
		for (int n = 0; n < n_cfgpreds; ++n) {
			workset_t *pred_workset = pred_worksets[n];
			if (pred_workset == NULL)
				continue;

			loc_t const *const pred_loc = workset_contains(pred_workset, value);
			if (pred_loc && pred_loc->spilled) {
				spilled = true;
				break;
			}
		}

		loc->spilled = spilled;
	}
}

/**
 * For the given block @p block, decide for each values
 * whether it is used from a register or is reloaded
 * before the use.
 */
static void process_block(ir_node *block)
{
	/* no need to process a block twice */
	assert(get_block_info(block) == NULL);

	/* construct start workset */
	int arity = get_Block_n_cfgpreds(block);
	if (arity == 0) {
		/* no predecessor -> empty set */
		workset_clear(ws);
	} else if (arity == 1) {
		/* one predecessor, copy its end workset */
		ir_node      *pred_block = get_Block_cfgpred_block(block, 0);
		block_info_t *pred_info  = get_block_info(pred_block);

		assert(pred_info != NULL);
		workset_copy(ws, pred_info->end_workset);
	} else {
		/* multiple predecessors, do more advanced magic :) */
		decide_start_workset(block);
	}

	DB((dbg, DBG_DECIDE, "\n"));
	DB((dbg, DBG_DECIDE, "Decide for %+F\n", block));

	DB((dbg, DBG_WSETS, "Start workset for %+F:\n", block));
#ifdef DEBUG_libfirm
	ir_node *irn = NULL;
	workset_foreach(ws, irn, iter) {
		DB((dbg, DBG_WSETS, "  %+F (%u)\n", irn, workset_get_time(ws, iter)));
	}
#endif

	block_info_t *block_info = new_block_info(block);
	block_info->start_workset = workset_clone(ws);

	/* process the block from start to end */
	DB((dbg, DBG_WSETS, "Processing...\n"));
	workset_t *new_vals = temp_workset;

	sched_foreach_non_phi(block, irn) {
		/* Phis are no real instr (see insert_starters()) */

		assert(workset_used_length(ws) <= n_regs);
		DB((dbg, DBG_DECIDE, "  ...%+F\n", irn));

		/* allocate all values _used_ by this instruction */
		workset_clear(new_vals);
		be_foreach_use(irn, cls, in_req_, in, in_req,
			/* (note that "spilled" is irrelevant here) */
			workset_insert(new_vals, in, false);
		);
		be_add_pressure_t const add_pressure = arch_get_additional_pressure(irn, cls);
		displace(new_vals, true, irn, MAX(add_pressure, 0));

		/* allocate all values _defined_ by this instruction */
		workset_clear(new_vals);
		be_foreach_definition(irn, cls, value, req,
			workset_insert(new_vals, value, false);
		);
		displace(new_vals, false, irn, MAX(-add_pressure, 0));
	}

	/* Remember end-workset for this block */
	block_info->end_workset = workset_clone(ws);
	DB((dbg, DBG_WSETS, "End workset for %+F:\n", block));
#ifdef DEBUG_libfirm
	workset_foreach(ws, irn, iter) {
		DB((dbg, DBG_WSETS, "  %+F (%u)\n", irn, workset_get_time(ws, iter)));
	}
#endif
}

/**
 * 'decide' is block-local and makes assumptions
 * about the set of live-ins. Thus we must adapt the
 * live-outs to the live-ins at each block-border.
 */
static void fix_block_borders(ir_node *block, void *data)
{
	(void) data;
	DB((dbg, DBG_FIX, "\n"));
	DB((dbg, DBG_FIX, "Fixing %+F\n", block));

	/* process all pred blocks */
	workset_t *start_workset = get_block_info(block)->start_workset;
	for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(block); i < n_cfgpreds;
	     ++i) {
		ir_node   *pred = get_Block_cfgpred_block(block, i);
		workset_t *pred_end_workset = get_block_info(pred)->end_workset;

		DB((dbg, DBG_FIX, "  Pred %+F\n", pred));

		/* spill all values not used anymore */
		ir_node *node = NULL;
		workset_foreach(pred_end_workset, node, iter) {
			/* Note that we do not look at phi inputs, because the values will
			 * be either live-end and need no spill or they have other users in
			 * which must be somewhere else in the workset. */
			if (workset_contains(start_workset, node))
				continue;

			if (move_spills && be_is_live_in(lv, block, node)
			    && !pred_end_workset->vals[iter].spilled) {
				ir_node *insert_point;
				if (n_cfgpreds > 1) {
					insert_point = be_get_end_of_block_insertion_point(pred);
					insert_point = sched_prev(insert_point);
				} else {
					insert_point = block;
				}
				DB((dbg, DBG_SPILL, "Spill %+F after %+F\n", node,
				     insert_point));
				be_add_spill(senv, node, insert_point);
			}
		}

		/* reload missing values in predecessors, add missing spills */
		workset_foreach(start_workset, node, iter) {
			const loc_t *l = &start_workset->vals[iter];

			/* if node is a phi of the current block we reload
			 * the corresponding argument, else node itself */
			if (is_Phi(node) && get_nodes_block(node) == block) {
				node = get_irn_n(node, i);
				assert(!l->spilled);

				/* we might have unknowns as argument for the phi */
				if (!arch_irn_consider_in_reg_alloc(cls, node))
					continue;
			}

			/* check if node is in a register at end of pred */
			const loc_t *pred_loc = workset_contains(pred_end_workset, node);
			if (pred_loc != NULL) {
				/* we might have to spill value on this path */
				if (move_spills && !pred_loc->spilled && l->spilled) {
					ir_node *insert_point
						= be_get_end_of_block_insertion_point(pred);
					insert_point = sched_prev(insert_point);
					DB((dbg, DBG_SPILL, "Spill %+F after %+F\n", node,
					    insert_point));
					be_add_spill(senv, node, insert_point);
				}
			} else {
				/* node is not in register at the end of pred -> reload it */
				DB((dbg, DBG_FIX, "    reload %+F\n", node));
				DB((dbg, DBG_SPILL, "Reload %+F before %+F,%d\n", node, block, i));
				be_add_reload_on_edge(senv, node, block, i);
			}
		}
	}
}

static void be_spill_belady(ir_graph *irg, const arch_register_class_t *rcls,
                            const regalloc_if_t *regif)
{
	be_assure_live_sets(irg);

	stat_ev_tim_push();
	assure_loopinfo(irg);
	stat_ev_tim_pop("belady_time_backedges");

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	stat_ev_tim_push();
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	stat_ev_tim_pop("belady_time_clear_links");

	/* init belady env */
	stat_ev_tim_push();
	obstack_init(&obst);
	cls          = rcls;
	lv           = be_get_irg_liveness(irg);
	n_regs       = be_get_n_allocatable_regs(irg, cls);
	ws           = new_workset();
	uses         = be_begin_uses(irg, lv);
	loop_ana     = be_new_loop_pressure(irg, cls);
	senv         = be_new_spill_env(irg, regif);
	blocklist    = be_get_cfgpostorder(irg);
	temp_workset = new_workset();
	stat_ev_tim_pop("belady_time_init");

	stat_ev_tim_push();
	/* walk blocks in reverse postorder */
	for (size_t i = ARR_LEN(blocklist); i-- > 0; ) {
		process_block(blocklist[i]);
	}
	DEL_ARR_F(blocklist);
	stat_ev_tim_pop("belady_time_belady");

	stat_ev_tim_push();
	/* belady was block-local, fix the global flow by adding reloads on the
	 * edges */
	irg_block_walk_graph(irg, fix_block_borders, NULL, NULL);
	stat_ev_tim_pop("belady_time_fix_borders");

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* Insert spill/reload nodes into the graph and fix usages */
	be_insert_spills_reloads(senv);

	/* clean up */
	be_delete_spill_env(senv);
	be_end_uses(uses);
	be_free_loop_pressure(loop_ana);
	obstack_free(&obst, NULL);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillbelady)
void be_init_spillbelady(void)
{
	lc_opt_entry_t *be_grp       = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *belady_group = lc_opt_get_grp(be_grp, "belady");
	lc_opt_add_table(belady_group, options);

	be_register_spiller("belady", be_spill_belady);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.belady");
}
