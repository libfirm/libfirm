/**
 * Author:      Daniel Grund
 * Date:		20.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * NOTE: Comments my be (partially) wrong, since there was a major bug
 *       (spilling of phis, prespill) whose fixing changed a lot.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "obst.h"
#include "set.h"
#include "pset.h"
#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "ircons_t.h"

#include "beutil.h"
#include "bearch.h"
#include "bespillbelady.h"
#include "beuses_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bechordal_t.h"

#define DBG_SPILL   1
#define DBG_WSETS   2
#define DBG_FIX     4
#define DBG_DECIDE  8
#define DBG_START  16
#define DBG_SLOTS  32
#define DBG_TRACE  64
#define DEBUG_LVL 0 //(DBG_START | DBG_DECIDE | DBG_WSETS | DBG_FIX | DBG_SPILL)
static firm_dbg_module_t *dbg = NULL;

#define MIN(a,b) (((a)<(b))?(a):(b))
#undef SINGLE_START_PROJS

typedef struct _workset_t workset_t;

typedef struct _belady_env_t {
	struct obstack ob;
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	int n_regs;			/** number of regs in this reg-class */

	workset_t *ws;		/**< the main workset used while processing a block. ob-allocated */
	be_uses_t *uses;	/**< env for the next-use magic */
	ir_node *instr;		/**< current instruction */
	unsigned instr_nr;	/**< current instruction number (relative to block start) */
	pset *used;			/**< holds the values used (so far) in the current BB */

	spill_env_t *senv;	/* see bespill.h */
	pset *reloads;		/**< all reload nodes placed */
} belady_env_t;

struct _workset_t {
	belady_env_t *bel;
	int i;				/**< used for iteration TODO remove this form the struct */
	int len;			/**< current length */
	loc_t vals[1];		/**< inlined array of the values/distances in this working set */
};

typedef struct _block_info_t {
	workset_t *ws_start, *ws_end;
} block_info_t;

/**
 * Alloc a new workset on obstack @p ob with maximum size @p max
 */
static INLINE workset_t *new_workset(struct obstack *ob, belady_env_t *bel) {
	workset_t *res;
	size_t size = sizeof(*res) + (bel->n_regs-1)*sizeof(res->vals[0]);
	res = obstack_alloc(ob, size);
	memset(res, 0, size);
	res->bel = bel;
	return res;
}

/**
 * Alloc a new instance on obstack and make it equal to @param ws
 */
static INLINE workset_t *workset_clone(struct obstack *ob, workset_t *ws) {
	workset_t *res;
	size_t size = sizeof(*res) + (ws->bel->n_regs-1)*sizeof(res->vals[0]);
	res = obstack_alloc(ob, size);
	memcpy(res, ws, size);
	return res;
}

/**
 * Do NOT alloc anything. Make @param tgt equal to @param src.
 * returns @param tgt for convinience
 */
static INLINE workset_t *workset_copy(workset_t *tgt, workset_t *src) {
	size_t size = sizeof(*src) + (src->bel->n_regs-1)*sizeof(src->vals[0]);
	memcpy(tgt, src, size);
	return tgt;
}

/**
 * Overwrites the current content array of @param ws with the
 * @param count locations given at memory @param locs.
 * Set the length of @param ws to count.
 */
#define workset_bulk_fill(ws, count, locs) memcpy(&(ws)->vals[0], locs, ((ws)->len=count)*sizeof(locs[0]));


/**
 * Inserts the value @p val into the workset, iff it is not
 * already contained. The workset must not be full.
 */
static INLINE void workset_insert(workset_t *ws, ir_node *val) {
	int i;
	/* check for current regclass */
	if (arch_get_irn_reg_class(ws->bel->arch, val, -1) != ws->bel->cls) {
		DBG((dbg, DBG_DECIDE, "Dropped %+F\n", val));
		return;
	}

	/* check if val is already contained */
	for(i=0; i<ws->len; ++i)
		if (ws->vals[i].irn == val)
			return;

	/* insert val */
	assert(ws->len < ws->bel->n_regs && "Workset already full!");
	ws->vals[ws->len++].irn = val;
}

/**
 * Inserts all values in array @p vals of length @p cnt
 * into the workset. There must be enough space for the
 * entries.
 */
static INLINE void workset_bulk_insert(workset_t *ws, int cnt, ir_node **vals) {
	int i, o;

	for(o=0; o<cnt; ++o) {
		ir_node *val = vals[o];
		DBG((dbg, DBG_TRACE, "Bulk insert %+F\n", val));
		/* check for current regclass */
		if (arch_get_irn_reg_class(ws->bel->arch, val, -1) != ws->bel->cls) {
			DBG((dbg, DBG_TRACE, "Wrong reg class\n"));
			goto no_insert;
		}

		/* check if val is already contained */
		for(i=0; i<ws->len; ++i)
			if (ws->vals[i].irn == val) {
				DBG((dbg, DBG_TRACE, "Already contained\n"));
				goto no_insert;
			}

		/* insert val */
		assert(ws->len < ws->bel->n_regs && "Workset does not have enough room!");
		ws->vals[ws->len++].irn = val;
		DBG((dbg, DBG_TRACE, "Inserted\n"));

no_insert:
		/*epsilon statement :)*/;
	}
}

/**
 * Removes all entries from this workset
 */
#define workset_clear(ws) (ws)->len = 0;

/**
 * Removes the value @p val from the workset if present.
 */
static INLINE void workset_remove(workset_t *ws, ir_node *val) {
	int i;
	for(i=0; i<ws->len; ++i)
		if (ws->vals[i].irn == val) {
			ws->vals[i] = ws->vals[--ws->len];
			return;
		}
}

static INLINE int workset_contains(const workset_t *ws, const ir_node *val) {
	int i;
	for(i=0; i<ws->len; ++i)
		if (ws->vals[i].irn == val)
			return 1;
	return 0;
}

#define workset_foreach(ws, v)	for(ws->i=0; \
									v=(ws->i < ws->len) ? ws->vals[ws->i].irn : NULL, ws->i < ws->len; \
									ws->i++)

#define workset_set_time(ws, i, t) (ws)->vals[i].time=t
#define workset_set_length(ws, length) (ws)->len = length
#define workset_get_length(ws) ((ws)->len)
#define workset_get_val(ws, i) ((ws)->vals[i].irn)
#define workset_sort(ws) qsort((ws)->vals, (ws)->len, sizeof((ws)->vals[0]), loc_compare);


static int is_mem_phi(const ir_node *irn, void *data) {
	workset_t *sws;
	ir_node *blk = get_nodes_block(irn);

	DBG((dbg, DBG_SPILL, "Is %+F a mem-phi?\n", irn));
	sws = ((block_info_t *) get_irn_link(blk))->ws_start;
	DBG((dbg, DBG_SPILL, "  %d\n", !workset_contains(sws, irn)));
	return !workset_contains(sws, irn);
}

/**
 * Collects all values live-in at block @p blk and all phi results in this block.
 * Then it adds the best values (at most n_regs) to the blocks start_workset.
 * The phis among the remaining values get spilled: Introduce psudo-copies of
 *  their args to break interference and make it possible to spill them to the
 *  same spill slot.
 */
static void compute_block_start_info(ir_node *blk, void *env) {
	belady_env_t *bel = env;
	block_info_t *blk_info;
	ir_node *irn, *first;
	irn_live_t *li;
	int i, count, ws_count;
	loc_t loc, *starters;
	ir_graph *irg = get_irn_irg(blk);
	struct obstack ob;

	obstack_init(&ob);

	/* Get all values living at the block start */
	DBG((dbg, DBG_START, "Living at start of %+F:\n", blk));
	first = sched_first(blk);
	count = 0;
	sched_foreach(blk, irn)
		if (is_Phi(irn) && arch_get_irn_reg_class(bel->arch, irn, -1) == bel->cls) {
			loc.irn = irn;
			loc.time = be_get_next_use(bel->uses, first, 0, irn, 0);
			obstack_grow(&ob, &loc, sizeof(loc));
			DBG((dbg, DBG_START, "    %+F:\n", irn));
			count++;
		} else
			break;

	live_foreach(blk, li)
		if (live_is_in(li) && arch_get_irn_reg_class(bel->arch, li->irn, -1) == bel->cls) {
			loc.irn = (ir_node *)li->irn;
			loc.time = be_get_next_use(bel->uses, first, 0, li->irn, 0);
			obstack_grow(&ob, &loc, sizeof(loc));
			DBG((dbg, DBG_START, "    %+F:\n", irn));
			count++;
		}
	starters = obstack_finish(&ob);

	/* Sort all values */
	qsort(starters, count, sizeof(starters[0]), loc_compare);

	/* Create the start workset for this block. Copy the best ones from starters */
	blk_info = obstack_alloc(&bel->ob, sizeof(*blk_info));
	set_irn_link(blk, blk_info);

	ws_count = MIN(count, bel->n_regs);
	blk_info->ws_start = new_workset(&bel->ob, bel);
	workset_bulk_fill(blk_info->ws_start, ws_count, starters);

	/* Spill the phis among the remaining values */
	for (i=ws_count; i<count; ++i) {
		int o, max;

		irn = starters[i].irn;
		if (!is_Phi(irn) || get_nodes_block(irn) != blk)
			continue;

		DBG((dbg, DBG_START, "For %+F:\n", irn));

		for (max=get_irn_arity(irn), o=0; o<max; ++o) {
			ir_node *arg = get_irn_n(irn, o);
			ir_node *pred_block = get_Block_cfgpred_block(get_nodes_block(irn), o);
			ir_node *cpy = be_new_Copy(bel->cls, irg, pred_block, arg);
			DBG((dbg, DBG_START, "    place a %+F of %+F in %+F\n", cpy, arg, pred_block));
			sched_add_before(pred_block, cpy);
			set_irn_n(irn, o, cpy);
		}
	}

	obstack_free(&ob, NULL);
}

/**
 * Performs the actions neccessary to grant the request that:
 * - new_vals can be held in registers
 * - as few as possible other values are disposed
 * - the worst values get disposed
 *
 * @p is_usage indicates that the values in new_vals are used (not defined)
 * In this case reloads must be performed
 */
static void displace(belady_env_t *bel, workset_t *new_vals, int is_usage) {
	ir_node *val;
	int i, len, max_allowed, demand;
	workset_t *ws = bel->ws;
	ir_node **to_insert = alloca(bel->n_regs * sizeof(*to_insert));

	/*
	 * 1. Identify the number of needed slots and the values to reload
	 */
	demand = 0;
	workset_foreach(new_vals, val) {
		/* mark value as used */
		if (is_usage)
			pset_insert_ptr(bel->used, val);

		if (!workset_contains(ws, val)) {
			DBG((dbg, DBG_DECIDE, "    insert %+F\n", val));
			to_insert[demand++] = val;
			if (is_usage)
				be_add_reload(bel->senv, val, bel->instr);
		} else
			DBG((dbg, DBG_DECIDE, "    skip %+F\n", val));
	}
	DBG((dbg, DBG_DECIDE, "    demand = %d\n", demand));


	/*
	 * 2. Make room for at least 'demand' slots
	 */
	len = workset_get_length(ws);
	max_allowed = bel->n_regs - demand;

	/* Only make more free room if we do not have enough */
	if (len > max_allowed) {
		/* get current next-use distance */
		for (i=0; i<ws->len; ++i)
			workset_set_time(ws, i, be_get_next_use(bel->uses, bel->instr, bel->instr_nr, workset_get_val(ws, i), !is_usage));

		/* sort entries by increasing nextuse-distance*/
		workset_sort(ws);

		/* Logic for not needed live-ins: If a value is disposed
		   before its first usage, remove it from start workset */
		for (i=max_allowed; i<ws->len; ++i) {
			ir_node *irn = ws->vals[i].irn;
			if (!pset_find_ptr(bel->used, irn)) {
				ir_node *curr_bb = get_nodes_block(bel->instr);
				workset_t *ws_start = ((block_info_t *) get_irn_link(curr_bb))->ws_start;
				workset_remove(ws_start, irn);

				DBG((dbg, DBG_DECIDE, "    dispose %+F dumb\n", irn));
			} else
				DBG((dbg, DBG_DECIDE, "    dispose %+F\n", irn));
		}

		/* kill the last 'demand' entries in the array */
		workset_set_length(ws, max_allowed);
	}

	/*
	 * 3. Insert the new values into the workset
	 */
	workset_bulk_insert(bel->ws, demand, to_insert);
}

/**
 * For the given block @p blk, decide for each values
 * whether it is used from a register or is reloaded
 * before the use.
 */
static void belady(ir_node *blk, void *env) {
	belady_env_t *bel = env;
	workset_t *new_vals;
	ir_node *irn;
#ifdef SINGLE_START_PROJS
	ir_node *start_blk = get_irg_start_block(get_irn_irg(blk));
#endif
	block_info_t *blk_info = get_irn_link(blk);

	DBG((dbg, DBG_DECIDE, "\n"));
	DBG((dbg, DBG_DECIDE, "Decide for %+F\n", blk));

	workset_copy(bel->ws, blk_info->ws_start);
	DBG((dbg, DBG_WSETS, "Initial start workset for %+F:\n", blk));
	workset_foreach(bel->ws, irn)
		DBG((dbg, DBG_WSETS, "  %+F\n", irn));

	/* process the block from start to end */
	DBG((dbg, DBG_WSETS, "Processing...\n"));
	bel->used = pset_new_ptr(32);
	bel->instr_nr = 0;
	new_vals = new_workset(&bel->ob, bel);
	sched_foreach(blk, irn) {
		assert(workset_get_length(bel->ws) <= bel->n_regs && "Too much values in workset!");


#ifdef SINGLE_START_PROJS
		if (is_Phi(irn) ||
			(is_Proj(irn) && blk!=start_blk) ||
			(get_irn_mode(irn) == mode_T && blk==start_blk)) {
			DBG((dbg, DBG_DECIDE, "  ...%+F skipped\n", irn));
			continue;
		}
#else
		/* projs are handled with the tuple value.
		 * Phis are no real instr (see insert_starters)
		 * instr_nr does not increase */
		if (is_Proj(irn) || is_Phi(irn)) {
			DBG((dbg, DBG_DECIDE, "  ...%+F skipped\n", irn));
			continue;
		}
#endif
		DBG((dbg, DBG_DECIDE, "  ...%+F\n", irn));

		/* set instruction in the workset */
		bel->instr = irn;

		/* allocate all values _used_ by this instruction */
		workset_clear(new_vals);
		workset_bulk_insert(new_vals, get_irn_arity(irn)+1, get_irn_in(irn));
		displace(bel, new_vals, 1);

		/* allocate all values _defined_ by this instruction */
		workset_clear(new_vals);
		if (get_irn_mode(irn) == mode_T) { /* special handling for tuples and projs */
			ir_node *proj;
			for(proj=sched_next(irn); is_Proj(proj); proj=sched_next(proj))
				workset_insert(new_vals, proj);
		} else {
			workset_insert(new_vals, irn);
		}
		displace(bel, new_vals, 0);

		bel->instr_nr++;
	}
	del_pset(bel->used);

	/* Remember end-workset for this block */
	blk_info->ws_end = workset_clone(&bel->ob, bel->ws);
	DBG((dbg, DBG_WSETS, "Start workset for %+F:\n", blk));
	workset_foreach(blk_info->ws_start, irn)
		DBG((dbg, DBG_WSETS, "  %+F\n", irn));
	DBG((dbg, DBG_WSETS, "End workset for %+F:\n", blk));
	workset_foreach(blk_info->ws_end, irn)
		DBG((dbg, DBG_WSETS, "  %+F\n", irn));
}

/**
 * 'decide' is block-local and makes assumptions
 * about the set of live-ins. Thus we must adapt the
 * live-outs to the live-ins at each block-border.
 */
static void fix_block_borders(ir_node *blk, void *env) {
	workset_t *wsb;
	belady_env_t *bel = env;
	int i, max;

	DBG((dbg, DBG_FIX, "\n"));
	DBG((dbg, DBG_FIX, "Fixing %+F\n", blk));

	wsb = ((block_info_t *)get_irn_link(blk))->ws_start;

	/* process all pred blocks */
	for (i=0, max=get_irn_arity(blk); i<max; ++i) {
		ir_node *irnb, *irnp, *pred = get_Block_cfgpred_block(blk, i);
		workset_t *wsp = ((block_info_t *)get_irn_link(pred))->ws_end;

		DBG((dbg, DBG_FIX, "  Pred %+F\n", pred));

		workset_foreach(wsb, irnb) {
			/* if irnb is a phi of the current block we reload
			 * the corresponding argument, else irnb itself */
			if(is_Phi(irnb) && blk == get_nodes_block(irnb))
				irnb = get_irn_n(irnb, i);

			/* Unknowns are available everywhere */
			if(get_irn_opcode(irnb) == iro_Unknown)
				continue;

			/* check if irnb is in a register at end of pred */
			workset_foreach(wsp, irnp)
				if (irnb == irnp)
					goto next_value;

			/* irnb is in memory at the end of pred, so we have to reload it */
			DBG((dbg, DBG_FIX, "    reload %+F\n", irnb));
			be_add_reload_on_edge(bel->senv, irnb, blk, i);

next_value:
			/*epsilon statement :)*/;
		}
	}
}

/**
 * Removes all used reloads from bel->reloads.
 * The remaining nodes in bel->reloads will be removed from the graph.
 */
static void rescue_used_reloads_and_remove_copies(ir_node *irn, void *env) {
	pset *rlds = (pset *)env;
	if (pset_find_ptr(rlds, irn))
		pset_remove_ptr(rlds, irn);


	/* remove copies introduced for phi-spills */
	if (be_is_Copy(irn)) {
		ir_node *src, *spill;
		assert(get_irn_n_edges(irn) == 1 && "This is not a copy introduced in 'compute_block_start_info()'. Who created it?");

		spill = get_irn_edge(get_irn_irg(irn), irn, 0)->src;
		assert(be_is_Spill(spill) && "This is not a copy introduced in 'compute_block_start_info()'. Who created it?");

		src = get_irn_n(irn, 0);
		set_irn_n(spill, 0, src);
	}
}

/**
 * Finds all unused reloads and remove them from the schedule
 * Also removes spills if they are not used anymore after removing reloads
 */
static void remove_copies_and_unused_reloads(ir_graph *irg, belady_env_t *bel) {
	ir_node *irn;

	irg_walk_graph(irg, rescue_used_reloads_and_remove_copies, NULL, bel->reloads);
	for(irn = pset_first(bel->reloads); irn; irn = pset_next(bel->reloads)) {
		ir_node *spill;
		DBG((dbg, DBG_SPILL, "Removing %+F before %+F in %+F\n", irn, sched_next(irn), get_nodes_block(irn)));

		spill = get_irn_n(irn, 0);

		/* remove reload */
		set_irn_n(irn, 0, new_Bad());
		sched_remove(irn);

		/* if spill not used anymore, remove it too
		 * test of regclass is necessary since spill may be a phi-M */
		if (get_irn_n_edges(spill) == 0 && bel->cls == arch_get_irn_reg_class(bel->arch, spill, -1)) {
			set_irn_n(spill, 0, new_Bad());
			sched_remove(spill);
		}
	}
}

void be_spill_belady(const be_chordal_env_t *chordal_env) {
	belady_env_t bel;

	dbg = firm_dbg_register("ir.be.spillbelady");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	/* init belady env */
	obstack_init(&bel.ob);
	bel.arch    = chordal_env->main_env->arch_env;
	bel.cls     = chordal_env->cls;
	bel.n_regs  = arch_register_class_n_regs(bel.cls);
	bel.ws      = new_workset(&bel.ob, &bel);
	bel.uses    = be_begin_uses(chordal_env->irg, chordal_env->main_env->arch_env, bel.cls);
	bel.senv    = be_new_spill_env(dbg, chordal_env, is_mem_phi, NULL);
	bel.reloads = pset_new_ptr_default();

	/* do the work */
	irg_block_walk_graph(chordal_env->irg, compute_block_start_info, NULL, &bel);
	irg_block_walk_graph(chordal_env->irg, belady, NULL, &bel);
	irg_block_walk_graph(chordal_env->irg, fix_block_borders, NULL, &bel);
	be_insert_spills_reloads(bel.senv, bel.reloads);
	remove_copies_and_unused_reloads(chordal_env->irg, &bel);


	/* clean up */
	del_pset(bel.reloads);
	be_delete_spill_env(bel.senv);
	be_end_uses(bel.uses);
	obstack_free(&bel.ob, NULL);
}
