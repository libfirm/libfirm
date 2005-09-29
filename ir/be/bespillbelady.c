/**
 * Author:      Daniel Grund
 * Date:		20.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <alloca.h>
#include "obst.h"
#include "set.h"
#include "pset.h"
#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "ircons.h"
#include "irgwalk.h"
#include "iredges_t.h"

#include "beutil.h"
#include "bearch.h"
#include "bespillbelady.h"
#include "beuses_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "belive_t.h"
#include "benode_t.h"

#define MIN(a,b) (((a)<(b))?(a):(b))

#define DBG_DECIDE 1
#define DBG_WSETS 2
#define DBG_FIX 4
#define DBG_SPILL 8
#define DBG_TRACE 32
#define DEBUG_LVL SET_LEVEL_0 //(DBG_DECIDE | DBG_WSETS | DBG_FIX | DBG_SPILL)
static firm_dbg_module_t *dbg = NULL;

typedef struct _workset_t workset_t;
typedef struct _block_info_t block_info_t;
typedef struct _reloader_t reloader_t;
typedef struct _spill_info_t spill_info_t;
typedef struct _belady_env_t belady_env_t;

struct _workset_t {
	belady_env_t *bel;
	int i;				/**< used for iteration */
	int len;			/**< current length */
	loc_t vals[1];		/**< inlined array of the values/distances in this working set */
};

struct _block_info_t {
	workset_t *ws_start, *ws_end;
};

struct _reloader_t {
	reloader_t *next;
	ir_node *reloader;
};

struct _spill_info_t {
	ir_node *spilled_node;
	reloader_t *reloaders;
};

struct _belady_env_t {
	struct obstack ob;
	const be_node_factory_t *factory;
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	int n_regs;			/** number of regs in this reg-class */

	workset_t *ws;		/**< the main workset used while processing a block. ob-allocated */
	be_uses_t *uses;	/**< env for the next-use magic */
	ir_node *instr;		/**< current instruction */
	unsigned instr_nr;	/**< current instruction number (relative to block start) */
	pset *used;			/**< holds the values used (so far) in the current BB */
	set *spills;		/**< all spill_info_t's, which must be placed */

	spill_env_t senv;	/* see bespill.h */
	pset *reloads;		/**< all reload nodes placed */
};

static int set_cmp_spillinfo(const void *x, const void *y, size_t size) {
	const spill_info_t *xx = x;
	const spill_info_t *yy = y;
	return ! (xx->spilled_node == yy->spilled_node);
}


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

static INLINE workset_t *workset_clone(struct obstack *ob, workset_t *ws) {
	workset_t *res;
	size_t size = sizeof(*res) + (ws->bel->n_regs-1)*sizeof(res->vals[0]);
	res = obstack_alloc(ob, size);
	memcpy(res, ws, size);
	return res;
}

/**
 * Inserts the value @p val into the workset, iff it is not
 * already contained. The workset must not be full.
 */
static INLINE void workset_insert(workset_t *ws, ir_node *val) {
	int i;
	assert(ws->len < ws->bel->n_regs && "Workset already full!");
	/* check for current regclass */
	if (arch_get_irn_reg_class(ws->bel->arch, val, 0) != ws->bel->cls) {
		DBG((dbg, 0, "Dropped %+F\n", val));
		return;
	}

	/* check if val is already contained */
	for(i=0; i<ws->len; ++i)
		if (ws->vals[i].irn == val)
			return;

	/* insert val */
	ws->vals[ws->len++].irn = val;
}

/**
 * Inserts all values in array @p vals of length @p cnt
 * into the workset. There must be enough space for the
 * entries.
 */
static INLINE void workset_bulk_insert(workset_t *ws, int cnt, ir_node **vals) {
	int i, o;
	assert(ws->len + cnt <= ws->bel->n_regs && "Workset does not have enough room!");

	for(o=0; o<cnt; ++o) {
		ir_node *val = vals[o];
		DBG((dbg, DBG_TRACE, "Bulk insert %+F\n", val));
		/* check for current regclass */
		if (arch_get_irn_reg_class(ws->bel->arch, val, 0) != ws->bel->cls) {
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
		ws->vals[ws->len++].irn = val;
		DBG((dbg, DBG_TRACE, "Inserted\n"));

no_insert:
		/*epsilon statement :)*/;
	}
}

/**
 * Overwrites the current contents of @p ws with the
 * locations given in @p locs
 */
#define workset_bulk_fill(ws, count, locs) memcpy(&(ws)->vals[0], locs, ((ws)->len=count)*sizeof(locs[0]));

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

static INLINE int workset_contains(const workset_t *ws, ir_node *val) {
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


/**
 * Collects all values live-in at block @p blk and all phi results in this block.
 * Then it adds the best values (at most n_regs) to the ws.
 */
static void build_start_set(belady_env_t *bel, ir_node *blk) {
	workset_t *ws = bel->ws;
	ir_node *irn, *first;
	irn_live_t *li;
	int count;
	loc_t loc, *starters;
	struct obstack ob;

	obstack_init(&ob);

	/* get all values */
	first = sched_first(blk);
	count = 0;
	sched_foreach(blk, irn)
		if (is_Phi(irn) && arch_get_irn_reg_class(bel->arch, irn, 0) == bel->cls) {
			loc.irn = irn;
			loc.time = be_get_next_use(bel->uses, first, 0, irn, 1);
			obstack_grow(&ob, &loc, sizeof(loc));
			count++;
		} else
			break;

	live_foreach(blk, li)
		if (live_is_in(li) && arch_get_irn_reg_class(bel->arch, li->irn, 0) == bel->cls) {
			loc.irn = (ir_node *)li->irn;
			loc.time = be_get_next_use(bel->uses, first, 0, li->irn, 1);
			obstack_grow(&ob, &loc, sizeof(loc));
			count++;
		}
	starters = obstack_finish(&ob);

	/* sort all values */
	qsort(starters, count, sizeof(starters[0]), loc_compare);

	/* copy the best ones to the ws */
	count = MIN(count, ws->bel->n_regs);
	workset_bulk_fill(ws, count, starters);

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

			if (is_usage) {
				spill_info_t si, *found;
				reloader_t *rld;

				/* find the spill info or create it */
				si.spilled_node = val;
				si.reloaders = NULL;
				found = set_insert(bel->spills, &si, sizeof(si), HASH_PTR(si.spilled_node));

				/* insert the reloader into the linked list */
				rld = obstack_alloc(&bel->ob, sizeof(*rld));
				rld->reloader = bel->instr;
				rld->next = found->reloaders;
				found->reloaders = rld;
			}
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
			workset_set_time(ws, i, be_get_next_use(bel->uses, bel->instr, bel->instr_nr, workset_get_val(ws, i), is_usage));

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
static void decide(ir_node *blk, void *env) {
	belady_env_t *bel = env;
	workset_t *new_vals;
	ir_node *irn;
	block_info_t *blk_info = obstack_alloc(&bel->ob, sizeof(*blk_info));
	set_irn_link(blk, blk_info);

	DBG((dbg, DBG_DECIDE, "\n"));
	DBG((dbg, DBG_DECIDE, "Decide for %+F\n", blk));

	/* build starting-workset for this block */
	build_start_set(bel, blk);
	blk_info->ws_start = workset_clone(&bel->ob, bel->ws);
	DBG((dbg, DBG_WSETS, "Initial start workset for %+F:\n", blk));
	workset_foreach(blk_info->ws_start, irn)
		DBG((dbg, DBG_WSETS, "  %+F\n", irn));

	/* process the block from start to end */
	DBG((dbg, DBG_WSETS, "Processing...\n"));
	bel->used = pset_new_ptr(32);
	bel->instr_nr = 0;
	new_vals = new_workset(&bel->ob, bel);
	sched_foreach(blk, irn) {
		ir_node *iii;
		DBG((dbg, DBG_WSETS, "Current workset for %+F:\n", blk));
		workset_foreach(bel->ws, iii)
			DBG((dbg, DBG_WSETS, "  %+F\n", iii));
		assert(workset_get_length(bel->ws) <= bel->n_regs && "Too much values in workset!");

		DBG((dbg, DBG_DECIDE, "  ...%+F\n", irn));

		/* projs are handled with the tuple value.
		 * Phis are no real instr (see insert_starters)
		 * instr_nr does not increase */
		if (is_Proj(irn) || is_Phi(irn))
			continue;

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
 * 'decide' is block-local and makes assumtions
 * about the set of live-ins. Thus we must adapt the
 * live-outs to the live-ins at each block-border.
 */
static void fix_block_borders(ir_node *blk, void *env) {
	belady_env_t *bel = env;
	int i, max;

	DBG((dbg, DBG_FIX, "\n"));
	DBG((dbg, DBG_FIX, "Fixing %+F\n", blk));

	workset_t *wsb = ((block_info_t *)get_irn_link(blk))->ws_start;

	/* process all pred blocks */
	for (i=0, max=get_irn_arity(blk); i<max; ++i) {
		ir_node *irnb, *irnp, *pred = get_Block_cfgpred_block(blk, i);
		workset_t *wsp = ((block_info_t *)get_irn_link(pred))->ws_end;

		DBG((dbg, DBG_FIX, "  Pred %+F\n", pred));

		workset_foreach(wsb, irnb) {
			spill_info_t si, *found;
			reloader_t *rld;

			/* if irnb is a phi of the current block we reload
			 * the corresponding argument, else irnb itself */
			if(is_Phi(irnb) && blk == get_nodes_block(irnb))
				irnb = get_irn_n(irnb, i);

			/* check if irnb is in a register at end of pred */
			workset_foreach(wsp, irnp)
				if (irnb == irnp)
					goto next_value;

			/* irnb is in memory at the end of pred, so we have to reload it */

			/* find the spill info or create it */
			si.spilled_node = irnb;
			si.reloaders = NULL;
			found = set_insert(bel->spills, &si, sizeof(si), HASH_PTR(si.spilled_node));

			/* insert the reloader into the linked list.
			 * the schedule position depends on the cf-situation of the block */
			rld = obstack_alloc(&bel->ob, sizeof(*rld));
			rld->reloader = (max==1) ? sched_skip(sched_first(blk), 1, sched_skip_phi_predicator, NULL) : pred;
			rld->next = found->reloaders;
			found->reloaders = rld;

			DBG((dbg, DBG_FIX, "    reload %+F before %+F\n", irnb, rld->reloader));

next_value:
			/*epsilon statement :)*/;
		}
	}
}

static void insert_spills_reloads(ir_graph *irg, belady_env_t *bel) {
	ir_node *irn;
	spill_info_t *si;
	struct obstack ob;
	obstack_init(&ob);

	/* get all special spilled phis */
	for(si = set_first(bel->spills); si; si = set_next(bel->spills)) {
		irn = si->spilled_node;
		if (is_Phi(irn)) {
			ir_node *blk = get_nodes_block(irn);
			workset_t *sws = ((block_info_t *)get_irn_link(blk))->ws_start;
			if (!workset_contains(sws, irn))
				pset_insert_ptr(bel->senv.mem_phis, irn);
		}
	}

	/* process each spilled node */
	for(si = set_first(bel->spills); si; si = set_next(bel->spills)) {
		reloader_t *rld;
		ir_node **reloads;
		int n_reloads = 0;
		ir_mode *mode = get_irn_mode(si->spilled_node);

		/* go through all reloads for this spill */
		for(rld = si->reloaders; rld; rld = rld->next) {
			/* the spill for this reloader */
			ir_node *spill   = be_spill_node(&bel->senv, si->spilled_node);

			/* the reload */
			ir_node *bl      = is_Block(rld->reloader) ? rld->reloader : get_nodes_block(rld->reloader);
			ir_node *reload  = new_Reload(bel->factory, bel->cls, irg, bl, mode, spill);
			DBG((dbg, DBG_SPILL, " RELOADER %+F   Reload %+F of %+F\n", rld->reloader, reload, si->spilled_node));
			pset_insert_ptr(bel->reloads, reload);

			/* remember the reaload */
			obstack_ptr_grow(&ob, reload);
			sched_add_before(rld->reloader, reload);
			n_reloads++;
		}

		assert(n_reloads > 0);
		reloads = obstack_finish(&ob);
		be_introduce_copies_ignore(bel->senv.session->dom_front, si->spilled_node, n_reloads, reloads, bel->senv.mem_phis);
		obstack_free(&ob, reloads);
	}

	obstack_free(&ob, NULL);

	be_remove_spilled_phis(&bel->senv);
}

/**
 * Removes all used reloads from bel->reloads.
 * The remaining nodes in bel->reloads will be removed from the graph.
 */
static void rescue_used_reloads(ir_node *irn, void *env) {
	pset *rlds = ((belady_env_t *)env)->reloads;
	if (pset_find_ptr(rlds, irn)) {
		DBG((dbg, DBG_SPILL, "Removing %+F in %+F\n", irn, get_nodes_block(irn)));
		pset_remove_ptr(rlds, irn);
	}
}

void be_spill_belady(const be_main_session_env_t *session, const arch_register_class_t *cls) {
	ir_node *irn;

	dbg = firm_dbg_register("ir.be.spillbelady");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	/* init belady env */
	belady_env_t *bel = alloca(sizeof(*bel));
	obstack_init(&bel->ob);
	bel->senv.session = session;
	bel->factory = session->main_env->node_factory;
	bel->arch = session->main_env->arch_env;
	bel->cls = cls;
	bel->n_regs = arch_register_class_n_regs(cls);
	bel->ws = new_workset(&bel->ob, bel);
	bel->uses = be_begin_uses(session->irg, session->main_env->arch_env, cls);
	bel->spills = new_set(set_cmp_spillinfo, 32);
	bel->senv.spill_ctxs = new_set(be_set_cmp_spillctx, 32);
	bel->senv.mem_phis = pset_new_ptr_default();
	bel->reloads = pset_new_ptr_default();

	/* do the work */
	irg_block_walk_graph(session->irg, decide, NULL, bel);
	irg_block_walk_graph(session->irg, fix_block_borders, NULL, bel);
	insert_spills_reloads(session->irg, bel);

	/* find all unused reloads and remove them from the schedule */
	irg_walk_graph(session->irg, rescue_used_reloads, NULL, bel);
	for(irn = pset_first(bel->reloads); irn; irn = pset_next(bel->reloads))
		sched_remove(irn);

	/* clean up */
	del_pset(bel->reloads);
	del_pset(bel->senv.mem_phis);
	del_set(bel->senv.spill_ctxs);
	del_set(bel->spills);
	be_end_uses(bel->uses);
	obstack_free(&bel->ob, NULL);
}
