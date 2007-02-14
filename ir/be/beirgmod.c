/**
 * This file contains the following IRG modifications for be routines:
 * - backend dominance information
 * - SSA construction for a set of nodes
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 *
 * Author:      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * Date:        04.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#ifdef HAVE_MALLOC_H
 #include <malloc.h>
#endif
#ifdef HAVE_ALLOCA_H
 #include <alloca.h>
#endif

#include "hashptr.h"
#include "pdeq.h"
#include "pset.h"
#include "pmap.h"
#include "util.h"
#include "debug.h"

#include "irflag_t.h"
#include "ircons_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irmode_t.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irgopt.h"
#include "irprintf_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "beutil.h"
#include "beinsn_t.h"

#include "beirgmod.h"

#define DBG_MODULE "firm.be.irgmod"
#define DBG_LEVEL SET_LEVEL_0

/*
  ____                  _
 |  _ \  ___  _ __ ___ (_)_ __   __ _ _ __   ___ ___
 | | | |/ _ \| '_ ` _ \| | '_ \ / _` | '_ \ / __/ _ \
 | |_| | (_) | | | | | | | | | | (_| | | | | (_|  __/
 |____/ \___/|_| |_| |_|_|_| |_|\__,_|_| |_|\___\___|
 |  ___| __ ___  _ __ | |_(_) ___ _ __ ___
 | |_ | '__/ _ \| '_ \| __| |/ _ \ '__/ __|
 |  _|| | | (_) | | | | |_| |  __/ |  \__ \
 |_|  |_|  \___/|_| |_|\__|_|\___|_|  |___/

*/

/**
 * The dominance frontier for a graph.
 */
struct _be_dom_front_info_t {
	pmap *df_map;         /**< A map, mapping every block to a list of its dominance frontier blocks. */
	struct obstack obst;  /**< An obstack holding all the frontier data. */
};

/**
 * A wrapper for get_Block_idom.
 * This function returns the block itself, if the block is the start
 * block. Returning NULL would make any != comparison true which
 * suggests, that the start block is dominated by some other node.
 * @param bl The block.
 * @return The immediate dominator of the block.
 */
static INLINE ir_node *get_idom(ir_node *bl)
{
	ir_node *idom = get_Block_idom(bl);
	return idom == NULL ? bl : idom;
}

/**
 * Compute the dominance frontier for a given block.
 *
 * @param blk   the block where the calculation starts
 *
 * @return the list of all blocks in the dominance frontier of blk
 */
static ir_node **compute_df(ir_node *blk, be_dom_front_info_t *info)
{
	ir_node *c;
	const ir_edge_t *edge;
	ir_node **df_list = NEW_ARR_F(ir_node *, 0);
	ir_node **df;
	int len;

	/* Add local dominance frontiers */
	foreach_block_succ(blk, edge) {
		ir_node *y = edge->src;

		if (get_idom(y) != blk) {
			ARR_APP1(ir_node *, df_list, y);
		}
	}

	/*
	 * Go recursively down the dominance tree and add all blocks
	 * into the dominance frontiers of the children, which are not
	 * dominated by the given block.
	 */
	for (c = get_Block_dominated_first(blk); c; c = get_Block_dominated_next(c)) {
		int i;
		ir_node **df_c_list = compute_df(c, info);

		for (i = ARR_LEN(df_c_list) - 1; i >= 0; --i) {
			ir_node *w = df_c_list[i];
			if (get_idom(w) != blk)
				ARR_APP1(ir_node *, df_list, w);
		}
	}

	/* now copy the flexible array to the obstack */
	len = ARR_LEN(df_list);
	df = NEW_ARR_D(ir_node *, &info->obst, len);
	memcpy(df, df_list, len * sizeof(df[0]));
	DEL_ARR_F(df_list);

	pmap_insert(info->df_map, blk, df);
	return df;
}

be_dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg)
{
	be_dom_front_info_t *info = xmalloc(sizeof(*info));

	edges_assure(irg);
	obstack_init(&info->obst);
	info->df_map = pmap_create();
	assure_doms(irg);
	(void)compute_df(get_irg_start_block(irg), info);

	return info;
}

void be_free_dominance_frontiers(be_dom_front_info_t *info)
{
	obstack_free(&info->obst, NULL);
	pmap_destroy(info->df_map);
	free(info);
}

/* Get the dominance frontier of a block. */
ir_node **be_get_dominance_frontier(be_dom_front_info_t *info, ir_node *block)
{
	return pmap_get(info->df_map, block);
}

static void determine_phi_blocks(pset *copies, pset *copy_blocks, pset *phi_blocks, be_dom_front_info_t *df_info)
{
	ir_node *bl;
	waitq *worklist = new_waitq();
	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg, DBG_MODULE);

	/*
	 * Fill the worklist queue and the rest of the orig blocks array.
	 */
	for (bl = pset_first(copy_blocks); bl; bl = pset_next(copy_blocks)) {
		waitq_put(worklist, bl);
	}

	while (!pdeq_empty(worklist)) {
		ir_node *bl  = waitq_get(worklist);
		ir_node **df = be_get_dominance_frontier(df_info, bl);
		int i;

		ir_node *y;

		DBG((dbg, LEVEL_3, "dom front of %+F\n", bl));
		DEBUG_ONLY(
			for (i = ARR_LEN(df) - 1; i >= 0; --i)
				DBG((dbg, LEVEL_3, "\t%+F\n", df[i]))
		);

		for (i = ARR_LEN(df) - 1; i >= 0; --i) {
			y = df[i];
			if (!pset_find_ptr(phi_blocks, y)) {
				pset_insert_ptr(phi_blocks, y);

				/*
				 * Clear the link field of a possible phi block, since
				 * the possibly created phi will be stored there. See,
				 * search_def()
				 */
				set_irn_link(y, NULL);

				if(!pset_find_ptr(copy_blocks, y))
					waitq_put(worklist, y);
			}
		}
	}

	del_waitq(worklist);
}

/*
  ____ ____    _
 / ___/ ___|  / \
 \___ \___ \ / _ \
  ___) |__) / ___ \
 |____/____/_/   \_\
   ____                _                   _   _
  / ___|___  _ __  ___| |_ _ __ _   _  ___| |_(_) ___  _ __
 | |   / _ \| '_ \/ __| __| '__| | | |/ __| __| |/ _ \| '_ \
 | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_| | (_) | | | |
  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|_|\___/|_| |_|

*/

/**
 * Find the copy of the given original node whose value is 'active'
 * at a usage.
 *
 * The usage is given as a node and a position. Initially, the given operand
 * points to a node for which copies were introduced. We have to find
 * the valid copy for this usage. This is done by traversing the
 * dominance tree upwards. If the usage is a phi function, we start
 * traversing from the predecessor block which corresponds to the phi
 * usage.
 *
 * @param usage       The node which uses the original node.
 * @param pos         The position of the argument which corresponds to the original node.
 * @param copies      A set containing all node which are copies from the original node.
 * @param copy_blocks A set containing all basic block in which copies of the original node are located.
 * @param phis        A set where all created phis are recorded.
 * @param phi_blocks  A set of all blocks where Phis shall be inserted (iterated dominance frontier).
 * @param mode        The mode for the Phi if one has to be created.
 * @return            The valid copy for usage.
 */
static ir_node *search_def(ir_node *usage, int pos, pset *copies, pset *copy_blocks, pset *phis, pset *phi_blocks, ir_mode *mode)
{
	ir_node *curr_bl;
	ir_node *start_irn;
	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg, DBG_MODULE);

	curr_bl = get_nodes_block(usage);

	DBG((dbg, LEVEL_1, "Searching valid def for use %+F at pos %d\n", usage, pos));
	/*
	 * If the usage is in a phi node, search the copy in the
	 * predecessor denoted by pos.
	 */
	if(is_Phi(usage)) {
		curr_bl = get_Block_cfgpred_block(curr_bl, pos);
		start_irn = sched_last(curr_bl);
	} else {
		start_irn = sched_prev(usage);
	}

	/*
	 * Traverse the dominance tree upwards from the
	 * predecessor block of the usage.
  	 */
	while(curr_bl != NULL) {
		ir_node *phim;

	    /*
		 * If this block contains a copy, search the block
		 * instruction by instruction. If nothing is found
		 * search for a not scheduled PhiM.
		 */
		if(pset_find_ptr(copy_blocks, curr_bl)) {
			ir_node *irn;

			/* Look at each instruction from last to first. */
			sched_foreach_reverse_from(start_irn, irn) {

				/* Take the first copy we find. */
				if(pset_find_ptr(copies, irn))
					return irn;
			}

			for(phim = pset_first(copies); phim; phim = pset_next(copies)) {
				if(!is_Phi(phim) || !(get_irn_mode(phim) == mode_M))
					continue;

				if(get_nodes_block(phim) == curr_bl) {
					pset_break(copies);
					return phim;
				}
			}
		}

		if(pset_find_ptr(phi_blocks, curr_bl)) {
			ir_node *phi = get_irn_link(curr_bl);

			if(phi == NULL) {
				int i, n_preds = get_irn_arity(curr_bl);
				ir_graph *irg = get_irn_irg(curr_bl);
				ir_node **ins = alloca(n_preds * sizeof(ins[0]));

				for(i = 0; i < n_preds; ++i)
					ins[i] = new_r_Bad(irg);

				phi = new_r_Phi(irg, curr_bl, n_preds, ins, mode);
				DBG((dbg, LEVEL_2, "\tcreating phi %+F in %+F\n", phi, curr_bl));

				set_irn_link(curr_bl, phi);
				if(mode != mode_M)
					sched_add_after(curr_bl, phi);

				for(i = 0; i < n_preds; ++i) {
					ir_node *arg = search_def(phi, i, copies, copy_blocks, phis, phi_blocks, mode);
					if(arg == NULL) {
						ir_node *irn;

						ir_fprintf(stderr, "no definition found for %+F at position %d\nCopies: ", phi, i);
						for(irn = pset_first(copies); irn; irn = pset_next(copies)) {
							ir_fprintf(stderr, "%+F ", irn);
						}
						ir_fprintf(stderr, "\n\n");
						assert(arg && "no definition found");
					}
					DBG((dbg, LEVEL_2, "\t\t%+F(%d) -> %+F\n", phi, i, arg));
					set_irn_n(phi, i, arg);
				}

				if(phis != NULL)
					pset_insert_ptr(phis, phi);
			}

			return phi;
		}

		/* If were not done yet, look in the immediate dominator */
		curr_bl = get_Block_idom(curr_bl);
		if(curr_bl)
			start_irn = sched_last(curr_bl);
	}

	return NULL;
}

static void fix_usages(pset *copies, pset *copy_blocks, pset *phi_blocks, pset *phis, pset *ignore_uses)
{
	int n_outs = 0;
	struct obstack obst;
	ir_node *irn;
	int i;
	struct out {
		ir_node *irn;
		int pos;
	} *outs;

	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg, DBG_MODULE);

	obstack_init(&obst);

	/*
 	 * Put all outs into an array.
 	 * This is necessary, since the outs would be modified while
 	 * iterating on them what could bring the outs module in trouble.
	 */
	for (irn = pset_first(copies); irn; irn = pset_next(copies)) {
		const ir_edge_t *edge;
		foreach_out_edge(irn, edge) {
			ir_node *src = get_edge_src_irn(edge);
			/* ignore all users from ignore_uses or keep-alives (user is End node) */
			if (! pset_find_ptr(ignore_uses, src) && ! is_End(src)) {
				struct out tmp;
				tmp.irn = src;
				tmp.pos = get_edge_src_pos(edge);
				obstack_grow(&obst, &tmp, sizeof(tmp));
				n_outs++;
			}
		}
	}
	outs = obstack_finish(&obst);

	/*
	 * Search the valid def for each out and set it.
	 */
	for(i = 0; i < n_outs; ++i) {
		ir_node *irn  = outs[i].irn;
		int pos       = outs[i].pos;
		ir_mode *mode = get_irn_mode(get_irn_n(irn, pos));

		ir_node *def;

		def = search_def(irn, pos, copies, copy_blocks, phis, phi_blocks, mode);
		DBG((dbg, LEVEL_2, "\t%+F(%d) -> %+F\n", irn, pos, def));

		if(def == NULL) {
			ir_fprintf(stderr, "no definition found for %+F at position %d\nCopies: ", irn, pos);
			for(irn = pset_first(copies); irn; irn = pset_next(copies)) {
				ir_fprintf(stderr, "%+F ", irn);
			}
			ir_fprintf(stderr, "\n\n");
			assert(def && "no definition found");
		}
		set_irn_n(irn, pos, def);
	}

	obstack_free(&obst, NULL);
}

#if 0
/**
 * Remove phis which are not necessary.
 * During place_phi_functions() phi functions are put on the dominance
 * frontiers blindly. However some of them will never be used (these
 * have at least one predecessor which is NULL, see search_def() for
 * this case). Since place_phi_functions() enters them into the
 * schedule, we have to remove them from there.
 *
 * @param copies The set of all copies made (including the phi functions).
 */
static void remove_odd_phis(pset *copies, pset *unused_copies)
{
	ir_node *irn;

	for(irn = pset_first(copies); irn; irn = pset_next(copies)) {
		if(is_Phi(irn)) {
			int i, n;
			int illegal = 0;

			assert(sched_is_scheduled(irn) && "phi must be scheduled");
			for(i = 0, n = get_irn_arity(irn); i < n && !illegal; ++i)
				illegal = get_irn_n(irn, i) == NULL;

			if(illegal) {
				for(i = 0, n = get_irn_arity(irn); i < n; ++i)
					set_irn_n(irn, i, new_Bad());
				sched_remove(irn);
			}
		}
	}

	for(irn = pset_first(unused_copies); irn; irn = pset_next(unused_copies)) {
		for(i = 0, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_Bad());
		sched_remove(irn);
	}
}
#endif /* if 0 */

void be_ssa_constr_phis_ignore(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[], pset *phis, pset *ignore_uses)
{
	pset *irns = pset_new_ptr(n);
	int i;

	for(i = 0; i < n; ++i)
		pset_insert_ptr(irns, nodes[i]);
	be_ssa_constr_set_phis_ignore(info, lv, irns, phis, ignore_uses);
	del_pset(irns);
}

void be_ssa_constr_ignore(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[], pset *ignore_uses)
{
	be_ssa_constr_phis_ignore(info, lv, n, nodes, NULL, ignore_uses);
}

void be_ssa_constr(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[])
{
	pset *empty_set = be_empty_set();
	be_ssa_constr_ignore(info, lv, n, nodes, empty_set);
}

void be_ssa_constr_set_phis_ignore(be_dom_front_info_t *df, be_lv_t *lv, pset *nodes, pset *phis, pset *ignore_uses)
{
	int n                  = pset_count(nodes);
	pset *blocks           = pset_new_ptr(n);
	pset *phi_blocks       = pset_new_ptr(n);
	int save_optimize      = get_optimize();
	int save_normalize     = get_opt_normalize();
	int phis_set_created   = 0;
	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg, DBG_MODULE);

	ir_node *irn;

	/* We need to collect the phi functions to compute their liveness. */
	if(lv && !phis) {
		phis_set_created = 1;
		phis = pset_new_ptr_default();
	}

	DBG((dbg, LEVEL_1, "Introducing following copies for:\n"));

	/* Fill the sets. */
	for(irn = pset_first(nodes); irn; irn = pset_next(nodes)) {
		ir_node *bl = get_nodes_block(irn);
		pset_insert_ptr(blocks, bl);
		DBG((dbg, LEVEL_1, "\t%+F in %+F\n", irn, bl));
	}

	/*
	 * Disable optimization so that the phi functions do not
	 * disappear.
	 */
	set_optimize(0);
	set_opt_normalize(0);

	/*
	 * Place the phi functions and reroute the usages.
	 */
	determine_phi_blocks(nodes, blocks, phi_blocks, df);
	fix_usages(nodes, blocks, phi_blocks, phis, ignore_uses);

	/* reset the optimizations */
	set_optimize(save_optimize);
	set_opt_normalize(save_normalize);

	del_pset(phi_blocks);
	del_pset(blocks);

	/* Recompute the liveness (if wanted) of the original nodes, the copies and the inserted phis. */
	if(lv) {
#if 1
		foreach_pset(nodes, irn)
			be_liveness_update(lv, irn);

		foreach_pset(phis, irn)
			be_liveness_introduce(lv, irn);
#else
		be_liveness_recompute(lv);
#endif
	}

	/* Free the phi set of we created it. */
	if(phis_set_created)
		del_pset(phis);

}

void be_ssa_constr_set_phis(be_dom_front_info_t *df, be_lv_t *lv, pset *nodes, pset *phis)
{
	pset *empty_set = be_empty_set();
	be_ssa_constr_set_phis_ignore(df, lv, nodes, phis, empty_set);
}

void be_ssa_constr_set_ignore(be_dom_front_info_t *df, be_lv_t *lv, pset *nodes, pset *ignore_uses)
{
	be_ssa_constr_set_phis_ignore(df, lv, nodes, NULL, ignore_uses);
}

void be_ssa_constr_set(be_dom_front_info_t *info, be_lv_t *lv, pset *nodes)
{
	pset *empty_set = be_empty_set();
	be_ssa_constr_set_ignore(info, lv, nodes, empty_set);
}

/*
  ___                     _     ____
 |_ _|_ __  ___  ___ _ __| |_  |  _ \ ___ _ __ _ __ ___
  | || '_ \/ __|/ _ \ '__| __| | |_) / _ \ '__| '_ ` _ \
  | || | | \__ \  __/ |  | |_  |  __/  __/ |  | | | | | |
 |___|_| |_|___/\___|_|   \__| |_|   \___|_|  |_| |_| |_|

*/

ir_node *insert_Perm_after(const arch_env_t *arch_env,
						   be_lv_t *lv,
						   const arch_register_class_t *cls,
						   be_dom_front_info_t *dom_front,
						   ir_node *pos)
{
	ir_node *bl     = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_graph *irg   = get_irn_irg(bl);
	pset *live      = pset_new_ptr_default();
	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg, "be.node");

	ir_node *curr, *irn, *perm, **nodes;
	int i, n;

	DBG((dbg, LEVEL_1, "Insert Perm after: %+F\n", pos));

	be_liveness_nodes_live_at(lv, arch_env, cls, pos, live);

	n = pset_count(live);

	if(n == 0) {
		del_pset(live);
		return NULL;
	}

	nodes = xmalloc(n * sizeof(nodes[0]));

	DBG((dbg, LEVEL_1, "live:\n"));
	for(irn = pset_first(live), i = 0; irn; irn = pset_next(live), i++) {
		DBG((dbg, LEVEL_1, "\t%+F\n", irn));
		nodes[i] = irn;
	}
	del_pset(live);

	perm = be_new_Perm(cls, irg, bl, n, nodes);
	sched_add_after(pos, perm);
	free(nodes);

	curr = perm;
	for (i = 0; i < n; ++i) {
		ir_node *copies[2];
		ir_node *perm_op = get_irn_n(perm, i);
		const arch_register_t *reg = arch_get_irn_register(arch_env, perm_op);

		ir_mode *mode = get_irn_mode(perm_op);
		ir_node *proj = new_r_Proj(irg, bl, perm, mode, i);
		arch_set_irn_register(arch_env, proj, reg);

		sched_add_after(curr, proj);
		curr = proj;

		copies[0] = perm_op;
		copies[1] = proj;

		be_ssa_constr(dom_front, lv, 2, copies);
	}

	return perm;
}

struct _elr_closure_t {
	struct obstack obst;
	const be_chordal_env_t *cenv;
};

static void elr_split_walker(ir_node *bl, void *data)
{
	struct _elr_closure_t *c     = data;
	const be_chordal_env_t *cenv = c->cenv;
	const arch_env_t *aenv       = cenv->birg->main_env->arch_env;
	be_lv_t *lv                  = cenv->birg->lv;
	be_dom_front_info_t *dom_front = cenv->birg->dom_front;
	be_insn_t *insn;
	be_insn_env_t ie;

	be_insn_env_init(&ie, cenv->birg, cenv->cls, &c->obst);

	for(insn = be_scan_insn(&ie, sched_first(bl)); !is_Block(insn->irn); insn = be_scan_insn(&ie, insn->next_insn)) {
		ir_node *pred = sched_prev(insn->irn);
		if(!is_Block(pred) && !is_Phi(insn->irn))
			insert_Perm_after(aenv, lv, cenv->cls, dom_front, insn->irn);
	}
}

void extreme_liverange_splitting(struct _be_chordal_env_t *cenv)
{
	struct _elr_closure_t c;
	be_lv_t *lv = cenv->birg->lv;

	c.cenv = cenv;
	obstack_init(&c.obst);
	be_liveness_recompute(lv);
	irg_block_walk_graph(cenv->irg, elr_split_walker, NULL, &c);
	be_liveness_recompute(lv);
	obstack_free(&c.obst, NULL);
}

/**
 * Post-block-walker: Find blocks containing only one jump and
 * remove them.
 */
static void remove_empty_block(ir_node *block, void *data) {
	const ir_edge_t *edge, *next;
	ir_node *node;
	int *changed = data;
	ir_node *jump = NULL;

	assert(is_Block(block));

	if (get_Block_n_cfgpreds(block) != 1)
		return;

	sched_foreach(block, node) {
		if (! is_Jmp(node))
			return;
		if (jump != NULL) {
			/* we should never have 2 jumps in a block */
			assert(0 && "We should never have 2 jumps in a block");
			return;
		}
		jump = node;
	}

	if (jump == NULL)
		return;

	node = get_Block_cfgpred(block, 0);
	foreach_out_edge_safe(jump, edge, next) {
		ir_node *block = get_edge_src_irn(edge);
		int     pos    = get_edge_src_pos(edge);

		set_irn_n(block, pos, node);
	}

	set_Block_cfgpred(block, 0, new_Bad());
	sched_remove(jump);
	*changed = 1;
}

/* removes basic blocks that just contain a jump instruction */
int be_remove_empty_blocks(ir_graph *irg) {
	int changed = 0;

	irg_block_walk_graph(irg, remove_empty_block, NULL, &changed);
	if (changed) {
		/* invalidate analysis info */
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_outs_inconsistent(irg);
	}
	return changed;
}

typedef struct _be_dead_out_env_t {
	ir_graph *irg;
	bitset_t *reachable;
	DEBUG_ONLY(firm_dbg_module_t *dbg);
} be_dead_out_env_t;

/**
 * Check all block out edges and kill all unreachable nodes.
 */
static void kill_dead_outs(ir_node *block, void *wenv) {
	be_dead_out_env_t *env   = wenv;
	ir_graph          *irg   = env->irg;
	ir_node           *globs = get_irg_globals(irg);
	ir_node           *tls   = get_irg_tls(irg);
	const ir_edge_t   *edge, *tmp_edge;

	/* check all out edges */
	foreach_out_edge_safe(block, edge, tmp_edge) {
		ir_node *src = get_edge_src_irn(edge);

		if (! bitset_is_set(env->reachable, get_irn_idx(src))) {
			/* BEWARE: do not kill anchors or TLS */
			if (src != globs && src != tls) {
				DBG((env->dbg, LEVEL_1, "killing %+F, only reachable from %+F\n", src, block));
				be_kill_node(src);
			}
		}
	}
}

static void set_reachable(ir_node *node, void *data) {
	bitset_t *reachable = data;
	bitset_set(reachable, get_irn_idx(node));
}

/**
 * Set input of all nodes only reachable via out edges to BAD.
 */
void be_kill_dead_nodes(ir_graph *irg) {
	be_dead_out_env_t env;

	env.irg       = irg;
	env.reachable = bitset_alloca(get_irg_last_idx(irg));
	FIRM_DBG_REGISTER(env.dbg, "firm.be.killdead");

	/* collect all reachable nodes */
	irg_walk_in_or_dep_graph(irg, set_reachable, NULL, env.reachable);

	/* go over out edges of block and kill all unreachable nodes */
	irg_block_walk_graph(irg, kill_dead_outs, NULL, &env);
}
