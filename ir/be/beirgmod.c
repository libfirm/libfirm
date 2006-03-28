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

#include "be_t.h"
#include "bearch.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "beutil.h"

#include "beirgmod.h"

#define DBG_MODULE firm_dbg_register("firm.be.irgmod")
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


struct _dom_front_info_t {
  pmap *df_map;
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

static void compute_df(ir_node *n, pmap *df_map)
{
  ir_node *c;
  const ir_edge_t *edge;
  pset *df = pset_new_ptr_default();

  /* Add local dominance frontiers */
  foreach_block_succ(n, edge) {
    ir_node *y = edge->src;

    if(get_idom(y) != n)
      pset_insert_ptr(df, y);
  }

  /*
   * Go recursively down the dominance tree and add all blocks
   * int the dominance frontiers of the children, which are not
   * dominated by the given block.
   */
  for(c = get_Block_dominated_first(n); c; c = get_Block_dominated_next(c)) {
    pset *df_c;
    ir_node *w;

    compute_df(c, df_map);
    df_c = pmap_get(df_map, c);

    for(w = pset_first(df_c); w; w = pset_next(df_c)) {
      if(get_idom(w) != n)
        pset_insert_ptr(df, w);
    }
  }

  pmap_insert(df_map, n, df);

}

dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg)
{
  dom_front_info_t *info = xmalloc(sizeof(*info));

  edges_assure(irg);
  info->df_map = pmap_create();
  compute_doms(irg);
  compute_df(get_irg_start_block(irg), info->df_map);

  return info;
}

void be_free_dominance_frontiers(dom_front_info_t *info)
{
  pmap_entry *ent;

  for(ent = pmap_first(info->df_map); ent; ent = pmap_next(info->df_map))
    del_pset(ent->value);

  pmap_destroy(info->df_map);
  free(info);
}

pset *be_get_dominance_frontier(dom_front_info_t *info, ir_node *block)
{
  return pmap_get(info->df_map, block);
}

static void determine_phi_blocks(pset *copies, pset* copy_blocks, pset *phi_blocks, dom_front_info_t *df_info)
{
	ir_node *bl;
	pdeq *worklist = new_pdeq();
	firm_dbg_module_t *dbg = DBG_MODULE;

	/*
	* Fill the worklist queue and the rest of the orig blocks array.
	*/
	for(bl = pset_first(copy_blocks); bl; bl = pset_next(copy_blocks)) {
		pdeq_putr(worklist, bl);
	}

	while(!pdeq_empty(worklist)) {
		ir_node *bl = pdeq_getl(worklist);
		pset *df    = be_get_dominance_frontier(df_info, bl);

		ir_node *y;

		DBG((dbg, LEVEL_3, "dom front of %+F\n", bl));
		for(y = pset_first(df); y; y = pset_next(df))
			DBG((dbg, LEVEL_3, "\t%+F\n", y));

		for(y = pset_first(df); y; y = pset_next(df)) {
			if(!pset_find_ptr(phi_blocks, y)) {
				pset_insert_ptr(phi_blocks, y);

				/*
				* Clear the link field of a possible phi block, since
				* the possibly created phi will be stored there. See,
				* search_def()
				*/
				set_irn_link(y, NULL);

				if(!pset_find_ptr(copy_blocks, y))
					pdeq_putr(worklist, y);

			}
		}
	}

	del_pdeq(worklist);
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
 * the valid copy for this usage. This is done by travering the
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
	firm_dbg_module_t *dbg = DBG_MODULE;

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

	    /*
		 * If this block contains a copy, search the block
		 * instruction by instruction.
		 */
		if(pset_find_ptr(copy_blocks, curr_bl)) {
			ir_node *irn;

			/* Look at each instruction from last to first. */
			sched_foreach_reverse_from(start_irn, irn) {

				/* Take the first copy we find. */
				if(pset_find_ptr(copies, irn))
					return irn;
			}
		}

		if(pset_find_ptr(phi_blocks, curr_bl)) {
			ir_node *phi = get_irn_link(curr_bl);

			if(!phi) {
				int i, n_preds = get_irn_arity(curr_bl);
				ir_graph *irg = get_irn_irg(curr_bl);
				ir_node **ins = xmalloc(n_preds * sizeof(ins[0]));

				for(i = 0; i < n_preds; ++i)
					ins[i] = new_r_Bad(irg);

				phi = new_r_Phi(irg, curr_bl, n_preds, ins, mode);
				DBG((dbg, LEVEL_2, "\tcreating phi %+F in %+F\n", phi, curr_bl));

				set_irn_link(curr_bl, phi);
				sched_add_after(curr_bl, phi);
				free(ins);

				for(i = 0; i < n_preds; ++i) {
					ir_node *arg = search_def(phi, i, copies, copy_blocks, phis, phi_blocks, mode);
					DBG((dbg, LEVEL_2, "\t\t%+F(%d) -> %+F\n", phi, i, arg));
					set_irn_n(phi, i, arg);
				}

				if(phis)
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
	firm_dbg_module_t *dbg = DBG_MODULE;
	int n_outs             = 0;

	struct obstack obst;
	ir_node *irn;
	int i;

	struct out {
		ir_node *irn;
		int pos;
	} *outs;

	obstack_init(&obst);

	/*
 	* Put all outs into an array.
 	* This is necessary, since the outs would be modified while
 	* iterating on them what could bring the outs module in trouble.
	*/
	for(irn = pset_first(copies); irn; irn = pset_next(copies)) {
		const ir_edge_t *edge;
		foreach_out_edge(irn, edge) {
			if(!pset_find_ptr(ignore_uses, get_edge_src_irn(edge))) {
				struct out tmp;
				tmp.irn = get_edge_src_irn(edge);
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

		if(def != NULL)
			set_irn_n(irn, pos, def);
	}

	obstack_free(&obst, NULL);
}

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

      if(illegal)
        sched_remove(irn);
    }
  }

  for(irn = pset_first(unused_copies); irn; irn = pset_next(unused_copies)) {
		sched_remove(irn);
	}
}

void be_ssa_constr_phis_ignore(dom_front_info_t *info, int n, ir_node *nodes[], pset *phis, pset *ignore_uses)
{
	pset *irns = pset_new_ptr(n);
	int i;

	for(i = 0; i < n; ++i)
		pset_insert_ptr(irns, nodes[i]);
	be_ssa_constr_set_phis_ignore(info, irns, phis, ignore_uses);
	del_pset(irns);
}

void be_ssa_constr_ignore(dom_front_info_t *info, int n, ir_node *nodes[], pset *ignore_uses)
{
	be_ssa_constr_phis_ignore(info, n, nodes, NULL, ignore_uses);
}

void be_ssa_constr(dom_front_info_t *info, int n, ir_node *nodes[])
{
	pset *empty_set = be_empty_set();
	assert(pset_count(empty_set) == 0);
	be_ssa_constr_ignore(info, n, nodes, empty_set);
}

void be_ssa_constr_set_phis_ignore(dom_front_info_t *df, pset *nodes, pset *phis, pset *ignore_uses)
{
	int n                  = pset_count(nodes);
	pset *blocks           = pset_new_ptr(n);
	pset *phi_blocks       = pset_new_ptr(n);
	int save_optimize      = get_optimize();
	int save_normalize     = get_opt_normalize();
	firm_dbg_module_t *dbg = DBG_MODULE;

	ir_node *irn;

	firm_dbg_set_mask(dbg, DBG_LEVEL);
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

}

void be_ssa_constr_set_phis(dom_front_info_t *df, pset *nodes, pset *phis)
{
	pset *empty_set = be_empty_set();
	assert(pset_count(empty_set) == 0);

	be_ssa_constr_set_phis_ignore(df, nodes,phis, empty_set);
}

void be_ssa_constr_set_ignore(dom_front_info_t *df, pset *nodes, pset *ignore_uses)
{
	be_ssa_constr_set_phis_ignore(df, nodes, NULL, ignore_uses);
}

void be_ssa_constr_set(dom_front_info_t *info, pset *nodes)
{
	pset *empty_set = be_empty_set();
	assert(pset_count(empty_set) == 0);
	be_ssa_constr_set_ignore(info, nodes, empty_set);
}

/*
  ___                     _     ____
 |_ _|_ __  ___  ___ _ __| |_  |  _ \ ___ _ __ _ __ ___
  | || '_ \/ __|/ _ \ '__| __| | |_) / _ \ '__| '_ ` _ \
  | || | | \__ \  __/ |  | |_  |  __/  __/ |  | | | | | |
 |___|_| |_|___/\___|_|   \__| |_|   \___|_|  |_| |_| |_|

*/

ir_node *insert_Perm_after(const arch_env_t *arch_env,
						   const arch_register_class_t *cls,
						   dom_front_info_t *dom_front,
						   ir_node *pos)
{
	ir_node *bl                 = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_graph *irg               = get_irn_irg(bl);
	pset *live                  = pset_new_ptr_default();
	firm_dbg_module_t *dbg      = firm_dbg_register("be.node");

	ir_node *curr, *irn, *perm, **nodes;
	int i, n;

	DBG((dbg, LEVEL_1, "Insert Perm after: %+F\n", pos));

	if(!be_liveness_nodes_live_at(arch_env, cls, pos, live));

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
	for(i = 0; i < n; ++i) {
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
		be_ssa_constr(dom_front, 2, copies);
	}
	return perm;
}
