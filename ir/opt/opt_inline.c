/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief    Dead node elimination and Procedure Inlining.
 * @author   Michael Beck, Goetz Lindenmaier
 * @version  $Id$
 */
#include "config.h"

#include <limits.h>
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"

#include "iroptimize.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "array_t.h"
#include "list.h"
#include "pset.h"
#include "pmap.h"
#include "pdeq.h"
#include "xmalloc.h"
#include "pqueue.h"

#include "irouts.h"
#include "irloop_t.h"
#include "irbackedge_t.h"
#include "opt_inline_t.h"
#include "cgana.h"
#include "trouts.h"
#include "error.h"

#include "analyze_irg_args.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irhooks.h"
#include "irtools.h"
#include "iropt_dbg.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/*------------------------------------------------------------------*/
/* Routines for dead node elimination / copying garbage collection  */
/* of the obstack.                                                  */
/*------------------------------------------------------------------*/

/**
 * Remember the new node in the old node by using a field all nodes have.
 */
#define set_new_node(oldn, newn)  set_irn_link(oldn, newn)

/**
 * Get this new node, before the old node is forgotten.
 */
#define get_new_node(oldn) get_irn_link(oldn)

/**
 * Check if a new node was set.
 */
#define has_new_node(n) (get_new_node(n) != NULL)

/**
 * We use the block_visited flag to mark that we have computed the
 * number of useful predecessors for this block.
 * Further we encode the new arity in this flag in the old blocks.
 * Remembering the arity is useful, as it saves a lot of pointer
 * accesses.  This function is called for all Phi and Block nodes
 * in a Block.
 */
static inline int
compute_new_arity(ir_node *b) {
	int i, res, irn_arity;
	int irg_v, block_v;

	irg_v = get_irg_block_visited(current_ir_graph);
	block_v = get_Block_block_visited(b);
	if (block_v >= irg_v) {
		/* we computed the number of preds for this block and saved it in the
		   block_v flag */
		return block_v - irg_v;
	} else {
		/* compute the number of good predecessors */
		res = irn_arity = get_irn_arity(b);
		for (i = 0; i < irn_arity; i++)
			if (is_Bad(get_irn_n(b, i))) res--;
			/* save it in the flag. */
			set_Block_block_visited(b, irg_v + res);
			return res;
	}
}

/**
 * Copies the node to the new obstack. The Ins of the new node point to
 * the predecessors on the old obstack.  For block/phi nodes not all
 * predecessors might be copied.  n->link points to the new node.
 * For Phi and Block nodes the function allocates in-arrays with an arity
 * only for useful predecessors.  The arity is determined by counting
 * the non-bad predecessors of the block.
 *
 * @param n    The node to be copied
 * @param env  if non-NULL, the node number attribute will be copied to the new node
 *
 * Note: Also used for loop unrolling.
 */
static void copy_node(ir_node *n, void *env) {
	ir_node *nn, *block;
	int new_arity;
	ir_op *op = get_irn_op(n);
	(void) env;

	if (op == op_Bad) {
		/* node copied already */
		return;
	} else if (op == op_Block) {
		block = NULL;
		new_arity = compute_new_arity(n);
		n->attr.block.graph_arr = NULL;
	} else {
		block = get_nodes_block(n);
		if (op == op_Phi) {
			new_arity = compute_new_arity(block);
		} else {
			new_arity = get_irn_arity(n);
		}
	}
	nn = new_ir_node(get_irn_dbg_info(n),
		current_ir_graph,
		block,
		op,
		get_irn_mode(n),
		new_arity,
		get_irn_in(n) + 1);
	/* Copy the attributes.  These might point to additional data.  If this
	   was allocated on the old obstack the pointers now are dangling.  This
	   frees e.g. the memory of the graph_arr allocated in new_immBlock. */
	if (op == op_Block) {
		/* we cannot allow blocks WITHOUT macroblock input */
		set_Block_MacroBlock(nn, get_Block_MacroBlock(n));
	}
	copy_node_attr(n, nn);

#ifdef DEBUG_libfirm
	{
		int copy_node_nr = env != NULL;
		if (copy_node_nr) {
			/* for easier debugging, we want to copy the node numbers too */
			nn->node_nr = n->node_nr;
		}
	}
#endif

	set_new_node(n, nn);
	hook_dead_node_elim_subst(current_ir_graph, n, nn);
}

/**
 * Copies new predecessors of old node to new node remembered in link.
 * Spare the Bad predecessors of Phi and Block nodes.
 */
static void copy_preds(ir_node *n, void *env) {
	ir_node *nn, *block;
	int i, j, irn_arity;
	(void) env;

	nn = get_new_node(n);

	if (is_Block(n)) {
		/* copy the macro block header */
		ir_node *mbh = get_Block_MacroBlock(n);

		if (mbh == n) {
			/* this block is a macroblock header */
			set_Block_MacroBlock(nn, nn);
		} else {
			/* get the macro block header */
			ir_node *nmbh = get_new_node(mbh);
			assert(nmbh != NULL);
			set_Block_MacroBlock(nn, nmbh);
		}

		/* Don't copy Bad nodes. */
		j = 0;
		irn_arity = get_irn_arity(n);
		for (i = 0; i < irn_arity; i++) {
			if (! is_Bad(get_irn_n(n, i))) {
				ir_node *pred = get_irn_n(n, i);
				set_irn_n(nn, j, get_new_node(pred));
				j++;
			}
		}
		/* repair the block visited flag from above misuse. Repair it in both
		   graphs so that the old one can still be used. */
		set_Block_block_visited(nn, 0);
		set_Block_block_visited(n, 0);
		/* Local optimization could not merge two subsequent blocks if
		   in array contained Bads.  Now it's possible.
		   We don't call optimize_in_place as it requires
		   that the fields in ir_graph are set properly. */
		if ((get_opt_control_flow_straightening()) &&
			(get_Block_n_cfgpreds(nn) == 1) &&
			is_Jmp(get_Block_cfgpred(nn, 0))) {
			ir_node *old = get_nodes_block(get_Block_cfgpred(nn, 0));
			if (nn == old) {
				/* Jmp jumps into the block it is in -- deal self cycle. */
				assert(is_Bad(get_new_node(get_irg_bad(current_ir_graph))));
				exchange(nn, get_new_node(get_irg_bad(current_ir_graph)));
			} else {
				exchange(nn, old);
			}
		}
	} else if (is_Phi(n) && get_irn_arity(n) > 0) {
		/* Don't copy node if corresponding predecessor in block is Bad.
		   The Block itself should not be Bad. */
		block = get_nodes_block(n);
		set_nodes_block(nn, get_new_node(block));
		j = 0;
		irn_arity = get_irn_arity(n);
		for (i = 0; i < irn_arity; i++) {
			if (! is_Bad(get_irn_n(block, i))) {
				ir_node *pred = get_irn_n(n, i);
				set_irn_n(nn, j, get_new_node(pred));
				/*if (is_backedge(n, i)) set_backedge(nn, j);*/
				j++;
			}
		}
		/* If the pre walker reached this Phi after the post walker visited the
		   block block_visited is > 0. */
		set_Block_block_visited(get_nodes_block(n), 0);
		/* Compacting the Phi's ins might generate Phis with only one
		   predecessor. */
		if (get_irn_arity(nn) == 1)
			exchange(nn, get_irn_n(nn, 0));
	} else {
		irn_arity = get_irn_arity(n);
		for (i = -1; i < irn_arity; i++)
			set_irn_n(nn, i, get_new_node(get_irn_n(n, i)));
	}
	/* Now the new node is complete.  We can add it to the hash table for CSE.
	   @@@ inlining aborts if we identify End. Why? */
	if (!is_End(nn))
		add_identities(current_ir_graph->value_table, nn);
}

/**
 * Copies the graph recursively, compacts the keep-alives of the end node.
 *
 * @param irg           the graph to be copied
 * @param copy_node_nr  If non-zero, the node number will be copied
 */
static void copy_graph(ir_graph *irg, int copy_node_nr) {
	ir_node *oe, *ne, *ob, *nb, *om, *nm; /* old end, new end, old bad, new bad, old NoMem, new NoMem */
	ir_node *ka;      /* keep alive */
	int i, irn_arity;
	unsigned long vfl;

	/* Some nodes must be copied by hand, sigh */
	vfl = get_irg_visited(irg);
	set_irg_visited(irg, vfl + 1);

	oe = get_irg_end(irg);
	mark_irn_visited(oe);
	/* copy the end node by hand, allocate dynamic in array! */
	ne = new_ir_node(get_irn_dbg_info(oe),
		irg,
		NULL,
		op_End,
		mode_X,
		-1,
		NULL);
	/* Copy the attributes.  Well, there might be some in the future... */
	copy_node_attr(oe, ne);
	set_new_node(oe, ne);

	/* copy the Bad node */
	ob = get_irg_bad(irg);
	mark_irn_visited(ob);
	nb = new_ir_node(get_irn_dbg_info(ob),
		irg,
		NULL,
		op_Bad,
		mode_T,
		0,
		NULL);
	copy_node_attr(ob, nb);
	set_new_node(ob, nb);

	/* copy the NoMem node */
	om = get_irg_no_mem(irg);
	mark_irn_visited(om);
	nm = new_ir_node(get_irn_dbg_info(om),
		irg,
		NULL,
		op_NoMem,
		mode_M,
		0,
		NULL);
	copy_node_attr(om, nm);
	set_new_node(om, nm);

	/* copy the live nodes */
	set_irg_visited(irg, vfl);
	irg_walk(get_nodes_block(oe), copy_node, copy_preds, INT_TO_PTR(copy_node_nr));

	/* Note: from yet, the visited flag of the graph is equal to vfl + 1 */

	/* visit the anchors as well */
	for (i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *n = get_irg_anchor(irg, i);

		if (n && (get_irn_visited(n) <= vfl)) {
			set_irg_visited(irg, vfl);
			irg_walk(n, copy_node, copy_preds, INT_TO_PTR(copy_node_nr));
		}
	}

	/* copy_preds for the end node ... */
	set_nodes_block(ne, get_new_node(get_nodes_block(oe)));

	/*- ... and now the keep alives. -*/
	/* First pick the not marked block nodes and walk them.  We must pick these
	   first as else we will oversee blocks reachable from Phis. */
	irn_arity = get_End_n_keepalives(oe);
	for (i = 0; i < irn_arity; i++) {
		ka = get_End_keepalive(oe, i);
		if (is_Block(ka)) {
			if (get_irn_visited(ka) <= vfl) {
				/* We must keep the block alive and copy everything reachable */
				set_irg_visited(irg, vfl);
				irg_walk(ka, copy_node, copy_preds, INT_TO_PTR(copy_node_nr));
			}
			add_End_keepalive(ne, get_new_node(ka));
		}
	}

	/* Now pick other nodes.  Here we will keep all! */
	irn_arity = get_End_n_keepalives(oe);
	for (i = 0; i < irn_arity; i++) {
		ka = get_End_keepalive(oe, i);
		if (!is_Block(ka)) {
			if (get_irn_visited(ka) <= vfl) {
				/* We didn't copy the node yet.  */
				set_irg_visited(irg, vfl);
				irg_walk(ka, copy_node, copy_preds, INT_TO_PTR(copy_node_nr));
			}
			add_End_keepalive(ne, get_new_node(ka));
		}
	}

	/* start block sometimes only reached after keep alives */
	set_nodes_block(nb, get_new_node(get_nodes_block(ob)));
	set_nodes_block(nm, get_new_node(get_nodes_block(om)));
}

/**
 * Copies the graph reachable from current_ir_graph->end to the obstack
 * in current_ir_graph and fixes the environment.
 * Then fixes the fields in current_ir_graph containing nodes of the
 * graph.
 *
 * @param copy_node_nr  If non-zero, the node number will be copied
 */
static void
copy_graph_env(int copy_node_nr) {
	ir_graph *irg = current_ir_graph;
	ir_node *old_end, *new_anchor;
	int i;

	/* remove end_except and end_reg nodes */
	old_end = get_irg_end(irg);
	set_irg_end_except (irg, old_end);
	set_irg_end_reg    (irg, old_end);

	/* Not all nodes remembered in irg might be reachable
	   from the end node.  Assure their link is set to NULL, so that
	   we can test whether new nodes have been computed. */
	for (i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *n = get_irg_anchor(irg, i);
		if (n != NULL)
			set_new_node(n, NULL);
	}
	/* we use the block walk flag for removing Bads from Blocks ins. */
	inc_irg_block_visited(irg);

	/* copy the graph */
	copy_graph(irg, copy_node_nr);

	/* fix the anchor */
	old_end    = get_irg_end(irg);
	new_anchor = new_Anchor(irg);

	for (i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *n = get_irg_anchor(irg, i);
		if (n)
			set_irn_n(new_anchor, i, get_new_node(n));
	}
	free_End(old_end);
	irg->anchor = new_anchor;

	/* ensure the new anchor is placed in the endblock */
	set_nodes_block(new_anchor, get_irg_end_block(irg));
}

/**
 * Copies all reachable nodes to a new obstack.  Removes bad inputs
 * from block nodes and the corresponding inputs from Phi nodes.
 * Merges single exit blocks with single entry blocks and removes
 * 1-input Phis.
 * Adds all new nodes to a new hash table for CSE.  Does not
 * perform CSE, so the hash table might contain common subexpressions.
 */
void dead_node_elimination(ir_graph *irg) {
	ir_graph *rem;
#ifdef INTERPROCEDURAL_VIEW
	int rem_ipview = get_interprocedural_view();
#endif
	struct obstack *graveyard_obst = NULL;
	struct obstack *rebirth_obst   = NULL;

	edges_deactivate(irg);

	/* inform statistics that we started a dead-node elimination run */
	hook_dead_node_elim(irg, 1);

	/* Remember external state of current_ir_graph. */
	rem = current_ir_graph;
	current_ir_graph = irg;
#ifdef INTERPROCEDURAL_VIEW
	set_interprocedural_view(0);
#endif

	assert(get_irg_phase_state(irg) != phase_building);

	/* Handle graph state */
	free_callee_info(irg);
	free_irg_outs(irg);
	free_trouts();

	/* @@@ so far we loose loops when copying */
	free_loop_information(irg);

	set_irg_doms_inconsistent(irg);

	/* A quiet place, where the old obstack can rest in peace,
	   until it will be cremated. */
	graveyard_obst = irg->obst;

	/* A new obstack, where the reachable nodes will be copied to. */
	rebirth_obst = XMALLOC(struct obstack);
	irg->obst = rebirth_obst;
	obstack_init(irg->obst);
	irg->last_node_idx = 0;

	/* We also need a new value table for CSE */
	del_identities(irg->value_table);
	irg->value_table = new_identities();

	/* Copy the graph from the old to the new obstack */
	copy_graph_env(/*copy_node_nr=*/1);

	/* Free memory from old unoptimized obstack */
	obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
	xfree(graveyard_obst);            /* ... then free it.           */

	/* inform statistics that the run is over */
	hook_dead_node_elim(irg, 0);

	current_ir_graph = rem;
#ifdef INTERPROCEDURAL_VIEW
	set_interprocedural_view(rem_ipview);
#endif
}

/**
 * Relink bad predecessors of a block and store the old in array to the
 * link field. This function is called by relink_bad_predecessors().
 * The array of link field starts with the block operand at position 0.
 * If block has bad predecessors, create a new in array without bad preds.
 * Otherwise let in array untouched.
 */
static void relink_bad_block_predecessors(ir_node *n, void *env) {
	ir_node **new_in, *irn;
	int i, new_irn_n, old_irn_arity, new_irn_arity = 0;
	(void) env;

	/* if link field of block is NULL, look for bad predecessors otherwise
	   this is already done */
	if (is_Block(n) && get_irn_link(n) == NULL) {
		/* save old predecessors in link field (position 0 is the block operand)*/
		set_irn_link(n, get_irn_in(n));

		/* count predecessors without bad nodes */
		old_irn_arity = get_irn_arity(n);
		for (i = 0; i < old_irn_arity; i++)
			if (!is_Bad(get_irn_n(n, i)))
				++new_irn_arity;

		/* arity changing: set new predecessors without bad nodes */
		if (new_irn_arity < old_irn_arity) {
			/* Get new predecessor array. We do not resize the array, as we must
			   keep the old one to update Phis. */
			new_in = NEW_ARR_D(ir_node *, current_ir_graph->obst, (new_irn_arity+1));

			/* set new predecessors in array */
			new_in[0] = NULL;
			new_irn_n = 1;
			for (i = 0; i < old_irn_arity; i++) {
				irn = get_irn_n(n, i);
				if (!is_Bad(irn)) {
					new_in[new_irn_n] = irn;
					is_backedge(n, i) ? set_backedge(n, new_irn_n-1) : set_not_backedge(n, new_irn_n-1);
					++new_irn_n;
				}
			}
			/* ARR_SETLEN(int, n->attr.block.backedge, new_irn_arity); */
			ARR_SHRINKLEN(n->attr.block.backedge, new_irn_arity);
			n->in = new_in;
		} /* ir node has bad predecessors */
	} /* Block is not relinked */
}

/**
 * Relinks Bad predecessors from Blocks and Phis called by walker
 * remove_bad_predecesors(). If n is a Block, call
 * relink_bad_block_redecessors(). If n is a Phi-node, call also the relinking
 * function of Phi's Block. If this block has bad predecessors, relink preds
 * of the Phi-node.
 */
static void relink_bad_predecessors(ir_node *n, void *env) {
	ir_node *block, **old_in;
	int i, old_irn_arity, new_irn_arity;

	/* relink bad predecessors of a block */
	if (is_Block(n))
		relink_bad_block_predecessors(n, env);

	/* If Phi node relink its block and its predecessors */
	if (is_Phi(n)) {
		/* Relink predecessors of phi's block */
		block = get_nodes_block(n);
		if (get_irn_link(block) == NULL)
			relink_bad_block_predecessors(block, env);

		old_in = (ir_node **)get_irn_link(block); /* Of Phi's Block */
		old_irn_arity = ARR_LEN(old_in);

		/* Relink Phi predecessors if count of predecessors changed */
		if (old_irn_arity != ARR_LEN(get_irn_in(block))) {
			/* set new predecessors in array
			   n->in[0] remains the same block */
			new_irn_arity = 1;
			for(i = 1; i < old_irn_arity; i++)
				if (!is_Bad(old_in[i])) {
					n->in[new_irn_arity] = n->in[i];
					is_backedge(n, i) ? set_backedge(n, new_irn_arity) : set_not_backedge(n, new_irn_arity);
					++new_irn_arity;
				}

				ARR_SETLEN(ir_node *, n->in, new_irn_arity);
				ARR_SETLEN(int, n->attr.phi.u.backedge, new_irn_arity);
		}
	} /* n is a Phi node */
}

/*
 * Removes Bad Bad predecessors from Blocks and the corresponding
 * inputs to Phi nodes as in dead_node_elimination but without
 * copying the graph.
 * On walking up set the link field to NULL, on walking down call
 * relink_bad_predecessors() (This function stores the old in array
 * to the link field and sets a new in array if arity of predecessors
 * changes).
 */
void remove_bad_predecessors(ir_graph *irg) {
	panic("Fix backedge handling first");
	irg_walk_graph(irg, firm_clear_link, relink_bad_predecessors, NULL);
}


/*
   __                      _  __ __
  (_     __    o     _    | \/  |_
  __)|_| | \_/ | \_/(/_   |_/\__|__

  The following stuff implements a facility that automatically patches
  registered ir_node pointers to the new node when a dead node elimination occurs.
*/

struct _survive_dce_t {
	struct obstack obst;
	pmap *places;
	pmap *new_places;
	hook_entry_t dead_node_elim;
	hook_entry_t dead_node_elim_subst;
};

typedef struct _survive_dce_list_t {
	struct _survive_dce_list_t *next;
	ir_node **place;
} survive_dce_list_t;

static void dead_node_hook(void *context, ir_graph *irg, int start) {
	survive_dce_t *sd = context;
	(void) irg;

	/* Create a new map before the dead node elimination is performed. */
	if (start) {
		sd->new_places = pmap_create_ex(pmap_count(sd->places));
	} else {
		/* Patch back all nodes if dead node elimination is over and something is to be done. */
		pmap_destroy(sd->places);
		sd->places     = sd->new_places;
		sd->new_places = NULL;
	}
}

/**
 * Hook called when dead node elimination replaces old by nw.
 */
static void dead_node_subst_hook(void *context, ir_graph *irg, ir_node *old, ir_node *nw) {
	survive_dce_t *sd = context;
	survive_dce_list_t *list = pmap_get(sd->places, old);
	(void) irg;

	/* If the node is to be patched back, write the new address to all registered locations. */
	if (list) {
		survive_dce_list_t *p;

		for (p = list; p; p = p->next)
			*(p->place) = nw;

		pmap_insert(sd->new_places, nw, list);
	}
}

/**
 * Make a new Survive DCE environment.
 */
survive_dce_t *new_survive_dce(void) {
	survive_dce_t *res = XMALLOC(survive_dce_t);
	obstack_init(&res->obst);
	res->places     = pmap_create();
	res->new_places = NULL;

	res->dead_node_elim.hook._hook_dead_node_elim = dead_node_hook;
	res->dead_node_elim.context                   = res;
	res->dead_node_elim.next                      = NULL;

	res->dead_node_elim_subst.hook._hook_dead_node_elim_subst = dead_node_subst_hook;
	res->dead_node_elim_subst.context = res;
	res->dead_node_elim_subst.next    = NULL;

	register_hook(hook_dead_node_elim, &res->dead_node_elim);
	register_hook(hook_dead_node_elim_subst, &res->dead_node_elim_subst);
	return res;
}

/**
 * Free a Survive DCE environment.
 */
void free_survive_dce(survive_dce_t *sd) {
	obstack_free(&sd->obst, NULL);
	pmap_destroy(sd->places);
	unregister_hook(hook_dead_node_elim, &sd->dead_node_elim);
	unregister_hook(hook_dead_node_elim_subst, &sd->dead_node_elim_subst);
	xfree(sd);
}

/**
 * Register a node pointer to be patched upon DCE.
 * When DCE occurs, the node pointer specified by @p place will be
 * patched to the new address of the node it is pointing to.
 *
 * @param sd    The Survive DCE environment.
 * @param place The address of the node pointer.
 */
void survive_dce_register_irn(survive_dce_t *sd, ir_node **place) {
	if (*place != NULL) {
		ir_node *irn      = *place;
		survive_dce_list_t *curr = pmap_get(sd->places, irn);
		survive_dce_list_t *nw   = obstack_alloc(&sd->obst, sizeof(nw[0]));

		nw->next  = curr;
		nw->place = place;

		pmap_insert(sd->places, irn, nw);
	}
}

/*--------------------------------------------------------------------*/
/*  Functionality for inlining                                         */
/*--------------------------------------------------------------------*/

/**
 * Copy node for inlineing.  Updates attributes that change when
 * inlineing but not for dead node elimination.
 *
 * Copies the node by calling copy_node() and then updates the entity if
 * it's a local one.  env must be a pointer of the frame type of the
 * inlined procedure. The new entities must be in the link field of
 * the entities.
 */
static void copy_node_inline(ir_node *n, void *env) {
	ir_node *nn;
	ir_type *frame_tp = (ir_type *)env;

	copy_node(n, NULL);
	if (is_Sel(n)) {
		nn = get_new_node (n);
		assert(is_Sel(nn));
		if (get_entity_owner(get_Sel_entity(n)) == frame_tp) {
			set_Sel_entity(nn, get_entity_link(get_Sel_entity(n)));
		}
	} else if (is_Block(n)) {
		nn = get_new_node (n);
		nn->attr.block.irg = current_ir_graph;
	}
}

/**
 * Copies new predecessors of old node and move constants to
 * the Start Block.
 */
static void copy_preds_inline(ir_node *n, void *env) {
	ir_node *nn;

	copy_preds(n, env);
	nn = skip_Id(get_new_node(n));
	if (is_irn_constlike(nn)) {
		/* move Constants into the start block */
		set_nodes_block(nn, get_irg_start_block(current_ir_graph));

		n = identify_remember(current_ir_graph->value_table, nn);
		if (nn != n) {
			DBG_OPT_CSE(nn, n);
			exchange(nn, n);
		}
	}
}

/**
 * Walker: checks if P_value_arg_base is used.
 */
static void find_addr(ir_node *node, void *env) {
	int *allow_inline = env;
	if (is_Proj(node) &&
			is_Start(get_Proj_pred(node)) &&
			get_Proj_proj(node) == pn_Start_P_value_arg_base) {
		*allow_inline = 0;
	} else if (is_Alloc(node) && get_Alloc_where(node) == stack_alloc) {
		/* From GCC:
		 * Refuse to inline alloca call unless user explicitly forced so as this
		 * may change program's memory overhead drastically when the function
		 * using alloca is called in loop.  In GCC present in SPEC2000 inlining
		 * into schedule_block cause it to require 2GB of ram instead of 256MB.
		 *
		 * Sorrily this is true with our implementation also.
		 * Moreover, we cannot differentiate between alloca() and VLA yet, so this
		 * disables inlining of functions using VLA (with are completely save).
		 *
		 * 2 Solutions:
		 * - add a flag to the Alloc node for "real" alloca() calls
		 * - add a new Stack-Restore node at the end of a function using alloca()
		 */
		*allow_inline = 0;
	}
}

/**
 * Check if we can inline a given call.
 * Currently, we cannot inline two cases:
 * - call with compound arguments
 * - graphs that take the address of a parameter
 *
 * check these conditions here
 */
static int can_inline(ir_node *call, ir_graph *called_graph) {
	ir_type *call_type = get_Call_type(call);
	int params, ress, i, res;
	assert(is_Method_type(call_type));

	params = get_method_n_params(call_type);
	ress   = get_method_n_ress(call_type);

	/* check parameters for compound arguments */
	for (i = 0; i < params; ++i) {
		ir_type *p_type = get_method_param_type(call_type, i);

		if (is_compound_type(p_type))
			return 0;
	}

	/* check results for compound arguments */
	for (i = 0; i < ress; ++i) {
		ir_type *r_type = get_method_res_type(call_type, i);

		if (is_compound_type(r_type))
			return 0;
	}

	res = 1;
	irg_walk_graph(called_graph, find_addr, NULL, &res);

	return res;
}

enum exc_mode {
	exc_handler    = 0, /**< There is a handler. */
	exc_to_end     = 1, /**< Branches to End. */
	exc_no_handler = 2  /**< Exception handling not represented. */
};

/* Inlines a method at the given call site. */
int inline_method(ir_node *call, ir_graph *called_graph) {
	ir_node             *pre_call;
	ir_node             *post_call, *post_bl;
	ir_node             *in[pn_Start_max];
	ir_node             *end, *end_bl, *block;
	ir_node             **res_pred;
	ir_node             **cf_pred;
	ir_node             **args_in;
	ir_node             *ret, *phi;
	int                 arity, n_ret, n_exc, n_res, i, n, j, rem_opt, irn_arity, n_params;
	enum exc_mode       exc_handling;
	ir_type             *called_frame, *curr_frame, *mtp, *ctp;
	ir_entity           *ent;
	ir_graph            *rem, *irg;
	irg_inline_property prop = get_irg_inline_property(called_graph);
	unsigned long       visited;

	if (prop == irg_inline_forbidden)
		return 0;

	ent = get_irg_entity(called_graph);

	mtp = get_entity_type(ent);
	ctp = get_Call_type(call);
	if (get_method_n_params(mtp) > get_method_n_params(ctp)) {
		/* this is a bad feature of C: without a prototype, we can can call a function with less
		parameters than needed. Currently we don't support this, although it would be
		to use Unknown than. */
		return 0;
	}

	/* Argh, compiling C has some bad consequences:
	   the call type AND the method type might be different.
	   It is implementation defendant what happens in that case.
	   We support inlining, if the bitsize of the types matches AND
	   the same arithmetic is used. */
	n_params = get_method_n_params(mtp);
	for (i = n_params - 1; i >= 0; --i) {
		ir_type *param_tp = get_method_param_type(mtp, i);
		ir_type *arg_tp   = get_method_param_type(ctp, i);

		if (param_tp != arg_tp) {
			ir_mode *pmode = get_type_mode(param_tp);
			ir_mode *amode = get_type_mode(arg_tp);

			if (pmode == NULL || amode == NULL)
				return 0;
			if (get_mode_size_bits(pmode) != get_mode_size_bits(amode))
				return 0;
			if (get_mode_arithmetic(pmode) != get_mode_arithmetic(amode))
				return 0;
			/* otherwise we can simply "reinterpret" the bits */
		}
	}

	irg = get_irn_irg(call);

	/*
	 * We cannot inline a recursive call. The graph must be copied before
	 * the call the inline_method() using create_irg_copy().
	 */
	if (called_graph == irg)
		return 0;

	/*
	 * currently, we cannot inline two cases:
	 * - call with compound arguments
	 * - graphs that take the address of a parameter
	 */
	if (! can_inline(call, called_graph))
		return 0;

	rem = current_ir_graph;
	current_ir_graph = irg;

	DB((dbg, LEVEL_1, "Inlining %+F(%+F) into %+F\n", call, called_graph, irg));

	/* --  Turn off optimizations, this can cause problems when allocating new nodes. -- */
	rem_opt = get_opt_optimize();
	set_optimize(0);

	/* Handle graph state */
	assert(get_irg_phase_state(irg) != phase_building);
	assert(get_irg_pinned(irg) == op_pin_state_pinned);
	assert(get_irg_pinned(called_graph) == op_pin_state_pinned);
	set_irg_outs_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);
	set_irg_callee_info_state(irg, irg_callee_info_inconsistent);

	/* -- Check preconditions -- */
	assert(is_Call(call));

	/* here we know we WILL inline, so inform the statistics */
	hook_inline(call, called_graph);

	/* -- Decide how to handle exception control flow: Is there a handler
	   for the Call node, or do we branch directly to End on an exception?
	   exc_handling:
	   0 There is a handler.
	   1 Branches to End.
	   2 Exception handling not represented in Firm. -- */
	{
		ir_node *proj, *Mproj = NULL, *Xproj = NULL;
		for (proj = get_irn_link(call); proj; proj = get_irn_link(proj)) {
			long proj_nr = get_Proj_proj(proj);
			if (proj_nr == pn_Call_X_except) Xproj = proj;
			if (proj_nr == pn_Call_M_except) Mproj = proj;
		}
		if      (Mproj) { assert(Xproj); exc_handling = exc_handler; } /*  Mproj           */
		else if (Xproj) {                exc_handling = exc_to_end; } /* !Mproj &&  Xproj   */
		else            {                exc_handling = exc_no_handler; } /* !Mproj && !Xproj   */
	}

	/* create the argument tuple */
	NEW_ARR_A(ir_type *, args_in, n_params);

	block = get_nodes_block(call);
	for (i = n_params - 1; i >= 0; --i) {
		ir_node *arg      = get_Call_param(call, i);
		ir_type *param_tp = get_method_param_type(mtp, i);
		ir_mode *mode     = get_type_mode(param_tp);

		if (mode != get_irn_mode(arg)) {
			arg = new_r_Conv(irg, block, arg, mode);
		}
		args_in[i] = arg;
	}

	/* --
	   the procedure and later replaces the Start node of the called graph.
	   Post_call is the old Call node and collects the results of the called
	   graph. Both will end up being a tuple.  -- */
	post_bl = get_nodes_block(call);
	set_irg_current_block(irg, post_bl);
	/* XxMxPxPxPxT of Start + parameter of Call */
	in[pn_Start_X_initial_exec]   = new_Jmp();
	in[pn_Start_M]                = get_Call_mem(call);
	in[pn_Start_P_frame_base]     = get_irg_frame(irg);
	in[pn_Start_P_tls]            = get_irg_tls(irg);
	in[pn_Start_T_args]           = new_Tuple(n_params, args_in);
	/* in[pn_Start_P_value_arg_base] = ??? */
	assert(pn_Start_P_value_arg_base == pn_Start_max - 1 && "pn_Start_P_value_arg_base not supported, fix");
	pre_call = new_Tuple(pn_Start_max - 1, in);
	post_call = call;

	/* --
	   The new block gets the ins of the old block, pre_call and all its
	   predecessors and all Phi nodes. -- */
	part_block(pre_call);

	/* -- Prepare state for dead node elimination -- */
	/* Visited flags in calling irg must be >= flag in called irg.
	   Else walker and arity computation will not work. */
	if (get_irg_visited(irg) <= get_irg_visited(called_graph))
		set_irg_visited(irg, get_irg_visited(called_graph) + 1);
	if (get_irg_block_visited(irg) < get_irg_block_visited(called_graph))
		set_irg_block_visited(irg, get_irg_block_visited(called_graph));
	visited = get_irg_visited(irg);

	/* Set pre_call as new Start node in link field of the start node of
	   calling graph and pre_calls block as new block for the start block
	   of calling graph.
	   Further mark these nodes so that they are not visited by the
	   copying. */
	set_irn_link(get_irg_start(called_graph), pre_call);
	set_irn_visited(get_irg_start(called_graph), visited);
	set_irn_link(get_irg_start_block(called_graph), get_nodes_block(pre_call));
	set_irn_visited(get_irg_start_block(called_graph), visited);

	set_irn_link(get_irg_bad(called_graph), get_irg_bad(current_ir_graph));
	set_irn_visited(get_irg_bad(called_graph), visited);

	set_irn_link(get_irg_no_mem(called_graph), get_irg_no_mem(current_ir_graph));
	set_irn_visited(get_irg_no_mem(called_graph), visited);

	/* Initialize for compaction of in arrays */
	inc_irg_block_visited(irg);

	/* -- Replicate local entities of the called_graph -- */
	/* copy the entities. */
	irp_reserve_resources(irp, IR_RESOURCE_ENTITY_LINK);
	called_frame = get_irg_frame_type(called_graph);
	curr_frame   = get_irg_frame_type(irg);
	for (i = 0, n = get_class_n_members(called_frame); i < n; ++i) {
		ir_entity *new_ent, *old_ent;
		old_ent = get_class_member(called_frame, i);
		new_ent = copy_entity_own(old_ent, curr_frame);
		set_entity_link(old_ent, new_ent);
	}

	/* visited is > than that of called graph.  With this trick visited will
	   remain unchanged so that an outer walker, e.g., searching the call nodes
	    to inline, calling this inline will not visit the inlined nodes. */
	set_irg_visited(irg, get_irg_visited(irg)-1);

	/* -- Performing dead node elimination inlines the graph -- */
	/* Copies the nodes to the obstack of current_ir_graph. Updates links to new
	   entities. */
	irg_walk(get_irg_end(called_graph), copy_node_inline, copy_preds_inline,
	         get_irg_frame_type(called_graph));

	irp_free_resources(irp, IR_RESOURCE_ENTITY_LINK);

	/* Repair called_graph */
	set_irg_visited(called_graph, get_irg_visited(irg));
	set_irg_block_visited(called_graph, get_irg_block_visited(irg));
	set_Block_block_visited(get_irg_start_block(called_graph), 0);

	/* -- Merge the end of the inlined procedure with the call site -- */
	/* We will turn the old Call node into a Tuple with the following
	   predecessors:
	   -1:  Block of Tuple.
	   0: Phi of all Memories of Return statements.
	   1: Jmp from new Block that merges the control flow from all exception
	   predecessors of the old end block.
	   2: Tuple of all arguments.
	   3: Phi of Exception memories.
	   In case the old Call directly branches to End on an exception we don't
	   need the block merging all exceptions nor the Phi of the exception
	   memories.
	*/

	/* -- Precompute some values -- */
	end_bl = get_new_node(get_irg_end_block(called_graph));
	end = get_new_node(get_irg_end(called_graph));
	arity = get_Block_n_cfgpreds(end_bl);    /* arity = n_exc + n_ret  */
	n_res = get_method_n_ress(get_Call_type(call));

	res_pred = XMALLOCN(ir_node*, n_res);
	cf_pred  = XMALLOCN(ir_node*, arity);

	set_irg_current_block(irg, post_bl); /* just to make sure */

	/* -- archive keepalives -- */
	irn_arity = get_irn_arity(end);
	for (i = 0; i < irn_arity; i++) {
		ir_node *ka = get_End_keepalive(end, i);
		if (! is_Bad(ka))
			add_End_keepalive(get_irg_end(irg), ka);
	}

	/* The new end node will die.  We need not free as the in array is on the obstack:
	   copy_node() only generated 'D' arrays. */

	/* -- Replace Return nodes by Jump nodes. -- */
	n_ret = 0;
	for (i = 0; i < arity; i++) {
		ir_node *ret;
		ret = get_Block_cfgpred(end_bl, i);
		if (is_Return(ret)) {
			cf_pred[n_ret] = new_r_Jmp(irg, get_nodes_block(ret));
			n_ret++;
		}
	}
	set_irn_in(post_bl, n_ret, cf_pred);

	/* -- Build a Tuple for all results of the method.
	   Add Phi node if there was more than one Return.  -- */
	turn_into_tuple(post_call, pn_Call_max);
	/* First the Memory-Phi */
	n_ret = 0;
	for (i = 0; i < arity; i++) {
		ret = get_Block_cfgpred(end_bl, i);
		if (is_Return(ret)) {
			cf_pred[n_ret] = get_Return_mem(ret);
			n_ret++;
		}
	}
	phi = new_Phi(n_ret, cf_pred, mode_M);
	set_Tuple_pred(call, pn_Call_M_regular, phi);
	/* Conserve Phi-list for further inlinings -- but might be optimized */
	if (get_nodes_block(phi) == post_bl) {
		set_irn_link(phi, get_irn_link(post_bl));
		set_irn_link(post_bl, phi);
	}
	/* Now the real results */
	if (n_res > 0) {
		for (j = 0; j < n_res; j++) {
			n_ret = 0;
			for (i = 0; i < arity; i++) {
				ret = get_Block_cfgpred(end_bl, i);
				if (is_Return(ret)) {
					cf_pred[n_ret] = get_Return_res(ret, j);
					n_ret++;
				}
			}
			if (n_ret > 0)
				phi = new_Phi(n_ret, cf_pred, get_irn_mode(cf_pred[0]));
			else
				phi = new_Bad();
			res_pred[j] = phi;
			/* Conserve Phi-list for further inlinings -- but might be optimized */
			if (get_nodes_block(phi) == post_bl) {
				set_Phi_next(phi, get_Block_phis(post_bl));
				set_Block_phis(post_bl, phi);
			}
		}
		set_Tuple_pred(call, pn_Call_T_result, new_Tuple(n_res, res_pred));
	} else {
		set_Tuple_pred(call, pn_Call_T_result, new_Bad());
	}
	/* handle the regular call */
	set_Tuple_pred(call, pn_Call_X_regular, new_Jmp());

	/* For now, we cannot inline calls with value_base */
	set_Tuple_pred(call, pn_Call_P_value_res_base, new_Bad());

	/* Finally the exception control flow.
	   We have two (three) possible situations:
	   First if the Call branches to an exception handler: We need to add a Phi node to
	   collect the memory containing the exception objects.  Further we need
	   to add another block to get a correct representation of this Phi.  To
	   this block we add a Jmp that resolves into the X output of the Call
	   when the Call is turned into a tuple.
	   Second the Call branches to End, the exception is not handled.  Just
	   add all inlined exception branches to the End node.
	   Third: there is no Exception edge at all. Handle as case two. */
	if (exc_handling == exc_handler) {
		n_exc = 0;
		for (i = 0; i < arity; i++) {
			ir_node *ret, *irn;
			ret = get_Block_cfgpred(end_bl, i);
			irn = skip_Proj(ret);
			if (is_fragile_op(irn) || is_Raise(irn)) {
				cf_pred[n_exc] = ret;
				++n_exc;
			}
		}
		if (n_exc > 0) {
			new_Block(n_exc, cf_pred);      /* watch it: current_block is changed! */
			set_Tuple_pred(call, pn_Call_X_except, new_Jmp());
			/* The Phi for the memories with the exception objects */
			n_exc = 0;
			for (i = 0; i < arity; i++) {
				ir_node *ret;
				ret = skip_Proj(get_Block_cfgpred(end_bl, i));
				if (is_Call(ret)) {
					cf_pred[n_exc] = new_r_Proj(irg, get_nodes_block(ret), ret, mode_M, 3);
					n_exc++;
				} else if (is_fragile_op(ret)) {
					/* We rely that all cfops have the memory output at the same position. */
					cf_pred[n_exc] = new_r_Proj(irg, get_nodes_block(ret), ret, mode_M, 0);
					n_exc++;
				} else if (is_Raise(ret)) {
					cf_pred[n_exc] = new_r_Proj(irg, get_nodes_block(ret), ret, mode_M, 1);
					n_exc++;
				}
			}
			set_Tuple_pred(call, pn_Call_M_except, new_Phi(n_exc, cf_pred, mode_M));
		} else {
			set_Tuple_pred(call, pn_Call_X_except, new_Bad());
			set_Tuple_pred(call, pn_Call_M_except, new_Bad());
		}
	} else {
		ir_node *main_end_bl;
		int main_end_bl_arity;
		ir_node **end_preds;

		/* assert(exc_handling == 1 || no exceptions. ) */
		n_exc = 0;
		for (i = 0; i < arity; i++) {
			ir_node *ret = get_Block_cfgpred(end_bl, i);
			ir_node *irn = skip_Proj(ret);

			if (is_fragile_op(irn) || is_Raise(irn)) {
				cf_pred[n_exc] = ret;
				n_exc++;
			}
		}
		main_end_bl       = get_irg_end_block(irg);
		main_end_bl_arity = get_irn_arity(main_end_bl);
		end_preds         = XMALLOCN(ir_node*, n_exc + main_end_bl_arity);

		for (i = 0; i < main_end_bl_arity; ++i)
			end_preds[i] = get_irn_n(main_end_bl, i);
		for (i = 0; i < n_exc; ++i)
			end_preds[main_end_bl_arity + i] = cf_pred[i];
		set_irn_in(main_end_bl, n_exc + main_end_bl_arity, end_preds);
		set_Tuple_pred(call, pn_Call_X_except,  new_Bad());
		set_Tuple_pred(call, pn_Call_M_except,  new_Bad());
		free(end_preds);
	}
	free(res_pred);
	free(cf_pred);

	/* --  Turn CSE back on. -- */
	set_optimize(rem_opt);
	current_ir_graph = rem;

	return 1;
}

/********************************************************************/
/* Apply inlining to small methods.                                 */
/********************************************************************/

static struct obstack  temp_obst;

/** Represents a possible inlinable call in a graph. */
typedef struct _call_entry {
	ir_node    *call;       /**< The Call node. */
	ir_graph   *callee;     /**< The callee IR-graph. */
	list_head  list;        /**< List head for linking the next one. */
	int        loop_depth;  /**< The loop depth of this call. */
	int        benefice;    /**< The calculated benefice of this call. */
	unsigned   local_adr:1; /**< Set if this call gets an address of a local variable. */
	unsigned   all_const:1; /**< Set if this call has only constant parameters. */
} call_entry;

/**
 * environment for inlining small irgs
 */
typedef struct _inline_env_t {
	struct obstack obst;  /**< An obstack where call_entries are allocated on. */
	list_head      calls; /**< The call entry list. */
} inline_env_t;

/**
 * Returns the irg called from a Call node. If the irg is not
 * known, NULL is returned.
 *
 * @param call  the call node
 */
static ir_graph *get_call_called_irg(ir_node *call) {
	ir_node *addr;

	addr = get_Call_ptr(call);
	if (is_Global(addr)) {
		ir_entity *ent = get_Global_entity(addr);
		return get_entity_irg(ent);
	}

	return NULL;
}

/**
 * Walker: Collect all calls to known graphs inside a graph.
 */
static void collect_calls(ir_node *call, void *env) {
	(void) env;
	if (is_Call(call)) {
		ir_graph *called_irg = get_call_called_irg(call);

		if (called_irg != NULL) {
			/* The Call node calls a locally defined method.  Remember to inline. */
			inline_env_t *ienv  = env;
			call_entry   *entry = obstack_alloc(&ienv->obst, sizeof(*entry));
			entry->call       = call;
			entry->callee     = called_irg;
			entry->loop_depth = 0;
			entry->benefice   = 0;
			entry->local_adr  = 0;
			entry->all_const  = 0;

			list_add_tail(&entry->list, &ienv->calls);
		}
	}
}

/**
 * Inlines all small methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_small_irgs(ir_graph *irg, int size) {
	ir_graph *rem = current_ir_graph;
	inline_env_t env;
	call_entry *entry;

	current_ir_graph = irg;
	/* Handle graph state */
	assert(get_irg_phase_state(irg) != phase_building);
	free_callee_info(irg);

	/* Find Call nodes to inline.
	   (We can not inline during a walk of the graph, as inlining the same
	   method several times changes the visited flag of the walked graph:
	   after the first inlining visited of the callee equals visited of
	   the caller.  With the next inlining both are increased.) */
	obstack_init(&env.obst);
	INIT_LIST_HEAD(&env.calls);
	irg_walk_graph(irg, NULL, collect_calls, &env);

	if (! list_empty(&env.calls)) {
		/* There are calls to inline */
		collect_phiprojs(irg);

		list_for_each_entry(call_entry, entry, &env.calls, list) {
			ir_graph            *callee = entry->callee;
			irg_inline_property prop    = get_irg_inline_property(callee);

			if (prop == irg_inline_forbidden || get_irg_additional_properties(callee) & mtp_property_weak) {
				/* do not inline forbidden / weak graphs */
				continue;
			}

			if (prop >= irg_inline_forced ||
			    _obstack_memory_used(callee->obst) - (int)obstack_room(callee->obst) < size) {
				inline_method(entry->call, callee);
			}
		}
	}
	obstack_free(&env.obst, NULL);
	current_ir_graph = rem;
}

/**
 * Environment for inlining irgs.
 */
typedef struct {
	list_head calls;             /**< List of of all call nodes in this graph. */
	unsigned  *local_weights;    /**< Once allocated, the beneficial weight for transmitting local addresses. */
	unsigned  n_nodes;           /**< Number of nodes in graph except Id, Tuple, Proj, Start, End. */
	unsigned  n_blocks;          /**< Number of Blocks in graph without Start and End block. */
	unsigned  n_nodes_orig;      /**< for statistics */
	unsigned  n_call_nodes;      /**< Number of Call nodes in the graph. */
	unsigned  n_call_nodes_orig; /**< for statistics */
	unsigned  n_callers;         /**< Number of known graphs that call this graphs. */
	unsigned  n_callers_orig;    /**< for statistics */
	unsigned  got_inline:1;      /**< Set, if at least one call inside this graph was inlined. */
	unsigned  local_vars:1;      /**< Set, if an inlined function got the address of a local variable. */
	unsigned  recursive:1;       /**< Set, if this function is self recursive. */
} inline_irg_env;

/**
 * Allocate a new environment for inlining.
 */
static inline_irg_env *alloc_inline_irg_env(void) {
	inline_irg_env *env    = obstack_alloc(&temp_obst, sizeof(*env));
	INIT_LIST_HEAD(&env->calls);
	env->local_weights     = NULL;
	env->n_nodes           = -2; /* do not count count Start, End */
	env->n_blocks          = -2; /* do not count count Start, End Block */
	env->n_nodes_orig      = -2; /* do not count Start, End */
	env->n_call_nodes      = 0;
	env->n_call_nodes_orig = 0;
	env->n_callers         = 0;
	env->n_callers_orig    = 0;
	env->got_inline        = 0;
	env->local_vars        = 0;
	env->recursive         = 0;
	return env;
}

typedef struct walker_env {
	inline_irg_env *x;     /**< the inline environment */
	char ignore_runtime;   /**< the ignore runtime flag */
	char ignore_callers;   /**< if set, do change callers data */
} wenv_t;

/**
 * post-walker: collect all calls in the inline-environment
 * of a graph and sum some statistics.
 */
static void collect_calls2(ir_node *call, void *ctx) {
	wenv_t         *env = ctx;
	inline_irg_env *x = env->x;
	ir_opcode      code = get_irn_opcode(call);
	ir_graph       *callee;
	call_entry     *entry;

	/* count meaningful nodes in irg */
	if (code != iro_Proj && code != iro_Tuple && code != iro_Sync) {
		if (code != iro_Block) {
			++x->n_nodes;
			++x->n_nodes_orig;
		} else {
			++x->n_blocks;
		}
	}

	if (code != iro_Call) return;

	/* check, if it's a runtime call */
	if (env->ignore_runtime) {
		ir_node *symc = get_Call_ptr(call);

		if (is_Global(symc)) {
			ir_entity *ent = get_Global_entity(symc);

			if (get_entity_additional_properties(ent) & mtp_property_runtime)
				return;
		}
	}

	/* collect all call nodes */
	++x->n_call_nodes;
	++x->n_call_nodes_orig;

	callee = get_call_called_irg(call);
	if (callee != NULL) {
		if (! env->ignore_callers) {
			inline_irg_env *callee_env = get_irg_link(callee);
			/* count all static callers */
			++callee_env->n_callers;
			++callee_env->n_callers_orig;
		}
		if (callee == current_ir_graph)
			x->recursive = 1;

		/* link it in the list of possible inlinable entries */
		entry = obstack_alloc(&temp_obst, sizeof(*entry));
		entry->call       = call;
		entry->callee     = callee;
		entry->loop_depth = get_irn_loop(get_nodes_block(call))->depth;
		entry->benefice   = 0;
		entry->local_adr  = 0;
		entry->all_const  = 0;

		list_add_tail(&entry->list, &x->calls);
	}
}

/**
 * Returns TRUE if the number of callers is 0 in the irg's environment,
 * hence this irg is a leave.
 */
inline static int is_leave(ir_graph *irg) {
	inline_irg_env *env = get_irg_link(irg);
	return env->n_call_nodes == 0;
}

/**
 * Returns TRUE if the number of nodes in the callee is
 * smaller then size in the irg's environment.
 */
inline static int is_smaller(ir_graph *callee, unsigned size) {
	inline_irg_env *env = get_irg_link(callee);
	return env->n_nodes < size;
}

/**
 * Duplicate a call entry.
 *
 * @param entry     the original entry to duplicate
 * @param new_call  the new call node
 * @param loop_depth_delta
 *                  delta value for the loop depth
 */
static call_entry *duplicate_call_entry(const call_entry *entry,
                                        ir_node *new_call, int loop_depth_delta) {
	call_entry *nentry = obstack_alloc(&temp_obst, sizeof(*nentry));
	nentry->call       = new_call;
	nentry->callee     = entry->callee;
	nentry->benefice   = entry->benefice;
	nentry->loop_depth = entry->loop_depth + loop_depth_delta;
	nentry->local_adr  = entry->local_adr;
	nentry->all_const  = entry->all_const;

	return nentry;
}

/**
 * Append all call nodes of the source environment to the nodes of in the destination
 * environment.
 *
 * @param dst         destination environment
 * @param src         source environment
 * @param loop_depth  the loop depth of the call that is replaced by the src list
 */
static void append_call_list(inline_irg_env *dst, inline_irg_env *src, int loop_depth) {
	call_entry *entry, *nentry;

	/* Note that the src list points to Call nodes in the inlined graph, but
	   we need Call nodes in our graph. Luckily the inliner leaves this information
	   in the link field. */
	list_for_each_entry(call_entry, entry, &src->calls, list) {
		nentry = duplicate_call_entry(entry, get_irn_link(entry->call), loop_depth);
		list_add_tail(&nentry->list, &dst->calls);
	}
	dst->n_call_nodes += src->n_call_nodes;
	dst->n_nodes      += src->n_nodes;
}

/*
 * Inlines small leave methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_leave_functions(unsigned maxsize, unsigned leavesize,
                            unsigned size, int ignore_runtime)
{
	inline_irg_env   *env;
	ir_graph         *irg;
	int              i, n_irgs;
	ir_graph         *rem;
	int              did_inline;
	wenv_t           wenv;
	call_entry       *entry, *next;
	const call_entry *centry;
	pmap             *copied_graphs;
	pmap_entry       *pm_entry;

	rem = current_ir_graph;
	obstack_init(&temp_obst);

	/* a map for the copied graphs, used to inline recursive calls */
	copied_graphs = pmap_create();

	/* extend all irgs by a temporary data structure for inlining. */
	n_irgs = get_irp_n_irgs();
	for (i = 0; i < n_irgs; ++i)
		set_irg_link(get_irp_irg(i), alloc_inline_irg_env());

	/* Pre-compute information in temporary data structure. */
	wenv.ignore_runtime = ignore_runtime;
	wenv.ignore_callers = 0;
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);

		assert(get_irg_phase_state(irg) != phase_building);
		free_callee_info(irg);

		assure_cf_loop(irg);
		wenv.x = get_irg_link(irg);
		irg_walk_graph(irg, NULL, collect_calls2, &wenv);
	}

	/* -- and now inline. -- */

	/* Inline leaves recursively -- we might construct new leaves. */
	do {
		did_inline = 0;

		for (i = 0; i < n_irgs; ++i) {
			ir_node *call;
			int phiproj_computed = 0;

			current_ir_graph = get_irp_irg(i);
			env              = get_irg_link(current_ir_graph);

			list_for_each_entry_safe(call_entry, entry, next, &env->calls, list) {
				ir_graph            *callee;
				irg_inline_property  prop;

				if (env->n_nodes > maxsize)
					break;

				call   = entry->call;
				callee = entry->callee;

				prop = get_irg_inline_property(callee);
				if (prop == irg_inline_forbidden || get_irg_additional_properties(callee) & mtp_property_weak) {
					/* do not inline forbidden / weak graphs */
					continue;
				}

				if (is_leave(callee) && (
				    is_smaller(callee, leavesize) || prop >= irg_inline_forced)) {
					if (!phiproj_computed) {
						phiproj_computed = 1;
						collect_phiprojs(current_ir_graph);
					}
					did_inline = inline_method(call, callee);

					if (did_inline) {
						inline_irg_env *callee_env = get_irg_link(callee);

						/* call was inlined, Phi/Projs for current graph must be recomputed */
						phiproj_computed = 0;

						/* Do some statistics */
						env->got_inline = 1;
						--env->n_call_nodes;
						env->n_nodes += callee_env->n_nodes;
						--callee_env->n_callers;

						/* remove this call from the list */
						list_del(&entry->list);
						continue;
					}
				}
			}
		}
	} while (did_inline);

	/* inline other small functions. */
	for (i = 0; i < n_irgs; ++i) {
		ir_node *call;
		int phiproj_computed = 0;

		current_ir_graph = get_irp_irg(i);
		env              = get_irg_link(current_ir_graph);

		/* note that the list of possible calls is updated during the process */
		list_for_each_entry_safe(call_entry, entry, next, &env->calls, list) {
			irg_inline_property prop;
			ir_graph            *callee;
			pmap_entry          *e;

			call   = entry->call;
			callee = entry->callee;

			prop = get_irg_inline_property(callee);
			if (prop == irg_inline_forbidden || get_irg_additional_properties(callee) & mtp_property_weak) {
				/* do not inline forbidden / weak graphs */
				continue;
			}

			e = pmap_find(copied_graphs, callee);
			if (e != NULL) {
				/*
				 * Remap callee if we have a copy.
				 * FIXME: Should we do this only for recursive Calls ?
				 */
				callee = e->value;
			}

			if (prop >= irg_inline_forced ||
			    (is_smaller(callee, size) && env->n_nodes < maxsize) /* small function */) {
				if (current_ir_graph == callee) {
					/*
					 * Recursive call: we cannot directly inline because we cannot walk
					 * the graph and change it. So we have to make a copy of the graph
					 * first.
					 */

					inline_irg_env *callee_env;
					ir_graph       *copy;

					/*
					 * No copy yet, create one.
					 * Note that recursive methods are never leaves, so it is sufficient
					 * to test this condition here.
					 */
					copy = create_irg_copy(callee);

					/* create_irg_copy() destroys the Proj links, recompute them */
					phiproj_computed = 0;

					/* allocate new environment */
					callee_env = alloc_inline_irg_env();
					set_irg_link(copy, callee_env);

					assure_cf_loop(copy);
					wenv.x              = callee_env;
					wenv.ignore_callers = 1;
					irg_walk_graph(copy, NULL, collect_calls2, &wenv);

					/*
					 * Enter the entity of the original graph. This is needed
					 * for inline_method(). However, note that ent->irg still points
					 * to callee, NOT to copy.
					 */
					set_irg_entity(copy, get_irg_entity(callee));

					pmap_insert(copied_graphs, callee, copy);
					callee = copy;

					/* we have only one caller: the original graph */
					callee_env->n_callers      = 1;
					callee_env->n_callers_orig = 1;
				}
				if (! phiproj_computed) {
					phiproj_computed = 1;
					collect_phiprojs(current_ir_graph);
				}
				did_inline = inline_method(call, callee);
				if (did_inline) {
					inline_irg_env *callee_env = (inline_irg_env *)get_irg_link(callee);

					/* call was inlined, Phi/Projs for current graph must be recomputed */
					phiproj_computed = 0;

					/* callee was inline. Append it's call list. */
					env->got_inline = 1;
					--env->n_call_nodes;
					append_call_list(env, callee_env, entry->loop_depth);
					--callee_env->n_callers;

					/* after we have inlined callee, all called methods inside callee
					   are now called once more */
					list_for_each_entry(call_entry, centry, &callee_env->calls, list) {
						inline_irg_env *penv = get_irg_link(centry->callee);
						++penv->n_callers;
					}

					/* remove this call from the list */
					list_del(&entry->list);
					continue;
				}
			}
		}
	}

	for (i = 0; i < n_irgs; ++i) {
		irg = get_irp_irg(i);
		env = get_irg_link(irg);

		if (env->got_inline) {
			optimize_graph_df(irg);
			optimize_cf(irg);
		}
		if (env->got_inline || (env->n_callers_orig != env->n_callers)) {
			DB((dbg, LEVEL_1, "Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
			env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
			env->n_callers_orig, env->n_callers,
			get_entity_name(get_irg_entity(irg))));
		}
	}

	/* kill the copied graphs: we don't need them anymore */
	foreach_pmap(copied_graphs, pm_entry) {
		ir_graph *copy = pm_entry->value;

		/* reset the entity, otherwise it will be deleted in the next step ... */
		set_irg_entity(copy, NULL);
		free_ir_graph(copy);
	}
	pmap_destroy(copied_graphs);

	obstack_free(&temp_obst, NULL);
	current_ir_graph = rem;
}

/**
 * Calculate the parameter weights for transmitting the address of a local variable.
 */
static unsigned calc_method_local_weight(ir_node *arg) {
	int      i, j, k;
	unsigned v, weight = 0;

	for (i = get_irn_n_outs(arg) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(arg, i);

		switch (get_irn_opcode(succ)) {
		case iro_Load:
		case iro_Store:
			/* Loads and Store can be removed */
			weight += 3;
			break;
		case iro_Sel:
			/* check if all args are constant */
			for (j = get_Sel_n_indexs(succ) - 1; j >= 0; --j) {
				ir_node *idx = get_Sel_index(succ, j);
				if (! is_Const(idx))
					return 0;
			}
			/* Check users on this Sel. Note: if a 0 is returned here, there was
			   some unsupported node. */
			v = calc_method_local_weight(succ);
			if (v == 0)
				return 0;
			/* we can kill one Sel with constant indexes, this is cheap */
			weight += v + 1;
			break;
		case iro_Id:
			/* when looking backward we might find Id nodes */
			weight += calc_method_local_weight(succ);
			break;
		case iro_Tuple:
			/* unoptimized tuple */
			for (j = get_Tuple_n_preds(succ) - 1; j >= 0; --j) {
				ir_node *pred = get_Tuple_pred(succ, j);
				if (pred == arg) {
					/* look for Proj(j) */
					for (k = get_irn_n_outs(succ) - 1; k >= 0; --k) {
						ir_node *succ_succ = get_irn_out(succ, k);
						if (is_Proj(succ_succ)) {
							if (get_Proj_proj(succ_succ) == j) {
								/* found */
								weight += calc_method_local_weight(succ_succ);
							}
						} else {
							/* this should NOT happen */
							return 0;
						}
					}
				}
			}
			break;
		default:
			/* any other node: unsupported yet or bad. */
			return 0;
		}
	}
	return weight;
}

/**
 * Calculate the parameter weights for transmitting the address of a local variable.
 */
static void analyze_irg_local_weights(inline_irg_env *env, ir_graph *irg) {
	ir_entity *ent = get_irg_entity(irg);
	ir_type  *mtp;
	int      nparams, i, proj_nr;
	ir_node  *irg_args, *arg;

	mtp      = get_entity_type(ent);
	nparams  = get_method_n_params(mtp);

	/* allocate a new array. currently used as 'analysed' flag */
	env->local_weights = NEW_ARR_D(unsigned, &temp_obst, nparams);

	/* If the method haven't parameters we have nothing to do. */
	if (nparams <= 0)
		return;

	assure_irg_outs(irg);
	irg_args = get_irg_args(irg);
	for (i = get_irn_n_outs(irg_args) - 1; i >= 0; --i) {
		arg     = get_irn_out(irg_args, i);
		proj_nr = get_Proj_proj(arg);
		env->local_weights[proj_nr] = calc_method_local_weight(arg);
	}
}

/**
 * Calculate the benefice for transmitting an local variable address.
 * After inlining, the local variable might be transformed into a
 * SSA variable by scalar_replacement().
 */
static unsigned get_method_local_adress_weight(ir_graph *callee, int pos) {
	inline_irg_env *env = get_irg_link(callee);

	if (env->local_weights != NULL) {
		if (pos < ARR_LEN(env->local_weights))
			return env->local_weights[pos];
		return 0;
	}

	analyze_irg_local_weights(env, callee);

	if (pos < ARR_LEN(env->local_weights))
		return env->local_weights[pos];
	return 0;
}

/**
 * Calculate a benefice value for inlining the given call.
 *
 * @param call       the call node we have to inspect
 * @param callee     the called graph
 */
static int calc_inline_benefice(call_entry *entry, ir_graph *callee)
{
	ir_node   *call = entry->call;
	ir_entity *ent  = get_irg_entity(callee);
	ir_node   *frame_ptr;
	ir_type   *mtp;
	int       weight = 0;
	int       i, n_params, all_const;
	unsigned  cc, v;
	irg_inline_property prop;

	inline_irg_env *callee_env;

	prop = get_irg_inline_property(callee);
	if (prop == irg_inline_forbidden) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: inlining forbidden\n",
		    call, callee));
		return entry->benefice = INT_MIN;
	}

	if (get_irg_additional_properties(callee) & (mtp_property_noreturn | mtp_property_weak)) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: not inlining noreturn or weak\n",
		    call, callee));
		return entry->benefice = INT_MIN;
	}

	/* costs for every passed parameter */
	n_params = get_Call_n_params(call);
	mtp      = get_entity_type(ent);
	cc       = get_method_calling_convention(mtp);
	if (cc & cc_reg_param) {
		/* register parameter, smaller costs for register parameters */
		int max_regs = cc & ~cc_bits;

		if (max_regs < n_params)
			weight += max_regs * 2 + (n_params - max_regs) * 5;
		else
			weight += n_params * 2;
	} else {
		/* parameters are passed an stack */
		weight += 5 * n_params;
	}

	/* constant parameters improve the benefice */
	frame_ptr = get_irg_frame(current_ir_graph);
	all_const = 1;
	for (i = 0; i < n_params; ++i) {
		ir_node *param = get_Call_param(call, i);

		if (is_Const(param)) {
			weight += get_method_param_weight(ent, i);
		} else {
			all_const = 0;
			if (is_SymConst(param))
				weight += get_method_param_weight(ent, i);
			else if (is_Sel(param) && get_Sel_ptr(param) == frame_ptr) {
				/*
				 * An address of a local variable is transmitted. After
				 * inlining, scalar_replacement might be able to remove the
				 * local variable, so honor this.
				 */
				v = get_method_local_adress_weight(callee, i);
				weight += v;
				if (v > 0)
					entry->local_adr = 1;
			}
		}
	}
	entry->all_const = all_const;

	callee_env = get_irg_link(callee);
	if (callee_env->n_callers == 1 &&
	    callee != current_ir_graph &&
		get_entity_visibility(ent) == visibility_local) {
		weight += 700;
	}

	/* give a bonus for functions with one block */
	if (callee_env->n_blocks == 1)
		weight = weight * 3 / 2;

	/* and one for small non-recursive functions: we want them to be inlined in mostly every case */
	if (callee_env->n_nodes < 30 && !callee_env->recursive)
		weight += 2000;

	/* and finally for leaves: they do not increase the register pressure
	   because of callee safe registers */
	if (callee_env->n_call_nodes == 0)
		weight += 400;

	/** it's important to inline inner loops first */
	if (entry->loop_depth > 30)
		weight += 30 * 1024;
	else
		weight += entry->loop_depth * 1024;

	/*
	 * All arguments constant is probably a good sign, give an extra bonus
	 */
	if (all_const)
		weight += 1024;

	return entry->benefice = weight;
}

static ir_graph **irgs;
static int      last_irg;

/**
 * Callgraph walker, collect all visited graphs.
 */
static void callgraph_walker(ir_graph *irg, void *data) {
	(void) data;
	irgs[last_irg++] = irg;
}

/**
 * Creates an inline order for all graphs.
 *
 * @return the list of graphs.
 */
static ir_graph **create_irg_list(void) {
	ir_entity **free_methods;
	int       arr_len;
	int       n_irgs = get_irp_n_irgs();

	cgana(&arr_len, &free_methods);
	xfree(free_methods);

	compute_callgraph();

	last_irg = 0;
	irgs     = XMALLOCNZ(ir_graph*, n_irgs);

	callgraph_walk(NULL, callgraph_walker, NULL);
	assert(n_irgs == last_irg);

	return irgs;
}

/**
 * Push a call onto the priority list if its benefice is big enough.
 *
 * @param pqueue   the priority queue of calls
 * @param call     the call entry
 * @param inlien_threshold
 *                 the threshold value
 */
static void maybe_push_call(pqueue_t *pqueue, call_entry *call,
                            int inline_threshold)
{
	ir_graph            *callee  = call->callee;
	irg_inline_property prop     = get_irg_inline_property(callee);
	int                 benefice = calc_inline_benefice(call, callee);

	DB((dbg, LEVEL_2, "In %+F Call %+F to %+F has benefice %d\n",
	    get_irn_irg(call->call), call->call, callee, benefice));

	if (prop < irg_inline_forced && benefice < inline_threshold) {
		return;
	}

	pqueue_put(pqueue, call, benefice);
}

/**
 * Try to inline calls into a graph.
 *
 * @param irg      the graph into which we inline
 * @param maxsize  do NOT inline if the size of irg gets
 *                 bigger than this amount
 * @param inline_threshold
 *                 threshold value for inline decision
 * @param copied_graphs
 *                 map containing copied of recursive graphs
 */
static void inline_into(ir_graph *irg, unsigned maxsize,
                        int inline_threshold, pmap *copied_graphs)
{
	int            phiproj_computed = 0;
	inline_irg_env *env = get_irg_link(irg);
	call_entry     *curr_call;
	wenv_t         wenv;
	pqueue_t       *pqueue;

	if (env->n_call_nodes == 0)
		return;

	if (env->n_nodes > maxsize) {
		DB((dbg, LEVEL_2, "%+F: too big (%d)\n", irg, env->n_nodes));
		return;
	}

	current_ir_graph = irg;

	/* put irgs into the pqueue */
	pqueue = new_pqueue();

	list_for_each_entry(call_entry, curr_call, &env->calls, list) {
		assert(is_Call(curr_call->call));
		maybe_push_call(pqueue, curr_call, inline_threshold);
	}

	/* note that the list of possible calls is updated during the process */
	while (!pqueue_empty(pqueue)) {
		int                 did_inline;
		call_entry          *curr_call  = pqueue_pop_front(pqueue);
		ir_graph            *callee     = curr_call->callee;
		ir_node             *call_node  = curr_call->call;
		inline_irg_env      *callee_env = get_irg_link(callee);
		irg_inline_property prop        = get_irg_inline_property(callee);
		int                 loop_depth;
		const call_entry    *centry;
		pmap_entry          *e;

		if ((prop < irg_inline_forced) && env->n_nodes + callee_env->n_nodes > maxsize) {
			DB((dbg, LEVEL_2, "%+F: too big (%d) + %+F (%d)\n", irg,
						env->n_nodes, callee, callee_env->n_nodes));
			continue;
		}

		e = pmap_find(copied_graphs, callee);
		if (e != NULL) {
			int benefice = curr_call->benefice;
			/*
			 * Reduce the weight for recursive function IFF not all arguments are const.
			 * inlining recursive functions is rarely good.
			 */
			if (!curr_call->all_const)
				benefice -= 2000;
			if (benefice < inline_threshold)
				continue;

			/*
			 * Remap callee if we have a copy.
			 */
			callee     = e->value;
			callee_env = get_irg_link(callee);
		}

		if (current_ir_graph == callee) {
			/*
			 * Recursive call: we cannot directly inline because we cannot
			 * walk the graph and change it. So we have to make a copy of
			 * the graph first.
			 */
			int benefice = curr_call->benefice;
			ir_graph *copy;

			/*
			 * Reduce the weight for recursive function IFF not all arguments are const.
			 * inlining recursive functions is rarely good.
			 */
			if (!curr_call->all_const)
				benefice -= 2000;
			if (benefice < inline_threshold)
				continue;

			/*
			 * No copy yet, create one.
			 * Note that recursive methods are never leaves, so it is
			 * sufficient to test this condition here.
			 */
			copy = create_irg_copy(callee);

			/* create_irg_copy() destroys the Proj links, recompute them */
			phiproj_computed = 0;

			/* allocate a new environment */
			callee_env = alloc_inline_irg_env();
			set_irg_link(copy, callee_env);

			assure_cf_loop(copy);
			wenv.x              = callee_env;
			wenv.ignore_callers = 1;
			irg_walk_graph(copy, NULL, collect_calls2, &wenv);

			/*
			 * Enter the entity of the original graph. This is needed
			 * for inline_method(). However, note that ent->irg still points
			 * to callee, NOT to copy.
			 */
			set_irg_entity(copy, get_irg_entity(callee));

			pmap_insert(copied_graphs, callee, copy);
			callee = copy;

			/* we have only one caller: the original graph */
			callee_env->n_callers      = 1;
			callee_env->n_callers_orig = 1;
		}
		if (! phiproj_computed) {
			phiproj_computed = 1;
			collect_phiprojs(current_ir_graph);
		}
		did_inline = inline_method(call_node, callee);
		if (!did_inline)
			continue;

		/* call was inlined, Phi/Projs for current graph must be recomputed */
		phiproj_computed = 0;

		/* remove it from the caller list */
		list_del(&curr_call->list);

		/* callee was inline. Append it's call list. */
		env->got_inline = 1;
		if (curr_call->local_adr)
			env->local_vars = 1;
		--env->n_call_nodes;

		/* we just generate a bunch of new calls */
		loop_depth = curr_call->loop_depth;
		list_for_each_entry(call_entry, centry, &callee_env->calls, list) {
			inline_irg_env *penv = get_irg_link(centry->callee);
			ir_node        *new_call;
			call_entry     *new_entry;

			/* after we have inlined callee, all called methods inside
			 * callee are now called once more */
			++penv->n_callers;

			/* Note that the src list points to Call nodes in the inlined graph,
			 * but we need Call nodes in our graph. Luckily the inliner leaves
			 * this information in the link field. */
			new_call = get_irn_link(centry->call);
			assert(is_Call(new_call));

			new_entry = duplicate_call_entry(centry, new_call, loop_depth);
			list_add_tail(&new_entry->list, &env->calls);
			maybe_push_call(pqueue, new_entry, inline_threshold);
		}

		env->n_call_nodes += callee_env->n_call_nodes;
		env->n_nodes += callee_env->n_nodes;
		--callee_env->n_callers;
	}

	del_pqueue(pqueue);
}

/*
 * Heuristic inliner. Calculates a benefice value for every call and inlines
 * those calls with a value higher than the threshold.
 */
void inline_functions(unsigned maxsize, int inline_threshold) {
	inline_irg_env   *env;
	int              i, n_irgs;
	ir_graph         *rem;
	wenv_t           wenv;
	pmap             *copied_graphs;
	pmap_entry       *pm_entry;
	ir_graph         **irgs;

	rem = current_ir_graph;
	obstack_init(&temp_obst);

	irgs = create_irg_list();

	/* a map for the copied graphs, used to inline recursive calls */
	copied_graphs = pmap_create();

	/* extend all irgs by a temporary data structure for inlining. */
	n_irgs = get_irp_n_irgs();
	for (i = 0; i < n_irgs; ++i)
		set_irg_link(irgs[i], alloc_inline_irg_env());

	/* Pre-compute information in temporary data structure. */
	wenv.ignore_runtime = 0;
	wenv.ignore_callers = 0;
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		free_callee_info(irg);

		wenv.x = get_irg_link(irg);
		assure_cf_loop(irg);
		irg_walk_graph(irg, NULL, collect_calls2, &wenv);
	}

	/* -- and now inline. -- */
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		inline_into(irg, maxsize, inline_threshold, copied_graphs);
	}

	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		env = get_irg_link(irg);
		if (env->got_inline) {
			/* this irg got calls inlined: optimize it */
			if (get_opt_combo()) {
				if (env->local_vars) {
					scalar_replacement_opt(irg);
				}
				combo(irg);
			} else {
				if (env->local_vars) {
					if (scalar_replacement_opt(irg)) {
						optimize_graph_df(irg);
					}
				}
				optimize_cf(irg);
			}
		}
		if (env->got_inline || (env->n_callers_orig != env->n_callers)) {
			DB((dbg, LEVEL_1, "Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
			env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
			env->n_callers_orig, env->n_callers,
			get_entity_name(get_irg_entity(irg))));
		}
	}

	/* kill the copied graphs: we don't need them anymore */
	foreach_pmap(copied_graphs, pm_entry) {
		ir_graph *copy = pm_entry->value;

		/* reset the entity, otherwise it will be deleted in the next step ... */
		set_irg_entity(copy, NULL);
		free_ir_graph(copy);
	}
	pmap_destroy(copied_graphs);

	xfree(irgs);

	obstack_free(&temp_obst, NULL);
	current_ir_graph = rem;
}

void firm_init_inline(void) {
	FIRM_DBG_REGISTER(dbg, "firm.opt.inline");
}
