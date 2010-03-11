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
 * @brief    Dead node elimination
 * @author   Michael Beck, Goetz Lindenmaier
 * @version  $Id: opt_inline.c 27265 2010-03-07 15:13:00Z matze $
 *
 * Strictly speaking dead node elimination is unnecessary in firm - everthying
 * which is not used can't be found by any walker.
 * The only drawback is that the nodes still take up memory. This phase fixes
 * this by copying all (reachable) nodes to a new obstack and throwing away
 * the old one.
 */
#include "config.h"

#include "iroptimize.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irphase_t.h"
#include "iredges_t.h"
#include "irhooks.h"
#include "irtools.h"
#include "irgwalk.h"
#include "cgana.h"
#include "irouts.h"
#include "trouts.h"
#include "iropt_t.h"
#include "irpass.h"
#include "pmap.h"

/** a pointer to the new phases */
static ir_phase *new_phases[PHASE_LAST + 1];

/**
 * Reroute the inputs of a node from nodes in the old graph to copied nodes in
 * the new graph
 */
static void rewire_inputs(ir_node *node, void *env)
{
	(void) env;
	irn_rewire_inputs(node);
}

static void copy_node_dce(ir_node *node, void *env)
{
	int       i;
	ir_node  *new_node = exact_copy(node);
	ir_graph *irg      = get_irn_irg(new_node);
	(void) env;

	/* preserve the node numbers for easier debugging */
	new_node->node_nr = node->node_nr;

	/* copy phase information for this node */
	for (i = 0; i <= PHASE_LAST; i++) {
		ir_phase *phase = irg_get_phase(irg, i);
		if (phase == NULL)
			continue;
		if (!phase_get_irn_data(phase, node))
			continue;
		phase_set_irn_data(new_phases[i], new_node,
		                   phase_get_irn_data(phase, node));
	}

	set_irn_link(node, new_node);
	hook_dead_node_elim_subst(irg, node, new_node);
}

/**
 * Copies the graph reachable from current_ir_graph->end to the obstack
 * in current_ir_graph and fixes the environment.
 * Then fixes the fields in current_ir_graph containing nodes of the
 * graph.
 *
 * @param copy_node_nr  If non-zero, the node number will be copied
 */
static void copy_graph_env(ir_graph *irg)
{
	ir_node *new_anchor;
	int i;

	/* init the new_phases array */
	/* TODO: this is wrong, it should only allocate a new data_ptr inside
	 * the phase! */
	for (i = 0; i <= PHASE_LAST; i++) {
		ir_phase *old_ph = irg_get_phase(irg, i);
		if (old_ph == NULL) {
			new_phases[i] = NULL;
		} else {
			new_phases[i] = new_phase(irg, old_ph->data_init);
			new_phases[i]->priv = old_ph->priv;
		}
	}

	/* copy nodes */
	irg_walk_anchors(irg, copy_node_dce, rewire_inputs, NULL);

	/* fix the anchor */
	new_anchor = get_irn_link(irg->anchor);
	assert(new_anchor != NULL);
	irg->anchor = new_anchor;

	/* copy the new phases into the irg */
	for (i = 0; i <= PHASE_LAST; i++) {
		ir_phase *old_ph = irg_get_phase(irg, i);
		if (old_ph == NULL)
			continue;

		phase_free(old_ph);
		irg->phases[i] = new_phases[i];
	}
}

/**
 * Copies all reachable nodes to a new obstack.  Removes bad inputs
 * from block nodes and the corresponding inputs from Phi nodes.
 * Merges single exit blocks with single entry blocks and removes
 * 1-input Phis.
 * Adds all new nodes to a new hash table for CSE.  Does not
 * perform CSE, so the hash table might contain common subexpressions.
 */
void dead_node_elimination(ir_graph *irg)
{
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
	copy_graph_env(irg);

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

ir_graph_pass_t *dead_node_elimination_pass(const char *name)
{
	return def_graph_pass(name ? name : "dce", dead_node_elimination);
}


/*
   __                      _  __ __
  (_     __    o     _    | \/  |_
  __)|_| | \_/ | \_/(/_   |_/\__|__

  The following stuff implements a facility that automatically patches
  registered ir_node pointers to the new node when a dead node elimination occurs.


  Warning: This is considered a hack - try hard to avoid this!
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

static void dead_node_hook(void *context, ir_graph *irg, int start)
{
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
static void dead_node_subst_hook(void *context, ir_graph *irg, ir_node *old, ir_node *nw)
{
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
survive_dce_t *new_survive_dce(void)
{
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
void free_survive_dce(survive_dce_t *sd)
{
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
void survive_dce_register_irn(survive_dce_t *sd, ir_node **place)
{
	if (*place != NULL) {
		ir_node *irn      = *place;
		survive_dce_list_t *curr = pmap_get(sd->places, irn);
		survive_dce_list_t *nw   = OALLOC(&sd->obst, survive_dce_list_t);

		nw->next  = curr;
		nw->place = place;

		pmap_insert(sd->places, irn, nw);
	}
}
