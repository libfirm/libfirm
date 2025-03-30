/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Compute and access out edges (also called def-use edges).
 * @author   Goetz Lindenmaier, Michael Beck
 * @date     1.2002
 */
#include "irouts_t.h"

#include "ircons.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "xmalloc.h"

unsigned get_irn_n_outs(const ir_node *node)
{
	return node->o.out->n_edges;
}

ir_node *get_irn_out(const ir_node *def, unsigned pos)
{
	assert(pos < get_irn_n_outs(def));
	return def->o.out->edges[pos].use;
}

ir_node *get_irn_out_ex(const ir_node *def, unsigned pos, int *in_pos)
{
	assert(pos < get_irn_n_outs(def));
	*in_pos = def->o.out->edges[pos].pos;
	return def->o.out->edges[pos].use;
}

unsigned get_Block_n_cfg_outs(const ir_node *bl)
{
	assert(is_Block(bl));
	unsigned n_cfg_outs = 0;
	foreach_irn_out(bl, i, succ) {
		if (get_irn_mode(succ) != mode_X)
			continue;
		if (is_End(succ) || is_Bad(succ))
			continue;
		n_cfg_outs += get_irn_n_outs(succ);
	}
	return n_cfg_outs;
}

unsigned get_Block_n_cfg_outs_ka(const ir_node *bl)
{
	assert(is_Block(bl));
	unsigned n_cfg_outs = 0;
	foreach_irn_out(bl, i, succ) {
		if (get_irn_mode(succ) != mode_X)
			continue;
		if (is_Bad(succ))
			continue;
		if (is_End(succ)) {
			ir_node *end_bl = get_nodes_block(succ);
			if (end_bl == bl)
				continue;
			++n_cfg_outs;
			continue;
		}
		n_cfg_outs += get_irn_n_outs(succ);
	}
	return n_cfg_outs;
}

ir_node *get_Block_cfg_out(const ir_node *bl, unsigned pos)
{
	assert(is_Block(bl));
	foreach_irn_out(bl, i, succ) {
		if (get_irn_mode(succ) != mode_X)
			continue;
		if (is_End(succ) || is_Bad(succ))
			continue;

		unsigned n_outs = get_irn_n_outs(succ);
		if (pos < n_outs)
			return get_irn_out(succ, pos);
		else
			pos -= n_outs;
	}
	return NULL;
}

ir_node *get_Block_cfg_out_ex(const ir_node *bl, unsigned pos, int *in_pos)
{
	assert(is_Block(bl));
	foreach_irn_out(bl, i, succ) {
		if (get_irn_mode(succ) != mode_X)
			continue;
		if (is_End(succ) || is_Bad(succ))
			continue;

		unsigned n_outs = get_irn_n_outs(succ);
		if (pos < n_outs)
			return get_irn_out_ex(succ, pos, in_pos);
		else
			pos -= n_outs;
	}
	return NULL;
}

ir_node *get_Block_cfg_out_ka(const ir_node *bl, unsigned pos)
{
	assert(is_Block(bl));
	foreach_irn_out(bl, i, succ) {
		if (get_irn_mode(succ) != mode_X)
			continue;
		if (is_Bad(succ))
			continue;

		if (is_End(succ)) {
			ir_node *end_bl = get_nodes_block(succ);
			if (end_bl == bl) {
				/* ignore End if we are in the Endblock */
				continue;
			}
			if (pos == 0) {
				/* handle keep-alive here: return the Endblock instead of the End node */
				return end_bl;
			} else {
				--pos;
				continue;
			}
		}
		unsigned n_outs = get_irn_n_outs(succ);
		if (pos < n_outs)
			return get_irn_out(succ, pos);
		else
			pos -= n_outs;
	}
	return NULL;
}

static void irg_out_walk_2(ir_node *node, irg_walk_func *pre,
                           irg_walk_func *post, void *env)
{
	ir_graph *const irg = get_irn_irg(node);
	assert(get_irn_visited(node) < get_irg_visited(irg));

	set_irn_visited(node, get_irg_visited(irg));

	if (pre != NULL)
		pre(node, env);

	foreach_irn_out(node, i, succ) {
		if (get_irn_visited(succ) < get_irg_visited(irg))
			irg_out_walk_2(succ, pre, post, env);
	}

	if (post != NULL)
		post(node, env);
}

void irg_out_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                  void *env)
{
	assert(node != NULL);
	ir_graph *irg = get_irn_irg(node);
	if (irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS)) {
		inc_irg_visited(irg);
		irg_out_walk_2(node, pre, post, env);
	}
}

static void irg_out_block_walk2(ir_node *bl, irg_walk_func *pre,
                                irg_walk_func *post, void *env)
{
	if (Block_block_visited(bl))
		return;
	mark_Block_block_visited(bl);

	if (pre != NULL)
		pre(bl, env);

	for (int i = 0, n = get_Block_n_cfg_outs(bl); i < n; ++i) {
		/* find the corresponding predecessor block. */
		ir_node *pred = get_Block_cfg_out(bl, i);
		/* recursion */
		irg_out_block_walk2(pred, pre, post, env);
	}

	if (post != NULL)
		post(bl, env);
}

void irg_out_block_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                        void *env)
{
	ir_graph *irg = get_irn_irg(node);
	assert(is_Block(node) || (get_irn_mode(node) == mode_X));

	inc_irg_block_visited(irg);

	if (get_irn_mode(node) == mode_X) {
		foreach_irn_out(node, i, succ) {
			irg_out_block_walk2(succ, pre, post, env);
		}
	} else {
		irg_out_block_walk2(node, pre, post, env);
	}
}

/*--------------------------------------------------------------------*/
/** Building and Removing the out data structure                     **/
/**                                                                  **/
/** The outs of a graph are allocated in a single, large array.      **/
/** This allows to allocate and deallocate the memory for the outs   **/
/** on demand.  The large array is separated into many small ones    **/
/** for each node.  Only a single field to reference the out array   **/
/** is stored in each node and a field referencing the large out     **/
/** array in irgraph.  The 0 field of each out array contains the    **/
/** size of this array.  This saves memory in the irnodes themselves.**/
/** The construction does two passes over the graph.  The first pass **/
/** counts the overall number of outs and the outs of each node.  It **/
/** stores the outs of each node in the out reference of the node.   **/
/** Then the large array is allocated.  The second iteration chops   **/
/** the large array into smaller parts, sets the out edges and       **/
/** recounts the out edges.                                          **/
/*--------------------------------------------------------------------*/


/** Returns the amount of out edges for not yet visited successors. */
static void count_outs_node(ir_node *n)
{
	if (irn_visited_else_mark(n))
		return;

	/* initialize our counter */
	n->o.n_outs = 0;

	int start = is_Block(n) ? 0 : -1;
	for (int i = start, irn_arity = get_irn_arity(n); i < irn_arity; ++i) {
		ir_node *def = get_irn_n(n, i);
		count_outs_node(def);
		++def->o.n_outs;
	}
}


/** Returns the amount of out edges for not yet visited successors.
 *  This version handles some special nodes like irg_frame, irg_args etc. */
static void count_outs(ir_graph *irg)
{
	inc_irg_visited(irg);
	count_outs_node(get_irg_end(irg));
	foreach_irn_in(get_irg_anchor(irg), i, n) {
		if (irn_visited_else_mark(n))
			continue;
		n->o.n_outs = 0;
	}
}

static void set_out_edges_node(ir_node *node, struct obstack *obst)
{
	if (irn_visited_else_mark(node))
		return;

	/* Allocate my array */
	unsigned n_outs = node->o.n_outs;
	node->o.out          = OALLOCF(obst, ir_def_use_edges, edges, n_outs);
	node->o.out->n_edges = 0;

	/* add def->use edges from my predecessors to me */
	int start = is_Block(node) ? 0 : -1;
	for (int i = start, irn_arity = get_irn_arity(node); i < irn_arity; ++i) {
		ir_node *def = get_irn_n(node, i);

		/* recurse, ensures that out array of pred is already allocated */
		set_out_edges_node(def, obst);

		/* Remember this Def-Use edge */
		unsigned pos = def->o.out->n_edges++;
		def->o.out->edges[pos].use = node;
		def->o.out->edges[pos].pos = i;
	}
}

static void set_out_edges(ir_graph *irg)
{
	struct obstack *obst = &irg->out_obst;
	obstack_init(obst);
	irg->out_obst_allocated = true;

	inc_irg_visited(irg);
	set_out_edges_node(get_irg_end(irg), obst);
	foreach_irn_in(get_irg_anchor(irg), i, n) {
		if (irn_visited_else_mark(n))
			continue;
		n->o.out          = OALLOCF(obst, ir_def_use_edges, edges, 0);
		n->o.out->n_edges = 0;
	}
}

void compute_irg_outs(ir_graph *irg)
{
	free_irg_outs(irg);

	/* This first iteration counts the overall number of out edges and the
	   number of out edges for each node. */
	count_outs(irg);

	/* The second iteration splits the irg->outs array into smaller arrays
	   for each node and writes the back edges into this array. */
	set_out_edges(irg);

	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
}

void assure_irg_outs(ir_graph *irg)
{
	if (!irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS))
		compute_irg_outs(irg);
}

#ifdef DEBUG_libfirm
/** Clear the outs of a node */
static void reset_outs(ir_node *node, void *unused)
{
	(void)unused;
	node->o.out = NULL;
}
#endif

void free_irg_outs(ir_graph *irg)
{
	if (irg->out_obst_allocated) {
		obstack_free(&irg->out_obst, NULL);
		irg->out_obst_allocated = false;
	}

#ifdef DEBUG_libfirm
	/* when debugging, *always* reset all nodes' outs!  irg->outs might
	   have been lying to us */
	irg_walk_graph(irg, reset_outs, NULL, NULL);
#endif
}
