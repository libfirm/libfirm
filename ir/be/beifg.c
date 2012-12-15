/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interface for interference graphs.
 * @author      Sebastian Hack
 * @date        18.11.2005
 */
#include "config.h"

#include <stdlib.h>

#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "timing.h"
#include "bitset.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "irtools.h"
#include "beifg.h"
#include "error.h"
#include "xmalloc.h"

#include "becopystat.h"
#include "becopyopt.h"
#include "beirg.h"
#include "bemodule.h"
#include "beintlive_t.h"

void be_ifg_free(be_ifg_t *self)
{
	free(self);
}

int be_ifg_connected(const be_ifg_t *ifg, const ir_node *a, const ir_node *b)
{
	be_lv_t *lv = be_get_irg_liveness(ifg->env->irg);
	return be_values_interfere(lv, a, b);
}

static void nodes_walker(ir_node *bl, void *data)
{
	nodes_iter_t     *it   = (nodes_iter_t*)data;
	struct list_head *head = get_block_border_head(it->env, bl);

	foreach_border_head(head, b) {
		if (b->is_def && b->is_real) {
			obstack_ptr_grow(&it->obst, b->irn);
			it->n++;
		}
	}
}

static void find_nodes(const be_ifg_t *ifg, nodes_iter_t *iter)
{
	obstack_init(&iter->obst);
	iter->n     = 0;
	iter->curr  = 0;
	iter->env   = ifg->env;

	irg_block_walk_graph(ifg->env->irg, nodes_walker, NULL, iter);
	obstack_ptr_grow(&iter->obst, NULL);
	iter->nodes = (ir_node**)obstack_finish(&iter->obst);
}

static inline void node_break(nodes_iter_t *it, int force)
{
	if ((it->curr >= it->n || force) && it->nodes) {
		obstack_free(&it->obst, NULL);
		it->nodes = NULL;
	}
}

static ir_node *get_next_node(nodes_iter_t *it)
{
	ir_node *res = NULL;

	if (it->curr < it->n)
		res = it->nodes[it->curr++];

	node_break(it, 0);

	return res;
}

ir_node *be_ifg_nodes_begin(const be_ifg_t *ifg, nodes_iter_t *iter)
{
	find_nodes(ifg, iter);
	return get_next_node(iter);
}

ir_node *be_ifg_nodes_next(nodes_iter_t *iter)
{
	return get_next_node(iter);
}

void be_ifg_nodes_break(nodes_iter_t *iter)
{
	node_break(iter, 1);
}

static void find_neighbour_walker(ir_node *block, void *data)
{
	neighbours_iter_t *it    = (neighbours_iter_t*)data;
	struct list_head  *head  = get_block_border_head(it->env, block);
	be_lv_t           *lv    = be_get_irg_liveness(it->env->irg);

	int has_started = 0;

	if (!be_is_live_in(lv, block, it->irn) && block != get_nodes_block(it->irn))
		return;

	foreach_border_head(head, b) {
		ir_node *irn = b->irn;

		if (irn == it->irn) {
			if (b->is_def)
				has_started = 1;
			else
				break; /* if we reached the end of the node's lifetime we can safely break */
		}
		else if (b->is_def) {
			/* if any other node than the one in question starts living, add it to the set */
			ir_nodeset_insert(&it->neighbours, irn);
		}
		else if (!has_started) {
			/* we only delete, if the live range in question has not yet started */
			ir_nodeset_remove(&it->neighbours, irn);
		}

	}
}

static void find_neighbours(const be_ifg_t *ifg, neighbours_iter_t *it, const ir_node *irn)
{
	it->env         = ifg->env;
	it->irn         = irn;
	it->valid       = 1;
	ir_nodeset_init(&it->neighbours);

	dom_tree_walk(get_nodes_block(irn), find_neighbour_walker, NULL, it);

	ir_nodeset_iterator_init(&it->iter, &it->neighbours);
}

static inline void neighbours_break(neighbours_iter_t *it, int force)
{
	(void) force;
	assert(it->valid == 1);
	ir_nodeset_destroy(&it->neighbours);
	it->valid = 0;
}

static ir_node *get_next_neighbour(neighbours_iter_t *it)
{
	ir_node *res = ir_nodeset_iterator_next(&it->iter);

	if (res == NULL) {
		ir_nodeset_destroy(&it->neighbours);
	}
	return res;
}

ir_node *be_ifg_neighbours_begin(const be_ifg_t *ifg, neighbours_iter_t *iter,
                                 const ir_node *irn)
{
	find_neighbours(ifg, iter, irn);
	return get_next_neighbour(iter);
}

ir_node *be_ifg_neighbours_next(neighbours_iter_t *iter)
{
	return get_next_neighbour(iter);
}

void be_ifg_neighbours_break(neighbours_iter_t *iter)
{
	neighbours_break(iter, 1);
}

static inline void free_clique_iter(cliques_iter_t *it)
{
	it->n_blocks = -1;
	obstack_free(&it->ob, NULL);
	del_pset(it->living);
}

static void get_blocks_dom_order(ir_node *blk, void *env)
{
	cliques_iter_t *it = (cliques_iter_t*)env;
	obstack_ptr_grow(&it->ob, blk);
}

/**
 * NOTE: Be careful when changing this function!
 *       First understand the control flow of consecutive calls.
 */
static inline int get_next_clique(cliques_iter_t *it)
{

	/* continue in the block we left the last time */
	for (; it->blk < it->n_blocks; it->blk++) {
		int output_on_shrink = 0;
		struct list_head *head = get_block_border_head(it->cenv, it->blocks[it->blk]);

		/* on entry to a new block set the first border ... */
		if (!it->bor)
			it->bor = head->prev;

		/* ... otherwise continue with the border we left the last time */
		for (; it->bor != head; it->bor = it->bor->prev) {
			border_t *b = list_entry(it->bor, border_t, list);

			/* if its a definition irn starts living */
			if (b->is_def) {
				pset_insert_ptr(it->living, b->irn);
				if (b->is_real)
					output_on_shrink = 1;
			} else

			/* if its the last usage the irn dies */
			{
				/* before shrinking the set, return the current maximal clique */
				if (output_on_shrink) {
					int count = 0;

					/* fill the output buffer */
					foreach_pset(it->living, ir_node, irn) {
						it->buf[count++] = irn;
					}

					assert(count > 0 && "We have a 'last usage', so there must be sth. in it->living");

					return count;
				}

				pset_remove_ptr(it->living, b->irn);
			}
		}

		it->bor = NULL;
		assert(0 == pset_count(it->living) && "Something has survived! (At the end of the block it->living must be empty)");
	}

	if (it->n_blocks != -1)
		free_clique_iter(it);

	return -1;
}

int be_ifg_cliques_begin(const be_ifg_t *ifg, cliques_iter_t *it,
                         ir_node **buf)
{
	obstack_init(&it->ob);
	dom_tree_walk_irg(ifg->env->irg, get_blocks_dom_order, NULL, it);

	it->cenv     = ifg->env;
	it->buf      = buf;
	it->n_blocks = obstack_object_size(&it->ob) / sizeof(void *);
	it->blocks   = (ir_node**)obstack_finish(&it->ob);
	it->blk      = 0;
	it->bor      = NULL;
	it->living   = pset_new_ptr(2 * arch_register_class_n_regs(it->cenv->cls));

	return get_next_clique(it);
}

int be_ifg_cliques_next(cliques_iter_t *iter)
{
	return get_next_clique(iter);
}

void be_ifg_cliques_break(cliques_iter_t *iter)
{
	free_clique_iter(iter);
}

int be_ifg_degree(const be_ifg_t *ifg, const ir_node *irn)
{
	neighbours_iter_t it;
	int degree;
	find_neighbours(ifg, &it, irn);
	degree = ir_nodeset_size(&it.neighbours);
	neighbours_break(&it, 1);
	return degree;
}

be_ifg_t *be_create_ifg(const be_chordal_env_t *env)
{
	be_ifg_t *ifg = XMALLOC(be_ifg_t);
	ifg->env = env;

	return ifg;
}

static void int_comp_rec(be_ifg_t *ifg, ir_node *n, bitset_t *seen)
{
	neighbours_iter_t neigh_it;

	be_ifg_foreach_neighbour(ifg, &neigh_it, n, m) {
		if (bitset_is_set(seen, get_irn_idx(m)))
			continue;

		arch_register_req_t const *const req = arch_get_irn_register_req(m);
		if (arch_register_req_is(req, ignore))
			continue;

		bitset_set(seen, get_irn_idx(m));
		int_comp_rec(ifg, m, seen);
	}

}

static int int_component_stat(ir_graph *irg, be_ifg_t *ifg)
{
	int      n_comp    = 0;
	nodes_iter_t nodes_it;
	bitset_t *seen     = bitset_malloc(get_irg_last_idx(irg));

	be_ifg_foreach_node(ifg, &nodes_it, n) {
		if (bitset_is_set(seen, get_irn_idx(n)))
			continue;

		arch_register_req_t const *const req = arch_get_irn_register_req(n);
		if (arch_register_req_is(req, ignore))
			continue;

		++n_comp;
		bitset_set(seen, get_irn_idx(n));
		int_comp_rec(ifg, n, seen);
	}

	free(seen);
	return n_comp;
}

void be_ifg_stat(ir_graph *irg, be_ifg_t *ifg, be_ifg_stat_t *stat)
{
	nodes_iter_t      nodes_it;
	neighbours_iter_t neigh_it;

	memset(stat, 0, sizeof(stat[0]));

	size_t n_edges = 0;
	be_ifg_foreach_node(ifg, &nodes_it, n) {
		stat->n_nodes += 1;
		be_ifg_foreach_neighbour(ifg, &neigh_it, n, m) {
			++n_edges;
		}
	}

	/* Every interference edge was counted twice, once for each end. */
	stat->n_edges = n_edges / 2;
	stat->n_comps = int_component_stat(irg, ifg);
}
