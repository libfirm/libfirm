/**
 * @file   beifg.c
 * @date   18.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef __linux__
#include <malloc.h>
#endif /* __linux__ */

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>
#endif /* WITH_LIBCORE */

#include "bitset.h"

#include "irgwalk.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "irtools.h"
#include "beifg_t.h"
#include "beifg_impl.h"
#include "irphase.h"
#include "irphase_t.h"
#include "bechordal.h"

#include "becopystat.h"
#include "becopyopt.h"

/** Defines values for the ifg performance test */
#define BE_CH_PERFORMANCETEST_MIN_NODES (50)
#define BE_CH_PERFORMANCETEST_COUNT (500)

typedef struct _coloring_t coloring_t;

struct _coloring_t {
	phase_t ph;
	const arch_env_t *arch_env;
	ir_graph *irg;
};

size_t (be_ifg_nodes_iter_size)(const void *self)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->nodes_iter_size;
}

size_t (be_ifg_neighbours_iter_size)(const void *self)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->neighbours_iter_size;
}

size_t (be_ifg_cliques_iter_size)(const void *self)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->cliques_iter_size;
}

static void *regs_irn_data_init(phase_t *ph, ir_node *irn, void *data)
{
	coloring_t *coloring = (coloring_t *) ph;
	return (void *) arch_get_irn_register(coloring->arch_env, irn);
}

coloring_t *coloring_init(coloring_t *c, ir_graph *irg, const arch_env_t *aenv)
{
	phase_init(&c->ph, "regs_map", irg, PHASE_DEFAULT_GROWTH, regs_irn_data_init);
	c->arch_env = aenv;
	c->irg = irg;
	return c;
}

static void get_irn_color(ir_node *irn, void *c)
{
	coloring_t *coloring = c;
	phase_get_or_set_irn_data(&coloring->ph, irn);
}

static void restore_irn_color(ir_node *irn, void *c)
{
	coloring_t *coloring = c;
	const arch_register_t *reg = phase_get_irn_data(&coloring->ph, irn);
	if(reg)
		arch_set_irn_register(coloring->arch_env, irn, reg);
}

void coloring_save(coloring_t *c)
{
	irg_walk_graph(c->irg, NULL, get_irn_color, c);
}

void coloring_restore(coloring_t *c)
{
	irg_walk_graph(c->irg, NULL, restore_irn_color, c);
}

void (be_ifg_free)(void *self)
{
	be_ifg_t *ifg = self;
	ifg->impl->free(self);
}

int (be_ifg_connected)(const void *self, const ir_node *a, const ir_node *b)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->connected(self, a, b);
}

ir_node *(be_ifg_neighbours_begin)(const void *self, void *iter, const ir_node *irn)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->neighbours_begin(self, iter, irn);
}

ir_node *(be_ifg_neighbours_next)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->neighbours_next(self, iter);
}

void (be_ifg_neighbours_break)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	ifg->impl->neighbours_break(self, iter);
}

ir_node *(be_ifg_nodes_begin)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->nodes_begin(self, iter);
}

ir_node *(be_ifg_nodes_next)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->nodes_next(self, iter);
}

void (be_ifg_nodes_break)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	ifg->impl->nodes_break(self, iter);
}

int (be_ifg_cliques_begin)(const void *self, void *iter, ir_node **buf)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->cliques_begin(self, iter, buf);
}

int (be_ifg_cliques_next)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->cliques_next(self, iter);
}

void (be_ifg_cliques_break)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	ifg->impl->cliques_break(self, iter);
}

int (be_ifg_degree)(const void *self, const ir_node *irn)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->degree(self, irn);
}


int be_ifg_is_simplicial(const be_ifg_t *ifg, const ir_node *irn)
{
	int degree = be_ifg_degree(ifg, irn);
	void *iter = be_ifg_neighbours_iter_alloca(ifg);

	ir_node **neighbours = xmalloc(degree * sizeof(neighbours[0]));

	ir_node *curr;
	int i, j;

	i = 0;
	be_ifg_foreach_neighbour(ifg, iter, irn, curr)
		neighbours[i++] = curr;

	for(i = 0; i < degree; ++i) {
		for(j = 0; j < i; ++j)
			if(!be_ifg_connected(ifg, neighbours[i], neighbours[j])) {
				free(neighbours);
				return 0;
			}
	}


	free(neighbours);
	return 1;
}

void be_ifg_check(const be_ifg_t *ifg)
{
	void *iter1 = be_ifg_nodes_iter_alloca(ifg);
	void *iter2 = be_ifg_neighbours_iter_alloca(ifg);

	ir_node *n, *m;
	int node_count = 0;
	int neighbours_count = 0;
	int degree = 0;

	/* count all nodes */
	ir_printf("\n\nFound the following nodes in the graph %+F:\n\n", current_ir_graph);
	be_ifg_foreach_node(ifg,iter1,n)
	{
		node_count++;
		degree = be_ifg_degree(ifg, n);
		ir_printf("%d. %+F with degree: %d\n", node_count, n, degree);
	}

	ir_printf("\n\nNumber of nodes: %d\n\n", node_count);

	/* Check, if all neighbours are indeed connected to the node. */
	be_ifg_foreach_node(ifg, iter1, n)
	{
		ir_printf("\n%+F; ", n);
		be_ifg_foreach_neighbour(ifg, iter2, n, m)
		{
			ir_printf("%+F; ", m);
			neighbours_count++;
			if(!be_ifg_connected(ifg, n, m))
				ir_fprintf(stderr, "%+F is a neighbour of %+F but they are not connected!\n", n, m);
		}
	}
	ir_printf("\n\nFound %d nodes in the 'check neighbour section'\n", neighbours_count);
}

int be_ifg_check_get_node_count(const be_ifg_t *ifg)
{
	void *iter = be_ifg_nodes_iter_alloca(ifg);
	int node_count = 0;
	ir_node *n;

	be_ifg_foreach_node(ifg, iter, n)
	{
		node_count++;
	}

	return node_count;
}

static int be_ifg_check_cmp_nodes(const void *a, const void *b)
{
	const ir_node *node_a = *(ir_node **)a;
	const ir_node *node_b = *(ir_node **)b;

	int nr_a = node_a->node_nr;
	int nr_b = node_b->node_nr;

	return QSORT_CMP(nr_a, nr_b);
}

void be_ifg_check_sorted(const be_ifg_t *ifg)
{
	void *iter1 = be_ifg_nodes_iter_alloca(ifg);
	void *iter2 = be_ifg_neighbours_iter_alloca(ifg);

	ir_node *n, *m;
	const int node_count = be_ifg_check_get_node_count(ifg);
	int i = 0;

	ir_node **all_nodes = xmalloc(node_count * sizeof(all_nodes[0]));

	be_ifg_foreach_node(ifg, iter1, n)
	{
		if(!node_is_in_irgs_storage(ifg->env->irg, n))
		{
			ir_printf("+%F is in ifg but not in the current irg!", n);
			assert (node_is_in_irgs_storage(ifg->env->irg, n));
		}

		all_nodes[i] = n;
		i++;
	}

	qsort(all_nodes, node_count, sizeof(all_nodes[0]), be_ifg_check_cmp_nodes);

	for (i = 0; i < node_count; i++)
	{
		ir_node **neighbours = xmalloc(node_count * sizeof(neighbours[0]));
		int j = 0;
		int k = 0;
		int degree = 0;

		degree = be_ifg_degree(ifg, all_nodes[i]);

		be_ifg_foreach_neighbour(ifg, iter2, all_nodes[i], m)
		{
			neighbours[j] = m;
			j++;
		}

		qsort(neighbours, j, sizeof(neighbours[0]), be_ifg_check_cmp_nodes);

		ir_printf("%d. %+F's neighbours(%d): ", i+1, all_nodes[i], degree);

		for(k = 0; k < j; k++)
		{
			ir_printf("%+F, ", neighbours[k]);
		}

		ir_printf("\n");

		free(neighbours);
	}

	free(all_nodes);

}

void be_ifg_check_sorted_to_file(const be_ifg_t *ifg, FILE *f)
{
	void *iter1 = be_ifg_nodes_iter_alloca(ifg);
	void *iter2 = be_ifg_neighbours_iter_alloca(ifg);

	ir_node *n, *m;
	const int node_count = be_ifg_check_get_node_count(ifg);
	int i = 0;

	ir_node **all_nodes = xmalloc(node_count * sizeof(all_nodes[0]));

	be_ifg_foreach_node(ifg, iter1, n)
	{
		if(!node_is_in_irgs_storage(ifg->env->irg, n))
		{
			ir_fprintf (f,"+%F is in ifg but not in the current irg!",n);
			assert (node_is_in_irgs_storage(ifg->env->irg, n));
		}

		all_nodes[i] = n;
		i++;
	}

	qsort(all_nodes, node_count, sizeof(all_nodes[0]), be_ifg_check_cmp_nodes);

	for (i = 0; i < node_count; i++)
	{
		ir_node **neighbours = xmalloc(node_count * sizeof(neighbours[0]));
		int j = 0;
		int k = 0;
		int degree = 0;

		degree = be_ifg_degree(ifg, all_nodes[i]);

		be_ifg_foreach_neighbour(ifg, iter2, all_nodes[i], m)
		{
			neighbours[j] = m;
			j++;
		}

		qsort(neighbours, j, sizeof(neighbours[0]), be_ifg_check_cmp_nodes);

		ir_fprintf (f,"%d. %+F's neighbours(%d): ", i+1, all_nodes[i], degree);

		for(k = 0; k < j; k++)
		{
			ir_fprintf (f,"%+F, ", neighbours[k]);
		}

		ir_fprintf (f,"\n");

		free(neighbours);
	}

	free(all_nodes);

}

void be_ifg_check_performance(be_chordal_env_t *chordal_env)
{
#ifdef WITH_LIBCORE
	int tests = BE_CH_PERFORMANCETEST_COUNT;
	coloring_t coloring;

	int used_memory;

	int i = 0;
	int rt;
	copy_opt_t *co;
	be_ifg_t *old_if = chordal_env->ifg;

	lc_timer_t *timer = lc_timer_register("getTime","get Time of copy minimization using the ifg");
	unsigned long elapsed_usec = 0;

	if (get_irg_estimated_node_cnt(chordal_env->irg) >= BE_CH_PERFORMANCETEST_MIN_NODES)
	{
		coloring_init(&coloring, chordal_env->irg, chordal_env->birg->main_env->arch_env);
		coloring_save(&coloring);

		lc_timer_reset(timer);

		for (i = 0; i<tests; i++) /* performance test with std */
		{

			used_memory = lc_get_heap_used_bytes();

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			chordal_env->ifg = be_ifg_std_new(chordal_env);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			used_memory = lc_get_heap_used_bytes() - used_memory;

			coloring_restore(&coloring);

			co = NULL;
			co = new_copy_opt(chordal_env, co_get_costs_loop_depth);
			co_build_ou_structure(co);
			co_build_graph_structure(co);

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			co_solve_heuristic_new(co);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			co_free_graph_structure(co);
			co_free_ou_structure(co);
			free_copy_opt(co);
			be_ifg_free(chordal_env->ifg);

		}

		elapsed_usec = lc_timer_elapsed_usec(timer);
		/* calculating average */
		elapsed_usec = elapsed_usec / tests;

		ir_printf("\nstd:; %+F; %u; %u ",current_ir_graph, used_memory, elapsed_usec);

		used_memory=0;
		elapsed_usec=0;

		for (i = 0; i<tests; i++)  /* performance test with clique */
		{
			used_memory = lc_get_heap_used_bytes();

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			chordal_env->ifg = be_ifg_clique_new(chordal_env);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			used_memory = lc_get_heap_used_bytes() - used_memory;

			coloring_restore(&coloring);

			co = NULL;
			co = new_copy_opt(chordal_env, co_get_costs_loop_depth);
			co_build_ou_structure(co);
			co_build_graph_structure(co);

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			co_solve_heuristic_new(co);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			co_free_graph_structure(co);
			co_free_ou_structure(co);
			free_copy_opt(co);
			be_ifg_free(chordal_env->ifg);

		}

		elapsed_usec = lc_timer_elapsed_usec(timer);
		/* calculating average */
		elapsed_usec = elapsed_usec / tests;

		ir_printf("\nclique:; %+F; %u; %u ",current_ir_graph, used_memory, elapsed_usec);

		used_memory=0;
		elapsed_usec=0;

		for (i = 0; i<tests; i++)  /* performance test with list */
		{
			used_memory = lc_get_heap_used_bytes();

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			chordal_env->ifg = be_ifg_list_new(chordal_env);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			used_memory = lc_get_heap_used_bytes() - used_memory;

			coloring_restore(&coloring);

			co = NULL;
			co = new_copy_opt(chordal_env, co_get_costs_loop_depth);
			co_build_ou_structure(co);
			co_build_graph_structure(co);

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			co_solve_heuristic_new(co);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			co_free_graph_structure(co);
			co_free_ou_structure(co);
			free_copy_opt(co);
			be_ifg_free(chordal_env->ifg);

		}

		elapsed_usec = lc_timer_elapsed_usec(timer);
		/* calculating average */
		elapsed_usec = elapsed_usec / tests;

		ir_printf("\nlist:; %+F; %u; %u ",current_ir_graph, used_memory, elapsed_usec);

		used_memory=0;
		elapsed_usec=0;

		for (i = 0; i<tests; i++)  /* performance test with pointer */
		{
			used_memory = lc_get_heap_used_bytes();

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			chordal_env->ifg = be_ifg_pointer_new(chordal_env);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			used_memory = lc_get_heap_used_bytes() - used_memory;

			coloring_restore(&coloring);

			co = NULL;
			co = new_copy_opt(chordal_env, co_get_costs_loop_depth);
			co_build_ou_structure(co);
			co_build_graph_structure(co);

			rt = lc_timer_enter_high_priority();
			lc_timer_start(timer);

			co_solve_heuristic_new(co);

			lc_timer_stop(timer);
			rt = lc_timer_leave_high_priority();

			co_free_graph_structure(co);
			co_free_ou_structure(co);
			free_copy_opt(co);
			be_ifg_free(chordal_env->ifg);

		}

		elapsed_usec = lc_timer_elapsed_usec(timer);
		/* calculating average */
		elapsed_usec = elapsed_usec / tests;

		ir_printf("\npointer:; %+F; %u; %u ",current_ir_graph, used_memory, elapsed_usec);

		i=0;
		used_memory=0;
		elapsed_usec=0;
	}

	chordal_env->ifg = old_if;
#endif /* WITH_LIBCORE */
}

void be_ifg_dump_dot(be_ifg_t *ifg, ir_graph *irg, FILE *file, const be_ifg_dump_dot_cb_t *cb, void *self)
{
	void *nodes_it  = be_ifg_nodes_iter_alloca(ifg);
	void *neigh_it  = be_ifg_neighbours_iter_alloca(ifg);
	bitset_t *nodes = bitset_malloc(get_irg_last_idx(irg));

	ir_node *n, *m;

	fprintf(file, "graph G {\n\tgraph [");
	if(cb->graph_attr)
		cb->graph_attr(file, self);
	fprintf(file, "];\n");

	if(cb->at_begin)
		cb->at_begin(file, self);

	be_ifg_foreach_node(ifg, nodes_it, n) {
		if(cb->is_dump_node && cb->is_dump_node(self, n)) {
			int idx = get_irn_idx(n);
			bitset_set(nodes, idx);
			fprintf(file, "\tnode [");
			if(cb->node_attr)
				cb->node_attr(file, self, n);
			fprintf(file, "]; n%d;\n", idx);
		}
	}

	/* Check, if all neighbours are indeed connected to the node. */
	be_ifg_foreach_node(ifg, nodes_it, n) {
		be_ifg_foreach_neighbour(ifg, neigh_it, n, m) {
			int n_idx = get_irn_idx(n);
			int m_idx = get_irn_idx(m);

			if(n_idx < m_idx && bitset_is_set(nodes, n_idx) && bitset_is_set(nodes, m_idx)) {
				fprintf(file, "\tn%d -- n%d [", n_idx, m_idx);
				if(cb->edge_attr)
					cb->edge_attr(file, self, n, m);
				fprintf(file, "];\n");
			}
		}
	}

	if(cb->at_end)
		cb->at_end(file, self);

	fprintf(file, "}\n");
	bitset_free(nodes);
}
