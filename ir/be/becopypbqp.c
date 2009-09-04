/*
 * becopypbqp.c
 *
 *  Created on: Aug 28, 2009
 *      Author: bersch
 */

#include "kaps.h"
#include "pbqp_t.h"
#include "vector.h"
#include "matrix.h"
#include "html_dumper.h"
#include "heuristical.h"
#include "pbqp_node_t.h"

#include "becopyopt_t.h"
#include "beifg.h"
#include "beifg_t.h"
#include "irprintf_t.h"

#include "error.h"
#include "bitset.h"
#include "pmap.h"

static FILE *my_open(const be_chordal_env_t *env, const char *prefix, const char *suffix)
{
	FILE *result;
	char buf[1024];
	size_t i, n;
	char *tu_name;

	n = strlen(env->birg->main_env->cup_name);
	tu_name = XMALLOCN(char, n + 1);
	strcpy(tu_name, env->birg->main_env->cup_name);
	for (i = 0; i < n; ++i)
		if (tu_name[i] == '.')
			tu_name[i] = '_';

	ir_snprintf(buf, sizeof(buf), "%s%s_%F_%s%s", prefix, tu_name, env->irg, env->cls->name, suffix);
	xfree(tu_name);
	result = fopen(buf, "wt");
	if(result == NULL) {
		panic("Couldn't open '%s' for writing.", buf);
	}

	return result;
}

int co_solve_heuristic_pbqp(copy_opt_t *co) {
	void *nodes_it  = be_ifg_nodes_iter_alloca(co->cenv->ifg);
	void *neigh_it  = be_ifg_neighbours_iter_alloca(co->cenv->ifg);
	ir_node *ifg_node, *if_neighb_node;

	pbqp *pbqp_problem;
	unsigned number_registers = co->cls->n_regs;
	unsigned number_nodes = 0;
	unsigned int nodeIdx = 0;

	bitset_t *ignore_registers;
	pmap *map;

	// count nodes in ifg
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		number_nodes++;
	}

	// create empty PBQP instance
	pbqp_problem = alloc_pbqp(number_nodes);

	//
	ignore_registers = bitset_malloc(number_registers);
	be_put_ignore_regs(co->cenv->birg, co->cls, ignore_registers);

	// create map
	map = pmap_create_ex(number_nodes);

	// add costs vector to nodes
	nodeIdx = 0;
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		// create costs vector
		struct vector *costs_vector = vector_alloc(pbqp_problem, number_registers);

		// set costs
		unsigned int cnt;
		for(cnt = 0; cnt < costs_vector->len; cnt++) {
			if (bitset_is_set(ignore_registers,cnt)) {
				vector_set(costs_vector, cnt, INF_COSTS);
			}
			else {
				if (!arch_reg_out_is_allocatable(ifg_node, arch_register_for_index(co->cls, cnt)))
				{
					vector_set(costs_vector, cnt, INF_COSTS);
				}
				else {
					vector_set(costs_vector, cnt, 0);
				}
			}

			// add description
			vector_set_description(costs_vector, cnt, arch_register_for_index(co->cls, cnt)->name);
		}

		// add costs vector to node
		add_node_costs(pbqp_problem, nodeIdx, costs_vector);

		// insert ir_node and pbqp_node into map
		pmap_insert(map, ifg_node, get_node(pbqp_problem,nodeIdx));

		// increment nodeIndex
		nodeIdx++;
	}

	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		pbqp_node *src_node = pmap_find(map,ifg_node)->value;
		unsigned int srcNodeIdx = src_node->index;

		// add costs matrix between nodes (interference edge)
		be_ifg_foreach_neighbour(co->cenv->ifg, neigh_it, ifg_node, if_neighb_node) {
			pbqp_node *trg_node = pmap_find(map,if_neighb_node)->value;
			if(get_edge(pbqp_problem, srcNodeIdx, trg_node->index) == NULL) {
				// create costs matrix
				struct pbqp_matrix *matrix = pbqp_matrix_alloc(pbqp_problem, number_registers, number_registers);

				// set costs
				unsigned row, col;
				for(row = 0; row < number_registers; row++) {
					for(col = 0; col < number_registers; col++) {
						if(row == col) {
							pbqp_matrix_set(matrix, row, col, INF_COSTS);
						}
						else {
							pbqp_matrix_set(matrix, row, col, 2);
						}
					}
				}

				// add costs matrix to interference edge
				add_edge_costs(pbqp_problem, srcNodeIdx, trg_node->index , matrix);
			}
		}

		// add costs matrix between nodes (affinety edge)
		affinity_node_t *aff_node = get_affinity_info(co, ifg_node);
		neighb_t *aff_neighb_node;
		if(aff_node != NULL) {
			co_gs_foreach_neighb(aff_node, aff_neighb_node) {
				pbqp_node *trg_node = pmap_find(map,aff_neighb_node->irn)->value;
				if(get_edge(pbqp_problem, srcNodeIdx, trg_node->index) == NULL) {
					// create costs matrix
					struct pbqp_matrix *matrix = pbqp_matrix_alloc(pbqp_problem, number_registers, number_registers);

					// set costs
					unsigned row, col;
					for(row = 0; row < number_registers; row++) {
						for(col = 0; col < number_registers; col++) {
							if(row == col) {
								pbqp_matrix_set(matrix, row, col, 0);
							}
							else {
								pbqp_matrix_set(matrix, row, col, 2);
							}
						}
					}

					// add costs matrix to interference edge
					add_edge_costs(pbqp_problem, srcNodeIdx, trg_node->index , matrix);
				}
			}
		}
	}

#if KAPS_DUMP
	// dump graph before solving pbqp
	FILE *file_before = my_open(co->cenv, "", "-before.html");
	set_dumpfile(pbqp_problem, file_before);
	pbqp_dump_input(pbqp_problem);
#endif


	// solve pbqp problem
	solve_pbqp_heuristical(pbqp_problem);
	//solve_pbqp_brute_force(pbqp_problem);
	num solution = get_solution(pbqp_problem);

	printf("Solution (%s): %d\n",co->name, (int)solution);

	// coloring ifg
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		pbqp_node *node = pmap_find(map,ifg_node)->value;
		num index = get_node_solution(pbqp_problem, node->index);
		const arch_register_t *reg = arch_register_for_index(co->cls, index);
		arch_set_irn_register(ifg_node, reg);
	}

#if KAPS_DUMP
	// dump graph after solving pbqp
	FILE *file_after = my_open(co->cenv, "", "-after.html");
	set_dumpfile(pbqp_problem, file_after);
	pbqp_dump_input(pbqp_problem);
#endif


	// free pbqp resources
#if KAPS_DUMP
	fclose(file_before);
	fclose(file_after);
#endif
	bitset_free(ignore_registers);
	pmap_destroy(map);
	free_pbqp(pbqp_problem);

	return 0;
}
