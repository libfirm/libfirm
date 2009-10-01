/*
 * becopypbqp.c
 *
 *  Created on: Aug 28, 2009
 *      Author: bersch
 */
#include "config.h"

#ifdef FIRM_KAPS

#include "becopypbqp.h"

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
#include "bemodule.h"
#include "irprintf_t.h"

#include "besched.h"
#include "bearch.h"
#include "irdom.h"
#include "iredges.h"

#include "error.h"
#include "bitset.h"
#include "pmap.h"
#include "plist.h"
#include "pqueue.h"

#if KAPS_DUMP
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
#endif

static void insert_into_reverse_peo(ir_node *block, void *data) {
	pqueue_t  *queue = new_pqueue();
	pqueue_t  *constatQueue = new_pqueue();
	pbqp_co_t *pbqp_co = data;
	ir_node   *irn;

	sched_foreach(block, irn) {
		pbqp_node *node;

		if (get_irn_mode(irn) == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(pbqp_co->cls, proj))
					continue;

				// get related pbqp_node and insert into reverse peo
				assert(node && "No corresponding PBQP-Node found!");

				// insert proj node into priority queue (descending by their degree in ifg)
				if(bitset_is_set(pbqp_co->restricted_nodes, get_irn_idx(proj))) {
					pqueue_put(constatQueue,proj, be_ifg_degree(pbqp_co->ifg,proj));
				}
				else {
					pqueue_put(queue,proj, be_ifg_degree(pbqp_co->ifg,proj));
				}
			}

			/* first insert all restricted nodes */
			while(!pqueue_empty(constatQueue)) {
				plist_insert_back(pbqp_co->rpeo, get_node(pbqp_co->pbqp, get_irn_idx(pqueue_pop_front(constatQueue))));
			}

			/* insert proj nodes into reverse perfect elimination order (descending by their degree in ifg) */
			while(!pqueue_empty(queue)) {
				plist_insert_back(pbqp_co->rpeo, get_node(pbqp_co->pbqp, get_irn_idx(pqueue_pop_front(queue))));
			}

		} else {
			if (!arch_irn_consider_in_reg_alloc(pbqp_co->cls, irn))
				continue;

			/* get related pbqp_node and insert into reverse peo */
			assert(node && "No corresponding PBQP-Node found!");
			plist_insert_back(pbqp_co->rpeo, get_node(pbqp_co->pbqp, get_irn_idx(irn)));
		}
	}

	/* free priority queues */
	del_pqueue(queue);
	del_pqueue(constatQueue);
}

static int co_solve_heuristic_pbqp(copy_opt_t *co) {
	void *nodes_it  = be_ifg_nodes_iter_alloca(co->cenv->ifg);
	void *neigh_it  = be_ifg_neighbours_iter_alloca(co->cenv->ifg);
	ir_node *ifg_node;
	ir_node *if_neighb_node;

	pbqp_co_t pbqp_co;

	unsigned number_registers = co->cls->n_regs;
	unsigned number_nodes = get_irg_last_idx(co->irg);

	/* create and initialize data structure for pbqp copy minimization optimization */
	pbqp_co.cls = co->cls;
	pbqp_co.rpeo = plist_new();;
	pbqp_co.map = pmap_create_ex(number_nodes);
	pbqp_co.pbqp = alloc_pbqp(number_nodes);
	pbqp_co.ignore_reg = bitset_malloc(number_registers);
	pbqp_co.restricted_nodes = bitset_malloc(number_nodes);
	pbqp_co.ifg = co->cenv->ifg;

	/* get ignored registers */
	be_put_ignore_regs(co->cenv->birg, co->cls, pbqp_co.ignore_reg);

	/* add costs vector to nodes */
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		int cntFreeChoosableRegs = 0;

		/* create costs vector */
		struct vector *costs_vector = vector_alloc(pbqp_co.pbqp, number_registers);

		/* set costs */
		unsigned int cnt;
		for(cnt = 0; cnt < costs_vector->len; cnt++) {
			if (bitset_is_set(pbqp_co.ignore_reg,cnt)) {
				vector_set(costs_vector, cnt, INF_COSTS);
			}
			else {
				if (!arch_reg_out_is_allocatable(ifg_node, arch_register_for_index(co->cls, cnt)))
				{
					vector_set(costs_vector, cnt, INF_COSTS);
				}
				else {
					vector_set(costs_vector, cnt, 0);
					cntFreeChoosableRegs++;
				}
			}
#if KAPS_ENABLE_VECTOR_NAMES
			/* add description */
			vector_set_description(costs_vector, cnt, arch_register_for_index(co->cls, cnt)->name);
#endif
		}

		/* add costs vector to node */
		add_node_costs(pbqp_co.pbqp, get_irn_idx(ifg_node), costs_vector);

		/* insert ir_node and pbqp_node into map */
		pmap_insert(pbqp_co.map, ifg_node, get_node(pbqp_co.pbqp, get_irn_idx(ifg_node)));

		if(cntFreeChoosableRegs <= 4) {
			/* node is restricted */
			bitset_set(pbqp_co.restricted_nodes, get_irn_idx(ifg_node));
		}
		else
		{
			/* node is not restricted */
			bitset_clear(pbqp_co.restricted_nodes, get_irn_idx(ifg_node));
		}

	}

	/* add pbqp edges and cost matrix */
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		/* add costs matrix between nodes (interference edge) */
		be_ifg_foreach_neighbour(co->cenv->ifg, neigh_it, ifg_node, if_neighb_node) {
			if(get_edge(pbqp_co.pbqp,get_irn_idx(ifg_node), get_irn_idx(if_neighb_node)) == NULL) {
				/* create costs matrix */
				struct pbqp_matrix *matrix = pbqp_matrix_alloc(pbqp_co.pbqp, number_registers, number_registers);

				/* set costs */
				unsigned row, col;
				for(row = 0; row < number_registers; row++) {
					for(col = 0; col < number_registers; col++) {
						if(row == col) {
							pbqp_matrix_set(matrix, row, col, INF_COSTS);
						}
						else {
							pbqp_matrix_set(matrix, row, col, 0);
						}
					}
				}

				/* add costs matrix to interference edge */
				add_edge_costs(pbqp_co.pbqp, get_irn_idx(ifg_node), get_irn_idx(if_neighb_node) , matrix);
			}
		}

		/* add costs matrix between nodes (affinity edge) */
		affinity_node_t *aff_node = get_affinity_info(co, ifg_node);
		neighb_t *aff_neighb_node;
		if(aff_node != NULL) {
			co_gs_foreach_neighb(aff_node, aff_neighb_node) {
				pmap_entry *ptr_pbqp_node = pmap_find(pbqp_co.map,aff_neighb_node->irn);

				/* ignore Unknowns */
				if(ptr_pbqp_node == NULL) {
					continue;
				}

				if(get_edge(pbqp_co.pbqp, get_irn_idx(aff_node->irn), get_irn_idx(aff_neighb_node->irn)) == NULL) {
					/* create costs matrix */
					struct pbqp_matrix *matrix = pbqp_matrix_alloc(pbqp_co.pbqp, number_registers, number_registers);

					/* set costs */
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

					/* add costs matrix to affinity edge */
					add_edge_costs(pbqp_co.pbqp, get_irn_idx(aff_node->irn), get_irn_idx(aff_neighb_node->irn) , matrix);
				}
			}
		}
	}

	/* create reverse perfect elimination order */
	assure_doms(co->irg);
	dom_tree_walk_irg(co->irg, insert_into_reverse_peo, NULL, &pbqp_co);

#if KAPS_DUMP
	// dump graph before solving pbqp
	FILE *file_before = my_open(co->cenv, "", "-before.html");
	set_dumpfile(pbqp_co.pbqp, file_before);
	pbqp_dump_input(pbqp_co.pbqp);
#endif


	/* solve pbqp problem using a reverse perfect elimination order */
	solve_pbqp_heuristical_co(pbqp_co.pbqp, pbqp_co.rpeo);
    num solution = get_solution(pbqp_co.pbqp);

    assert(solution != INF_COSTS && "No PBQP solution found");

#if KAPS_DUMP
	/* dump graph after solving pbqp */
	FILE *file_after = my_open(co->cenv, "", "-after.html");
	set_dumpfile(pbqp_co.pbqp, file_after);
	pbqp_dump_input(pbqp_co.pbqp);
#endif

	/* coloring ifg */
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		num index = get_node_solution(pbqp_co.pbqp, get_irn_idx(ifg_node));
		const arch_register_t *reg = arch_register_for_index(co->cls, index);
		arch_set_irn_register(ifg_node, reg);
	}

	/* free pbqp resources */
#if KAPS_DUMP
	fclose(file_before);
	fclose(file_after);
#endif
	bitset_free(pbqp_co.ignore_reg);
	pmap_destroy(pbqp_co.map);
	plist_free(pbqp_co.rpeo);
	free_pbqp(pbqp_co.pbqp);

	return 0;
}

void be_init_copypbqp(void)
{
	static co_algo_info copyheur = {
		co_solve_heuristic_pbqp, 0
	};

	be_register_copyopt("pbqp", &copyheur);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copypbqp);

#endif
