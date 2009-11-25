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
#include "timing.h"

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
	pqueue_t  *queue                = new_pqueue();
	pqueue_t  *restrictedNodesQueue = new_pqueue();
	pbqp_co_t *pbqp_co              = data;
	ir_node   *irn;

	sched_foreach(block, irn) {
		if (get_irn_mode(irn) == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(pbqp_co->cls, proj))
					continue;

				// insert proj node into priority queue (descending by their degree in ifg)
				if(bitset_is_set(pbqp_co->restricted_nodes, get_irn_idx(proj))) {
					pqueue_put(restrictedNodesQueue,proj, be_ifg_degree(pbqp_co->ifg,proj));
				}
				else {
					pqueue_put(queue,proj, be_ifg_degree(pbqp_co->ifg,proj));
				}
			}

			/* first insert all restricted nodes */
			while(!pqueue_empty(restrictedNodesQueue)) {
				plist_insert_back(pbqp_co->rpeo, get_node(pbqp_co->pbqp, get_irn_idx(pqueue_pop_front(restrictedNodesQueue))));
			}

			/* insert proj nodes into reverse perfect elimination order (descending by their degree in ifg) */
			while(!pqueue_empty(queue)) {
				plist_insert_back(pbqp_co->rpeo, get_node(pbqp_co->pbqp, get_irn_idx(pqueue_pop_front(queue))));
			}

		} else {
			if (!arch_irn_consider_in_reg_alloc(pbqp_co->cls, irn))
				continue;

			/* insert pbqp node into reverse peo */
			plist_insert_back(pbqp_co->rpeo, get_node(pbqp_co->pbqp, get_irn_idx(irn)));
		}
	}

	/* free priority queues */
	del_pqueue(queue);
	del_pqueue(restrictedNodesQueue);
}

static int co_solve_heuristic_pbqp(copy_opt_t *co) {
	void *nodes_it                       = be_ifg_nodes_iter_alloca(co->cenv->ifg);
	void *neigh_it                       = be_ifg_neighbours_iter_alloca(co->cenv->ifg);
	unsigned number_registers            = co->cls->n_regs;
	unsigned number_nodes                = get_irg_last_idx(co->irg);
	ir_timer_t *t_ra_copymin_pbqp_create = ir_timer_register("be_co_pbqp_create", "copy minimization pbqp create");
	ir_timer_t *t_ra_copymin_pbqp_solve  = ir_timer_register("be_co_pbqp_solve", "copy minimization pbqp solve");
	ir_node *ifg_node;
	ir_node *if_neighb_node;
	pbqp_co_t pbqp_co;
	unsigned row, col;

	#if KAPS_TIMING
	printf("==>> START PBQP TIMING on IRG %s (%s) <<==\n", get_entity_name(get_irg_entity(co->irg)), arch_register_class_name(co->cls));
	#endif

	/* start timer */
	ir_timer_reset_and_start(t_ra_copymin_pbqp_create);

	/* create and initialize data structure for pbqp copy minimization optimization */
	pbqp_co.cls              = co->cls;
	pbqp_co.rpeo             = plist_new();
	pbqp_co.pbqp             = alloc_pbqp(number_nodes);
	pbqp_co.ignore_reg       = bitset_malloc(number_registers);
	pbqp_co.restricted_nodes = bitset_malloc(number_nodes);
	pbqp_co.ifg              = co->cenv->ifg;

	/* no node is restricted at the beginning */
	bitset_clear_all(pbqp_co.restricted_nodes);

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

		if(cntFreeChoosableRegs <= 4) {
			/* node is restricted */
			bitset_set(pbqp_co.restricted_nodes, get_irn_idx(ifg_node));
		}
	}

	/* create costs matrix for interference edges */
	struct pbqp_matrix *ife_matrix = pbqp_matrix_alloc(pbqp_co.pbqp, number_registers, number_registers);
	/* set costs */
	for(row = 0; row < number_registers; row++) {
		for(col = 0; col < number_registers; col++) {
			if(row == col) {
				pbqp_matrix_set(ife_matrix, row, col, INF_COSTS);
			}
			else {
				pbqp_matrix_set(ife_matrix, row, col, 0);
			}
		}
	}

	/* create costs matrix for affinity edges */
	struct pbqp_matrix *afe_matrix = pbqp_matrix_alloc(pbqp_co.pbqp, number_registers, number_registers);
	/* set costs */
	for(row = 0; row < number_registers; row++) {
		for(col = 0; col < number_registers; col++) {
			if(row == col) {
				pbqp_matrix_set(afe_matrix, row, col, 0);
			}
			else {
				pbqp_matrix_set(afe_matrix, row, col, 2);
			}
		}
	}

	/* add pbqp edges and cost matrix */
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		/* add costs matrix between nodes (interference edge) */
		be_ifg_foreach_neighbour(co->cenv->ifg, neigh_it, ifg_node, if_neighb_node) {
			if(get_edge(pbqp_co.pbqp,get_irn_idx(ifg_node), get_irn_idx(if_neighb_node)) == NULL) {
				/* copy matrix */
				struct pbqp_matrix *matrix = pbqp_matrix_copy(pbqp_co.pbqp, ife_matrix);

				/* add costs matrix to interference edge */
				add_edge_costs(pbqp_co.pbqp, get_irn_idx(ifg_node), get_irn_idx(if_neighb_node) , matrix);

			}
		}

		/* add costs matrix between nodes (affinity edge) */
		affinity_node_t *aff_node = get_affinity_info(co, ifg_node);
		neighb_t *aff_neighb_node;
		if(aff_node != NULL) {
			co_gs_foreach_neighb(aff_node, aff_neighb_node) {
				/* ignore Unknowns */
				if(get_node(pbqp_co.pbqp, get_irn_idx(aff_node->irn)) == NULL)
					continue;

				if(get_edge(pbqp_co.pbqp, get_irn_idx(aff_node->irn), get_irn_idx(aff_neighb_node->irn)) == NULL) {
					/* copy matrix */
					struct pbqp_matrix *matrix = pbqp_matrix_copy(pbqp_co.pbqp, afe_matrix);

					/* add costs matrix to affinity edge */
					add_edge_costs(pbqp_co.pbqp, get_irn_idx(aff_node->irn), get_irn_idx(aff_neighb_node->irn) , matrix);
				}
			}
		}
	}

	/* create reverse perfect elimination order */
	assure_doms(co->irg);
	dom_tree_walk_irg(co->irg, insert_into_reverse_peo, NULL, &pbqp_co);

	/* stop timer */
	ir_timer_stop(t_ra_copymin_pbqp_create);

	#if KAPS_DUMP
	// dump graph before solving pbqp
	FILE *file = my_open(co->cenv, "", "-pbqp_copymin.html");
	set_dumpfile(pbqp_co.pbqp, file);
	#endif

	/* start timer */
	ir_timer_reset_and_start(t_ra_copymin_pbqp_solve);

	/* solve pbqp problem using a reverse perfect elimination order */
	solve_pbqp_heuristical_co(pbqp_co.pbqp, pbqp_co.rpeo);
    num solution = get_solution(pbqp_co.pbqp);

    /* stop time */
    ir_timer_stop(t_ra_copymin_pbqp_solve);

	#if KAPS_STATISTIC
    printf("==>> PBQP STATISTIC on IRG %s (%s) <<==\n", get_entity_name(get_irg_entity(co->irg)), arch_register_class_name(co->cls));
	printf("Number of Nodes: %d\n", number_nodes);
	printf("Number of independent edges   : %d\n", pbqp_co.pbqp->num_edges);
	printf("Number of trivial solved nodes: %d\n", pbqp_co.pbqp->num_r0);
	printf("Number of R1 reductions       : %d\n", pbqp_co.pbqp->num_r1);
	printf("Number of R2 reductions       : %d\n", pbqp_co.pbqp->num_r2);
	printf("Number of RN reductions       : %d\n", pbqp_co.pbqp->num_rn);
    #endif

	#if KAPS_TIMING
	printf("%-20s: %8.3lf msec\n" , ir_timer_get_description(t_ra_copymin_pbqp_create), (double)ir_timer_elapsed_usec(t_ra_copymin_pbqp_create) / 1000.0);
	printf("%-20s: %8.3lf msec\n" , ir_timer_get_description(t_ra_copymin_pbqp_solve), (double)ir_timer_elapsed_usec(t_ra_copymin_pbqp_solve) / 1000.0);
	printf("==>> END PBQP TIMING on IRG %s (%s) <<==\n", get_entity_name(get_irg_entity(co->irg)), arch_register_class_name(co->cls));
	#endif

	assert(solution != INF_COSTS && "No PBQP solution found");

	/* coloring ifg */
	be_ifg_foreach_node(co->cenv->ifg, nodes_it, ifg_node) {
		num index = get_node_solution(pbqp_co.pbqp, get_irn_idx(ifg_node));
		const arch_register_t *reg = arch_register_for_index(co->cls, index);
		arch_set_irn_register(ifg_node, reg);
	}

	/* free allocated memory */
	#if KAPS_DUMP
	fclose(file);
	#endif
	bitset_free(pbqp_co.ignore_reg);
	bitset_free(pbqp_co.restricted_nodes);
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
