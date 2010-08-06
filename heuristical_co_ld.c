/*
 * heuristical_co_ld.c
 *
 *  Created on: 06.05.2010
 *      Author: berschth
 */

#include "config.h"

#include "adt/array.h"
#include "assert.h"
#include "error.h"

#include "bucket.h"
#include "heuristical_co_ld.h"
#include "optimal.h"
#if	KAPS_DUMP
#include "html_dumper.h"
#endif
#include "kaps.h"
#include "matrix.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

#include "plist.h"
#include "timing.h"

static void back_propagate_RI(pbqp *pbqp, pbqp_node *node)
{
	pbqp_edge   *edge;
	pbqp_node   *other;
	pbqp_matrix *mat;
	vector      *vec;
	int          is_src;

	assert(pbqp);
	assert(node);

	(void) pbqp;

	edge = node->edges[0];
	mat = edge->costs;
	is_src = edge->src == node;
	vec = node->costs;

	if (is_src) {
		other = edge->tgt;
		assert(other);

		node->solution = pbqp_matrix_get_col_min_index(mat, other->solution, vec);
	} else {
		other = edge->src;
		assert(other);

		node->solution = pbqp_matrix_get_row_min_index(mat, other->solution, vec);
	}

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br>\n", node->index, node->solution);
	}
#endif
}

static void back_propagate_RII(pbqp *pbqp, pbqp_node *node)
{
	pbqp_edge   *src_edge   = node->edges[0];
	pbqp_edge   *tgt_edge   = node->edges[1];
	int          src_is_src = src_edge->src == node;
	int          tgt_is_src = tgt_edge->src == node;
	pbqp_matrix *src_mat;
	pbqp_matrix *tgt_mat;
	pbqp_node   *src_node;
	pbqp_node   *tgt_node;
	vector      *vec;
	vector      *node_vec;
	unsigned     col_index;
	unsigned     row_index;

	assert(pbqp);

	if (src_is_src) {
		src_node = src_edge->tgt;
	} else {
		src_node = src_edge->src;
	}

	if (tgt_is_src) {
		tgt_node = tgt_edge->tgt;
	} else {
		tgt_node = tgt_edge->src;
	}

	/* Swap nodes if necessary. */
	if (tgt_node->index < src_node->index) {
		pbqp_node *tmp_node;
		pbqp_edge *tmp_edge;

		tmp_node = src_node;
		src_node = tgt_node;
		tgt_node = tmp_node;

		tmp_edge = src_edge;
		src_edge = tgt_edge;
		tgt_edge = tmp_edge;

		src_is_src = src_edge->src == node;
		tgt_is_src = tgt_edge->src == node;
	}

	src_mat = src_edge->costs;
	tgt_mat = tgt_edge->costs;

	node_vec = node->costs;

	row_index = src_node->solution;
	col_index = tgt_node->solution;

	vec = vector_copy(pbqp, node_vec);

	if (src_is_src) {
		vector_add_matrix_col(vec, src_mat, row_index);
	} else {
		vector_add_matrix_row(vec, src_mat, row_index);
	}

	if (tgt_is_src) {
		vector_add_matrix_col(vec, tgt_mat, col_index);
	} else {
		vector_add_matrix_row(vec, tgt_mat, col_index);
	}

	node->solution = vector_get_min_index(vec);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br>\n", node->index, node->solution);
	}
#endif

	obstack_free(&pbqp->obstack, vec);
}

static void back_propagate_RN(pbqp *pbqp, pbqp_node *node)
{
	vector 			*vec		= NULL;
	pbqp_node		*neighbor	= NULL;
	pbqp_edge		*edge		= NULL;
	unsigned 		 idx 		= 0;

	assert(pbqp);

	vec = vector_copy(pbqp, node->costs);

	for(idx = 0; idx < pbqp_node_get_degree(node); idx++) {
		/* get neighbor node */
		edge = node->edges[idx];
		neighbor = edge->src == node ? edge->tgt : edge->src;

		if(edge->src == node)  	/* node is edge src node */
			vector_add_matrix_col(vec, edge->costs, neighbor->solution);
		else					/* node is edge tgt node */
			vector_add_matrix_row(vec, edge->costs, neighbor->solution);
	}

	assert(vector_get_min(vec) != INF_COSTS);
	node->solution = vector_get_min_index(vec);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br>\n", node->index, node->solution);
	}
#endif

	obstack_free(&pbqp->obstack, vec);
}

static void back_propagate_ld(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len   = node_bucket_get_length(reduced_bucket);

	assert(pbqp);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		dump_section(pbqp->dump_file, 2, "Back Propagation");
	}
#endif

	for (node_index = node_len; node_index > 0; --node_index) {
		pbqp_node *node = reduced_bucket[node_index - 1];

		switch (pbqp_node_get_degree(node)) {
			case 1:
				back_propagate_RI(pbqp, node);
				break;
			case 2:
				back_propagate_RII(pbqp, node);
				break;
			default:
				back_propagate_RN(pbqp, node);
				break;
		}
	}
}

static void apply_RN_co_without_selection(pbqp *pbqp, plist_t *rpeo)
{
	pbqp_node   *node         	= NULL;
	pbqp_node	*neighbor	  	= NULL;
	pbqp_edge   *edge         	= NULL;
	unsigned	 idx		  	= 0;

	(void)pbqp;

	do {
		/* get last element from reverse perfect elimination order */
		node = plist_last(rpeo)->data;
		/* remove element from reverse perfect elimination order */
		plist_erase(rpeo, plist_last(rpeo));
		/* insert node at the beginning of rpeo so the rpeo already exits after pbqp solving */
		plist_insert_front(rpeo, node);
	} while(node_is_reduced(node));

	assert(node);
	assert(pbqp_node_get_degree(node) > 2);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RN-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
	}
#endif

	/* Disconnect neighbor nodes */
	for(idx = 0; idx < pbqp_node_get_degree(node); idx++) {
		/* get neighbor node */
		edge = node->edges[idx];
		neighbor = edge->src == node ? edge->tgt : edge->src;

		assert(neighbor != node);

		if(!is_connected(neighbor, edge))
			continue;

		disconnect_edge(neighbor, edge);
		reorder_node(neighbor);
	}

	/* Remove node from old bucket */
	node_bucket_remove(&node_buckets[3], node);

	/* Add node to back propagation list. */
	node_bucket_insert(&reduced_bucket, node);
}



static void apply_heuristic_reductions_co(pbqp *pbqp, plist_t *rpeo)
{
	#if KAPS_TIMING
		/* create timers */
		ir_timer_t *t_edge = ir_timer_new();
		ir_timer_t *t_r1   = ir_timer_new();
		ir_timer_t *t_r2   = ir_timer_new();
		ir_timer_t *t_rn   = ir_timer_new();
	#endif

	for (;;) {
		if (edge_bucket_get_length(edge_bucket) > 0) {
			#if KAPS_TIMING
				ir_timer_start(t_edge);
			#endif

			apply_edge(pbqp);

			#if KAPS_TIMING
				ir_timer_stop(t_edge);
			#endif
		} else if (node_bucket_get_length(node_buckets[1]) > 0) {
			#if KAPS_TIMING
				ir_timer_start(t_r1);
			#endif

			apply_RI(pbqp);

			#if KAPS_TIMING
				ir_timer_stop(t_r1);
			#endif
		} else if (node_bucket_get_length(node_buckets[2]) > 0) {
			#if KAPS_TIMING
				ir_timer_start(t_r2);
			#endif

			apply_RII(pbqp);

			#if KAPS_TIMING
				ir_timer_stop(t_r2);
			#endif
		} else if (node_bucket_get_length(node_buckets[3]) > 0) {
			#if KAPS_TIMING
				ir_timer_start(t_rn);
			#endif

			apply_RN_co_without_selection(pbqp, rpeo);

			#if KAPS_TIMING
				ir_timer_stop(t_rn);
			#endif
		} else {
			#if KAPS_TIMING
				printf("PBQP RE reductions:           %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_edge) / 1000.0);
				printf("PBQP R1 reductions:           %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_r1) / 1000.0);
				printf("PBQP R2 reductions:           %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_r2) / 1000.0);
				printf("PBQP RN reductions:           %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_rn) / 1000.0);
			#endif

			return;
		}
	}
}

void solve_pbqp_heuristical_co_ld(pbqp *pbqp, plist_t *rpeo)
{
	/* Reduce nodes degree ... */
	initial_simplify_edges(pbqp);

	/* ... and put node into bucket representing their degree. */
	fill_node_buckets(pbqp);

	#if KAPS_STATISTIC
		FILE *fh = fopen("solutions.pb", "a");
		fprintf(fh, "Solution");
		fclose(fh);
	#endif

	apply_heuristic_reductions_co(pbqp, rpeo);

	pbqp->solution = determine_solution(pbqp);

	#if KAPS_STATISTIC
		fh = fopen("solutions.pb", "a");
		#if KAPS_USE_UNSIGNED
			fprintf(fh, ": %u RE:%u R0:%u R1:%u R2:%u RM:%u RN/BF:%u\n", pbqp->solution,
					pbqp->num_edges, pbqp->num_r0, pbqp->num_r1, pbqp->num_r2,
					pbqp->num_rm, pbqp->num_rn);
		#else
			fprintf(fh, ": %lld RE:%u R0:%u R1:%u R2:%u RM:%u RN/BF:%u\n", pbqp->solution,
					pbqp->num_edges, pbqp->num_r0, pbqp->num_r1, pbqp->num_r2,
					pbqp->num_rm, pbqp->num_rn);
		#endif
		fclose(fh);
	#endif

	/* Solve reduced nodes. */
	back_propagate_ld(pbqp);

	free_buckets();
}
