/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Heuristic PBQP solver for SSA-based register allocation.
 * @date    18.09.2009
 * @author  Thomas Bersch
 */
#include <assert.h>

#include "adt/array.h"

#include "bucket.h"
#include "heuristical_co.h"
#include "optimal.h"
#if KAPS_DUMP
#include "html_dumper.h"
#endif
#include "kaps.h"
#include "matrix.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

#include "pdeq.h"
#include "timing.h"

static void merge_into_RN_node(pbqp_t *pbqp, deq_t *rpeo)
{
	pbqp_node_t *node;

	/* We want to reduce the first node in reverse perfect elimination order. */
	do {
		/* get first element from reverse perfect elimination order */
		node = deq_pop_pointer_left(pbqp_node_t, rpeo);
		/* insert node at the end of rpeo so the rpeo already exits after pbqp
		 * solving */
		deq_push_pointer_right(rpeo, node);
	} while (node_is_reduced(node));

	assert(pbqp_node_get_degree(node) > 2);

	/* Check whether we can merge a neighbor into the current node. */
	apply_RM(pbqp, node);
}

static void apply_RN_co(pbqp_t *pbqp)
{
	pbqp_node_t *node = merged_node;
	merged_node = NULL;

	if (node_is_reduced(node))
		return;

#if KAPS_DUMP
	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RN-Reduction of Node n%u", node->index);
		pbqp_dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
	}
#endif

	unsigned min_index = get_local_minimal_alternative(pbqp, node);

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%u is set to %u<br><br>\n", node->index, min_index);
	}
#endif

#if KAPS_STATISTIC
		FILE *fh = fopen("solutions.pb", "a");
		fprintf(fh, "[%u]", min_index);
		fclose(fh);
		pbqp->num_rn++;
#endif

	/* Now that we found the local minimum set all other costs to infinity. */
	select_alternative(node, min_index);
}

static void apply_heuristic_reductions_co(pbqp_t *pbqp, deq_t *rpeo)
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
		} else if (merged_node != NULL) {
			#if KAPS_TIMING
				ir_timer_start(t_rn);
			#endif

			apply_RN_co(pbqp);

			#if KAPS_TIMING
				ir_timer_stop(t_rn);
			#endif
		} else if (node_bucket_get_length(node_buckets[3]) > 0) {
			#if KAPS_TIMING
				ir_timer_start(t_rn);
			#endif

			merge_into_RN_node(pbqp, rpeo);

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

void solve_pbqp_heuristical_co(pbqp_t *pbqp, deq_t *rpeo)
{
#ifndef NDEBUG
	assert(pbqp);
	assert(rpeo);
	assert(pbqp->solution == INF_COSTS && "PBQP already solved");
	pbqp->solution = 0;
#endif

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
	back_propagate(pbqp);

	free_buckets();
}
