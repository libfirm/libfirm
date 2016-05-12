/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Heuristic PBQP solver.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include <assert.h>

#include "adt/array.h"

#include "bucket.h"
#include "heuristical.h"
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

#include "timing.h"

static void apply_RN(pbqp_t *pbqp)
{
	/* We want to reduce a node with maximum degree. */
	pbqp_node_t *node = get_node_with_max_degree();
	assert(pbqp_node_get_degree(node) > 2);

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

static void apply_heuristic_reductions(pbqp_t *pbqp)
{
	for (;;) {
		if (edge_bucket_get_length(edge_bucket) > 0) {
			apply_edge(pbqp);
		} else if (node_bucket_get_length(node_buckets[1]) > 0) {
			apply_RI(pbqp);
		} else if (node_bucket_get_length(node_buckets[2]) > 0) {
			apply_RII(pbqp);
		} else if (node_bucket_get_length(node_buckets[3]) > 0) {
			apply_RN(pbqp);
		} else {
			return;
		}
	}
}

void solve_pbqp_heuristical(pbqp_t *pbqp)
{
#ifndef NDEBUG
	assert(pbqp);
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

	apply_heuristic_reductions(pbqp);

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
