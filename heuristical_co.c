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
 * @brief   Heuristic PBQP solver for SSA-based register allocation.
 * @date    18.09.2009
 * @author  Thomas Bersch
 * @version $Id$
 */
#include "config.h"

#include "adt/array.h"
#include "assert.h"
#include "error.h"

#include "bucket.h"
#include "heuristical_co.h"
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

static void apply_RN_co(pbqp *pbqp, plist_t *rpeo)
{
	pbqp_node   *node         = NULL;
	unsigned     min_index    = 0;

	assert(pbqp);

	/* We want to reduce the first node in reverse perfect elimination order. */
	do {
		/* get first element from reverse perfect elimination order */
		node = plist_first(rpeo)->data;
		/* remove element from reverse perfect elimination order */
		plist_erase(rpeo, plist_first(rpeo));
		/* insert node at the end of rpeo so the rpeo already exits after pbqp solving */
		plist_insert_back(rpeo, node);
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
#if KAPS_STATISTIC
	/* Check whether we can merge a neighbor into the current node. */
	for (min_index = 0; min_index < pbqp_node_get_degree(node); ++min_index) {
		check_melting_possibility(pbqp, node->edges[min_index]);
	}
#endif

	min_index = get_local_minimal_alternative(pbqp, node);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br><br>\n",
					node->index, min_index);
	}
#endif

#if KAPS_STATISTIC
	if (dump == 0) {
		FILE *fh = fopen("solutions.pb", "a");
		fprintf(fh, "[%u]", min_index);
		fclose(fh);
		pbqp->num_rn++;
	}
#endif

	/* Now that we found the local minimum set all other costs to infinity. */
	select_alternative(node, min_index);
}

static void apply_heuristic_reductions_co(pbqp *pbqp, plist_t *rpeo)
{
	#if KAPS_TIMING
		/* create timers */
		ir_timer_t *t_edge = ir_timer_register("be_pbqp_edges", "pbqp reduce independent edges");
		ir_timer_t *t_r0 = ir_timer_register("be_pbqp_r0", "pbqp R0 reductions");
		ir_timer_t *t_r1 = ir_timer_register("be_pbqp_r1", "pbqp R1 reductions");
		ir_timer_t *t_r2 = ir_timer_register("be_pbqp_r2", "pbqp R2 reductions");
		ir_timer_t *t_rn = ir_timer_register("be_pbqp_rN", "pbqp RN reductions");

		/* reset timers */
		ir_timer_reset(t_edge);
		ir_timer_reset(t_r0);
		ir_timer_reset(t_r1);
		ir_timer_reset(t_r2);
		ir_timer_reset(t_rn);
	#endif

	for (;;) {
		if (edge_bucket_get_length(edge_bucket) > 0) {
			#if KAPS_TIMING
				ir_timer_start(t_r0);
			#endif

			apply_edge(pbqp);

			#if KAPS_TIMING
				ir_timer_stop(t_r0);
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

			apply_RN_co(pbqp, rpeo);

			#if KAPS_TIMING
				ir_timer_stop(t_rn);
			#endif
		} else {
			#if KAPS_TIMING
				printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_edge), (double)ir_timer_elapsed_usec(t_edge) / 1000.0);
				printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_r0), (double)ir_timer_elapsed_usec(t_r0) / 1000.0);
				printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_r1), (double)ir_timer_elapsed_usec(t_r1) / 1000.0);
				printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_r2), (double)ir_timer_elapsed_usec(t_r2) / 1000.0);
				printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_rn), (double)ir_timer_elapsed_usec(t_rn) / 1000.0);
			#endif

			return;
		}
	}
}

void solve_pbqp_heuristical_co(pbqp *pbqp, plist_t *rpeo)
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
		fprintf(fh, ": %lld RE:%u R0:%u R1:%u R2:%u RN/BF:%u\n", pbqp->solution,
					pbqp->num_edges, pbqp->num_r0, pbqp->num_r1, pbqp->num_r2,
					pbqp->num_rn);
		fclose(fh);
	#endif

	/* Solve reduced nodes. */
	back_propagate(pbqp);

	free_buckets();
}
