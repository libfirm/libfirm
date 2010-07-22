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
 * @brief   Brute force PBQP solver.
 * @date    02.12.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#include "config.h"

#include "assert.h"
#include "error.h"

#include "bucket.h"
#include "brute_force.h"
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

#if KAPS_STATISTIC
static int dump = 0;
#endif

/* Forward declarations. */
static void apply_Brute_Force(pbqp *pbqp);

static void apply_brute_force_reductions(pbqp *pbqp)
{
	for (;;) {
		if (edge_bucket_get_length(edge_bucket) > 0) {
			apply_edge(pbqp);
		} else if (node_bucket_get_length(node_buckets[1]) > 0) {
			apply_RI(pbqp);
		} else if (node_bucket_get_length(node_buckets[2]) > 0) {
			apply_RII(pbqp);
		} else if (node_bucket_get_length(node_buckets[3]) > 0) {
			apply_Brute_Force(pbqp);
		} else {
			return;
		}
	}
}

static unsigned get_minimal_alternative(pbqp *pbqp, pbqp_node *node)
{
	vector      *node_vec;
	unsigned     node_index;
	unsigned     node_len;
	unsigned     min_index    = 0;
	num          min          = INF_COSTS;
	unsigned     bucket_index;

	assert(pbqp);
	assert(node);
	node_vec     = node->costs;
	node_len     = node_vec->len;
	bucket_index = node->bucket_index;

	for (node_index = 0; node_index < node_len; ++node_index) {
		pbqp_node_bucket bucket_deg3;
		num              value;
		unsigned         bucket_0_length;
		unsigned         bucket_red_length;

		char *tmp = obstack_finish(&pbqp->obstack);

		node_bucket_init(&bucket_deg3);

		/* Some node buckets and the edge bucket should be empty. */
		assert(node_bucket_get_length(node_buckets[1]) == 0);
		assert(node_bucket_get_length(node_buckets[2]) == 0);
		assert(edge_bucket_get_length(edge_bucket)     == 0);

		/* char *tmp = obstack_finish(&pbqp->obstack); */

		/* Save current PBQP state. */
		node_bucket_copy(&bucket_deg3, node_buckets[3]);
		node_bucket_shrink(&node_buckets[3], 0);
		node_bucket_deep_copy(pbqp, &node_buckets[3], bucket_deg3);
		node_bucket_update(pbqp, node_buckets[3]);
		bucket_0_length   = node_bucket_get_length(node_buckets[0]);
		bucket_red_length = node_bucket_get_length(reduced_bucket);

		/* Select alternative and solve PBQP recursively. */
		select_alternative(node_buckets[3][bucket_index], node_index);
		apply_brute_force_reductions(pbqp);

		value = determine_solution(pbqp);

		if (value < min) {
			min = value;
			min_index = node_index;
		}

		/* Some node buckets and the edge bucket should still be empty. */
		assert(node_bucket_get_length(node_buckets[1]) == 0);
		assert(node_bucket_get_length(node_buckets[2]) == 0);
		assert(edge_bucket_get_length(edge_bucket)     == 0);

		/* Clear modified buckets... */
		node_bucket_shrink(&node_buckets[3], 0);

		/* ... and restore old PBQP state. */
		node_bucket_shrink(&node_buckets[0], bucket_0_length);
		node_bucket_shrink(&reduced_bucket, bucket_red_length);
		node_bucket_copy(&node_buckets[3], bucket_deg3);
		node_bucket_update(pbqp, node_buckets[3]);

		/* Free copies. */
		/* obstack_free(&pbqp->obstack, tmp); */
		node_bucket_free(&bucket_deg3);

		obstack_free(&pbqp->obstack, tmp);
	}

	return min_index;
}

static void apply_Brute_Force(pbqp *pbqp)
{
	pbqp_node   *node         = NULL;
	unsigned     min_index    = 0;

	assert(pbqp);

	/* We want to reduce a node with maximum degree. */
	node = get_node_with_max_degree();
	assert(node);
	assert(pbqp_node_get_degree(node) > 2);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "BF-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
	}
#endif

#if KAPS_STATISTIC
	dump++;
#endif

	min_index = get_minimal_alternative(pbqp, node);
	node = pbqp->nodes[node->index];

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br><br>\n",
					node->index, min_index);
	}
#endif

#if KAPS_STATISTIC
	dump--;
	if (dump == 0) {
		FILE *fh = fopen("solutions.pb", "a");
		fprintf(fh, "[%u]", min_index);
		fclose(fh);
		pbqp->num_bf++;
	}
#endif

	/* Now that we found the minimum set all other costs to infinity. */
	select_alternative(node, min_index);
}



static void back_propagate_RI(pbqp *pbqp, pbqp_node *node)
{
	pbqp_edge   *edge;
	pbqp_node   *other;
	pbqp_matrix *mat;
	vector      *vec;
	int          is_src;

	assert(pbqp);
	assert(node);

	edge = node->edges[0];
	mat = edge->costs;
	is_src = edge->src == node;
	vec = node->costs;

	if (is_src) {
		other = edge->tgt;
		assert(other);

		/* Update pointer for brute force solver. */
		other = pbqp->nodes[other->index];

		node->solution = pbqp_matrix_get_col_min_index(mat, other->solution, vec);
	} else {
		other = edge->src;
		assert(other);

		/* Update pointer for brute force solver. */
		other = pbqp->nodes[other->index];

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

	/* Update pointer for brute force solver. */
	src_node = pbqp->nodes[src_node->index];
	tgt_node = pbqp->nodes[tgt_node->index];

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

static void back_propagate_brute_force(pbqp *pbqp)
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
				panic("Only nodes with degree one or two should be in this bucket");
				break;
		}
	}
}

void solve_pbqp_brute_force(pbqp *pbqp)
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

	apply_brute_force_reductions(pbqp);

	pbqp->solution = determine_solution(pbqp);

#if KAPS_STATISTIC
	fh = fopen("solutions.pb", "a");
	fprintf(fh, ": %lld RE:%u R0:%u R1:%u R2:%u RN/BF:%u\n", pbqp->solution,
			pbqp->num_edges, pbqp->num_r0, pbqp->num_r1, pbqp->num_r2,
			pbqp->num_bf);
	fclose(fh);
#endif

	/* Solve reduced nodes. */
	back_propagate_brute_force(pbqp);

	free_buckets();
}
