/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Optimal reductions and helper functions.
 * @date    28.12.2009
 * @author  Sebastian Buchwald
 */
#include "kaps.h"

#include "adt/array.h"
#include "bucket.h"
#include "matrix.h"
#include "optimal.h"
#include "panic.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "timing.h"
#include "vector.h"
#include <assert.h>
#include <stdbool.h>

#if KAPS_DUMP
#include "html_dumper.h"
#endif

pbqp_edge_t **edge_bucket;
static pbqp_edge_t **rm_bucket;
pbqp_node_t **node_buckets[4];
pbqp_node_t **reduced_bucket = NULL;
pbqp_node_t  *merged_node = NULL;
static int  buckets_filled = 0;

static void insert_into_edge_bucket(pbqp_edge_t *edge)
{
	if (edge_bucket_contains(edge_bucket, edge)) {
		/* Edge is already inserted. */
		return;
	}

	edge_bucket_insert(&edge_bucket, edge);
}

static void insert_into_rm_bucket(pbqp_edge_t *edge)
{
	if (edge_bucket_contains(rm_bucket, edge)) {
		/* Edge is already inserted. */
		return;
	}

	edge_bucket_insert(&rm_bucket, edge);
}

static void init_buckets(void)
{
	edge_bucket_init(&edge_bucket);
	edge_bucket_init(&rm_bucket);
	node_bucket_init(&reduced_bucket);

	for (int i = 0; i < 4; ++i) {
		node_bucket_init(&node_buckets[i]);
	}
}

void free_buckets(void)
{
	for (int i = 0; i < 4; ++i) {
		node_bucket_free(&node_buckets[i]);
	}

	edge_bucket_free(&edge_bucket);
	edge_bucket_free(&rm_bucket);
	node_bucket_free(&reduced_bucket);

	buckets_filled = 0;
}

void fill_node_buckets(pbqp_t *pbqp)
{
	unsigned node_len = pbqp->num_nodes;

	#if KAPS_TIMING
		ir_timer_t *t_fill_buckets = ir_timer_new();
		ir_timer_start(t_fill_buckets);
	#endif

	for (unsigned node_index = 0; node_index < node_len; ++node_index) {
		unsigned     degree;
		pbqp_node_t *node = get_node(pbqp, node_index);

		if (!node) continue;

		degree = pbqp_node_get_degree(node);

		/* We have only one bucket for nodes with arity >= 3. */
		if (degree > 3) {
			degree = 3;
		}

		node_bucket_insert(&node_buckets[degree], node);
	}

	buckets_filled = 1;

	#if KAPS_TIMING
		ir_timer_stop(t_fill_buckets);
		printf("PBQP Fill Nodes into buckets: %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_fill_buckets) / 1000.0);
	#endif
}

static void normalize_towards_source(pbqp_edge_t *edge)
{
	pbqp_matrix_t *mat          = edge->costs;
	pbqp_node_t   *src_node     = edge->src;
	pbqp_node_t   *tgt_node     = edge->tgt;
	vector_t      *src_vec      = src_node->costs;
	vector_t      *tgt_vec      = tgt_node->costs;
	unsigned       src_len      = src_vec->len;
	unsigned       new_infinity = 0;

	assert(src_len > 0);
	assert(tgt_vec->len > 0);

	/* Normalize towards source node. */
	for (unsigned src_index = 0; src_index < src_len; ++src_index) {
		num min = pbqp_matrix_get_row_min(mat, src_index, tgt_vec);

		if (min != 0) {
			if (src_vec->entries[src_index].data == INF_COSTS) {
				pbqp_matrix_set_row_value(mat, src_index, 0);
				continue;
			}

			pbqp_matrix_sub_row_value(mat, src_index, tgt_vec, min);
			src_vec->entries[src_index].data = pbqp_add(src_vec->entries[src_index].data, min);

			if (min == INF_COSTS) {
				new_infinity = 1;
			}
		}
	}

	if (new_infinity) {
		unsigned edge_len = pbqp_node_get_degree(src_node);

		for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge_t *edge_candidate = src_node->edges[edge_index];

			if (edge_candidate != edge) {
				insert_into_edge_bucket(edge_candidate);
			}
		}
	}
}

static void normalize_towards_target(pbqp_edge_t *edge)
{
	pbqp_matrix_t *mat          = edge->costs;
	pbqp_node_t   *src_node     = edge->src;
	pbqp_node_t   *tgt_node     = edge->tgt;
	vector_t      *src_vec      = src_node->costs;
	vector_t      *tgt_vec      = tgt_node->costs;
	unsigned       tgt_len      = tgt_vec->len;
	unsigned       new_infinity = 0;

	assert(src_vec->len > 0);
	assert(tgt_len > 0);


	/* Normalize towards target node. */
	for (unsigned tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
		num min = pbqp_matrix_get_col_min(mat, tgt_index, src_vec);

		if (min != 0) {
			if (tgt_vec->entries[tgt_index].data == INF_COSTS) {
				pbqp_matrix_set_col_value(mat, tgt_index, 0);
				continue;
			}

			pbqp_matrix_sub_col_value(mat, tgt_index, src_vec, min);
			tgt_vec->entries[tgt_index].data = pbqp_add(tgt_vec->entries[tgt_index].data, min);

			if (min == INF_COSTS) {
				new_infinity = 1;
			}
		}
	}

	if (new_infinity) {
		unsigned edge_len = pbqp_node_get_degree(tgt_node);

		for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge_t *edge_candidate = tgt_node->edges[edge_index];

			if (edge_candidate != edge) {
				insert_into_edge_bucket(edge_candidate);
			}
		}
	}
}

/**
 * Tries to apply RM for the source node of the given edge.
 *
 * Checks whether the source node of edge can be merged into the target node of
 * edge, and performs the merge, if possible.
 */
static void merge_source_into_target(pbqp_t *pbqp, pbqp_edge_t *edge)
{
	pbqp_matrix_t *mat      = edge->costs;
	pbqp_node_t   *src_node = edge->src;
	pbqp_node_t   *tgt_node = edge->tgt;
	vector_t      *src_vec  = src_node->costs;
	vector_t      *tgt_vec  = tgt_node->costs;
	unsigned       src_len  = src_vec->len;
	unsigned       tgt_len  = tgt_vec->len;

	/* Matrizes are normalized. */
	assert(src_len > 1);
	assert(tgt_len > 1);

	unsigned *mapping = NEW_ARR_F(unsigned, tgt_len);

	/* Check that each column has at most one zero entry. */
	for (unsigned tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
		if (tgt_vec->entries[tgt_index].data == INF_COSTS)
			continue;

		unsigned onlyOneZero = 0;

		for (unsigned src_index = 0; src_index < src_len; ++src_index) {
			if (src_vec->entries[src_index].data == INF_COSTS)
				continue;

			if (mat->entries[src_index * tgt_len + tgt_index] == INF_COSTS)
				continue;

			/* Matrix entry is finite. */
			if (onlyOneZero) {
				DEL_ARR_F(mapping);
				return;
			}

			onlyOneZero        = 1;
			mapping[tgt_index] = src_index;
		}
	}

	/* We know that we can merge the source node into the target node. */
	unsigned edge_len = pbqp_node_get_degree(src_node);

#if KAPS_STATISTIC
	pbqp->num_rm++;
#endif

#if KAPS_DUMP
	if (pbqp->dump_file) {
		char txt[100];
		sprintf(txt, "Merging n%u into n%u", src_node->index, tgt_node->index);
		pbqp_dump_section(pbqp->dump_file, 3, txt);
	}
#endif

	/* Reconnect the source's edges with the target node. */
	for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge_t *old_edge = src_node->edges[edge_index];
		assert(old_edge);

		if (old_edge == edge)
			continue;

		pbqp_matrix_t *old_matrix = old_edge->costs;
		pbqp_node_t   *other_node;
		unsigned       other_len;

		if (old_edge->tgt == src_node) {
			other_node = old_edge->src;
			other_len  = old_matrix->rows;
		} else {
			other_node = old_edge->tgt;
			other_len  = old_matrix->cols;
		}

		vector_t      *other_vec  = other_node->costs;
		pbqp_matrix_t *new_matrix = pbqp_matrix_alloc(pbqp, tgt_len, other_len);

		/* Source node selects the column of the old_matrix. */
		if (old_edge->tgt == src_node) {
			for (unsigned tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
				if (tgt_vec->entries[tgt_index].data == INF_COSTS)
					continue;

				unsigned src_index = mapping[tgt_index];

				for (unsigned other_index = 0; other_index < other_len; ++other_index) {
					if (other_vec->entries[other_index].data == INF_COSTS)
						continue;

					new_matrix->entries[tgt_index * other_len + other_index] = old_matrix->entries[other_index * src_len + src_index];
				}
			}
		} else {
			/* Source node selects the row of the old_matrix. */
			for (unsigned tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
				if (tgt_vec->entries[tgt_index].data == INF_COSTS)
					continue;

				unsigned src_index = mapping[tgt_index];

				for (unsigned other_index = 0; other_index < other_len; ++other_index) {
					if (other_vec->entries[other_index].data == INF_COSTS)
						continue;

					new_matrix->entries[tgt_index * other_len + other_index] = old_matrix->entries[src_index * other_len + other_index];
				}
			}
		}

		pbqp_edge_t *new_edge = get_edge(pbqp, tgt_node->index, other_node->index);

		add_edge_costs(pbqp, tgt_node->index, other_node->index, new_matrix);

		if (new_edge == NULL) {
			reorder_node_after_edge_insertion(tgt_node);
			reorder_node_after_edge_insertion(other_node);
		}

		delete_edge(old_edge);

		new_edge = get_edge(pbqp, tgt_node->index, other_node->index);
		simplify_edge(pbqp, new_edge);

		insert_into_rm_bucket(new_edge);
	}

#if KAPS_STATISTIC
	pbqp->num_r1--;
#endif
	DEL_ARR_F(mapping);
}

/**
 * Tries to apply RM for the target node of the given edge.
 *
 * Checks whether the target node of edge can be merged into the source node of
 * edge, and performs the merge, if possible.
 */
static void merge_target_into_source(pbqp_t *pbqp, pbqp_edge_t *edge)
{
	pbqp_node_t *src_node = edge->src;
	pbqp_node_t *tgt_node = edge->tgt;
	vector_t    *src_vec  = src_node->costs;
	vector_t    *tgt_vec  = tgt_node->costs;
	unsigned     src_len  = src_vec->len;
	unsigned     tgt_len  = tgt_vec->len;

	/* Matrizes are normalized. */
	assert(src_len > 1);
	assert(tgt_len > 1);

	pbqp_matrix_t *mat     = edge->costs;
	unsigned      *mapping = NEW_ARR_F(unsigned, src_len);

	/* Check that each row has at most one zero entry. */
	for (unsigned src_index = 0; src_index < src_len; ++src_index) {
		if (src_vec->entries[src_index].data == INF_COSTS)
			continue;

		unsigned onlyOneZero = 0;

		for (unsigned tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
			if (tgt_vec->entries[tgt_index].data == INF_COSTS)
				continue;

			if (mat->entries[src_index * tgt_len + tgt_index] == INF_COSTS)
				continue;

			/* Matrix entry is finite. */
			if (onlyOneZero) {
				DEL_ARR_F(mapping);
				return;
			}

			onlyOneZero        = 1;
			mapping[src_index] = tgt_index;
		}
	}

	/* We know that we can merge the target node into the source node. */
	unsigned edge_len = pbqp_node_get_degree(tgt_node);

#if KAPS_STATISTIC
	pbqp->num_rm++;
#endif

#if KAPS_DUMP
	if (pbqp->dump_file) {
		char txt[100];
		sprintf(txt, "Merging n%u into n%u", tgt_node->index, src_node->index);
		pbqp_dump_section(pbqp->dump_file, 3, txt);
	}
#endif

	/* Reconnect the target's edges with the source node. */
	for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge_t *old_edge = tgt_node->edges[edge_index];
		assert(old_edge);

		if (old_edge == edge)
			continue;

		pbqp_matrix_t *old_matrix = old_edge->costs;
		pbqp_node_t   *other_node;
		unsigned       other_len;

		if (old_edge->tgt == tgt_node) {
			other_node = old_edge->src;
			other_len  = old_matrix->rows;
		} else {
			other_node = old_edge->tgt;
			other_len  = old_matrix->cols;
		}

		vector_t      *other_vec  = other_node->costs;
		pbqp_matrix_t *new_matrix = pbqp_matrix_alloc(pbqp, src_len, other_len);

		/* Target node selects the column of the old_matrix. */
		if (old_edge->tgt == tgt_node) {
			for (unsigned src_index = 0; src_index < src_len; ++src_index) {
				if (src_vec->entries[src_index].data == INF_COSTS)
					continue;

				unsigned tgt_index = mapping[src_index];

				for (unsigned other_index = 0; other_index < other_len; ++other_index) {
					if (other_vec->entries[other_index].data == INF_COSTS)
						continue;

					new_matrix->entries[src_index * other_len + other_index] = old_matrix->entries[other_index * tgt_len + tgt_index];
				}
			}
		} else {
			/* Source node selects the row of the old_matrix. */
			for (unsigned src_index = 0; src_index < src_len; ++src_index) {
				if (src_vec->entries[src_index].data == INF_COSTS)
					continue;

				unsigned tgt_index = mapping[src_index];

				for (unsigned other_index = 0; other_index < other_len; ++other_index) {
					if (other_vec->entries[other_index].data == INF_COSTS)
						continue;

					new_matrix->entries[src_index * other_len + other_index] = old_matrix->entries[tgt_index * other_len + other_index];
				}
			}
		}

		pbqp_edge_t *new_edge = get_edge(pbqp, src_node->index, other_node->index);

		add_edge_costs(pbqp, src_node->index, other_node->index, new_matrix);

		if (new_edge == NULL) {
			reorder_node_after_edge_insertion(src_node);
			reorder_node_after_edge_insertion(other_node);
		}

		delete_edge(old_edge);

		new_edge = get_edge(pbqp, src_node->index, other_node->index);
		simplify_edge(pbqp, new_edge);

		insert_into_rm_bucket(new_edge);
	}

#if KAPS_STATISTIC
	pbqp->num_r1--;
#endif
	DEL_ARR_F(mapping);
}

/**
 * Merge neighbors into the given node.
 */
void apply_RM(pbqp_t *pbqp, pbqp_node_t *node)
{
	pbqp_edge_t **edges    = node->edges;
	unsigned      edge_len = pbqp_node_get_degree(node);

	/* Check all incident edges. */
	for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge_t *edge = edges[edge_index];

		insert_into_rm_bucket(edge);
	}

	/* ALAP: Merge neighbors into given node. */
	while (edge_bucket_get_length(rm_bucket) > 0) {
		pbqp_edge_t *edge = edge_bucket_pop(&rm_bucket);

		/* If the edge is not deleted: Try a merge. */
		if (edge->src == node)
			merge_target_into_source(pbqp, edge);
		else if (edge->tgt == node)
			merge_source_into_target(pbqp, edge);
	}

	merged_node = node;
}

void reorder_node_after_edge_deletion(pbqp_node_t *node)
{
	unsigned    degree     = pbqp_node_get_degree(node);
	/* Assume node lost one incident edge. */
	unsigned    old_degree = degree + 1;

	if (!buckets_filled)
		return;

	/* Same bucket as before */
	if (degree > 2)
		return;

	/* Delete node from old bucket... */
	node_bucket_remove(&node_buckets[old_degree], node);

	/* ..and add to new one. */
	node_bucket_insert(&node_buckets[degree], node);
}

void reorder_node_after_edge_insertion(pbqp_node_t *node)
{
	unsigned    degree     = pbqp_node_get_degree(node);
	/* Assume node lost one incident edge. */
	unsigned    old_degree = degree - 1;

	if (!buckets_filled)
		return;

	/* Same bucket as before */
	if (old_degree > 2)
		return;

	/* Delete node from old bucket... */
	node_bucket_remove(&node_buckets[old_degree], node);

	/* ..and add to new one. */
	node_bucket_insert(&node_buckets[degree], node);
}

void simplify_edge(pbqp_t *pbqp, pbqp_edge_t *edge)
{
	(void)pbqp;

	/* If edge are already deleted, we have nothing to do. */
	if (is_deleted(edge))
		return;

	pbqp_node_t *src_node = edge->src;
	pbqp_node_t *tgt_node = edge->tgt;

	assert(src_node);
	assert(tgt_node);

#if KAPS_DUMP
	if (pbqp->dump_file) {
		char txt[100];
		sprintf(txt, "Simplification of Edge n%u-n%u", src_node->index, tgt_node->index);
		pbqp_dump_section(pbqp->dump_file, 3, txt);
	}
#endif

	vector_t *src_vec = src_node->costs;
	vector_t *tgt_vec = tgt_node->costs;
	assert(src_vec->len > 0);
	assert(tgt_vec->len > 0);

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("Input:<br>\n", pbqp->dump_file);
		pbqp_dump_simplifyedge(pbqp, edge);
	}
#endif

	normalize_towards_source(edge);
	normalize_towards_target(edge);

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("<br>\nOutput:<br>\n", pbqp->dump_file);
		pbqp_dump_simplifyedge(pbqp, edge);
	}
#endif

	pbqp_matrix_t *mat = edge->costs;

	if (pbqp_matrix_is_zero(mat, src_vec, tgt_vec)) {
#if KAPS_DUMP
		if (pbqp->dump_file) {
			fputs("edge has been eliminated<br>\n", pbqp->dump_file);
		}
#endif

#if KAPS_STATISTIC
		pbqp->num_edges++;
#endif

		delete_edge(edge);
	}
}

void initial_simplify_edges(pbqp_t *pbqp)
{
	#if KAPS_TIMING
		ir_timer_t *t_int_simpl = ir_timer_new();
		ir_timer_start(t_int_simpl);
	#endif

#if KAPS_DUMP
	if (pbqp->dump_file) {
		pbqp_dump_input(pbqp);
		pbqp_dump_section(pbqp->dump_file, 1, "2. Simplification of Cost Matrices");
	}
#endif

	unsigned node_len = pbqp->num_nodes;

	init_buckets();

	/* First simplify all edges. */
	for (unsigned node_index = 0; node_index < node_len; ++node_index) {
		pbqp_node_t *node = get_node(pbqp, node_index);

		if (!node)
			continue;

		pbqp_edge_t **edges    = node->edges;
		unsigned      edge_len = pbqp_node_get_degree(node);

		for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge_t *edge = edges[edge_index];

			/* Simplify only once per edge. */
			if (node != edge->src)
				continue;

			simplify_edge(pbqp, edge);
		}
	}

	#if KAPS_TIMING
		ir_timer_stop(t_int_simpl);
		printf("PBQP Initial simplify edges:  %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_int_simpl) / 1000.0);
	#endif
}

num determine_solution(pbqp_t *pbqp)
{
	(void)pbqp;

#if KAPS_TIMING
	ir_timer_t *t_det_solution = ir_timer_new();
	ir_timer_reset_and_start(t_det_solution);
#endif

#if KAPS_DUMP
	FILE *file = pbqp->dump_file;

	if (file) {
		pbqp_dump_section(file, 1, "4. Determine Solution/Minimum");
		pbqp_dump_section(file, 2, "4.1. Trivial Solution");
	}
#endif

	/* Solve trivial nodes and calculate solution. */
	unsigned node_len = node_bucket_get_length(node_buckets[0]);

#if KAPS_STATISTIC
	pbqp->num_r0 = node_len;
#endif

	num solution = 0;

	for (unsigned node_index = 0; node_index < node_len; ++node_index) {
		pbqp_node_t *node = node_buckets[0][node_index];

		node->solution = vector_get_min_index(node->costs);
		solution       = pbqp_add(solution, node->costs->entries[node->solution].data);

#if KAPS_DUMP
		if (file) {
			fprintf(file, "node n%u is set to %u<br>\n", node->index, node->solution);
			pbqp_dump_node(file, node);
		}
#endif
	}

#if KAPS_DUMP
	if (file) {
		pbqp_dump_section(file, 2, "Minimum");
#if KAPS_USE_UNSIGNED
		fprintf(file, "Minimum is equal to %u.", solution);
#else
		fprintf(file, "Minimum is equal to %lld.", solution);
#endif
	}
#endif

#if KAPS_TIMING
	ir_timer_stop(t_det_solution);
	printf("PBQP Determine Solution:      %10.3lf msec\n", (double)ir_timer_elapsed_usec(t_det_solution) / 1000.0);
#endif

	return solution;
}

static void back_propagate_RI(pbqp_t *pbqp, pbqp_node_t *node)
{
	(void)pbqp;

	pbqp_edge_t   *edge   = node->edges[0];
	pbqp_matrix_t *mat    = edge->costs;
	bool           is_src = edge->src == node;
	vector_t      *vec    = node->costs;

	if (is_src) {
		pbqp_node_t *other = edge->tgt;
		node->solution = pbqp_matrix_get_col_min_index(mat, other->solution, vec);
	} else {
		pbqp_node_t *other = edge->src;
		node->solution = pbqp_matrix_get_row_min_index(mat, other->solution, vec);
	}

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%u is set to %u<br>\n", node->index, node->solution);
	}
#endif
}

static void back_propagate_RII(pbqp_t *pbqp, pbqp_node_t *node)
{
	pbqp_edge_t *src_edge   = node->edges[0];
	bool         src_is_src = src_edge->src == node;
	pbqp_node_t *src_node;

	if (src_is_src) {
		src_node = src_edge->tgt;
	} else {
		src_node = src_edge->src;
	}

	pbqp_edge_t *tgt_edge   = node->edges[1];
	bool         tgt_is_src = tgt_edge->src == node;
	pbqp_node_t *tgt_node;

	if (tgt_is_src) {
		tgt_node = tgt_edge->tgt;
	} else {
		tgt_node = tgt_edge->src;
	}

	/* Swap nodes if necessary. */
	if (tgt_node->index < src_node->index) {
		pbqp_node_t *tmp_node = src_node;
		src_node = tgt_node;
		tgt_node = tmp_node;

		pbqp_edge_t *tmp_edge = src_edge;
		src_edge = tgt_edge;
		tgt_edge = tmp_edge;

		src_is_src = src_edge->src == node;
		tgt_is_src = tgt_edge->src == node;
	}


	pbqp_matrix_t *src_mat   = src_edge->costs;
	pbqp_matrix_t *tgt_mat   = tgt_edge->costs;
	vector_t      *node_vec  = node->costs;
	unsigned       row_index = src_node->solution;
	unsigned       col_index = tgt_node->solution;
	vector_t      *vec       = vector_copy(pbqp, node_vec);

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

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%u is set to %u<br>\n", node->index, node->solution);
	}
#endif

	obstack_free(&pbqp->obstack, vec);
}

void back_propagate(pbqp_t *pbqp)
{
#if KAPS_DUMP
	if (pbqp->dump_file) {
		pbqp_dump_section(pbqp->dump_file, 2, "Back Propagation");
	}
#endif

	unsigned node_len = node_bucket_get_length(reduced_bucket);

	for (unsigned node_index = node_len; node_index > 0; --node_index) {
		pbqp_node_t *node = reduced_bucket[node_index - 1];

		switch (pbqp_node_get_degree(node)) {
			case 1:
				back_propagate_RI(pbqp, node);
				break;
			case 2:
				back_propagate_RII(pbqp, node);
				break;
			default:
				panic("only nodes with degree one or two should be in this bucket");
		}
	}
}

void apply_edge(pbqp_t *pbqp)
{
	pbqp_edge_t *edge = edge_bucket_pop(&edge_bucket);

	simplify_edge(pbqp, edge);
}

void apply_RI(pbqp_t *pbqp)
{
	(void)pbqp;

	pbqp_node_t *node       = node_bucket_pop(&node_buckets[1]);
	pbqp_edge_t *edge       = node->edges[0];
	bool         is_src     = edge->src == node;
	pbqp_node_t *other_node;

	assert(pbqp_node_get_degree(node) == 1);

	if (is_src) {
		other_node = edge->tgt;
	} else {
		other_node = edge->src;
	}

#if KAPS_DUMP
	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RI-Reduction of Node n%u", node->index);
		pbqp_dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
		fputs("<br>\nBefore reduction:<br>\n", pbqp->dump_file);
		pbqp_dump_node(pbqp->dump_file, node);
		pbqp_dump_node(pbqp->dump_file, other_node);
		pbqp_dump_edge(pbqp->dump_file, edge);
	}
#endif

	pbqp_matrix_t *mat = edge->costs;

	if (is_src) {
		pbqp_matrix_add_to_all_cols(mat, node->costs);
		normalize_towards_target(edge);
	} else {
		pbqp_matrix_add_to_all_rows(mat, node->costs);
		normalize_towards_source(edge);
	}

	disconnect_edge(other_node, edge);

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("<br>\nAfter reduction:<br>\n", pbqp->dump_file);
		pbqp_dump_node(pbqp->dump_file, other_node);
	}
#endif

	reorder_node_after_edge_deletion(other_node);

#if KAPS_STATISTIC
	pbqp->num_r1++;
#endif

	/* Add node to back propagation list. */
	node_bucket_insert(&reduced_bucket, node);
}

void apply_RII(pbqp_t *pbqp)
{
	pbqp_node_t *node       = node_bucket_pop(&node_buckets[2]);
	pbqp_edge_t *src_edge   = node->edges[0];
	bool         src_is_src = src_edge->src == node;
	pbqp_node_t *src_node;

	assert(pbqp_node_get_degree(node) == 2);

	if (src_is_src) {
		src_node = src_edge->tgt;
	} else {
		src_node = src_edge->src;
	}

	pbqp_edge_t *tgt_edge   = node->edges[1];
	bool         tgt_is_src = tgt_edge->src == node;
	pbqp_node_t *tgt_node;

	if (tgt_is_src) {
		tgt_node = tgt_edge->tgt;
	} else {
		tgt_node = tgt_edge->src;
	}

	/* Swap nodes if necessary. */
	if (tgt_node->index < src_node->index) {
		pbqp_node_t *tmp_node = src_node;
		src_node = tgt_node;
		tgt_node = tmp_node;

		pbqp_edge_t *tmp_edge = src_edge;
		src_edge = tgt_edge;
		tgt_edge = tmp_edge;

		src_is_src = src_edge->src == node;
		tgt_is_src = tgt_edge->src == node;
	}

#if KAPS_DUMP
	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RII-Reduction of Node n%u", node->index);
		pbqp_dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
		fputs("<br>\nBefore reduction:<br>\n", pbqp->dump_file);
		pbqp_dump_node(pbqp->dump_file, src_node);
		pbqp_dump_edge(pbqp->dump_file, src_edge);
		pbqp_dump_node(pbqp->dump_file, node);
		pbqp_dump_edge(pbqp->dump_file, tgt_edge);
		pbqp_dump_node(pbqp->dump_file, tgt_node);
	}
#endif


	pbqp_matrix_t *src_mat  = src_edge->costs;
	pbqp_matrix_t *tgt_mat  = tgt_edge->costs;
	vector_t      *src_vec  = src_node->costs;
	vector_t      *tgt_vec  = tgt_node->costs;
	vector_t      *node_vec = node->costs;
	unsigned       col_len  = tgt_vec->len;
	unsigned       row_len  = src_vec->len;
	pbqp_matrix_t *mat      = pbqp_matrix_alloc(pbqp, row_len, col_len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		for (unsigned col_index = 0; col_index < col_len; ++col_index) {
			vector_t *vec = vector_copy(pbqp, node_vec);

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

			mat->entries[row_index * col_len + col_index] = vector_get_min(vec);

			obstack_free(&pbqp->obstack, vec);
		}
	}

	pbqp_edge_t *edge = get_edge(pbqp, src_node->index, tgt_node->index);

	/* Disconnect node. */
	disconnect_edge(src_node, src_edge);
	disconnect_edge(tgt_node, tgt_edge);

#if KAPS_STATISTIC
	pbqp->num_r2++;
#endif

	/* Add node to back propagation list. */
	node_bucket_insert(&reduced_bucket, node);

	if (edge == NULL) {
		edge = alloc_edge(pbqp, src_node->index, tgt_node->index, mat);
	} else {
		// matrix
		pbqp_matrix_add(edge->costs, mat);

		/* Free local matrix. */
		obstack_free(&pbqp->obstack, mat);

		reorder_node_after_edge_deletion(src_node);
		reorder_node_after_edge_deletion(tgt_node);
	}

#if KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("<br>\nAfter reduction:<br>\n", pbqp->dump_file);
		pbqp_dump_edge(pbqp->dump_file, edge);
	}
#endif

	/* Edge has changed so we simplify it. */
	simplify_edge(pbqp, edge);
}

static void select_column(pbqp_edge_t *edge, unsigned col_index)
{
	pbqp_node_t *src_node = edge->src;
	pbqp_node_t *tgt_node = edge->tgt;
	vector_t    *src_vec  = src_node->costs;
	vector_t    *tgt_vec  = tgt_node->costs;
	unsigned     src_len  = src_vec->len;
	unsigned     tgt_len  = tgt_vec->len;

	assert(src_len > 0);
	assert(tgt_len > 0);

	pbqp_matrix_t *mat          = edge->costs;
	unsigned       new_infinity = 0;

	for (unsigned src_index = 0; src_index < src_len; ++src_index) {
		num elem = mat->entries[src_index * tgt_len + col_index];

		if (elem != 0) {
			if (elem == INF_COSTS && src_vec->entries[src_index].data != INF_COSTS)
				new_infinity = 1;

			src_vec->entries[src_index].data = pbqp_add(src_vec->entries[src_index].data, elem);
		}
	}

	if (new_infinity) {
		unsigned edge_len = pbqp_node_get_degree(src_node);

		for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge_t *edge_candidate = src_node->edges[edge_index];

			if (edge_candidate != edge) {
				insert_into_edge_bucket(edge_candidate);
			}
		}
	}

	delete_edge(edge);
}

static void select_row(pbqp_edge_t *edge, unsigned row_index)
{
	pbqp_matrix_t *mat          = edge->costs;
	pbqp_node_t   *tgt_node     = edge->tgt;
	vector_t      *tgt_vec      = tgt_node->costs;
	unsigned       tgt_len      = tgt_vec->len;
	unsigned       new_infinity = 0;

	assert(tgt_len > 0);

	for (unsigned tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
		num elem = mat->entries[row_index * tgt_len + tgt_index];

		if (elem != 0) {
			if (elem == INF_COSTS && tgt_vec->entries[tgt_index].data != INF_COSTS)
				new_infinity = 1;

			tgt_vec->entries[tgt_index].data = pbqp_add(tgt_vec->entries[tgt_index].data, elem);
		}
	}

	if (new_infinity) {
		unsigned edge_len = pbqp_node_get_degree(tgt_node);

		for (unsigned edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge_t *edge_candidate = tgt_node->edges[edge_index];

			if (edge_candidate != edge) {
				insert_into_edge_bucket(edge_candidate);
			}
		}
	}

	delete_edge(edge);
}

void select_alternative(pbqp_node_t *node, unsigned selected_index)
{
	unsigned  max_degree = pbqp_node_get_degree(node);
	vector_t *node_vec   = node->costs;
	unsigned  node_len   = node_vec->len;
	assert(selected_index < node_len);

	node->solution = selected_index;

	/* Set all other costs to infinity. */
	for (unsigned node_index = 0; node_index < node_len; ++node_index) {
		if (node_index != selected_index) {
			node_vec->entries[node_index].data = INF_COSTS;
		}
	}

	/* Select corresponding row/column for incident edges. */
	for (unsigned edge_index = 0; edge_index < max_degree; ++edge_index) {
		pbqp_edge_t *edge = node->edges[edge_index];

		if (edge->src == node)
			select_row(edge, selected_index);
		else
			select_column(edge, selected_index);
	}
}

pbqp_node_t *get_node_with_max_degree(void)
{
	pbqp_node_t **bucket     = node_buckets[3];
	unsigned      bucket_len = node_bucket_get_length(bucket);
	unsigned      max_degree = 0;
	pbqp_node_t  *result     = NULL;

	for (unsigned bucket_index = 0; bucket_index < bucket_len; ++bucket_index) {
		pbqp_node_t *candidate = bucket[bucket_index];
		unsigned     degree    = pbqp_node_get_degree(candidate);

		if (degree > max_degree) {
			result = candidate;
			max_degree = degree;
		}
	}

	return result;
}

unsigned get_local_minimal_alternative(pbqp_t *pbqp, pbqp_node_t *node)
{
	vector_t *node_vec   = node->costs;
	unsigned  node_len   = node_vec->len;
	unsigned  max_degree = pbqp_node_get_degree(node);
	unsigned  min_index  = 0;
	num       min        = INF_COSTS;

	for (unsigned node_index = 0; node_index < node_len; ++node_index) {
		num value = node_vec->entries[node_index].data;

		for (unsigned edge_index = 0; edge_index < max_degree; ++edge_index) {
			pbqp_edge_t   *edge   = node->edges[edge_index];
			pbqp_matrix_t *mat    = edge->costs;
			bool           is_src = edge->src == node;
			vector_t      *vec;

			if (is_src) {
				vec = vector_copy(pbqp, edge->tgt->costs);
				vector_add_matrix_row(vec, mat, node_index);
			} else {
				vec = vector_copy(pbqp, edge->src->costs);
				vector_add_matrix_col(vec, mat, node_index);
			}

			value = pbqp_add(value, vector_get_min(vec));

			obstack_free(&pbqp->obstack, vec);
		}

		if (value < min) {
			min = value;
			min_index = node_index;
		}
	}

	return min_index;
}

int node_is_reduced(pbqp_node_t *node)
{
	if (!reduced_bucket)
		return 0;

	if (pbqp_node_get_degree(node) == 0)
		return 1;

	return node_bucket_contains(reduced_bucket, node);
}
