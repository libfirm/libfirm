#include "adt/array.h"
#include "assert.h"
#include "error.h"

#include "bucket.h"
#include "heuristical.h"
#include "html_dumper.h"
#include "kaps.h"
#include "matrix.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

static pbqp_edge **edge_bucket;
static pbqp_node **node_buckets[4];
static pbqp_node **reduced_bucket = NULL;
static int         buckets_filled = 0;

/* Forward declarations. */
static void apply_Brute_Force(pbqp *pbqp);

static void insert_into_edge_bucket(pbqp_edge *edge)
{
	if (edge_bucket_contains(edge_bucket, edge)) {
		/* Edge is already inserted. */
		return;
	}

	edge_bucket_insert(&edge_bucket, edge);
}

static void init_buckets(void)
{
	int i;

	edge_bucket_init(&edge_bucket);
	node_bucket_init(&reduced_bucket);

	for (i = 0; i < 4; ++i) {
		node_bucket_init(&node_buckets[i]);
	}
}

static void free_buckets(void)
{
	int i;

	for (i = 0; i < 4; ++i) {
		node_bucket_free(&node_buckets[i]);
	}

	edge_bucket_free(&edge_bucket);
	node_bucket_free(&reduced_bucket);

	buckets_filled = 0;
}

static void fill_node_buckets(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len;

	assert(pbqp);
	node_len = pbqp->num_nodes;

	for (node_index = 0; node_index < node_len; ++node_index) {
		unsigned   degree;
		pbqp_node *node = get_node(pbqp, node_index);

		if (!node) continue;

		degree = pbqp_node_get_degree(node);

		/* We have only one bucket for nodes with arity >= 3. */
		if (degree > 3) {
			degree = 3;
		}

		node_bucket_insert(&node_buckets[degree], node);
	}

	buckets_filled = 1;
}

static void normalize_towards_source(pbqp *pbqp, pbqp_edge *edge)
{
	pbqp_matrix    *mat;
	pbqp_node      *src_node;
	pbqp_node      *tgt_node;
	vector         *src_vec;
	vector         *tgt_vec;
	int             src_len;
	int             tgt_len;
	int             src_index;

	assert(pbqp);
	assert(edge);

	src_node = edge->src;
	tgt_node = edge->tgt;
	assert(src_node);
	assert(tgt_node);

	src_vec = src_node->costs;
	tgt_vec = tgt_node->costs;
	assert(src_vec);
	assert(tgt_vec);

	src_len = src_vec->len;
	tgt_len = tgt_vec->len;
	assert(src_len > 0);
	assert(tgt_len > 0);

	mat = edge->costs;
	assert(mat);

	/* Normalize towards source node. */
	for (src_index = 0; src_index < src_len; ++src_index) {
		num min = pbqp_matrix_get_row_min(mat, src_index, tgt_vec);

		if (min != 0) {
			if (src_vec->entries[src_index].data == INF_COSTS) {
				pbqp_matrix_set_row_value(mat, src_index, 0);
			} else {
				pbqp_matrix_sub_row_value(mat, src_index, tgt_vec, min);
			}
			src_vec->entries[src_index].data = pbqp_add(
					src_vec->entries[src_index].data, min);

			if (min == INF_COSTS) {
				unsigned edge_index;
				unsigned edge_len = pbqp_node_get_degree(src_node);

				for (edge_index = 0; edge_index < edge_len; ++edge_index) {
					pbqp_edge *edge_candidate = src_node->edges[edge_index];
					if (edge_candidate != edge) {
						insert_into_edge_bucket(edge_candidate);
					}
				}
			}
		}
	}
}

static void normalize_towards_target(pbqp *pbqp, pbqp_edge *edge)
{
	pbqp_matrix    *mat;
	pbqp_node      *src_node;
	pbqp_node      *tgt_node;
	vector         *src_vec;
	vector         *tgt_vec;
	int             src_len;
	int             tgt_len;
	int             tgt_index;

	assert(pbqp);
	assert(edge);

	src_node = edge->src;
	tgt_node = edge->tgt;
	assert(src_node);
	assert(tgt_node);

	src_vec = src_node->costs;
	tgt_vec = tgt_node->costs;
	assert(src_vec);
	assert(tgt_vec);

	src_len = src_vec->len;
	tgt_len = tgt_vec->len;
	assert(src_len > 0);
	assert(tgt_len > 0);

	mat = edge->costs;
	assert(mat);

	for (tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
		num min = pbqp_matrix_get_col_min(mat, tgt_index, src_vec);

		if (min != 0) {
			if (tgt_vec->entries[tgt_index].data == INF_COSTS) {
				pbqp_matrix_set_col_value(mat, tgt_index, 0);
			} else {
				pbqp_matrix_sub_col_value(mat, tgt_index, src_vec, min);
			}
			tgt_vec->entries[tgt_index].data = pbqp_add(
					tgt_vec->entries[tgt_index].data, min);

			if (min == INF_COSTS) {
				unsigned edge_index;
				unsigned edge_len = pbqp_node_get_degree(tgt_node);

				for (edge_index = 0; edge_index < edge_len; ++edge_index) {
					pbqp_edge *edge_candidate = tgt_node->edges[edge_index];
					if (edge_candidate != edge) {
						insert_into_edge_bucket(edge_candidate);
					}
				}
			}
		}
	}
}

static void reorder_node(pbqp_node *node)
{
	unsigned    degree     = pbqp_node_get_degree(node);
	/* Assume node lost one incident edge. */
	unsigned    old_degree = degree + 1;

	if (!buckets_filled) return;

	/* Same bucket as before */
	if (degree > 2) return;

	if (!node_bucket_contains(node_buckets[old_degree], node)) {
		/* Old arity is new arity, so we have nothing to do. */
		assert(node_bucket_contains(node_buckets[degree], node));
		return;
	}

	/* Delete node from old bucket... */
	node_bucket_remove(&node_buckets[old_degree], node);

	/* ..and add to new one. */
	node_bucket_insert(&node_buckets[degree], node);
}

static void check_melting_possibility(pbqp *pbqp, pbqp_edge *edge)
{
	pbqp_matrix    *mat;
	pbqp_node      *src_node;
	pbqp_node      *tgt_node;
	vector         *src_vec;
	vector         *tgt_vec;
	int             src_len;
	int             tgt_len;
	int             src_index;
	int             tgt_index;

	assert(pbqp);
	assert(edge);

	src_node = edge->src;
	tgt_node = edge->tgt;
	assert(src_node);
	assert(tgt_node);

	src_vec = src_node->costs;
	tgt_vec = tgt_node->costs;
	assert(src_vec);
	assert(tgt_vec);

	src_len = src_vec->len;
	tgt_len = tgt_vec->len;
	assert(src_len > 0);
	assert(tgt_len > 0);

	mat = edge->costs;
	assert(mat);

	if (src_len == 1 && tgt_len == 1) {
		//panic("Something is wrong");
	}

	int allRowsOk = 1;
	for (src_index = 0; src_index < src_len; ++src_index) {
		int onlyOneZero = 0;
		if (src_vec->entries[src_index].data == INF_COSTS) {
			continue;
		}
		for (tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
			if (tgt_vec->entries[tgt_index].data == INF_COSTS) {
				continue;
			}
			if (mat->entries[src_index * tgt_len + tgt_index] == 0) {
				if (onlyOneZero) {
					onlyOneZero = 0;
					break;
				} else {
					onlyOneZero = 1;
					continue;
				}
			}
			if (mat->entries[src_index * tgt_len + tgt_index] == INF_COSTS) {
				continue;
			}
			onlyOneZero = 0;
			break;
		}
		allRowsOk &= onlyOneZero;
	}

	int allColsOk = 1;
	for (tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
		int onlyOneZero = 0;
		if (tgt_vec->entries[tgt_index].data == INF_COSTS) {
			continue;
		}
		for (src_index = 0; src_index < src_len; ++src_index) {
			if (src_vec->entries[src_index].data == INF_COSTS) {
				continue;
			}
			if (mat->entries[src_index * tgt_len + tgt_index] == 0) {
				if (onlyOneZero) {
					onlyOneZero = 0;
					break;
				} else {
					onlyOneZero = 1;
					continue;
				}
			}
			if (mat->entries[src_index * tgt_len + tgt_index] == INF_COSTS) {
				continue;
			}
			onlyOneZero = 0;
			break;
		}
		allColsOk &= onlyOneZero;
	}

	if (allRowsOk && allColsOk) {
		panic("Hurray");
	}
}

static void simplify_edge(pbqp *pbqp, pbqp_edge *edge)
{
	pbqp_matrix    *mat;
	pbqp_node      *src_node;
	pbqp_node      *tgt_node;
	vector         *src_vec;
	vector         *tgt_vec;
	int             src_len;
	int             tgt_len;

	assert(pbqp);
	assert(edge);

	src_node = edge->src;
	tgt_node = edge->tgt;
	assert(src_node);
	assert(tgt_node);

	/* If edge are already deleted, we have nothing to do. */
	if (!is_connected(src_node, edge) || !is_connected(tgt_node, edge))
		return;

	if (pbqp->dump_file) {
		char txt[100];
		sprintf(txt, "Simplification of Edge n%d-n%d", src_node->index, tgt_node->index);
		dump_section(pbqp->dump_file, 3, txt);
	}

	src_vec = src_node->costs;
	tgt_vec = tgt_node->costs;
	assert(src_vec);
	assert(tgt_vec);

	src_len = src_vec->len;
	tgt_len = tgt_vec->len;
	assert(src_len > 0);
	assert(tgt_len > 0);

	mat = edge->costs;
	assert(mat);

	if (pbqp->dump_file) {
		fputs("Input:<br>\n", pbqp->dump_file);
		dump_simplifyedge(pbqp, edge);
	}

	normalize_towards_source(pbqp, edge);
	normalize_towards_target(pbqp, edge);

	if (pbqp->dump_file) {
		fputs("<br>\nOutput:<br>\n", pbqp->dump_file);
		dump_simplifyedge(pbqp, edge);
	}

	if (pbqp_matrix_is_zero(mat, src_vec, tgt_vec)) {
		if (pbqp->dump_file) {
			fputs("edge has been eliminated<br>\n", pbqp->dump_file);
		}

		delete_edge(edge);
		reorder_node(src_node);
		reorder_node(tgt_node);
	} else {
		//check_melting_possibility(pbqp, edge);
	}
}

static void initial_simplify_edges(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len;

	assert(pbqp);

	if (pbqp->dump_file) {
		pbqp_dump_input(pbqp);
		dump_section(pbqp->dump_file, 1, "2. Simplification of Cost Matrices");
	}

	node_len = pbqp->num_nodes;

	init_buckets();

	/* First simplify all edges. */
	for (node_index = 0; node_index < node_len; ++node_index) {
		unsigned    edge_index;
		pbqp_node  *node = get_node(pbqp, node_index);
		pbqp_edge **edges;
		unsigned    edge_len;

		if (!node) continue;

		edges = node->edges;
		edge_len = pbqp_node_get_degree(node);

		for (edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge *edge = edges[edge_index];

			/* Simplify only once per edge. */
			if (node != edge->src) continue;

			simplify_edge(pbqp, edge);
		}
	}
}

num determine_solution(FILE *file)
{
	unsigned node_index;
	unsigned node_len;
	num      solution;

	if (file) {
		dump_section(file, 1, "4. Determine Solution/Minimum");
		dump_section(file, 2, "4.1. Trivial Solution");
	}

	/* Solve trivial nodes and calculate solution. */
	node_len = node_bucket_get_length(node_buckets[0]);
	for (node_index = 0; node_index < node_len; ++node_index) {
		pbqp_node *node = node_buckets[0][node_index];
		assert(node);

		node->solution = vector_get_min_index(node->costs);
		solution       = pbqp_add(solution,
				node->costs->entries[node->solution].data);
		if (file) {
			fprintf(file, "node n%d is set to %d<br>\n", node->index, node->solution);
			dump_node(file, node);
		}
	}

	if (file) {
		dump_section(file, 2, "Minimum");
		fprintf(file, "Minimum is equal to %lld.", solution);
	}

	return solution;
}

static void back_propagate(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len   = node_bucket_get_length(reduced_bucket);

	assert(pbqp);
	if (pbqp->dump_file) {
		dump_section(pbqp->dump_file, 2, "Back Propagation");
	}

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

static void apply_heuristic_reductions(pbqp *pbqp)
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

void solve_pbqp_heuristical(pbqp *pbqp)
{
	/* Reduce nodes degree ... */
	initial_simplify_edges(pbqp);

	/* ... and put node into bucket representing their degree. */
	fill_node_buckets(pbqp);

	apply_heuristic_reductions(pbqp);

	pbqp->solution = determine_solution(pbqp->dump_file);

	/* Solve reduced nodes. */
	back_propagate(pbqp);

	free_buckets();
}

void apply_edge(pbqp *pbqp)
{
	pbqp_edge *edge = edge_bucket_pop(&edge_bucket);

	simplify_edge(pbqp, edge);
}

void apply_RI(pbqp *pbqp)
{
	pbqp_node   *node       = node_bucket_pop(&node_buckets[1]);
	pbqp_edge   *edge       = node->edges[0];
	pbqp_matrix *mat        = edge->costs;
	int          is_src     = edge->src == node;
	pbqp_node   *other_node;

	if (is_src) {
		other_node = edge->tgt;
	} else {
		other_node = edge->src;
	}

	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RI-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
		fputs("<br>\nBefore reduction:<br>\n", pbqp->dump_file);
		dump_node(pbqp->dump_file, node);
		dump_node(pbqp->dump_file, other_node);
		dump_edge(pbqp->dump_file, edge);
	}

	if (is_src) {
		pbqp_matrix_add_to_all_cols(mat, node->costs);
		normalize_towards_target(pbqp, edge);
	} else {
		pbqp_matrix_add_to_all_rows(mat, node->costs);
		normalize_towards_source(pbqp, edge);
	}
	disconnect_edge(other_node, edge);

	if (pbqp->dump_file) {
		fputs("<br>\nAfter reduction:<br>\n", pbqp->dump_file);
		dump_node(pbqp->dump_file, other_node);
	}

	reorder_node(other_node);

	/* Add node to back propagation list. */
	node_bucket_insert(&reduced_bucket, node);
}

void apply_RII(pbqp *pbqp)
{
	pbqp_node   *node       = node_bucket_pop(&node_buckets[2]);
	pbqp_edge   *src_edge   = node->edges[0];
	pbqp_edge   *tgt_edge   = node->edges[1];
	int          src_is_src = src_edge->src == node;
	int          tgt_is_src = tgt_edge->src == node;
	pbqp_matrix *src_mat;
	pbqp_matrix *tgt_mat;
	pbqp_node   *src_node;
	pbqp_node   *tgt_node;
	pbqp_matrix *mat;
	vector      *vec;
	vector      *node_vec;
	vector      *src_vec;
	vector      *tgt_vec;
	unsigned     col_index;
	unsigned     col_len;
	unsigned     row_index;
	unsigned     row_len;
	unsigned     node_len;

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

	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RII-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
		fputs("<br>\nBefore reduction:<br>\n", pbqp->dump_file);
		dump_node(pbqp->dump_file, src_node);
		dump_edge(pbqp->dump_file, src_edge);
		dump_node(pbqp->dump_file, node);
		dump_edge(pbqp->dump_file, tgt_edge);
		dump_node(pbqp->dump_file, tgt_node);
	}

	src_mat = src_edge->costs;
	tgt_mat = tgt_edge->costs;

	src_vec  = src_node->costs;
	tgt_vec  = tgt_node->costs;
	node_vec = node->costs;

	row_len  = src_vec->len;
	col_len  = tgt_vec->len;
	node_len = node_vec->len;

	mat = pbqp_matrix_alloc(pbqp, row_len, col_len);

	for (row_index = 0; row_index < row_len; ++row_index) {
		for (col_index = 0; col_index < col_len; ++col_index) {
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

			mat->entries[row_index * col_len + col_index] = vector_get_min(vec);

			obstack_free(&pbqp->obstack, vec);
		}
	}

	pbqp_edge *edge = get_edge(pbqp, src_node->index, tgt_node->index);

	/* Disconnect node. */
	disconnect_edge(src_node, src_edge);
	disconnect_edge(tgt_node, tgt_edge);

	/* Add node to back propagation list. */
	node_bucket_insert(&reduced_bucket, node);

	if (edge == NULL) {
		edge = alloc_edge(pbqp, src_node->index, tgt_node->index, mat);
	} else {
		pbqp_matrix_add(edge->costs, mat);

		/* Free local matrix. */
		obstack_free(&pbqp->obstack, mat);

		reorder_node(src_node);
		reorder_node(tgt_node);
	}

	if (pbqp->dump_file) {
		fputs("<br>\nAfter reduction:<br>\n", pbqp->dump_file);
		dump_edge(pbqp->dump_file, edge);
	}

	/* Edge has changed so we simplify it. */
	simplify_edge(pbqp, edge);
}

static void select_alternative(pbqp_node *node, unsigned selected_index)
{
	unsigned  edge_index;
	unsigned  node_index;
	unsigned  node_len;
	vector   *node_vec;
	unsigned  max_degree = pbqp_node_get_degree(node);

	assert(selected_index < max_degree);
	assert(node);
	node->solution = selected_index;
	node_vec = node->costs;
	node_len = node_vec->len;

	/* Set all other costs to infinity. */
	for (node_index = 0; node_index < node_len; ++node_index) {
		if (node_index != selected_index) {
			node_vec->entries[node_index].data = INF_COSTS;
		}
	}

	/* Add all incident edges to edge bucket, since they are now independent. */
	for (edge_index = 0; edge_index < max_degree; ++edge_index) {
		insert_into_edge_bucket(node->edges[edge_index]);
	}
}

static pbqp_node *get_node_with_max_degree(void)
{
	pbqp_node  **bucket       = node_buckets[3];
	unsigned     bucket_len   = node_bucket_get_length(bucket);
	unsigned     bucket_index;
	unsigned     max_degree   = 0;
	pbqp_node   *result       = NULL;

	for (bucket_index = 0; bucket_index < bucket_len; ++bucket_index) {
		pbqp_node *candidate = bucket[bucket_index];
		unsigned   degree    = pbqp_node_get_degree(candidate);

		if (degree > max_degree) {
			result = candidate;
			max_degree = degree;
		}
	}

	return result;
}

static unsigned get_local_minimal_alternative(pbqp *pbqp, pbqp_node *node)
{
	pbqp_edge   *edge;
	vector      *node_vec;
	vector      *vec;
	pbqp_matrix *mat;
	unsigned     edge_index;
	unsigned     max_degree   = 0;
	unsigned     node_index;
	unsigned     node_len;
	unsigned     min_index    = 0;
	num          min          = INF_COSTS;
	int          is_src;

	assert(pbqp);
	assert(node);
	node_vec = node->costs;
	node_len = node_vec->len;

	for (node_index = 0; node_index < node_len; ++node_index) {
		num value = node_vec->entries[node_index].data;

		for (edge_index = 0; edge_index < max_degree; ++edge_index) {
			edge   = node->edges[edge_index];
			mat    = edge->costs;
			is_src = edge->src == node;

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

void apply_RN(pbqp *pbqp)
{
	pbqp_node   *node         = NULL;
	unsigned     min_index    = 0;

	assert(pbqp);

	/* We want to reduce a node with maximum degree. */
	node = get_node_with_max_degree();
	assert(node);

	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RN-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
	}

	min_index = get_local_minimal_alternative(pbqp, node);

	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br><br>\n",
					node->index, min_index);
	}

	/* Now that we found the local minimum set all other costs to infinity. */
	select_alternative(node, min_index);
}

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
	pbqp_edge   *edge;
	vector      *node_vec;
	vector      *vec;
	pbqp_matrix *mat;
	unsigned     edge_index;
	unsigned     max_degree   = 0;
	unsigned     node_index;
	unsigned     node_len;
	unsigned     min_index    = 0;
	num          min          = INF_COSTS;
	int          is_src;

	assert(pbqp);
	assert(node);
	node_vec = node->costs;
	node_len = node_vec->len;

	for (node_index = 0; node_index < node_len; ++node_index) {
		num value;

		/* Some node buckets and the edge bucket should be empty. */
		assert(node_bucket_get_length(node_buckets[1]) == 0);
		assert(node_bucket_get_length(node_buckets[2]) == 0);
		assert(edge_bucket_get_length(edge_bucket)     == 0);

		/* Save current PBQP state. */
		pbqp_node_bucket *bucket_deg0 = node_bucket_deep_copy(node_buckets[0]);
		pbqp_node_bucket *bucket_deg3 = node_bucket_deep_copy(node_buckets[3]);
		pbqp_node_bucket *bucket_red  = node_bucket_deep_copy(reduced_bucket);

		/* Select alternative and solve PBQP recursively. */
		select_alternative(node, node_index);
		apply_brute_force_reductions(pbqp);

		value = determine_solution(pbqp->dump_file);

		if (value < min) {
			min = value;
			min_index = node_index;
		}

		/* TODO Restore old PBQP state. */

	}

	return min_index;
}

void apply_Brute_Force(pbqp *pbqp)
{
	pbqp_node   *node         = NULL;
	unsigned     min_index    = 0;

	assert(pbqp);

	/* We want to reduce a node with maximum degree. */
	node = get_node_with_max_degree();
	assert(node);

	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "BF-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
	}

	min_index = get_minimal_alternative(pbqp, node);

	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br><br>\n",
					node->index, min_index);
	}

	/* Now that we found the minimum set all other costs to infinity. */
	select_alternative(node, min_index);
}

void solve_pbqp_brute_force(pbqp *pbqp)
{
	/* Reduce nodes degree ... */
	initial_simplify_edges(pbqp);

	/* ... and put node into bucket representing their degree. */
	fill_node_buckets(pbqp);

	apply_brute_force_reductions(pbqp);

	pbqp->solution = determine_solution(pbqp->dump_file);

	/* Solve reduced nodes. */
	back_propagate(pbqp);

	free_buckets();
}

void back_propagate_RI(pbqp *pbqp, pbqp_node *node)
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
		vector_add_matrix_col(vec, mat, other->solution);
	} else {
		other = edge->src;
		assert(other);
		vector_add_matrix_row(vec, mat, other->solution);
	}

	node->solution = vector_get_min_index(vec);
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br>\n", node->index, node->solution);
	}
}

void back_propagate_RII(pbqp *pbqp, pbqp_node *node)
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
	if (pbqp->dump_file) {
		fprintf(pbqp->dump_file, "node n%d is set to %d<br>\n", node->index, node->solution);
	}

	obstack_free(&pbqp->obstack, vec);
}

int node_is_reduced(pbqp_node *node)
{
	if (!reduced_bucket) return 0;

	if (pbqp_node_get_degree(node) == 0) return 1;

	return node_bucket_contains(reduced_bucket, node);
}
