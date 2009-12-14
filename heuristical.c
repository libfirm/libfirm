#include "adt/array.h"
#include "assert.h"
#include "error.h"

#include "bucket.h"
#include "heuristical.h"
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

static pbqp_edge **edge_bucket;
static pbqp_node **node_buckets[4];
static pbqp_node **reduced_bucket = NULL;
static int         buckets_filled = 0;

#if KAPS_STATISTIC
static int dump = 0;
#endif

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

	#if KAPS_TIMING
		ir_timer_t *t_fill_buckets = ir_timer_register("be_pbqp_fill_buckets", "PBQP Fill Nodes into buckets");
		ir_timer_reset_and_start(t_fill_buckets);
	#endif

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

	#if KAPS_TIMING
		ir_timer_stop(t_fill_buckets);
		printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_fill_buckets), (double)ir_timer_elapsed_usec(t_fill_buckets) / 1000.0);
	#endif
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

#if 0
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
#endif

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

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		char txt[100];
		sprintf(txt, "Simplification of Edge n%d-n%d", src_node->index, tgt_node->index);
		dump_section(pbqp->dump_file, 3, txt);
	}
#endif

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

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("Input:<br>\n", pbqp->dump_file);
		dump_simplifyedge(pbqp, edge);
	}
#endif

	normalize_towards_source(pbqp, edge);
	normalize_towards_target(pbqp, edge);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("<br>\nOutput:<br>\n", pbqp->dump_file);
		dump_simplifyedge(pbqp, edge);
	}
#endif

	if (pbqp_matrix_is_zero(mat, src_vec, tgt_vec)) {
#if	KAPS_DUMP
		if (pbqp->dump_file) {
			fputs("edge has been eliminated<br>\n", pbqp->dump_file);
		}
#endif

#if KAPS_STATISTIC
		if (dump == 0) {
			pbqp->num_edges++;
		}
#endif

		delete_edge(edge);
		reorder_node(src_node);
		reorder_node(tgt_node);
	}
}

static void initial_simplify_edges(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len;

	assert(pbqp);

	#if KAPS_TIMING
		ir_timer_t *t_int_simpl = ir_timer_register("be_pbqp_init_simp", "PBQP Initial simplify edges");
		ir_timer_reset_and_start(t_int_simpl);
	#endif

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		pbqp_dump_input(pbqp);
		dump_section(pbqp->dump_file, 1, "2. Simplification of Cost Matrices");
	}
#endif

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

	#if KAPS_TIMING
		ir_timer_stop(t_int_simpl);
		printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_int_simpl), (double)ir_timer_elapsed_usec(t_int_simpl) / 1000.0);
	#endif
}

static num determine_solution(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len;
	num      solution   = 0;

	#if KAPS_TIMING
		ir_timer_t *t_det_solution = ir_timer_register("be_det_solution", "PBQP Determine Solution");
		ir_timer_reset_and_start(t_det_solution);
	#endif

#if	KAPS_DUMP
	FILE     *file;
#endif

	assert(pbqp);

#if	KAPS_DUMP
	file = pbqp->dump_file;

	if (file) {
		dump_section(file, 1, "4. Determine Solution/Minimum");
		dump_section(file, 2, "4.1. Trivial Solution");
	}
#endif

	/* Solve trivial nodes and calculate solution. */
	node_len = node_bucket_get_length(node_buckets[0]);

#if KAPS_STATISTIC
	if (dump == 0) {
		pbqp->num_r0 = node_len;
	}
#endif

	for (node_index = 0; node_index < node_len; ++node_index) {
		pbqp_node *node = node_buckets[0][node_index];
		assert(node);

		node->solution = vector_get_min_index(node->costs);
		solution       = pbqp_add(solution,
				node->costs->entries[node->solution].data);

#if	KAPS_DUMP
		if (file) {
			fprintf(file, "node n%d is set to %d<br>\n", node->index, node->solution);
			dump_node(file, node);
		}
#endif
	}

#if	KAPS_DUMP
	if (file) {
		dump_section(file, 2, "Minimum");
#if KAPS_USE_UNSIGNED
		fprintf(file, "Minimum is equal to %u.", solution);
#else
		fprintf(file, "Minimum is equal to %lld.", solution);
#endif
	}
#endif

	#if KAPS_TIMING
		ir_timer_stop(t_det_solution);
		printf("%-20s: %8.3lf msec\n", ir_timer_get_description(t_det_solution), (double)ir_timer_elapsed_usec(t_det_solution) / 1000.0);
	#endif

	return solution;
}

static void back_propagate(pbqp *pbqp)
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

void solve_pbqp_heuristical(pbqp *pbqp)
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

	apply_heuristic_reductions(pbqp);

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

	assert(pbqp_node_get_degree(node) == 1);

	if (is_src) {
		other_node = edge->tgt;
	} else {
		other_node = edge->src;
	}

#if	KAPS_DUMP
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
#endif

	if (is_src) {
		pbqp_matrix_add_to_all_cols(mat, node->costs);
		normalize_towards_target(pbqp, edge);
	} else {
		pbqp_matrix_add_to_all_rows(mat, node->costs);
		normalize_towards_source(pbqp, edge);
	}
	disconnect_edge(other_node, edge);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("<br>\nAfter reduction:<br>\n", pbqp->dump_file);
		dump_node(pbqp->dump_file, other_node);
	}
#endif

	reorder_node(other_node);

#if KAPS_STATISTIC
	if (dump == 0) {
		pbqp->num_r1++;
	}
#endif

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
	assert(pbqp_node_get_degree(node) == 2);

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

#if	KAPS_DUMP
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
#endif

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

#if KAPS_STATISTIC
	if (dump == 0) {
		pbqp->num_r2++;
	}
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

		reorder_node(src_node);
		reorder_node(tgt_node);
	}

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		fputs("<br>\nAfter reduction:<br>\n", pbqp->dump_file);
		dump_edge(pbqp->dump_file, edge);
	}
#endif

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

	assert(node);
	node->solution = selected_index;
	node_vec = node->costs;
	node_len = node_vec->len;
	assert(selected_index < node_len);

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
	assert(pbqp_node_get_degree(node) > 2);

#if	KAPS_DUMP
	if (pbqp->dump_file) {
		char     txt[100];
		sprintf(txt, "RN-Reduction of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
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

void apply_RN_co(pbqp *pbqp, plist_t *rpeo)
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

void apply_Brute_Force(pbqp *pbqp)
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

int node_is_reduced(pbqp_node *node)
{
	if (!reduced_bucket) return 0;

	if (pbqp_node_get_degree(node) == 0) return 1;

	return node_bucket_contains(reduced_bucket, node);
}
