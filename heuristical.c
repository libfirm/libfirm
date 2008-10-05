#include "adt/array.h"
#include "assert.h"
#include "error.h"

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
static pbqp_node **reduced_bucket;

static void init_buckets(void)
{
	int i;

	edge_bucket = NEW_ARR_F(pbqp_edge *, 0);
	reduced_bucket = NEW_ARR_F(pbqp_node *, 0);

	for (i = 0; i < 4; ++i) {
		node_buckets[i] = NEW_ARR_F(pbqp_node *, 0);
	}
}

static void fill_node_buckets(pbqp *pbqp)
{
	unsigned node_index;
	unsigned node_len;

	assert(pbqp);
	node_len = pbqp->num_nodes;

	for (node_index = 0; node_index < node_len; ++node_index) {
		unsigned   arity;
		pbqp_node *node = get_node(pbqp, node_index);

		if (!node) continue;

		arity = ARR_LEN(node->edges);

		/* We have only one bucket for nodes with arity >= 3. */
		if (arity > 3) {
			arity = 3;
		}

		node->bucket_index = ARR_LEN(node_buckets[arity]);

		ARR_APP1(pbqp_node *, node_buckets[arity], node);
	}
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
			pbqp_matrix_sub_row_value(mat, src_index, tgt_vec, min);
			src_vec->entries[src_index].data += min;

			// TODO add to edge_list if inf
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
			pbqp_matrix_sub_col_value(mat, tgt_index, src_vec, min);
			tgt_vec->entries[tgt_index].data += min;

			// TODO add to edge_list if inf
		}
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

	if(pbqp->dump_file) {
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
			fputs("edge has been eliminated", pbqp->dump_file);

			delete_edge(edge);
		}
	}
}

static void reorder_node(pbqp_node *node)
{
	unsigned arity;
	unsigned old_arity;
	unsigned old_bucket_len;

	assert(node);

	arity = ARR_LEN(node->edges);

	/* Equal bucket as before */
	if (arity > 2) return;

	/* Assume node lost one incident edge. */
	old_arity = arity + 1;

	if (ARR_LEN(node_buckets[old_arity]) <= (int)node->bucket_index
			|| node_buckets[old_arity][node->bucket_index] != node) {
		/* Old arity is new arity, so we have nothing to do. */
		return;
	}

	old_bucket_len = ARR_LEN(node_buckets[old_arity]);
	assert (node_buckets[old_arity][node->bucket_index] == node);

	/* Delete node from old bucket... */
	node_buckets[old_arity][node->bucket_index]
			= node_buckets[old_arity][old_bucket_len - 1];
	ARR_SHRINKLEN(node_buckets[old_arity], (int)old_bucket_len - 1);

	/* ..and add to new one. */
	node->bucket_index = ARR_LEN(node_buckets[arity]);
	ARR_APP1(pbqp_node *, node_buckets[arity], node);
}

void solve_pbqp_heuristical(pbqp *pbqp)
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
		edge_len = ARR_LEN(edges);

		for (edge_index = 0; edge_index < edge_len; ++edge_index) {
			pbqp_edge *edge = edges[edge_index];

			/* Simplify only once per edge. */
			if (node_index != edge->src->index) continue;

			simplify_edge(pbqp, edge);
		}
	}

	/* Put node into bucket representing their arity. */
	fill_node_buckets(pbqp);

	for (;;) {
		if (ARR_LEN(edge_bucket) > 0) {
			panic("Please implement edge simplification");
		} else if (ARR_LEN(node_buckets[1]) > 0) {
			applyRI(pbqp);
		} else if (ARR_LEN(node_buckets[2]) > 0) {
			panic("Please implement RII simplification");
		} else if (ARR_LEN(node_buckets[3]) > 0) {
			panic("Please implement RN simplification");
		} else {
			break;
		}
	}

	if (pbqp->dump_file) {
		dump_section(pbqp->dump_file, 1, "4. Determine Solution/Minimum");
		dump_section(pbqp->dump_file, 2, "4.1. Trivial Solution");
	}

	/* Solve trivial nodes and calculate solution. */
	node_len = ARR_LEN(node_buckets[0]);
	for (node_index = 0; node_index < node_len; ++node_index) {
		pbqp_node *node = node_buckets[0][node_index];
		assert(node);

		node->solution = vector_get_min_index(node->costs);
		pbqp->solution += node->costs->entries[node->solution].data;
		if (pbqp->dump_file) {
			fprintf(pbqp->dump_file, "node n%d is set to %d<br>\n", node->index, node->solution);
			dump_node(pbqp, node);
		}
	}

	if (pbqp->dump_file) {
		dump_section(pbqp->dump_file, 2, "Minimum");
		fprintf(pbqp->dump_file, "Minimum is equal to %d.", pbqp->solution);
		dump_section(pbqp->dump_file, 2, "Back Propagation");
	}

	/* Solve reduced nodes. */
	node_len = ARR_LEN(reduced_bucket);
	for (node_index = node_len; node_index > 0; --node_index) {
		pbqp_node *node = reduced_bucket[node_index - 1];
		assert(node);

		switch (ARR_LEN(node->edges)) {
			case 1:
				back_propagate_RI(pbqp, node);
				break;
			case 2:
				panic("Please implement back propagation for RII");
				break;
			default:
				panic("Only nodes with degree one or two should be in this bucket");
				break;
		}
	}
}

void applyRI(pbqp *pbqp)
{
	pbqp_node  **bucket     = node_buckets[1];
	unsigned     bucket_len = ARR_LEN(bucket);
	pbqp_node   *node       = bucket[bucket_len - 1];
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
		sprintf(txt, "RI-Reduktion of Node n%d", node->index);
		dump_section(pbqp->dump_file, 2, txt);
		pbqp_dump_graph(pbqp);
		fputs("<br>\nBefore reduction:<br>\n", pbqp->dump_file);
		dump_node(pbqp, node);
		dump_node(pbqp, other_node);
		dump_edge(pbqp, edge);
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
		dump_node(pbqp, other_node);
	}

	/* Remove node from bucket... */
	ARR_SHRINKLEN(bucket, (int)bucket_len - 1);
	reorder_node(other_node);

	/* ...and add it to back propagation list. */
	ARR_APP1(pbqp_node *, reduced_bucket, node);
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
