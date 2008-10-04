#include "adt/array.h"
#include "assert.h"
#include "error.h"

#include "heuristical.h"
#include "html_dumper.h"
#include "kaps.h"
#include "matrix.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node_t.h"
#include "vector.h"

static pbqp_edge **edge_bucket;
static pbqp_node **node_buckets[4];

static void init_buckets(void)
{
	int i;

	edge_bucket = NEW_ARR_F(pbqp_edge *, 0);

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

		ARR_APP1(pbqp_node *, node_buckets[arity], node);
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
	int             src_index;
	int             tgt_index;

	assert(pbqp);
	assert(edge);

	if(pbqp->dump_file) {
		char txt[100];
		sprintf(txt, "Simplification of Edge n%d-n%d", edge->src, edge->tgt);
		dump_section(pbqp->dump_file, 3, txt);
	}

	src_node = get_node(pbqp, edge->src);
	tgt_node = get_node(pbqp, edge->tgt);
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

	if (pbqp->dump_file) {
		fputs("Input:<br>\n", pbqp->dump_file);
		dump_simplifyedge(pbqp, edge);
	}

	/* Normalize towards source node. */
	for (src_index = 0; src_index < src_len; ++src_index) {
		num min = pbqp_matrix_get_row_min(mat, src_index, tgt_vec);

		if (min != 0) {
			pbqp_matrix_sub_row_value(mat, src_index, tgt_vec, min);
			vector_add_value(src_vec, min);

			// TODO add to edge_list if inf
		}
	}

	/* Normalize towards target node. */
	for (tgt_index = 0; tgt_index < tgt_len; ++tgt_index) {
		num min = pbqp_matrix_get_col_min(mat, tgt_index, src_vec);

		if (min != 0) {
			pbqp_matrix_sub_col_value(mat, tgt_index, src_vec, min);
			vector_add_value(tgt_vec, min);

			// TODO add to edge_list if inf
		}
	}

	if (pbqp->dump_file) {
		fputs("<br>\nOutput:<br>\n", pbqp->dump_file);
		dump_simplifyedge(pbqp, edge);
	}

	if (pbqp_matrix_is_zero(mat, src_vec, tgt_vec)) {
		if (pbqp->dump_file) {
			fputs("edge has been eliminated", pbqp->dump_file);

			delete_edge(pbqp, edge);
		}
	}
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
			if (node_index != edge->src) continue;

			simplify_edge(pbqp, edge);
		}
	}

	/* Put node into bucket representing their arity. */
	fill_node_buckets(pbqp);

	for (;;) {
		if (ARR_LEN(edge_bucket) > 0) {
			panic("Please implement edge simplification");
		} else if (ARR_LEN(node_buckets[1]) > 0) {
			panic("Please implement RI simplification");
		} else if (ARR_LEN(node_buckets[2]) > 0) {
			panic("Please implement RII simplification");
		} else if (ARR_LEN(node_buckets[3]) > 0) {
			panic("Please implement RN simplification");
		} else {
			panic("Please implement back propagation");
			// break;
		}
	}
}
