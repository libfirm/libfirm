/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   HTML dumper for PBQP.
 * @date    03.10.2008
 * @author  Sebastian Buchwald
 */
#include "html_dumper.h"

#include "adt/array.h"
#include "kaps.h"
#include "optimal.h"
#include "pbqp_edge_t.h"
#include "pbqp_node_t.h"
#include <assert.h>

/* Caution: Due to static buffer use only once per statement */
static const char *cost2a(num const cost)
{
	if (cost == INF_COSTS)
		return "inf";

	static char buf[10];

#if KAPS_USE_UNSIGNED
	sprintf(buf, "%u", cost);
#else
	sprintf(buf, "%10lld", cost);
#endif

	return buf;
}

/* print vector */
static void dump_vector(FILE *f, vector_t *vec)
{
	unsigned len = vec->len;
	assert(len > 0);

	fprintf(f, "<span class=\"vector\">( ");

	for (unsigned index = 0; index < len; ++index) {
#if KAPS_ENABLE_VECTOR_NAMES
		fprintf(f, "<span title=\"%s\">%s</span> ",
		        vec->entries[index].name, cost2a(vec->entries[index].data));
#else
		fprintf(f, "%s ", cost2a(vec->entries[index].data));
#endif
	}

	fprintf(f, " )</span>\n");
}

static void dump_matrix(FILE *f, pbqp_matrix_t *mat)
{
	assert(mat->cols > 0);
	assert(mat->rows > 0);

	num *p = mat->entries;

	fprintf(f, "\t\\begin{pmatrix}\n");

	for (unsigned row = 0; row < mat->rows; ++row) {
		fprintf(f, "\t %s", cost2a(*p++));

		for (unsigned col = 1; col < mat->cols; ++col) {
			fprintf(f, "& %s", cost2a(*p++));
		}

		fprintf(f, "\\\\\n");
	}

	fprintf(f, "\t\\end{pmatrix}\n");
}

void pbqp_dump_edge(FILE *file, pbqp_edge_t *edge)
{
	fputs("<tex>\n", file);
	fprintf(file, "\t\\overline\n{C}_{%u,%u}=\n",
	        edge->src->index, edge->tgt->index);
	dump_matrix(file, edge->costs);
	fputs("</tex><br>", file);
}

static void dump_edge_costs(pbqp_t *pbqp)
{
	fputs("<p>", pbqp->dump_file);

	for (unsigned src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node_t *src_node = get_node(pbqp, src_index);
		size_t edge_index;
		size_t len;

		if (!src_node)
			continue;

		len = ARR_LEN(src_node->edges);
		for (edge_index = 0; edge_index < len; ++edge_index) {
			pbqp_edge_t *edge      = src_node->edges[edge_index];
			unsigned     tgt_index = edge->tgt->index;
			if (src_index < tgt_index) {
				pbqp_dump_edge(pbqp->dump_file, edge);
			}
		}
	}

	fputs("</p>", pbqp->dump_file);
}

void pbqp_dump_node(FILE *file, pbqp_node_t *node)
{
	if (node) {
		fprintf(file, "\tc<sub>%u</sub> = ", node->index);
		dump_vector(file, node->costs);
		fputs("<br>\n", file);
	}
}

static void dump_node_costs(pbqp_t *pbqp)
{
	/* dump node costs */
	fputs("<p>", pbqp->dump_file);

	for (unsigned index = 0; index < pbqp->num_nodes; ++index) {
		pbqp_dump_node(pbqp->dump_file, get_node(pbqp, index));
	}

	fputs("</p>", pbqp->dump_file);
}

void pbqp_dump_section(FILE *f, int level, const char *txt)
{
	fprintf(f, "<h%d>%s</h%d>\n", level, txt, level);
}

void pbqp_dump_graph(pbqp_t *pbqp)
{
	fputs("<p>\n<graph>\n\tgraph input {\n", pbqp->dump_file);

	for (unsigned src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node_t *node = get_node(pbqp, src_index);

		if (node && !node_is_reduced(node)) {
			fprintf(pbqp->dump_file, "\t n%u;\n", src_index);
		}
	}

	for (unsigned src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node_t *node = get_node(pbqp, src_index);

		if (!node)
			continue;

		if (node_is_reduced(node))
			continue;

		unsigned len = ARR_LEN(node->edges);

		for (unsigned edge_index = 0; edge_index < len; ++edge_index) {
			pbqp_node_t *tgt_node  = node->edges[edge_index]->tgt;
			unsigned     tgt_index = tgt_node->index;

			if (node_is_reduced(tgt_node))
				continue;

			if (src_index < tgt_index) {
				fprintf(pbqp->dump_file, "\t n%u -- n%u;\n", src_index,
				        tgt_index);
			}
		}
	}

	fputs("\t}\n</graph>\n</p>\n", pbqp->dump_file);
}

void pbqp_dump_input(pbqp_t *pbqp)
{
	pbqp_dump_section(pbqp->dump_file, 1, "1. PBQP Problem");
	pbqp_dump_section(pbqp->dump_file, 2, "1.1 Topology");
	pbqp_dump_graph(pbqp);
	pbqp_dump_section(pbqp->dump_file, 2, "1.2 Cost Vectors");
	dump_node_costs(pbqp);
	pbqp_dump_section(pbqp->dump_file, 2, "1.3 Cost Matrices");
	dump_edge_costs(pbqp);
}

void pbqp_dump_simplifyedge(pbqp_t *pbqp, pbqp_edge_t *edge)
{
	pbqp_dump_node(pbqp->dump_file, edge->src);
	pbqp_dump_edge(pbqp->dump_file, edge);
	pbqp_dump_node(pbqp->dump_file, edge->tgt);
}
