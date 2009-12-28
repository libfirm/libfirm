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
 * @brief   HTML dumper for PBQP.
 * @date    03.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#include "config.h"

#include "adt/array.h"
#include "assert.h"

#include "pbqp_edge_t.h"
#include "pbqp_node_t.h"
#include "optimal.h"
#include "html_dumper.h"
#include "kaps.h"

/* Caution: Due to static buffer use only once per statement */
static const char *cost2a(num const cost)
{
	static char buf[10];

	if (cost == INF_COSTS) return "inf";
	sprintf(buf, "%10lld", cost);
	return buf;
}

/* print vector */
static void dump_vector(FILE *f, vector *vec)
{
	unsigned index;
	assert(vec);

	fprintf(f, "<span class=\"vector\">( ");
	unsigned len = vec->len;
	assert(len > 0);
	for (index = 0; index < len; ++index) {
#if KAPS_ENABLE_VECTOR_NAMES
		fprintf(f, "<span title=\"%s\">%s</span> ",
				vec->entries[index].name, cost2a(vec->entries[index].data));
#else
		fprintf(f, "%s ", cost2a(vec->entries[index].data));
#endif
	}
	fprintf(f, " )</span>\n");
}

static void dump_matrix(FILE *f, pbqp_matrix *mat)
{
	unsigned row, col;
	assert(mat);
	num *p = mat->entries;

	assert(mat->cols> 0);
	assert(mat->rows> 0);
	fprintf(f, "\t\\begin{pmatrix}\n");
	for (row = 0; row < mat->rows; ++row) {
		fprintf(f, "\t %s", cost2a(*p++));

		for (col = 1; col < mat->cols; ++col) {
			fprintf(f, "& %s", cost2a(*p++));
		}
		fprintf(f, "\\\\\n");
	}
	fprintf(f, "\t\\end{pmatrix}\n");
}

void dump_edge(FILE *file, pbqp_edge *edge)
{
	fputs("<tex>\n", file);
	fprintf(file, "\t\\overline\n{C}_{%d,%d}=\n",
			edge->src->index, edge->tgt->index);
	dump_matrix(file, edge->costs);
	fputs("</tex><br>", file);
}

static void dump_edge_costs(pbqp *pbqp)
{
	unsigned src_index;

	assert(pbqp);
	assert(pbqp->dump_file);

	fputs("<p>", pbqp->dump_file);
	for (src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node *src_node = get_node(pbqp, src_index);

		if (!src_node)
			continue;

		unsigned edge_index;
		unsigned len = ARR_LEN(src_node->edges);
		for (edge_index = 0; edge_index < len; ++edge_index) {
			pbqp_edge *edge = src_node->edges[edge_index];
			unsigned tgt_index = edge->tgt->index;
			if (src_index < tgt_index) {
				dump_edge(pbqp->dump_file, edge);
			}
		}
	}
	fputs("</p>", pbqp->dump_file);
}

void dump_node(FILE *file, pbqp_node *node)
{
	assert(file);

	if (node) {
		fprintf(file, "\tc<sub>%d</sub> = ", node->index);
		dump_vector(file, node->costs);
		fputs("<br>\n", file);
	}
}

static void dump_node_costs(pbqp *pbqp)
{
	unsigned index;

	assert(pbqp);
	assert(pbqp->dump_file);

	/* dump node costs */
	fputs("<p>", pbqp->dump_file);
	for (index = 0; index < pbqp->num_nodes; ++index) {
		dump_node(pbqp->dump_file, get_node(pbqp, index));
	}
	fputs("</p>", pbqp->dump_file);
}

void dump_section(FILE *f, int level, char *txt)
{
	assert(f);

	fprintf(f, "<h%d>%s</h%d>\n", level, txt, level);
}

void pbqp_dump_graph(pbqp *pbqp)
{
	unsigned src_index;

	assert(pbqp);
	assert(pbqp->dump_file);

	fputs("<p>\n<graph>\n\tgraph input {\n", pbqp->dump_file);
	for (src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node *node = get_node(pbqp, src_index);
		if (node && !node_is_reduced(node)) {
			fprintf(pbqp->dump_file, "\t n%d;\n", src_index);
		}
	}

	for (src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node *node = get_node(pbqp, src_index);

		if (!node)
			continue;

		if (node_is_reduced(node))
			continue;

		unsigned len = ARR_LEN(node->edges);
		unsigned edge_index;
		for (edge_index = 0; edge_index < len; ++edge_index) {
			pbqp_node *tgt_node = node->edges[edge_index]->tgt;
			unsigned tgt_index = tgt_node->index;

			if (node_is_reduced(tgt_node))
				continue;

			if (src_index < tgt_index) {
				fprintf(pbqp->dump_file, "\t n%d -- n%d;\n", src_index,
						tgt_index);
			}
		}
	}
	fputs("\t}\n</graph>\n</p>\n", pbqp->dump_file);
}

void pbqp_dump_input(pbqp *pbqp)
{
	assert(pbqp);
	assert(pbqp->dump_file);

	dump_section(pbqp->dump_file, 1, "1. PBQP Problem");
	dump_section(pbqp->dump_file, 2, "1.1 Topology");
	pbqp_dump_graph(pbqp);
	dump_section(pbqp->dump_file, 2, "1.2 Cost Vectors");
	dump_node_costs(pbqp);
	dump_section(pbqp->dump_file, 2, "1.3 Cost Matrices");
	dump_edge_costs(pbqp);
}

void dump_simplifyedge(pbqp *pbqp, pbqp_edge *edge)
{
	assert(pbqp);
	assert(pbqp->dump_file);

	dump_node(pbqp->dump_file, edge->src);
	dump_edge(pbqp->dump_file, edge);
	dump_node(pbqp->dump_file, edge->tgt);
}
