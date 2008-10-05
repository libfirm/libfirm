#include "adt/array.h"
#include "assert.h"

#include "pbqp_edge_t.h"
#include "pbqp_node_t.h"
#include "html_dumper.h"
#include "kaps.h"
#include "pbqp_t.h"

/* print vector */
static void dump_vector(FILE *f, vector *vec)
{
	unsigned index;
	assert(vec);

	fprintf(f, "<span class=\"vector\">( ");
	unsigned len = vec->len;
	assert(len > 0);
	for (index = 0; index < len; ++index) {
#if EXT_GRS_DEBUG
		if (vec->entries[index].data == INF_COSTS) {
			fprintf(f, "<span title=\"%s\"> inf </span>",
					vec->entries[index].name);
		} else {
			fprintf(f, "<span title=\"%s\">%6d</span>",
					vec->entries[index].name, vec->entries[index].data);
		}
#else
		if (vec->entries[index].data == INF_COSTS) {
			fputs(" inf ", f);
		} else {
			fprintf(f, "%6d", vec->entries[index].data);
		}
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
		if (*p == INF_COSTS) {
			fputs("\t inf", f);
			p++;
		} else {
			fprintf(f, "\t %6d", *p++);
		}

		for (col = 1; col < mat->cols; ++col) {
			if (*p == INF_COSTS) {
				fputs("& inf", f);
				p++;
			} else {
				fprintf(f, "& %6d", *p++);
			}
		}
		fprintf(f, "\\\\\n");
	}
	fprintf(f, "\t\\end{pmatrix}\n");
}

void dump_edge(pbqp *pbqp, pbqp_edge *edge)
{
	fputs("<tex>\n", pbqp->dump_file);
	fprintf(pbqp->dump_file, "\t\\overline\n{C}_{%d,%d}=\n",
			edge->src->index, edge->tgt->index);
	dump_matrix(pbqp->dump_file, edge->costs);
	fputs("</tex><br>", pbqp->dump_file);
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
				dump_edge(pbqp, edge);
			}
		}
	}
	fputs("</p>", pbqp->dump_file);
}

void dump_node(pbqp *pbqp, pbqp_node *node)
{
	assert(pbqp);
	assert(pbqp->dump_file);

	if (node) {
		fprintf(pbqp->dump_file, "\tc<sub>%d</sub> = ", node->index);
		dump_vector(pbqp->dump_file, node->costs);
		fputs("<br>\n", pbqp->dump_file);
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
		dump_node(pbqp, get_node(pbqp, index));
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
		if (node) {
			fprintf(pbqp->dump_file, "\t n%d;\n", src_index);
		}
	}

	for (src_index = 0; src_index < pbqp->num_nodes; ++src_index) {
		pbqp_node *node = get_node(pbqp, src_index);

		if (!node)
			continue;

		unsigned len = ARR_LEN(node->edges);
		unsigned edge_index;
		for (edge_index = 0; edge_index < len; ++edge_index) {
			unsigned tgt_index = node->edges[edge_index]->tgt->index;

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

	dump_node(pbqp, edge->src);
	dump_edge(pbqp, edge);
	dump_node(pbqp, edge->tgt);
}
