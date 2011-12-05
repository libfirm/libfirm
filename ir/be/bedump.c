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
 * @brief       Code for dumping backend datastructures (i.e. interference graphs)
 * @author      Matthias Braun
 */
#include "config.h"

#include "bedump.h"

#include "irdump_t.h"
#include "irgwalk.h"
#include "beifg.h"
#include "becopyopt_t.h"
#include "belive_t.h"

static void dump_ifg_nodes(FILE *F, const be_ifg_t *ifg)
{
	nodes_iter_t ifg_iter;
	ir_node     *node;
	be_ifg_foreach_node(ifg, &ifg_iter, node) {
		dump_node(F, node);
	}
}

static void dump_ifg_edges(FILE *F, const be_ifg_t *ifg)
{
	nodes_iter_t ifg_iter;
	ir_node     *node;

	be_ifg_foreach_node(ifg, &ifg_iter, node) {
		neighbours_iter_t neigh_iter;
		ir_node          *neighbour;

		be_ifg_foreach_neighbour(ifg, &neigh_iter, node, neighbour) {
			/* interference is bidirectional, but it's enough to dump 1
			 * direction */
			if (get_irn_node_nr(node) >= get_irn_node_nr(neighbour))
				continue;

			fprintf(F, "edge: {sourcename: \"");
			PRINT_NODEID(node);
			fprintf(F, "\" targetname: \"");
			PRINT_NODEID(neighbour);
			fprintf(F, "\" arrowstyle:none class:1}\n");
		}
	}
}

void be_dump_ifg(FILE *F, ir_graph *irg, const be_ifg_t *ifg)
{
	ir_fprintf(F,
		"graph: { title: \"interference graph of %+F\"\n"
		"layoutalgorithm: mindepth //$ \"circular\"\n"
		"classname 1: \"interference\"\n"
		, irg);
	dump_vcg_infonames(F);
	dump_vcg_header_colors(F);

	dump_ifg_nodes(F, ifg);
	dump_ifg_edges(F, ifg);

	fprintf(F, "}\n");
}

static void dump_affinity_edges(FILE *F, const copy_opt_t *co,
                                bool dump_costs, bool dump_colors)
{
	affinity_node_t *a;
	co_gs_foreach_aff_node(co, a) {
		neighb_t *n;

		co_gs_foreach_neighb(a, n) {
			/* edges are bidirection, dumping one direction is enough */
			if (get_irn_node_nr(a->irn) >= get_irn_node_nr(n->irn))
				continue;

			fprintf(F, "edge: {sourcename: \"");
			PRINT_NODEID(a->irn);
			fprintf(F, "\" targetname: \"");
			PRINT_NODEID(n->irn);
			fprintf(F, "\" arrowstyle:none");

			if (dump_costs)
				fprintf(F, " label:\"%d\"", n->costs);
			if (dump_colors) {
				const arch_register_t *ar = arch_get_irn_register(a->irn);
				const arch_register_t *nr = arch_get_irn_register(n->irn);
				const char *color = nr == ar ? "blue" : "red";
				fprintf(F, " color:%s", color);
			}
			fprintf(F, " linestyle:dashed class:2");
			fprintf(F, "}\n");
		}
	}
}

void be_dump_ifg_co(FILE *F, const copy_opt_t *co, bool dump_costs,
                    bool dump_colors)
{
	ir_graph *irg = co->irg;
	be_ifg_t *ifg = co->cenv->ifg;

	ir_fprintf(F,
		"graph: { title: \"interference graph of %+F\"\n"
		"layoutalgorithm: mindepth //$ \"circular\"\n"
		"classname 1: \"interference\"\n"
		"classname 2: \"affinity\"\n"
		, irg);
	dump_vcg_infonames(F);
	dump_vcg_header_colors(F);

	dump_ifg_nodes(F, ifg);
	dump_ifg_edges(F, ifg);
	dump_affinity_edges(F, co, dump_costs, dump_colors);

	fprintf(F, "}\n");
}

static const char *lv_flags_to_str(unsigned flags)
{
	static const char *states[] = {
		"---",
		"i--",
		"-e-",
		"ie-",
		"--o",
		"i-o",
		"-eo",
		"ieo"
	};

	return states[flags & 7];
}

void be_dump_liveness_block(void *context, FILE *F, const ir_node *bl)
{
	if (is_Block(bl)) {
		be_lv_t *lv = (be_lv_t*)context;
		be_lv_info_t *info = (be_lv_info_t*)ir_nodehashmap_get(&lv->map, bl);

		fprintf(F, "liveness:\n");
		if (info != NULL) {
			unsigned n = info[0].head.n_members;
			unsigned i;

			for (i = 0; i < n; ++i) {
				be_lv_info_node_t *n = &info[i+1].node;
				ir_fprintf(F, "%s %+F\n", lv_flags_to_str(n->flags), get_idx_irn(lv->irg, n->idx));
			}
		}
	}
}

typedef struct lv_walker_t {
	be_lv_t *lv;
	void *data;
} lv_walker_t;

static void lv_dump_block_walker(ir_node *irn, void *data)
{
	lv_walker_t *w = (lv_walker_t*)data;
	if (!is_Block(irn))
		return;
	be_dump_liveness_block(w->lv, (FILE*)w->data, irn);
}

void be_liveness_dump(FILE *F, const be_lv_t *lv)
{
	lv_walker_t w;

	w.lv   = (be_lv_t *) lv;
	w.data = F;
	irg_block_walk_graph(lv->irg, lv_dump_block_walker, NULL, &w);
}
