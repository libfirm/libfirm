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
 * @brief       Contains some useful function for the backend.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#include "config.h"

#include <stdio.h>

#include "pset.h"

#include "irgraph.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irdom_t.h"
#include "ircons.h"
#include "iropt.h"
#include "irgopt.h"
#include "irtools.h"
#include "irprintf.h"
#include "iredges_t.h"

#include "beutil.h"
#include "besched.h"
#include "bearch.h"

/**
 * Edge hook to dump the schedule edges.
 */
static int sched_edge_hook(FILE *F, ir_node *irn)
{
	if (is_Proj(irn))
		return 1;
	if (sched_is_scheduled(irn) && sched_has_prev(irn)) {
		ir_node *prev = sched_prev(irn);
		fprintf(F, "edge:{sourcename:\"");
		PRINT_NODEID(irn);
		fprintf(F, "\" targetname:\"");
		PRINT_NODEID(prev);
		fprintf(F, "\" color:magenta}\n");
	}
	return 1;
}

void dump_ir_block_graph_sched(ir_graph *irg, const char *suffix)
{
	DUMP_NODE_EDGE_FUNC old = get_dump_node_edge_hook();

	dump_consts_local(0);
	if (have_sched_info(irg))
		set_dump_node_edge_hook(sched_edge_hook);
	dump_ir_block_graph(irg, suffix);
	set_dump_node_edge_hook(old);
}

void dump_ir_extblock_graph_sched(ir_graph *irg, const char *suffix)
{
	DUMP_NODE_EDGE_FUNC old = get_dump_node_edge_hook();

	dump_consts_local(0);
	if (have_sched_info(irg))
		set_dump_node_edge_hook(sched_edge_hook);
	dump_ir_extblock_graph(irg, suffix);
	set_dump_node_edge_hook(old);
}

/**
 * Dumps a graph and numbers all dumps.
 * @param irg    The graph
 * @param suffix A suffix to its file name.
 * @param dumper The dump function
 */
void be_dump(ir_graph *irg, const char *suffix, void (*dumper)(ir_graph *, const char *))
{
	static ir_graph *last_irg = NULL;
	static int       nr       = 0;
	char             buf[128];

	if (irg != last_irg) {
		last_irg = irg;
		nr       = strcmp(suffix, "-abi") ? 0 : 1;
	}

	snprintf(buf, sizeof(buf), "-%02d%s", nr++, suffix);
	buf[sizeof(buf) - 1] = '\0';
	dumper(irg, buf);
}

void be_clear_links(ir_graph *irg)
{
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
}

static void count_num_reachable_nodes(ir_node *irn, void *env)
{
	int *num = env;
	(*num)++;
	(void) irn;
}

unsigned get_num_reachable_nodes(ir_graph *irg)
{
	int num = 0;
	irg_walk_graph(irg, count_num_reachable_nodes, NULL, &num);
	return num;
}

/**
 * Gets the Proj with number pn from irn.
 */
ir_node *be_get_Proj_for_pn(const ir_node *irn, long pn)
{
	const ir_edge_t *edge;
	ir_node         *proj;
	assert(get_irn_mode(irn) == mode_T && "need mode_T");

	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);

		if (is_Proj(proj) && get_Proj_proj(proj) == pn)
			return proj;
	}

	return NULL;
}

FILE *be_ffopen(const char *base, const char *ext, const char *mode)
{
	FILE *out;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s.%s", base, ext);
	buf[sizeof(buf) - 1] = '\0';
	if (! (out = fopen(buf, mode))) {
		fprintf(stderr, "Cannot open file %s in mode %s\n", buf, mode);
		return NULL;
	}
	return out;
}

static void add_to_postorder(ir_node *block, void *data)
{
	ir_node ***list = (ir_node***) data;
	ARR_APP1(ir_node*, *list, block);
}

ir_node **be_get_cfgpostorder(ir_graph *irg)
{
	ir_node **list      = NEW_ARR_F(ir_node*, 0);
	ir_node  *end_block = get_irg_end_block(irg);

	/* end block may be unreachable in case of endless loops */
	if (get_Block_n_cfgpreds(end_block) == 0)
		ARR_APP1(ir_node*, list, end_block);

	/* walk blocks */
	irg_block_edges_walk(get_irg_start_block(irg), NULL, add_to_postorder,
	                     &list);

	return list;
}

ir_node *get_first_block_succ(const ir_node *block)
{
	const ir_edge_t *edge = get_irn_out_edge_first_kind(block, EDGE_KIND_BLOCK);
	assert(edge != NULL);
	return get_edge_src_irn(edge);
}
