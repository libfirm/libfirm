/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Verifier for VFirm graphs.
 * @author  Julian Oppermann
 * @version $Id: $
 */

#include "config.h"
#include "irgwalk.h"
#include "irnode.h"
#include "iredges.h"
#include "pdeq.h"
#include "irnodemap.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

typedef struct obstack obstack;

static ir_nodemap *la_nodemap;
static pdeq       *la_worklist;
static obstack    *la_obst;

struct la_info {
	unsigned loop_depth;
};
typedef struct la_info la_info;

static la_info *get_la_info(const ir_node *irn)
{
	return ir_nodemap_get(la_nodemap, irn);
}

static la_info *get_or_set_la_info(const ir_node *irn)
{
	la_info *lai = get_la_info(irn);
	if (! lai) {
		lai = obstack_alloc(la_obst, sizeof(la_info));
		lai->loop_depth = 0;
		ir_nodemap_insert(la_nodemap, irn, lai);
	}
	return lai;
}

static void la_init_node(ir_node *node, void *env)
{
	if (! (bool)env)
		pdeq_putr(la_worklist, node);
	la_info *lai = get_or_set_la_info(node);
	lai->loop_depth = 0;
}

static void la_analyze_node(ir_node *node)
{
	la_info *lai    = get_la_info(node);
	unsigned old_ld = lai->loop_depth;
	lai->loop_depth = 0;

	if (is_Theta(node))
		lai->loop_depth = get_Theta_depth(node);

	int arity = get_irn_arity(node), i;
	for (i = 0; i < arity; i++) {
		ir_node *pred     = get_irn_n(node, i);
		la_info *pred_lai = get_la_info(pred);
		lai->loop_depth = lai->loop_depth > pred_lai->loop_depth ? lai->loop_depth : pred_lai->loop_depth;
	}

	if (is_Eta(node))
		lai->loop_depth = lai->loop_depth > 0 ? lai->loop_depth-1 : 0;

	bool changed = old_ld != lai->loop_depth;
	if (! changed)
		return;

	const ir_edge_t *edge, *tmp;
	foreach_out_edge_safe(node, edge, tmp) {
		ir_node *src = get_edge_src_irn(edge);
		if (is_Anchor(src))
			continue;
		pdeq_putr(la_worklist, src);
	}
}

static void la_analyze(ir_graph *irg)
{
	assert (pdeq_len(la_worklist) == 0);

	irg_walk_graph(irg, NULL, la_init_node, NULL);

	ir_node *n;
	while (pdeq_len(la_worklist) > 0) {
		n = pdeq_getl(la_worklist);
		la_analyze_node(n);
	}
}

static void verify_vfirm_node_walk(ir_node *node, void *env)
{
	(void) env;
	if (is_Theta(node))
		assert (get_la_info(node)->loop_depth == get_Theta_depth(node)
				&& "VFirm verifier: Theta has invalid loop depth");

	if (is_Eta(node))
		assert ((get_la_info(get_Eta_value(node))->loop_depth > 0 || get_la_info(get_Eta_cond(node))->loop_depth > 0)
				&& "VFirm verifier: Eta using loop invariant values");
}

void vf_verify(ir_graph *irg)
{
	edges_assure(irg);

	la_worklist = new_pdeq();
	obstack obst;
	la_obst = &obst;
	obstack_init(la_obst);

	la_nodemap = obstack_alloc(la_obst, sizeof(ir_nodemap));
	ir_nodemap_init(la_nodemap, irg);

	la_analyze(irg);

	ir_node *end_block = get_irg_end_block(irg);
	assert (get_Block_n_cfgpreds(end_block) == 1);

	ir_node *ret       = get_Block_cfgpred(end_block, 0);
	assert (is_Return(ret));

	la_info *lai = get_la_info(ret);
	assert (lai->loop_depth == 0 && "VFirm verifier: Return has loop depth != 0");

	irg_walk_graph(irg, NULL, verify_vfirm_node_walk, NULL);

	ir_nodemap_destroy(la_nodemap);
	obstack_free(la_obst, NULL);
	del_pdeq(la_worklist);

	la_nodemap  = NULL;
	la_obst     = NULL;
	la_worklist = NULL;
}
