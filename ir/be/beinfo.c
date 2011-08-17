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
 * @author      Matthias Braun
 * @version     $Id$
 */
#include "config.h"

#include <stdbool.h>

#include "beinfo.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irdump_t.h"
#include "error.h"

static copy_attr_func old_phi_copy_attr;

void be_info_new_node(ir_node *node)
{
	struct obstack *obst;
	backend_info_t *info;

	/* Projs need no be info, all info is fetched from their predecessor */
	if (is_Proj(node))
		return;

	obst = be_get_be_obst(current_ir_graph);
	info = OALLOCZ(obst, backend_info_t);

	assert(node->backend_info == NULL);
	node->backend_info = info;

	/*
	 * Set backend info for some middleend nodes which still appear in
	 * backend graphs
	 */
	switch (get_irn_opcode(node)) {
	case iro_Block:
	case iro_Dummy:
	case iro_NoMem:
	case iro_Anchor:
	case iro_Pin:
	case iro_Sync:
	case iro_Bad:
	case iro_End:
	case iro_Unknown:
		info->flags |= arch_irn_flags_not_scheduled;
		info->out_infos = NEW_ARR_D(reg_out_info_t, obst, 1);
		memset(info->out_infos, 0, 1 * sizeof(info->out_infos[0]));
		info->out_infos[0].req = arch_no_register_req;
		break;
	case iro_Phi:
		info->out_infos = NEW_ARR_D(reg_out_info_t, obst, 1);
		memset(info->out_infos, 0, 1 * sizeof(info->out_infos[0]));
		info->out_infos[0].req = arch_no_register_req;
		break;
	default:
		break;
	}
}

static void new_phi_copy_attr(ir_graph *irg, const ir_node *old_node,
                              ir_node *new_node)
{
	backend_info_t *old_info = be_get_info(old_node);
	backend_info_t *new_info = be_get_info(new_node);

	*new_info = *old_info;

	old_phi_copy_attr(irg, old_node, new_node);
}

int be_nodes_equal(const ir_node *node1, const ir_node *node2)
{
	const backend_info_t *info1 = be_get_info(node1);
	const backend_info_t *info2 = be_get_info(node2);
	size_t                len   = ARR_LEN(info1->out_infos);
	int                   arity = get_irn_arity(node1);
	int                   in;
	size_t                i;

	if (ARR_LEN(info2->out_infos) != len)
		return false;

	assert(arity == get_irn_arity(node2));

	for (in = 0; in < arity; ++in) {
		if (info1->in_reqs[in] != info2->in_reqs[in])
			return false;
	}

	for (i = 0; i < len; ++i) {
		const reg_out_info_t *out1 = &info1->out_infos[i];
		const reg_out_info_t *out2 = &info2->out_infos[i];
		if (out1->reg != out2->reg)
			return false;
		if (!reg_reqs_equal(out1->req, out2->req))
			return false;
	}

	return true;
}

static void init_walker(ir_node *node, void *data)
{
	(void) data;
	be_info_new_node(node);
}

static bool initialized = false;

void be_info_init(void)
{
	if (initialized)
		panic("double initialization of be_info");

	old_phi_copy_attr = op_Phi->ops.copy_attr;
	op_Phi->ops.copy_attr = new_phi_copy_attr;
	initialized = true;

	/* phis have register and register requirements now which we want to dump */
	assert(op_Phi->ops.dump_node == NULL);
	op_Phi->ops.dump_node = be_dump_phi_reg_reqs;
}

/**
 * Edge hook to dump the schedule edges.
 */
static void sched_edge_hook(FILE *F, ir_node *irn)
{
	if (is_Proj(irn))
		return;
	if (get_irn_irg(irn)->be_data == NULL)
		return;

	if (sched_is_scheduled(irn) && sched_has_prev(irn)) {
		ir_node *prev = sched_prev(irn);
		fprintf(F, "edge:{sourcename:\"");
		PRINT_NODEID(irn);
		fprintf(F, "\" targetname:\"");
		PRINT_NODEID(prev);
		fprintf(F, "\" color:magenta}\n");
	}
}

void be_info_init_irg(ir_graph *irg)
{
	irg_walk_anchors(irg, init_walker, NULL, NULL);

	set_dump_node_edge_hook(sched_edge_hook);
}

void be_info_free(void)
{
	if (!initialized)
		panic("be_info_free called without prior init");

	assert(op_Phi->ops.copy_attr == new_phi_copy_attr);
	op_Phi->ops.copy_attr = old_phi_copy_attr;
	initialized = false;

	assert(op_Phi->ops.dump_node == be_dump_phi_reg_reqs);
	op_Phi->ops.dump_node = NULL;
}

int be_info_initialized(const ir_graph *irg)
{
	(void) irg;
	return initialized;
}
