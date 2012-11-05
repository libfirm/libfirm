/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author      Matthias Braun
 */
#include <stdbool.h>

#include "beinfo.h"
#include "beirg.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "bedump.h"
#include "belive_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irdump_t.h"
#include "irhooks.h"
#include "error.h"

static copy_attr_func old_phi_copy_attr;

void be_info_new_node(ir_graph *irg, ir_node *node)
{
	struct obstack *obst;
	backend_info_t *info;

	/* Projs need no be info, all info is fetched from their predecessor */
	if (is_Proj(node))
		return;

	obst = be_get_be_obst(irg);
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
		info->flags |= arch_irn_flag_not_scheduled;
		/* FALLTHROUGH */
	case iro_Phi:
		info->out_infos        = NEW_ARR_DZ(reg_out_info_t, obst, 1);
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

	new_info->in_reqs = old_info->in_reqs;
	size_t n_outs = arch_get_irn_n_outs(old_node);
	memcpy(new_info->out_infos, old_info->out_infos,
	       n_outs * sizeof(old_info->out_infos[0]));

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
	ir_graph *irg = get_irn_irg(node);
	(void) data;
	be_info_new_node(irg, node);
}

static bool         initialized = false;
static hook_entry_t hook_liveness_info;

static void dump_liveness_info_hook(void *context, FILE *F, const ir_node *node)
{
	(void)context;
	if (!is_Block(node))
		return;
	ir_graph *irg = get_irn_irg(node);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND))
		return;

	be_lv_t *lv = be_get_irg_liveness(irg);
	if (lv == NULL)
		return;
	if (!lv->sets_valid)
		return;

	be_dump_liveness_block(lv, F, node);
}

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

	hook_liveness_info.hook._hook_node_info = dump_liveness_info_hook;
	register_hook(hook_node_info, &hook_liveness_info);
}

/**
 * Edge hook to dump the schedule edges.
 */
static void sched_edge_hook(FILE *F, const ir_node *irn)
{
	ir_graph *irg = get_irn_irg(irn);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND))
		return;

	if (is_Proj(irn) || is_Block(irn) || !sched_is_scheduled(irn))
		return;

	ir_node *const prev = sched_prev(irn);
	if (!sched_is_begin(prev)) {
		fprintf(F, "edge:{sourcename: ");
		print_nodeid(F, irn);
		fprintf(F, " targetname: ");
		print_nodeid(F, prev);
		fprintf(F, " color:magenta}\n");
	}
}

void be_info_init_irg(ir_graph *irg)
{
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_BACKEND);
	irg_walk_anchors(irg, init_walker, NULL, NULL);

	set_dump_node_edge_hook(sched_edge_hook);
}

void be_info_free(void)
{
	if (!initialized)
		panic("called without prior init");

	assert(op_Phi->ops.copy_attr == new_phi_copy_attr);
	op_Phi->ops.copy_attr = old_phi_copy_attr;
	initialized = false;

	assert(op_Phi->ops.dump_node == be_dump_phi_reg_reqs);
	op_Phi->ops.dump_node = NULL;

	unregister_hook(hook_node_info, &hook_liveness_info);
}
