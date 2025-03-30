/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author      Matthias Braun
 */
#include "beinfo.h"

#include "bedump.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "irdump_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "panic.h"
#include "util.h"
#include <stdbool.h>

static copy_attr_func old_phi_copy_attr;

void be_info_init_irn(ir_node *const node, arch_irn_flags_t const flags, arch_register_req_t const **const in_reqs, unsigned const n_res)
{
	ir_graph       *const irg  = get_irn_irg(node);
	struct obstack *const obst = get_irg_obstack(irg);
	backend_info_t *const info = be_get_info(node);
	info->flags     = flags;
	info->in_reqs   = in_reqs;
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_res);
}

void be_info_new_node(ir_graph *irg, ir_node *node)
{
	/* Projs need no be info, all info is fetched from their predecessor */
	if (is_Proj(node))
		return;

	struct obstack *obst = be_get_be_obst(irg);
	backend_info_t *info = OALLOCZ(obst, backend_info_t);

	assert(node->backend_info == NULL);
	node->backend_info = info;

	/*
	 * Set backend info for some middleend nodes which still appear in
	 * backend graphs
	 */
	arch_irn_flags_t           flags = arch_irn_flag_not_scheduled;
	arch_register_req_t const *req   = arch_no_register_req;
	switch (get_irn_opcode(node)) {
	case iro_Block:
	case iro_Dummy:
	case iro_Anchor:
	case iro_Bad:
	case iro_End:
	case iro_Unknown:
		break;
	case iro_NoMem:
	case iro_Pin:
	case iro_Sync:
		req = arch_memory_req;
		break;
	case iro_Phi:
		flags = arch_irn_flag_schedule_first;
		break;
	default:
		return;
	}

	info->flags     = flags;
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, 1);
	info->out_infos[0].req = req;
}

static void new_phi_copy_attr(ir_graph *irg, const ir_node *old_node,
                              ir_node *new_node)
{
	backend_info_t *old_info = be_get_info(old_node);
	backend_info_t *new_info = be_get_info(new_node);

	new_info->in_reqs = old_info->in_reqs;
	size_t const n_outs = arch_get_irn_n_outs(old_node);
	MEMCPY(new_info->out_infos, old_info->out_infos, n_outs);

	old_phi_copy_attr(irg, old_node, new_node);
}

int attrs_equal_be_node(const ir_node *node1, const ir_node *node2)
{
	const backend_info_t *info1 = be_get_info(node1);
	const backend_info_t *info2 = be_get_info(node2);
	size_t                len   = ARR_LEN(info1->out_infos);
	if (ARR_LEN(info2->out_infos) != len)
		return false;

	int arity = get_irn_arity(node1);
	assert(arity == get_irn_arity(node2));
	for (int in = 0; in < arity; ++in) {
		if (info1->in_reqs[in] != info2->in_reqs[in])
			return false;
	}

	for (size_t i = 0; i < len; ++i) {
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
static hook_entry_t hook_backend_info;

static void dump_backend_info_hook(void *context, FILE *F, const ir_node *node)
{
	(void)context;

	ir_graph *const irg = get_irn_irg(node);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND))
		return;

	be_dump_reqs_and_registers(F, node);

	if (is_Block(node)) {
		be_lv_t *const lv = be_get_irg_liveness(irg);
		if (lv->sets_valid)
			be_dump_liveness_block(lv, F, node);
	}

#ifndef NDEBUG
	if (!is_Proj(node)) {
		char const *const orig = be_get_info(node)->orig_node;
		fprintf(F, "orig node = %s\n", orig ? orig : "n/a");
	}
#endif
}

void be_info_init(void)
{
	if (initialized)
		panic("double initialization of be_info");

	old_phi_copy_attr = op_Phi->ops.copy_attr;
	set_op_copy_attr(op_Phi, new_phi_copy_attr);
	initialized = true;

	hook_backend_info.hook._hook_node_info = dump_backend_info_hook;
	register_hook(hook_node_info, &hook_backend_info);
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

	unregister_hook(hook_node_info, &hook_backend_info);
}
