/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   arm graph touchups before emitting
 * @author  Matthias Braun
 */
#include "arm_bearch_t.h"
#include "arm_new_nodes.h"
#include "arm_optimize.h"
#include "be2addr.h"
#include "be_types.h"
#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "bespillslots.h"
#include "bestack.h"
#include "firm_types.h"
#include "gen_arm_regalloc_if.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "panic.h"

static bool is_frame_load(const ir_node *node)
{
	return is_arm_Ldr(node) || is_arm_Ldf(node);
}

static void arm_collect_frame_entity_nodes(ir_node *node, void *data)
{
	if (!is_frame_load(node))
		return;

	const arm_load_store_attr_t *attr = get_arm_load_store_attr_const(node);
	if (!attr->is_frame_entity)
		return;
	const ir_entity *entity = attr->entity;
	if (entity != NULL)
		return;

	be_fec_env_t *const env  = (be_fec_env_t*)data;
	unsigned      const size = get_mode_size_bytes(attr->load_store_mode);
	be_load_needs_frame_entity(env, node, size, log2_floor(size));
}

static void arm_set_frame_entity(ir_node *node, ir_entity *entity,
                                 unsigned size, unsigned po2align)
{
	(void)size;
	(void)po2align;
	arm_load_store_attr_t *attr = get_arm_load_store_attr(node);
	attr->entity = entity;
}

static void introduce_epilog(ir_node *ret)
{
	assert(arch_get_irn_register_req_in(ret, n_arm_Return_sp) == &arm_single_reg_req_gp_sp);

	ir_node  *const sp         = get_irn_n(ret, n_arm_Return_sp);
	ir_node  *const block      = get_nodes_block(ret);
	ir_graph *const irg        = get_irn_irg(ret);
	ir_type  *const frame_type = get_irg_frame_type(irg);
	unsigned  const frame_size = get_type_size(frame_type);
	ir_node  *const incsp      = be_new_IncSP(block, sp, -frame_size, true);
	set_irn_n(ret, n_arm_Return_sp, incsp);
	sched_add_before(ret, incsp);
}

static void introduce_prolog_epilog(ir_graph *irg)
{
	/* introduce epilog for every return node */
	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_arm_Return(ret));
		introduce_epilog(ret);
	}

	ir_node  *const start      = get_irg_start(irg);
	ir_node  *const block      = get_nodes_block(start);
	ir_node  *const initial_sp = be_get_Start_proj(irg, &arm_registers[REG_SP]);
	ir_type  *const frame_type = get_irg_frame_type(irg);
	unsigned  const frame_size = get_type_size(frame_type);

	ir_node *const incsp = be_new_IncSP(block, initial_sp, frame_size, false);
	edges_reroute_except(initial_sp, incsp, incsp);
	sched_add_after(start, incsp);
}


static void arm_determine_frameoffset(ir_node *node, int sp_offset)
{
	if (be_is_MemPerm(node)) {
		ir_graph *irg = get_irn_irg(node);
		if (arm_get_irg_data(irg)->omit_fp)
			be_set_MemPerm_offset(node, sp_offset);
		return;
	}
	if (!is_arm_irn(node))
		return;
	const arm_attr_t *attr   = get_arm_attr_const(node);
	if (is_arm_FrameAddr(node)) {
		arm_Address_attr_t *const attr   = get_arm_Address_attr(node);
		ir_entity const    *const entity = attr->entity;
		if (entity != NULL)
			attr->fp_offset += get_entity_offset(entity);
		attr->fp_offset += sp_offset;
	} else if (attr->is_load_store) {
		arm_load_store_attr_t *const load_store_attr
			= get_arm_load_store_attr(node);
		if (load_store_attr->is_frame_entity) {
			ir_entity const *const entity = load_store_attr->entity;
			if (entity != NULL)
				load_store_attr->offset += get_entity_offset(entity);
			load_store_attr->offset += sp_offset;
		}
	}
}

static void arm_sp_sim(ir_node *const node, stack_pointer_state_t *state)
{
	arm_determine_frameoffset(node, state->offset);
}

void arm_finish_graph(ir_graph *irg)
{
	bool omit_fp = arm_get_irg_data(irg)->omit_fp;

	be_fec_env_t *fec_env = be_new_frame_entity_coalescer(irg);
	irg_walk_graph(irg, NULL, arm_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, arm_set_frame_entity, omit_fp);
	be_free_frame_entity_coalescer(fec_env);

	ir_type *const frame = get_irg_frame_type(irg);
	be_sort_frame_entities(frame, omit_fp);
	unsigned const misalign = 0;
	be_layout_frame_type(frame, 0, misalign);

	introduce_prolog_epilog(irg);

	/* fix stack entity offsets */
	be_fix_stack_nodes(irg, &arm_registers[REG_SP]);
	be_birg_from_irg(irg)->non_ssa_regs = NULL;
	be_sim_stack_pointer(irg, misalign, ARM_PO2_STACK_ALIGNMENT, arm_sp_sim);

	/* do peephole optimizations and fix stack offsets */
	arm_peephole_optimization(irg);

	be_handle_2addr(irg, NULL);
}
