/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief   Manage addressing into the stackframe
 * @author  Matthias Braun
 * @version $Id$
 */
#include "config.h"

#include "firm_types.h"
#include "irnode_t.h"
#include "bearch_sparc_t.h"
#include "sparc_new_nodes.h"
#include "sparc_cconv.h"
#include "bitfiddle.h"
#include "../bearch.h"
#include "../benode.h"
#include "../besched.h"

static void set_irn_sp_bias(ir_node *node, int new_bias)
{
	if (be_is_IncSP(node)) {
		be_set_IncSP_offset(node, new_bias);
	} else if (is_sparc_Save(node)) {
		sparc_attr_t *attr = get_sparc_attr(node);
		attr->immediate_value = -new_bias;
	} else if (is_sparc_Restore(node)) {
		sparc_attr_t *attr = get_sparc_attr(node);
		attr->immediate_value = new_bias;
	}
}

static void process_bias(ir_node *block, bool sp_relative, int bias, int free_bytes)
{
	const ir_edge_t *edge;
	ir_node         *irn;

	mark_Block_block_visited(block);

	/* process schedule */
	sched_foreach(block, irn) {
		int irn_bias;

		/* set bias to nodes with entities */
		ir_entity *entity = arch_get_frame_entity(irn);
		if (entity != NULL) {
			int offset = get_entity_offset(entity);
			if (sp_relative)
				offset -= bias;
			arch_set_frame_offset(irn, offset);
		}

		irn_bias = arch_get_sp_bias(irn);
		if (irn_bias == 0) {
			/* do nothing */
		} else if (irn_bias == SP_BIAS_RESET) {
			bias = 0;
		} else {
			/* adjust values to respect stack alignment */
			int new_bias_unaligned;
			int new_bias_aligned;
			irn_bias -= free_bytes;

			new_bias_unaligned = bias + irn_bias;
			new_bias_aligned   = round_up2(new_bias_unaligned, 8);
			free_bytes = new_bias_aligned - new_bias_unaligned;
			set_irn_sp_bias(irn, new_bias_aligned - bias);
			bias = new_bias_aligned;
		}
	}

#ifndef NDEBUG
	if (block == get_irg_end_block(get_irn_irg(block))) {
		assert(bias == 0);
	}
#endif

	/* continue at the successor blocks */
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		if (Block_block_visited(succ))
			continue;
		process_bias(succ, sp_relative, bias, free_bytes);
	}
}

static void adjust_entity_offsets(ir_type *type, long offset)
{
	size_t n_members = get_compound_n_members(type);
	size_t i;

	for (i = 0; i < n_members; ++i) {
		ir_entity *member        = get_compound_member(type, i);
		int        member_offset = get_entity_offset(member);
		set_entity_offset(member, member_offset + offset);
	}
}

static ir_type *compute_arg_type(ir_graph *irg, calling_convention_t *cconv)
{
	ir_entity  *entity    = get_irg_entity(irg);
	ir_type    *mtp       = get_entity_type(entity);
	size_t      n_params  = get_method_n_params(mtp);
	ir_entity **param_map = ALLOCANZ(ir_entity*, n_params);

	ir_type *frame_type      = get_irg_frame_type(irg);
	size_t   n_frame_members = get_compound_n_members(frame_type);
	size_t   f;
	size_t   i;

	ir_type *res = new_type_struct(id_mangle_u(get_entity_ident(entity), new_id_from_chars("arg_type", 8)));

	/* search for existing value_param entities */
	for (f = n_frame_members; f > 0; ) {
		ir_entity *member = get_compound_member(frame_type, --f);
		size_t     num;
		const reg_or_stackslot_t *param;

		if (!is_parameter_entity(member))
			continue;
		num = get_entity_parameter_number(member);
		assert(num < n_params);
		if (param_map[num] != NULL)
			panic("multiple entities for parameter %u in %+F found", f, irg);

		param = &cconv->parameters[num];
		if (param->reg0 != NULL)
			continue;

		param_map[num] = member;
		/* move to new arg_type */
		set_entity_owner(member, res);
	}

	for (i = 0; i < n_params; ++i) {
		reg_or_stackslot_t *param = &cconv->parameters[i];
		ir_entity          *entity;

		if (param->reg0 != NULL)
			continue;
		entity = param_map[i];
		if (entity == NULL)
			entity = new_parameter_entity(res, i, param->type);
		param->entity = entity;
		set_entity_offset(entity, param->offset);
	}
	set_type_size_bytes(res, cconv->param_stack_size);

	return res;
}

void sparc_create_stacklayout(ir_graph *irg, calling_convention_t *cconv)
{
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
	ir_type           *between_type;
	memset(layout, 0, sizeof(*layout));

	between_type = new_type_class(new_id_from_str("sparc_between_type"));
	set_type_size_bytes(between_type, SPARC_MIN_STACKSIZE);

	layout->frame_type     = get_irg_frame_type(irg);
	layout->between_type   = between_type;
	layout->arg_type       = compute_arg_type(irg, cconv);
	layout->initial_offset = 0;
	layout->initial_bias   = 0;
	layout->sp_relative    = cconv->omit_fp;

	assert(N_FRAME_TYPES == 3);
	layout->order[0] = layout->frame_type;
	layout->order[1] = layout->between_type;
	layout->order[2] = layout->arg_type;
}

/* Assign entity offsets, to all stack-related entities.
 * The offsets are relative to the begin of the stack frame.
 */
static void process_frame_types(ir_graph *irg)
{
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);

	/* initially the stackpointer points to the begin of our stackframe.
	 * Situation at the begin of our function:
	 *
	 *      high address |----------------------------|
	 *                   | ...                        |
	 *          arg-type | stackarg 1                 |
	 *                   | stackarg 0                 |
	 *                   |----------------------------|
	 *      between type | 92-bytes utility+save area |
	 *  stack pointer -> |----------------------------|
	 *                   | high end of stackframe     |
	 *                   |          ...               |
	 *                   | low end of stackframe      |
	 *                   |----------------------------|
	 */
	ir_type *between_type = layout->between_type;
	unsigned between_size = get_type_size_bytes(between_type);

	ir_type *frame_type = get_irg_frame_type(irg);
	unsigned frame_size = get_type_size_bytes(frame_type);

	ir_type *arg_type = layout->arg_type;

	adjust_entity_offsets(frame_type, -(long)frame_size);
	/* no need to adjust between type, it's already at 0 */
	adjust_entity_offsets(arg_type, between_size);
}

void sparc_fix_stack_bias(ir_graph *irg)
{
	ir_node           *start_block  = get_irg_start_block(irg);

	process_frame_types(irg);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	process_bias(start_block, be_get_irg_stack_layout(irg)->sp_relative, 0, 0);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}
