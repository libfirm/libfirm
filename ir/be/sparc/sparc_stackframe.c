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
#include "bearch.h"
#include "benode.h"
#include "besched.h"

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

static void process_bias(ir_node *block, bool sp_relative, int bias,
                         int free_bytes)
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
				offset += bias;
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
			new_bias_aligned
				= round_up2(new_bias_unaligned, SPARC_STACK_ALIGNMENT);
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

/**
 * Perform some fixups for variadic functions.
 * To make the rest of the frontend code easier to understand we add
 * "dummy" parameters until the number of parameters transmitted in registers.
 * (because otherwise the backend wouldn't store the value of the register
 *  parameters into memory for the VLA magic)
 */
bool sparc_variadic_fixups(ir_graph *irg, calling_convention_t *cconv)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *mtp    = get_entity_type(entity);
	if (get_method_variadicity(mtp) != variadicity_variadic)
		return false;

	if (cconv->n_param_regs >= SPARC_N_PARAM_REGS)
		return false;

	{
	size_t         n_params     = get_method_n_params(mtp);
	type_dbg_info *dbgi         = get_type_dbg_info(mtp);
	size_t         n_ress       = get_method_n_ress(mtp);
	size_t         new_n_params
		= n_params + (SPARC_N_PARAM_REGS - cconv->n_param_regs);
	ir_type       *new_mtp      = new_d_type_method(new_n_params, n_ress, dbgi);
	ir_mode       *gp_reg_mode  = sparc_reg_classes[CLASS_sparc_gp].mode;
	ir_type       *gp_reg_type  = get_type_for_mode(gp_reg_mode);
	ir_type       *frame_type   = get_irg_frame_type(irg);
	size_t         i;

	for (i = 0; i < n_ress; ++i) {
		ir_type *type = get_method_res_type(mtp, i);
		set_method_res_type(new_mtp, i, type);
	}
	for (i = 0; i < n_params; ++i) {
		ir_type *type = get_method_param_type(mtp, i);
		set_method_param_type(new_mtp, i, type);
	}
	for ( ; i < new_n_params; ++i) {
		set_method_param_type(new_mtp, i, gp_reg_type);
		new_parameter_entity(frame_type, i, gp_reg_type);
	}

	set_method_variadicity(new_mtp, get_method_variadicity(mtp));
	set_method_calling_convention(new_mtp, get_method_calling_convention(mtp));
	set_method_additional_properties(new_mtp, get_method_additional_properties(mtp));
	set_higher_type(new_mtp, mtp);

	set_entity_type(entity, new_mtp);
	}
	return true;
}

static ir_type *compute_arg_type(ir_graph *irg, calling_convention_t *cconv,
                                 ir_type *between_type)
{
	ir_entity       *va_start_entity = NULL;
	const ir_entity *entity          = get_irg_entity(irg);
	const ir_type   *mtp             = get_entity_type(entity);
	size_t           n_params        = get_method_n_params(mtp);
	ir_entity      **param_map       = ALLOCANZ(ir_entity*, n_params);

	ir_type *frame_type      = get_irg_frame_type(irg);
	size_t   n_frame_members = get_compound_n_members(frame_type);
	size_t   f;
	size_t   i;

	ir_type *res = new_type_struct(id_mangle_u(get_entity_ident(entity), new_id_from_chars("arg_type", 8)));

	/* search for existing value_param entities */
	for (f = n_frame_members; f > 0; ) {
		ir_entity *member = get_compound_member(frame_type, --f);
		size_t     num;

		if (!is_parameter_entity(member))
			continue;
		num = get_entity_parameter_number(member);
		if (num == IR_VA_START_PARAMETER_NUMBER) {
			if (va_start_entity != NULL)
				panic("multiple va_start entities found (%+F,%+F)",
				      va_start_entity, member);
			va_start_entity = member;
			continue;
		}
		assert(num < n_params);
		if (param_map[num] != NULL)
			panic("multiple entities for parameter %u in %+F found", f, irg);

		param_map[num] = member;
		/* move to new arg_type */
		set_entity_owner(member, res);
	}

	/* calculate offsets/create missing entities */
	for (i = 0; i < n_params; ++i) {
		reg_or_stackslot_t *param  = &cconv->parameters[i];
		ir_entity          *entity = param_map[i];

		if (param->reg0 != NULL) {
			/* use reserved spill space on between type */
			if (entity != NULL) {
				long offset = SPARC_PARAMS_SPILL_OFFSET + i * SPARC_REGISTER_SIZE;
				assert(i < SPARC_N_PARAM_REGS);
				set_entity_owner(entity, between_type);
				set_entity_offset(entity, offset);
			}
			continue;
		}

		if (entity == NULL)
			entity = new_parameter_entity(res, i, param->type);
		param->entity = entity;
		set_entity_offset(entity, param->offset);
	}

	if (va_start_entity != NULL) {
		/* sparc_variadic_fixups() fiddled with our type, find out the
		 * original number of parameters */
		ir_type *non_lowered   = get_higher_type(mtp);
		size_t   orig_n_params = get_method_n_params(non_lowered);
		long     offset;
		assert(get_method_variadicity(mtp) == variadicity_variadic);
		if (orig_n_params < n_params) {
			assert(param_map[orig_n_params] != NULL);
			offset = get_entity_offset(param_map[orig_n_params]);
			set_entity_owner(va_start_entity, between_type);
			set_entity_offset(va_start_entity, offset);
		} else {
			set_entity_owner(va_start_entity, res);
			set_entity_offset(va_start_entity, cconv->param_stack_size);
		}
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
	layout->arg_type       = compute_arg_type(irg, cconv, between_type);
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
	 *      high address |-----------------------------|
	 *                   |            ...              |
	 *          arg-type |         stackarg 1          |
	 *                   |         stackarg 0          |
	 *                   |-----------------------------|
	 *                   | space for storing regarg0-5 |
	 *      between type | pointer to aggregate return |
	 *                   |      16 words save are      |
	 *  stack pointer -> |-----------------------------|
	 *                   |    high end of stackframe   |
	 *                   |            ...              |
	 *                   |    low end of stackframe    |
	 *      low address  |-----------------------------|
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
	ir_node *start_block = get_irg_start_block(irg);

	process_frame_types(irg);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	process_bias(start_block, be_get_irg_stack_layout(irg)->sp_relative, 0, 0);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}
