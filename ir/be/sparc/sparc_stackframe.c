/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Manage addressing into the stackframe
 * @author  Matthias Braun
 */
#include "beirg.h"
#include "panic.h"
#include "firm_types.h"
#include "iredges_t.h"
#include "irnode_t.h"
#include "bearch_sparc_t.h"
#include "sparc_new_nodes.h"
#include "sparc_cconv.h"
#include "bitfiddle.h"
#include "benode.h"
#include "besched.h"
#include "bestack.h"

static ir_entity *va_start_entity;

static void sparc_set_frame_offset(ir_node *node, int offset)
{
	if (be_is_MemPerm(node)) {
		be_set_MemPerm_offset(node, offset);
	} else {
		sparc_attr_t *attr = get_sparc_attr(node);
		attr->immediate_value += offset;

		/* must be a FrameAddr or a load/store node with frame_entity */
		assert(is_sparc_FrameAddr(node) ||
				get_sparc_load_store_attr_const(node)->is_frame_entity);
	}
}

static int sparc_get_sp_bias(const ir_node *node)
{
	if (be_is_IncSP(node))
		return be_get_IncSP_offset(node);
	if (is_sparc_Save(node)) {
		const sparc_attr_t *attr = get_sparc_attr_const(node);
		if (get_irn_arity(node) == 3)
			panic("no support for _reg variant yet");

		return -attr->immediate_value;
	}
	if (is_sparc_RestoreZero(node))
		return SP_BIAS_RESET;
	return 0;
}

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

ir_entity *sparc_get_frame_entity(const ir_node *node)
{
	if (be_is_MemPerm(node))
		return be_get_MemPerm_in_entity(node, 0);
	if (is_sparc_FrameAddr(node)) {
		const sparc_attr_t *attr = get_sparc_attr_const(node);
		return attr->immediate_value_entity;
	}

	if (sparc_has_load_store_attr(node)) {
		const sparc_load_store_attr_t *load_store_attr
			= get_sparc_load_store_attr_const(node);
		if (load_store_attr->is_frame_entity) {
			return load_store_attr->base.immediate_value_entity;
		}
	}

	return NULL;
}

static void process_bias(ir_node *block, bool sp_relative, int bias,
                         int free_bytes)
{
	mark_Block_block_visited(block);

	/* process schedule */
	sched_foreach(block, irn) {
		/* set bias to nodes with entities */
		ir_entity *entity = sparc_get_frame_entity(irn);
		if (entity != NULL) {
			int offset = get_entity_offset(entity);
			if (sp_relative)
				offset += bias + SPARC_MIN_STACKSIZE;
			sparc_set_frame_offset(irn, offset);
		}

		/* The additional alignment bytes cannot be used
		 * anymore after alloca. */
		if (is_sparc_SubSP(irn)) {
			free_bytes = 0;
		} else if (is_sparc_AddSP(irn)) {
			assert(free_bytes == 0);
		}

		int irn_bias = sparc_get_sp_bias(irn);
		if (irn_bias == 0) {
			/* do nothing */
		} else if (irn_bias == SP_BIAS_RESET) {
			bias = 0;
		} else {
			/* adjust values to respect stack alignment */
			irn_bias -= free_bytes;

			int new_bias_unaligned = bias + irn_bias;
			int new_bias_aligned
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
	for (size_t i = 0, n_members = get_compound_n_members(type);
	     i < n_members; ++i) {
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
	if (!is_method_variadic(mtp))
		return false;

	if (cconv->n_param_regs >= SPARC_N_PARAM_REGS)
		return false;

	size_t   const n_params     = get_method_n_params(mtp);
	size_t   const n_ress       = get_method_n_ress(mtp);
	size_t   const new_n_params = n_params + (SPARC_N_PARAM_REGS - cconv->n_param_regs);
	ir_type *const new_mtp      = new_type_method(new_n_params, n_ress);

	type_dbg_info *const dbgi = get_type_dbg_info(mtp);
	set_type_dbg_info(new_mtp, dbgi);

	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *type = get_method_res_type(mtp, i);
		set_method_res_type(new_mtp, i, type);
	}
	size_t i;
	for (i = 0; i < n_params; ++i) {
		ir_type *type = get_method_param_type(mtp, i);
		set_method_param_type(new_mtp, i, type);
	}
	ir_type *const frame_type  = get_irg_frame_type(irg);
	ir_mode *const gp_reg_mode = sparc_reg_classes[CLASS_sparc_gp].mode;
	ir_type *const gp_reg_type = get_type_for_mode(gp_reg_mode);
	for (; i < new_n_params; ++i) {
		set_method_param_type(new_mtp, i, gp_reg_type);
		new_parameter_entity(frame_type, i, gp_reg_type);
	}

	copy_method_properties(new_mtp, mtp);
	set_higher_type(new_mtp, mtp);

	set_entity_type(entity, new_mtp);
	return true;
}

static ir_type *compute_arg_type(ir_graph *irg, calling_convention_t *cconv,
                                 ir_type *between_type)
{
	const ir_entity *entity          = get_irg_entity(irg);
	size_t           n_params        = cconv->n_parameters;
	ir_entity      **param_map       = ALLOCANZ(ir_entity*, n_params);

	ir_type *frame_type      = get_irg_frame_type(irg);
	size_t   n_frame_members = get_compound_n_members(frame_type);

	ir_type *const res = new_type_struct(new_id_fmt("%s_arg_type", get_entity_ident(entity)));

	/* search for existing value_param entities */
	for (size_t f = n_frame_members; f-- > 0; ) {
		ir_entity *member = get_compound_member(frame_type, f);
		if (!is_parameter_entity(member))
			continue;

		size_t num = get_entity_parameter_number(member);
		assert(num < n_params);
		if (param_map[num] != NULL)
			panic("multiple entities for parameter %u in %+F found", f, irg);

		param_map[num] = member;
		/* move to new arg_type */
		set_entity_owner(member, res);
	}

	/* calculate offsets/create missing entities */
	for (size_t i = 0; i < n_params; ++i) {
		reg_or_stackslot_t *param  = &cconv->parameters[i];
		ir_entity          *entity = param_map[i];

		if (param->reg0 != NULL) {
			/* use reserved spill space on between type */
			if (entity != NULL) {
				set_entity_owner(entity, between_type);
				set_entity_offset(entity, param->offset);
			}
			continue;
		}

		if (entity == NULL)
			entity = new_parameter_entity(res, i, param->type);
		param->entity = entity;
		set_entity_offset(entity, param->offset);
	}

	ir_type *const mtp = get_entity_type(entity);
	if (is_method_variadic(mtp)) {
		ir_type *unknown = get_unknown_type();
		va_start_entity = new_parameter_entity(res, IR_VA_START_PARAMETER_NUMBER, unknown);

		/* sparc_variadic_fixups() fiddled with our type, find out the
		 * original number of parameters */
		ir_type       *const non_lowered   = get_higher_type(mtp);
		size_t         const orig_n_params = get_method_n_params(non_lowered);
		assert(is_method_variadic(mtp));
		long offset;
		if (orig_n_params < n_params) {
			assert(param_map[orig_n_params] != NULL);
			offset = get_entity_offset(param_map[orig_n_params]);
			set_entity_owner(va_start_entity, between_type);
			set_entity_offset(va_start_entity, offset);
		} else {
			set_entity_owner(va_start_entity, res);
			set_entity_offset(va_start_entity, cconv->param_stack_size);
		}
	} else {
		va_start_entity = NULL;
	}

	set_type_size(res, cconv->param_stack_size);

	return res;
}

ir_entity *sparc_get_va_start_entity(void)
{
	return va_start_entity;
}

void sparc_create_stacklayout(ir_graph *irg, calling_convention_t *cconv)
{
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
	memset(layout, 0, sizeof(*layout));

	ir_type *between_type = new_type_class(new_id_from_str("sparc_between_type"));
	if (cconv->omit_fp) {
		set_type_size(between_type, 0);
	} else {
		set_type_size(between_type, SPARC_MIN_STACKSIZE);
	}

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
void sparc_adjust_stack_entity_offsets(ir_graph *irg)
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
	 *                   |      16 words save area     |
	 *  stack pointer -> |-----------------------------|
	 *                   |    high end of stackframe   |
	 *                   |            ...              |
	 *                   |    low end of stackframe    |
	 *      low address  |-----------------------------|
	 */
	ir_type *between_type = layout->between_type;
	unsigned between_size = get_type_size(between_type);

	ir_type *frame_type  = get_irg_frame_type(irg);
	unsigned frame_size  = get_type_size(frame_type);
	unsigned frame_align = get_type_alignment(frame_type);

	/* There's the tricky case of the stackframe size not being a multiple
	 * of the alignment. There are 2 variants:
	 *
	 * - frame-pointer relative addressing:
	 *   Increase frame_size in case it is not a multiple of the alignment as we
	 *   address entities from the "top" with negative offsets
	 * - stack-pointer relative addressing:
	 *   Stackframesize + SPARC_MIN_STACK_SIZE has to be aligned. Increase
	 *   frame_size accordingly.
	 */
	if (!layout->sp_relative) {
		frame_size = round_up2(frame_size, frame_align);
	} else {
		unsigned misalign = (SPARC_MIN_STACKSIZE+frame_size) % frame_align;
		frame_size += misalign;
	}
	set_type_size(frame_type, frame_size);

	ir_type *arg_type = layout->arg_type;

	adjust_entity_offsets(frame_type, -(long)frame_size);
	/* no need to adjust between type, it's already at 0 */
	adjust_entity_offsets(arg_type, between_size);
}

void sparc_fix_stack_bias(ir_graph *irg)
{
	bool sp_relative = be_get_irg_stack_layout(irg)->sp_relative;

	ir_node *start_block = get_irg_start_block(irg);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	process_bias(start_block, sp_relative, 0, 0);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}
