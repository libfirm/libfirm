/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Manage addressing into the stackframe
 * @author  Matthias Braun
 *
 * Logical sparc stacklayout (omitfp==false), situation after a call:
 *
 *        high address |-----------------------------|
 *                     |            ...              |
 *                     |         stackarg 1          |
 *                     |         stackarg 0          |
 * SPARC_MIN_STACKSIZE |-----------------------------|  entity offset 0
 *                     | space for storing regarg0-5 |
 *                     | pointer to aggregate return |
 *                     |      16 words save area     |
 *    stack pointer -> |-----------------------------|
 *                     |    high end of stackframe   |
 *                     |            ...              |
 *                     |    low end of stackframe    |
 *        low address  |-----------------------------|
 */
#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "bestack.h"
#include "betranshlp.h"
#include "bevarargs.h"
#include "bitfiddle.h"
#include "firm_types.h"
#include "iredges_t.h"
#include "irnode_t.h"
#include "panic.h"
#include "sparc_bearch_t.h"
#include "sparc_cconv.h"
#include "sparc_new_nodes.h"

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

static bool node_has_sp_base(ir_node const *const node)
{
	int input;
	if (is_sparc_FrameAddr(node)) {
		input = n_sparc_FrameAddr_base;
	} else if (is_sparc_Ld(node)) {
		input = n_sparc_Ld_ptr;
	} else if (is_sparc_St(node)) {
		input = n_sparc_St_ptr;
	} else if (is_sparc_Ldf(node)) {
		input = n_sparc_Ldf_ptr;
	} else if (is_sparc_Stf(node)) {
		input = n_sparc_Stf_ptr;
	} else {
		panic("Unexpected node %+F", node);
	}
	arch_register_t const *const reg = arch_get_irn_register_in(node, input);
	return reg == &sparc_registers[REG_SP];
}

static void sparc_determine_frameoffset(ir_node *const node,
                                        int const sp_offset)
{
	if (is_sparc_FrameAddr(node)) {
		sparc_attr_t    *const attr   = get_sparc_attr(node);
		ir_entity const *const entity = attr->immediate_value_entity;
		if (entity != NULL) {
			attr->immediate_value += get_entity_offset(entity);
			if (node_has_sp_base(node))
				attr->immediate_value += sp_offset + SPARC_MIN_STACKSIZE;
		}
	} else if (sparc_has_load_store_attr(node)) {
		sparc_load_store_attr_t *const attr = get_sparc_load_store_attr(node);
		if (!attr->is_frame_entity)
			return;
		ir_entity const *const entity
			= attr->base.immediate_value_entity;
		if (entity != NULL) {
			attr->base.immediate_value += get_entity_offset(entity);
			if (node_has_sp_base(node))
				attr->base.immediate_value += sp_offset + SPARC_MIN_STACKSIZE;
		}
	} else if (be_is_MemPerm(node)) {
		ir_graph *irg = get_irn_irg(node);
		if (sparc_get_irg_data(irg)->omit_fp)
			be_set_MemPerm_offset(node, sp_offset + SPARC_MIN_STACKSIZE);
	}
}

static void sparc_sp_sim(ir_node *const node, stack_pointer_state_t *state)
{
	sparc_determine_frameoffset(node, state->offset);

	if (is_sparc_Save(node)) {
		sparc_attr_t *const attr = get_sparc_attr(node);
		if (get_irn_arity(node) == 3)
			panic("no support for _reg variant yet");

		/* Adjust for alignment */
		assert(state->misalign == 0);
		int const prev_offset = state->offset;
		int const new_offset  = prev_offset - state->align_padding
		                        - attr->immediate_value;
		int const aligned     = round_up2(new_offset, 1u << state->p2align);
		attr->immediate_value = -(aligned - prev_offset);
		state->align_padding  = aligned - new_offset;
		state->offset         = aligned;
	} else if (is_sparc_SubSP(node) || is_sparc_AddSP(node)) {
		state->align_padding = 0;
	} else if (is_sparc_RestoreZero(node)) {
		state->offset        = 0;
		state->align_padding = 0;
	}
}

void sparc_fix_stack_bias(ir_graph *irg)
{
	unsigned const misalign = 0;
	be_sim_stack_pointer(irg, misalign, SPARC_PO2_STACK_ALIGNMENT,
	                     sparc_sp_sim);
}

/**
 * Perform some fixups for variadic functions.
 * To make the rest of the frontend code easier to understand we add
 * "dummy" parameters until the number of parameters transmitted in registers.
 * (because otherwise the backend wouldn't store the value of the register
 *  parameters into memory for the VLA magic)
 */
static bool sparc_variadic_fixups(ir_graph *const irg, calling_convention_t *const cconv)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *mtp    = get_entity_type(entity);
	if (!is_method_variadic(mtp))
		return false;

	if (cconv->n_param_regs >= SPARC_N_PARAM_REGS)
		return false;

	size_t                    const n_params     = get_method_n_params(mtp);
	size_t                    const n_ress       = get_method_n_ress(mtp);
	size_t                    const new_n_params = n_params + (SPARC_N_PARAM_REGS - cconv->n_param_regs);
	unsigned                  const cc_mask      = get_method_calling_convention(mtp);
	mtp_additional_properties const props        = get_method_additional_properties(mtp);
	ir_type                  *const new_mtp      = new_type_method(new_n_params, n_ress, true, cc_mask, props);

	type_dbg_info *const dbgi = get_type_dbg_info(mtp);
	set_type_dbg_info(new_mtp, dbgi);

	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *type = get_method_res_type(mtp, i);
		set_method_res_type(new_mtp, i, type);
	}
	for (size_t i = 0; i < n_params; ++i) {
		ir_type *type = get_method_param_type(mtp, i);
		set_method_param_type(new_mtp, i, type);
	}
	ir_type *const frame_type  = get_irg_frame_type(irg);
	ir_mode *const gp_reg_mode = sparc_reg_classes[CLASS_sparc_gp].mode;
	ir_type *const gp_reg_type = get_type_for_mode(gp_reg_mode);
	for (size_t i = n_params; i < new_n_params; ++i) {
		set_method_param_type(new_mtp, i, gp_reg_type);
		new_parameter_entity(frame_type, i, gp_reg_type);
	}

	set_entity_type(entity, new_mtp);
	return true;
}

static void sparc_layout_param_entities(ir_graph *const irg, calling_convention_t *const cconv, ir_type *const non_lowered)
{
	ir_entity **const param_map  = be_collect_parameter_entities(irg);
	ir_type    *const frame_type = get_irg_frame_type(irg);
	size_t      const n_params   = cconv->n_parameters;
	/* calculate offsets/create missing entities */
	for (size_t i = 0; i < n_params; ++i) {
		reg_or_stackslot_t *const param  = &cconv->parameters[i];
		ir_entity          *      entity = param_map[i];
		if (entity == NULL) {
			if (!param->already_stored)
				continue;
			entity = new_parameter_entity(frame_type, i, param->type);
		}
		param->entity = entity;
		set_entity_offset(entity, param->offset);
	}

	ir_entity *const function      = get_irg_entity(irg);
	ir_type   *const function_type = get_entity_type(function);
	if (is_method_variadic(function_type)) {
		/* sparc_variadic_fixups() fiddled with our type, find out the
		 * original number of parameters */
		size_t const orig_n_params = get_method_n_params(non_lowered);
		long offset;
		if (orig_n_params < n_params) {
			assert(param_map[orig_n_params] != NULL);
			offset = get_entity_offset(param_map[orig_n_params]);
		} else {
			offset = cconv->param_stack_size + SPARC_MIN_STACKSIZE;
		}

		cconv->va_start_addr = be_make_va_start_entity(frame_type, offset);
	}

	free(param_map);
}

calling_convention_t *sparc_prepare_calling_convention(ir_graph *const irg)
{
	ir_entity *const entity      = get_irg_entity(irg);
	ir_type   *const non_lowered = get_entity_type(entity);
	calling_convention_t *cconv = sparc_decide_calling_convention(get_entity_type(entity), irg);
	if (sparc_variadic_fixups(irg, cconv)) {
		sparc_free_calling_convention(cconv);
		cconv = sparc_decide_calling_convention(get_entity_type(entity), irg);
	}
	sparc_layout_param_entities(irg, cconv, non_lowered);
	return cconv;
}
