/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 */
#include "x86_cconv.h"

#include "betranshlp.h"
#include "bevarargs.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irnode_t.h"
#include <stdlib.h>

void x86_free_calling_convention(x86_cconv_t *cconv)
{
	free(cconv->parameters);
	free(cconv->results);
	free(cconv->caller_saves);
	free(cconv->callee_saves);
	free(cconv);
}

void x86_create_parameter_loads(ir_graph *irg, const x86_cconv_t *cconv)
{
	ir_node *start       = get_irg_start(irg);
	ir_node *start_block = get_irg_start_block(irg);
	ir_node *nomem       = get_irg_no_mem(irg);
	ir_node *frame       = get_irg_frame(irg);
	ir_node *proj_args   = get_Proj_for_pn(start, pn_Start_T_args);

	ir_node *start_mem   = be_get_Start_mem(irg);
	ir_node *mem_dummy   = new_r_Dummy(irg, mode_M);
	edges_reroute_except(start_mem, mem_dummy, get_irg_anchor(irg));
	ir_node **syncs       = NEW_ARR_F(ir_node*, 1);
	syncs[0] = start_mem;

	foreach_out_edge_safe(proj_args, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;
		unsigned pn   = get_Proj_num(proj);
		const reg_or_stackslot_t *param = &cconv->parameters[pn];
		ir_entity *entity = param->entity;
		if (entity == NULL)
			continue;
		ir_type  *const type   = get_entity_type(entity);
		ir_mode  *const mode   = get_type_mode(type);
		dbg_info *const dbgi   = get_irn_dbg_info(proj);
		unsigned        offset = param->offset + 4;
		ir_node  *const c      = new_r_Const_long(irg, mode_Is, offset);
		ir_node  *const add    = new_rd_Add(dbgi, start_block, frame, c);
		ir_node  *const load   = new_rd_Load(dbgi, start_block, nomem, add, mode, type, cons_none);
		ir_node  *const res    = new_r_Proj(load, mode, pn_Load_res);
		ir_node  *const mem    = new_r_Proj(load, mode_M, pn_Load_M);
		ARR_APP1(ir_node *, syncs, mem);
		exchange(proj, res);
	}
	ir_node *sync = new_r_Sync(start_block, ARR_LEN(syncs), syncs);
	edges_reroute(mem_dummy, sync);
	DEL_ARR_F(syncs);
}

void x86_layout_param_entities(ir_graph *const irg, x86_cconv_t *const cconv,
                               int const parameters_offset)
{
	ir_type *const frame_type = get_irg_frame_type(irg);
	/** Return address is on the stack after that comes the parameters */

	/* Create missing entities and set offsets */
	ir_entity **const param_map = be_collect_parameter_entities(irg);
	size_t      const n_params  = cconv->n_parameters;
	for (size_t p = 0; p < n_params; ++p) {
		reg_or_stackslot_t *param = &cconv->parameters[p];
		if (param->type == NULL)
			continue;

		ir_entity *entity = param_map[p];
		if (entity == NULL)
			entity = new_parameter_entity(frame_type, p, param->type);
		param->entity = entity;
		/* Adjust for return address on stack */
		int const offset = param->offset + parameters_offset;
		set_entity_offset(param->entity, offset);
	}
	free(param_map);

	ir_entity *const entity        = get_irg_entity(irg);
	ir_type   *const function_type = get_entity_type(entity);
	if (is_method_variadic(function_type)) {
		int const offset = cconv->param_stacksize + parameters_offset;
		cconv->va_start_addr = be_make_va_start_entity(frame_type, offset);
	}
}
