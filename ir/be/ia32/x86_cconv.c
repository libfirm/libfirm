/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 */
#include <stdlib.h>

#include "irnode_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "x86_cconv.h"

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
		ir_node  *const member = new_rd_Member(dbgi, start_block, frame, entity);
		ir_node  *const load   = new_rd_Load(dbgi, start_block, nomem, member, mode, type, cons_none);
		ir_node  *const res    = new_r_Proj(load, mode, pn_Load_res);
		exchange(proj, res);
	}
}
