/*
 * This file is part of libFirm.
 * Copyright (C) 2015 Matthias Braun
 */

/**
 * @file
 * @brief       position independent code adjustments
 * @author      Matthias Braun
 */
#include "bearch_amd64_t.h"

#include "be_t.h"
#include "beutil.h"
#include "entity_t.h"
#include "amd64_new_nodes.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "x86_imm.h"

static void fix_address_pic(ir_node *const node, void *const data)
{
	(void)data;
	foreach_irn_in(node, i, pred) {
		if (!is_Address(pred))
			continue;
		ir_entity *const entity = get_Address_entity(pred);
		if (is_tls_entity(entity))
			continue;

		ir_graph *const irg = get_irn_irg(node);
		ir_node  *      res;
		if (i == n_Call_ptr && is_Call(node)) {
			// Somehow we can always call PC relative. Are there trampolines
			// involved?
			res = be_new_Relocation(irg, X86_IMM_PCREL, entity, mode_P);
		} else if (entity_has_definition(entity)
		        && !(get_entity_linkage(entity) & IR_LINKAGE_MERGE)) {
			res = be_new_Relocation(irg, X86_IMM_PCREL, entity, mode_P);
		} else {
			ir_node *const addr
				= be_new_Relocation(irg, X86_IMM_GOTPCREL, entity, mode_P);
			ir_type *const type  = get_entity_type(entity);
			ir_node *const nomem = get_irg_no_mem(irg);
			ir_node *const block = get_irg_start_block(irg);
			ir_node *const load  = new_rd_Load(NULL, block, nomem, addr, mode_P,
			                                   type, cons_floats);
			res = new_r_Proj(load, mode_P, pn_Load_res);
		}
		set_irn_n(node, i, res);
	}
}

void amd64_adjust_pic(ir_graph *irg)
{
	if (!be_options.pic)
		return;
	irg_walk_graph(irg, fix_address_pic, NULL, NULL);
	be_dump(DUMP_BE, irg, "pic");
}
