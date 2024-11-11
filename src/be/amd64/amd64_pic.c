/*
 * This file is part of libFirm.
 * Copyright (C) 2015 Matthias Braun
 */

/**
 * @file
 * @brief       position independent code adjustments
 * @author      Matthias Braun
 */
#include "amd64_bearch_t.h"
#include "amd64_new_nodes.h"
#include "beutil.h"
#include "entity_t.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "panic.h"
#include "platform_t.h"
#include "x86_node.h"

static bool is_externally_visible(ir_entity const *const entity)
{
	ir_visibility const vis = get_entity_visibility(entity);
	switch (vis) {
	case ir_visibility_private:
	case ir_visibility_local:
		return false;
	case ir_visibility_external:
	case ir_visibility_external_private:
	case ir_visibility_external_protected:
		return true;
	}
	panic("invalid visibility in %+F", entity);
}

static ir_node *create_gotpcrel_load(dbg_info *const dbgi, ir_graph *const irg, ir_entity *const entity)
{
	ir_node *const addr
		= be_new_Relocation(dbgi, irg, X86_IMM_GOTPCREL, entity, mode_P);
	ir_type *const type  = get_entity_type(entity);
	ir_node *const nomem = get_irg_no_mem(irg);
	ir_node *const block = get_irg_start_block(irg);
	ir_node *const load  = new_rd_Load(dbgi, block, nomem, addr, mode_P,
	                                   type, cons_floats);
	return new_r_Proj(load, mode_P, pn_Load_res);
}

static void fix_address_pic_mach_o(ir_node *const node, void *const data)
{
	(void)data;
	foreach_irn_in(node, i, pred) {
		if (!is_Address(pred))
			continue;
		ir_entity *const entity = get_Address_entity(pred);
		if (is_tls_entity(entity))
			continue;

		ir_node        *res;
		dbg_info *const dbgi = get_irn_dbg_info(pred);
		ir_graph *const irg  = get_irn_irg(node);
		if (i == n_Call_ptr && is_Call(node)) {
			// Somehow we can always call PC relative. Are there trampolines
			// involved?
			res = be_new_Relocation(dbgi, irg, X86_IMM_PCREL, entity, mode_P);
		} else if (entity_has_definition(entity)
		        && !(get_entity_linkage(entity) & IR_LINKAGE_MERGE)) {
			res = be_new_Relocation(dbgi, irg, X86_IMM_PCREL, entity, mode_P);
		} else {
			res = create_gotpcrel_load(dbgi, irg, entity);
		}
		set_irn_n(node, i, res);
	}
}

static void fix_address_pic_elf(ir_node *const node, void *const data)
{
	(void)data;
	foreach_irn_in(node, i, pred) {
		if (!is_Address(pred))
			continue;
		ir_entity *const entity = get_Address_entity(pred);
		if (is_tls_entity(entity))
			continue;

		dbg_info *const dbgi        = get_irn_dbg_info(pred);
		ir_graph *const irg         = get_irn_irg(node);
		bool      const ext_visible = is_externally_visible(entity);
		ir_node  *      res;
		if (i == n_Call_ptr && is_Call(node)) {
			/* We can compilation-unit local functions directly, everything else
			 * goes through the PLT */
			x86_immediate_kind_t const reloc
				= ext_visible ? X86_IMM_PLT : X86_IMM_PCREL;
			res = be_new_Relocation(dbgi, irg, reloc, entity, mode_P);
		} else if (!ext_visible) {
			res = be_new_Relocation(dbgi, irg, X86_IMM_PCREL, entity, mode_P);
		} else {
			res = create_gotpcrel_load(dbgi, irg, entity);
		}
		set_irn_n(node, i, res);
	}
}

void amd64_adjust_pic(ir_graph *irg)
{
	switch (ir_platform.pic_style) {
	case BE_PIC_NONE:
		return;
	case BE_PIC_ELF_PLT:
		irg_walk_graph(irg, fix_address_pic_elf, NULL, NULL);
		break;
	case BE_PIC_ELF_NO_PLT:
		panic("amd64 elf/no-plt not implemented yet");
	case BE_PIC_MACH_O:
		irg_walk_graph(irg, fix_address_pic_mach_o, NULL, NULL);
		break;
	}
	be_dump(DUMP_BE, irg, "pic");
}
