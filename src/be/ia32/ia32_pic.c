/*
 * This file is part of libFirm.
 * Copyright (C) 2013 University of Karlsruhe.
 */

/**
 * @file
 * @brief       position independent code adjustments
 * @author      Matthias Braun
 */
#include "adt/pmap.h"
#include "begnuas.h"
#include "beirg.h"
#include "beutil.h"
#include "entity_t.h"
#include "ia32_bearch_t.h"
#include "ia32_new_nodes.h"
#include "ident_t.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "platform_t.h"
#include "x86_node.h"

/**
 * Create a trampoline entity for the given method.
 */
static ir_entity *create_trampoline(be_main_env_t *be, ir_entity *method)
{
	ir_type   *type   = get_entity_type(method);
	ident     *old_id = get_entity_ld_ident(method);
	ident     *id     = new_id_fmt("%s$stub", old_id);
	ir_type   *parent = be->pic_trampolines_type;
	ir_entity *ent    = new_global_entity(parent, id, type,
	                                      ir_visibility_private,
	                                      IR_LINKAGE_DEFAULT);
	/* We misuse the ident field to point to the old entity */
	set_entity_ident(ent, old_id);
	return ent;
}

/**
 * Returns the trampoline entity for the given method.
 */
static ir_entity *get_trampoline(be_main_env_t *env, ir_entity *method)
{
	ir_entity *result = pmap_get(ir_entity, env->ent_trampoline_map, method);
	if (result == NULL) {
		result = create_trampoline(env, method);
		pmap_insert(env->ent_trampoline_map, method, result);
	}

	return result;
}

static ir_entity *create_nonlazyptr(be_main_env_t *be, ir_entity *entity)
{
	ident     *old_id = get_entity_ld_ident(entity);
	ident     *id     = new_id_fmt("%s$non_lazy_ptr", old_id);
	ir_type   *e_type = get_entity_type(entity);
	ir_type   *type   = new_type_pointer(e_type);
	ir_type   *parent = be->pic_symbols_type;
	ir_entity *ent    = new_global_entity(parent, id, type,
	                                      ir_visibility_private,
	                                      IR_LINKAGE_DEFAULT);
	set_entity_ident(ent, old_id);

	return ent;
}

static ir_entity *get_nonlazyptr(be_main_env_t *env, ir_entity *entity)
{
	ir_entity *result = pmap_get(ir_entity, env->ent_pic_symbol_map, entity);
	if (result == NULL) {
		result = create_nonlazyptr(env, entity);
		pmap_insert(env->ent_pic_symbol_map, entity, result);
	}
	return result;
}

static ir_node *get_eip_relative(dbg_info *const dbgi, ir_graph *const irg, x86_immediate_kind_t const kind, ir_entity *const entity)
{
	/* Everything else is accessed relative to EIP. */
	ir_node *const pic_base = ia32_get_pic_base(irg);
	/* cheat a bit and set pic_base node to mode_P for now */
	set_irn_mode(pic_base, mode_P);
	ir_node *const block    = get_irg_start_block(irg);
	ir_mode *const offset_mode = get_reference_offset_mode(mode_P);
	ir_node *const reloc       = be_new_Relocation(dbgi, irg, (unsigned)kind, entity, offset_mode);
	/* All ok now for locally constructed stuff. */
	ir_node *const add         = new_rd_Add(dbgi, block, pic_base, reloc);
	/* Make sure the walker doesn't visit this add again. */
	mark_irn_visited(add);
	return add;
}

static ir_node *get_table_load(dbg_info *const dbgi, ir_graph *const irg, x86_immediate_kind_t const kind, ir_entity *const entity)
{
	ir_node *const addr  = get_eip_relative(dbgi, irg, kind, entity);
	ir_type *const type  = get_entity_type(entity);
	ir_node *const nomem = get_irg_no_mem(irg);
	ir_node *const block = get_irg_start_block(irg);
	ir_node *const load  = new_rd_Load(dbgi, block, nomem, addr, mode_P, type,
	                                   cons_floats);
	return new_r_Proj(load, mode_P, pn_Load_res);
}

static void fix_address_elf(ir_node *const node, void *const data)
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
			/* Calls can jump to relative addresses, so we can directly jump to
			 * the (relatively) known call address or the trampoline */
			if (entity_has_definition(entity)
			 && !(get_entity_linkage(entity) & IR_LINKAGE_MERGE))
				continue;

			if (ir_platform.pic_style == BE_PIC_ELF_PLT) {
				res = be_new_Relocation(dbgi, irg, X86_IMM_PLT, entity, mode_P);
			} else {
				assert(ir_platform.pic_style == BE_PIC_ELF_NO_PLT);
				res = get_table_load(dbgi, irg, X86_IMM_GOT, entity);
			}
		} else {
			ir_visibility const visibility = get_entity_visibility(entity);
			if (visibility != ir_visibility_external
			 && visibility != ir_visibility_external_private) {
				assert(visibility == ir_visibility_local
				    || visibility == ir_visibility_private);
				res = get_eip_relative(dbgi, irg, X86_IMM_GOTOFF, entity);
			} else {
				res = get_table_load(dbgi, irg, X86_IMM_GOT, entity);
			}
		}
		set_irn_n(node, i, res);
	}
}

/** patches Addresses to work in position independent code */
static void fix_address_macho(ir_node *const node, void *const data)
{
	(void)data;

	foreach_irn_in(node, i, pred) {
		if (!is_Address(pred))
			continue;
		ir_entity *const entity = get_Address_entity(pred);
		if (is_tls_entity(entity))
			continue;

		ir_node             *res;
		dbg_info      *const dbgi = get_irn_dbg_info(pred);
		ir_graph      *const irg  = get_irn_irg(node);
		be_main_env_t *const be   = be_get_irg_main_env(irg);
		if (i == n_Call_ptr && is_Call(node)) {
			/* Calls can jump to relative addresses, so we can directly jump to
			 * the (relatively) known call address or the trampoline */
			if (entity_has_definition(entity)
			 && !(get_entity_linkage(entity) & IR_LINKAGE_MERGE))
				continue;

			ir_entity *const trampoline = get_trampoline(be, entity);
			res = be_new_Relocation(dbgi, irg, X86_IMM_ADDR, trampoline, mode_P);
		} else {
			/* Everything else is accessed relative to EIP. */
			if (entity_has_definition(entity)
			 && !(get_entity_linkage(entity) & IR_LINKAGE_MERGE)) {
				res = get_eip_relative(dbgi, irg, X86_IMM_PICBASE_REL, entity);
			} else {
				ir_entity *const nonlazyptr = get_nonlazyptr(be, entity);
				res = get_table_load(dbgi, irg, X86_IMM_PICBASE_REL, nonlazyptr);
			}
		}
		set_irn_n(node, i, res);
	}
}

void ia32_adjust_pic(ir_graph *irg)
{
	switch (ir_platform.pic_style) {
	case BE_PIC_NONE:
		return;
	case BE_PIC_ELF_PLT:
	case BE_PIC_ELF_NO_PLT:
		irg_walk_graph(irg, fix_address_elf, NULL, NULL);
		return;
	case BE_PIC_MACH_O:
		irg_walk_graph(irg, fix_address_macho, NULL, NULL);
		return;
	}
}
