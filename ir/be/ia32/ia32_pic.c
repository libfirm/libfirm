/*
 * This file is part of libFirm.
 * Copyright (C) 2013 University of Karlsruhe.
 */

/**
 * @file
 * @brief       position independent code adjustments
 * @author      Matthias Braun
 */
#include "bearch_ia32_t.h"

#include "adt/pmap.h"
#include "bearch.h"
#include "begnuas.h"
#include "beirg.h"
#include "be_t.h"
#include "entity_t.h"
#include "ident_t.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "irnode_t.h"

/**
 * Create a trampoline entity for the given method.
 */
static ir_entity *create_trampoline(be_main_env_t *be, ir_entity *method)
{
	ir_type   *type   = get_entity_type(method);
	ident     *old_id = get_entity_ld_ident(method);
	ident     *id     = id_mangle3("", old_id, "$stub");
	ir_type   *parent = be->pic_trampolines_type;
	ir_entity *ent    = new_entity(parent, old_id, type);
	set_entity_ld_ident(ent, id);
	set_entity_visibility(ent, ir_visibility_private);

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

static ir_entity *create_pic_symbol(be_main_env_t *be, ir_entity *entity)
{
	ident     *old_id = get_entity_ld_ident(entity);
	ident     *id     = id_mangle3("", old_id, "$non_lazy_ptr");
	ir_type   *e_type = get_entity_type(entity);
	ir_type   *type   = new_type_pointer(e_type);
	ir_type   *parent = be->pic_symbols_type;
	ir_entity *ent    = new_entity(parent, old_id, type);
	set_entity_ld_ident(ent, id);
	set_entity_visibility(ent, ir_visibility_private);

	return ent;
}

static ir_entity *get_pic_symbol(be_main_env_t *env, ir_entity *entity)
{
	ir_entity *result = pmap_get(ir_entity, env->ent_pic_symbol_map, entity);
	if (result == NULL) {
		result = create_pic_symbol(env, entity);
		pmap_insert(env->ent_pic_symbol_map, entity, result);
	}

	return result;
}

/**
 * Returns non-zero if a given entity can be accessed using a relative address.
 */
static int can_address_relative(ir_entity *entity)
{
	return entity_has_definition(entity) && !(get_entity_linkage(entity) & IR_LINKAGE_MERGE);
}

/** patches Addresses to work in position independent code */
static void fix_pic_addresses(ir_node *node, void *data)
{
	(void) data;
	ir_graph      *irg = get_irn_irg(node);
	be_main_env_t *be  = be_get_irg_main_env(irg);

	foreach_irn_in(node, i, pred) {
		if (!is_Address(pred))
			continue;

		/* calls can jump to relative addresses, so we can directly jump to
		   the (relatively) known call address or the trampoline */
		ir_entity *const entity = get_Address_entity(pred);
		ir_node   *const block  = get_nodes_block(pred);
		dbg_info  *const dbgi   = get_irn_dbg_info(pred);
		if (i == 1 && is_Call(node)) {
			if (can_address_relative(entity))
				continue;

			ir_entity *const trampoline       = get_trampoline(be, entity);
			ir_node   *const trampoline_const = new_rd_Address(dbgi, irg, trampoline);
			set_irn_n(node, i, trampoline_const);
			continue;
		} else if (get_entity_type(entity) == get_code_type()) {
			/* block labels can always be addressed directly */
			continue;
		}

		/* everything else is accessed relative to EIP */
		ir_mode *const mode     = get_irn_mode(pred);
		ir_node *const pic_base = ia32_get_pic_base(irg);

		/* all ok now for locally constructed stuff */
		if (can_address_relative(entity)) {
			ir_node *add = new_r_Add(block, pic_base, pred, mode);

			/* make sure the walker doesn't visit this add again */
			mark_irn_visited(add);
			set_irn_n(node, i, add);
			continue;
		}

		/* get entry from pic symbol segment */
		ir_entity *const pic_symbol  = get_pic_symbol(be, entity);
		ir_node   *const pic_address = new_rd_Address(dbgi, irg, pic_symbol);
		ir_node   *const add         = new_r_Add(block, pic_base, pic_address, mode);
		mark_irn_visited(add);

		/* we need an extra indirection for global data outside our current
		   module. The loads are always safe and can therefore float
		   and need no memory input */
		ir_node *const load
			= new_r_Load(block, get_irg_no_mem(irg), add, mode, cons_floats);
		ir_node *const load_res = new_r_Proj(load, mode, pn_Load_res);

		set_irn_n(node, i, load_res);
	}
}

void ia32_adjust_pic(ir_graph *irg)
{
	irg_walk_graph(irg, fix_pic_addresses, NULL, NULL);
}
