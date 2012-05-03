/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Removal of unreachable methods.
 * @author   Matthias Braun
 */
#include "config.h"

#include "iroptimize.h"
#include "typerep.h"
#include "type_t.h"
#include "entity_t.h"
#include "irprog_t.h"
#include "irprintf.h"
#include "irpass.h"
#include "irgwalk.h"
#include "error.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static void visit_entity(ir_entity *entity);

static void visit_node(ir_node *node, void *env)
{
	ir_entity *entity;
	(void) env;

	if (is_SymConst(node)) {
		if (!SYMCONST_HAS_ENT(get_SymConst_kind(node)))
			return;
		entity = get_SymConst_entity(node);
	} else if (is_Sel(node)) {
		entity = get_Sel_entity(node);
	} else {
		return;
	}

	visit_entity(entity);
}

static void start_visit_node(ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);

	if (get_irg_visited(irg) < get_max_irg_visited()) {
		set_irg_visited(irg, get_max_irg_visited());
	}
	irg_walk_2(node, visit_node, NULL, NULL);
}

static void visit_initializer(ir_initializer_t *initializer)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_CONST:
		start_visit_node(initializer->consti.value);
		return;
	case IR_INITIALIZER_TARVAL:
	case IR_INITIALIZER_NULL:
		return;

	case IR_INITIALIZER_COMPOUND: {
		size_t i;
		for (i = 0; i < initializer->compound.n_initializers; ++i) {
			ir_initializer_t *subinitializer
				= initializer->compound.initializers[i];
			visit_initializer(subinitializer);
		}
		return;
	}
	}
	panic("invalid initializer found");
}

static void visit_entity(ir_entity *entity)
{
	ir_graph *irg;

	if (entity_visited(entity))
		return;
	mark_entity_visited(entity);

	if (entity->initializer != NULL) {
		visit_initializer(entity->initializer);
	}  else if (entity_has_compound_ent_values(entity)) {
		size_t i;
		size_t n_members = get_compound_ent_n_values(entity);
		for (i = 0; i < n_members; ++i) {
			ir_node *node = get_compound_ent_value(entity, i);
			start_visit_node(node);
		}
	}

	irg = get_entity_irg(entity);
	if (irg != NULL) {
		start_visit_node(get_irg_end(irg));
	}
}

static void visit_segment(ir_type *segment)
{
	int n_entities = get_compound_n_members(segment);
	int i;

	for (i = 0; i < n_entities; ++i) {
		ir_entity *entity = get_compound_member(segment, i);
		if (get_entity_visibility(entity) != ir_visibility_external
				&& !(get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER))
			continue;

		visit_entity(entity);
	}
}

static void garbage_collect_in_segment(ir_type *segment)
{
	int i;

	for (i = get_compound_n_members(segment)-1; i >= 0; --i) {
		ir_entity *entity = get_compound_member(segment, i);

		if (entity_visited(entity))
			continue;

		DB((dbg, LEVEL_1, "  removing entity %+F\n", entity));

		free_entity(entity);
	}
}

void garbage_collect_entities(void)
{
	size_t       i;
	ir_segment_t s;

	FIRM_DBG_REGISTER(dbg, "firm.opt.garbagecollect");

	/* start a type walk for all externally visible entities */
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	inc_max_irg_visited();

	for (s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *type = get_segment_type(s);
		mark_type_visited(type);

		visit_segment(type);
	}

	/* remove graphs of non-visited functions
	 * (we have to count backwards so we can safely call remove_irp_irg
	 *  while iterating) */
	for (i = get_irp_n_irgs(); i > 0;) {
		ir_graph  *irg    = get_irp_irg(--i);
		ir_entity *entity = get_irg_entity(irg);

		if (entity_visited(entity))
			continue;

		DB((dbg, LEVEL_1, "  freeing method %+F\n", entity));
		remove_irp_irg(irg);
	}

	/* we can now remove all non-visited (global) entities */
	for (s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *type = get_segment_type(s);
		garbage_collect_in_segment(type);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

ir_prog_pass_t *garbage_collect_entities_pass(const char *name)
{
	return def_prog_pass(name ? name : "garbage_collect",
	                     garbage_collect_entities);
}
