/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Removal of unreachable methods.
 * @author   Matthias Braun
 */
#include "debug.h"
#include "entity_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "panic.h"
#include "type_t.h"
#include "typerep.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static void visit_entity(ir_entity *entity);

static void visit_node(ir_node *node, void *env)
{
	(void)env;
	ir_entity *entity = get_irn_entity_attr(node);
	if (entity != NULL)
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
		for (size_t i = 0; i < initializer->compound.n_initializers; ++i) {
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
	if (entity_visited(entity))
		return;
	mark_entity_visited(entity);

	switch (get_entity_kind(entity)) {
	case IR_ENTITY_NORMAL: {
		ir_initializer_t *const init = get_entity_initializer(entity);
		if (init)
			visit_initializer(init);
		break;
	}

	case IR_ENTITY_METHOD: {
		ir_graph *irg = get_entity_irg(entity);
		if (irg != NULL)
			start_visit_node(get_irg_end(irg));
		break;
	}

	case IR_ENTITY_ALIAS: {
		ir_entity *aliased = get_entity_alias(entity);
		visit_entity(aliased);
		break;
	}

	default:
		break;
	}
}

static void visit_segment(ir_type *segment)
{
	for (int i = 0, n = get_compound_n_members(segment); i < n; ++i) {
		ir_entity *entity = get_compound_member(segment, i);
		if (!entity_is_externally_visible(entity)
		 && !(get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN))
			continue;

		visit_entity(entity);
	}
}

static void garbage_collect_in_segment(ir_type *segment)
{
	for (int i = get_compound_n_members(segment); i-- > 0; ) {
		ir_entity *entity = get_compound_member(segment, i);
		if (entity_visited(entity))
			continue;

		DB((dbg, LEVEL_1, "  removing entity %+F\n", entity));

		free_entity(entity);
	}
}

void garbage_collect_entities(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.garbagecollect");

	/* start a type walk for all externally visible entities */
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	inc_max_irg_visited();

	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *type = get_segment_type(s);
		mark_type_visited(type);

		visit_segment(type);
	}

	/* remove graphs of non-visited functions
	 * (we have to count backwards, because freeing the graph moves the last
	 *  graph in the list to the free position) */
	foreach_irp_irg_r(i, irg) {
		ir_entity *entity = get_irg_entity(irg);

		if (entity_visited(entity))
			continue;

		DB((dbg, LEVEL_1, "  freeing method %+F\n", entity));
		free_ir_graph(irg);
	}

	/* we can now remove all non-visited (global) entities */
	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *type = get_segment_type(s);
		garbage_collect_in_segment(type);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}
