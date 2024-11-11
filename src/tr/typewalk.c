/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Functionality to modify the type graph.
 * @author  Goetz Lindenmaier
 * @brief
 *
 * Traverse the type information.  The walker walks the whole ir graph
 * to find the distinct type trees in the type graph forest.
 * - execute the pre function before recursion
 * - execute the post function after recursion
 */
#include "entity_t.h"
#include "ircons.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "panic.h"
#include "type_t.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * The walker environment
 */
typedef struct {
	type_walk_func *pre;    /**< Pre-walker function */
	type_walk_func *post;   /**< Post-walker function */
	void           *env;    /**< environment for walker functions */
} type_walk_env;

/** a walker for irn's */
static void irn_type_walker(ir_node *node, type_walk_func *pre,
                            type_walk_func *post, void *env);

static void walk_initializer(ir_initializer_t *initializer,
                             type_walk_func *pre, type_walk_func *post,
                             void *env)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_CONST:
		irn_type_walker(initializer->consti.value, pre, post, env);
		return;
	case IR_INITIALIZER_TARVAL:
	case IR_INITIALIZER_NULL:
		return;

	case IR_INITIALIZER_COMPOUND: {
		for (size_t i = 0; i < initializer->compound.n_initializers; ++i) {
			ir_initializer_t *subinitializer
				= initializer->compound.initializers[i];
			walk_initializer(subinitializer, pre, post, env);
		}
		return;
	}
	}
	panic("invalid initializer found");
}

/**
 * Main walker: walks over all used types/entities of a
 * type entity.
 */
static void do_type_walk(ir_type *const tp, ir_entity *const ent,
                         type_walk_func *pre, type_walk_func *post, void *env)
{
	/* marked? */
	if (ent) {
		if (entity_visited(ent))
			return;
		mark_entity_visited(ent);
	} else {
		if (type_visited(tp))
			return;
		mark_type_visited(tp);
	}

	/* execute pre method */
	if (pre)
		pre(tp, ent, env);

	/* iterate */
	if (ent) {
		do_type_walk(get_entity_owner(ent), NULL, pre, post, env);
		do_type_walk(get_entity_type(ent),  NULL, pre, post, env);

		switch (get_entity_kind(ent)) {
		case IR_ENTITY_ALIAS: {
			ir_entity *const e = get_entity_alias(ent);
			if (e)
				do_type_walk(NULL, e, pre, post, env);
			break;
		}

		case IR_ENTITY_NORMAL: {
			/* walk over the value types */
			ir_initializer_t *const init = get_entity_initializer(ent);
			if (init)
				walk_initializer(init, pre, post, env);
			break;
		}

		case IR_ENTITY_METHOD:
		case IR_ENTITY_UNKNOWN:
		case IR_ENTITY_PARAMETER:
		case IR_ENTITY_LABEL:
		case IR_ENTITY_COMPOUND_MEMBER:
		case IR_ENTITY_SPILLSLOT:
			break;
		}
	} else {
		switch (get_type_opcode(tp)) {
		case tpo_class:
			for (size_t i = 0, n_types = get_class_n_supertypes(tp);
			     i < n_types; ++i) {
				do_type_walk(get_class_supertype(tp, i), NULL, pre, post, env);
			}
			for (size_t i = 0, n_mem = get_compound_n_members(tp);
			     i < n_mem; ++i) {
				do_type_walk(NULL, get_compound_member(tp, i), pre, post, env);
			}
			for (size_t i = 0, n_types = get_class_n_subtypes(tp);
			     i < n_types; ++i) {
				do_type_walk(get_class_subtype(tp, i), NULL, pre, post, env);
			}
			break;

		case tpo_struct:
		case tpo_union:
		case tpo_segment:
			for (size_t i = 0, n_mem = get_compound_n_members(tp);
			     i < n_mem; ++i) {
				do_type_walk(NULL, get_compound_member(tp, i), pre, post, env);
			}
			break;

		case tpo_method:
			for (size_t i = 0, n_params = get_method_n_params(tp);
			     i < n_params; ++i) {
				do_type_walk(get_method_param_type(tp, i), NULL, pre, post, env);
			}
			for (size_t i = 0, n_res = get_method_n_ress(tp); i < n_res; ++i) {
				do_type_walk(get_method_res_type(tp, i), NULL, pre, post, env);
			}
			break;

		case tpo_array:
			do_type_walk(get_array_element_type(tp), NULL, pre, post, env);
			break;

		case tpo_pointer:
			do_type_walk(get_pointer_points_to_type(tp), NULL, pre, post, env);
			break;

		case tpo_code:
		case tpo_primitive:
		case tpo_unknown:
			/* a leave. */
			break;
		case tpo_uninitialized:
			panic("faulty type");
		}
	}

	/* execute post method */
	if (post)
		post(tp, ent, env);
}

/**
 * Check whether node contains types or entities as an attribute.
 * If so start a walk over that information.
 */
static void irn_type_walker(ir_node *node, type_walk_func *pre,
                            type_walk_func *post, void *env)
{
	ir_entity *const ent = get_irn_entity_attr(node);
	if (ent)
		do_type_walk(NULL, ent, pre, post, env);
	ir_type *const typ = get_irn_type_attr(node);
	if (typ)
		do_type_walk(typ, NULL, pre, post, env);
}

/**
 * Check whether node contains types or entities as an attribute.
 * If so start a walk over that information.
 */
static void start_type_walk(ir_node *node, void *ctx)
{
	type_walk_env  *env  = (type_walk_env*)ctx;
	type_walk_func *pre  = env->pre;
	type_walk_func *post = env->post;
	void           *envi = env->env;
	irn_type_walker(node, pre, post, envi);
}

void type_walk(type_walk_func *pre, type_walk_func *post, void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		do_type_walk(get_irp_type(i), NULL, pre, post, env);
	}
	do_type_walk(get_glob_type(), NULL, pre, post, env);
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

void type_walk_irg(ir_graph *irg, type_walk_func *pre, type_walk_func *post,
                   void *env)
{
	/* this is needed to pass the parameters to the walker that actually
	   walks the type information */
	type_walk_env type_env;
	type_env.pre  = pre;
	type_env.post = post;
	type_env.env  = env;

	/* We walk over the irg to find all IR-nodes that contain an attribute
	 * with type information.  If we find one we call a type walker to
	 * touch the reachable type information.  The same type can be referenced
	 * by several IR-nodes.  To avoid repeated visits of the same type node we
	 * must decrease the type visited flag for each walk.  This is done in
	 * start_type_walk().  Here we initially increase the flag.  We only call
	 * do_type_walk that does not increase the flag.
	 */
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	irg_walk(get_irg_end(irg), start_type_walk, NULL, &type_env);

	do_type_walk(NULL, get_irg_entity(irg), pre, post, env);

	do_type_walk(get_irg_frame_type(irg), NULL, pre, post, env);
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

static void type_walk_s2s_2(ir_type *const tp, type_walk_func *pre,
                            type_walk_func *post, void *env)
{
	if (type_visited(tp))
		return;

	/* iterate */
	mark_type_visited(tp);
	if (is_Class_type(tp)) {
		for (size_t i = 0, n = get_class_n_supertypes(tp); i < n; ++i) {
			type_walk_s2s_2(get_class_supertype(tp, i), pre, post, env);
		}
		/* execute pre method */
		if (pre)
			pre(tp, NULL, env);

		for (size_t i = 0, n = get_class_n_subtypes(tp); i < n; ++i) {
			type_walk_s2s_2(get_class_subtype(tp, i), pre, post, env);
		}

		/* execute post method */
		if (post)
			post(tp, NULL, env);
	}
}

void type_walk_super2sub(type_walk_func *pre, type_walk_func *post, void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	type_walk_s2s_2(get_glob_type(), pre, post, env);
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		type_walk_s2s_2(get_irp_type(i), pre, post, env);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

static void type_walk_super_2(ir_type *const tp, type_walk_func *pre,
                              type_walk_func *post, void *env)
{
	if (type_visited(tp))
		return;

	/* iterate */
	mark_type_visited(tp);
	if (is_Class_type(tp)) {
		/* execute pre method */
		if (pre)
			pre(tp, NULL, env);

		for (size_t i = 0, n = get_class_n_supertypes(tp); i < n; ++i) {
			type_walk_super_2(get_class_supertype(tp, i), pre, post, env);
		}

		/* execute post method */
		if (post)
			post(tp, NULL, env);
	}
}

void type_walk_super(type_walk_func *pre, type_walk_func *post, void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	type_walk_super_2(get_glob_type(), pre, post, env);
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		type_walk_super_2(get_irp_type(i), pre, post, env);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

static void class_walk_s2s_2(ir_type *tp, class_walk_func *pre,
                             class_walk_func *post, void *env)
{
	/* marked? */
	if (type_visited(tp))
		return;

	/* Assure all supertypes are visited before */
	for (size_t i = 0, n = get_class_n_supertypes(tp); i < n; ++i) {
		if (!type_visited(get_class_supertype(tp, i)))
			return;
	}

	mark_type_visited(tp);

	/* execute pre method */
	if (pre)
		pre(tp, env);

	for (size_t i = 0, n = get_class_n_subtypes(tp); i < n; ++i) {
		class_walk_s2s_2(get_class_subtype(tp, i), pre, post, env);
	}
	/* execute post method */
	if (post)
		post(tp, env);
}

void class_walk_super2sub(class_walk_func *pre, class_walk_func *post,
                          void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; i++) {
		ir_type *tp = get_irp_type(i);
		if (is_Class_type(tp) &&
		    (get_class_n_supertypes(tp) == 0) &&
		    !type_visited(tp) &&
		    (! is_frame_type(tp)) &&
		    (tp != get_glob_type())) {
			class_walk_s2s_2(tp, pre, post, env);
		}
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

void walk_types_entities(ir_type *tp, entity_walk_func *doit, void *env)
{
	switch (get_type_opcode(tp)) {
	case tpo_class:
	case tpo_struct:
	case tpo_union:
	case tpo_segment:
		for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i)
			doit(get_compound_member(tp, i), env);
		return;
	case tpo_array:
	case tpo_code:
	case tpo_method:
	case tpo_pointer:
	case tpo_primitive:
	case tpo_uninitialized:
	case tpo_unknown:
		return;
	}
	panic("invalid type");
}
