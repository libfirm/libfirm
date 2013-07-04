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
#include <stdlib.h>
#include <stdio.h>

#include "entity_t.h"
#include "type_t.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "error.h"
#include "ircons.h"

/**
 * The walker environment
 */
typedef struct type_walk_env {
	type_walk_func *pre;    /**< Pre-walker function */
	type_walk_func *post;   /**< Post-walker function */
	void *env;              /**< environment for walker functions */
} type_walk_env;

/* a walker for irn's */
static void irn_type_walker(
	ir_node *node, type_walk_func *pre, type_walk_func *post, void *env);

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
static void do_type_walk(type_or_ent tore,
                         type_walk_func *pre,
                         type_walk_func *post,
                         void *env)
{
	const firm_kind  kind = get_kind(tore.ent);

	/* marked? */
	ir_entity *ent = NULL;
	ir_type   *tp  = NULL;
	switch (kind) {
	case k_entity:
		ent = tore.ent;
		if (entity_visited(ent))
			return;
		mark_entity_visited(ent);
		break;
	case k_type:
		tp = tore.typ;
		if (type_visited(tp))
			return;
		mark_type_visited(tp);
		break;
	default:
		break;
	}

	/* execute pre method */
	if (pre)
		pre(tore, env);

	/* iterate */
	type_or_ent cont;
	switch (kind) {
	case k_entity:
		cont.typ = get_entity_owner(ent);
		do_type_walk(cont, pre, post, env);
		cont.typ = get_entity_type(ent);
		do_type_walk(cont, pre, post, env);

		/* walk over the value types */
		if (ent->initializer != NULL) {
			walk_initializer(ent->initializer, pre, post, env);
		}
		break;
	case k_type:
		switch (get_type_tpop_code(tp)) {
		case tpo_class:
			for (size_t i = 0, n_types = get_class_n_supertypes(tp);
			     i < n_types; ++i) {
				cont.typ = get_class_supertype(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			for (size_t i = 0, n_mem = get_class_n_members(tp);
			     i < n_mem; ++i) {
				cont.ent = get_class_member(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			for (size_t i = 0, n_types = get_class_n_subtypes(tp);
			     i < n_types; ++i) {
				cont.typ = get_class_subtype(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			break;

		case tpo_struct:
			for (size_t i = 0, n_mem = get_struct_n_members(tp);
			     i < n_mem; ++i) {
				cont.ent = get_struct_member(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			break;

		case tpo_method:
			for (size_t i = 0, n_params = get_method_n_params(tp);
			     i < n_params; ++i) {
				cont.typ = get_method_param_type(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			for (size_t i = 0, n_res = get_method_n_ress(tp); i < n_res; ++i) {
				cont.typ = get_method_res_type(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			break;

		case tpo_union:
			for (size_t i = 0, n_members = get_union_n_members(tp);
			     i < n_members; ++i) {
				cont.ent = get_union_member(tp, i);
				do_type_walk(cont, pre, post, env);
			}
			break;

		case tpo_array:
			cont.typ = get_array_element_type(tp);
			do_type_walk(cont, pre, post, env);
			cont.ent = get_array_element_entity(tp);
			do_type_walk(cont, pre, post, env);
			break;

		case tpo_enumeration:
			/* a leave */
			break;

		case tpo_pointer:
			cont.typ = get_pointer_points_to_type(tp);
			do_type_walk(cont, pre, post, env);
			break;

		case tpo_code:
		case tpo_primitive:
		case tpo_unknown:
			/* a leave. */
			break;
		case tpo_uninitialized:
			panic("Faulty type");
		}
		break; /* end case k_type */

	default:
		printf(" *** Faulty type or entity! \n");
		break;
	}

	/* execute post method */
	if (post)
		post(tore, env);
}

/**  Check whether node contains types or entities as an attribute.
     If so start a walk over that information. */
static void irn_type_walker(ir_node *node, type_walk_func *pre,
                            type_walk_func *post, void *env)
{
	type_or_ent cont;
	cont.ent = get_irn_entity_attr(node);
	if (cont.ent)
		do_type_walk(cont, pre, post, env);
	cont.typ = get_irn_type_attr(node);
	if (cont.typ)
		do_type_walk(cont, pre, post, env);
}

/**  Check whether node contains types or entities as an attribute.
     If so start a walk over that information. */
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
	type_or_ent cont;
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		cont.typ = get_irp_type(i);
		do_type_walk(cont, pre, post, env);
	}
	cont.typ = get_glob_type();
	do_type_walk(cont, pre, post, env);
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

void type_walk_irg(ir_graph *irg,
                   type_walk_func *pre,
                   type_walk_func *post,
                   void *env)
{
	/* this is needed to pass the parameters to the walker that actually
	   walks the type information */
	type_walk_env type_env;
	type_env.pre  = pre;
	type_env.post = post;
	type_env.env  = env;

	ir_graph *rem = current_ir_graph;
	current_ir_graph = irg;

	/* We walk over the irg to find all IR-nodes that contain an attribute
	   with type information.  If we find one we call a type walker to
	   touch the reachable type information.
	   The same type can be referenced by several IR-nodes.  To avoid
	   repeated visits of the same type node we must decrease the
	   type visited flag for each walk.  This is done in start_type_walk().
	   Here we initially increase the flag.  We only call do_type_walk that does
	   not increase the flag.
	*/
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	irg_walk(get_irg_end(irg), start_type_walk, NULL, &type_env);

	type_or_ent cont;
	cont.ent = get_irg_entity(irg);
	do_type_walk(cont, pre, post, env);

	cont.typ = get_irg_frame_type(irg);
	do_type_walk(cont, pre, post, env);
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);

	current_ir_graph = rem;
}

static void type_walk_s2s_2(type_or_ent tore,
                            type_walk_func *pre,
                            type_walk_func *post,
                            void *env)
{
	/* marked? */
	switch (get_kind(tore.ent)) {
	case k_entity:
		if (entity_visited(tore.ent)) return;
		break;
	case k_type:
		if (type_visited(tore.typ)) return;
		break;
	default:
		break;
	}

	/* iterate */
	type_or_ent cont;
	switch (get_kind(tore.typ)) {
	case k_type:
		{
			ir_type *tp = tore.typ;
			mark_type_visited(tp);
			switch (get_type_tpop_code(tp)) {
			case tpo_class: {
				for (size_t i = 0, n = get_class_n_supertypes(tp); i < n; ++i) {
					cont.typ = get_class_supertype(tp, i);
					type_walk_s2s_2(cont, pre, post, env);
				}
				/* execute pre method */
				if (pre)
					pre(tore, env);

				for (size_t i = 0, n = get_class_n_subtypes(tp); i < n; ++i) {
					cont.typ = get_class_subtype(tp, i);
					type_walk_s2s_2(cont, pre, post, env);
				}

				/* execute post method */
				if (post)
					post(tore, env);
				break;
			}
			case tpo_struct:
			case tpo_method:
			case tpo_union:
			case tpo_array:
			case tpo_enumeration:
			case tpo_pointer:
			case tpo_primitive:
				/* dont care */
				break;
			default:
				printf(" *** Faulty type! \n");
				break;
			}
		} break; /* end case k_type */
	case k_entity:
		/* don't care */
		break;
	default:
		printf(" *** Faulty type or entity! \n");
		break;
	}
}

void type_walk_super2sub(type_walk_func *pre,
                         type_walk_func *post,
                         void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	type_or_ent cont;
	cont.typ = get_glob_type();
	type_walk_s2s_2(cont, pre, post, env);
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		cont.typ = get_irp_type(i);
		type_walk_s2s_2(cont, pre, post, env);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

/*****************************************************************************/

static void type_walk_super_2(type_or_ent tore, type_walk_func *pre,
                              type_walk_func *post, void *env)
{
	/* marked? */
	switch (get_kind(tore.ent)) {
	case k_entity:
		if (entity_visited(tore.ent))
			return;
		break;
	case k_type:
		if (type_visited(tore.typ))
			return;
		break;
	default:
		break;
	}

	/* iterate */
	type_or_ent cont;
	switch (get_kind(tore.typ)) {
	case k_type: {
		ir_type *tp = tore.typ;
		mark_type_visited(tp);
		switch (get_type_tpop_code(tp)) {
		case tpo_class:
			/* execute pre method */
			if (pre)
				pre(tore, env);

			for (size_t i = 0, n = get_class_n_supertypes(tp); i < n; ++i) {
				cont.typ = get_class_supertype(tp, i);
				type_walk_super_2(cont, pre, post, env);
			}

			/* execute post method */
			if (post)
				post(tore, env);
			break;
		case tpo_struct:
		case tpo_method:
		case tpo_union:
		case tpo_array:
		case tpo_enumeration:
		case tpo_pointer:
		case tpo_primitive:
			/* don't care */
			break;
		default:
			printf(" *** Faulty type! \n");
			break;
		}
		break;
	}
	case k_entity:
		/* don't care */
		break;
	default:
		printf(" *** Faulty type or entity! \n");
		break;
	}
}

void type_walk_super(type_walk_func *pre, type_walk_func *post, void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	type_or_ent cont;
	cont.typ = get_glob_type();
	type_walk_super_2(cont, pre, post, env);
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		cont.typ = get_irp_type(i);
		type_walk_super_2(cont, pre, post, env);
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

/*****************************************************************************/


static void class_walk_s2s_2(ir_type *tp, class_walk_func *pre,
                             class_walk_func *post, void *env)
{
	/* marked? */
	if (type_visited(tp))
		return;

	/* Assure all supertypes are visited before */
	for (size_t i = 0, n = get_class_n_supertypes(tp); i < n; ++i) {
		if (type_not_visited(get_class_supertype(tp, i)))
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

void class_walk_super2sub(class_walk_func *pre,
                          class_walk_func *post,
                          void *env)
{
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; i++) {
		ir_type *tp = get_irp_type(i);
		if (is_Class_type(tp) &&
		    (get_class_n_supertypes(tp) == 0) &&
		    type_not_visited(tp) &&
		    (! is_frame_type(tp)) &&
		    (tp != get_glob_type())) {
			class_walk_s2s_2(tp, pre, post, env);
		}
	}
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}


void walk_types_entities(ir_type *tp,
                         entity_walk_func *doit,
                         void *env)
{
	switch (get_type_tpop_code(tp)) {
	case tpo_class:
		for (size_t i = 0, n = get_class_n_members(tp); i < n; ++i)
			doit(get_class_member(tp, i), env);
		break;
	case tpo_struct:
		for (size_t i = 0, n = get_struct_n_members(tp); i < n; ++i)
			doit(get_struct_member(tp, i), env);
		break;
	case tpo_union:
		for (size_t i = 0, n = get_union_n_members(tp); i < n; ++i)
			doit(get_union_member(tp, i), env);
		break;
	case tpo_array:
		doit(get_array_element_entity(tp), env);
		break;
	case tpo_method:
	case tpo_enumeration:
	case tpo_pointer:
	case tpo_primitive:
	default:
		break;
	}
}
