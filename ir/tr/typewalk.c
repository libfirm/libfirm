/*
 * Project:     libFIRM
 * File name:   ir/tr/typewalk.c
 * Purpose:     Traverse the type information.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file typewalk.c
 *
 * Traverse the type information.  The walker walks the whole ir graph
 * to find the distinct type trees in the type graph forest.
 * - execute the pre function before recursion
 * - execute the post function after recursion
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <stdio.h>

#include "typewalk.h"
#include "entity_t.h"
#include "type_t.h"
#include "type_or_entity.h"
#include "typegmod.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"

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

/**
 * Main walker: walks over all used types/entities of a
 * type entity.
 */
static void do_type_walk(type_or_ent *tore,
			type_walk_func *pre,
			type_walk_func *post,
			void *env)
{
  int       i, n_types, n_mem;
  ir_entity *ent = NULL;
  ir_type   *tp = NULL;
  ir_node   *n;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    ent = (ir_entity *)tore;
    if (entity_visited(ent)) return;
    break;
  case k_type:
    tp = skip_tid((ir_type *)tore);
    if (type_visited(tp)) return;
    break;
  default:
    break;
  }

  /* execute pre method */
  if (pre)
    pre(tore, env);

  /* iterate */
  switch (get_kind(tore)) {
  case k_entity:
    mark_entity_visited(ent);
    do_type_walk((type_or_ent *)get_entity_owner(ent), pre, post, env);
    do_type_walk((type_or_ent *)get_entity_type(ent), pre, post, env);

    if (get_entity_variability(ent) != variability_uninitialized) {
      /* walk over the value types */
      if (is_atomic_entity(ent)) {
        n = get_atomic_ent_value(ent);
        irn_type_walker(n, pre, post, env);
      }
      else {
        n_mem = get_compound_ent_n_values(ent);
        for (i = 0; i < n_mem; ++i) {
          n = get_compound_ent_value(ent, i);
          irn_type_walker(n, pre, post, env);
        }
      }
    }
    break;
  case k_type:
    mark_type_visited(tp);
    switch (get_type_tpop_code(tp)) {

    case tpo_class:
      n_types = get_class_n_supertypes(tp);
      for (i = 0; i < n_types; ++i)
        do_type_walk((type_or_ent *)get_class_supertype(tp, i), pre, post, env);

      n_mem = get_class_n_members(tp);
      for (i = 0; i < n_mem; ++i)
        do_type_walk((type_or_ent *)get_class_member(tp, i), pre, post, env);

      n_types = get_class_n_subtypes(tp);
      for (i = 0; i < n_types; ++i)
        do_type_walk((type_or_ent *)get_class_subtype(tp, i), pre, post, env);
      break;

    case tpo_struct:
      n_mem = get_struct_n_members(tp);
      for (i = 0; i < n_mem; ++i)
        do_type_walk((type_or_ent *)get_struct_member(tp, i), pre, post, env);
      break;

    case tpo_method:
      n_mem = get_method_n_params(tp);
      for (i = 0; i < n_mem; ++i)
        do_type_walk((type_or_ent *)get_method_param_type(tp, i), pre, post, env);

      n_mem = get_method_n_ress(tp);
      for (i = 0; i < n_mem; ++i)
        do_type_walk((type_or_ent *)get_method_res_type(tp, i), pre, post, env);
      break;

    case tpo_union:
      n_mem = get_union_n_members(tp);
      for (i = 0; i < n_mem; ++i)
        do_type_walk((type_or_ent *)get_union_member(tp, i), pre, post, env);
  	  break;

    case tpo_array:
      do_type_walk((type_or_ent *)get_array_element_type(tp),
		        pre, post, env);
      do_type_walk((type_or_ent *)get_array_element_entity(tp),
		        pre, post, env);
      break;

    case tpo_enumeration:
      /* a leave */
      break;

    case tpo_pointer:
      do_type_walk((type_or_ent *)get_pointer_points_to_type(tp),
		        pre, post, env);
      break;

    case tpo_primitive:
    case tpo_id:
    case tpo_none:
    case tpo_unknown:
      /* a leave. */
      break;
    default:
      assert(0 && "Faulty type");
      break;
    }
    break; /* end case k_type */

  default:
    printf(" *** Faulty type or entity! \n");
    break;
  }

  /* execute post method */
  if (post)
    post(tore, env);

  return;
}

/**  Check whether node contains types or entities as an attribute.
     If so start a walk over that information. */
static void irn_type_walker(
  ir_node *node, type_walk_func *pre, type_walk_func *post, void *env)
{
  ir_entity *ent;
  ir_type *tp;

  assert(node);

  ent = get_irn_entity_attr(node);
  if (ent)
    do_type_walk((type_or_ent *)ent, pre, post, env);
  tp  = get_irn_type_attr(node);
  if (tp)
    do_type_walk((type_or_ent *)tp, pre, post, env);
}

/**  Check whether node contains types or entities as an attribute.
     If so start a walk over that information. */
static void start_type_walk(ir_node *node, void *ctx) {
  type_walk_env *env = ctx;
  type_walk_func *pre;
  type_walk_func *post;
  void *envi;

  pre  = env->pre;
  post = env->post;
  envi = env->env;

  irn_type_walker(node, pre, post, envi);
}

/* walker: walks over all types */
void type_walk(type_walk_func *pre, type_walk_func *post, void *env) {
  int i, n_types = get_irp_n_types();

  inc_master_type_visited();
  for (i = 0; i < n_types; ++i) {
    do_type_walk((type_or_ent *)get_irp_type(i), pre, post, env);
  }
  do_type_walk((type_or_ent *)get_glob_type(), pre, post, env);
}

void type_walk_irg (ir_graph *irg,
		    void (*pre)(type_or_ent*, void*),
		    void (*post)(type_or_ent*, void*),
		    void *env)
{
  ir_graph *rem = current_ir_graph;
  /* this is needed to pass the parameters to the walker that actually
     walks the type information */
  type_walk_env type_env;

  type_env.pre  = pre;
  type_env.post = post;
  type_env.env  = env;

  current_ir_graph = irg;

  /* We walk over the irg to find all irnodes that contain an attribute
     with type information.  If we find one we call a type walker to
     touch the reachable type information.
     The same type can be referenced by several irnodes.  To avoid
     repeated visits of the same type node we must decrease the
     type visited flag for each walk.  This is done in start_type_walk().
     Here we initially increase the flag.  We only call do_type_walk that does
     not increase the flag.
  */
  inc_master_type_visited();
  irg_walk(get_irg_end(irg), start_type_walk, NULL, &type_env);

  do_type_walk((type_or_ent *)get_irg_entity(irg), pre, post, env);

  do_type_walk((type_or_ent *)get_irg_frame_type(irg), pre, post, env);

  current_ir_graph = rem;
  return;
}

static void type_walk_s2s_2(type_or_ent *tore,
			    void (*pre)(type_or_ent*, void*),
			    void (*post)(type_or_ent*, void*),
			    void *env)
{
  int i, n;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    if (entity_visited((ir_entity *)tore)) return;
    break;
  case k_type:
    if (type_id == get_type_tpop((ir_type*)tore)) {
      type_walk_s2s_2((type_or_ent *)skip_tid((ir_type *)tore), pre, post, env);
      return;
    }
    if (type_visited((ir_type *)tore)) return;
    break;
  default:
    break;
  }

  /* iterate */
  switch (get_kind(tore)) {
  case k_type:
    {
      ir_type *tp = (ir_type *)tore;
      mark_type_visited(tp);
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
        {
          n = get_class_n_supertypes(tp);
          for (i = 0; i < n; ++i) {
            type_walk_s2s_2((type_or_ent *)get_class_supertype(tp, i), pre,
                            post, env);
          }
          /* execute pre method */
          if (pre)
            pre(tore, env);
          tp = skip_tid((ir_type*)tore);

          n = get_class_n_subtypes(tp);
          for (i = 0; i < n; ++i) {
            type_walk_s2s_2((type_or_ent *)get_class_subtype(tp, i), pre,
                            post, env);
          }

          /* execute post method */
          if (post)
            post(tore, env);
        }
        break;
      case tpo_struct:
      case tpo_method:
      case tpo_union:
      case tpo_array:
      case tpo_enumeration:
      case tpo_pointer:
      case tpo_primitive:
      case tpo_id:
        /* dont care */
        break;
      default:
        printf(" *** Faulty type! \n");
        break;
      }
    } break; /* end case k_type */
  case k_entity:
    /* dont care */
    break;
  default:
    printf(" *** Faulty type or entity! \n");
    break;
  }
  return;
}

void type_walk_super2sub(
		  void (*pre)(type_or_ent*, void*),
		  void (*post)(type_or_ent*, void*),
		  void *env)
{
  int i, n_types = get_irp_n_types();
  ir_type *tp;

  inc_master_type_visited();
  type_walk_s2s_2((type_or_ent *)get_glob_type(), pre, post, env);
  for (i = 0; i < n_types; ++i) {
    tp = get_irp_type(i);
    type_walk_s2s_2((type_or_ent *)tp, pre, post, env);
  }
}

/*****************************************************************************/

static void
type_walk_super_2(type_or_ent *tore,
		  void (*pre)(type_or_ent*, void*),
		  void (*post)(type_or_ent*, void*),
		  void *env)
{
  int i, n;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    if (entity_visited((ir_entity *)tore)) return;
    break;
  case k_type:
    if (type_id == get_type_tpop((ir_type*)tore)) {
      type_walk_super_2((type_or_ent *)skip_tid((ir_type *)tore), pre, post, env);
      return;
    }
    if (type_visited((ir_type *)tore)) return;
    break;
  default:
    break;
  }

  /* iterate */
  switch (get_kind(tore)) {
  case k_type:
    {
      ir_type *tp = (ir_type *)tore;
      mark_type_visited(tp);
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
        {
          /* execute pre method */
          if (pre)
            pre(tore, env);
          tp = skip_tid((ir_type*)tore);

          n = get_class_n_supertypes(tp);
          for (i = 0; i < n; ++i) {
            type_walk_super_2((type_or_ent *)get_class_supertype(tp, i), pre,
                              post, env);
          }

          /* execute post method */
          if (post)
            post(tore, env);
        }
        break;
      case tpo_struct:
      case tpo_method:
      case tpo_union:
      case tpo_array:
      case tpo_enumeration:
      case tpo_pointer:
      case tpo_primitive:
      case tpo_id:
        /* dont care */
        break;
      default:
        printf(" *** Faulty type! \n");
        break;
      }
    } break; /* end case k_type */
  case k_entity:
    /* dont care */
    break;
  default:
    printf(" *** Faulty type or entity! \n");
    break;
  }
  return;
}

void type_walk_super(
		  void (*pre)(type_or_ent*, void*),
		  void (*post)(type_or_ent*, void*),
		  void *env) {
  int i, n_types = get_irp_n_types();
  ir_type *tp;

  inc_master_type_visited();
  type_walk_super_2((type_or_ent *)get_glob_type(), pre, post, env);
  for (i = 0; i < n_types; ++i) {
    tp = get_irp_type(i);
    type_walk_super_2((type_or_ent *)tp, pre, post, env);
  }
}

/*****************************************************************************/


static void
class_walk_s2s_2(ir_type *tp,
		 void (*pre)(ir_type*, void*),
		 void (*post)(ir_type*, void*),
		 void *env)
{
  int i, n;

  /* marked? */
  if (type_visited(tp)) return;

  assert(is_Class_type(tp));
  /* Assure all supertypes are visited before */
  n = get_class_n_supertypes(tp);
  for (i = 0; i < n; ++i) {
    if (type_not_visited(get_class_supertype(tp, i)))
      return;
  }

  mark_type_visited(tp);

  /* execute pre method */
  if (pre)
    pre(tp, env);

  tp = skip_tid(tp);
  n = get_class_n_subtypes(tp);
  for (i = 0; i < n; ++i) {
    class_walk_s2s_2(get_class_subtype(tp, i), pre, post, env);
  }
  /* execute post method */
  if (post)
    post(tp, env);

  return;
}

void class_walk_super2sub(
		  void (*pre)(ir_type*, void*),
		  void (*post)(ir_type*, void*),
		  void *env)
{
  int i, n_types = get_irp_n_types();
  ir_type *tp;

  inc_master_type_visited();
  for (i = 0; i < n_types; i++) {
    tp = get_irp_type(i);
    if (is_Class_type(tp) &&
	      (get_class_n_supertypes(tp) == 0) &&
	      type_not_visited(tp)) {
      assert(! is_frame_type(tp));
      assert(tp != get_glob_type());
      class_walk_s2s_2(tp, pre, post, env);
    }
  }
}


/* Walks over all entities in the type */
void walk_types_entities(
		  ir_type *tp,
		  void (*doit)(ir_entity*, void*),
		  void *env)
{
  int i, n;

  switch (get_type_tpop_code(tp)) {
  case tpo_class:
    n = get_class_n_members(tp);
    for (i = 0; i < n; ++i)
      doit(get_class_member(tp, i), env);
    break;
  case tpo_struct:
    n = get_struct_n_members(tp);
    for (i = 0; i < n; ++i)
      doit(get_struct_member(tp, i), env);
    break;
  case tpo_union:
    n = get_union_n_members(tp);
    for (i = 0; i < n; ++i)
      doit(get_union_member(tp, i), env);
    break;
  case tpo_array:
    doit(get_array_element_entity(tp), env);
    break;
  case tpo_method:
  case tpo_enumeration:
  case tpo_pointer:
  case tpo_primitive:
  case tpo_id:
  default:
    break;
  }
}
