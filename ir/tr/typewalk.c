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

/*
 * traverse the type information.  The walker walks the whole ir graph
 * to find the distinct type trees in the type graph forest.
 * - execute the pre function before recursion
 * - execute the post function after recursion
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
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

typedef struct type_walk_env {
  type_walk_func *pre;
  type_walk_func *post;
  void *env;
} type_walk_env;


static void type_walk_2(type_or_ent *tore,
			void (*pre) (type_or_ent*, void*),
			void (*post)(type_or_ent*, void*),
			void *env)
{
  int i;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    if (((entity *)tore)->visit >= type_visited) return;
    break;
  case k_type:
    if(type_id == get_type_tpop((type*)tore)) {
      type_walk_2((type_or_ent *)skip_tid((type *)tore), pre, post, env);
      return;
    }
    if (((type *)tore)->visit >= type_visited) return;
    break;
  default:
    break;
  }

  /* execute pre method */
  if(pre)
    pre(tore, env);

  /* iterate */
  switch (get_kind(tore)) {
  case k_entity:
    {

      entity *ent = (entity *)tore;
      ent->visit = type_visited;
      type_walk_2((type_or_ent *)get_entity_owner(ent), pre, post, env);
      type_walk_2((type_or_ent *)get_entity_type(ent), pre, post, env);
    }
    break;
  case k_type:
    {
      type *tp = (type *)tore;
      mark_type_visited(tp);
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
	{
	  for (i=0; i<get_class_n_supertypes(tp); i++)
	    type_walk_2((type_or_ent *)get_class_supertype(tp, i), pre, post, env);
	  for (i=0; i<get_class_n_members(tp); i++)
	    type_walk_2((type_or_ent *)get_class_member(tp, i), pre, post, env);
	  for (i=0; i<get_class_n_subtypes(tp); i++)
	    type_walk_2((type_or_ent *)get_class_subtype(tp, i), pre, post, env);
	}
	break;
      case tpo_struct:
	{
	  for (i=0; i<get_struct_n_members(tp); i++)
	    type_walk_2((type_or_ent *)get_struct_member(tp, i), pre, post, env);
	}
	break;
      case tpo_method:
	{
	  for (i = 0; i < get_method_n_params(tp); i++)
	    type_walk_2((type_or_ent *)get_method_param_type(tp, i), pre, post, env);
	  for (i = 0; i < get_method_n_ress(tp); i++)
	    type_walk_2((type_or_ent *)get_method_res_type(tp, i), pre, post, env);
	}
	break;
      case tpo_union:
	{
	  for (i = 0; i < get_union_n_members(tp); i++)
	    type_walk_2((type_or_ent *)get_union_member(tp, i), pre, post, env);
	}
	break;
      case tpo_array:
	type_walk_2((type_or_ent *)get_array_element_type(tp),
		    pre, post, env);
	type_walk_2((type_or_ent *)get_array_element_entity(tp),
		    pre, post, env);
	break;
      case tpo_enumeration:
	/* a leave */
	break;
      case tpo_pointer:
	type_walk_2((type_or_ent *)get_pointer_points_to_type(tp),
		    pre, post, env);
	break;
      case tpo_primitive:
      case tpo_id:
	/* a leave. */
	break;
      default:
	printf(" *** Faulty type! \n");
	break;
      }
    } break; /* end case k_type */
  default:
    printf(" *** Faulty type or entity! \n");
    break;
  }

  /* execute post method */
  if(post)
    post(tore, env);

  return;
}

/**  Check wether node contains types or entities as an attribute.
     If so start a walk over that information. */
static void start_type_walk(ir_node *node, void *env) {
 type_walk_func *pre;
 type_walk_func *post;
 void *envi;

  pre  = ((type_walk_env *)env)->pre;
  post = ((type_walk_env *)env)->post;
  envi = ((type_walk_env *)env)->env;

  assert(node);

  switch (get_irn_opcode(node)) {  /* node label */
  case iro_SymConst:
    if (   (get_SymConst_kind(node) ==symconst_type_tag)
	   || (get_SymConst_kind(node) ==symconst_size))
      type_walk_2((type_or_ent *)get_SymConst_type(node), pre, post, envi);
    break;
  case iro_Sel:
    type_walk_2((type_or_ent *)get_Sel_entity(node), pre, post, envi);
    break;
  case iro_Call:
    type_walk_2((type_or_ent *)get_Call_type(node), pre, post, envi);
    break;
  case iro_Alloc:
    type_walk_2((type_or_ent *)get_Alloc_type(node), pre, post, envi);
    break;
  case iro_Free:
    type_walk_2((type_or_ent *)get_Free_type(node), pre, post, envi);
    break;
  case iro_Cast:
    type_walk_2((type_or_ent *)get_Cast_type(node), pre, post, envi);
    break;
  default:
    break;
  }
}

void type_walk(type_walk_func *pre,
	       type_walk_func *post,
	       void *env) {
  int i, n_types = get_irp_n_types();
  ++type_visited;
  /*type_walk_2((type_or_ent *)get_glob_type(), pre, post, env);
   global type is on the list visited below, too. */
  for (i = 0; i < n_types; i++) {
    type_walk_2((type_or_ent *)get_irp_type(i), pre, post, env);
  }
  type_walk_2((type_or_ent *)get_glob_type(), pre, post, env);
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
     Here we initially increase the flag.  We only call type_walk_2 that does
     not increase the flag.
  */
  ++type_visited;
  irg_walk(get_irg_end(irg), start_type_walk, NULL, &type_env);

  type_walk_2((type_or_ent *)get_irg_ent(irg), pre, post, env);

  type_walk_2((type_or_ent *)get_irg_frame_type(irg), pre, post, env);

  current_ir_graph = rem;
  return;
}

static void type_walk_s2s_2(type_or_ent *tore,
			    void (*pre)(type_or_ent*, void*),
			    void (*post)(type_or_ent*, void*),
			    void *env)
{
  int i;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    if (((entity *)tore)->visit >= type_visited) return;
    break;
  case k_type:
    if(type_id == get_type_tpop((type*)tore)) {
      type_walk_s2s_2((type_or_ent *)skip_tid((type *)tore), pre, post, env);
      return;
    }
    if (((type *)tore)->visit >= type_visited) return;
    break;
  default:
    break;
  }

  /* iterate */
  switch (get_kind(tore)) {
  case k_type:
    {
      type *tp = (type *)tore;
      mark_type_visited(tp);
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
	{
	  for (i = 0; i < get_class_n_supertypes(tp); i++) {
	    type_walk_s2s_2((type_or_ent *)get_class_supertype(tp, i), pre,
			    post, env);
	  }
	  /* execute pre method */
	  if(pre)
	    pre(tore, env);
	  tp = skip_tid((type*)tore);

	  for (i = 0; i < get_class_n_subtypes(tp); i++) {
	    type_walk_s2s_2((type_or_ent *)get_class_subtype(tp, i), pre,
			    post, env);
	  }

	  /* execute post method */
	  if(post)
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
  type *tp;
  ++type_visited;
  type_walk_s2s_2((type_or_ent *)get_glob_type(), pre, post, env);
  for (i = 0; i < n_types; i++) {
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
  int i;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    if (((entity *)tore)->visit >= type_visited) return;
    break;
  case k_type:
    if(type_id == get_type_tpop((type*)tore)) {
      type_walk_super_2((type_or_ent *)skip_tid((type *)tore), pre, post, env);
      return;
    }
    if (((type *)tore)->visit >= type_visited) return;
    break;
  default:
    break;
  }

  /* iterate */
  switch (get_kind(tore)) {
  case k_type:
    {
      type *tp = (type *)tore;
      mark_type_visited(tp);
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
	{
	  /* execute pre method */
	  if(pre)
	    pre(tore, env);
	  tp = skip_tid((type*)tore);

	  for (i = 0; i < get_class_n_supertypes(tp); i++) {
	    type_walk_super_2((type_or_ent *)get_class_supertype(tp, i), pre,
			      post, env);
	  }

	  /* execute post method */
	  if(post)
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
  type *tp;
  ++type_visited;
  type_walk_super_2((type_or_ent *)get_glob_type(), pre, post, env);
  for (i = 0; i < n_types; i++) {
    tp = get_irp_type(i);
    type_walk_super_2((type_or_ent *)tp, pre, post, env);
  }
}

/*****************************************************************************/


static void
class_walk_s2s_2(type *tp,
		 void (*pre)(type*, void*),
		 void (*post)(type*, void*),
		 void *env)
{
  int i;

  /* marked? */
  if (tp->visit >= type_visited) return;

  assert(is_class_type(tp));
  /* Assure all supertypes are visited before */
  for (i=0; i < get_class_n_supertypes(tp); i++) {
    if (get_type_visited(get_class_supertype(tp, i)) < type_visited)
      return;
  }

  mark_type_visited(tp);

  /* execute pre method */
  if(pre)
    pre(tp, env);

  tp = skip_tid((type*)tp);
  for (i=0; i<get_class_n_subtypes(tp); i++) {
    class_walk_s2s_2(get_class_subtype(tp, i), pre, post, env);
  }
  /* execute post method */
  if(post)
    post(tp, env);

  return;
}

void class_walk_super2sub(
		  void (*pre)(type*, void*),
		  void (*post)(type*, void*),
		  void *env)
{
  int i, n_types = get_irp_n_types();
  type *tp;

  ++type_visited;
  for (i = 0; i < n_types; i++) {
    tp = get_irp_type(i);
    if (is_class_type(tp) &&
	(get_class_n_supertypes(tp) == 0) &&
	(tp->visit < type_visited))  {
      assert(!is_frame_type(tp));
      assert(tp != get_glob_type());
      class_walk_s2s_2(tp, pre, post, env);
    }
  }
}


/* Walks over all entities in the type */
void walk_types_entities(
		  type *tp,
		  void (*doit)(entity*, void*),
		  void *env)
{
  int i;
  switch(get_type_tpop_code(tp)) {
  case tpo_class: {
    for (i=0; i<get_class_n_members(tp); i++)
      doit(get_class_member(tp, i), env);
  } break;
  case tpo_struct: {
    for (i=0; i<get_struct_n_members(tp); i++)
      doit(get_struct_member(tp, i), env);
  } break;
  case tpo_union: {
    for (i = 0; i < get_union_n_members(tp); i++)
      doit(get_union_member(tp, i), env);
  } break;
  case tpo_array: {
      doit(get_array_element_entity(tp), env);
  } break;
  case tpo_method:
  case tpo_enumeration:
  case tpo_pointer:
  case tpo_primitive:
  case tpo_id:
  default:
    break;
  }
}
