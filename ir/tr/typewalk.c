/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Goetz Lindenmaier
**
** traverse the type information.  The walker walks the whole ir graph
** to find the distinct type trees in the type graph forest.
** - execute the pre function before recursion
** - execute the post function after recursion
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "irgwalk.h"
#include "irgraph.h"
#include "irnode.h"
#include "irprog.h"
#include "type_or_entity.h"

/* Make types visible to allow most efficient access */
#include "entity_t.h"
#include "type_t.h"

typedef struct type_walk_env {
  void *pre;
  void *post;
  void *env;
} type_walk_env;


void type_walk_2(type_or_ent *tore,
	       void (pre)(type_or_ent*, void*),
	       void (post)(type_or_ent*, void*),
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
	  for (i=0; i<get_class_n_supertype(tp); i++)
	    type_walk_2((type_or_ent *)get_class_supertype(tp, i), pre, post, env);
	  for (i=0; i<get_class_n_member(tp); i++)
	    type_walk_2((type_or_ent *)get_class_member(tp, i), pre, post, env);
	  for (i=0; i<get_class_n_subtype(tp); i++)
	    type_walk_2((type_or_ent *)get_class_subtype(tp, i), pre, post, env);
	}
	break;
      case tpo_struct:
	{
	  for (i=0; i<get_struct_n_member(tp); i++)
	    type_walk_2((type_or_ent *)get_struct_member(tp, i), pre, post, env);
	}
	break;
      case tpo_method:
	{
	  for (i = 0; i < get_method_n_params(tp); i++)
	    type_walk_2((type_or_ent *)get_method_param_type(tp, i), pre, post, env);
	  for (i = 0; i < get_method_n_res(tp); i++)
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

void start_type_walk(ir_node *node, void *env) {
  void *pre  = ((type_walk_env *)env)->pre;
  void *post = ((type_walk_env *)env)->post;
  void *envi = ((type_walk_env *)env)->env;

  assert(node);

  switch (get_irn_opcode(node)) {  /* node label */
  case iro_SymConst:
    if (   (get_SymConst_kind(node) == type_tag)
	   || (get_SymConst_kind(node) == size))
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
  default:
    break;
  }
}

void type_walk(void (pre)(type_or_ent*, void*),
	       void (post)(type_or_ent*, void*),
	       void *env) {
  int i;
  ++type_visited;
  /*type_walk_2((type_or_ent *)get_glob_type(), pre, post, env);
   global type is on the list visited below, too. */
  for (i = 0; i < get_irp_n_types(); i++) {
    type_walk_2((type_or_ent *)get_irp_type(i), pre, post, env);
  }
}

void type_walk_irg (ir_graph *irg,
		    void (pre)(type_or_ent*, void*),
		    void (post)(type_or_ent*, void*),
		    void *env)
{
  /* this is needed to pass the parameters to the walker that actually
     walks the type information */
  type_walk_env* type_env;
  type_env = (type_walk_env *) malloc (sizeof(type_walk_env));
  type_env->pre = pre;
  type_env->post = post;
  type_env->env = env;

  ++type_visited;
  irg_walk(get_irg_end(irg), start_type_walk, NULL, type_env);

  type_walk_2((type_or_ent *)get_irg_ent(irg), pre, post, env);

  free(type_env);
  return;
}

void type_walk_s2s_2(type_or_ent *tore,
		     void (pre)(type_or_ent*, void*),
		     void (post)(type_or_ent*, void*),
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
	  for (i=0; i<get_class_n_supertype(tp); i++)
	    type_walk_s2s_2((type_or_ent *)get_class_supertype(tp, i), pre, post, env);

	  /* execute pre method */
	  if(pre)
	    pre(tore, env);
	  tp = skip_tid((type*)tore);

	  for (i=0; i<get_class_n_subtype(tp); i++)
	    type_walk_s2s_2((type_or_ent *)get_class_subtype(tp, i), pre, post, env);

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

void type_walk_super2sub(void (pre)(type_or_ent*, void*),
			 void (post)(type_or_ent*, void*),
			 void *env) {
  int i;
  ++type_visited;
  type_walk_s2s_2((type_or_ent *)get_glob_type(), pre, post, env);
  for (i = 0; i < get_irp_n_types(); i++) {
    type_walk_s2s_2((type_or_ent *)get_irp_type(i), pre, post, env);
  }
}
