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

#include "irgwalk.h"
#include "irgraph.h"
#include "irnode.h"
#include "type_or_entity.h"

/* Make types visible to allow most efficient access */
# include "entity_t.h"

typedef struct type_walk_env {
  void *pre;
  void *post;
  void *env;
} type_walk_env;


void type_walk_2(type_or_ent *tore,
	      void (pre)(type_or_ent*, void*), void (post)(type_or_ent*, void*),
	    void *env)
{
  int i, visited = 0;

  /* marked? */
  switch (get_kind(tore)) {
  case k_entity:
    if (((entity *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_class:
    if (((type_class *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_strct:
    if (((type_strct *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_method:
    if (((type_method *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_union:
    if (((type_union *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_array:
    if (((type_array *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_enumeration:
    if (((type_enumeration *)tore)->visit >= type_visited) visited = 1; break;
  case k_type_pointer:
    if (((type_pointer *)tore)->visit >= type_visited) visited = 1;  break;
  case k_type_primitive:
    if (((type_primitive *)tore)->visit >= type_visited) visited = 1;  break;
  default:
    break;
  }

  if (!visited) { /* not marked. */

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
    case k_type_class:
      ((type_class *)tore)->visit = type_visited;
      /* !!!!! */
      break;
    case k_type_strct:
      ((type_strct *)tore)->visit = type_visited;
      /* !!!!! */
      break;
    case k_type_method:
      {
	type_method *meth  = (type_method *)tore;
	meth->visit = type_visited;
	for (i = 0; i < get_method_arity(meth); i++)
	  type_walk_2((type_or_ent *)get_method_param_type(meth, i), pre, post, env);
	for (i = 0; i < get_method_n_res(meth); i++)
	  type_walk_2((type_or_ent *)get_method_res_type(meth, i), pre, post, env);
      }
      break;
    case k_type_union:
      ((type_union *)tore)->visit = type_visited;
      break;
    case k_type_array:
      ((type_array *)tore)->visit = type_visited;
      type_walk_2((type_or_ent *)get_array_element_type((type_array *)tore),
		  pre, post, env);
      break;
    case k_type_enumeration:
      ((type_enumeration *)tore)->visit = type_visited;
      /* a leave */
      break;
    case k_type_pointer:
      ((type_pointer *)tore)->visit = type_visited;
      type_walk_2((type_or_ent *)get_pointer_points_to_type((type_pointer *)tore),
		  pre, post, env);
      break;
    case k_type_primitive:
      ((type_primitive *)tore)->visit = type_visited;
      /* a leave. */
      break;
    default:
      break;
    }

    /* execute post method */
    if(post)
      post(tore, env);
  }

  return;
}

void start_type_walk(ir_node *node, void *env) {
  void *pre  = ((type_walk_env *)env)->pre;
  void *post = ((type_walk_env *)env)->post;
  void *envi  = ((type_walk_env *)env)->env;

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
      printf("here in typewalk\n");
      type_walk_2((type_or_ent *)get_Free_type(node), pre, post, envi);
      break;
  assert(node);
    default:
      break;
    }
}

void type_walk(ir_graph *irg,
	      void (pre)(type_or_ent*, void*), void (post)(type_or_ent*, void*),
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
  irg_walk(irg->end, start_type_walk, NULL, type_env);

  type_walk_2((type_or_ent *)get_irg_ent(irg), pre, post, env);

  return;
}
