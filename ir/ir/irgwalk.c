/*
 * Project:     libFIRM
 * File name:   ir/ir/irgwalk.c
 * Purpose:
 * Author:      Boris Boesler
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* traverse an ir graph
* - execute the pre function before recursion
* - execute the post function after recursion
*/


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <stdlib.h>

# include "irnode_t.h"
# include "irgraph_t.h" /* visited flag */
# include "irprog.h"
# include "irgwalk.h"
# include "typewalk.h"

# include "eset.h"
# include "array.h"

/* walk over an interprocedural graph (callgraph). Visits only graphs in irg_set. */
static void irg_walk_cg(ir_node * node, int visited, eset * irg_set,
			irg_walk_func *pre, irg_walk_func *post, void * env) {
  int i;
  ir_graph * rem = current_ir_graph;
  ir_node * pred;

  assert(node && node->kind == k_ir_node);
  if (get_irn_visited(node) >= visited) return;

  set_irn_visited(node, visited);

  pred = skip_Proj(node);
  if (intern_get_irn_op(pred) == op_CallBegin
      || intern_get_irn_op(pred) == op_EndReg
      || intern_get_irn_op(pred) == op_EndExcept) {
    current_ir_graph = get_irn_irg(pred);
  }

  if (pre) pre(node, env);

  if (is_no_Block(node))
    irg_walk_cg(get_nodes_block(node), visited, irg_set, pre, post, env);

  if (intern_get_irn_op(node) == op_Block) { /* block */
    for (i = intern_get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node * exec = intern_get_irn_n(node, i);
      ir_node * pred = skip_Proj(exec);
      if ((intern_get_irn_op(pred) != op_CallBegin
	   && intern_get_irn_op(pred) != op_EndReg
	   && intern_get_irn_op(pred) != op_EndExcept)
	  || eset_contains(irg_set, get_irn_irg(pred))) {
	irg_walk_cg(exec, visited, irg_set, pre, post, env);
      }
    }
  } else if (intern_get_irn_op(node) == op_Filter) {
    for (i = intern_get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node * pred = intern_get_irn_n(node, i);
      if (intern_get_irn_op(pred) == op_Unknown || intern_get_irn_op(pred) == op_Bad) {
	irg_walk_cg(pred, visited, irg_set, pre, post, env);
      } else {
	ir_node * exec;
	exec = skip_Proj(get_Block_cfgpred(get_nodes_block(node), i));
	assert(intern_get_irn_op(exec) == op_CallBegin
	       || intern_get_irn_op(exec) == op_EndReg
	       || intern_get_irn_op(exec) == op_EndExcept);
	if (eset_contains(irg_set, get_irn_irg(exec))) {
	  current_ir_graph = get_irn_irg(exec);
	  irg_walk_cg(pred, visited, irg_set, pre, post, env);
	  current_ir_graph = rem;
	}
      }
    }
  } else {
    for (i = intern_get_irn_arity(node) - 1; i >= 0; --i) {
      irg_walk_cg(intern_get_irn_n(node, i), visited, irg_set, pre, post, env);
    }
  }

  if (post) post(node, env);

  current_ir_graph = rem;
}


/* Insert all ir_graphs in irg_set, that are (transitive) reachable. */
static void collect_irgs(ir_node * node, eset * irg_set) {
  if (intern_get_irn_op(node) == op_Call) {
    int i;
    for (i = get_Call_n_callees(node) - 1; i >= 0; --i) {
      entity * ent = get_Call_callee(node, i);
      ir_graph * irg = ent ? get_entity_irg(ent) : NULL;
      if (irg && !eset_contains(irg_set, irg)) {
	eset_insert(irg_set, irg);
	irg_walk_graph(irg, (irg_walk_func *) collect_irgs, NULL, irg_set);
      }
    }
  }
}

static void
irg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void * env)
{
  int i;
  assert(node && node->kind==k_ir_node);
#if 0 /* safe */
  if (get_irn_visited(node) < get_irg_visited(current_ir_graph)) {
    set_irn_visited(node, get_irg_visited(current_ir_graph));

    if (pre) pre(node, env);

    if (is_no_Block(node))
      irg_walk_2(get_nodes_block(node), pre, post, env);
    for (i = intern_get_irn_arity(node) - 1; i >= 0; --i)
      irg_walk_2(intern_get_irn_n(node, i), pre, post, env);

    if (post) post(node, env);
  }
#else /* faster */
  if (node->visited < current_ir_graph->visited) {
    set_irn_visited(node, current_ir_graph->visited);

    if (pre) pre(node, env);

    if (node->op != op_Block)
      irg_walk_2(intern_get_irn_n(node, -1), pre, post, env);
    for (i = intern_get_irn_arity(node) - 1; i >= 0; --i)
      irg_walk_2(intern_get_irn_n(node, i), pre, post, env);

    if (post) post(node, env);
  }
#endif
}


void irg_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  assert(node  && node->kind==k_ir_node);
  if (interprocedural_view) {
    eset * irg_set = eset_create();
    int visited;
    ir_graph * irg;
    interprocedural_view = false;
    eset_insert(irg_set, current_ir_graph);
    irg_walk(node, (irg_walk_func *) collect_irgs, NULL, irg_set);
    interprocedural_view = true;
    visited = get_max_irg_visited() + 1;
    for (irg = eset_first(irg_set); irg; irg = eset_next(irg_set)) {
      set_irg_visited(irg, visited);
    }
    irg_walk_cg(node, visited, irg_set, pre, post, env);
    eset_destroy(irg_set);
  } else {
    inc_irg_visited(current_ir_graph);
    irg_walk_2(node, pre, post, env);
  }
  return;
}


void irg_walk_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env) {
  ir_graph * rem = current_ir_graph;
  current_ir_graph = irg;
  irg_walk(get_irg_end(irg), pre, post, env);
  current_ir_graph = rem;
}

/* Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
   Sets current_ir_graph properly for each walk.  Conserves current
   current_ir_graph. */
void all_irg_walk(irg_walk_func *pre, irg_walk_func *post, void *env) {
  int i;
  ir_graph *irg, *rem;

  rem = current_ir_graph;

  for (i = 0; i < get_irp_n_irgs(); i++) {
    irg = get_irp_irg(i);
    current_ir_graph = irg;
    irg_walk(get_irg_end(irg), pre, post, env);
  }
  current_ir_graph = rem;
}

/***************************************************************************/

/* Returns current_ir_graph and sets it to the irg of predecessor index
   of node n. */
static INLINE ir_graph *
switch_irg (ir_node *n, int index) {
  ir_graph *old_current = current_ir_graph;

  if (interprocedural_view) {
    /* Only Filter and Block nodes can have predecessors in other graphs. */
    if (intern_get_irn_op(n) == op_Filter)
      n = get_nodes_block(n);
    if (intern_get_irn_op(n) == op_Block) {
      ir_node *cfop = skip_Proj(get_Block_cfgpred(n, index));
      if (is_ip_cfop(cfop)) {
	current_ir_graph = get_irn_irg(cfop);
      }
    }
  }

  return old_current;
}

static void
cg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void * env)
{
  int i;
  ir_graph *rem = NULL;
  assert(node);

  if (get_irn_visited(node) < get_irg_visited(current_ir_graph)) {
    set_irn_visited(node, get_irg_visited(current_ir_graph));

    if (pre) pre(node, env);

    if (is_no_Block(node))
      cg_walk_2(get_nodes_block(node), pre, post, env);
    for (i = intern_get_irn_arity(node) - 1; i >= 0; --i) {
      rem = switch_irg(node, i);  /* @@@ AS: Is this wrong? We do have to
                                             switch to the irg of the predecessor, don't we? */
      cg_walk_2(intern_get_irn_n(node, i), pre, post, env);
      current_ir_graph = rem;
    }

    if (post) post(node, env);
  }
  return;
}


/* Walks all irgs in interprocedural view.  Visits each node only once. */
void cg_walk(irg_walk_func *pre, irg_walk_func *post, void *env) {
  int i;
  ir_graph *rem = current_ir_graph;
  int rem_view = interprocedural_view;

  interprocedural_view = true;

  inc_max_irg_visited();
  /* Fix all irg_visited flags */
  for (i = 0; i < get_irp_n_irgs(); i++)
    set_irg_visited(get_irp_irg(i), get_max_irg_visited());

  /* Walk starting at unreachable procedures. Only these
   * have End blocks visible in interprocedural view. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);

    sb = get_irg_start_block(current_ir_graph);

    if ((get_Block_n_cfgpreds(sb) > 1) ||
	(get_nodes_block(get_Block_cfgpred(sb, 0)) != sb)) continue;

    cg_walk_2(get_irg_end(current_ir_graph), pre, post, env);
  }

  /* Check whether we walked all procedures: there could be procedures
     with cyclic calls but no call from the outside. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);

    /* Test start block: if inner procedure end and end block are not
     * visible and therefore not marked. */
    sb = get_irg_start_block(current_ir_graph);
    if (get_irn_visited(sb) < get_irg_visited(current_ir_graph)) {
      cg_walk_2(sb, pre, post, env);
    }
  }

  /* Walk all endless loops in inner procedures.
   * We recognize an inner procedure if the End node is not visited. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *e;
    current_ir_graph = get_irp_irg(i);
    e = get_irg_end(current_ir_graph);
    if (get_irn_visited(e) < get_irg_visited(current_ir_graph)) {
      int j;
      /* Don't visit the End node. */
      for (j = 0; j < get_End_n_keepalives(e); j++)
	cg_walk_2(get_End_keepalive(e, j), pre, post, env);
    }
  }

  interprocedural_view = rem_view;
  current_ir_graph = rem;
}


/***************************************************************************/

/* Walks back from n until it finds a real cf op. */
static ir_node *get_cf_op(ir_node *n) {
  ir_node *pred;

  n = skip_nop(n);
  n = skip_Tuple(n);
  pred = skip_Proj(n);
  if (!(is_cfop(pred) || is_fragile_op(pred) ||
	(intern_get_irn_op(pred) == op_Bad)))
    n = get_cf_op(n);

  return skip_Proj(n);
}

static void irg_block_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  int i;

  if(get_Block_block_visited(node) < get_irg_block_visited(current_ir_graph)) {
    set_Block_block_visited(node, get_irg_block_visited(current_ir_graph));

    if(pre) pre(node, env);

    for(i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
      /* find the corresponding predecessor block. */
      ir_node *pred = get_cf_op(get_Block_cfgpred(node, i));
      pred = get_nodes_block(pred);
      if(intern_get_irn_opcode(pred) == iro_Block) {
	/* recursion */
	irg_block_walk_2(pred, pre, post, env);
      }
      else {
	assert(get_irn_opcode(pred) == iro_Bad);
      }
    }

    if(post) post(node, env);
  }
  return;
}


/* walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_block_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  ir_node *block, *pred;
  int i;

  assert(node);
  assert(!interprocedural_view);   /* interprocedural_view not implemented, because it
				    * interleaves with irg_walk */
  inc_irg_block_visited(current_ir_graph);
  if (is_no_Block(node)) block = get_nodes_block(node); else block = node;
  assert(get_irn_opcode(block)  == iro_Block);
  irg_block_walk_2(block, pre, post, env);
  /* keepalive: the endless loops ... */
  if (intern_get_irn_op(node) == op_End) {
    int arity = intern_get_irn_arity(node);
    for (i = 0; i < arity; i++) {
      pred = intern_get_irn_n(node, i);
      if (intern_get_irn_op(pred) == op_Block)
	irg_block_walk_2(pred, pre, post, env);
    }
  }

  return;
}


void irg_block_walk_graph(ir_graph *irg, irg_walk_func *pre,
			  irg_walk_func *post, void *env) {
  ir_graph * rem = current_ir_graph;
  current_ir_graph = irg;
  irg_block_walk(get_irg_end(irg), pre, post, env);
  current_ir_graph = rem;
}

/********************************************************************/

typedef struct walk_env {
  irg_walk_func *pre;
  irg_walk_func *post;
  void *env;
} walk_env;

/* Walk to all constant expressions in this entity. */
static void walk_entity(entity *ent, void *env)
{
  walk_env *my_env = (walk_env *)env;

  if (get_entity_variability(ent) != variability_uninitialized) {
    if (is_atomic_entity(ent)) {
      irg_walk(get_atomic_ent_value(ent), my_env->pre, my_env->post, my_env->env);
    }
    else {
      int i, n = get_compound_ent_n_values(ent);

      for (i = 0; i < n; i++)
	irg_walk(get_compound_ent_value(ent, i), my_env->pre, my_env->post, my_env->env);
    }
  }
}

/* Walks over all code in const_code_irg. */
void walk_const_code(irg_walk_func *pre, irg_walk_func *post, void *env) {
  int i, j;
  walk_env my_env;

  ir_graph *rem = current_ir_graph;
  current_ir_graph = get_const_code_irg();
  inc_irg_visited(current_ir_graph);

  my_env.pre = pre;
  my_env.post = post;
  my_env.env = env;

  /* Walk all types that can contain constant entities.  */
  walk_types_entities(get_glob_type(), &walk_entity, &my_env);
  for (i = 0; i < get_irp_n_types(); i++)
    walk_types_entities(get_irp_type(i), &walk_entity, &my_env);
  for (i = 0; i < get_irp_n_irgs(); i++)
    walk_types_entities(get_irg_frame_type(get_irp_irg(i)), &walk_entity, &my_env);

  /* Walk constant array bounds. */
  for (i = 0; i < get_irp_n_types(); i++) {
    type *tp = get_irp_type(i);
    if (is_array_type(tp)) {
      for (j = 0; j < get_array_n_dimensions(tp); j++) {
	ir_node *n;
	n = get_array_lower_bound(tp, j);
	if (n) irg_walk(n, pre, post, env);
	n = get_array_upper_bound(tp, j);
	if (n) irg_walk(n, pre, post, env);
      }
    }
  }

  current_ir_graph = rem;
}


/********************************************************************/
/** Walking support for interprocedural analysis                   **/
/**                                                                **/
/** @@@ Don't use, not operational yet, doesn't grok recursions!!  **/
/** @@@ Header for irgwalk.h, here until it works. **/
/**                                                                **/
/** Interprocedural walking should not walk all predecessors of    **/
/** all nodes.  When leaving a procedure the walker should only    **/
/** follow the edge corresponding to the most recent entry of the  **/
/** procedure.  The following functions use an internal stack to   **/
/** remember the current call site of a procedure.                 **/
/** They also set current_ir_graph correctly.                      **/
/**                                                                **/
/** Usage example:                                                 **/
/**                                                                **/
/** void init_ip_walk ();                                          **/
/** work_on_graph(some_end_node);                                  **/
/** void finish_ip_walk();                                         **/
/**                                                                **/
/** work_on_graph(ir_node *n) {                                    **/
/**   for (i = 0; i < get_irn_arity(n); i++) {                     **/
/**     if (...) continue;                                         **/
/**     ir_node *m = get_irn_ip_pred(n, i);                        **/
/**     if !m continue;                                            **/
/**     work_on_graph(m);                                          **/
/**     return_recur(n, i);                                        **/
/**   }                                                            **/
/** }                                                              **/
/********************************************************************/

/* Call for i in {0|-1 ... get_irn_arity(n)}.
   If n is a conventional node returns the same node as get_irn_n(n, i).
   If the predecessors of n are in the callee of the procedure n belongs
   to, returns get_irn_n(n, i) if this node is in the callee on the top
   of the stack, else returns NULL.
   If the predecessors of n are in a procedure called by the procedure n
   belongs to pushes the caller on the caller stack in the callee.
   Sets current_ir_graph to the graph the node returned is in. */
ir_node *get_irn_ip_pred(ir_node *n, int pos);

/* If get_irn_ip_pred() returned a node (not NULL) this must be
   called to clear up the stacks.
   Sets current_ir_graph to the graph n is in. */
void return_recur(ir_node *n, int pos);


/********************************************************************/
/** Walking support for interprocedural analysis                   **/
/********************************************************************/

#define MIN_STACK_SIZE 40

typedef struct callsite_stack {
  int tos;
  ir_node **s;
} callsite_stack;

/* Access the stack in the irg **************************************/

static INLINE void
set_irg_callsite_stack(ir_graph *g, callsite_stack *s) {
  set_irg_link(g, s);
}

static INLINE callsite_stack *
get_irg_callsite_stack(ir_graph *g) {
  return (callsite_stack *) get_irg_link(g);
}

/* A stack of callsites *********************************************/

/* @@@ eventually change the implementation so the new_ only sets the field
   to NULL, and the stack is only allocated if used.  Saves Memory! */
static INLINE callsite_stack *
new_callsite_stack(ir_graph *g) {
  callsite_stack *res = (callsite_stack *)malloc(sizeof(callsite_stack));
  res->tos = 0;
  res->s = NEW_ARR_F (ir_node *, MIN_STACK_SIZE);
  set_irg_callsite_stack(g, res);
  return(res);
}

static INLINE void
free_callsite_stack(ir_graph *g) {
  callsite_stack *s = get_irg_callsite_stack(g);
  DEL_ARR_F(s->s);
  free(s);
}

static INLINE void
push_callsite(ir_graph *callee, ir_node *callsite) {
  callsite_stack *s = get_irg_callsite_stack(callee);
  if (s->tos == ARR_LEN(s->s)) {
    int nlen = ARR_LEN (s->s) * 2;
    ARR_RESIZE (ir_node *, s->s, nlen);
  }
  s->s[s->tos] = callsite;
  s->tos++;
}

static INLINE ir_node *
get_top_of_callsite_stack(ir_graph *callee) {
  callsite_stack *s = get_irg_callsite_stack(callee);
  return (s->s[s->tos-1]);
}

static INLINE
ir_node * pop_callsite(ir_graph *callee) {
  callsite_stack *s = get_irg_callsite_stack(callee);
  s->tos--;
  return (s->s[s->tos]);
}


/* Initialization routines ******************************************/

void init_ip_walk (void) {
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++)
    new_callsite_stack(get_irp_irg(i));
}

void finish_ip_walk(void) {
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++)
    free_callsite_stack(get_irp_irg(i));
  set_irg_link(get_irp_irg(i), NULL);
}

/* walker routines **************************************************/

/* cf_pred is End* */
static INLINE void
enter_procedure(ir_node *block, ir_node *cf_pred, int pos) {
  ir_node *callbegin;
  ir_graph *irg = get_irn_irg(cf_pred);

  assert(interprocedural_view);

  interprocedural_view = 0;
  callbegin = skip_Proj(intern_get_irn_n(block, 0));
  assert(intern_get_irn_op(callbegin) == op_CallBegin);
  interprocedural_view = 1;

  push_callsite(irg, callbegin);
  current_ir_graph = irg;
}

/* cf_pred is CallBegin */
static INLINE bool
leave_procedure(ir_node *block, ir_node *cf_pred, int pos) {
  ir_node *tos = get_top_of_callsite_stack(current_ir_graph);

  assert(get_irn_op(cf_pred) == op_CallBegin);

  if (tos == cf_pred) {
    /* We entered this procedure by the call pred pos refers to. */
    pop_callsite(current_ir_graph);
    current_ir_graph = get_CallBegin_irg(cf_pred);
    return true;
  } else {
    /* We won't walk. */
    return false;
  }
}

ir_node *get_irn_ip_pred(ir_node *n, int pos) {

  if (interprocedural_view) {

    /* Find the cf_pred refering to pos. */
    ir_node *block = n;
    ir_node *cf_pred;
    if (intern_get_irn_opcode(n) == iro_Filter) block = get_nodes_block(n);
    cf_pred = skip_Proj(intern_get_irn_n(block, pos));

    /* Check whether we enter or leave a procedure and act according. */
    if ((intern_get_irn_op(cf_pred) == op_EndReg) ||
	(intern_get_irn_op(cf_pred) == op_EndExcept))
      enter_procedure(block, cf_pred, pos);
    if (intern_get_irn_op(cf_pred) == op_CallBegin)
      if (!leave_procedure(block, cf_pred, pos)) return NULL;
  }

  return intern_get_irn_n(n, pos);
}

static INLINE void
re_enter_procedure(ir_node *block, ir_node *cf_pred, int pos) {
  ir_node *callbegin = pop_callsite(current_ir_graph);
  assert(interprocedural_view);
  current_ir_graph = get_CallBegin_irg(callbegin);
}

static INLINE void
re_leave_procedure(ir_node *block, ir_node *cf_pred, int pos) {
  ir_node *proj;
  ir_graph *callee;

  assert(get_irn_op(cf_pred) == op_CallBegin);

  /* Find the irg block is in. */
  proj = get_Block_cg_cfgpred(block, pos);
  assert(is_Proj(proj));
  callee = get_entity_irg(get_Call_callee(get_CallBegin_call(cf_pred),
					  get_Proj_proj(proj)));
  current_ir_graph = callee;
  push_callsite(callee, cf_pred);
}

void
return_recur(ir_node *n, int pos) {
  ir_node *block;
  ir_node *cf_pred;

  if (!interprocedural_view) return;

  /* Find the cf_pred refering to pos. */
  block = n;
  if (intern_get_irn_opcode(n) == iro_Filter) block = get_nodes_block(n);
  cf_pred = skip_Proj(intern_get_irn_n(block, pos));

  /* Check whether we re_enter or re_leave a procedure and act according. */
  if ((intern_get_irn_op(cf_pred) == op_EndReg) ||
      (intern_get_irn_op(cf_pred) == op_EndExcept))
    re_enter_procedure(block, cf_pred, pos);
  if (intern_get_irn_op(cf_pred) == op_CallBegin)
    re_leave_procedure(block, cf_pred, pos);
}
