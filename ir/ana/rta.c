/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana/rta.c
 * Purpose:     Intraprozedural analyses to improve the call graph estimate.
 * Author:      Florian
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * Intraprozedurale Analyse zur Abschätzung der Aufrufrelation. Es
 * die Menge der instantiierten Klassen bestimmt, und daraus existierende Methoden
 * bestimmt.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rta.h"

#include <stdlib.h>
#include "cgana.h"              /* get_implementation */

#include "irnode_t.h"
#include "irprog.h"

#include "eset.h"
/* #include "pmap.h" */
/* #include "array.h" */
#include "irgwalk.h"
/* #include "ircons.h" */
/* #include "irgmod.h" */
/* #include "irflag_t.h" */

/* #include "dbginfo_t.h" */

# define TRUE 1
# define FALSE 0

/* base data */
static eset *_live_classes   = NULL;
static eset *_live_fields    = NULL;
static eset *_called_methods = NULL;

/* cache computed results */
static eset *_live_graphs    = NULL;
static eset *_dead_graphs    = NULL;

/* now the meat */

static void rta_act (ir_node *node, void *env)
{
  opcode op = get_irn_opcode (node);

  if (iro_Call == op) {
    entity *ent = NULL;

    ir_node *ptr = get_Call_ptr (node);
    // TODO: test:  ptr.op == Const

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    }

    if (ent) {
      eset_insert (_called_methods, ent);
    }
  } else if (iro_Load  == op) {
    ir_node *ptr = get_Load_ptr (node);
    entity  *ent = NULL;

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    }
    if (ent) {
      eset_insert (_live_fields, ent);
    }
  } else  if (iro_Store == op) {
    ir_node *ptr = get_Store_ptr (node);
    entity  *ent = NULL;

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    }
    if (ent) {
      eset_insert (_live_fields, ent);
    }
  } else if (iro_Alloc == op) {
    type *type = get_Alloc_type (node);
    eset_insert (_live_classes, type);
  }
}

static void rta_fill_graph (ir_graph* graph)
{
  if (NULL != graph) {
    if (NULL != get_irg_end_block (graph)) {
      irg_walk (get_irg_end_block (graph), rta_act, NULL, NULL);
    }
  }
}

static void rta_fill_all ()
{
  int i;
  int old_ip_view = interprocedural_view;

  interprocedural_view = 0;
  for (i = 0; i < get_irp_n_irgs(); i++) {
    rta_fill_graph (get_irp_irg (i));
  }
  interprocedural_view = old_ip_view;
}

static int is_call_target (entity *method)
{
  int is_target = FALSE;
  int i;
  int n_over;

  /* The method could be the target of a polymorphic call if it is
     called or if it overrides a method that is called. */

  if (eset_contains (_called_methods, method)) {
    return (TRUE);
  }

  n_over = get_entity_n_overwrittenby (method);
  for (i = 0; !is_target && (i < n_over); i ++) {
    entity *over = get_entity_overwrittenby (method, i);
    is_target |= is_call_target (over);
  }

  return (is_target);
}


static ir_graph *get_implementing_graph (entity *method)
{
  ir_graph *graph = get_entity_irg (method);

  if (NULL == graph) {
    int i;
    int n_over = get_entity_n_overwrites (method);

    for (i = 0; (NULL == graph) && (i < n_over); i ++) {
      entity *over = get_entity_overwrites (method, i);
      graph = get_implementing_graph (over);
    }
  }

  assert (graph && "no graph");

  return (graph);
}

static int has_live_call (entity *method, ir_graph *graph)
{
  int has_call = FALSE;
  int i, n_over;

  if (get_irg_ent (graph) != method) {
    return (FALSE);
  }

  if (is_call_target (method)) {
    return (TRUE);
  }

  n_over = get_entity_n_overwrittenby (method);
  for (i = 0; !has_call && (i < n_over); i ++) {
    entity *over = get_entity_overwrittenby(method, i);
    has_call |= has_live_call (over, graph);
  }

  return (has_call);
}

static int has_graph (type *clazz, ir_graph *graph)
{
  int has_the_graph = FALSE;
  int i;
  int n_sub;

  if (eset_contains (_live_classes, clazz)) {
    int n_meth = get_class_n_members (clazz);

    for (i = 0; i < n_meth; i ++) {
      entity *member = get_class_member (clazz, i);
      if (is_method_type (get_entity_type (member))) {
        ir_graph *impl = get_entity_irg (member);

        if (impl == graph) {
          return (TRUE);
        }
      } /* is_method */
    } /* all methods */
  } /* _live_classes.contains (clazz) */

  n_sub = get_class_n_subtypes (clazz);
  for (i = 0; !has_the_graph && (i < n_sub); i ++) {
    type *sub = get_class_subtype (clazz, i);

    has_the_graph |= has_graph (sub, graph);
  }

  return (has_the_graph);
}

static int has_live_class (entity *method, ir_graph *graph)
{
  int has_class = FALSE;
  int i;
  int n_over;
  type *clazz;

  if (get_implementing_graph (method) != graph) {
    return (FALSE);
  }

  clazz = get_entity_owner (method);
  if (has_graph (clazz, graph)) {
    return (TRUE);
  }

  n_over = get_entity_n_overwrittenby (method);
  for (i = 0; !has_class && (i < n_over); i ++) {
    entity *over = get_entity_overwrittenby (method, i);
    has_class |= has_live_class (over, graph);
  }

  return (has_class);
}

static int rta_check (ir_graph *graph)
{
  return (rta_is_alive_graph (graph));
}


void rta_init ()
{
  _live_classes   = eset_create ();
  _live_fields    = eset_create ();
  _called_methods = eset_create ();

  _live_graphs = eset_create ();
  _dead_graphs = eset_create ();

  /*
   * shold be:
   * rta_fill_queue ()
   */
  if (get_irp_main_irg ()) {
    eset_insert (_live_graphs, get_irp_main_irg ());
  }

  rta_fill_all ();
}

void rta_cleanup ()
{
  int i;
  int n_live_graphs = 0;
  int n_graphs = get_irp_n_irgs();

  for (i = 0; i < n_graphs; i++) {
    ir_graph *graph = get_irp_irg(i);

    if (rta_check (graph)) {
      char *name = NULL;

      n_live_graphs ++;
      name = get_entity_name (get_irg_ent (graph));

      fprintf (stdout, "LIVE  %s\n", name);
    }
  }

  fprintf (stdout, "RES   %s: %i graphs, %i live\n", __FUNCTION__, n_graphs, n_live_graphs);

  if (_live_classes) {
    eset_destroy (_live_classes);
    _live_classes = NULL;
  }

  if (_live_fields) {
    eset_destroy (_live_fields);
    _live_fields = NULL;
  }

  if (_called_methods) {
    eset_destroy (_called_methods);
    _called_methods = NULL;
  }

  if (_live_graphs) {
    eset_destroy (_live_graphs);
    _live_graphs = NULL;
  }

  if (_dead_graphs) {
    eset_destroy (_dead_graphs);
    _dead_graphs = NULL;
  }
}

int  rta_is_alive_class  (type   *clazz)
{
  return (eset_contains (_live_classes, clazz));
}

int  rta_is_alive_graph (ir_graph *graph)
{
  if (eset_contains (_live_graphs, graph)) {
    return (TRUE);
  }

  if (eset_contains (_dead_graphs, graph)) {
    return (FALSE);
  }

  entity *meth = get_irg_ent (graph);

  if (has_live_call (meth, graph) && has_live_class (meth, graph)) {
    eset_insert (_live_graphs, graph);

    return (TRUE);
  } else {
    eset_insert (_dead_graphs, graph);

    return (FALSE);
  }
}

int  rta_is_alive_field  (entity *field)
{
  return (eset_contains (_live_fields, field));
}



/*
 * $Log$
 * Revision 1.3  2004/06/12 17:09:46  liekweg
 * RTA works, outedges breaks.  "Yay." --flo
 *
 * Revision 1.2  2004/06/11 18:25:39  liekweg
 * Added todo
 *
 * Revision 1.1  2004/06/11 18:24:18  liekweg
 * Added RTA --flo
 *
 */
