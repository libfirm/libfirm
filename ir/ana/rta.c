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

static void init_tables ()
{
  _live_classes   = eset_create ();
  _live_fields    = eset_create ();
  _called_methods = eset_create ();

  _live_graphs = eset_create ();
  _dead_graphs = eset_create ();

  if (get_irp_main_irg ()) {
    eset_insert (_live_graphs, get_irp_main_irg ());
  }

  if (get_glob_type ()) {
    eset_insert (_live_classes, get_glob_type ());
  }

}

/**
   Enter all method and field accesses and all class allocations into our tables.
*/
static void rta_act (ir_node *node, void *env)
{
  opcode op = get_irn_opcode (node);

  if (iro_Call == op) {
    entity *ent = NULL;

    ir_node *ptr = get_Call_ptr (node);

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    } else if (iro_Const == get_irn_opcode (ptr)) {
      ent = get_tarval_entity (get_Const_tarval (ptr));
    }

    /* assert (ent); */

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

/**
Traverse the given graph to collect method and field accesses and object allocations.
*/
static void rta_fill_graph (ir_graph* graph)
{
  if (NULL != graph) {
    if (NULL != get_irg_end_block (graph)) {
      irg_walk (get_irg_end_block (graph), rta_act, NULL, NULL);
    }
  }
}

static int is_alive (ir_graph *graph, eset *live_graphs, eset *dead_graphs)
{
  if (eset_contains (live_graphs, graph)) {
    return (TRUE);
  }

  if (eset_contains (dead_graphs, graph)) {
    return (FALSE);
  }

  assert (0 && "what's up");
}

/**
   Traverse all graphs to collect method and field accesses and object allocations.

   @param rerun Whether to rely on is_alive in a second run
*/
static void rta_fill_all (int rerun)
{
  int i;
  int old_ip_view = interprocedural_view;
  eset *live_graphs = NULL;
  eset *dead_graphs = NULL;

  interprocedural_view = 0;

  if (rerun) {
    int i;
    int n_graphs = get_irp_n_irgs ();

    /* force all graphs to be entered in either live_graphs or dead_graphs */
    for (i = 0; i < n_graphs; i ++) {
      rta_is_alive_graph (get_irp_irg (i));
    }

    /* remember existing infos for later */
    live_graphs = _live_graphs;
    dead_graphs = _dead_graphs;

    /* ensure that live_graphs and dead_graphs aren't deallocated by rta_cleanup */
    _live_graphs = NULL;
    _dead_graphs = NULL;

    rta_cleanup ();
    init_tables ();
  }

  /* consider all graphs, possibly taking into account existing infos */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *graph = get_irp_irg (i);

    if (!rerun || is_alive (graph, live_graphs, dead_graphs)) {
      rta_fill_graph (graph);
    }
  }

  if (rerun) {
    /* clean up the tables that we have retained */
    eset_destroy (live_graphs);
    eset_destroy (dead_graphs);
  }

  interprocedural_view = old_ip_view;
}

/**
   Find out whether the given method could be the target of a
   polymorphic call.
*/
static int is_call_target (const entity *method)
{
  int is_target = FALSE;
  int i;
  int n_over;

  /* The method could be the target of a polymorphic call if it is
     called or if it overrides a method that is called. */

  if (eset_contains (_called_methods, (entity*) method)) {
    return (TRUE);
  }

  /* not called?  check methods in superclass(es) */
  n_over = get_entity_n_overwrittenby ((entity*) method);
  for (i = 0; !is_target && (i < n_over); i ++) {
    entity *over = get_entity_overwrittenby ((entity*) method, i);
    is_target |= is_call_target (over);
  }

  return (is_target);
}

/**
   Given a method, find the firm graph that implements that method.
*/
static ir_graph *get_implementing_graph (const entity *method)
{
  ir_graph *graph = get_entity_irg ((entity*) method);

  if (NULL == graph) {
    int i;
    int n_over = get_entity_n_overwrites ((entity*) method);

    for (i = 0; (NULL == graph) && (i < n_over); i ++) {
      entity *over = get_entity_overwrites ((entity*) method, i);
      graph = get_implementing_graph (over);
    }
  }

  assert (graph && "no graph");

  return (graph);
}

/**
   Determine whether the give method or one of its overwriter have
   been used in a call to the given graph.
*/
static int has_live_call (entity *method, ir_graph *graph)
{
  int has_call = FALSE;
  int i, n_over;

  /* stop searching if a overwriting method comes with a new graph */
  if (get_irg_ent (graph) != method) {
    return (FALSE);
  }

  /* maybe we're called (possibly through polymorphy)? */
  if (is_call_target (method)) {
    return (TRUE);
  }

  /* maybe we're overwritten by a method that is called? */
  n_over = get_entity_n_overwrittenby ((entity*) method);
  for (i = 0; !has_call && (i < n_over); i ++) {
    entity *over = get_entity_overwrittenby((entity*) method, i);
    has_call |= has_live_call (over, graph);
  }

  return (has_call);
}

/**
   Determine whether the given class is live *and* uses the given
   graph at some point.
*/
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

  /* our subclasses might be using that graph, no? */
  n_sub = get_class_n_subtypes (clazz);
  for (i = 0; !has_the_graph && (i < n_sub); i ++) {
    type *sub = get_class_subtype (clazz, i);

    has_the_graph |= has_graph (sub, graph);
  }

  return (has_the_graph);
}

/**
   Determine wether the given method could be used in a call to the
   given graph on a live class.
*/
static int has_live_class (entity *method, ir_graph *graph)
{
  int has_class = FALSE;
  int i;
  int n_over;
  type *clazz;

  /* const char *name = get_entity_name (method); */

  /* stop searching when an overwriting method provides a new graph */
  if (get_implementing_graph (method) != graph) {
    return (FALSE);
  }

  clazz = get_entity_owner (method);

  if (has_graph (clazz, graph)) { /* this also checks whether clazz is live*/
    return (TRUE);
  }

  n_over = get_entity_n_overwrittenby (method);
  for (i = 0; !has_class && (i < n_over); i ++) {
    entity *over = get_entity_overwrittenby (method, i);
    has_class |= has_live_class (over, graph);
  }

  return (has_class);
}

static int stats ()
{
  int i;
  int n_live_graphs = 0;
  int n_graphs = get_irp_n_irgs();

  for (i = 0; i < n_graphs; i++) {
    ir_graph *graph = get_irp_irg(i);

    if (rta_is_alive_graph (graph)) {
      n_live_graphs ++;
    }
  }

  return (n_live_graphs);
}

void rta_init (int verbose)
{
  int n_live_graphs = get_irp_n_irgs ();
  int n_graphs = 0;
  int n_runs = 0;

  init_tables ();

  printf ("RTA: run %i (%i graphs to go)\n", n_runs, n_live_graphs);
  rta_fill_all (FALSE);

  while (n_graphs != n_live_graphs) {
    n_graphs = n_live_graphs;
    n_runs ++;
    rta_fill_all (TRUE);
    n_live_graphs = stats ();
    if (verbose) {
      printf ("RTA: run %i (%i graphs to go)\n", n_runs, n_live_graphs);
    }
  }

  if (verbose) {
    printf ("RTA: n_graphs      = %i\n", get_irp_n_irgs ());
    printf ("RTA: n_live_graphs = %i\n", n_live_graphs);
    printf ("RTA: n_runs        = %i\n", n_runs);
  }
}

void rta_cleanup ()
{
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
 * Revision 1.5  2004/06/13 15:03:45  liekweg
 * RTA auf Iterative RTA aufgebohrt --flo
 *
 * Revision 1.4  2004/06/12 19:35:04  liekweg
 * Kommentare eingef"ugt --flo
 *
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
