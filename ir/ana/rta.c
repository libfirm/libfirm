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

#include "irnode_t.h"
#include "irprog.h"

#include "eset.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "trvrfy.h"

# define TRUE 1
# define FALSE 0

/* base data */
static eset *_live_classes   = NULL;

/* cache computed results */
static eset *_live_graphs    = NULL;

static int whole_world = 0;
static int verbose     = 0;

/**
   Given a method, find the firm graph that implements that method.
*/
static ir_graph *get_implementing_graph (entity *method)
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

  /* we *must* always return a graph != NULL, *except* when we're used
     inside remove_irg or force_description */
  /* assert (graph && "no graph"); */

  return (graph);
}

static int add_graph (ir_graph *graph)
{
  if (!eset_contains (_live_graphs, graph)) {
    if (verbose > 1) {
      fprintf(stdout, "RTA:        new graph of ");
      DDMEO(get_irg_ent (graph));
    }

    eset_insert (_live_graphs, graph);
    return (TRUE);
  }

  return (FALSE);
}

static int add_class (type *clazz)
{
  if (!eset_contains (_live_classes, clazz)) {
  if (verbose > 1) {
    fprintf(stdout, "RTA:        new class: ");
    DDMT(clazz);
  }

    eset_insert (_live_classes, clazz);
    return (TRUE);
  }

  return (FALSE);
}

/**
   Given an entity, add all implementing graphs that belong to live classes to _live_graphs.

   Iff additions occurred, return TRUE, else FALSE.
*/
static int add_implementing_graphs (entity *method)
{
  int i;
  int n_over = get_entity_n_overwrittenby (method);
  ir_graph *graph = get_entity_irg (method);
  int change = FALSE;

  if (NULL == graph) {
    graph = get_implementing_graph (method);
  }

  if (verbose > 1) {
    fprintf(stdout, "RTA:        new call to ");
    DDMEO(method);
  }

  if (rta_is_alive_class (get_entity_owner (method))) {
    if (NULL != graph) {
      change = add_graph (graph);
    }
  }

  for (i = 0; i < n_over; i ++) {
    entity *over = get_entity_overwrittenby (method, i);
    change |= add_implementing_graphs (over);
  }

  return (change);
}

/**
   Enter all method accesses and all class allocations into
   our tables.

   Set *(int*)env to true iff (possibly) new graphs have been found.
*/
static void rta_act (ir_node *node, void *env)
{
  int *change = (int*) env;

  opcode op = get_irn_opcode (node);

  if (iro_Call == op) {         /* CALL */
    entity *ent = NULL;

    ir_node *ptr = get_Call_ptr (node);

    if (iro_Sel == get_irn_opcode (ptr)) { /* CALL SEL */
      ent = get_Sel_entity (ptr);
      *change = add_implementing_graphs (ent);
    } else if (iro_Const == get_irn_opcode (ptr)) { /* CALL CONST */
      ent = get_tarval_entity (get_Const_tarval (ptr));
      ir_graph *graph = get_entity_irg (ent);
      /* don't use get_implementing_graph on a Const! */
      if (graph) {
        *change = add_graph (graph);
      } else {
        /* it's an externally allocated thing. */
      }
    } else if (iro_SymConst == get_irn_opcode (ptr)) { /* CALL SYMCONST */
      /* If this SymConst refers to a method the method is external_visible
         and therefore must be considered live anyways. */
      /* assert (ent && "couldn't determine entity of call to symConst"); */
    }
  } else if (iro_Alloc == op) { /* ALLOC */
    type *type = get_Alloc_type (node);

    *change = add_class (type);
  }
}

/**
   Traverse the given graph to collect method accesses and
   object allocations.
*/
static int rta_fill_graph (ir_graph* graph)
{
  int change = FALSE;

  current_ir_graph = graph;

  irg_walk (get_irg_end (graph), rta_act, NULL, &change);

  return (change);
}

/**
   Traverse all graphs to collect method accesses and object allocations.

   @param rerun Whether to rely on is_alive in a second run
*/
static int rta_fill_incremental (void)
{
  int i;
  int n_runs = 0;
  int rerun  = TRUE;
  int old_ip_view = interprocedural_view;

  interprocedural_view = 0;     /* save this for later */

  /* init_tables has added main_irg to _live_graphs */

  /* Need to take care of graphs that are externally
     visible. Pretend that they are called: */

  if (! whole_world) {
    for (i = 0; i < get_irp_n_irgs(); i++) {
      ir_graph *graph = get_irp_irg (i);

      entity *ent = get_irg_entity (graph);
      if (visibility_external_visible == get_entity_visibility (ent)) {

        eset_insert (_live_graphs, graph);

        /* eset_insert (_live_classes, get_entity_owner (ent)); */
      }
    }
  }

  while (rerun) {
    ir_graph *graph;

    /* start off new */
    eset *live_graphs = _live_graphs;
    _live_graphs = eset_create ();

    if (verbose > 1) {
      fprintf(stdout, "RTA: RUN %i\n", n_runs);
    }

    /* collect what we have found previously */
    eset_insert_all (_live_graphs, live_graphs);

    rerun = FALSE;

    for (graph = eset_first (live_graphs);
         graph;
         graph = eset_next (live_graphs)) {

      if (verbose > 1) {
        fprintf(stdout, "RTA: RUN %i: considering graph of ", n_runs);
        DDMEO(get_irg_ent (graph));
      }

      rerun |= rta_fill_graph (graph);
    }

    eset_destroy (live_graphs);

    n_runs ++;
  }

  interprocedural_view = old_ip_view; /* cover up our traces */

  return (n_runs);
}

/*
   Count the number of graphs that we have found to be live.
*/
static int stats (void)
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

/* remove a graph, part I */
/*
   We removed the first graph to implement the entity, so we're
   abstract now.  Pretend that it wasn't there at all, and every
   entity that used to inherit this entity's graph is now abstract.
*/
/* Since we *know* that this entity will not be called, this is OK. */
static void force_description (entity *ent, entity *addr)
{
  int i, n_over = get_entity_n_overwrittenby (ent);

  set_entity_peculiarity (ent, peculiarity_description);

  for (i = 0; i < n_over; i ++) {
    entity *over = get_entity_overwrittenby (ent, i);

    if (peculiarity_inherited == get_entity_peculiarity (over)) {
      /* We rely on the fact that cse is performed on the const_code_irg. */
      entity *my_addr =
        tarval_to_entity(get_Const_tarval(get_atomic_ent_value(over)));

      if (addr == my_addr) {
        force_description (over, addr);
      }
    } else if (peculiarity_existent == get_entity_peculiarity (over)) {
      /* check whether 'over' forces 'inheritance' of *our* graph: */
      ir_node *f_addr = get_atomic_ent_value (over);
      entity *impl_ent = tarval_to_entity (get_Const_tarval (f_addr));

      assert ((get_irn_op(f_addr) == op_Const) && "can't do complex addrs");
      if (impl_ent == addr) {
        assert (0 && "gibt's denn sowas");
        force_description (over, addr);
      }
    }
  }
}

/* remove a graph, part II */
/*
   Note: get_implementing_graph is not well defined here (graph->ent
   could overwrite more than one superclass implementation (graph).
   Since we *know* that this entity will not be called, this is OK.
*/
static void remove_irg (ir_graph *graph)
{
  entity *ent = get_irg_entity (graph);

/*   DDMEO (get_irg_ent(graph)); */

  /* delete the ir_graph data */
  remove_irp_irg (graph);
  /* remove reference to the graph */
  set_entity_irg (ent, NULL);
  /* find the implementation (graph) from *some* superclass: */
  graph = get_implementing_graph (ent);

  if (TRUE || (NULL == graph)) { /* always pretend to be 'abstract'; let the others figure this out */
    /* nothing to inherit!  pretend we're abstract */
    force_description (ent, ent);
  } else {
    /* pretend that we inherit the implementation (graph) from some superclass: */
    set_entity_peculiarity (ent, peculiarity_inherited);

    exchange (get_atomic_ent_value (ent),
              get_atomic_ent_value (get_irg_ent(graph)));
  }
}

/**
   Initialise the static data structures.
*/
static void init_tables (void)
{
  _live_classes   = eset_create ();

  _live_graphs = eset_create ();

  if (get_irp_main_irg ()) {
    eset_insert (_live_graphs, get_irp_main_irg ());
  }

  /*
  if (get_glob_type ()) {
    eset_insert (_live_classes, get_glob_type ());
  }
  */
}

/* Initialise the RTA data structures, and perform RTA.
   @param   do_verbose Iff != 0, print statistics as we go along
   @param   do_whole_world Iff != 0, assume whole-world
*/
void rta_init (int do_verbose, int do_whole_world)
{
  int n_live_graphs = 0;
  int n_runs = 0;

# ifdef DEBUG_libfirm
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++) {
    irg_vrfy (get_irp_irg(i));
  }
  tr_vrfy ();
# endif /* defined DEBUG_libfirm */

  whole_world = do_whole_world;
  verbose = do_verbose;

  init_tables ();

  n_runs = rta_fill_incremental ();

  n_live_graphs = stats ();

  if (verbose) {
    if (whole_world) {
      printf ("RTA: whole-world assumption\n");
    }
    printf ("RTA: n_graphs      = %i\n", get_irp_n_irgs ());
    printf ("RTA: n_live_graphs = %i\n", n_live_graphs);
    printf ("RTA: n_runs        = %i\n", n_runs);
  }

# ifdef DEBUG_libfirm
  for (i = 0; i < get_irp_n_irgs(); i++) {
    irg_vrfy (get_irp_irg(i));
  }
  tr_vrfy ();
# endif /* defined DEBUG_libfirm */
}

/* Delete all graphs that we have found to be dead from the program */
void rta_delete_dead_graphs (void)
{
  int i;
  int n_graphs = get_irp_n_irgs ();
  ir_graph *graph = NULL;

  eset *dead_graphs = eset_create ();

  for (i = 0; i < n_graphs; i++) {
    graph = get_irp_irg(i);

    if (rta_is_alive_graph (graph)) {
      /* do nothing (except some debugging fprintf`s :-) */
    } else {
# ifdef DEBUG_libfirm
      entity *ent = get_irg_entity (graph);
      assert (whole_world ||
              (visibility_external_visible != get_entity_visibility (ent)));
# endif /* defined DEBUG_libfirm */

      eset_insert (dead_graphs, graph);
    }
  }

  /* now delete the graphs. */
  for (graph = eset_first (dead_graphs);
       graph;
       graph = (ir_graph*) eset_next (dead_graphs)) {

    if (verbose) {
      fprintf(stdout, "RTA: removing graph of ");
      DDMEO(get_irg_ent (graph));
    }

    remove_irg (graph);
  }
}

/* Clean up the RTA data structures.  Call this after calling rta_init */
void rta_cleanup (void)
{
# ifdef DEBUG_libfirm
  int i;
    for (i = 0; i < get_irp_n_irgs(); i++) {
      irg_vrfy (get_irp_irg(i));
    }
    tr_vrfy ();
# endif /* defined DEBUG_libfirm */

  if (_live_classes) {
    eset_destroy (_live_classes);
    _live_classes = NULL;
  }

  if (_live_graphs) {
    eset_destroy (_live_graphs);
    _live_graphs = NULL;
  }
}

/* Say whether this class might be instantiated at any point in the program: */
int  rta_is_alive_class  (type   *clazz)
{
  return (eset_contains (_live_classes, clazz));
}

/* Say whether this graph might be run at any time in the program: */
int  rta_is_alive_graph (ir_graph *graph)
{
  if (eset_contains (_live_graphs, graph)) {
    return (TRUE);
  }

  return (FALSE);
}

/* dump our opinion */
void rta_report (void)
{
  int i;

  for (i = 0; i < get_irp_n_types(); ++i) {
    type *tp = get_irp_type(i);
    if (is_class_type(tp) && rta_is_alive_class(tp)) {
      fprintf(stdout, "RTA: considered allocated: "); DDMT(tp);
    }
  }

  for (i = 0; i < get_irp_n_irgs(); i++) {
    if (rta_is_alive_graph (get_irp_irg(i))) {
      fprintf(stdout, "RTA: considered called: graph of ");
      DDMEO(get_irg_ent (get_irp_irg(i)));
    }
  }
}


/*
 * $Log$
 * Revision 1.15  2004/06/18 15:47:19  liekweg
 * minor bug fix (go forward, not backward) --flo
 *
 * Revision 1.14  2004/06/18 13:12:43  liekweg
 * final bug fix (calls via consts)
 *
 * Revision 1.13  2004/06/17 16:34:33  liekweg
 * removed DD*s
 *
 * Revision 1.12  2004/06/17 16:33:33  liekweg
 * minor bug fix
 *
 * Revision 1.11  2004/06/17 14:21:13  liekweg
 * major bugfix
 *
 * Revision 1.10  2004/06/17 10:31:41  goetz
 * irscc: bugfix, can now deal with loops not reachable from start
 * cgana: bugfix, skip_Tuple
 * rta: improved
 *
 * Revision 1.9  2004/06/17 08:56:03  liekweg
 * Fixed typos in comments
 *
 * Revision 1.8  2004/06/17 08:33:01  liekweg
 * Added comments; added remove_irg
 *
 * Revision 1.6  2004/06/14 13:02:03  goetz
 * bugfixesbug
 *
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
