/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana/rta.c
 * Purpose:     Interprocedural analysis to improve the call graph estimate.
 * Author:      Florian
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "rta.h"

#include <stdlib.h>

#include "irnode_t.h"
#include "irprog_t.h"
#include "irgraph_t.h"

#include "eset.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "trvrfy.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

/* flags */
static int verbose     = 0;


/* base data */
static eset *_live_classes   = NULL;

/* cache computed results */
static eset *_live_graphs    = NULL;

/**
   Given a method, find the firm graph that implements that method.
*/
static ir_graph *get_implementing_graph (entity *method)
{
#if 0
  ir_graph *graph = get_entity_irg ((entity*) method);

  /* Search upwards in the overwrites graph. */
  /* GL: this will not work for multiple inheritance */
  if (NULL == graph) {
    int i;
    int n_over = get_entity_n_overwrites ((entity*) method);

    for (i = 0; (NULL == graph) && (i < n_over); i ++) {
      entity *over = get_entity_overwrites ((entity*) method, i);
      graph = get_implementing_graph (over);
    }
  }

  /* GL   this is not valid in our remove irg algorithm ... which we removed by now ...  */
  assert(get_entity_peculiarity(method) == peculiarity_description
     || graph == get_entity_irg(get_SymConst_entity(get_atomic_ent_value(method))));

  /* we *must* always return a graph != NULL, *except* when we're used
     inside remove_irg or force_description */
  /* assert (graph && "no graph"); */

  return (graph);
#else
  ir_graph *graph = NULL;

  if (get_entity_peculiarity(method) != peculiarity_description)
    graph = get_entity_irg(get_SymConst_entity(get_atomic_ent_value(method)));

  return graph;
#endif
}

static int add_graph (ir_graph *graph)
{
  if (!eset_contains (_live_graphs, graph)) {
    if (verbose > 1) {
      fprintf(stdout, "RTA:        new graph of ");
      DDMEO(get_irg_entity (graph));
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

/** Given an entity, add all implementing graphs that belong to live classes
 *  to _live_graphs.
 *
 *  Iff additions occurred, return TRUE, else FALSE.
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

/** Enter all method accesses and all class allocations into
 *  our tables.
 *
 *  Set *(int*)env to true iff (possibly) new graphs have been found.
 */
static void rta_act (ir_node *node, void *env)
{
  int *change = (int*) env;

  opcode op = get_irn_opcode (node);

  if (iro_Call == op) {         /* CALL */
    entity *ent = NULL;

    ir_node *ptr = get_Call_ptr (node);

    /* CALL SEL */
    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
      *change |= add_implementing_graphs (ent);

      /* CALL SYMCONST */
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind(ptr) == symconst_addr_ent) {
        ir_graph *graph;

        ent = get_SymConst_entity (ptr);
        graph = get_entity_irg (ent);
        if (graph) {
          *change |= add_graph (graph);
        } else {
          /* it's an external allocated thing. */
        }
      } else if (get_SymConst_kind(ptr) == symconst_addr_name) {
	    /* Entities of kind addr_name may not be allocated in this compilation unit.
	       If so, the frontend built faulty Firm.  So just ignore. */
	    /* if (get_SymConst_name(ptr) != new_id_from_str("iro_Catch"))
        assert (ent && "couldn't determine entity of call to SymConst of kind addr_name."); */
      } else {
        /* other symconst. */
        assert(0 && "This SymConst can not be an address for a method call.");
      }

      /* STRANGE */
    } else {
      DDMN(ptr);
      assert(0 && "Unexpected address expression: can not analyse, therefore can not do correct rta!");
    }

  } else if (iro_Alloc == op) { /* ALLOC */
    type *type = get_Alloc_type (node);

    *change |= add_class (type);
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

  irg_walk_graph (graph, rta_act, NULL, &change);

  return (change);
}

/** Traverse all graphs to collect method accesses and object allocations.
 *
 *  @param rerun Whether to rely on is_alive in a second run
 */
static int rta_fill_incremental (void)
{
  int i;
  int n_runs = 0;
  int rerun  = TRUE;
  int old_ip_view = get_interprocedural_view();

  set_interprocedural_view(false);     /* save this for later */

  /* init_tables has added main_irg to _live_graphs */

  /* Need to take care of graphs that are externally
     visible or sticky. Pretend that they are called: */

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *graph = get_irp_irg (i);
    entity *ent = get_irg_entity (graph);

    if ((visibility_external_visible == get_entity_visibility (ent)) ||
        (stickyness_sticky == get_entity_stickyness (ent))) {
      eset_insert (_live_graphs, graph);
      // printf("external visible: "); DDMG(graph);
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
        DDMEO(get_irg_entity (graph));
      }

      rerun |= rta_fill_graph (graph);
    }

    eset_destroy (live_graphs);

    n_runs ++;
  }

  set_interprocedural_view(old_ip_view); /* cover up our traces */

  return (n_runs);
}

/**
 * Count the number of graphs that we have found to be live.
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
      entity *my_addr = get_SymConst_entity(get_atomic_ent_value(over));

      if (addr == my_addr) {
        force_description (over, addr);
      }
    } else if (peculiarity_existent == get_entity_peculiarity (over)) {
      /* check whether 'over' forces 'inheritance' of *our* graph: */
      ir_node *f_addr = get_atomic_ent_value (over);
      entity *impl_ent = get_SymConst_entity (f_addr);

      assert ((get_irn_op(f_addr) == op_SymConst) && "can't do complex addrs");
      if (impl_ent == addr) {
        assert (0 && "gibt's denn sowas");
        force_description (over, addr);
      }
    }
  }
}

/**
   Initialize the static data structures.
*/
static void init_tables (void)
{
  int i, n_globs = get_class_n_members(get_glob_type());

  _live_classes = eset_create ();
  _live_graphs  = eset_create ();

  if (get_irp_main_irg ()) {
    eset_insert (_live_graphs, get_irp_main_irg ());
  }

  /* Find static allocated classes */
  for (i = 0; i < n_globs; ++i) {
    type *member_type = get_entity_type(get_class_member(get_glob_type(), i));
    if (is_class_type(member_type))
      eset_insert(_live_classes, member_type);
  }
}

/*
 * Initialize the RTA data structures, and perform RTA.
 * do_verbose If == 1, print statistics, if > 1, chatter about every detail
 */
void rta_init (int do_verbose)
{
  int i, n_runs = 0;

  int rem_vpi = get_visit_pseudo_irgs();
  set_visit_pseudo_irgs(1);

# ifdef DEBUG_libfirm
  for (i = 0; i < get_irp_n_irgs(); i++) {
    irg_vrfy (get_irp_irg(i));
  }
  tr_vrfy ();
# endif /* defined DEBUG_libfirm */

  verbose = do_verbose;

  init_tables ();

  n_runs = rta_fill_incremental ();

  if (verbose) {
    int n_live_graphs = stats ();

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

  set_visit_pseudo_irgs(rem_vpi);
}

/**
 * walker for all types and entities
 *
 * Changes the peculiarity of entities that represents
 * dead graphs to peculiarity_description.
 */
static void make_entity_to_description(type_or_ent *tore, void *env) {
  if (get_kind(tore) == k_entity) {
    entity *ent = (entity *)tore;

    if ((is_method_type(get_entity_type(ent)))                        &&
        (get_entity_peculiarity(ent) != peculiarity_description)      &&
        (get_entity_visibility(ent)  != visibility_external_allocated)   ) {
      ir_graph *irg = get_entity_irg(get_SymConst_entity(get_atomic_ent_value(ent)));
      if (!eset_contains (_live_graphs, irg)) {
        set_entity_peculiarity(ent, peculiarity_description);
        set_entity_irg(ent, NULL);
      }
    }
  }
}

/* Delete all graphs that we have found to be dead from the program
   If verbose == 1, print statistics, if > 1, chatter about every detail
*/
void rta_delete_dead_graphs (void)
{
  int i;
  int n_graphs = get_irp_n_irgs ();
  ir_graph *graph = NULL;
  int n_dead_graphs = 0;
  ir_graph **dead_graphs;

  int rem_vpi = get_visit_pseudo_irgs();
  set_visit_pseudo_irgs(1);

  if (!get_optimize() || !get_opt_dead_method_elimination()) return;

  dead_graphs = xmalloc(sizeof(*dead_graphs) * get_irp_n_irgs());

  for (i = 0; i < n_graphs; i++) {
    graph = get_irp_irg(i);

    if (rta_is_alive_graph (graph)) {
      /* do nothing (except some debugging fprintf`s :-) */
    } else {
# ifdef DEBUG_libfirm
      entity *ent = get_irg_entity (graph);
      assert (visibility_external_visible != get_entity_visibility (ent));
# endif /* defined DEBUG_libfirm */

      dead_graphs[n_dead_graphs] = graph;
      n_dead_graphs ++;
    }
  }

  type_walk(make_entity_to_description, NULL, NULL);
  for (i = 0; i < n_dead_graphs; ++i) {
    remove_irp_irg (dead_graphs[i]);
  }

  if (verbose) {
    printf ("RTA: n_dead_graphs = %i\n", n_dead_graphs);
  }

  set_visit_pseudo_irgs(rem_vpi);

  free(dead_graphs);
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
  return eset_contains (_live_graphs, graph);
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
      DDMEO(get_irg_entity (get_irp_irg(i)));
    }
  }
}


/*
 * $Log$
 * Revision 1.31  2004/12/21 13:45:14  beck
 * removed C99 constructs
 *
 * Revision 1.30  2004/12/02 16:16:11  beck
 * fixed config.h include
 * used xmalloc instead of malloc
 *
 * Revision 1.29  2004/11/11 13:28:08  goetz
 * made pseudo irg aware
 *
 * Revision 1.28  2004/11/03 14:47:18  beck
 * removed gloval intraprocedural_view variable and replaced by get_*() set_*() functions
 *
 * Revision 1.27  2004/10/21 07:23:34  goetz
 * comments
 *
 * Revision 1.26  2004/10/20 14:59:27  liekweg
 * Removed ecg
 *
 * Revision 1.25  2004/10/18 12:47:08  liekweg
 * avoid warning
 *
 * Revision 1.24  2004/09/24 13:59:04  beck
 * fixed doxygen comments, removed initialization for description entities
 *
 * Revision 1.23  2004/08/19 16:51:02  goetz
 * fixed some errors, pushed closer to inteded firm semantics
 *
 * Revision 1.22  2004/08/13 08:57:15  beyhan
 * normalized names of functions, enums ...
 * added suffix as argument to dumpers, removed global variable
 * removed support for tarval/Const
 *
 * Revision 1.21  2004/07/08 15:50:56  goetz
 * firmstat added
 *
 * Revision 1.20  2004/07/08 11:17:40  goetz
 * *** empty log message ***
 *
 * Revision 1.19  2004/07/06 12:30:37  beyhan
 * new SymConst semantics
 *
 * Revision 1.18  2004/06/27 21:17:41  liekweg
 * Added comment
 *
 * Revision 1.17  2004/06/25 13:45:13  liekweg
 * observe stickyness; minor refactoring
 *
 * Revision 1.16  2004/06/24 06:42:14  goetz
 * test of firm flags
 *
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
