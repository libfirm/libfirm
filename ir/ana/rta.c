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

#include "rta.h"

# define TRUE 1
# define FALSE 0

/* # define RTA_SET */
# ifdef RTA_SET
typedef struct rta_set_elt
{
  struct rta_set_elt *_next;
  void *_obj;
} rta_set_elt_t;

typedef struct rta_set
{
  rta_set_elt_t *_list;
} rta_set_t;

#  define SET_T rta_set_t

# else /* if defined RTA_SET */
#  define SET_T eset
#  define new_set eset_create
#  define delete_set(SET)       eset_destroy(SET)
#  define set_insert(SET,ELT)   eset_insert(SET,ELT)
#  define set_contains(SET,ELT) eset_contains(SET,ELT)
# endif /* defined RTA_SET */

static SET_T *_live_classes   = NULL;
static SET_T *_live_fields    = NULL;
static SET_T *_called_methods = NULL;

/* cache computed results */
static SET_T *_live_graphs    = NULL;
static SET_T *_dead_graphs    = NULL;

# ifdef RTA_SET
/* Reinvent the wheel, err, set. */
/* eset uses obstacks, which fucks up the graph somehow */
static rta_set_t *new_set ()
{
  rta_set_t *set = (rta_set_t*) xmalloc (sizeof (rta_set_t));

  set->_list = NULL;

  return (set);
}

static void delete_set (rta_set_t *set)
{
  rta_set_elt_t *elt = set->_list;

  while (NULL != elt) {
    rta_set_elt_t *old = elt;
    elt = elt->_next;

    old->_next = NULL;
    old->_obj  = NULL;
    free (old);
  }

  free (set);
}

static void set_insert (rta_set_t *set, void *obj)
{
  rta_set_elt_t *elt = (rta_set_elt_t*) xmalloc (sizeof (rta_set_elt_t));

  elt->_obj = obj;
  elt->_next = set->_list;
  set->_list = elt;
}

static int set_contains (rta_set_t *set, void *obj)
{
  rta_set_elt_t *elt = set->_list;

  while (NULL != elt) {
    if (elt->_obj == obj) {
      return (TRUE);
    }

    elt = elt->_next;
  }

  return (FALSE);
}
# endif /* defined RTA_SET */

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
      set_insert (_called_methods, ent);
    }
  } else if (iro_Load  == op) {
    ir_node *ptr = get_Load_ptr (node);
    entity  *ent = NULL;

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    }
    if (ent) {
      set_insert (_live_fields, ent);
    }
  } else  if (iro_Store == op) {
    ir_node *ptr = get_Store_ptr (node);
    entity  *ent = NULL;

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    }
    if (ent) {
      set_insert (_live_fields, ent);
    }
  } else if (iro_Alloc == op) {
    type *type = get_Alloc_type (node);
    set_insert (_live_classes, type);
  }
}

static void rta_fill_graph (ir_graph* graph)
{
  irg_walk (get_irg_end_block (graph), rta_act, NULL, NULL);
}

static void rta_fill_all ()
{
  int i;

  for (i = 0; i < get_irp_n_irgs(); i++) {
    rta_fill_graph (get_irp_irg (i));
  }
}

static int is_call_target (entity *method)
{
  return (TRUE);
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

  if (set_contains (_live_classes, clazz)) {
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


void rta_init ()
{
  fprintf (stdout, "BEGIN %s(%i): %s\n", __FILE__, __LINE__, __FUNCTION__);

  _live_classes   = new_set ();
  _live_fields    = new_set ();
  _called_methods = new_set ();

  _live_graphs = new_set ();
  _dead_graphs = new_set ();

  fprintf (stdout, "FILL  %s(%i): %s\n", __FILE__, __LINE__, __FUNCTION__);
  rta_fill_all ();
  fprintf (stdout, "DONE  %s(%i): %s\n", __FILE__, __LINE__, __FUNCTION__);

  /*
   * shold be:
   * rta_fill_queue ()
   */
}

void rta_cleanup ()
{
  if (_live_classes) {
    delete_set (_live_classes);
    _live_classes = NULL;
  }

  if (_live_fields) {
    delete_set (_live_fields);
    _live_fields = NULL;
  }

  if (_called_methods) {
    delete_set (_called_methods);
    _called_methods = NULL;
  }

  if (_live_graphs) {
    delete_set (_live_graphs);
    _live_graphs = NULL;
  }

  if (_dead_graphs) {
    delete_set (_dead_graphs);
    _dead_graphs = NULL;
  }
}

int  rta_is_alive_class  (type   *clazz)
{
  return (set_contains (_live_classes, clazz));
}

int  rta_is_alive_graph (ir_graph *graph)
{
  if (set_contains (_live_graphs, graph)) {
    return (TRUE);
  }

  if (set_contains (_dead_graphs, graph)) {
    return (FALSE);
  }

  entity *meth = get_irg_ent (graph);

  if (has_live_call (meth, graph) && has_live_class (meth, graph)) {
    set_insert (_live_graphs, graph);

    return (TRUE);
  } else {
    set_insert (_dead_graphs, graph);

    return (FALSE);
  }
}

int  rta_is_alive_field  (entity *field)
{
  return (set_contains (_live_fields, field));
}



/*
 * $Log$
 * Revision 1.2  2004/06/11 18:25:39  liekweg
 * Added todo
 *
 * Revision 1.1  2004/06/11 18:24:18  liekweg
 * Added RTA --flo
 *
 */
