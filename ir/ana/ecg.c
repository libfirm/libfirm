/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana/ecg.c
 * Purpose:     Extended Call Graph
 * Author:      Florian
 * Modified by:
 * Created:     14.09.2004
 * CVS-ID:      $$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/**
   Erweiterter Aufrufgraph.
 */

#include "irnode.h"
#include "pmap.h"
/* #include "eset.h" */
#include "irgwalk.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "trvrfy.h"
#include "xmalloc.h"

# define TRUE 1
# define FALSE 0

# define BUF_SIZE 1024

# include "ecg.h"

/*
  data structures
*/

/* Lists, err, Sets */
typedef struct lset_entry
{
  void *data;
  struct lset_entry *next;
} lset_entry_t;

typedef struct lset
{
  lset_entry_t *first;
  lset_entry_t *last;           /* useful for lset_append */
  lset_entry_t *curs;           /* for lset_first/lset_next */
  int n_entries;
} lset_t;


typedef enum typalise_kind_enum {
  type_invalid = 0,             /* invalid (only set at deletion time) */
  type_exact = 1,               /* this and only this type (res.type) */
  type_types = 2,               /* these types (res.types) */
  type_type  = 3                /* this type and sub types (res.type) */
} typalise_kind;

typedef struct typalise
{
  typalise_kind kind;
  union
  {
    type *type;                 /* for kind == kind_exact and kind == kind_type */
    lset_t *types;              /* for kind == kind_types */
  } res;
  int id;
} typalise_t;

/*
   le flag
*/
static int verbose     = 0;
static int do_typalise = 0;

/*
   globals
*/

/* mapping from method graphs (callR) to method graphs (lset_ts of callEds) */
/* static pmap *calls; */
static pmap *graph_infos;

/** Counters for ecg_ecg and friends */
static long _graphs = 0;
static long _calls  = 0;

static int _depth = 0;
static int _max_depth = 0;

static int _max_callEds = 0;
static entity* _max_callEds_callR = NULL;

static long ta_id = 0;

/* ====================
   Lists, err, Sets
 ==================== */
/* create a new lset */
static lset_t *lset_create ()
{
  lset_t *lset = xmalloc (sizeof (lset_t));

  return (lset);
}

/* check whether the lset contains an entry for the given data */
static int lset_contains (lset_t *lset, void *data)
{
  lset_entry_t *entry = lset->first;

  while (NULL != entry) {
    if (data == entry->data) {
      return (TRUE);
    }

    entry = entry->next;
  }

  return (FALSE);
}

/* check whether the given lset is empty */
static int lset_empty (lset_t *lset)
{
  return (NULL == lset->first);
}


/* insert the data into the lset (unless there's an entry for it
   already) */
static void lset_insert (lset_t *lset, void *data)
{
  if (! lset_contains (lset, data)) {
    lset_entry_t *entry = xmalloc (sizeof (lset_entry_t));
    entry->data = data;
    entry->next = lset->first;
    lset->first = entry;

    if (NULL == lset->last) {
      lset->last = entry;
    }

    lset->n_entries ++;
  }
}

/* insert all entries from src into tgt */
static void lset_insert_all (lset_t *tgt, lset_t *src)
{
  lset_entry_t *curs = src->first;

  while (NULL != curs) {
    lset_insert (tgt, curs->data);

    curs = curs->next;
  }
}

/* append src to tgt. src is deallocated. */
static void lset_append (lset_t *tgt, lset_t *src)
{
  assert (! tgt->last->next);

  tgt->last->next = src->first;
  tgt->last = src->last;
  tgt->n_entries += src->n_entries;

  memset (src, 0x00, sizeof (lset_t));
  free (src);
}

/* remove the entry for the given data element from the lset. return
   TRUE iff it was on the list in the first place, FALSE else */
static int lset_remove (lset_t *lset, void *data)
{
  lset_entry_t *entry = lset->first;
  lset_entry_t *prev = NULL;

  while (NULL != entry) {
    if (data == entry->data) {
      /* ok, dike it out */

      if (NULL == prev) { /* ok, it's lset->first that needs diking */
        lset->first = entry->next;
      } else {
        prev->next = entry->next;
      }

      memset (entry, 0x00, sizeof (lset_entry_t));
      free (entry);

      lset->n_entries --;

      return (TRUE);
    }

    prev = entry;
    entry = entry->next;
  }

  return (FALSE);
}

/* prepare the given lset for an iteration. return the first element. */
static void *lset_first (lset_t *lset)
{
  lset->curs = lset->first;

  if (lset->first) {
    return (lset->first->data);
  } else {
    return (NULL);
  }
}

/* after calling lset_first, get the next element, if applicable, or
   NULL */
static void *lset_next (lset_t *lset)
{
  lset->curs = lset->curs->next;

  if (lset->curs) {
    return (lset->curs->data);
  } else {
    return (NULL);
  }
}

/* say how many entries there are in the given lset */
static int lset_n_entries (lset_t *lset)
{
  return (lset->n_entries);
}

/* deallocate the lset and all of its entries */
static void lset_destroy (lset_t *lset)
{
  lset_entry_t *curs = lset->first;

  while (NULL != curs) {
    lset_entry_t *tmp = curs->next;

    memset (curs, 0x00, sizeof (lset_entry_t));
    free (curs);

    curs = tmp;
  }

  memset (lset, 0x00, sizeof (lset_t));
  free (lset);
}

/* ====================
  Typalize Ptr
 ==================== */
/**
   Find out whether the given clazz uses the given implementation of a
   method.  Presumably, this is because clazz inherits the graph as
   the implementation for a method.
*/
static int uses_graph (type *clazz, entity *meth, ir_graph *graph)
{
  type *g_clazz = get_entity_owner (meth);

  if (g_clazz == clazz) {
    return (TRUE);
  }

  if (peculiarity_existent == get_entity_peculiarity (meth)) {
    ir_graph *g_graph = get_entity_irg (meth);

    if (g_graph != graph) {
      return (FALSE);
    }
  }

  /* else inherited or description */
  int use = FALSE;
  int i;
  int n_over = get_entity_n_overwrittenby (meth); /* DOWN-wards */

  for (i = 0; (i < n_over) && (!use); i ++) {
    entity *over = get_entity_overwrittenby (meth, i);

    use |= uses_graph (clazz, over, graph);
  }

  return (use);
}


/**
   Find out whether otype is a subtype of stype.
   Return non-zero iff otype is a subtype of stype.
*/
static int is_subtype (type *otype, type *stype)
{
  int n_sub = get_class_n_subtypes (stype);
  int is_sub = FALSE;
  int i;

  if (otype == stype) {
    return (TRUE);
  }

  for (i = 0; (!is_sub) && (i < n_sub); i ++) {
    type *sub = get_class_subtype (stype, i);

    is_sub |= is_subtype (otype, sub);
  }


  return (is_sub);
}

/**
    Compute the closure of all subtypes of otype (including otype
    itself)
*/
static void _collect_subtypes (type *otype, lset_t *set)
{
  lset_insert (set, otype);

  int n_sub = get_class_n_subtypes (otype);
  int i;

  for (i = 0; i < n_sub; i ++) {
    type *sub = get_class_subtype (otype, i);

    _collect_subtypes (sub, set);
  }
}

static lset_t *subtype_closure (type *otype)
{
  lset_t *set = lset_create ();

  _collect_subtypes (otype, set);

  return (set);
}

/**
   Helper method for get_owner_types
*/
static void _collect_owner_types (entity *method, ir_graph *graph, lset_t *tps)
{
  /* search DOWNwards in clazz hierarchy */

  if ((peculiarity_description == get_entity_peculiarity (method)) ||
      (peculiarity_inherited   == get_entity_peculiarity (method))) {
    lset_insert (tps, get_entity_owner (method));
  } else if (peculiarity_existent == get_entity_peculiarity (method)) {
    ir_graph *ex_graph = get_entity_irg (method);

    if ((NULL == ex_graph) || (ex_graph == graph)) {
      /* wtf? they define the same graph again? well, whatever: */
      lset_insert (tps, get_entity_owner (method));
    } else {
      /* aha: they define a new graph. can't have that, so bail out */
      return;
    }
  }

  int n_over = get_entity_n_overwrittenby (method);
  int i;

  for (i = 0; i < n_over; i ++) {
    entity *ometh = get_entity_overwrittenby (method, i);

    _collect_owner_types (ometh, graph, tps);
  }
}


/**
   Collect all classes that use the given implementation of a method.
*/
static lset_t *get_owner_types (ir_graph *graph)
{
  lset_t *tps = lset_create ();
  entity *meth = get_irg_entity (graph);

  _collect_owner_types (meth, graph, tps);

  return (tps);
}


/**
   Convenience funcs to create a typalise_t
*/
static typalise_t *ta_exact (type *tp)
{
  typalise_t *ta = (typalise_t*) xmalloc (sizeof (typalise_t));
  ta->kind = type_exact;
  ta->res.type = tp;
  ta->id = ta_id ++;

  assert (is_class_type (tp));

  return (ta);
}

static typalise_t *ta_types (lset_t *set)
{
  typalise_t *ta = (typalise_t*) xmalloc (sizeof (typalise_t));
  ta->kind = type_types;
  ta->res.types = set;
  ta->id = ta_id ++;

  return (ta);
}

static typalise_t *ta_type (type *tp)
{
  typalise_t *ta = (typalise_t*) xmalloc (sizeof (typalise_t));
  ta->kind = type_type;
  ta->res.type = tp;
  ta->id = ta_id ++;

  assert (is_class_type (tp));

  return (ta);
}

static void ta_delete (typalise_t *ta)
{
  if (type_types == ta->kind) {
    lset_destroy (ta->res.types);
    ta->res.types = NULL;
  } else {
    ta->res.type = NULL;
  }

  ta->kind = type_invalid;

  free (ta);
}

/**
    Join 'one' and 'two'; both args are deallocated, result is freshly
    allocated.
*/
static typalise_t *ta_join (typalise_t *one, typalise_t *two)
{
  typalise_t *res = NULL;

  switch (one->kind) {
  case (type_exact): {
    switch (two->kind) {
    case (type_exact): {
      if (one->res.type == two->res.type) {
        res = one;
      } else {
        lset_t *set = lset_create ();
        lset_insert (set, one->res.type);
        lset_insert (set, two->res.type);
        res = ta_types (set);

        ta_delete (one);
      }

      ta_delete (two);
    } break;
    case (type_types): {
      lset_insert (two->res.types, one->res.type);
      ta_delete (one);

      res = two;
    } break;
    case (type_type): {
      if (is_subtype (one->res.type, two->res.type)) {
        ta_delete (one);
        res = two;
      } else {
        lset_t *closure = subtype_closure (two->res.type);
        lset_insert (closure, one->res.type);

        ta_delete (two);

        res = one;
      }
    } break;
    }
  } break;
  case (type_types): {
    switch (two->kind) {
    case (type_exact): {
      res = ta_join (two, one);
    } break;
    case (type_types): {
      lset_insert_all (one->res.types, two->res.types);
      ta_delete (two);

      res = one;
    } break;
    case (type_type): {
      lset_t *closure = subtype_closure (two->res.type);
      lset_append (one->res.types, closure);

      ta_delete (two);

      res = one;
    } break;
    }
  } break;
  case (type_type): {
    switch (two->kind) {
    case (type_exact): {
      res = ta_join (two, one);
    } break;
    case (type_types): {
      res = ta_join (two, one);
    } break;
    case (type_type): {
      type *one_type = one->res.type;
      type *two_type = two->res.type;

      if (is_subtype (one_type, two_type)) {
        ta_delete (one);
        res = two;
      } else if (is_subtype (two_type, one_type)) {
        ta_delete (two);
        res = one;
      } else {
        lset_t *one_closure = subtype_closure (one->res.type);
        lset_t *two_closure = subtype_closure (two->res.type);

        lset_append (one_closure, two_closure);

        ta_delete (two);
        ta_delete (one);

        res = ta_types (one_closure);
      }
    } break;
    }
  } break;
  }

  assert (res && "no result");

  return (res);
}


static const char *ta_name (typalise_t *ta)
{
/* # define BUF_SIZE 1024 */
  static char buf [BUF_SIZE];

  int len = sprintf (buf, "[%d] ", ta->id);

  switch (ta->kind) {
  case (type_exact): {
    len += sprintf (buf+len, "only ");
    strncat (buf, get_type_name (ta->res.type), BUF_SIZE);
  } break;
  case (type_types): {
    len += sprintf (buf+len, "one_of ");

    type *iter = lset_first (ta->res.types);

    int size = BUF_SIZE - len - 1;
    while ((NULL != iter) && (0 < size)) {
      char *dest = strncat (buf, get_type_name (iter), size);
      size = (dest - buf);

      iter = lset_next (ta->res.types);
    }
  } break;
  case (type_type): {
    len += sprintf (buf+len, "poly ");
    strncat (buf, get_type_name (ta->res.type), BUF_SIZE);
  } break;
  }

  return (buf);
/* # undef BUF_SIZE */
}

/**
   Check whether the given typalise_t includes the given type.
*/
static int ta_supports (typalise_t *ta, ir_graph *graph)
{
  switch (ta->kind) {
  case (type_exact): {
    int res = FALSE;
    lset_t *tps = get_owner_types (graph);

    if (lset_contains (tps, ta->res.type)) {
      res = TRUE;
    }

    lset_destroy (tps);

    return (res);
  }
  case (type_type): {
    entity *meth = get_irg_entity (graph);
    type *tp = get_entity_owner (meth);
    int res = is_subtype (tp, ta->res.type);

    if (res) {
      return (TRUE);
    } else {
      res = uses_graph (ta->res.type, meth, graph);
    }

    return (res);
  }
  case (type_types): {
    type *tp = get_entity_owner (get_irg_entity (graph));

    return (lset_contains (ta->res.types, tp));
  }
  }

  assert (0 && "invalid ta");
}


/**
   Given a set of graphs and a typalise_t,  return the method(s) in
   the set that are supported by the typalise_t.  Also, deallocates
   the given set.
*/
static lset_t *filter_for_ta (lset_t *set, typalise_t *ta)
{
  lset_t *res = lset_create ();
  ir_graph *curs = (ir_graph*) lset_first (set);

  while (NULL != curs) {
    if (ta_supports (ta, curs)) {
      lset_insert (res, curs);
    }

    curs = lset_next (set);
  }

  lset_destroy (set);

  return (res);
}


/**
   Return a list containing all types of 'set' which are a subtype of 'type'.
*/
static lset_t *filter_for_type (lset_t *set, type *stype)
{
  type *curs = (type*) lset_first (set);
  lset_t *lset = lset_create ();

  while (NULL != curs) {
    if (is_subtype (curs, stype)) {
      lset_insert (lset, curs);
    }

    curs = lset_next (set);
  }

  return (lset);
}

/**
    Find an approximation to the given node's value's types
*/
static typalise_t *typalise (ir_node*);

/**
    Find an approximation to the given proj node's value's types
*/
static typalise_t *typalise_proj (ir_node *proj)
{
  typalise_t *res = NULL;
  ir_node *proj_in = get_Proj_pred (proj);

  if (iro_Proj  == get_irn_opcode (proj_in)) {
    /* fprintf (stdout, "\tProj (Proj)\n"); */

    proj_in = get_Proj_pred (proj_in);
    if (iro_Start == get_irn_opcode (proj_in)) {
      long n = get_Proj_proj (proj);
      if (1 == n) {
        /* yay proj this */
        ir_graph *graph = get_irn_irg (proj);
        entity   *meth  = get_irg_entity (graph);
        type     *tp    = get_entity_owner (meth);

        /* res = ta_exact (tp); */
        res = ta_type (tp);     /* TODO */
      } else {
        /* ugh proj arg */
        /* hey, even 'filtering' this NULL by the select of the actual
           call is probably as "precise" as anything: */
        return (NULL);
      }
    } else if (iro_Call == get_irn_opcode (proj_in)) {
      /* call result ... 'whatever' */
      ir_node *call_ptr = get_Call_ptr (proj_in);

      res = typalise (call_ptr);
    } else {
      fprintf (stdout, "\n Proj(Proj(%s)) not handled\n",
               get_op_name (get_irn_op (proj_in)));
      assert (0);
    }
  } else {
    opcode op = get_irn_opcode (proj_in);
    if ((iro_Load != op) && (iro_Alloc != op) && (iro_Call != op)) {
      fprintf (stdout, "\n Proj(%s) not handled\n",
               get_op_name (get_irn_op (proj_in)));
      assert (0);
    }
    res = typalise (proj_in);      /* everything else */
    /* Proj(Load), Proj(New), Proj(Call) */
  }

  return (res);
}


/**
   For the given ptr, do a quick check about what (class) types may be
   brought along on it.
*/
static typalise_t *typalise (ir_node *node)
{
  opcode op = get_irn_opcode (node);
  typalise_t *res = NULL;

  switch (op) {
  case (iro_Cast): {
    typalise_t *ta = NULL;
    type *tp = get_Cast_type (node);

    if (is_pointer_type (tp)) {
      tp = get_pointer_points_to_type (tp);
    }
    assert (is_class_type (tp));

    ta = typalise (get_Cast_op (node));

    if (NULL == ta) {           /* no type found */
      ta = ta_type (tp);
    } else if (type_exact == ta->kind) { /* one type found */
      /* nothing (maybe check cast? */
    } else if (type_type == ta->kind) { /* some types found */
      if (is_subtype (tp, ta->res.type)) {
        ta->res.type = tp;     /* assume cast is correct */
      } else {
        /* should assert (is_subtype (ta->res.type, tp)) */
      }
    } else if (type_types == ta->kind) {
      lset_t *ftp = filter_for_type (ta->res.types, tp);
      lset_destroy (ta->res.types);
      ta->res.types = ftp;
    }

    res = ta;
  } break;

  case (iro_Proj): {
    res = typalise_proj (node);
  } break;

  case (iro_Load): {
    /* presumably it's call(load(ptr)) we're analyzing. */
    ir_node *load_ptr = get_Load_ptr (node);

    res = typalise (load_ptr);
  } break;

  case (iro_Sel): {
    /* FILTER */
    /* it's call(sel(ptr)) or load(sel(ptr)) */
    entity *ent = get_Sel_entity (node);
    type *tp = get_entity_type (ent);

    if (is_method_type (tp)) {
      tp = get_entity_type (ent);
      tp = get_method_res_type (tp, 0);

      if (is_pointer_type (tp)) {
        tp = get_pointer_points_to_type (tp);
      }

      res = ta_type (tp);
    } else if (is_class_type (tp)) {
      tp = get_entity_type (ent);

      if (is_pointer_type (tp)) {
        tp = get_pointer_points_to_type (tp);
      }

      res = ta_type (tp);
    } else if (is_pointer_type (tp)) {
      tp = get_pointer_points_to_type (tp);
      res = ta_type (tp);
    } else {
      assert (0 && "select not handled");
    }
  } break;

  case (iro_Phi): {
    int n_ins = get_irn_arity (node);
    int i;
    ir_node *phi_in = NULL;
    typalise_t *ta = NULL;
    /* assert (0 && "Do we ever get here?"); */ /* apparently, we do. */

    for (i = 0; i < n_ins; i ++) {
      phi_in = get_irn_n (node, i);
      ta = (NULL == ta) ? typalise (phi_in) : ta_join (ta, typalise (phi_in));
    }

    res = ta;
  } break;

  case (iro_Alloc): {
    type *type = get_Alloc_type (node);
    res = ta_exact (type);
  } break;

  case (iro_Call): {
    /* presumably call(sel(proj(call))) */
    ir_node *ptr = get_Call_ptr (node);
    entity *meth = NULL;
    if (iro_Sel == get_irn_opcode (ptr)) {
      meth = get_Sel_entity (ptr);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind(ptr) == symconst_addr_ent) {
        meth = get_SymConst_entity (ptr);
      } else {
        meth = NULL;            /* WTF? */
      }
    }

    if (NULL != meth) {
      type *tp = get_method_res_type ((type*) meth, 0);
      res = ta_type (tp);
    } else {
      /* could be anything */
      /* fprintf (stdout, "meth=(null)"); */
      res = NULL;
    }

    fprintf (stdout, "]\n");

  } break;

  case (iro_SymConst): {
    if (get_SymConst_kind(node) == symconst_type_tag) {
      type *tp = get_SymConst_type (node);

      res = ta_type (tp);
    } else if (get_SymConst_kind (node) == symconst_addr_ent) {
      entity *ent = get_SymConst_entity (node);
      type *tp = get_entity_type (ent);
      tp = get_pointer_points_to_type (tp);
      assert (is_class_type (tp));

      res = ta_type (tp);       /* can't use ta_exact */
    } else {
      fprintf (stdout, "can't handle SymConst %s?\n",
               get_op_name (get_irn_op (node)));
      res = NULL;
    }
  } break;

  /* template:
     case (iro_Cast): {}
     break;
  */

  default: {
    fprintf (stdout, "what's with %s?\n", get_op_name (get_irn_op (node)));
    assert (0);
  } break;
  }

  return (res);
}


/* ====================
  CallEd stuff
  ==================== */
/**
   Append the given callEd to the given callEd info.
*/
static callEd_info_t *append_callEd_info (callEd_info_t *ced, ir_graph *callEd)
{
  callEd_info_t *nced = (callEd_info_t*) xmalloc (sizeof (sizeof (callEd_info_t)));

  nced->callEd = callEd;
  nced->prev = ced;

  return (nced);
}

/**
   Append all callEd methods of the given (call) node to the given graph_info.
*/
static void append_calls (graph_info_t *info, ir_node *call, lset_t *callEds)
{
  int n_callEds = 0;

  call_info_t *cinfo = (call_info_t*) xmalloc (sizeof (call_info_t));

  /* setup */
  cinfo->call = call;
  cinfo->prev = info->calls;
  info->calls = cinfo;
  cinfo->callEds = NULL;

  /* enter */
  ir_graph *callEd = lset_first (callEds);
  while (callEd) {
    cinfo->callEds = append_callEd_info (cinfo->callEds, callEd);

    callEd = lset_next (callEds);
  }
}

/**
   Append the (single) callEd to the given (call) node of the given graph_info.
*/
static void append_call (graph_info_t *info, ir_node *call, ir_graph *callEd)
{
  call_info_t *cinfo = (call_info_t*) xmalloc (sizeof (call_info_t));

  cinfo->call = call;
  cinfo->prev = info->calls;
  info->calls = cinfo;

  cinfo->callEds = append_callEd_info (cinfo->callEds, callEd);
}

/**
   Given a method, find the firm graph that implements that method.
   Return NULL for abstract and native methods.
*/
static ir_graph *_get_implementing_graph (entity *method)
{
  ir_graph *graph = NULL;

  /* What's up with the fenced out stuff in rta? */
  if (peculiarity_existent == get_entity_peculiarity (method)) {
    if (visibility_external_allocated == get_entity_visibility (method)) {
      /* Todo: native implementation */

      return (NULL);
    } else {
      graph = get_entity_irg (get_SymConst_entity (get_atomic_ent_value (method)));
      assert (graph && "no graph");

      return (graph);
    }
  } else if (0 && (peculiarity_description == get_entity_peculiarity (method))) {
    /* abstract --- can't find an implementation */
    graph = get_entity_irg (method);
    assert (!graph && "graph in abstract method");

    return (NULL);
  } else if ((peculiarity_description == get_entity_peculiarity (method)) ||
             (peculiarity_inherited == get_entity_peculiarity (method))) {
    /* search UPWARDS */
    int i;
    int n_over = get_entity_n_overwrites (method);

    assert (!graph);

    for (i = 0; (NULL == graph) && (i < n_over); i ++) {
      entity *over = get_entity_overwrites (method, i);

      graph = _get_implementing_graph (over);
    }
  } else {
    assert (0 && "invalid peculiarity");
  }


  return (graph);
}

/**
   Collect all graphs of 'method' in the given set.
*/
static void _collect_implementing_graphs (entity *method, lset_t *set)
{
  /* search DOWN-wards in clazz hierarchy */
  int i;
  int n_over = get_entity_n_overwrittenby (method);
  ir_graph *graph = get_entity_irg (method);

  if (NULL == graph) {
    graph = _get_implementing_graph (method);
  }

  if (graph) {
    lset_insert (set, graph);
  }

  for (i = 0; i < n_over; i ++) {
    entity *over = get_entity_overwrittenby (method, i);

    _collect_implementing_graphs (over, set);
  }
}


/**
   Collect all graphs that could possibly be executed when 'method' is called.
*/
static lset_t *get_implementing_graphs (entity *method, ir_node *select)
{
  lset_t *set = lset_create ();
  {
    ir_graph *impl = _get_implementing_graph (method);

    if (NULL != impl) {
      lset_insert (set, impl);
    } else {
      /* actually, abstract OR native */
    }
  }

  _collect_implementing_graphs (method, set);

  if (lset_empty (set)) {
    /* then it's a method which is only implemented natively, and we
       don' bother to analyse anything */
    return (set);
  }

  void *tmp = lset_first (set);
  int n_graphs = lset_n_entries (set);

  /* typalise select_in */
  if (do_typalise) {
    ir_node *select_in = get_Sel_ptr (select);
    typalise_t *ta = typalise (select_in);
    assert (ta && "typalise failed (go figure)");

    const char *res = ta_name (ta);

    /* fprintf (stdout, "typalyse res = %s\n", res); */

    if (1 != n_graphs) {
      set = filter_for_ta (set, ta);

      int n_filtered_graphs = lset_n_entries (set);

      fprintf (stdout, "%s: %02d %02d\n",
               __FUNCTION__,
               n_graphs,
               n_filtered_graphs,
               n_graphs - n_filtered_graphs);
      n_graphs = n_filtered_graphs;
    }
  }

  if (n_graphs > _max_callEds) {
    _max_callEds = n_graphs;
    _max_callEds_callR = method;
  }


  if (visibility_external_allocated != get_entity_visibility (method)) {
    if (0 == n_graphs) {
      /* fprintf (stdout, "no graphs for method %s\n", get_entity_name (method)); */
      assert (n_graphs && "no graphs for method");
    }
  }

  return (set);
}

/**
   Action for the graph.
*/
static void ecg_calls_act (ir_node *node, void *env)
{
  opcode op = get_irn_opcode (node);

  if (iro_Call == op) {         /* CALL */
    graph_info_t *graph_info = (graph_info_t*) env;

    entity *ent = NULL;
    ir_node *ptr = get_Call_ptr (node);

    /* CALL SEL */
    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
      lset_t *graphs = get_implementing_graphs (ent, ptr);

      append_calls (graph_info, node, graphs);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind(ptr) == symconst_addr_ent) {
        ent = get_SymConst_entity (ptr);
        ir_graph *graph = get_entity_irg (ent);

        if (graph) {
          append_call (graph_info, node, graph);
        } else {
          /* it's an externally allocated thingy */
        }
      } else if (get_SymConst_kind(ptr) == symconst_addr_name) {
        /* If this SymConst refers to a method the method is external_visible
           and therefore must be considered live anyways. */
        if (get_SymConst_name(ptr) != new_id_from_str("iro_Catch")) {
          assert (ent && "couldn't determine entity of call to symConst");
        }
      } else {
        /* other symconst. */
        assert (0 && "This SymConst can not be an address for a method call.");
      }

      /* STRANGE, no less ... */
    } else {
      DDMN(ptr);
      assert(0 && "Unexpected address expression");
    }
  }
}

/**
   Collect called graphs for the given graph.
*/
static void ecg_fill_graph_calls (ir_graph *graph)
{
  graph_info_t *graph_info = (graph_info_t*) xmalloc (sizeof (graph_info_t));

  graph_info->graph = graph;
  graph_info->calls = NULL;
  graph_info->ecg_seen = 0;

  entity *method  = get_irg_entity (graph);
  type   *clazz   = get_entity_owner (method);

  irg_walk_graph (graph, ecg_calls_act, NULL, graph_info);

  pmap_insert (graph_infos, graph, graph_info);
}

/**
   For each graph, collect called graphs, and enter them into calls.
*/
static void ecg_fill_calls ()
{
  int i;

  for (i = 0; i < get_irp_n_irgs (); i++) {
    ir_graph *graph = get_irp_irg (i);

    ecg_fill_graph_calls (graph);
  }
}

/* ====================
  ECG stuff
  ==================== */

/*
  get the call infos for the given graph
*/
graph_info_t *ecg_get_info (ir_graph *graph)
{
  graph_info_t *ginfo = (graph_info_t*) pmap_get (graph_infos, graph);

  assert (ginfo && "no info for graph");

  return (ginfo);
}



/**
   Dump the given graph and it's calls and it's calls callEds to the given file.
*/
static int ecg_ecg_graph (FILE *dot, ir_graph *graph)
{
  const char *name = get_irg_entity (graph) ?
    get_entity_name (get_irg_entity (graph)) : "noEntity";
  const char *color =
    (get_entity_stickyness
     (get_irg_entity (graph)) == stickyness_sticky) ?
    "red" : "lightyellow";

  graph_info_t *ginfo = (graph_info_t*) pmap_get (graph_infos, graph);

  if (0 != ginfo->ecg_seen) {
    fprintf (dot, "\t/* recursive call to \"%s\" (%d) */\n", name, ginfo->ecg_seen);
# if 0
    fprintf (dot, "\t/* recursive call to \"%s\" (0x%08x) */\n", name, graph);
# endif /* 0 */
    return (ginfo->ecg_seen);
  }

  assert (0L <= _graphs);

  const int graph_no = _graphs ++;
  ginfo->ecg_seen = graph_no;

  fprintf (dot, "\t/* Graph of \"%s.%s\" */\n",
           get_type_name (get_entity_owner (get_irg_entity (graph))),
           name);
  fprintf (dot, "\tgraph_%i [label=\"%s\\l%s\", color=\"%s\"];\n",
           graph_no,
           get_type_name (get_entity_owner (get_irg_entity (graph))),
           name,
           color);
  fprintf (dot, "\n");

  if (visibility_external_allocated ==
      get_entity_visibility (get_irg_entity (graph))) {
    fprintf (dot, "\t/* graph \"%s\" is external */\n", name);

    return (graph_no);
  }

  call_info_t *cinfo = ginfo->calls;
  while (NULL != cinfo) {
    ir_node *call = cinfo->call;
    callEd_info_t *ced = cinfo->callEds;
    const int call_no = _calls ++;

    fprintf (dot, "\t/* Call 0x%08x */\n", call);
    fprintf (dot, "\tcall_%i [label=\"call\\l0x%08x\"];\n",
             call_no, call);
    fprintf (dot, "\tgraph_%i -> call_%i [color=\"black\"];\n", graph_no, call_no);

    while (NULL != ced) {
      ir_graph *callEd_graph = ced->callEd;
      const int callEd_no = ecg_ecg_graph (dot, callEd_graph);
      const char *callEd_name = get_irg_entity (callEd_graph) ?
        get_entity_name (get_irg_entity (callEd_graph)) : "noEntity";
      const char *direction = (callEd_no <= graph_no) ? "forward" : "forward";
      const char *callEd_color     = (callEd_no <= graph_no) ? "red" : "lightyellow";

      fprintf (dot, "\t/* Call from graph \"%s\" to graph \"%s\" */\n",
               name,
               callEd_name);
      /* Check for recursive calls */
      /* if (callEd_no > graph_no) */ { /* do recursive calls (for now) */
      fprintf (dot, "\tcall_%i -> graph_%i [color=\"%s\", dir=\"%s\"];\n",
               call_no, callEd_no, callEd_color, direction);
      }

      ced = ced->prev;
      /* ced = NULL; */
    } /* done all calEds(call) */

    cinfo = cinfo->prev;
  } /* done all calls(graph) */

  fprintf (dot, "\t/* done with graph of \"%s\" */\n\n", name);

  fflush (dot);
  ginfo->ecg_seen = 0;

  return (graph_no);
}

/**
   Count how many nodes the ECG will have
*/
static char spaces [BUF_SIZE];

static void ecg_ecg_count (ir_graph *graph)
{
  graph_info_t *ginfo = (graph_info_t*) pmap_get (graph_infos, graph);

  if (0 != ginfo->ecg_seen) {
    return;
  }

  _depth ++;
  if (_depth > _max_depth) {
    _max_depth = _depth;

    /*
      fprintf (stdout, "_max_depth = %i\n", _max_depth);
      fprintf (stdout, "\tn_graphs: %i\n", _graphs);
    */
  }

  assert (0L <= _graphs);

  /*
    if (0 == (_graphs % 1000000)) {
    fprintf (stdout, "\tn_graphs: %i\n", _graphs);
    fprintf (stdout, "_depth = %i\n", _depth);
    }
  */

  const int graph_no = _graphs ++;
  ginfo->ecg_seen = graph_no;

  fprintf (stdout, "%sMethod \"%s.%s\"\n",
           spaces + BUF_SIZE - _depth,
           get_type_name (get_entity_owner (get_irg_entity (graph))),
           get_entity_name (get_irg_entity (graph)));

  call_info_t *cinfo = ginfo->calls;
  while (NULL != cinfo) {

    callEd_info_t *ced = cinfo->callEds;

    fprintf (stdout, "%sCall \"0x%08x\"\n",
             spaces + BUF_SIZE - _depth,
             cinfo->call);

    while (NULL != ced) {
      ir_graph *callEd_graph = ced->callEd;

      fprintf (stdout, "%sCall Target \"%s.%s\"\n",
               spaces + BUF_SIZE - _depth,
               get_type_name (get_entity_owner (get_irg_entity (callEd_graph))),
               get_entity_name (get_irg_entity (callEd_graph)));

      ecg_ecg_count (callEd_graph);

      ced = ced->prev;
    } /* done all calEds(call) */
    cinfo = cinfo->prev;
  } /* done all calls(graph) */

  ginfo->ecg_seen = 0;
  _depth --;
}

/* ====================
  Public Interface
  ==================== */

/**
   Initialise our data structures.
*/
void ecg_init (int typalise)
{
  do_typalise = typalise;

  graph_infos = pmap_create ();

  ecg_fill_calls ();
}

/**
   Clean up our mess.
*/
void ecg_cleanup ()
{
  int i;

  for (i = 0; i < get_irp_n_irgs (); i++) {
    ir_graph *graph = get_irp_irg (i);

    graph_info_t *info = pmap_get (graph_infos, graph);
    call_info_t *cinfo = info->calls;

    while (NULL != cinfo) {
      free (cinfo->callEds);
      cinfo->call = NULL;

      callEd_info_t *ced = cinfo->callEds;

      while (NULL != ced) {
        callEd_info_t *nced = ced->prev;
        free (ced);
        ced->prev = NULL;
        ced->callEd = NULL;
        ced = nced;
      }

      /* Todo: delete callEds */
      cinfo->callEds = NULL;

      free (cinfo);
      cinfo = cinfo->prev;
    }

    free (info);
    pmap_insert (graph_infos, graph, NULL);
  }


  pmap_destroy (graph_infos);

  /* BEGIN mild paranoia mode */
  graph_infos = NULL;
  /* END mild paranoia mode */
}

/**
   Show what we have found.
*/
void ecg_report ()
{
  int i;

  FILE *dot = fopen ("calls.dot", "w");

  fprintf (dot, "digraph \"calls\" {\n");
  fprintf (dot, "\tnode [shape = \"record\", style = \"filled\"];\n");
  fprintf (dot, "\tedge [color = \"black\"];\n");
  fprintf (dot, "\n");
  fprintf (dot, "\tsize = \"11, 7\";\n");
  fprintf (dot, "\trotate = \"90\";\n");
  fprintf (dot, "\tratio = \"fill\";\n");
  fprintf (dot, "\trankdir = \"LR\";\n");
  fprintf (dot, "\n");

  for (i = 0; i < get_irp_n_irgs (); i++) {
    ir_graph *graph = get_irp_irg (i);
    graph_info_t *info = (graph_info_t*) pmap_get (graph_infos, graph);

    const char *name = get_irg_entity (graph) ?
      get_entity_name (get_irg_entity (graph)) : "noEntity";

    const char *oname = get_type_name
      (get_entity_owner (get_irg_entity (graph)));

    const char *color =
      (get_entity_stickyness
       (get_irg_entity (graph)) == stickyness_sticky) ?
      "red" : "lightyellow";

    fprintf (dot, "\t/* graph_0x%08x (\"%s\") */\n", graph, name);
    fprintf (dot,
             "\tgraph_0x%08x [label=\"%s\\l%s\", color=\"%s\"];\n",
             graph, oname, name, color);
    fprintf (dot, "\n");
    fprintf (dot, "\t/* now the calls */\n");

    call_info_t *cinfo = info->calls;
    while (NULL != cinfo) {
      ir_node *call = cinfo->call;
      int i;

      fprintf (dot, "\t/* call_0x%08x */\n", call);
      fprintf (dot, "\tcall_0x%08x [label=\"call\\l0x%08x\"];\n", call, call);
      fprintf (dot, "\tgraph_0x%08x -> call_0x%08x;\n", graph, call);

      callEd_info_t *ced = cinfo->callEds;
      while (NULL != ced) {
        fprintf (dot, "\tcall_0x%08x -> graph_0x%08x;\n", call, ced->callEd);
        ced = ced->prev;
      }
      fprintf (dot, "\n");

      cinfo = cinfo->prev;
    }
    fprintf (dot, "\n");
  }
  fprintf (dot, "}\n");

  fprintf (stdout, " max_callEds: %i\n", _max_callEds);
  fprintf (stdout, " max_callEds_callR: \"%s\"\n",
           get_entity_name (_max_callEds_callR));

  fclose (dot);

  ecg_ecg ();
}

/**
   Experimental:  Print the ecg
*/
void ecg_ecg ()
{
  _graphs = 0;
  _calls  = 0;

  ir_graph *main_graph = get_irp_main_irg ();

  /*
  memset (spaces, '.', BUF_SIZE);
  spaces [BUF_SIZE-1] = '\0';

  ecg_ecg_count (main_graph);
  fprintf (stdout, "n_graphs: %i\n", _graphs);
  fprintf (stdout, "max_depth = %i\n", _max_depth);
  */

  /* return; */

  _graphs = 0;
  _calls  = 0;

  FILE *dot = fopen ("ecg.dot", "w");

  fprintf (dot, "digraph \"ecg\" {\n");
  fprintf (dot, "\tnode [shape = \"record\", style = \"filled\"];\n");
  fprintf (dot, "\tedge [color = \"black\"];\n");
  fprintf (dot, "\n");
  fprintf (dot, "\tsize = \"11, 7\";\n");
  fprintf (dot, "\trotate = \"90\";\n");
  fprintf (dot, "\tratio = \"fill\";\n");
  fprintf (dot, "\trankdir = \"LR\";\n");
  fprintf (dot, "\n");

  /* ir_graph *main_graph = get_irp_main_irg (); */
  ecg_ecg_graph (dot, main_graph);

  fprintf (dot, "\t/* Grand Total: */\n");
  fprintf (dot, "\t/* calls:  %i */\n", _calls);
  fprintf (dot, "\t/* graphs: %i */\n", _graphs);
  fprintf (dot, "\t/* (sales tax not included) */\n");

  fprintf (dot, "}\n");

  fclose (dot);
}
