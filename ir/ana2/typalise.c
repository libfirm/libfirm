/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto.c
 * Purpose:     Pto
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

# include "typalise.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

# include <assert.h>

#ifdef HAVE_STRING_H
# include <string.h>
#endif

# include "irnode_t.h"
# include "irgwalk.h"
# include "xmalloc.h"


/*
  Local Globals
*/

static long ta_id = 0;

/*
  Local Protos
*/
static typalise_t *typalise_proj (ir_node*);


/*
  Ctors, Dtors for typalise_t-s
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

/*
  Helpers for inheritance, overwriting and stuff
*/
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

/*
 Handle typalise_t-s
*/
/**
    Join 'one' and 'two'; both args are deallocated, result is freshly
    allocated.
*/
static typalise_t *ta_join (typalise_t *one, typalise_t *two)
{
  typalise_t *res = NULL;

  switch (one->kind) {
  case (type_invalid): { /* shut up, gcc */ }
  case (type_exact): {
    switch (two->kind) {
    case (type_invalid): { /* shut up, gcc */ }
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
    case (type_invalid): { /* shut up, gcc */ }
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
    case (type_invalid): { /* shut up, gcc */ }
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


# ifdef SHUT_UP_GCC
static const char *ta_name (typalise_t *ta)
{
# define BUF_SIZE 1024
  static char buf [BUF_SIZE];

  int len = sprintf (buf, "[%d] ", ta->id);

  switch (ta->kind) {
  case (type_invalid): { /* shut up, gcc */ }
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
# endif /* SHUT_UP_GCC */

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
   Check whether the given typalise_t includes the given type.
*/
static int ta_supports (typalise_t *ta, ir_graph *graph)
{
  switch (ta->kind) {
  case (type_invalid): { /* shut up, gcc */ }
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


/* =========== WHAT ELSE ? =========== */

/*
  Helper to typalise (ir_node*)
*/
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
      ir_graph *graph = get_irn_irg (proj);
      entity   *meth  = get_irg_entity (graph);

      long n = get_Proj_proj (proj);

      if (1 == n) {
        /* yay proj this */
        type     *tp    = get_entity_owner (meth);

        /* res = ta_exact (tp); */
        res = ta_type (tp);     /* TODO */
      } else {
        /* ugh proj arg */
        type *tp = get_method_param_type (get_entity_type (meth), n);
        if (is_pointer_type (tp)) {
          tp = get_pointer_points_to_type (tp);
        }

        res = ta_type (tp);
      }
    } else if (iro_Call == get_irn_opcode (proj_in)) {
      /* call result ... 'whatever' */
      /* hey, this is redundant (or the check for iro_Call further down) */
      ir_node *call_ptr = get_Call_ptr (proj_in);

      res = typalise (call_ptr);
    } else {
      fprintf (stdout, "\n Proj (Proj (%s)) not handled\n",
               get_op_name (get_irn_op (proj_in)));
      assert (0);
    }
  } else {
    opcode op = get_irn_opcode (proj_in);
    if ((iro_Load != op) && (iro_Alloc != op) && (iro_Call != op)) {
      fprintf (stdout, "\n Proj (%s) not handled\n",
               get_op_name (get_irn_op (proj_in)));
      assert (0);
    }
    res = typalise (proj_in);      /* everything else */
    /* Proj (Load), Proj (New), Proj (Call) */
  }

  return (res);
}



/*
  Public Interface
*/
/**
   Given a set of graphs and a typalise_t,  return the method (s) in
   the set that are supported by the typalise_t.  Also, deallocates
   the given set.
*/
lset_t *filter_for_ta (lset_t *set, typalise_t *ta)
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
   For the given ptr, do a quick check about what (class) types may be
   brought along on it.
*/
typalise_t *typalise (ir_node *node)
{
  opcode op = get_irn_opcode (node);
  typalise_t *res = NULL;

  switch (op) {
  case (iro_Cast): {
    /* casts always succeed */
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
    ir_node *load_ptr = get_Load_ptr (node);

    res = typalise (load_ptr);
  } break;

  case (iro_Sel): {
    /* FILTER */
    /* it's call (sel (ptr)) or load (sel (ptr)) */
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
    /* presumably call (sel (proj (call))) */
    ir_node *ptr = get_Call_ptr (node);
    entity *meth = NULL;
    if (iro_Sel == get_irn_opcode (ptr)) {
      meth = get_Sel_entity (ptr);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind (ptr) == symconst_addr_ent) {
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
      /* fprintf (stdout, "meth= (null)"); */
      res = NULL;
    }

    fprintf (stdout, "]\n");

  } break;

  case (iro_SymConst): {
    if (get_SymConst_kind (node) == symconst_type_tag) {
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





/*
  $Log$
  Revision 1.3  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.2  2004/10/22 09:53:10  liekweg
  Correctly handle proj_args

  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
