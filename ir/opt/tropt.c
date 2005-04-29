/**
 *
 * @file irsimpeltype.h
 *
 * Project:     libFIRM
 * File name:   ir/opt/tropt.h
 * Purpose:     Optimize the type representation.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     20.4.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Perform optimizations of the type representation.
 *
 *
 */

#include <alloca.h>

#include "tropt.h"

#include "irprog.h"
#include "mangle.h"

#include "tr_inheritance.h"
#include "irtypeinfo.h"
#include "irgwalk.h"
#include "irsimpletype.h"
#include "trouts.h"
#include "ircons.h"
#include "irgmod.h"
#include "irflag.h"

/* - statistics ---------------------------------------------- */

static int n_casts_normalized = 0;
static int n_casts_removed    = 0;
static int n_sels_concretized = 0;

/* - Cast normalization. ------------------------------------- */

static type *default_gen_pointer_type_to(type *tp);

#define PTR_TYPE_SUFFIX "cc_ptr_tp"  /* class cast pointer type. */
static ident *ptr_type_suffix = NULL;
static gen_pointer_type_to_func gen_pointer_type_to = default_gen_pointer_type_to;

static type *default_gen_pointer_type_to(type *tp) {
  type *res = NULL;
  if (get_trouts_state() == outs_consistent) {
    if (get_type_n_pointertypes_to(tp) > 0) {
      res = get_type_pointertype_to(tp, 0);
    } else {
      res = new_type_pointer(mangle_u(get_type_ident(tp), ptr_type_suffix), tp);
      /* Update trout for pointertypes, so we can use it in next call. */
      add_type_pointertype_to(tp, res);
    }
  } else {
    res = find_pointer_type_to_type(tp);
    if (res == firm_unknown_type)
      res = new_type_pointer(mangle_u(get_type_ident(tp), ptr_type_suffix), tp);
  }

  return res;
}

/* Return a type that is a depth times pointer to type. */
static type *pointerize_type(type *tp, int depth) {
  for (; depth > 0; --depth) {
    tp = gen_pointer_type_to(tp);
  }
  return tp;
}

static void normalize_irn_class_cast(ir_node *n, void *env) {
  if (get_irn_op(n) != op_Cast) return;


  type *fromtype = get_irn_typeinfo_type(get_Cast_op(n));
  type *totype   = get_Cast_type(n);
  int ref_depth = 0;

  if (totype == fromtype) return;   /* Case for optimization! */

  while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
    totype   = get_pointer_points_to_type(totype);
    fromtype = get_pointer_points_to_type(fromtype);
    ref_depth++;
  }

  if (!is_Class_type(totype))   return;
  if (!is_Class_type(fromtype)) return;

  if ((get_class_supertype_index(totype, fromtype) != -1) ||
      (get_class_supertype_index(fromtype, totype) != -1) ) {
    /* It's just what we want ... */
    return;
  }

  set_cur_block(get_nodes_block(n));

  if (is_subclass_of(totype, fromtype)) {
    /* downcast */
    while (get_class_subtype_index(fromtype, totype) == -1) {
      /* Insert a cast to a subtype of fromtype. */
      type *new_type = NULL;
      ir_node *new_cast;
      int i, n_subtypes = get_class_n_subtypes(fromtype);
      for (i = 0; i < n_subtypes && !new_type; ++i) {
	type *new_sub = get_class_subtype(fromtype, i);
	if (is_superclass_of(new_sub, totype))
	  new_type = new_sub;
      }
      assert(new_type);
      fromtype = new_type;
      new_type = pointerize_type(new_type, ref_depth);
      new_cast = new_Cast(get_Cast_op(n), new_type);
      n_casts_normalized ++;
      set_irn_typeinfo_type(new_cast, new_type);  /* keep type information up to date. */
      set_Cast_op(n, new_cast);
      if (get_trouts_state() != outs_none) add_type_cast(new_type, new_cast);
    }
  }
  else {
    assert(is_superclass_of(totype, fromtype));
    /* upcast */
    while (get_class_supertype_index(fromtype, totype) == -1) {
      /* Insert a cast to a supertype of fromtype. */
      type *new_type = NULL;
      ir_node *new_cast;
      int i, n_supertypes = get_class_n_supertypes(fromtype);
      for (i = 0; i < n_supertypes && !new_type; ++i) {
	type *new_super = get_class_supertype(fromtype, i);
	if (is_subclass_of(new_super, totype))
	  new_type = new_super;
      }
      assert(new_type);
      fromtype = new_type;
      new_type = pointerize_type(new_type, ref_depth);
      new_cast = new_Cast(get_Cast_op(n), new_type);
      n_casts_normalized ++;
      set_irn_typeinfo_type(new_cast, new_type);  /* keep type information up to date. */
      if (get_trouts_state() != outs_none) add_type_cast(new_type, new_cast);
      set_Cast_op(n, new_cast);
    }
  }
}


static void pure_normalize_irg_class_casts(ir_graph *irg) {
  assert(get_irg_class_cast_state(irg) != ir_class_casts_any &&
	 "Cannot normalize irregular casts.");
  if (get_irg_class_cast_state(irg) == ir_class_casts_normalized) {
    verify_irg_class_cast_state(irg);
    return;
  }

  irg_walk_graph(irg, NULL, normalize_irn_class_cast, NULL);
  set_irg_class_cast_state(irg, ir_class_casts_normalized);
}


void normalize_irg_class_casts(ir_graph *irg, gen_pointer_type_to_func gppt_fct) {
  assert(get_irp_typeinfo_state() == ir_typeinfo_consistent);

  if (gppt_fct) gen_pointer_type_to = gppt_fct;

  if (!ptr_type_suffix)
    ptr_type_suffix = new_id_from_str(PTR_TYPE_SUFFIX);

  pure_normalize_irg_class_casts(irg);

  gen_pointer_type_to = default_gen_pointer_type_to;
}

void normalize_irp_class_casts(gen_pointer_type_to_func gppt_fct) {
  int i, n_irgs = get_irp_n_irgs();
  if (gppt_fct) gen_pointer_type_to = gppt_fct;

  if (get_irp_typeinfo_state() != ir_typeinfo_consistent)
    simple_analyse_types();

  for (i = 0; i < n_irgs; ++i) {
    pure_normalize_irg_class_casts(get_irp_irg(i));
  }

  set_irp_class_cast_state(ir_class_casts_normalized);
  gen_pointer_type_to = default_gen_pointer_type_to;

  if (get_opt_optimize_class_casts_verbose() && get_firm_verbosity()) {
    printf(" Cast normalization: %d Casts inserted.\n", n_casts_normalized);
  }
}



/* - Cast optimization. ------------------------------------------- */

static void cancel_out_casts(ir_node *cast) {
  ir_node *orig, *pred = get_Cast_op(cast);
  type *tp_cast, *tp_pred, *tp_orig;
  int ref_depth = 0;

  if (get_irn_op(pred) != op_Cast) return;
  orig = get_Cast_op(pred);

  tp_cast = get_Cast_type(cast);
  tp_pred = get_Cast_type(pred);
  tp_orig = get_irn_typeinfo_type(orig);

  while (is_Pointer_type(tp_cast) &&
	 is_Pointer_type(tp_pred) &&
	 is_Pointer_type(tp_orig)   ) {
    tp_cast = get_pointer_points_to_type(tp_cast);
    tp_pred = get_pointer_points_to_type(tp_pred);
    tp_orig = get_pointer_points_to_type(tp_orig);
    ref_depth++;
  }

  if (!is_Class_type(tp_cast)) return;
  if (!is_Class_type(tp_pred)) return;
  if (!is_Class_type(tp_orig)) return;

  if (is_subclass_of(tp_pred, tp_cast) && get_opt_suppress_downcast_optimization())
    return;

  if (tp_cast == tp_orig) {
    exchange(cast, orig);
    n_casts_removed += 2;
    return;
  }

  if (!(is_subclass_of  (tp_cast, tp_orig) || is_subclass_of  (tp_orig, tp_cast)))
    /* Avoid (B2)(A)(new B1()) --> (B2)(new B1()) */
    return;

  if ((is_subclass_of  (tp_cast, tp_pred) && is_superclass_of(tp_pred, tp_orig)) ||
      (is_superclass_of(tp_cast, tp_pred) && is_subclass_of  (tp_pred, tp_orig))   ) {
    set_Cast_op (cast, orig);
    n_casts_removed ++;
  }
}

static void concretize_selected_entity(ir_node *sel) {
  ir_node *cast, *ptr = get_Sel_ptr(sel);
  type *orig_tp, *cast_tp;
  entity *new_ent, *sel_ent;

  sel_ent = get_Sel_entity(sel);
  cast = get_Sel_ptr(sel);

  while (get_irn_op(cast) == op_Cast) {
    cast_tp = get_Cast_type(cast);
    ptr = get_Cast_op(cast);
    orig_tp = get_irn_typeinfo_type(ptr);

    if (!is_Pointer_type(orig_tp)) return;
    if (!is_Pointer_type(cast_tp)) return;
    orig_tp = get_pointer_points_to_type(orig_tp);
    cast_tp = get_pointer_points_to_type(cast_tp);
    if (!is_Class_type(orig_tp)) return;
    if (!is_Class_type(cast_tp)) return;

    /* We only want to contretize, but not generalize. */
    if (!is_superclass_of(cast_tp, orig_tp)) return;

    /* Hmm, we are not properly typed. */
    if (get_class_member_index(cast_tp, sel_ent) == -1) return;

    new_ent = resolve_ent_polymorphy(orig_tp, sel_ent);

    /* New ent must be member of orig_tp. */
    if (get_class_member_index(orig_tp, new_ent) == -1) return;

    set_Sel_entity(sel, new_ent);
    set_Sel_ptr(sel, ptr);
    n_sels_concretized++;

    sel_ent = new_ent;
    cast = ptr;
  }
}

static void concretize_Phi_type(ir_node *phi) {
  int i, n_preds = get_Phi_n_preds(phi);
  ir_node **pred = alloca(n_preds * sizeof(ir_node *));
  ir_node *new;
  type *totype, *fromtype;

  if (n_preds == 0) return;
  pred[0] = get_Phi_pred(phi, 0);

  if (get_irn_op(pred[0]) != op_Cast) return;

  if (!is_Cast_upcast(pred[0])) return;

  fromtype = get_irn_typeinfo_type(get_Cast_op(pred[0]));
  totype   = get_Cast_type(pred[0]);

  pred[0] = get_Cast_op(pred[0]);
  for (i = 1; i < n_preds; ++i) {
    pred[i] = get_Phi_pred(phi, i);
    if (get_irn_op(pred[i]) != op_Cast) return;
    if (get_irn_typeinfo_type(get_Cast_op(pred[i])) != fromtype) return;
    pred[i] = get_Cast_op(pred[i]);
  }

  /* Transform Phi */
  set_cur_block(get_nodes_block(phi));
  new = new_Phi(n_preds, pred, get_irn_mode(phi));
  set_irn_typeinfo_type(new, fromtype);
  new = new_Cast(new, totype);
  set_irn_typeinfo_type(new, totype);
  exchange(phi, new);
}

void remove_Cmp_Null_cast(ir_node *cmp) {
  ir_node *cast, *null, *new_null;
  int cast_pos, null_pos;
  type *fromtype;

  cast = get_Cmp_left(cmp);
  cast_pos = 0;
  if (get_irn_op(cast) != op_Cast) {
    null = cast;
    null_pos = cast_pos;
    cast = get_Cmp_right(cmp);
    cast_pos = 1;
    if (get_irn_op(cast) != op_Cast) return;
  } else {
    null = get_Cmp_right(cmp);
    null_pos = 1;
  }

  if (get_irn_op(null) != op_Const) return;
  if (!mode_is_reference(get_irn_mode(null))) return;
  if (get_Const_tarval(null) != get_mode_null(get_irn_mode(null))) return;

  /* Transform Cmp */
  set_irn_n(cmp, cast_pos, get_Cast_op(cast));
  fromtype = get_irn_typeinfo_type(get_Cast_op(cast));
  new_null = new_Const_type(get_Const_tarval(null), fromtype);
  set_irn_typeinfo_type(new_null, fromtype);
  set_irn_n(cmp, null_pos, new_null);
  n_casts_removed ++;
}

static void irn_optimize_class_cast(ir_node *n, void *env) {
  if (get_irn_op(n) == op_Cast)
    cancel_out_casts(n);
  else if (get_irn_op(n) == op_Sel)
    concretize_selected_entity(n);
  else if (get_irn_op(n) == op_Phi)
    concretize_Phi_type(n);
  else if (get_irn_op(n) == op_Cmp)
    remove_Cmp_Null_cast(n);
}

void optimize_class_casts(void) {
  int i, n_irgs = get_irp_n_irgs();

  if (get_irp_typeinfo_state() != ir_typeinfo_consistent)
    simple_analyse_types();

  all_irg_walk(NULL, irn_optimize_class_cast, NULL);

  set_trouts_inconsistent();
  for (i = 0; i < n_irgs; ++i)
    set_irg_outs_inconsistent(get_irp_irg(i));

  if (get_opt_optimize_class_casts_verbose() && get_firm_verbosity()) {
    printf(" Cast optimization: %d Casts removed, %d sels concretized.\n",
	   n_casts_removed, n_sels_concretized);
  }
}
