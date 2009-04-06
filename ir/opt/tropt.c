/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Perform optimizations of the type representation.
 * @date    20.4.2005
 * @author  Goetz Lindenmaier
 * @version $Id$
 */
#include "config.h"

#include <assert.h>

#include "iroptimize.h"
#include "irprog.h"
#include "irtypeinfo.h"
#include "irgwalk.h"
#include "irsimpletype.h"
#include "trouts.h"
#include "ircons.h"
#include "irgmod.h"
#include "irflag_t.h"
#include "xmalloc.h"
#include "debug.h"
#include "tropt.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/* - statistics ---------------------------------------------- */

static int n_casts_normalized = 0;
static int n_casts_removed    = 0;
static int n_sels_concretized = 0;

/* - Cast normalization. ------------------------------------- */

static ir_type *default_gen_pointer_type_to(ir_type *tp);

#define PTR_TYPE_SUFFIX "cc_ptr_tp"  /* class cast pointer type. */
static ident *ptr_type_suffix = NULL;
static gen_pointer_type_to_func gen_pointer_type_to = default_gen_pointer_type_to;

/**
 * Find a pointer type to a given type.
 * Uses and updates trouts if available.
 */
static ir_type *default_gen_pointer_type_to(ir_type *tp) {
	ir_type *res = NULL;
	if (get_trouts_state() == outs_consistent) {
		if (get_type_n_pointertypes_to(tp) > 0) {
			res = get_type_pointertype_to(tp, 0);
		} else {
			ir_mode *mode = is_Method_type(tp) ? mode_P_code : mode_P_data;

			res = new_type_pointer(id_mangle_u(get_type_ident(tp), ptr_type_suffix), tp, mode);
			/* Update trout for pointer types, so we can use it in next call. */
			add_type_pointertype_to(tp, res);
		}
	}
	else {
		res = find_pointer_type_to_type(tp);
		if (res == firm_unknown_type)
			res = new_type_pointer(id_mangle_u(get_type_ident(tp), ptr_type_suffix), tp, mode_P_data);
	}

	return res;
}

/** Return a type that is a depth times pointer to type. */
static ir_type *pointerize_type(ir_type *tp, int depth) {
	for (; depth > 0; --depth) {
		tp = gen_pointer_type_to(tp);
	}
	return tp;
}


static ir_node *normalize_values_type(ir_type *totype, ir_node *pred) {
	ir_type *fromtype = get_irn_typeinfo_type(pred);
	ir_node *new_cast = pred;
	int ref_depth = 0;

	if (totype == fromtype) return pred;   /* Case for optimization! */

	while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
		totype   = get_pointer_points_to_type(totype);
		fromtype = get_pointer_points_to_type(fromtype);
		ref_depth++;
	}

	if (!is_Class_type(totype))   return pred;
	if (!is_Class_type(fromtype)) return pred;

	if ((get_class_supertype_index(totype, fromtype) != -1) ||
		(get_class_supertype_index(fromtype, totype) != -1) ) {
			/* It's just what we want ... */
			return pred;
	}

	set_cur_block(get_nodes_block(pred));

	if (is_SubClass_of(totype, fromtype)) {
		/* downcast */
		while (get_class_subtype_index(fromtype, totype) == -1) {
			/* Insert a cast to a subtype of fromtype. */
			ir_type *new_type = NULL;
			ir_node *new_cast;
			int i, n_subtypes = get_class_n_subtypes(fromtype);
			for (i = 0; i < n_subtypes && !new_type; ++i) {
				ir_type *new_sub = get_class_subtype(fromtype, i);
				if (is_SuperClass_of(new_sub, totype))
					new_type = new_sub;
			}
			assert(new_type);
			fromtype = new_type;
			new_type = pointerize_type(new_type, ref_depth);
			new_cast = new_Cast(pred, new_type);
			pred = new_cast;
			n_casts_normalized ++;
			set_irn_typeinfo_type(new_cast, new_type);  /* keep type information up to date. */
			if (get_trouts_state() != outs_none) add_type_cast(new_type, new_cast);
		}
	} else {
		assert(is_SuperClass_of(totype, fromtype));
		/* upcast */
		while (get_class_supertype_index(fromtype, totype) == -1) {
			/* Insert a cast to a supertype of fromtype. */
			ir_type *new_type = NULL;
			int i, n_supertypes = get_class_n_supertypes(fromtype);
			for (i = 0; i < n_supertypes && !new_type; ++i) {
				ir_type *new_super = get_class_supertype(fromtype, i);
				if (is_SubClass_of(new_super, totype))
					new_type = new_super;
			}
			assert(new_type);
			fromtype = new_type;
			new_type = pointerize_type(new_type, ref_depth);
			new_cast = new_Cast(pred, new_type);
			pred = new_cast;
			n_casts_normalized ++;
			set_irn_typeinfo_type(new_cast, new_type);  /* keep type information up to date. */
			if (get_trouts_state() != outs_none) add_type_cast(new_type, new_cast);
		}
	}
	return new_cast;
}

/**
 * Post-Walker.
 */
static void normalize_irn_class_cast(ir_node *n, void *env) {
	ir_node *res;
	(void) env;
	if (is_Cast(n)) {
		ir_node *pred   = get_Cast_op(n);
		ir_type *totype = get_Cast_type(n);
		res = normalize_values_type(totype, pred);
		set_Cast_op(n, res);
	} else if (is_Call(n)) {
		int i, n_params = get_Call_n_params(n);
		ir_type *tp = get_Call_type(n);
		for (i = 0; i < n_params; ++i) {
			res = normalize_values_type(get_method_param_type(tp, i), get_Call_param(n, i));
			set_Call_param(n, i, res);
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
	int i;
	if (gppt_fct) gen_pointer_type_to = gppt_fct;

	if (get_irp_typeinfo_state() != ir_typeinfo_consistent)
		simple_analyse_types();

	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		pure_normalize_irg_class_casts(get_irp_irg(i));
	}

	set_irp_class_cast_state(ir_class_casts_normalized);
	gen_pointer_type_to = default_gen_pointer_type_to;

	DB((dbg, SET_LEVEL_1, " Cast normalization: %d Casts inserted.\n", n_casts_normalized));
}



/* - Cast optimization. ------------------------------------------- */

/**
 * Optimizes Casts:
 *
 *  (T1)(T2)x<T1> -> x<T1>
 *
 *  (T3)(T2)x<T1> -> (T3)x<T1>
 *
 * if possible.
 *
 * @param cast  the Cast node
 *
 * @return 1 if the cast was changed
 */
static int cancel_out_casts(ir_node *cast) {
	ir_node *orig, *pred = get_Cast_op(cast);
	ir_type *tp_cast, *tp_pred, *tp_orig;
	int ref_depth = 0;

	if (!is_Cast(pred)) return 0;
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

	if (!is_Class_type(tp_cast) || !is_Class_type(tp_pred) || !is_Class_type(tp_orig))
		return 0;

	if (is_SubClass_of(tp_pred, tp_cast) && get_opt_suppress_downcast_optimization())
		return 0;

	if (tp_cast == tp_orig) {
		exchange(cast, orig);
		n_casts_removed += 2;
		return 1;
	}

	if (!(is_SubClass_of  (tp_cast, tp_orig) || is_SubClass_of  (tp_orig, tp_cast))) {
		/* Avoid (B2)(A)(new B1()) --> (B2)(new B1())
		* if B1 =!> B2  and  B2 =!> B1
		*/
		return 0;
	}

	if ((is_SubClass_of  (tp_cast, tp_pred) && is_SuperClass_of(tp_pred, tp_orig)) ||
	    (is_SuperClass_of(tp_cast, tp_pred) && is_SubClass_of  (tp_pred, tp_orig))) {
		/* Cast --> Pred --> Orig */
		set_Cast_op(cast, orig);
		++n_casts_removed;
		return 1;
	}
	return 0;
}

/**
 * Optimize Sel(Cast(type, ptr), ent) into Sel(ptr_type, ent_type)
 *
 * @param sel  the Sel node
 *
 * @return 1 if Cast's where removed
 */
static int concretize_selected_entity(ir_node *sel) {
	ir_node   *cast, *ptr = get_Sel_ptr(sel);
	ir_type   *orig_tp, *cast_tp;
	ir_entity *new_ent, *sel_ent;
	int       res = 0;

	sel_ent = get_Sel_entity(sel);
	cast    = get_Sel_ptr(sel);

	while (is_Cast(cast)) {
		cast_tp = get_Cast_type(cast);
		ptr     = get_Cast_op(cast);
		orig_tp = get_irn_typeinfo_type(ptr);

		/* we handle only classes */
		if (!is_Pointer_type(orig_tp)|| !is_Pointer_type(cast_tp))
			return res;
		orig_tp = get_pointer_points_to_type(orig_tp);
		cast_tp = get_pointer_points_to_type(cast_tp);
		if (!is_Class_type(orig_tp) || !is_Class_type(cast_tp))
			return res;

		/* We only want to concretize, but not generalize. */
		if (!is_SuperClass_of(cast_tp, orig_tp))
			return res;

		/* The sel entity should be a member of the cast_tp, else
		   the graph was not properly typed. */
		if (get_class_member_index(cast_tp, sel_ent) == -1)
			return res;

		new_ent = resolve_ent_polymorphy(orig_tp, sel_ent);

		/* New ent must be member of orig_tp. */
		if (get_class_member_index(orig_tp, new_ent) == -1)
			return res;

		/* all checks done, we can remove the Cast and update the Sel */
		set_Sel_entity(sel, new_ent);
		set_Sel_ptr(sel, ptr);
		++n_sels_concretized;

		sel_ent = new_ent;
		cast    = ptr;
		res     = 1;
	}
	return res;
}

/**
 * Move Casts of the same type through a Phi node, i.e.
 * Phi(Cast(type, x_0), ..., Cast(type, x_n)) -> Cast(type, Phi(x_0, ..., x_n))
 *
 * @param phi  the Phi node
 *
 * @return 1 if Cast's where moved
 */
static int concretize_Phi_type(ir_node *phi)
{
	int       n_preds = get_Phi_n_preds(phi);
	ir_node **pred    = ALLOCAN(ir_node*, n_preds);
	ir_node  *nn, *blk;
	ir_type  *totype;
	ir_type  *fromtype;
	int       i;

	if (n_preds == 0)
		return 0;
	pred[0] = get_Phi_pred(phi, 0);

	if (!is_Cast(pred[0]))
		return 0;

	if (!is_Cast_upcast(pred[0]))
		return 0;

	fromtype = get_irn_typeinfo_type(get_Cast_op(pred[0]));
	totype   = get_Cast_type(pred[0]);

	pred[0] = get_Cast_op(pred[0]);
	for (i = 1; i < n_preds; ++i) {
		pred[i] = get_Phi_pred(phi, i);
		if (!is_Cast(pred[i]))
			return 0;
		if (get_irn_typeinfo_type(get_Cast_op(pred[i])) != fromtype)
			return 0;
		pred[i] = get_Cast_op(pred[i]);
	}

	/* Transform Phi */
	blk = get_nodes_block(phi);
	nn  = new_r_Phi(current_ir_graph, blk, n_preds, pred, get_irn_mode(phi));
	set_irn_typeinfo_type(nn, fromtype);
	nn  = new_r_Cast(current_ir_graph, blk, nn, totype);
	set_irn_typeinfo_type(nn, totype);
	exchange(phi, nn);
	return 1;
}

/**
 * Remove Casted null checks, i.e.
 *
 * Cmp(Cast(type, x_orig), NULL_type) -> Cmp(x_orig, NULL_orig)
 *
 * @param cmp  the Cmp node
 *
 * @return 1 if Cast's where removed
 */
static int remove_Cmp_Null_cast(ir_node *cmp) {
	ir_node *cast, *null, *new_null;
	int     cast_pos, null_pos;
	ir_type *fromtype;
	ir_mode *mode;

	cast = get_Cmp_left(cmp);
	if (!is_Cast(cast)) {
		null     = cast;
		null_pos = 0;
		cast     = get_Cmp_right(cmp);
		cast_pos = 1;
		if (!is_Cast(cast))
			return 0;
	} else {
		null = get_Cmp_right(cmp);
		cast_pos = 0;
		null_pos = 1;
	}

	if (! is_Const(null))
		return 0;
	mode = get_irn_mode(null);
	if (!mode_is_reference(mode))
		return 0;
	if (get_Const_tarval(null) != get_mode_null(mode))
		return 0;

	/* Transform Cmp */
	set_irn_n(cmp, cast_pos, get_Cast_op(cast));
	fromtype = get_irn_typeinfo_type(get_Cast_op(cast));
	new_null = new_Const_type(get_Const_tarval(null), fromtype);
	set_irn_typeinfo_type(new_null, fromtype);
	set_irn_n(cmp, null_pos, new_null);
	++n_casts_removed;
	return 1;
}

/**
 * Post-Walker: Optimize class casts (mostly by trying to remove them)
 */
static void irn_optimize_class_cast(ir_node *n, void *env) {
	int *changed = env;

	if (is_Cast(n))
		*changed |= cancel_out_casts(n);
	else if (is_Sel(n))
		*changed |= concretize_selected_entity(n);
	else if (is_Phi(n))
		*changed |= concretize_Phi_type(n);
	else if (is_Cmp(n))
		*changed |= remove_Cmp_Null_cast(n);
}

void optimize_class_casts(void) {
	int changed;

	if (get_irp_typeinfo_state() != ir_typeinfo_consistent)
		simple_analyse_types();

	changed = 0;
	all_irg_walk(NULL, irn_optimize_class_cast, &changed);

	if (changed) {
		int i;

		set_trouts_inconsistent();
		for (i = get_irp_n_irgs() - 1; i >= 0; --i)
			set_irg_outs_inconsistent(get_irp_irg(i));
	}

	DB((dbg, SET_LEVEL_1, " Cast optimization: %d Casts removed, %d Sels concretized.\n",
		n_casts_removed, n_sels_concretized));
}

void firm_init_class_casts_opt(void) {
	FIRM_DBG_REGISTER(dbg, "firm.opt.tropt");
}
