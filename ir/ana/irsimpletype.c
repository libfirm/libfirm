/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief     Run most simple type analyses.
 * @author    Goetz Lindenmaier
 * @date      22.8.2003
 * @version   $Id$
 * @summary
 *  Runs most simple type analyses.
 *
 *  We compute type information for each node.  It is derived from the
 *  types of the origines of values, e.g. parameter types can be derived
 *  from the method type.
 *  The type information so far is saved in the link field.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irtypeinfo.h"
#include "irsimpletype.h"

#include "irnode_t.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "ident.h"
#include "trouts.h"

#define VERBOSE_UNKNOWN_TYPE(s) printf s

static ir_type *phi_cycle_type = NULL;


/* ------------ Building and Removing the type information  ----------- */


/**
 * init type link field so that types point to their pointers.
 */
static void precompute_pointer_types(void) {
#if 0
  int i;
  set_type_link(firm_unknown_type, firm_unknown_type);
  set_type_link(firm_none_type,    firm_unknown_type);

  for (i = get_irp_n_types() - 1; i >= 0; --i)
    set_type_link(get_irp_type(i), (void *)firm_unknown_type);

  for (i = get_irp_n_types() - 1; i >= 0; --i) {
    ir_type *tp = get_irp_type(i);
    if (is_Pointer_type(tp))
      set_type_link(get_pointer_points_to_type(tp), (void *)tp);
  }
#else
  compute_trouts();
#endif
}

/**
 * Returns a pointer to type which was stored in the link field
 * to speed up search.
 */
static ir_type *find_pointer_type_to (ir_type *tp) {
#if 0
  return (ir_type *)get_type_link(tp);
#else
  if (get_type_n_pointertypes_to(tp) > 0)
    return get_type_pointertype_to(tp, 0);
  else
    return firm_unknown_type;
#endif
}

static ir_type *compute_irn_type(ir_node *n);

/**
 * Try to determine a type for a Proj node.
 * If a type cannot be determined, return @p firm_none_type.
 */
static ir_type *find_type_for_Proj(ir_node *n) {
  ir_type *tp;

  /* Avoid nested Tuples. */
  ir_node *pred = skip_Tuple(get_Proj_pred(n));
  ir_mode *m = get_irn_mode(n);

  if (m == mode_T  ||
      m == mode_BB ||
      m == mode_X  ||
      m == mode_M  ||
      m == mode_b    )
    return firm_none_type;

  switch (get_irn_opcode(pred)) {
  case iro_Proj: {
    ir_node *pred_pred;
    /* Deal with Start / Call here: we need to know the Proj Nr. */
    assert(get_irn_mode(pred) == mode_T);
    pred_pred = get_Proj_pred(pred);
    if (get_irn_op(pred_pred) == op_Start)  {
      ir_type *mtp = get_entity_type(get_irg_entity(get_irn_irg(pred_pred)));
      tp = get_method_param_type(mtp, get_Proj_proj(n));
    } else if (get_irn_op(pred_pred) == op_Call) {
      ir_type *mtp = get_Call_type(pred_pred);
      tp = get_method_res_type(mtp, get_Proj_proj(n));
    } else if (get_irn_op(pred_pred) == op_Tuple) {
      assert(0 && "Encountered nested Tuple");
    } else {
      VERBOSE_UNKNOWN_TYPE(("Proj %ld from Proj from ??: unknown type\n", get_irn_node_nr(n)));
      tp = firm_unknown_type;
    }
  } break;
  case iro_Start: {
    /* frame pointer, globals and tls */
    switch (get_Proj_proj(n)) {
    case pn_Start_P_frame_base:
      tp = find_pointer_type_to(get_irg_frame_type(get_irn_irg(pred)));
      break;
    case pn_Start_P_globals:
      tp = find_pointer_type_to(get_glob_type());
      break;
    case pn_Start_P_tls:
      tp = find_pointer_type_to(get_tls_type());
      break;
    case pn_Start_P_value_arg_base:
      VERBOSE_UNKNOWN_TYPE(("Value arg base proj %ld from Start: unknown type\n", get_irn_node_nr(n)));
      tp =  firm_unknown_type; /* find_pointer_type_to(get....(get_entity_type(get_irg_entity(get_irn_irg(pred))))); */
      break;
    default:
      VERBOSE_UNKNOWN_TYPE(("Proj %ld %ld from Start: unknown type\n", get_Proj_proj(n), get_irn_node_nr(n)));
      tp = firm_unknown_type;
    }
  } break;
  case iro_Call: {
    /* value args pointer */
    if (get_Proj_proj(n) == pn_Call_P_value_res_base) {
      VERBOSE_UNKNOWN_TYPE(("Value res base Proj %ld from Call: unknown type\n", get_irn_node_nr(n)));
      tp = firm_unknown_type; /* find_pointer_type_to(get....get_Call_type(pred)); */
    } else {
      VERBOSE_UNKNOWN_TYPE(("Proj %ld %ld from Call: unknown type\n", get_Proj_proj(n), get_irn_node_nr(n)));
      tp = firm_unknown_type;
    }
  } break;
  case iro_Tuple: {
    tp = compute_irn_type(get_Tuple_pred(pred, get_Proj_proj(n)));
  } break;
  default:
    tp = compute_irn_type(pred);
  }

  return tp;
}

/**
 * Try to determine the type of a node.
 * If a type cannot be determined, return @p firm_none_type.
 */
static ir_type *find_type_for_node(ir_node *n) {
  ir_type *tp = firm_unknown_type;
  ir_type *tp1 = NULL, *tp2 = NULL;
  ir_node *a = NULL, *b = NULL;

  /* DDMN(n); */

  if (is_unop(n)) {
    a = get_unop_op(n);
    tp1 = compute_irn_type(a);
  }
  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
    tp1 = compute_irn_type(a);
    tp2 = compute_irn_type(b);
  }

  switch (get_irn_opcode(n)) {

  case iro_InstOf: {
    assert(0 && "op_InstOf not supported");
  } break;

  /* has no type */
  case iro_Return: {
    /* Check returned type. */
    /*
    int i;
    ir_type *meth_type = get_entity_type(get_irg_entity(current_ir_graph));
    for (i = 0; i < get_method_n_ress(meth_type); i++) {
      ir_type *res_type = get_method_res_type(meth_type, i);
      ir_type *ana_res_type = get_irn_type(get_Return_res(n, i));
      if (ana_res_type == firm_unknown_type) continue;
      if (res_type != ana_res_type && "return value has wrong type") {
        DDMN(n);
        assert(res_type == ana_res_type && "return value has wrong type");
      }
    }
    */
  }
  case iro_Block:
  case iro_Start:
  case iro_End:
  case iro_Jmp:
  case iro_Cond:
  case iro_Raise:
  case iro_Call:
  case iro_Cmp:
  case iro_Store:
  case iro_Free:
  case iro_Sync:
  case iro_Tuple:
  case iro_Bad:
  case iro_NoMem:
  case iro_Break:
  case iro_CallBegin:
  case iro_EndReg:
  case iro_EndExcept:
    break;

  /* compute the type */
  case iro_Const:  tp = get_Const_type(n); break;
  case iro_SymConst:
    tp = get_SymConst_value_type(n); break;
  case iro_Sel:
    tp = find_pointer_type_to(get_entity_type(get_Sel_entity(n))); break;
  /* asymmetric binops */
  case iro_Shl:
  case iro_Shr:
  case iro_Shrs:
  case iro_Rot:
    tp = tp1;  break;
  case iro_Cast:
    tp = get_Cast_type(n);  break;
  case iro_Phi: {
    int i;
    int n_preds = get_Phi_n_preds(n);

    if (n_preds == 0)
      break;

    /* initialize this Phi */
    set_irn_typeinfo_type(n, phi_cycle_type);

    /* find a first real type */
    for (i = 0; i < n_preds; ++i) {
      tp1 = compute_irn_type(get_Phi_pred(n, i));
      assert(tp1 != initial_type);
      if ((tp1 != phi_cycle_type) && (tp1 != firm_none_type))
        break;
    }

    /* find a second real type */
    tp2 = tp1;
    for (; (i < n_preds); ++i) {
      tp2 = compute_irn_type(get_Phi_pred(n, i));
      if ((tp2 == phi_cycle_type) || (tp2 == firm_none_type)) {
        tp2 = tp1;
        continue;
      }
      if (tp2 != tp1) break;
    }

    /* printf("Types in Phi %s and %s \n", get_type_name(tp1), get_type_name(tp2)); */

    if (tp1 == tp2) { tp = tp1; break; }

    if (get_firm_verbosity() > 55) {  // Do not commit 55! should be 1.
      VERBOSE_UNKNOWN_TYPE(("Phi %ld with two different types: %s, %s: unknown type.\n", get_irn_node_nr(n),
			    get_type_name(tp1), get_type_name(tp2)));
    }
    tp = firm_unknown_type;   /* Test for supertypes? */
  } break;
  case iro_Load: {
    ir_node *a = get_Load_ptr(n);
    if (is_Sel(a))
      tp = get_entity_type(get_Sel_entity(a));
    else if (is_Pointer_type(compute_irn_type(a))) {
      tp = get_pointer_points_to_type(get_irn_typeinfo_type(a));
      if (is_Array_type(tp))
        tp = get_array_element_type(tp);
    } else {
      VERBOSE_UNKNOWN_TYPE(("Load %ld with typeless address. result: unknown type\n", get_irn_node_nr(n)));
    }
  } break;
  case iro_Alloc:
    tp = find_pointer_type_to(get_Alloc_type(n));  break;
  case iro_Proj:
    tp = find_type_for_Proj(n); break;
  case iro_Id:
    tp = compute_irn_type(get_Id_pred(n)); break;
 case iro_Unknown:
    tp = firm_unknown_type;  break;
 case iro_Filter:
    assert(0 && "Filter not implemented"); break;

  /* catch special cases with fallthrough to binop/unop cases in default. */
  case iro_Sub: {
    if (mode_is_int(get_irn_mode(n))       &&
      mode_is_reference(get_irn_mode(a)) &&
      mode_is_reference(get_irn_mode(b))   ) {
      VERBOSE_UNKNOWN_TYPE(("Sub %ld ptr - ptr = int: unknown type\n", get_irn_node_nr(n)));
      tp =  firm_unknown_type; break;
    }
  } /* fall through to Add. */
  case iro_Add: {
    if (mode_is_reference(get_irn_mode(n)) &&
        mode_is_reference(get_irn_mode(a)) &&
        mode_is_int(get_irn_mode(b))         ) {
      tp = tp1; break;
    }
    if (mode_is_reference(get_irn_mode(n)) &&
        mode_is_int(get_irn_mode(a))       &&
        mode_is_reference(get_irn_mode(b))    ) {
      tp = tp2; break;
    }
    goto default_code;
  } break;
  case iro_Mul: {
    if (get_irn_mode(n) != get_irn_mode(a)) {
      VERBOSE_UNKNOWN_TYPE(("Mul %ld int1 * int1 = int2: unknown type\n", get_irn_node_nr(n)));
      tp = firm_unknown_type; break;
    }
    goto default_code;
  } break;
  case iro_Mux: {
    a = get_Mux_true(n);
    b = get_Mux_false(n);
    tp1 = compute_irn_type(a);
    tp2 = compute_irn_type(b);
    if (tp1 == tp2)
      tp = tp1;
  } break;
  case iro_Psi: {
    int i, n_conds = get_Psi_n_conds(n);
    tp1 = compute_irn_type(get_Psi_default(n));

    for (i = 0; i < n_conds; ++i) {
      tp2 = compute_irn_type(get_Psi_val(n, i));
      if (tp2 != tp1)
        break;
    }
    if (tp1 == tp2)
      tp = tp1;
  } break;
  case iro_Bound:
    tp = compute_irn_type(get_Bound_index(n));
    break;
  case iro_Confirm:
    tp = compute_irn_type(get_Confirm_value(n));
    break;
  case iro_Conv:
    /* Conv is a unop, but changing the mode implies
       changing the type. */
    break;

  default:
default_code: {

    if (is_unop(n)) {
      /* It's not proper to walk past a Conv, so this case is handled above. */
      tp = tp1;
      break;
    }

    if (is_binop(n)) {
      if (tp1 == tp2) {
	      tp = tp1;
	      break;
      }
      if((tp1 == phi_cycle_type) || (tp2 == phi_cycle_type)) {
	      tp = phi_cycle_type;
	      break;
      }
      if (get_firm_verbosity() > 55) {
        VERBOSE_UNKNOWN_TYPE(("Binop %ld with two different types: %s, %s: unknown type \n", get_irn_node_nr(n),
			      get_type_name(tp1), get_type_name(tp2)));
      }
      tp = firm_unknown_type;
      break;
    }

    printf(" not implemented: "); DDMN(n);
  } break; /* default */
  } /* end switch */

  /* printf (" found %s ", get_type_name(tp)); DDM; */

  return tp;
}

/** Compute the type of an IR node. */
static ir_type *compute_irn_type(ir_node *n) {
  ir_type *tp = get_irn_typeinfo_type(n);

  if (tp == initial_type) {
    tp = find_type_for_node(n);
    set_irn_typeinfo_type(n, tp);
  }

  /* printf (" found %s ", get_type_name(tp)); DDM; */

  return tp;
}

/**
 * Post-Walker: computes the type for every node
 * and store it into a map.
 * Post-walking ensures that the types for all predecessor
 * nodes are already computed.
 */
static void compute_type(ir_node *n, void *env) {

  ir_type *tp = get_irn_typeinfo_type(n);
  if (tp ==  phi_cycle_type) {
    /* printf(" recomputing for phi_cycle_type "); DDMN(n); */
    set_irn_typeinfo_type(n, initial_type);
  }
  compute_irn_type(n);
}

/**
 * Compute the types for all nodes of a graph.
 */
static void analyse_irg (ir_graph *irg) {
  set_irg_typeinfo_state(irg, ir_typeinfo_consistent);
  irg_walk_graph(irg, NULL, compute_type, NULL);
}

/**
 * Initialize the analysis by creating a phi_cycle_type and
 * computing pointer types for all class and struct types.
 */
static void init_irsimpletype(void) {
  init_irtypeinfo();
  if (!phi_cycle_type)
    phi_cycle_type = new_type_class(new_id_from_str("phi_cycle_type"));
  precompute_pointer_types();
}

/* Computes type information for each node in all ir graphs. */
void simple_analyse_types(void) {
  int i;
  init_irsimpletype();
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph *irg = get_irp_irg(i);
    analyse_irg(irg);
  }
  set_irp_typeinfo_state(ir_typeinfo_consistent);
}

void free_simple_type_information(void) {
  free_irtypeinfo();

  if (phi_cycle_type) {
    free_type(phi_cycle_type);
    phi_cycle_type = NULL;
  }
  set_irp_typeinfo_state(ir_typeinfo_none);
}
