/**
 *
 * @file irsimpeltype.c
 *
 * Project:     libFIRM
 * File name:   ir/ana/irsimpletype.c
 * Purpose:     Run most simple type analyses.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     22.8.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 *
 * We compute type information for each node.  It is derived from the
 * types of the origines of values, e.g. parameter types can be derived
 * from the method type.
 * The type information so far is saved in the link field.
 *
 */


# include "irtypeinfo.h"
# include "irsimpletype.h"

# include "irnode_t.h"
# include "irprog.h"
# include "irgwalk.h"
# include "ident.h"

#define VERBOSE_UNKNOWN_TYPE(s) printf s

static type *phi_cycle_type = NULL;


/* ------------ Building and Removing the type information  ----------- */


/* init type link field so that types point to their pointers. */
void precompute_pointer_types(void) {
  int i;
  set_type_link(unknown_type, unknown_type);
  set_type_link(none_type,    unknown_type);
  for (i = 0; i < get_irp_n_types(); ++i)
    set_type_link(get_irp_type(i), (void *)unknown_type);
  for (i = get_irp_n_types()-1; i>=0; --i) {
    type *tp = get_irp_type(i);
    if (is_pointer_type(tp))
      set_type_link(get_pointer_points_to_type(tp), (void *)tp);
  }
}

/* Store pointer in link to speed up search of pointer type. */
static type *find_pointer_type_to (type *tp) {
  return (type *)get_type_link(tp);
}

static type* compute_irn_type(ir_node *n);

static type *find_type_for_Proj(ir_node *n) {
  type *tp;
  ir_node *pred = skip_Tuple(get_Proj_pred(n));
  ir_mode *m = get_irn_mode(n);

  if (m == mode_T  ||
      m == mode_BB ||
      m == mode_X  ||
      m == mode_M  ||
      m == mode_b    )
    return none_type;

  switch (get_irn_opcode(pred)) {
  case iro_Proj: {
    ir_node *pred_pred;
    /* Deal with Start / Call here: we need to know the Proj Nr. */
    assert(get_irn_mode(pred) == mode_T);
    pred_pred = get_Proj_pred(pred);
    if (get_irn_op(pred_pred) == op_Start)  {
      type *mtp = get_entity_type(get_irg_ent(get_Start_irg(pred_pred)));
      tp = get_method_param_type(mtp, get_Proj_proj(n));
    } else if (get_irn_op(pred_pred) == op_Call) {
      type *mtp = get_Call_type(pred_pred);
      tp = get_method_res_type(mtp, get_Proj_proj(n));
    } else {
      VERBOSE_UNKNOWN_TYPE(("Proj %ld from Proj from ??: unknown type\n", get_irn_node_nr(n)));
      tp = unknown_type;
    }
  } break;
  case iro_Start: {
    /* globals and frame pointer */
    if (get_Proj_proj(n) == pns_frame_base)
      tp = find_pointer_type_to(get_irg_frame_type(get_Start_irg(pred)));
    else if (get_Proj_proj(n) == pns_globals)
      tp = find_pointer_type_to(get_glob_type());
    else  if (get_Proj_proj(n) == pns_value_arg_base) {
      VERBOSE_UNKNOWN_TYPE(("Value arg base proj %ld from Start: unknown type\n", get_irn_node_nr(n)));
      tp =  unknown_type; /* find_pointer_type_to(get....(get_entity_type(get_irg_ent(get_Start_irg(pred))))); */
    } else {
      VERBOSE_UNKNOWN_TYPE(("Proj %ld %ld from Start: unknown type\n", get_Proj_proj(n), get_irn_node_nr(n)));
      tp = unknown_type;
    }
  } break;
  case iro_Call: {
    /* value args pointer */
    if (get_Proj_proj(n) == pncl_value_res_base) {
      VERBOSE_UNKNOWN_TYPE(("Value res base Proj %ld from Call: unknown type\n", get_irn_node_nr(n)));
      tp = unknown_type; /* find_pointer_type_to(get....get_Call_type(pred)); */
    } else {
      VERBOSE_UNKNOWN_TYPE(("Proj %ld %ld from Call: unknown type\n", get_Proj_proj(n), get_irn_node_nr(n)));
      tp = unknown_type;
    }
  } break;
  default:
    tp = compute_irn_type(pred);
  }

  return tp;
}

static type *find_type_for_node(ir_node *n) {
  type *tp = NULL;
  type *tp1 = NULL, *tp2 = NULL;
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

  switch(get_irn_opcode(n)) {

  case iro_InstOf: {
    assert(0 && "op_InstOf not supported");
  } break;

  /* has no type */
  case iro_Return: {
    /* Check returned type. */
    /*
    int i;
    type *meth_type = get_entity_type(get_irg_ent(current_ir_graph));
    for (i = 0; i < get_method_n_ress(meth_type); i++) {
      type *res_type = get_method_res_type(meth_type, i);
      type *ana_res_type = get_irn_type(get_Return_res(n, i));
      if (ana_res_type == unknown_type) continue;
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
  case iro_Break:
  case iro_CallBegin:
  case iro_EndReg:
  case iro_EndExcept:
    tp = none_type; break;

  /* compute the type */
  case iro_Const:  tp = get_Const_type(n); break;
  case iro_SymConst:
    tp = unknown_type; break;
  case iro_Sel:
    tp = find_pointer_type_to(get_entity_type(get_Sel_entity(n))); break;
  /* assymetric binops */
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

    if (n_preds == 0)  {tp = none_type; break; }

    /* initialize this Phi */
    set_irn_type(n, phi_cycle_type);

    /* find a first real type */
    for (i = 0; i < n_preds; ++i) {
      tp1 = compute_irn_type(get_Phi_pred(n, i));
      assert(tp1 != initial_type);
      if ((tp1 != phi_cycle_type) && (tp1 != none_type))
    break;
    }

    /* find a second real type */
    tp2 = tp1;
    for (; (i < n_preds); ++i) {
      tp2 = compute_irn_type(get_Phi_pred(n, i));
      if ((tp2 == phi_cycle_type) || (tp2 == none_type)) {
    tp2 = tp1;
    continue;
      }
      if (tp2 != tp1) break;
    }

    /* printf("Types in Phi %s and %s \n", get_type_name(tp1), get_type_name(tp2)); */

    if (tp1 == tp2) { tp = tp1; break; }

    VERBOSE_UNKNOWN_TYPE(("Phi %ld with two different types: %s, %s: unknown type.\n", get_irn_node_nr(n),
              get_type_name(tp1), get_type_name(tp2)));
    tp = unknown_type;
  } break;
  case iro_Load: {
    ir_node *a = get_Load_ptr(n);
    if (get_irn_op(a) == op_Sel)
      tp = get_entity_type(get_Sel_entity(a));
    else if ((get_irn_op(a) == op_Const) &&
    (tarval_is_entity(get_Const_tarval(a))))
      tp = get_entity_type(tarval_to_entity(get_Const_tarval(a)));
    else if (is_pointer_type(compute_irn_type(a))) {
      tp = get_pointer_points_to_type(get_irn_type(a));
      if (is_array_type(tp)) tp = get_array_element_type(tp);
    } else {
      VERBOSE_UNKNOWN_TYPE(("Load %ld with typeless address. result: unknown type\n", get_irn_node_nr(n)));
      tp = unknown_type;
    }
  } break;
  case iro_Alloc:
    tp = find_pointer_type_to(get_Alloc_type(n));  break;
  case iro_Proj:
    tp = find_type_for_Proj(n); break;
  case iro_Id:
    tp = compute_irn_type(get_Id_pred(n)); break;
 case iro_Unknown:
    tp = unknown_type;  break;
 case iro_Filter:
    assert(0 && "Filter not implemented"); break;

  /* catch special cases with fallthrough to binop/unop cases in default. */
  case iro_Sub: {
    if (mode_is_int(get_irn_mode(n))       &&
    mode_is_reference(get_irn_mode(a)) &&
    mode_is_reference(get_irn_mode(b))   ) {
      VERBOSE_UNKNOWN_TYPE(("Sub %ld ptr - ptr = int: unknown type\n", get_irn_node_nr(n)));
      tp =  unknown_type; break;
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
      tp = unknown_type; break;
    }
    goto default_code;
  } break;

  default:
  default_code: {

    if (is_unop(n)) {
      /* Is is proper to walk past a Conv??? */
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
      VERBOSE_UNKNOWN_TYPE(("Binop %ld with two different types: %s, %s: unknown type \n", get_irn_node_nr(n),
                get_type_name(tp1), get_type_name(tp2)));
      tp = unknown_type;
      break;
    }

    printf(" not implemented: "); DDMN(n);
  } break; /* default */
  } /* end switch */

  /* printf (" found %s ", get_type_name(tp)); DDM; */

  return tp;
}


static type* compute_irn_type(ir_node *n) {
  /* DDMN(n); */

  type *tp = get_irn_type(n);

  if ((tp == initial_type)) {
    tp = find_type_for_node(n);
    set_irn_type(n, tp);
  }

  /* printf (" found %s ", get_type_name(tp)); DDM; */

  return tp;
}

static void compute_type(ir_node *n, void *env) {

  type *tp = get_irn_type(n);
  if (tp ==  phi_cycle_type) {
    /* printf(" recomputing for phi_cycle_type "); DDMN(n); */
    set_irn_type(n, initial_type);
  }

  compute_irn_type(n);
}

static void analyse_irg (ir_graph *irg) {
  set_irg_typeinfo_state(irg, irg_typeinfo_consistent);
  irg_walk_graph(irg, NULL, compute_type, NULL);
}

static void init_irsimpletype(void) {
  init_irtypeinfo();
  if (!phi_cycle_type)
    phi_cycle_type = new_type_class(new_id_from_str("phi_cycle_type"));
  precompute_pointer_types();
}

void simple_analyse_types(void) {
  int i;
  init_irsimpletype();
  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);
    analyse_irg(current_ir_graph);
  }
}

void free_simple_type_information(void) {
  free_irtypeinfo();

  if (phi_cycle_type) {
    free_type(phi_cycle_type);
    phi_cycle_type = NULL;
  }
}
