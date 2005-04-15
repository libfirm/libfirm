/*
 * Project:     libFIRM
 * File name:   ir/ana/field_temperature.c
 * Purpose:     Compute an estimate of field temperature, i.e., field access heuristic.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     21.7.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <math.h>

#include "field_temperature.h"

#include "trouts.h"
#include "execution_frequency.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "entity_t.h"
#include "irgwalk.h"

#include "array.h"

/* *************************************************************************** */
/* initialize, global variables.                                               */
/* *************************************************************************** */

/* *************************************************************************** */
/*   Access routines for irnodes                                               */
/* *************************************************************************** */

/* The entities that can be accessed by this Sel node. */
int get_Sel_n_accessed_entities(ir_node *sel) {
  return 1;
}

entity *get_Sel_accessed_entity(ir_node *sel, int pos) {
  return get_Sel_entity(sel);
}

/* *************************************************************************** */
/* The heuristic                                                               */
/* *************************************************************************** */

int get_irn_loop_call_depth(ir_node *n) {
  ir_graph *irg = get_irn_irg(n);
  return get_irg_loop_depth(irg);
}

int get_irn_loop_depth(ir_node *n) {
  ir_loop *l = get_irn_loop(get_nodes_block(n));
  if (l)
    return get_loop_depth(l);
  else
    return 0;
}

int get_irn_recursion_depth(ir_node *n) {
  ir_graph *irg = get_irn_irg(n);
  return get_irg_recursion_depth(irg);
}


/**   @@@ the second version of the heuristic. */
int get_weighted_loop_depth(ir_node *n) {
  int loop_call_depth = get_irn_loop_call_depth(n);
  int loop_depth      = get_irn_loop_depth(n);
  int recursion_depth = get_irn_recursion_depth(n);

  return loop_call_depth + loop_depth + recursion_depth;
}


/* *************************************************************************** */
/* The 2. heuristic                                                            */
/* *************************************************************************** */

static int default_recursion_weight = 5;

/* The final evaluation of a node.  In this function we can
   adapt the heuristic.  Combine execution freqency with
   recursion depth.
   @@@ the second version of the heuristic. */
double get_irn_final_cost(ir_node *n) {
  double cost_loop   = get_irn_exec_freq(n);
  double cost_method = get_irg_method_execution_frequency(get_irn_irg(n));
  int    rec_depth   = get_irn_recursion_depth(n);
  double cost_rec    = 0;
  if (rec_depth) cost_rec = pow(default_recursion_weight, rec_depth);
  return cost_loop*(cost_method + cost_rec);
}

double get_type_estimated_n_instances(type *tp) {
  int i, n_allocs = get_type_n_allocs(tp);
  double n_instances = 0;
  for (i = 0; i < n_allocs; ++i) {
    ir_node *alloc = get_type_alloc(tp, i);
    n_instances += get_irn_final_cost(alloc);
  }
  return n_instances;
}

double get_type_estimated_mem_consumption_bytes(type *tp) {
  assert(0);
  return 0.0;
}

int get_type_estimated_n_fields(type *tp) {
  int s = 0;
  switch(get_type_tpop_code(tp)) {

  case tpo_primitive:
  case tpo_pointer:
  case tpo_enumeration:
    s = 1;
    break;

  case tpo_class:
    s = 1; /* dispatch pointer */
    /* fall through */
  case tpo_struct: {
    int i, n_mem = get_compound_n_members(tp);
    for (i = 0; i < n_mem; ++i) {
      entity *mem = get_compound_member(tp, i);
      if (get_entity_allocation(mem) == allocation_automatic) {
	s += get_type_estimated_n_fields(get_entity_type(mem));
      }
    }
  } break;

  case tpo_array: {
    long n_elt = DEFAULT_N_ARRAY_ELEMENTS;
    assert(get_array_n_dimensions(tp) == 1 && "other not implemented");
    if ((get_irn_op(get_array_lower_bound(tp, 0)) == op_Const) &&
	(get_irn_op(get_array_upper_bound(tp, 0)) == op_Const)   ) {
      n_elt = get_array_upper_bound_int(tp, 0) - get_array_upper_bound_int(tp, 0);
    }
    s = n_elt;
  } break;

  default: DDMT(tp); assert(0);
  }

  return s;
}

int get_type_estimated_size_bytes(type *tp) {
  int s = 0;

  switch(get_type_tpop_code(tp)) {

  case tpo_primitive:
  case tpo_pointer:
  case tpo_enumeration:
    s = get_mode_size_bytes(get_type_mode(tp));
    break;

  case tpo_class:
    s = get_mode_size_bytes(mode_P_mach); /* dispatch pointer */
    /* fall through */
  case tpo_struct: {
    int i, n_mem = get_compound_n_members(tp);
    for (i = 0; i < n_mem; ++i) {
      entity *mem = get_compound_member(tp, i);
      s += get_type_estimated_size_bytes(get_entity_type(mem));

      if (get_entity_allocation(mem) == allocation_automatic) {
      } /* allocation_automatic */
    }
  } break;

  case tpo_array: {
    int elt_s = get_type_estimated_size_bytes(get_array_element_type(tp));
    long n_elt = DEFAULT_N_ARRAY_ELEMENTS;
    assert(get_array_n_dimensions(tp) == 1 && "other not implemented");
    if ((get_irn_op(get_array_lower_bound(tp, 0)) == op_Const) &&
	(get_irn_op(get_array_upper_bound(tp, 0)) == op_Const)   ) {
      n_elt = get_array_upper_bound_int(tp, 0) - get_array_lower_bound_int(tp, 0);
    }
    s = n_elt * elt_s;
    break;
  }

  default: DDMT(tp); assert(0);
  }

  return s;
}

double get_type_estimated_n_casts(type *tp) {
  int i, n_casts = get_type_n_casts(tp);
  double n_instances = 0;
  for (i = 0; i < n_casts; ++i) {
    ir_node *cast = get_type_cast(tp, i);
    n_instances += get_irn_final_cost(cast);
  }
  return n_instances;
}

double get_class_estimated_n_upcasts(type *clss) {
  double n_instances = 0;
  int i, j, n_casts, n_pointertypes;

  n_casts = get_type_n_casts(clss);
  for (i = 0; i < n_casts; ++i) {
    ir_node *cast = get_type_cast(clss, i);
    if (get_irn_opcode(cast) != iro_Cast) continue;  /* Could be optimized away. */

    if (is_Cast_upcast(cast))
      n_instances += get_irn_final_cost(cast);
  }

  n_pointertypes = get_type_n_pointertypes_to(clss);
  for (j = 0; j < n_pointertypes; ++j) {
    n_instances += get_class_estimated_n_upcasts(get_type_pointertype_to(clss, j));
  }

  return n_instances;
}

double get_class_estimated_n_downcasts(type *clss) {
  double n_instances = 0;
  int i, j, n_casts, n_pointertypes;

  n_casts = get_type_n_casts(clss);
  for (i = 0; i < n_casts; ++i) {
    ir_node *cast = get_type_cast(clss, i);
    if (get_irn_opcode(cast) != iro_Cast) continue;  /* Could be optimized away. */

    if (is_Cast_downcast(cast))
      n_instances += get_irn_final_cost(cast);
  }

  n_pointertypes = get_type_n_pointertypes_to(clss);
  for (j = 0; j < n_pointertypes; ++j) {
    n_instances += get_class_estimated_n_downcasts(get_type_pointertype_to(clss, j));
  }

  return n_instances;
}


double get_class_estimated_dispatch_writes(type *clss) {
  return get_type_estimated_n_instances(clss);
}

/** Returns the number of reads of the dispatch pointer. */
double get_class_estimated_dispatch_reads (type *clss) {
  int i, n_mems = get_class_n_members(clss);
  double n_calls = 0;
  for (i = 0; i < n_mems; ++i) {
    entity *mem = get_class_member(clss, i);
    n_calls += get_entity_estimated_n_dyncalls(mem);
  }
  return n_calls;
}

double get_class_estimated_n_dyncalls(type *clss) {
  return get_class_estimated_dispatch_reads(clss) +
         get_class_estimated_dispatch_writes(clss);
}

double get_entity_estimated_n_loads(entity *ent) {
  int i, n_acc = get_entity_n_accesses(ent);
  double n_loads = 0;
  for (i = 0; i < n_acc; ++i) {
    ir_node *acc = get_entity_access(ent, i);
    if (get_irn_op(acc) == op_Load) {
      n_loads += get_irn_final_cost(acc);
    }
  }
  return n_loads;
}

double get_entity_estimated_n_stores(entity *ent) {
  int i, n_acc = get_entity_n_accesses(ent);
  double n_stores = 0;
  for (i = 0; i < n_acc; ++i) {
    ir_node *acc = get_entity_access(ent, i);
    if (get_irn_op(acc) == op_Store)
      n_stores += get_irn_final_cost(acc);
  }
  return n_stores;
}

/* @@@ Should we evaluate the callee array?  */
double get_entity_estimated_n_calls(entity *ent) {
  int i, n_acc = get_entity_n_accesses(ent);
  double n_calls = 0;
  for (i = 0; i < n_acc; ++i) {
    ir_node *acc = get_entity_access(ent, i);
    if (get_irn_op(acc) == op_Call)

      n_calls += get_irn_final_cost(acc);
  }
  return n_calls;
}

double get_entity_estimated_n_dyncalls(entity *ent) {
  int i, n_acc = get_entity_n_accesses(ent);
  double n_calls = 0;
  for (i = 0; i < n_acc; ++i) {
    ir_node *acc = get_entity_access(ent, i);

    /* Call->Sel(ent) combination */
    if ((get_irn_op(acc) == op_Call)  &&
	(get_irn_op(get_Call_ptr(acc)) == op_Sel)) {
      n_calls += get_irn_final_cost(acc);

    /* MemOp->Sel combination for static, overwritten entities */
    } else if (is_memop(acc) && (get_irn_op(get_memop_ptr(acc)) == op_Sel)) {
      entity *ent = get_Sel_entity(get_memop_ptr(acc));
      if (is_Class_type(get_entity_owner(ent))) {
	/* We might call this for inner entities in compounds. */
	if (get_entity_n_overwrites(ent) > 0 ||
	    get_entity_n_overwrittenby(ent) > 0) {
	  n_calls += get_irn_final_cost(acc);
	}
      }
    }

  }
  return n_calls;
}

/* ------------------------------------------------------------------------- */
/* Auxiliary                                                                 */
/* ------------------------------------------------------------------------- */

int is_jack_rts_name(ident *name) {
  return  0;
  if (id_is_prefix(new_id_from_str("java/"), name)) return 1;
  if (id_is_prefix(new_id_from_str("["),     name)) return 1;
  if (id_is_prefix(new_id_from_str("gnu/"),  name)) return 1;
  if (id_is_prefix(new_id_from_str("java/"), name)) return 1;
  if (id_is_prefix(new_id_from_str("CStringToCoreString"), name)) return 1;

  return 0;
}


int is_jack_rts_class(type *t) {
  ident *name = get_type_ident(t);
  return is_jack_rts_name(name);
}

#include "entity_t.h"  // for the assertion.

int is_jack_rts_entity(entity *e) {
  ident *name;

  assert(e->ld_name);
  name = get_entity_ld_ident(e);

  return is_jack_rts_name(name);
}
