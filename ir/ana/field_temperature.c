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

#include "field_temperature.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "entity_t.h"
#include "irgwalk.h"

#include "array.h"

/* *************************************************************************** */
/* initialize, global variables.                                               */
/* *************************************************************************** */

/* A list of Load and Store operations that have no analyseable address. */
static ir_node **unrecognized_access = NULL;

static void add_unrecognized_access(ir_node *n) {
  ARR_APP1(ir_node *, unrecognized_access, n);
}

/* *************************************************************************** */
/*   Access routines for entities                                              */
/* *************************************************************************** */

int get_entity_n_accesses(entity *ent) {
  assert(ent && is_entity(ent));

  if (!ent->accesses) { ent->accesses = NEW_ARR_F(ir_node *, 0); }

  return ARR_LEN(ent->accesses);
}

ir_node *get_entity_access(entity *ent, int pos) {
  assert(0 <= pos && pos < get_entity_n_accesses(ent));

  return ent->accesses[pos];
}

void add_entity_access(entity *ent, ir_node *n) {
  assert(ent && is_entity(ent));
  assert(n && is_ir_node(n));

  if (!ent->accesses) ent->accesses = NEW_ARR_F(ir_node *, 0);

  ARR_APP1(ir_node *, ent->accesses, n);
}

void set_entity_access(entity *ent, int pos, ir_node *n) {
  assert(0 <= pos && pos < get_entity_n_accesses(ent));
  assert(n && is_ir_node(n));

  ent->accesses[pos] = n;
}


/* *************************************************************************** */
/*   Access routines for types                                                 */
/* *************************************************************************** */

/** Number of Alloc nodes that create an instance of this type */
int get_type_n_allocations(type *tp) {
  assert(tp && is_type(tp));

  if (!tp->allocations) { tp->allocations = NEW_ARR_F(ir_node *, 0); }

  return ARR_LEN(tp->allocations);
}

/** Alloc node that create an instance of this type */
ir_node *get_type_allocation(type *tp, int pos) {
  assert(0 <= pos && pos < get_type_n_allocations(tp));

  return tp->allocations[pos];
}

void add_type_allocation(type *tp, ir_node *n) {
  assert(tp && is_type(tp));
  assert(n && is_ir_node(n));

  if (!tp->allocations) tp->allocations = NEW_ARR_F(ir_node *, 0);

  ARR_APP1(ir_node *, tp->allocations, n);
}

void set_type_allocation(type *tp, int pos, ir_node *n) {
  assert(0 <= pos && pos < get_type_n_allocations(tp));
  assert(n && is_ir_node(n));

  tp->allocations[pos] = n;
}




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

/* An addr node is a SymConst or a Sel. */
int get_addr_n_entities(ir_node *addr) {
  int n_ents;

  switch (get_irn_opcode(addr)) {
  case iro_Sel:
    /* Treat jack array sels? */
    n_ents = get_Sel_n_accessed_entities(addr);
    break;
  case iro_SymConst:
    if (get_SymConst_kind(addr) == symconst_addr_ent) {
      n_ents = 1;
      break;
    }
  default:
    //assert(0 && "unexpected address expression");
    n_ents = 0;
  }

  return n_ents;
}

/* An addr node is a SymConst or a Sel. */
entity *get_addr_entity(ir_node *addr, int pos) {
  entity *ent;

  switch (get_irn_opcode(addr)) {
  case iro_Sel:
    /* Treat jack array sels? */
    assert (0 <= pos && pos < get_Sel_n_accessed_entities(addr));
    ent = get_Sel_accessed_entity(addr, pos);
    break;
  case iro_SymConst:
    if (get_SymConst_kind(addr) == symconst_addr_ent) {
      assert(pos == 0);
      ent = get_SymConst_entity(addr);
      break;
    }
  default:
    ent = NULL;
  }

  return ent;
}


int get_irn_loop_call_depth(ir_node *n) {
  ir_graph *irg = get_irn_irg(n);
  return get_irg_loop_depth(irg);
}

int get_irn_loop_depth(ir_node *n) {
  return get_loop_depth(get_irn_loop(get_nodes_block(n)));
}

int get_irn_recursion_depth(ir_node *n) {
  ir_graph *irg = get_irn_irg(n);
  return get_irg_recursion_depth(irg);
}


/* *************************************************************************** */
/* The heuristic                                                               */
/* *************************************************************************** */

int get_weighted_loop_depth(ir_node *n) {
  int loop_call_depth = get_irn_loop_call_depth(n);
  int loop_depth      = get_irn_loop_depth(n);
  int recursion_depth = get_irn_recursion_depth(n);

  return loop_call_depth + loop_depth + recursion_depth;
}

/* *************************************************************************** */
/* The analyses                                                                */
/* *************************************************************************** */

void init_field_temperature(void) {
  assert(!unrecognized_access);
  unrecognized_access = NEW_ARR_F(ir_node *, 0);
}


void chain_accesses(ir_node *n, void *env) {
  int i, n_ents;
  ir_node *addr;

  if (get_irn_op(n) == op_Alloc) {
    add_type_allocation(get_Alloc_type(n), n);
    return;
  }

  if (is_memop(n)) {
    addr = get_memop_ptr(n);
  } else {
    return;
  }

  n_ents = get_addr_n_entities(addr);
  for (i = 0; i < n_ents; ++i) {
    entity *ent = get_addr_entity(addr, i);
    if (ent)
      add_entity_access(ent, n);
    else
      add_unrecognized_access(n);
  }
}


/* compute the field temperature. */
void compute_field_temperature(void) {

  int i, n_irgs = get_irp_n_irgs();

  init_field_temperature();

  for (i=0; i < n_irgs; i++) {
    current_ir_graph = get_irp_irg(i);
    irg_walk_graph(current_ir_graph, NULL, chain_accesses, NULL);
  }
}

/* free occupied memory, reset */
void free_field_temperature(void) {
  DEL_ARR_F(unrecognized_access);
  unrecognized_access = NULL;
}



/* *************************************************************************** */
/* Auxiliary                                                                   */
/* *************************************************************************** */


int is_jack_rts_class(type *t) {
  ident *name = get_type_ident(t);

  if (id_is_prefix(new_id_from_str("java/"), name)) return 1;
  if (id_is_prefix(new_id_from_str("["), name)) return 1;
  if (id_is_prefix(new_id_from_str("gnu/"), name)) return 1;
  if (id_is_prefix(new_id_from_str("java/"), name)) return 1;

  return 0;
}
