/*
 * Project:     libFIRM
 * File name:   ir/ana/trouts.c
 * Purpose:     Reverse edges that reference types/entities.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     29.10.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "trouts.h"

#include "array.h"
#include "pmap.h"

#include "irprog.h"
#include "irgwalk.h"

/**------------------------------------------------------------------*/
/* We represent the fields in entities/types by hashmaps.            */
/**------------------------------------------------------------------*/

static pmap *entity_access_map = NULL;
static pmap *entity_reference_map = NULL;
static pmap *type_alloc_map = NULL;

static ir_node **get_entity_access_array(entity *ent) {
  ir_node **res;
  if (!entity_access_map) entity_access_map = pmap_create();

  if (pmap_contains(entity_access_map, (void *)ent)) {
    res = (ir_node **) pmap_get(entity_access_map, (void *)ent);
  } else {
    res = NEW_ARR_F(ir_node *, 0);
    pmap_insert(entity_access_map, (void *)ent, (void *)res);
  }

  return res;
}
void set_entity_access_array(entity *ent, ir_node **accs) {
  ir_node **old = pmap_get(entity_access_map, (void *)ent);
  if (old != accs)
    pmap_insert(entity_access_map, (void *)ent, (void *)accs);
}

static ir_node **get_entity_reference_array(entity *ent) {
  ir_node **res;
  if (!entity_reference_map) entity_reference_map = pmap_create();

  if (pmap_contains(entity_reference_map, (void *)ent)) {
    res = (ir_node **) pmap_get(entity_reference_map, (void *)ent);
  } else {
    res = NEW_ARR_F(ir_node *, 0);
    pmap_insert(entity_reference_map, (void *)ent, (void *)res);
  }

  return res;
}
void set_entity_reference_array(entity *ent, ir_node **refs) {
  ir_node **old = pmap_get(entity_reference_map, (void *)ent);
  if (old != refs)
    pmap_insert(entity_reference_map, (void *)ent, (void *)refs);
}

static ir_node **get_type_alloc_array(type *tp) {
  ir_node **res;
  if (!type_alloc_map) type_alloc_map = pmap_create();

  if (pmap_contains(type_alloc_map, (void *)tp)) {
    res = (ir_node **) pmap_get(type_alloc_map, (void *)tp);
  } else {
    res = NEW_ARR_F(ir_node *, 0);
    pmap_insert(type_alloc_map, (void *)tp, (void *)res);
  }

  return res;
}
void set_type_alloc_array(type *tp, ir_node **alls) {
  ir_node **old = pmap_get(type_alloc_map, (void *)tp);
  if (old != alls)
    pmap_insert(type_alloc_map, (void *)tp, (void *)alls);
}


/*------------------------------------------------------------------*/
/* Accessing the out datastructures.                                */
/* These routines only work properly if firm is in state            */
/* trouts_consistent or trouts_inconsistent.                        */
/*------------------------------------------------------------------*/

/**------------------------------------------------------------------*/
/*   Access routines for entities                                    */
/**------------------------------------------------------------------*/

int get_entity_n_accesses(entity *ent) {
  assert(ent && is_entity(ent));

  ir_node ** accs = (ir_node **)get_entity_access_array(ent);
  return ARR_LEN(accs);
}

ir_node *get_entity_access(entity *ent, int pos) {
  assert(0 <= pos && pos < get_entity_n_accesses(ent));

  ir_node ** accs = (ir_node **)get_entity_access_array(ent);
  return accs[pos];
}

void add_entity_access(entity *ent, ir_node *n) {
  assert(ent && is_entity(ent));
  assert(n && is_ir_node(n));

  ir_node ** accs = (ir_node **)get_entity_access_array(ent);
  ARR_APP1(ir_node *, accs, n);
  set_entity_access_array(ent, accs);
}

void set_entity_access(entity *ent, int pos, ir_node *n) {
  assert(0 <= pos && pos < get_entity_n_accesses(ent));
  assert(n && is_ir_node(n));

  ir_node ** accs = (ir_node **)get_entity_access_array(ent);
  accs[pos] = n;
}

/**------------------------------------------------------------------*/

int get_entity_n_references(entity *ent) {
  assert(ent && is_entity(ent));

  ir_node ** refs = (ir_node **)get_entity_reference_array(ent);
  return ARR_LEN(refs);
}

ir_node *get_entity_reference(entity *ent, int pos) {
  assert(0 <= pos && pos < get_entity_n_references(ent));

  ir_node ** refs = (ir_node **)get_entity_reference_array(ent);
  return refs[pos];
}

void add_entity_reference(entity *ent, ir_node *n) {
  assert(ent && is_entity(ent));
  assert(n && is_ir_node(n));

  ir_node ** refs = (ir_node **)get_entity_reference_array(ent);
  ARR_APP1(ir_node *, refs, n);
  set_entity_reference_array(ent, refs);
}

void set_entity_reference(entity *ent, int pos, ir_node *n) {
  assert(0 <= pos && pos < get_entity_n_references(ent));
  assert(n && is_ir_node(n));

  ir_node ** refs = (ir_node **)get_entity_reference_array(ent);
  refs[pos] = n;
}


/**------------------------------------------------------------------*/
/*   Access routines for types                                       */
/**------------------------------------------------------------------*/

/* Number of Alloc nodes that create an instance of this type */
int get_type_n_allocations(type *tp) {
  assert(tp && is_type(tp));

  ir_node **allocs = get_type_alloc_array(tp);
  return ARR_LEN(allocs);
}

/* Alloc node that creates an instance of this type */
ir_node *get_type_allocation(type *tp, int pos) {
  assert(0 <= pos && pos < get_type_n_allocations(tp));

  ir_node **allocs = get_type_alloc_array(tp);
  return allocs[pos];
}

void add_type_allocation(type *tp, ir_node *n) {
  assert(tp && is_type(tp));
  assert(n && is_ir_node(n));

  ir_node **allocs = get_type_alloc_array(tp);
  ARR_APP1(ir_node *, allocs, n);
  set_type_alloc_array(tp, allocs);
}

void set_type_allocation(type *tp, int pos, ir_node *n) {
  assert(0 <= pos && pos < get_type_n_allocations(tp));
  assert(n && is_ir_node(n));

  ir_node **allocs = get_type_alloc_array(tp);
  allocs[pos] = n;
}

/*------------------------------------------------------------------*/
/* Building and Removing the out datastructure                      */
/*------------------------------------------------------------------*/

static void init_trouts(void) {
}

/* The entities that can be accessed by this Sel node. */
static int get_Sel_n_accessed_entities(ir_node *sel) {
  return 1;
}

static entity *get_Sel_accessed_entity(ir_node *sel, int pos) {
  return get_Sel_entity(sel);
}

/* An addr node is a SymConst or a Sel. */
static int get_addr_n_entities(ir_node *addr) {
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
static entity *get_addr_entity(ir_node *addr, int pos) {
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

static void chain_accesses(ir_node *n, void *env) {
  int i, n_ents;
  ir_node *addr;

  if (get_irn_op(n) == op_Alloc) {
    add_type_allocation(get_Alloc_type(n), n);
    return;
  } else

  if (get_irn_op(n) == op_Sel) {
    add_entity_reference(get_Sel_entity(n), n);
    return;
  } else if (get_irn_op(n) == op_SymConst && (get_SymConst_kind(n) == symconst_addr_ent)) {
    add_entity_reference(get_SymConst_entity(n), n);
    return;
  } else

  if (is_memop(n)) {
    addr = get_memop_ptr(n);
  } else if (get_irn_op(n) == op_Call) {
    addr = get_Call_ptr(n);
    if (get_irn_op(addr) != op_Sel) return;  /* Sels before Calls mean a Load / polymorphic Call. */
  } else {
    return;
  }

  n_ents = get_addr_n_entities(addr);
  for (i = 0; i < n_ents; ++i) {
    entity *ent = get_addr_entity(addr, i);
    if (ent)
      add_entity_access(ent, n);
    //else
      //add_unrecognized_access(n);
  }
}

/* compute the field temperature. */
void compute_trouts(void) {
  int i, n_irgs = get_irp_n_irgs();

  init_trouts();

  for (i=0; i < n_irgs; i++) {
    current_ir_graph = get_irp_irg(i);
    irg_walk_graph(current_ir_graph, NULL, chain_accesses, NULL);
  }
  walk_const_code(NULL, chain_accesses, NULL);
}


void free_trouts(void) {
  if (entity_access_map) {
    ir_node **accs;
    for (accs = (ir_node **)pmap_first(entity_access_map);
	 accs;
	 accs = (ir_node **)pmap_next(entity_access_map))
      ; //DEL_ARR_F(accs);
    pmap_destroy(entity_access_map);
    entity_access_map = NULL;
  }
  if (entity_reference_map) {
    ir_node **refs;
    for (refs = (ir_node **)pmap_first(entity_reference_map);
	 refs;
	 refs = (ir_node **)pmap_next(entity_reference_map))
      ; //DEL_ARR_F(refs);
    pmap_destroy(entity_reference_map);
    entity_reference_map = NULL;
  }
  if (type_alloc_map) {
    ir_node **alls;
    for (alls = (ir_node **)pmap_first(type_alloc_map);
	 alls;
	 alls = (ir_node **)pmap_next(type_alloc_map))
      ; //DEL_ARR_F(alls);
    pmap_destroy(type_alloc_map);
    type_alloc_map = NULL;
  }
}
