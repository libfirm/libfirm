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
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "trouts.h"

#include "array.h"
#include "pmap.h"

#include "irprog_t.h"
#include "irgwalk.h"
#include "irnode.h"


/*------------------------------------------------------------------*/
/* We represent the fields in entities/types by hashmaps.           */
/*------------------------------------------------------------------*/

static pmap *entity_access_map = NULL;
static pmap *entity_reference_map = NULL;
static pmap *type_alloc_map = NULL;
static pmap *type_cast_map = NULL;
static pmap *type_pointertype_map = NULL;
static pmap *type_arraytype_map = NULL;

/**
 * Return a flexible array containing all IR-nodes
 * that access a given entity.
 */
static ir_node **get_entity_access_array(ir_entity *ent) {
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
void set_entity_access_array(ir_entity *ent, ir_node **accs) {
  ir_node **old = pmap_get(entity_access_map, (void *)ent);
  if (old != accs)
    pmap_insert(entity_access_map, (void *)ent, (void *)accs);
}

/**
 * Return a flexible array containing all IR-nodes
 * that reference a given entity.
 */
static ir_node **get_entity_reference_array(ir_entity *ent) {
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
void set_entity_reference_array(ir_entity *ent, ir_node **refs) {
  ir_node **old = pmap_get(entity_reference_map, (void *)ent);
  if (old != refs)
    pmap_insert(entity_reference_map, (void *)ent, (void *)refs);
}

/**
 * Return a flexible array containing all IR-nodes
 * that allocate a given type.
 */
static ir_node **get_type_alloc_array(ir_type *tp) {
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
void set_type_alloc_array(ir_type *tp, ir_node **alls) {
  ir_node **old = pmap_get(type_alloc_map, (void *)tp);
  if (old != alls)
    pmap_insert(type_alloc_map, (void *)tp, (void *)alls);
}

/**
 * Return a flexible array containing all Cast-nodes
 * that "create" a given type.
 */
static ir_node **get_type_cast_array(ir_type *tp) {
  ir_node **res;
  if (!type_cast_map) type_cast_map = pmap_create();

  if (pmap_contains(type_cast_map, (void *)tp)) {
    res = (ir_node **) pmap_get(type_cast_map, (void *)tp);
  } else {
    res = NEW_ARR_F(ir_node *, 0);
    pmap_insert(type_cast_map, (void *)tp, (void *)res);
  }
  return res;
}

void set_type_cast_array(ir_type *tp, ir_node **alls) {
  ir_node **old = pmap_get(type_cast_map, (void *)tp);
  if (old != alls)
    pmap_insert(type_cast_map, (void *)tp, (void *)alls);
}

/**
 * Return a flexible array containing all pointer
 * types that points-to a given type.
 */
static ir_type **get_type_pointertype_array(ir_type *tp) {
  ir_type **res;
  if (!type_pointertype_map) type_pointertype_map = pmap_create();

  if (pmap_contains(type_pointertype_map, (void *)tp)) {
    res = (ir_type **) pmap_get(type_pointertype_map, (void *)tp);
  } else {
    res = NEW_ARR_F(ir_type *, 0);
    pmap_insert(type_pointertype_map, (void *)tp, (void *)res);
  }

  return res;
}
void set_type_pointertype_array(ir_type *tp, ir_type **pts) {
  ir_type **old = pmap_get(type_pointertype_map, (void *)tp);
  if (old != pts)
    pmap_insert(type_pointertype_map, (void *)tp, (void *)pts);
}

/**
 * Return a flexible array containing all array
 * types that have a given type as element type.
 */
static ir_type **get_type_arraytype_array(ir_type *tp) {
  ir_type **res;
  if (!type_arraytype_map) type_arraytype_map = pmap_create();

  if (pmap_contains(type_arraytype_map, (void *)tp)) {
    res = (ir_type **) pmap_get(type_arraytype_map, (void *)tp);
  } else {
    res = NEW_ARR_F(ir_type *, 0);
    pmap_insert(type_arraytype_map, (void *)tp, (void *)res);
  }

  return res;
}

void set_type_arraytype_array(ir_type *tp, ir_type **pts) {
  ir_type **old = pmap_get(type_arraytype_map, (void *)tp);
  if (old != pts)
    pmap_insert(type_arraytype_map, (void *)tp, (void *)pts);
}

/*------------------------------------------------------------------*/
/* Accessing the out data structures.                               */
/* These routines only work properly if firm is in state            */
/* trouts_consistent or trouts_inconsistent.                        */
/*------------------------------------------------------------------*/

/**------------------------------------------------------------------*/
/*   Access routines for entities                                    */
/**------------------------------------------------------------------*/

int get_entity_n_accesses(ir_entity *ent) {
  ir_node ** accs;

  assert(ent && is_entity(ent));

  accs = get_entity_access_array(ent);
  return ARR_LEN(accs);
}

ir_node *get_entity_access(ir_entity *ent, int pos) {
  ir_node ** accs;

  assert(0 <= pos && pos < get_entity_n_accesses(ent));

  accs = get_entity_access_array(ent);
  return accs[pos];
}

void add_entity_access(ir_entity *ent, ir_node *n) {
  ir_node ** accs;

  assert(ent && is_entity(ent));
  assert(n && is_ir_node(n));

  accs = get_entity_access_array(ent);
  ARR_APP1(ir_node *, accs, n);
  set_entity_access_array(ent, accs);
}

void set_entity_access(ir_entity *ent, int pos, ir_node *n) {
  ir_node ** accs;

  assert(0 <= pos && pos < get_entity_n_accesses(ent));
  assert(n && is_ir_node(n));

  accs = get_entity_access_array(ent);
  accs[pos] = n;
}

/**------------------------------------------------------------------*/

int get_entity_n_references(ir_entity *ent) {
  ir_node ** refs;

  assert(ent && is_entity(ent));

  refs = get_entity_reference_array(ent);
  return ARR_LEN(refs);
}

ir_node *get_entity_reference(ir_entity *ent, int pos) {
  ir_node ** refs;

  assert(0 <= pos && pos < get_entity_n_references(ent));

  refs = get_entity_reference_array(ent);
  return refs[pos];
}

void add_entity_reference(ir_entity *ent, ir_node *n) {
  ir_node ** refs;

  assert(ent && is_entity(ent));
  assert(n && is_ir_node(n));

  refs = get_entity_reference_array(ent);
  ARR_APP1(ir_node *, refs, n);
  set_entity_reference_array(ent, refs);
}

void set_entity_reference(ir_entity *ent, int pos, ir_node *n) {
  ir_node ** refs;

  assert(0 <= pos && pos < get_entity_n_references(ent));
  assert(n && is_ir_node(n));

  refs = get_entity_reference_array(ent);
  refs[pos] = n;
}


/**------------------------------------------------------------------*/
/*   Access routines for types                                       */
/**------------------------------------------------------------------*/

/* Number of Alloc nodes that create an instance of this type */
int get_type_n_allocs(ir_type *tp) {
  ir_node **allocs;

  assert(tp && is_type(tp));

  allocs = get_type_alloc_array(tp);
  return ARR_LEN(allocs);
}

/* Alloc node that creates an instance of this type */
ir_node *get_type_alloc(ir_type *tp, int pos) {
  ir_node **allocs;
  assert(0 <= pos && pos < get_type_n_allocs(tp));

  allocs = get_type_alloc_array(tp);
  return allocs[pos];
}

void add_type_alloc(ir_type *tp, ir_node *n) {
  ir_node **allocs;

  assert(tp && is_type(tp));
  assert(n && is_ir_node(n));

  allocs = get_type_alloc_array(tp);
  ARR_APP1(ir_node *, allocs, n);
  set_type_alloc_array(tp, allocs);
}

void set_type_alloc(ir_type *tp, int pos, ir_node *n) {
  ir_node **allocs;

  assert(0 <= pos && pos < get_type_n_allocs(tp));
  assert(n && is_ir_node(n));

  allocs = get_type_alloc_array(tp);
  allocs[pos] = n;
}

/* Number of Cast nodes that create an instance of this type */
int get_type_n_casts(ir_type *tp) {
  ir_node **casts;

  assert(tp && is_type(tp));

  casts = get_type_cast_array(tp);
  return ARR_LEN(casts);
}


int get_class_n_upcasts(ir_type *clss) {
  int i, n_casts = get_type_n_casts(clss);
  int n_instances = 0;
  for (i = 0; i < n_casts; ++i) {
    ir_node *cast = get_type_cast(clss, i);
    if (is_Cast_upcast(cast)) n_instances ++;
  }
  return n_instances;
}

int get_class_n_downcasts(ir_type *clss) {
  int i, n_casts = get_type_n_casts(clss);
  int n_instances = 0;
  for (i = 0; i < n_casts; ++i) {
    ir_node *cast = get_type_cast(clss, i);
    if (is_Cast_downcast(cast)) n_instances ++;
  }
  return n_instances;
}


/* Cast node that creates an instance of this type */
ir_node *get_type_cast(ir_type *tp, int pos) {
  ir_node **casts;
  assert(0 <= pos && pos < get_type_n_casts(tp));

  casts = get_type_cast_array(tp);
  return casts[pos];
}

void add_type_cast(ir_type *tp, ir_node *n) {
  ir_node **casts;

  assert(tp && is_type(tp));
  assert(n && is_ir_node(n));

  casts = get_type_cast_array(tp);
  ARR_APP1(ir_node *, casts, n);
  set_type_cast_array(tp, casts);
}

void set_type_cast(ir_type *tp, int pos, ir_node *n) {
  ir_node **casts;

  assert(0 <= pos && pos < get_type_n_casts(tp));
  assert(n && is_ir_node(n));

  casts = get_type_cast_array(tp);
  casts[pos] = n;
}

/**------------------------------------------------------------------*/

int get_type_n_pointertypes_to(ir_type *tp) {
  ir_type ** pts;

  assert(tp && is_type(tp));

  pts = get_type_pointertype_array(tp);
  return ARR_LEN(pts);
}

ir_type *get_type_pointertype_to(ir_type *tp, int pos) {
  ir_type ** pts;

  assert(0 <= pos && pos < get_type_n_pointertypes_to(tp));

  pts = get_type_pointertype_array(tp);
  return pts[pos];
}

void add_type_pointertype_to(ir_type *tp, ir_type *ptp) {
  ir_type ** pts;

  assert(tp && is_type(tp));
  assert(ptp && is_Pointer_type(ptp));

  pts = get_type_pointertype_array(tp);
  ARR_APP1(ir_node *, pts, ptp);
  set_type_pointertype_array(tp, pts);
}

void set_type_pointertype_to(ir_type *tp, int pos, ir_type *ptp) {
  ir_type ** pts;

  assert(0 <= pos && pos < get_type_n_pointertypes_to(tp));
  assert(ptp && is_Pointer_type(ptp));

  pts = get_type_pointertype_array(tp);
  pts[pos] = ptp;
}


/**------------------------------------------------------------------*/

int   get_type_n_arraytypes_of(ir_type *tp) {
  ir_type ** pts;

  assert(tp && is_type(tp));

  pts = get_type_arraytype_array(tp);
  return ARR_LEN(pts);
}

ir_type *get_type_arraytype_of(ir_type *tp, int pos) {
  ir_type ** pts;

  assert(0 <= pos && pos < get_type_n_arraytypes_of(tp));

  pts = get_type_arraytype_array(tp);
  return pts[pos];
}

void  add_type_arraytype_of(ir_type *tp, ir_type *atp) {
  ir_type ** pts;

  assert(tp && is_type(tp));
  assert(atp && is_Array_type(atp));

  pts = get_type_arraytype_array(tp);
  ARR_APP1(ir_node *, pts, atp);
  set_type_arraytype_array(tp, pts);
}

void  set_type_arraytype_of(ir_type *tp, int pos, ir_type *atp) {
  ir_type ** pts;

  assert(0 <= pos && pos < get_type_n_arraytypes_of(tp));
  assert(atp && is_Array_type(atp));

  pts = get_type_arraytype_array(tp);
  pts[pos] = atp;
}

/*------------------------------------------------------------------*/
/* Building and Removing the out datastructure                      */
/*------------------------------------------------------------------*/

static void init_trouts(void) {

}

/** The number of entities that can be accessed by this Sel node. */
static int get_Sel_n_accessed_entities(ir_node *sel) {
	(void) sel;
	return 1;
}

/** The entity that cat be accessed by this Sel node. */
static ir_entity *get_Sel_accessed_entity(ir_node *sel) {
  return get_Sel_entity(sel);
}

/** An addr node is a SymConst or a Sel. */
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

/** An addr node is a SymConst or a Sel.
    If Sel follow to outermost of compound. */
static ir_entity *get_addr_entity(ir_node *addr, int pos) {
  ir_entity *ent;

  switch (get_irn_opcode(addr)) {
  case iro_Sel:
    /* Treat jack array sels? They are compounds!  Follow to outermost entity.  */
    while (is_Sel(get_Sel_ptr(addr))) {
      addr = get_Sel_ptr(addr);
    }
    assert (0 <= pos && pos < get_Sel_n_accessed_entities(addr));
    ent = get_Sel_accessed_entity(addr);
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

  (void) env;
  if (get_irn_op(n) == op_Alloc) {
    add_type_alloc(get_Alloc_type(n), n);
    return;
  } else

  if (get_irn_op(n) == op_Cast) {
    add_type_cast(get_Cast_type(n), n);
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
    if (! is_Sel(addr)) return;  /* Sels before Calls mean a Load / polymorphic Call. */
  } else {
    return;
  }

  n_ents = get_addr_n_entities(addr);  /* == 1 */
  for (i = 0; i < n_ents; ++i) {
    ir_entity *ent = get_addr_entity(addr, i);
    if (ent)
      add_entity_access(ent, n);
    //else
      //add_unrecognized_access(n);
  }
}

static void chain_types(ir_type *tp) {
  if (is_Pointer_type(tp)) {
    add_type_pointertype_to(get_pointer_points_to_type(tp), tp);
  } else if (is_Array_type(tp)) {
    add_type_arraytype_of(get_array_element_type(tp), tp);
  }
}

irg_outs_state get_trouts_state(void) {
  return irp->trouts_state;
}
void           set_trouts_inconsistent(void) {
  if (irp->trouts_state == outs_consistent)
    irp->trouts_state = outs_inconsistent;
}


/* compute the field temperature. */
void compute_trouts(void) {
  int i,
      n_irgs = get_irp_n_irgs(),
      n_types = get_irp_n_types();

  free_trouts();
  init_trouts();

  /* Compute outs for irnodes. */
  for (i = 0; i < n_irgs; i++) {
    irg_walk_graph(get_irp_irg(i), NULL, chain_accesses, NULL);
  }
  walk_const_code(NULL, chain_accesses, NULL);

  /* Compute outs for types */
  for (i = 0; i < n_types; ++i) {
    chain_types(get_irp_type(i));
  }

  irp->trouts_state = outs_consistent;
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

  if (type_cast_map) {
    ir_node **casts;
    for (casts = (ir_node **)pmap_first(type_cast_map);
	 casts;
	 casts = (ir_node **)pmap_next(type_cast_map))
      ; //DEL_ARR_F(alls);
    pmap_destroy(type_cast_map);
    type_cast_map = NULL;
  }

  if (type_pointertype_map) {
    ir_node **pts;
    for (pts = (ir_node **)pmap_first(type_pointertype_map);
	 pts;
	 pts = (ir_node **)pmap_next(type_pointertype_map))
      ; //DEL_ARR_F(pts);
    pmap_destroy(type_pointertype_map);
    type_pointertype_map = NULL;
  }

  if (type_arraytype_map) {
    ir_node **pts;
    for (pts = (ir_node **)pmap_first(type_arraytype_map);
	 pts;
	 pts = (ir_node **)pmap_next(type_arraytype_map))
      ; //DEL_ARR_F(pts);
    pmap_destroy(type_arraytype_map);
    type_arraytype_map = NULL;
  }

  irp->trouts_state = outs_none;
}
