/* Copyright (C) 2002 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors:  Goetz Lindenmaier
*
* irbackedges.c  Access function for backedges.
*
*/

/* $Id$ */

#include "irnode_t.h"
#include "array.h"
#include "irbackedge_t.h"

/*--------------------------------------------------------------------*/
/* Backedge information. *                                            */
/*--------------------------------------------------------------------*/


/**
 * Returns backarray if the node can have backedges, else returns
 * NULL.
 *
 * Does not assert whether the backarray is correct -- use
 * very careful!
 */
static INLINE int *mere_get_backarray(ir_node *n) {
  switch(get_irn_opcode(n)) {
  case iro_Block:
    if (!get_Block_matured(n)) return NULL;
    if (interprocedural_view && n->attr.block.in_cg) {
      assert(n->attr.block.cg_backedge && "backedge array not allocated!");
      return n->attr.block.cg_backedge;
    } else {
      assert(n->attr.block.backedge && "backedge array not allocated!");
      return n->attr.block.backedge;
    }
    break;
  case iro_Phi:
      assert(n->attr.phi_backedge && "backedge array not allocated!");
    return n->attr.phi_backedge;
    break;
  case iro_Filter:
    if (interprocedural_view) {
      assert(n->attr.filter.backedge && "backedge array not allocated!");
      return n->attr.filter.backedge;
    }
    break;
  default: ;
  }
  return NULL;
}

/**
 * Returns backarray if the node can have backedges, else returns
 * NULL.
 */
static INLINE int *get_backarray(ir_node *n) {
  int *ba = mere_get_backarray(n);

  if (ba) {
    int bal = ARR_LEN(ba);  /* avoid makro expansion in assertion. */
    int inl = ARR_LEN(get_irn_in(n)) -1;  /* Use get_irn_in -- sensitive to view! */
    assert(bal == inl && "backedge array with faulty length");
  }

  return ba;
}

/**
 * Returns true if node has no backarray, or
 *              if size of backarray == size of in array.
 */
static INLINE bool legal_backarray (ir_node *n) {
  int *ba = mere_get_backarray(n);
  if (ba && (ARR_LEN(ba) != ARR_LEN(get_irn_in(n))-1))  /* Use get_irn_in -- sensitive to view! */
    return false;
  return true;
}


INLINE void fix_backedges(struct obstack *obst, ir_node *n) {
  opcode opc = get_irn_opcode(n);
  int *arr = mere_get_backarray(n);
  if (ARR_LEN(arr) == ARR_LEN(get_irn_in(n))-1)
    return;
  if (ARR_LEN(arr) != ARR_LEN(get_irn_in(n))-1) {
    arr = new_backedge_arr(obst, ARR_LEN(get_irn_in(n))-1);
    if (opc == iro_Phi)    n->attr.phi_backedge = arr;
    if ((opc == iro_Block) && !interprocedural_view)
      n->attr.block.backedge = arr;
    if ((opc == iro_Block) && interprocedural_view)
      n->attr.block.cg_backedge = arr;
    if (opc == iro_Filter) n->attr.filter.backedge = arr;
    return;
  }
  assert(legal_backarray(n));
  /* @@@ more efficient in memory consumption, not possible with
   array implementation.
  if (ARR_LEN(arr) < ARR_LEN(get_irn_in(n))-1) {
    ARR_SETLEN(int, arr, ARR_LEN(get_irn_in(n))-1);
  }*/
}

/** Returns true if the predesessor pos is a backedge. */
bool is_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  if (ba) return ba[pos];
  return false;
}

/** Remarks that edge pos is a backedge. */
void set_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  assert(ba && "can only set backedges at Phi, Filter, Block nodes.");
  ba[pos] = 1;
}

/** Remarks that edge pos is a backedge. */
void set_not_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  assert(ba && "can only set backedges at Phi, Filter, Block nodes.");
  ba[pos] = 0;
}

/** Returns true if n has backedges. */
bool has_backedges (ir_node *n) {
  int i;
  int *ba = get_backarray (n);
  if (ba)
    for (i = 0; i < get_irn_arity(n); i++)
      if (ba[i]) return true;
  return false;
}

/** Sets all backedge information to zero. */
void clear_backedges (ir_node *n) {
  int i, rem = interprocedural_view;
  int *ba;
  interprocedural_view = 0;
  ba = get_backarray (n);
  if (ba)
    for (i = 0; i < get_irn_arity(n); i++)
      ba[i] = 0;
  interprocedural_view = 1;
  ba = get_backarray (n);
  if (ba)
    for (i = 0; i < get_irn_arity(n); i++)
      ba[i] = 0;
  interprocedural_view = rem;
}
