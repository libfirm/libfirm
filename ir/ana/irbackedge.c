/*
 * Project:     libFIRM
 * File name:   ir/ana/irbackedge.c
 * Purpose:     Access function for backedges.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     7.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "irnode_t.h"
#include "irgraph_t.h"
#include "array.h"
#include "irbackedge_t.h"

/*--------------------------------------------------------------------*/
/* Backedge information.                                              */
/*--------------------------------------------------------------------*/


/**
 * Returns backarray if the node can have backedges, else returns
 * NULL.
 *
 * Does not assert whether the backarray is correct -- use
 * very careful!
 */
static INLINE int *mere_get_backarray(ir_node *n) {
  switch (get_irn_opcode(n)) {
  case iro_Block:
    if (!get_Block_matured(n)) return NULL;
    if (get_interprocedural_view() && n->attr.block.in_cg) {
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
    if (get_interprocedural_view()) {
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
    if ((opc == iro_Block) && !get_interprocedural_view())
      n->attr.block.backedge = arr;
    if ((opc == iro_Block) && get_interprocedural_view())
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

/** Returns true if the predecessor pos is a backedge. */
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
  if (ba) {
    int arity = get_irn_arity(n);
    for (i = 0; i < arity; i++)
      if (ba[i]) return true;
  }
  return false;
}

/** Sets all backedge information to zero. */
void clear_backedges (ir_node *n) {
  int i, arity;
  int rem = get_interprocedural_view();
  int *ba;
  set_interprocedural_view(false);
  ba = get_backarray (n);
  if (ba) {
    arity = get_irn_arity(n);
    for (i = 0; i < arity; i++)
      ba[i] = 0;
  }
  set_interprocedural_view(true);
  ba = get_backarray (n);
  if (ba) {
    arity = get_irn_arity(n);
    for (i = 0; i < arity; i++)
      ba[i] = 0;
  }
  set_interprocedural_view(rem);
}
