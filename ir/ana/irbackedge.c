/* Copyright (C) 2002 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors:  Goetz Lindenmaier
**
** irbackedges.c  Access function for backedges.
**
*/

/* $Id$ */

#include "irnode_t.h"

/**********************************************************************/
/** Backedge information.                                            **/
/**********************************************************************/


/* Returns backarray if the node can have backedges.  Else returns
   NULL. */
inline int *get_backarray(ir_node *n) {
  switch(get_irn_opcode(n)) {
  case iro_Block:
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

/* Returns true if the predesessor pos is a backedge. */
bool is_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  if (ba) return ba[pos];
  return false;
}

/* Remarks that edge pos is a backedge. */
void set_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  assert(ba && "can only set backedges at Phi, Filter, Block nodes.");
  ba[pos] = 1;
}

/* Remarks that edge pos is a backedge. */
void set_not_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  assert(ba && "can only set backedges at Phi, Filter, Block nodes.");
  ba[pos] = 0;
}

/* Returns true if n has backedges. */
bool has_backedges (ir_node *n) {
  int i;
  int *ba = get_backarray (n);
  if (ba)
    for (i = 0; i < get_irn_arity(n); i++)
      if (ba[i]) return true;
  return false;
}

/* Sets all backedge information to zero. */
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
