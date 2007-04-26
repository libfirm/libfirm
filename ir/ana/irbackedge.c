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
 * @brief     Access function for backedges.
 * @author    Goetz Lindenmaier
 * @date      7.2002
 * @version   $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

#ifndef NDEBUG
  if (ba) {
    int bal = ARR_LEN(ba);  /* avoid makro expansion in assertion. */
    int inl = ARR_LEN(get_irn_in(n)) -1;  /* Use get_irn_in -- sensitive to view! */
    assert(bal == inl && "backedge array with faulty length");
  }
#endif

  return ba;
}

/**
 * Returns nin-zero if node has no backarray, or
 *                  if size of backarray == size of in array.
 */
static INLINE int legal_backarray (ir_node *n) {
  int *ba = mere_get_backarray(n);
  if (ba && (ARR_LEN(ba) != ARR_LEN(get_irn_in(n))-1))  /* Use get_irn_in -- sensitive to view! */
    return 0;
  return 1;
}


void fix_backedges(struct obstack *obst, ir_node *n) {
  int *arr = mere_get_backarray(n);
  ir_opcode opc;

  if (! arr)
    return;

  if (ARR_LEN(arr) != ARR_LEN(get_irn_in(n))-1) {
    arr = new_backedge_arr(obst, ARR_LEN(get_irn_in(n))-1);

    opc = get_irn_opcode(n);
    if (opc == iro_Phi)
      n->attr.phi_backedge = arr;
    else if (opc == iro_Block) {
      if (!get_interprocedural_view())
        n->attr.block.backedge = arr;
      else
        n->attr.block.cg_backedge = arr;
    }
    else if (opc == iro_Filter)
      n->attr.filter.backedge = arr;
  }

  assert(legal_backarray(n));

  /* @@@ more efficient in memory consumption, not possible with
   array implementation.
  if (ARR_LEN(arr) < ARR_LEN(get_irn_in(n))-1) {
    ARR_SETLEN(int, arr, ARR_LEN(get_irn_in(n))-1);
  }*/
}

int is_inter_backedge(ir_node *n, int pos) {
  int res;
  int rem = get_interprocedural_view();
  set_interprocedural_view(0);
  res = is_backedge(n, pos);
  set_interprocedural_view(rem);
  return res;
}

int is_intra_backedge(ir_node *n, int pos) {
  int res;
  int rem = get_interprocedural_view();
  set_interprocedural_view(1);
  res = is_backedge(n, pos);
  set_interprocedural_view(rem);
  return res;
}


/* Returns non-zero if the predecessor pos is a backedge. */
int is_backedge (ir_node *n, int pos) {
  int *ba = get_backarray (n);
  if (ba) return ba[pos];
  return 0;
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

/* Returns non-zero if n has backedges. */
int has_backedges (ir_node *n) {
  int i;
  int *ba = get_backarray (n);
  if (ba) {
    int arity = get_irn_arity(n);
    for (i = 0; i < arity; i++)
      if (ba[i]) return 1;
  }
  return 0;
}

/** Sets all backedge information to zero. */
void clear_backedges (ir_node *n) {
  int i, arity;
  int rem = get_interprocedural_view();
  int *ba;
  set_interprocedural_view(0);
  ba = get_backarray (n);
  if (ba) {
    arity = get_irn_arity(n);
    for (i = 0; i < arity; i++)
      ba[i] = 0;
  }
  set_interprocedural_view(1);
  ba = get_backarray (n);
  if (ba) {
    arity = get_irn_arity(n);
    for (i = 0; i < arity; i++)
      ba[i] = 0;
  }
  set_interprocedural_view(rem);
}

int *new_backedge_arr(struct obstack *obst, int size) {
  int *res = NEW_ARR_D (int, obst, size);
  memset(res, 0, sizeof(int) * size);
  return res;
}

/* TODO: add an ir_op operation */
void new_backedge_info(ir_node *n) {
  switch(get_irn_opcode(n)) {
  case iro_Block:
    n->attr.block.cg_backedge = NULL;
    n->attr.block.backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  case iro_Phi:
    n->attr.phi_backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  case iro_Filter:
    n->attr.filter.backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  default: ;
  }
}
