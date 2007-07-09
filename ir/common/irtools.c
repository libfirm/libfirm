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
 * @brief     Some often needed tool-functions
 * @author    Michael Beck
 * @version   $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "pset.h"

#include <stdlib.h>
#include "irnode_t.h"
#include "irbackedge_t.h"
#include "irtools.h"
#include "irprintf.h"

/* the famous clear_link implementation. */
void firm_clear_link(ir_node *n, void *env) {
  (void) env;
  set_irn_link(n, NULL);
}

/*
 * Copies a node to a new irg. The Ins of the new node point to
 * the predecessors on the old irg.  n->link points to the new node.
 *
 * Does NOT copy standard nodes like Start, End etc that are fixed
 * in an irg. Instead, the corresponding nodes of the new irg are returned.
 * Note further, that the new nodes have no block.
 */
void
copy_irn_to_irg(ir_node *n, ir_graph *irg)
{
  ir_op *op = get_irn_op(n);
  ir_graph *old_irg;
  ir_node *nn = NULL;

  /* do not copy standard nodes */
  if (op == op_Bad)
    nn = get_irg_bad(irg);
  else if (op == op_NoMem)
    n = get_irg_no_mem(irg);
  else if (op == op_Block) {
    old_irg = get_irn_irg(n);

    if (n == get_irg_start_block(old_irg))
      nn = get_irg_start_block(irg);
    else if (n == get_irg_end_block(old_irg))
      nn = get_irg_end_block(irg);
  }
  else if (op == op_Start)
    nn = get_irg_start(irg);
  else if (op == op_End)
    nn = get_irg_end(irg);
  else if (op == op_Proj) {
    old_irg = get_irn_irg(n);

    if (n == get_irg_frame(old_irg))
      nn = get_irg_frame(irg);
    else if (n == get_irg_globals(old_irg))
      nn = get_irg_globals(irg);
    else if (n == get_irg_initial_mem(old_irg))
      nn = get_irg_initial_mem(irg);
    else if (n == get_irg_args(old_irg))
      nn = get_irg_args(irg);
  }

  if (nn) {
    set_irn_link(n, nn);
    return;
  }

  nn = new_ir_node(get_irn_dbg_info(n),
         irg,
         NULL,            /* no block yet, will be set later */
         op,
         get_irn_mode(n),
         get_irn_arity(n),
         get_irn_in(n) + 1);


  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_node_attr(n, nn);
  new_backedge_info(nn);
  set_irn_link(n, nn);

  /* fix the irg for blocks */
  if (is_Block(nn))
    nn->attr.block.irg = irg;
}

/*
 * Creates an exact copy of a node.
 * The copy resides in the same graph in the same block.
 */
ir_node *exact_copy(const ir_node *n) {
	ir_graph *irg = get_irn_irg(n);
	ir_node *res, *block = NULL;

	if (is_no_Block(n))
		block = get_irn_n(n, -1);

	res = new_ir_node(get_irn_dbg_info(n),
		irg,
		block,
		get_irn_op(n),
		get_irn_mode(n),
		get_irn_arity(n),
		get_irn_in(n) + 1);


	/* Copy the attributes.  These might point to additional data.  If this
	   was allocated on the old obstack the pointers now are dangling.  This
	   frees e.g. the memory of the graph_arr allocated in new_immBlock. */
	copy_node_attr(n, res);
	new_backedge_info(res);
	return res;
}

void firm_pset_dump(pset *set)
{
	void *obj;

	foreach_pset(set, obj) {
		ir_fprintf(stderr, "%+F\n", obj);
	}
}
