/* Copyright (C) 2002 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Goetz Lindenmaier
*
* irdom.h:  This file contains routines to construct and access dominator
*   information.
*   The dominator information is stored in three fields of block nodes:
*     idom: a reference to the block that is the immediate dominator of
*       this block.
*     dom_depth: a number giving the depth of the block in the dominator
*       tree.
*     pre_num:  Number in preorder traversal.
*/

/* $Id$ */

# ifndef _IRDOM_H_
# define _IRDOM_H_

# include "irgraph.h"
# include "irnode.h"


/**********************************************************************/
/** Accessing the dopminator datastructure.                          **/
/** These routines only work properly if the ir_graph is in state    **/
/** dom_consistent or dom_inconsistent.                              **/
/**********************************************************************/

ir_node *get_Block_idom(ir_node *bl);
void set_Block_idom(ir_node *bl, ir_node *n);

int get_Block_dom_depth(ir_node *bl);
void set_Block_dom_depth(ir_node *bl, int depth);

int get_Block_pre_num(ir_node *bl);
void set_Block_pre_num(ir_node *bl, int num);


/**********************************************************************/
/* Building and Removing the dominator datasturcture                 **/
/**********************************************************************/

/* Computes the dominator trees.  Sets a flag in irg to "dom_consistent".
   If the control flow of the graph is changed this flag must be set to
   "dom_inconsistent".
   Does not compute dominator information for control dead code.  Blocks
   not reachable from Start contain the following information:
     idom = NULL;
     dom_depth = -1;
     pre_num = -1; */
void compute_doms(ir_graph *irg);

/* Frees the dominator datastructures.  Sets the flag in irg to "no_dom". */
void free_dom_and_peace(ir_graph *irg);

#endif /* _IRDOM_H_ */
