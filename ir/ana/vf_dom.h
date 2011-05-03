/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Compute the dominance tree for VFirm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_ANA_VF_DOM_H
#define FIRM_ANA_VF_DOM_H

#include <stdio.h>
#include "firm_types.h"
#include "plist.h"

/**
 * Calculates the dominance tree of the given VFirm graph, beginning at the
 * specified root node. Dominated roots are the children of the dominator, the
 * parent of a node is its immediate dominator.
 *
 * Given a node, parent and child nodes can be accessed with vd_node_get_parent
 * and the children iterator. Testing if a node is an ancestor of another (ie.
 * dominates it) is done by vd_node_dominates in O(1).
 */

typedef struct vd_info   vd_info;
typedef plist_element_t *vd_node_child_it;

/** Compute the dominance tree for the given irg. */
vd_info *vd_init(ir_graph *irg);

/**
 * Compute the dominance tree, starting at root. If "keep_block" is true, the
 * analysis will be restricted to the root nodes block.
 */
vd_info *vd_init_root(ir_node *root, int keep_block);

/** Free the dominance information for the given tree. */
void vd_free(vd_info *vdi);

/** Get the associated graph. */
ir_graph *vd_get_irg(vd_info *vdi);

/** Get the computation root node. */
ir_node *vd_get_root(vd_info *vdi);

/** Determine if the lhs node dominates the rhs node. */
int vd_node_dominates(vd_info *vdi, ir_node *lhs, ir_node *rhs);

/** Get the parent node in the dominator tree. */
ir_node *vd_node_get_parent(vd_info *vdi, ir_node *irn);

/** Iterate the child nodes of a node. */
void vd_node_child_it_init(vd_info *vdi, vd_node_child_it *it, ir_node *irn);
ir_node *vd_node_child_it_next(vd_node_child_it *it);
int vd_node_get_child_count(vd_info *vdi, ir_node *irn);

/** Dumps dominator tree debug information to the specified file. */
void vd_dump(vd_info *vdi, FILE *f);

#define foreach_vd_node_child(info, container, value, it) \
	for(vd_node_child_it_init((info), &(it), (container)), \
		(value) = vd_node_child_it_next(&(it)); \
		(value); \
		(value) = vd_node_child_it_next(&(it)))

#endif
