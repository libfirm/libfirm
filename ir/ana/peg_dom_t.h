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
 * @brief   Compute the dominance tree for PEG graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_ANA_PEG_DOM_T_H
#define FIRM_ANA_PEG_DOM_T_H

#include <stdio.h>
#include "firm_types.h"
#include "plist.h"

typedef struct pd_tree   pd_tree;
typedef plist_element_t *pd_children_iter;

/** Compute the dominance tree for the given irg. */
pd_tree *pd_init(ir_graph *irg);

/** Free the dominance information for the given tree. */
void pd_free(pd_tree *pdt);

/** Get the associated graph. */
ir_graph *pd_get_irg(pd_tree *pdt);

/** Get the dominator trees root node (ie. the return node). */
ir_node *pd_get_root(pd_tree *pdt);

/** Determine if the lhs node dominates the rhs node. */
int pd_dominates(pd_tree *pdt, ir_node *lhs, ir_node *rhs);

/** Get the parent node in the dominator tree. */
ir_node *pd_get_parent(pd_tree *pdt, ir_node *irn);

/** Get a count of the nodes children. */
int pd_get_children_count(pd_tree *pdt, ir_node *irn);

/** Initialize an iterator to iterate the nodes children. */
void pd_children_iter_init(pd_tree *pdt, ir_node *irn, pd_children_iter *it);

/** Get the next element from the given iterator. */
ir_node *pd_children_iter_next(pd_children_iter *it);

/** Dumps the dominator tree to the specified file. */
void pd_dump(pd_tree *pdt, FILE *f);

#define foreach_pd_children(pdt, irn, child, it) \
	for(pd_children_iter_init((pdt), (irn), &(it)), \
		(child) = pd_children_iter_next(&(it)); \
		(child); \
		(child) = pd_children_iter_next(&(it)))

#endif
