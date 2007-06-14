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
 * @brief       be transform helper extracted from the ia32 backend.
 * @author      Matthias Braun, Michael Beck
 * @date        14.06.2007
 * @version     $Id$
 */
#ifndef FIRM_BE_BETRANSHLP_H
#define FIRM_BE_BETRANSHLP_H

#include "firm_types.h"
#include "beirg.h"

/**
 * A callback to pre-transform some nodes before the transformation starts.
 */
typedef void (arch_pretrans_nodes)(void *arch_cg);

/**
 * The type of a transform function.
 */
typedef ir_node *(be_transform_func)(ir_node *node);

/**
 * Returns an old (saved) anchor of the currently transforming graph.
 */
ir_node *be_get_old_anchor(unsigned anchor);

/** pre-transform a node */
ir_node *be_pre_transform_node(ir_node *place);

/**
 * Calls transformation function for given node and marks it visited.
 */
ir_node *be_transform_node(ir_node *node);

/**
 * Duplicate all dependency edges of a node.
 */
void be_duplicate_deps(ir_node *old_node, ir_node *new_node);

/**
 * Duplicate a node during transformation.
 */
ir_node *be_duplicate_node(ir_node *node);

/**
 * Associate an old node with a transformed node. Uses link field.
 */
void be_set_transformed_node(ir_node *old_node, ir_node *new_node);

/**
 * enqueue all inputs into the transform queue.
 */
void be_enqueue_preds(ir_node *node);

/**
 * Transform a graph. Transformers must be registered first.
 */
void be_transform_graph(be_irg_t *birg, arch_pretrans_nodes *func, void *cg_env);

#endif /* FIRM_BE_BETRANSHLP_H */
