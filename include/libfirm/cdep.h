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
 * @brief   control dependence analysis
 * @author  Christoph Mallon
 * @version $Id$
 */
#ifndef FIRM_ANA_CDEP_H
#define FIRM_ANA_CDEP_H

#include "firm_types.h"

/**
 * An entry in the control dependence list.
 */
typedef struct cdep cdep;
struct cdep {
	ir_node *node;  /**< A node on which the current block is control dependent on. */
	cdep    *next;  /**< Link to the next one if any. */
};

/** Compute the control dependence graph for a graph. */
void compute_cdep(ir_graph *irg);

/** Free the control dependence info. */
void free_cdep(ir_graph *irg);

/**
 * Return a list of all control dependences of a block.
 */
cdep *find_cdep(const ir_node *block);

void exchange_cdep(ir_node *old, const ir_node *nw);

/**
 * Check whether dependee is (directly) control dependent on candidate.
 *
 * @param dependee   the (possible) dependent block
 * @param candidate  the (possible) block on which dependee is dependent
 */
int is_cdep_on(const ir_node *dependee, const ir_node *candidate);

/**
 * Check whether dependee is (possible iterated) control dependent on candidate.
 *
 * @param dependee   the (possible) dependent block
 * @param candidate  the (possible) block on which dependee is dependent
 */
int is_iterated_cdep_on(ir_node *dependee, ir_node *candidate);

/**
 * If block is control dependent on exactly one node, return this node, else NULL.
 *
 * @param block  the block to check
 */
ir_node *get_unique_cdep(const ir_node *block);

/**
 * check if the given block is control dependent of more than one node.
 *
 * @param block   the block to check
 */
int has_multiple_cdep(const ir_node *block);

#endif
