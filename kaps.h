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
 * @brief   Partitioned Boolean Quadratic Problem (PBQP) solver.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#ifndef KAPS_KAPS_H
#define KAPS_KAPS_H

#include "pbqp_t.h"

/**
 * Create an empty PBQP instance with the given number of nodes.
 */
pbqp* alloc_pbqp(unsigned number_nodes);

/**
 * Free the given PBQP.
 */
void free_pbqp(pbqp *pbqp);

/**
 * Add costs vector to given node.
 */
void add_node_costs(pbqp *pbqp, unsigned node_index, vector *costs);

/**
 * Add costs matrix between given nodes.
 */
void add_edge_costs(pbqp *pbqp, unsigned src_index, unsigned tgt_index, pbqp_matrix *costs);

pbqp_edge *get_edge(pbqp *pbqp, unsigned src_index, unsigned tgt_index);
pbqp_node *get_node(pbqp *pbqp, unsigned index);

num get_node_solution(pbqp *pbqp, unsigned node_index);
num get_solution(pbqp *pbqp);

void set_dumpfile(pbqp *pbqp, FILE *f);

#endif /* KAPS_KAPS_H */
