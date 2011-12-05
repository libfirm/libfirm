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
 * @brief       Code for dumping backend datastructures (i.e. interference graphs)
 * @author      Matthias Braun
 */
#ifndef FIRM_BE_BEDUMP_H
#define FIRM_BE_BEDUMP_H

#include <stdio.h>
#include <stdbool.h>
#include "firm_types.h"
#include "be_types.h"

/**
 * Dump interference graph
 */
void be_dump_ifg(FILE *F, ir_graph *irg, const be_ifg_t *ifg);

/**
 * Dump interference graph with affinity edges as calculated by a
 * copy-minimisation phase
 */
void be_dump_ifg_co(FILE *F, const copy_opt_t *co,
                    bool dump_costs, bool dump_colors);

/**
 * Dump the liveness information for a graph.
 * @param f The output.
 * @param irg The graph.
 */
void be_liveness_dump(FILE *F, const be_lv_t *lv);

/**
 * node_info hook that dumps liveness for blocks
 */
void be_dump_liveness_block(void *context, FILE *F, const ir_node *block);

#endif
